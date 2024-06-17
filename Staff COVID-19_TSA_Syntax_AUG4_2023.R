### TIME SERIES ANALYSIS ###

#load packages
library(readxl)       #for importing excel spreadsheets
library(tsibble)      #handles time series datasets
library(vctrs)        #needed for slider
library(slider)       #calculates moving averages
library(feasts)       #time series decomposition and autocorrelation
library(forecast)     #fits sin and cosin terms to data
library(trending)     #fits and assesses models
library(yardstick)    #looks at model accuracy
library(surveillance) #for aberrant detection
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)    #for using day() and month() units
library(scales)       #for changing the x-axis scale when it is in date format
library(stringr)      #for wrapping x-axis labels on two lines
library(rio)

###CODE FOR IMPORTING THE NEW STAFF_DATA DATASET
inmate_cases_raw <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023.xlsx", 
                               sheet = "Inmate Cases Raw", col_types = c("date", 
                                                                         "text", "text", "numeric", "numeric", 
                                                                         "text", "text", "text", "text"))
View(inmate_cases_raw)

staff_data_raw <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023.xlsx", 
                             sheet = "Staff Cases Raw", col_types = c("text", 
                                                                      "numeric", "text", "text", "text", 
                                                                      "text", "text", "text", "text", "text", 
                                                                      "text", "numeric", "date"))
View(staff_data_raw)

###EPIDEMIC CURVES

######################################################
###OVERALL CURVE FOR STAFF WITH ROLLING AVERAGE
staff_data_raw <- staff_data_raw %>%
  drop_na(date)

##group by date to get count data for staff dataset and rename
staff_data_all <- staff_data_raw %>%
  group_by(date) %>%
  tally()
head(staff_data_all)

#rename n as "new_staff_cases"
staff_data_all <- staff_data_all %>%
  rename(new_staff_cases = n)
head(staff_data_all)

#fill in dates to be continuous reporting
staff_data_all <- staff_data_all %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(min(date), max(date), by = "day"))
head(staff_data_all)

#fill in NAs as 0 cases
staff_data_all <- staff_data_all %>%
  mutate(new_staff_cases = replace_na(new_staff_cases, 0))
head(staff_data_all)

##rough ggplot with just a line showing all the data
ggplot(staff_data_all, aes(x = date, y = new_staff_cases)) +
  geom_line() +
  ylim(0, 115) +
  labs(x = "Date", y = "Daily COVID-19 incidence in CSC staff") +
  theme_classic()

##create moving average variable (also deals with missing data)
rolling_staff <- staff_data_all %>%
  mutate(
    reg_7day_ave = slide_dbl(      #name the 7 day sum column
                                   #slide_dbl works through each row, whereas slide_index_dbl would use a column as an index for "smart" rolling
      new_staff_cases,             #calculate the rolling mean from "new_staff_cases"
      .f = ~mean(.x, na.rm = T),   #function is mean() with the missing values removed
      .before = 6))                #window is the ROW and the 6 rows prior
head(rolling_staff)
tail(rolling_staff)

##plot the smoothed data
ggplot(data = rolling_staff) +
  geom_col(mapping = aes(x = date, y = new_staff_cases), size = 0.8, colour = "grey" ) +
  geom_line(mapping = aes(x = date, y = reg_7day_ave), size = 0.5) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels = "%b-%y") +
  labs(x = "Date", y = "Daily COVID-19 Incidence in CSC Staff") +
  theme_classic()

###PERIODICITY OF STAFF CASES
###working with count data in "staff_data_all"
##Add in an epiweek column
staff_data_all$date <- as.Date(staff_data_all$date)
staff_data_all <- staff_data_all %>%
  mutate(epiweek = yearweek(date, week_start = 1))
head(staff_data_all)

##Define the function
periodogram <- function(x, 
                        counts, 
                        start_week = c(2002, 1), 
                        period = 52, 
                        output = "weeks") {
  
  
  ## make sure is not a tsibble, filter to project and only keep columns of interest
  prepare_data <- dplyr::as_tibble(x)
  
  # prepare_data <- prepare_data[prepare_data[[strata]] == j, ]
  prepare_data <- dplyr::select(prepare_data, {{counts}})
  
  ## create an intermediate "zoo" time series to be able to use with spec.pgram
  zoo_cases <- zoo::zooreg(prepare_data, 
                           start = start_week, frequency = period)
  
  ## get a spectral periodogram not using fast fourier transform 
  periodo <- spec.pgram(zoo_cases, fast = FALSE, plot = FALSE)
  
  ## return the peak weeks 
  periodo_weeks <- 1 / periodo$freq[order(-periodo$spec)] * period
  
  if (output == "weeks") {
    periodo_weeks
  } else {
    periodo
  }
  
}

#get spectral periodogram for extracting weeks with the highest frequencies
#checking of seasonality
periodo <- periodogram(staff_data_all,
                       new_staff_cases,
                       start_week = c(2020, 7),
                       output = "periodogram")

#pull spectrum and frequence into a dataframe for plotting
periodo <- data.frame(periodo$freq, periodo$spec)

#plot a periodogram showing the most frequently occuring periodicity
ggplot(data = periodo,
       aes(x = 1/(periodo.freq/52),
       y = log(periodo.spec))) +
  geom_line() +
  labs(x = "Period (Weeks)",
       y = "Log(density)")

#get a vector weeks in ascending order
peak_weeks <- periodogram(staff_data_all,
                          new_staff_cases,
                          start_week = c(2020, 7),
                          output = "weeks")
peak_weeks

  
###DECOMPOSITION OF STAFF CASES (CAN'T DO THIS)
##Decompose the staff dataset to look at trend-cycle, seasonality, and random variation
#need to have our dataset as a tsibble
staff_data_all <- tsibble(staff_data_all, index = epiweek, key = date)

staff_data_all %>%
  #uses an additive classical decomposition model
  model(classical_decomposition(new_staff_cases, type = "multiplicative")) %>%
  #extract important information from the model
  components() %>%
  #generate a plot
  autoplot()
#Not very helpful, since our time series actually has no, or less than two periods (only 2020-2023)
  
###AUTOCORRELATION OF STAFF CASES
##full autocorrelation
staff_data_all %>%
  #calculate autocorrelation using a years-worth of lags
  ACF(new_staff_cases, lag_max = 52) %>%
  #show the plot
  autoplot()
#could not compute a lag, probably because of the short timeframe

##partial autocorrelation
staff_data_all %>%
  PACF(new_staff_cases, lag_max = 52) %>%
  autoplot() #also didn't work

##We can test for independence (no autocorrelation) just in case
Box.test(staff_data_all$new_staff_cases, type = "Ljung-Box")
#p-value is significant (<2.2e-16 so we know there is autocorrelation)


###################################################################################
##INMATE CURVE OVERALL WITH ROLLING AVERAGE
#Load inmate_data and clean it briefly
inmate_cases_raw <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023.xlsx", 
                               sheet = "Inmate Cases Raw", col_types = c("date", 
                                                                         "text", "text",
                                                                         "numeric"))
View(inmate_cases_raw)

#group by date to get count data for inmate dataset and rename
inmate_data_all <- inmate_cases_raw %>%
  group_by(date) %>%
  tally()
head(inmate_data_all)

#rename n as "new_inmate_cases"
inmate_data_all <- inmate_data_all %>%
  rename(new_inmate_cases = n)
head(inmate_data_all)

#make sure "date" is a date
inmate_data_all$date <- as.Date(inmate_data_all$date)

#rough ggplot with just a line showing all the data
ggplot(inmate_data_all, aes(x = date, y = new_inmate_cases)) +
  geom_line() +
  ylim(0, 500) +
  labs(x = "Date", y = "Daily COVID-19 Incidence in Incarcerated Population") +
  theme_classic()

#create moving average variable (also deals with missing data)
rolling_inmate <- inmate_data_all %>%
  mutate(
    indexed_14day_ave = slide_index_dbl(  #using 14 days to account for missingness
      #slide_index_dbl uses a column as an index for "smart" rolling
      new_inmate_cases,             #calculate the rolling mean from "new_staff_cases"
      .i = date,                    #indexed with "date"
      .f = ~mean(.x, na.rm = TRUE), #function is mean() with the missing values removed
      .before = days(13)))           #window is the DAY and the 6 rows prior
head(rolling_inmate)
tail(rolling_inmate)

#plot the smoothed data (a bit messy, better data needed)
ggplot(data = rolling_inmate) +
  geom_col(mapping = aes(x = date, y = new_inmate_cases), size = 0.8, colour = "grey" ) +
  geom_line(mapping = aes(x = date, y = indexed_14day_ave), size = 0.5) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels = "%b-%y") +
  labs(x = "Date", y = "Daily COVID-19 Incidence in Incarcerated Population") +
  theme_classic()


##################################################################################
##STAFF CURVE BY FACILITY
##Daily incidence with rolling average by facility
#Group by facility and date
staff_data_facility <- staff_data_raw %>%
  group_by(date, facility) %>%
  tally()
head(staff_data_facility)
staff_data_facility$date <- as.Date(staff_data_facility$date)

#rename n as "new_staff_cases"
staff_data_facility <- staff_data_facility %>%
  rename(new_staff_cases = n)
head(staff_data_facility)

#calculate rolling averages by group (don't need the above code)
rolling_staff_facility <- staff_data_raw %>%
  count(facility, date, name = "new_staff_cases") %>%
  arrange(facility, date) %>%     #arrange rows by facility then date
  group_by(facility) %>%          #group by facility
  mutate(
    staff_indexed_7day_facility = slide_index_dbl(  
                                  #slide_index_dbl gives "smart" rolling average so we don't need to fill out empty dates with 0s
    .x = new_staff_cases,        #count of cases per facility-day
    .i = date,                    #index on date of case
    .f = mean,                    #use mean()
    .before = days(6)            #use the day and the 6 days prior
  )
)
head(rolling_staff_facility)

ggplot(data = rolling_staff_facility) +
  geom_col(                          #plots daily case counts as grey bars
    mapping = aes(x = date, y = new_staff_cases),
    size = 0.8, colour = "grey") +
  geom_line(                         #plots rolling average as a line coloured by hospital
    mapping = aes(
      x = date,
      y = staff_indexed_7day_facility,
      colour = facility),
    size = 1) +
  facet_wrap(~facility, ncol = 10, scales = 'free') +  #5 columns for the facilities
  theme_classic() +                  #simplify the background
  scale_y_continuous(limits = c(0, 20)) + #y axis from 0 to 20
  scale_x_date(breaks = "3 months", minor_breaks = "1 month", date_labels = "%b-%y") +
  theme(legend.position = "none") +  #no legend  
  xlab("Date Case was Reported") +
  ylab("Daily COVID-19 Incidence in CSC Staff")


##############################################################################
###INMATE CURVE BY FACILITY
##Daily incidence with rolling average by facility
#group by date to get count data for inmate dataset and rename
inmate_data_facility <- inmate_cases_raw %>%
  group_by(date, facility) %>%
  tally()
head(inmate_data_facility)
inmate_data_facility$date <- as.Date(inmate_data_facility$date)

#rename n as "new_staff_cases"
inmate_data_facility <- inmate_data_facility %>%
  rename(new_inmate_cases = n)
head(inmate_data_facility)

#Calculate rolling averages by group
rolling_inmate_facility <- inmate_cases_raw %>%
  count(facility, date, name = "new_inmate_cases") %>%
  arrange(facility, date) %>%                 #arrange rows by facility then date
  group_by(facility) %>%                      #group by facility
  mutate(
    inmate_indexed_14day_facility = slide_index_dbl(  #slide_index_dbl gives "smart" rolling average so we don't need to fill out empty dates with 0s
      .x = new_inmate_cases,                     #count of cases per facility-day
      .i = date,                                #index on date of case
      .f = mean,                                #use mean()
      .before = days(13)                         #use the day and the 6 days prior
    )
  )
rolling_inmate_facility$date <- as.Date(rolling_inmate_facility$date)
head(rolling_inmate_facility)

#plot it (very choppy due to interrupted data)
ggplot(data = rolling_inmate_facility) +
  geom_col(                          #plots daily case counts as grey bars
    mapping = aes(x = date, y = new_inmate_cases),
    size = 0.8, colour = "grey") +
  geom_line(                         #plots rolling average as a line coloured by hospital
    mapping = aes(
      x = date,
      y = inmate_indexed_14day_facility,
      colour = facility),
    size = 1) +
  facet_wrap(~facility, ncol = 10, scales = 'free') +  #5 columns for the facilities
  theme_classic() +                  #simplify the background
  scale_y_continuous(limits = c(0, 170)) + #y axis from 0 to 20
  scale_x_date(breaks = "3 months", minor_breaks = "1 month", date_labels = "%b-%y") +
  theme(legend.position = "none") +  #no legend  
  xlab("Date Case was Reported") +
  ylab("Daily COVID-19 Incidence in Incarcerated Population")


######################################################################
###STAFF AND INMATES OVERALL
#Raw datasets joined manually in Excel and saved in sheet in main file
csc_raw <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023.xlsx", 
                      sheet = "All Cases Raw", col_types = c("date", 
                                                             "text", "text", "text", "text", "text", 
                                                             "text", "text", "text", "text", "text", 
                                                             "text", "numeric"))
View(csc_raw)

#group by date and case type to get count data
csc_data <- csc_raw %>%
  group_by(date, case_type) %>%
  tally()
head(csc_data)
View(csc_data)

#rename n as "new_cases"
csc_data <- csc_data %>%
  rename(new_cases = n)
head(csc_data)

#make sure "date" is a date
csc_data$date <- as.Date(csc_data$date)

##create moving average variable for each case type (also deals with missing data)
#remove NAs
csc_rolling <- csc_raw %>%
  drop_na(date) %>%
  count(case_type, date, name = "new_cases") %>%
  arrange(case_type, date) %>%
  group_by(case_type) %>%
  mutate(
    indexed_7day_ave = slide_index_dbl(  #using 7 days to account for missingness
      #slide_index_dbl uses a column as an index for "smart" rolling
      .x = new_cases,    #calculate the rolling mean from "new_staff_cases"
      .i = date,         #indexed with "date"
      .f = mean,        #function is mean() with the missing values removed
      .before = days(6)  #window is the DAY and the 6 rows prior
      )
    )           
head(csc_rolling)
tail(csc_rolling)
csc_rolling$date <- as.Date(csc_rolling$date)

#rough faceted ggplot showing the two series
ggplot(csc_rolling, aes(x = date, y = new_cases)) +
  facet_grid(case_type ~., scales = "free_y") +
  geom_line() +
  labs(x = "Date", y = "Daily COVID-19 Incidence") +
  theme_classic()

#plot the smoothed data
ggplot(data = csc_rolling) +
  geom_col(
    mapping = aes(
      x = date, 
      y = new_cases,
      fill = case_type), 
    size = 1,
    alpha = 0.4) +
  geom_line(
    mapping = aes(
      x = date, 
      y = indexed_7day_ave,
      colour = case_type), 
    size = 0.5) +
  scale_fill_manual(values = c("red", "royalblue"), 
                    name = "Population",
                    labels = c("Incarcerated \nIndividuals", "Staff")) +
  scale_colour_manual(values = c("darkred", "dodgerblue4"),
                      name = "Population",
                      labels = c("Incarcerated \nIndividuals", "Staff")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month", date_labels = "%b-%y") +
  labs(x = "Date", 
       y = "7-day Rolling Average of Daily COVID-19 Case Incidence") +
  theme_minimal()


##################################################################
###STAFF AND INMATES BY FACILITY (WEEKLY) 
##Explore the data overall
head(staff_data_raw)
staff_data_raw <- staff_data_raw %>%
  mutate(epiweek = yearweek(date, week_start = 1))

head(inmate_cases_raw)
inmate_cases_raw <- inmate_cases_raw %>%
  mutate(epiweek = yearweek(date, week_start = 1))

##Get count data for each dataset, tally by week
staff_data_raw$date <- as.Date(staff_data_raw$date)
staff_facility_week <- staff_data_raw %>%
  drop_na(date) %>%
  group_by(facility, num_staff) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date(     
      from = min(week),        #starting from the beginning week
      to = max(week),          #to the end week
      by = "week"),            #by week
    fill = list(n = 0)) %>%    #fill in all the nas with 0
  rename(new_staff_cases = n)

head(staff_facility_week)
table(staff_facility_week$facility)
export(staff_facility_week, "staff_facility_week.xlsx")

staff_facility_IRs <- staff_facility_week %>%
  mutate(IR_ten = new_staff_cases/num_staff*10000,
         IR_100 = new_staff_cases/num_staff*100) %>%
  group_by(facility) 

staff_facility_IRs$IR_ten <- as.numeric(staff_facility_IRs$IR_ten)

hist(staff_facility_IRs$IR_ten)

staff_IR_summary <- staff_facility_IRs %>%
  group_by(facility) %>%
  summarise(
    median = median(IR_100),
    IQR = quantile(IR_100)
    #max = max(IR_100),
    #min = min(IR_100),
    #mean = mean(IR_100),
    #period.prev = (sum(new_staff_cases)/num_staff*100)/159
  )
View(staff_IR_summary)
table(staff_IR_summary$facility)
staff_facility_IRs %>%
  summarise(
    total = sum(new_staff_cases)
  )

#Inmates
inmate_cases_raw$date <- as.Date(inmate_cases_raw$date)
inmate_facility_week <- inmate_cases_raw %>%
  drop_na(date) %>%
  group_by(facility, capacity) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date(     
      from = min(week),        #starting from the beginning week
      to = max(week),          #to the end week
      by = "week"),            #by week
    fill = list(n = 0)) %>%    #fill in all the nas with 0
  rename(new_inmate_cases = n)

head(inmate_facility_week)
table(staff_facility_week$facility)

inmate_facility_IRs <- inmate_facility_week %>%
  mutate(IR_ten = new_inmate_cases/(capacity/16262*12736)*10000,
         IR_100 = new_inmate_cases/(capacity/16262*12736)*100) %>%
  group_by(facility)
  
inmate_IR_summary <- inmate_facility_IRs %>%
  group_by(facility) %>%
  summarise(
    median = median(IR_100),
    #IQR = quantile(IR_100),
    #max = max(IR_100),
    #min = min(IR_100),
    period.prev = (sum(new_inmate_cases)/(capacity/16262*12736)*100)/159)

View(inmate_IR_summary)

####################################################################################
####CASES BY SECURITY LEVEL#######################################
###New datasets grouping by security level and number of staff
staff_data_raw$date <- as.Date(staff_data_raw$date)
staff_security_week <- staff_data_raw %>%
  drop_na(date) %>%
  group_by(security) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date(     
      from = min(week),        #starting from the beginning week
      to = max(week),          #to the end week
      by = "week"),            #by week
    fill = list(n = 0)) %>%    #fill in all the nas with 0
  rename(new_staff_cases = n)

#export, add in beginning and end dates by hand, import
export(staff_security_week, "staff_security_week.xlsx")

staff_security_week <- read_excel("staff_security_week.xlsx", 
                                  col_types = c("text", "date", "numeric"))

staff_security_week <- staff_security_week %>%
  mutate(num_staff = case_when(
    security == "max" ~ "2927",
    security == "med" ~ "5572",
    security == "min" ~ "1450",
    security == "multi" ~ "5388"
  ))

#change to numeric
staff_security_week$num_staff <- as.numeric(staff_security_week$num_staff)

staff_security_IRs <- staff_security_week %>%
  mutate(IR_ten = new_staff_cases/num_staff*10000,
         IR_1000 = new_staff_cases/num_staff*1000,
         IR_100 = new_staff_cases/num_staff*100) %>%
  group_by(security) 

staff_security_IRs$IR_1000 <- as.numeric(staff_security_IRs$IR_1000)

table(staff_security_IRs$security)
hist(staff_security_IRs$IR_1000)

staff_IR_security_summary <- staff_security_IRs %>%
  group_by(security) %>%
  summarise(
    median = median(IR_1000),
    IQR = quantile(IR_1000),
    #max = max(IR_100),
    #min = min(IR_100),
    #mean = mean(IR_100),
    #period.prev = (sum(new_staff_cases)/num_staff*1000)/159
  )
View(staff_IR_security_summary)

staff_facility_IRs %>%
  summarise(
    total = sum(new_staff_cases)
  )

#Inmates
inmate_cases_raw$date <- as.Date(inmate_cases_raw$date)
inmate_security_week <- inmate_cases_raw %>%
  drop_na(date) %>%
  group_by(security) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date( 
      #from = "2020-03-30",
      from = min(week),        #starting from the beginning week
      #to = 2023-02-19,
      to = max(week),          #to the end week
      by = "week",             #up by each week
      week_start = 7),         #week starts on a Sunday
    fill = list(n = NA)) %>%    #fill in all the nas with 0
  rename(new_inmate_cases = n)

#export file
export(inmate_security_week, "inmate_security_week.csv")

#redid the gaps in data by hand, with imputation
#bring in the new dataset
inmate_security_week <- read_excel("inmate_security_week.xlsx", 
                                   col_types = c("text", "date", "numeric"))

inmate_security_week <- inmate_security_week %>%
  mutate(num_inmates = case_when(
    security == "max" ~ "1811",
    security == "med" ~ "5312",
    security == "min" ~ "1368",
    security == "multi" ~ "4244"
  ))

inmate_security_week$num_inmates <- as.numeric(inmate_security_week$num_inmates)

head(inmate_security_week)
table(staff_security_week$security)

inmate_security_IRs <- inmate_security_week %>%
  mutate(IR_ten = new_inmate_cases/num_inmates*10000,
         IR_1000 = new_inmate_cases/num_inmates*1000,
         IR_100 = new_inmate_cases/num_inmates*100) %>%
  group_by(security)

inmate_security_IR_summary <- inmate_security_IRs %>%
  group_by(security) %>%
  summarise(
    median = median(IR_1000),
    IQR = quantile(IR_1000),
    #mean = mean(IR_1000)
    #max = max(IR_100),
    #min = min(IR_100),
    #period.prev = (sum(new_inmate_cases)/(capacity/16262*12736)*100)/159
    )

View(inmate_security_IR_summary)


####################################################################################
####CASES BY FACILITY TYPE#######################################
###New datasets grouping by security level and number of staff
staff_data_raw$date <- as.Date(staff_data_raw$date)
staff_type_week <- staff_data_raw %>%
  drop_na(date) %>%
  group_by(fac_type) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date(     
      from = min(week),        #starting from the beginning week
      to = max(week),          #to the end week
      by = "week"),            #by week
    fill = list(n = 0)) %>%    #fill in all the nas with 0
  rename(new_staff_cases = n)

#export, add in beginning and end dates by hand, import
export(staff_security_week, "staff_type_week.xlsx")

staff_security_week <- read_excel("staff_security_week.xlsx", 
                                  col_types = c("text", "date", "numeric"))

staff_security_week <- staff_security_week %>%
  mutate(num_staff = case_when(
    security == "max" ~ "2927",
    security == "med" ~ "5572",
    security == "min" ~ "1450",
    security == "multi" ~ "5388"
  ))

#change to numeric
staff_security_week$num_staff <- as.numeric(staff_security_week$num_staff)

staff_security_IRs <- staff_security_week %>%
  mutate(IR_ten = new_staff_cases/num_staff*10000,
         IR_1000 = new_staff_cases/num_staff*1000,
         IR_100 = new_staff_cases/num_staff*100) %>%
  group_by(security) 

staff_security_IRs$IR_1000 <- as.numeric(staff_security_IRs$IR_1000)

table(staff_security_IRs$security)
hist(staff_security_IRs$IR_1000)

staff_IR_security_summary <- staff_security_IRs %>%
  group_by(security) %>%
  summarise(
    median = median(IR_1000),
    IQR = quantile(IR_1000),
    #max = max(IR_100),
    #min = min(IR_100),
    #mean = mean(IR_100),
    #period.prev = (sum(new_staff_cases)/num_staff*1000)/159
  )
View(staff_IR_security_summary)

staff_facility_IRs %>%
  summarise(
    total = sum(new_staff_cases)
  )

#Inmates
inmate_cases_raw$date <- as.Date(inmate_cases_raw$date)
inmate_security_week <- inmate_cases_raw %>%
  drop_na(date) %>%
  group_by(security) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date( 
      #from = "2020-03-30",
      from = min(week),        #starting from the beginning week
      #to = 2023-02-19,
      to = max(week),          #to the end week
      by = "week",             #up by each week
      week_start = 7),         #week starts on a Sunday
    fill = list(n = NA)) %>%    #fill in all the nas with 0
  rename(new_inmate_cases = n)

#export file
export(inmate_security_week, "inmate_security_week.csv")

#redid the gaps in data by hand, with imputation
#bring in the new dataset
inmate_security_week <- read_excel("inmate_security_week.xlsx", 
                                   col_types = c("text", "date", "numeric"))

inmate_security_week <- inmate_security_week %>%
  mutate(num_inmates = case_when(
    security == "max" ~ "1811",
    security == "med" ~ "5312",
    security == "min" ~ "1368",
    security == "multi" ~ "4244"
  ))

inmate_security_week$num_inmates <- as.numeric(inmate_security_week$num_inmates)

head(inmate_security_week)
table(staff_security_week$security)

inmate_security_IRs <- inmate_security_week %>%
  mutate(IR_ten = new_inmate_cases/num_inmates*10000,
         IR_1000 = new_inmate_cases/num_inmates*1000,
         IR_100 = new_inmate_cases/num_inmates*100) %>%
  group_by(security)

inmate_security_IR_summary <- inmate_security_IRs %>%
  group_by(security) %>%
  summarise(
    median = median(IR_1000),
    IQR = quantile(IR_1000),
    #mean = mean(IR_1000)
    #max = max(IR_100),
    #min = min(IR_100),
    #period.prev = (sum(new_inmate_cases)/(capacity/16262*12736)*100)/159
  )

View(inmate_security_IR_summary)



################################################################
###2020 W5 (Feb1, 2020) to 2020 W44 (Oct31, 2020)
##Subset raw datasets and pull out just this date range
head(staff_data_raw)
staff_data_raw <- staff_data_raw %>%
  mutate(epiweek = yearweek(date, week_start = 1))

staff_sub_2020 <- staff_data_raw[staff_data_raw$date > "2020-01-31" &
                                      staff_data_raw$date < "2020-06-01",]
head(staff_sub_2020)
tail(staff_sub_2020)

table(staff_sub_2020$facility)

head(inmate_cases_raw)
inmate_cases_raw <- inmate_cases_raw %>%
  mutate(epiweek = yearweek(date, week_start = 1))

inmate_sub_2020 <- inmate_cases_raw[inmate_cases_raw$date > "2020-01-31" &
                                      inmate_cases_raw$date < "2020-06-01",]
head(inmate_sub_2020)
tail(inmate_sub_2020)

table(inmate_sub_2020$facility)

##Get count data for each dataset, tally by week
staff_sub_2020$date <- as.Date(staff_sub_2020$date)
staff_2020_weekly <- staff_sub_2020 %>%
  filter(facility %in% c("Port-Cartier Institution",  #pick out only these facilities
                         "Joliette Institution",
                         "Federal Training Centre",
                         "Mission Institution (Med)")) %>%
  group_by(facility) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date(     
      from = min(week),        #starting from the beginning week
      to = max(week),          #to the end week
      by = "week"),            #by week
    fill = list(n = 0)) %>%    #fill in all the nas with 0
  rename(new_staff_cases = n)

table(staff_2020_weekly$facility)

inmate_sub_2020$date <- as.Date(inmate_sub_2020$date)
inmate_2020_weekly <- inmate_sub_2020 %>%
  filter(facility %in% c("Port-Cartier Institution",  #pick out only these facilities
                         "Joliette Institution",
                         "Federal Training Centre",
                         "Mission Institution (Med)")) %>%
  group_by(facility) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date(     
      from = min(week),        #starting from the beginning week
      to = max(week),          #to the end week
      by = "week"),            #by week
    fill = list(n = 0)) %>%    #fill in all the nas with 0
  rename(new_inmate_cases = n)

table(inmate_2020_weekly$facility)

##Join
csc_2020 <- staff_2020_weekly %>%
  select(week, facility, new_staff_cases) %>%
  full_join(inmate_2020_weekly,
            by = c("week", "facility"))
head(csc_2020)

csc_2020$week <- as.Date(csc_2020$week) #week date as date

##Convert to long format
csc_2020_long <- csc_2020 %>%
  #keep the variables we are interested in (incidence rates and epiweek)
  select(week, facility, new_staff_cases, new_inmate_cases) %>%
  #change data to long format, using epiweek as your key
  pivot_longer(
    #!epiweek
    cols = c(`new_staff_cases`, `new_inmate_cases`),
    #move column names to the "case_type" column
    names_to = "case_type",
    #move cell values to the "new_cases" column
    values_to = "new_cases"
  )
head(csc_2020_long)

#replace NAs with 0s
csc_2020_long <- csc_2020_long %>%
  mutate(new_cases = replace_na(new_cases, 0))
head(csc_2020_long)
nrow(csc_2020_long)

#plot it
ggplot(data = csc_2020_long) +
  geom_col(                   #plots daily case counts as grey bars
    mapping = aes(x = week, 
                  y = new_cases,
                  fill = case_type),
    size = 0.8,
    alpha = 0.5) +
  geom_line(
    mapping = aes(x = week,
                  y = new_cases,
                  colour = case_type)) +
  facet_wrap(~facility, ncol = 2, scales = 'free') +  #5 columns for the facilities
  theme_classic() +                  #simplify the background
  scale_y_continuous(expand = c(0,0)) + #y axis from 0 to 20
  scale_x_date(breaks = "3 months", minor_breaks = "1 month", date_labels = "%b-%y") +
  xlab("Date Case was Reported") +
  ylab("Daily New Cases")


###2021 W19 (Mar5, 2022) to 2021 W45 (Jul16, 2022)
##Subset raw datasets and pull out just this date range
staff_sub_2022 <- staff_data_raw[staff_data_raw$date > "2022-03-05" &
                                   staff_data_raw$date < "2022-07-16",]
head(staff_sub_2022)
tail(staff_sub_2022)

table(staff_sub_2022$facility)

inmate_sub_2022 <- inmate_cases_raw[inmate_cases_raw$date > "2022-03-05" &
                                      inmate_cases_raw$date < "2022-07-16",]
head(inmate_sub_2022)
tail(inmate_sub_2022)

table(inmate_sub_2022$facility)

##Get count data for each dataset, tally by week
staff_sub_2022$date <- as.Date(staff_sub_2022$date)
staff_2022_weekly <- staff_sub_2022 %>%
  filter(facility %in% c("Archambault Institution (Med)",     #pick out only these facilities
                         #"Atlantic Institution",
                         #"Bath Institution",
                         #"Beaver Creek Institution (Med)",
                         #"Bowden Institution and Annex",
                         #"Collins Bay Institution",
                         "Cowansville Institution",
                         #"Donnacona Institution",
                         "Dorchester (Med)",
                         "Dorchester (Min)",
                         "Drummond Institution",
                         #"Federal Training Centre",
                         #"Grande Cache Institution",
                         #"Joyceville Institution",
                         "La Macaza Institution",
                         "Matsqui Institution",
                         #"Millhaven Institution",
                         "Mountain Institution",
                         #"Port-Cartier Institution",
                         #"Regional Reception Centre",
                         "Saskatchewan Penitentiary (Max and Med)",
                         "Springhill Institution")) %>%
  group_by(facility) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date(     
      from = min(week),        #starting from the beginning week
      to = max(week),          #to the end week
      by = "week"),            #by week
    fill = list(n = 0)) %>%    #fill in all the nas with 0
  rename(new_staff_cases = n)

table(staff_2022_weekly$facility)

inmate_sub_2022$date <- as.Date(inmate_sub_2022$date)
inmate_2022_weekly <- inmate_sub_2022 %>%
  filter(facility %in% c("Archambault Institution (Med)",     #pick out only these facilities
                         #"Atlantic Institution",
                         #"Bath Institution",
                         #"Beaver Creek Institution (Med)",
                         #"Bowden Institution and Annex",
                         #"Collins Bay Institution",
                         "Cowansville Institution",
                         #"Donnacona Institution",
                         "Dorchester (Med)",
                         "Dorchester (Min)",
                         "Drummond Institution",
                         #"Federal Training Centre",
                         #"Grande Cache Institution",
                         #"Joyceville Institution",
                         "La Macaza Institution",
                         "Matsqui Institution",
                         #"Millhaven Institution",
                         "Mountain Institution",
                         #"Port-Cartier Institution",
                         #"Regional Reception Centre",
                         "Saskatchewan Penitentiary (Max and Med)",
                         "Springhill Institution")) %>%
  group_by(facility) %>%
  mutate(week = floor_date(    #make new column for week the cases occured
    date,                      #using the date variable
    unit = "week")) %>%        #group by week
  count(week) %>%              #tally up the cases by week
  tidyr::complete(             #fill in the missing dates so it is continuous
    week = seq.Date(     
      from = min(week),        #starting from the beginning week
      to = max(week),          #to the end week
      by = "week"),            #by week
    fill = list(n = 0)) %>%    #fill in all the nas with 0
  rename(new_inmate_cases = n)

table(inmate_2022_weekly$facility)

##Join
csc_2022 <- staff_2022_weekly %>%
  select(week, facility, new_staff_cases) %>%
  full_join(inmate_2022_weekly,
            by = c("week", "facility"))
head(csc_2022)

csc_2022$week <- as.Date(csc_2022$week) #week date as date

##Convert to long format
csc_2022_long <- csc_2022 %>%
  #keep the variables we are interested in (incidence rates and epiweek)
  select(week, facility, new_staff_cases, new_inmate_cases) %>%
  #change data to long format, using epiweek as your key
  pivot_longer(
    #!epiweek
    cols = c(`new_staff_cases`, `new_inmate_cases`),
    #move column names to the "case_type" column
    names_to = "case_type",
    #move cell values to the "new_cases" column
    values_to = "new_cases"
  )
head(csc_2022_long)

#replace NAs with 0s
csc_2022_long <- csc_2022_long %>%
  mutate(new_cases = replace_na(new_cases, 0))
head(csc_2022_long)
nrow(csc_2022_long)

#plot it
ggplot(data = csc_2022_long) +
  geom_col(                   #plots daily case counts as grey bars
    mapping = aes(x = week, 
                  y = new_cases,
                  fill = case_type),
    size = 0.8,
    alpha = 0.4) +
  geom_line(
    mapping = aes(x = week,
                  y = new_cases,
                  colour = case_type),
    size = 0.7) +
  facet_wrap(~facility,
             ncol = 5,           #4 columns for the facilities
             scales = 'free',
             labeller = labeller(group = label_wrap_gen(width = 5))) +  
  scale_y_continuous(expand = c(0,0)) + #y axis from 0 to 20
  scale_x_date(breaks = "2 week", 
               minor_breaks = "1 week", 
               date_labels = "%b %e") +
  xlab("Date") +
  ylab("Reported COVID-19 cases") +
  scale_fill_manual(values = c("green4", "royalblue"),
                    labels = c("Incarcerated", "Staff"),
                    name = "Population") +
  scale_colour_manual(values = c("green4", "royalblue"),
                      labels = c("Incarcerated", "Staff"),
                      name = "Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), #angle the date labels so they fit
        legend.position = "top", #legend at the top
        text = element_text(family = "serif"),     #for Times New Roman font
        strip.text.x = element_text(size = 12),     #text size for facet labels
        axis.text = element_text(size = 18),        #text size for x-axis labels
        axis.title = element_text(size = 18),      #text size for axis titles
        legend.text = element_text(size = 18),      #text size for legend labels
        legend.title = element_text(size = 18))    #text size for legend title  





##################################################################
###################################################################
####POPULATION VS STAFF VS INMATES GRAPH TOTAL
###Importing and cleaning datasets
##Canada population dataset
can_pop_data <- read_excel("Canada COVID-19 Population_Dataset_AUG18(1).xlsx", 
                           sheet = "national", col_types = c("text", 
                                                             "date", "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
View(can_pop_data)
class(can_pop_data)

#make sure date is a date, and add epiweek variable starting on Monday
can_pop_data$date <- as.Date(can_pop_data$date) 
can_pop_data <- can_pop_data %>%                
  mutate(epiweek = yearweek(date, week_start = 1))

#define dataset as a time series object
can_pop_data <- tsibble(can_pop_data, index = epiweek)
class(can_pop_data)

#rename columns so we know it is dealing with the population dataset
can_pop_data <- can_pop_data %>%
  rename(new_pop_cases = new_cases,
         total_pop_cases = total_cases,
         pop_case_IR = case_IR,
         pop_rolling_IR_ave = rolling_IR_ave,
         pop_rolling_ave = rolling_ave)
head(can_pop_data) #make sure we kept "date" to use as our x axis labels

#quickly plot weekly incidence to check it out
ggplot(can_pop_data, aes(x = date, y = new_pop_cases)) +
  geom_line()

#and cases per 100,000
ggplot(can_pop_data, aes(x = date, y = pop_case_IR)) +
  geom_line()

#columns for cases per 100,000 people per week, with rolling ave per day 
ggplot(can_pop_data, aes(x = date)) +
  geom_col(aes(y = pop_case_IR)) +
  geom_line(aes(y = pop_rolling_IR_ave), colour = "red")

##Staff dataset
#drop NAs and add epiweek column
head(staff_data_raw)
staff_data_raw <- staff_data_raw %>%
  drop_na(date) %>%
  mutate(epiweek = yearweek(date, week_start = 1))

#group and get counts by epiweek
staff_data_week <- staff_data_raw %>%
  group_by(epiweek) %>%               
  tally() %>%
  #rename the count column
  rename(new_staff_cases = n) %>% 
  #add in a new column for the incidence rate per 100,000 staff
  mutate(staff_case_IR = new_staff_cases/15337*100000) %>%
  #add in a new column identifying all these cases as staff cases
  mutate(case_type = "Staff")
head(staff_data_week)

staff_data_week <- tsibble(staff_data_week, index = epiweek)

#quickly plot it and have a look
ggplot(staff_data_week, aes(x = epiweek, y = staff_case_IR)) +
  geom_line()

##Inmate dataset
#drop NAs and add epiweek column
head(inmate_cases_raw)
inmate_cases_raw <- inmate_cases_raw %>%
  drop_na(date) %>%
  mutate(epiweek = yearweek(date, week_start = 1))

#group and get counts by epiweek
inmate_data_week <- inmate_cases_raw %>%
  group_by(epiweek) %>%
  tally() %>%
  #rename count column
  rename(new_inmate_cases = n) %>%
  #add new column for incidence rate per 100,000 inmates
  mutate(inmate_case_IR = new_inmate_cases/12756*100000) %>%
  #add new column denoting all cases as inmate cases
  mutate(case_type = "Incarcerated Population")
head(inmate_data_week)
summary(inmate_data_week$inmate_case_IR)

inmate_data_week <- tsibble(inmate_data_week, index = epiweek)

#plot it and check it out
ggplot(inmate_data_week, aes(x = epiweek, y = inmate_case_IR)) +
  geom_line() #messy, but hopefully we get new data from ATIP

###Merge the three datasets together
#Checking the length of each dataset
#we should only end up with as many rows as in pop dataset
nrow(can_pop_data)      #159 rows
nrow(staff_data_week)   #134 rows
nrow(inmate_data_week)  #81 rows (not as good, which is why we need the full dataset)

#full_join staff data to canada pop data
covid_canada <- can_pop_data %>%
  select(case_type, pop_case_IR, epiweek, date) %>%
  full_join(staff_data_week, by = c("epiweek"))

#full_join inmate data to other two datasets
covid_canada <- covid_canada %>%
  select(pop_case_IR, epiweek, date, staff_case_IR) %>%
  full_join(inmate_data_week, by = "epiweek") %>%
  drop_na(date) #last row is inmate data only so we will drop it for now
nrow(covid_canada) #159 rows so we are good to go!
head(covid_canada)

###Plotting it
##Convert to long format
covid_canada_long <- covid_canada %>%
  #keep the variables we are interested in (incidence rates and epiweek)
  select(epiweek, date, pop_case_IR, staff_case_IR, inmate_case_IR) %>%
  #change data to long format, using epiweek as your key
  pivot_longer(
    #!epiweek
    cols = c(`pop_case_IR`, `staff_case_IR`, `inmate_case_IR`),
    #move column names to the "case_type" column
    names_to = "case_type",
    #move cell values to the "IR" column
    values_to = "IR"
  )
head(covid_canada_long)
nrow(covid_canada_long)

#rename the "case_type" levels to make it easier to understand
covid_canada_long <- covid_canada_long %>%
  mutate(case_type = recode(case_type,
                            #convert OLD = NEW
                            "pop_case_IR" = "General population",
                            "staff_case_IR" = "CSC staff",
                            "inmate_case_IR" = "Incarcerated population"))

#replace all NAs with 0s and add IR per 1,000 individuals
covid_canada_long <- covid_canada_long %>%
  mutate(IR = replace_na(IR, 0),
         IR_1000 = IR/100,
         IR_ten = IR/10)
head(covid_canada_long)

##Bring in "cumulative_all_weeks" dataset to check it out against "covid_canada_long" dataset
all_covid <- read_excel("covid_canada_dataset.xlsx", 
                        sheet = "cumulative_all weeks", col_types = c("text", 
                                                                      "text", "date", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "skip"))
head(all_covid)

all_covid <- all_covid %>%
  mutate(IR_ten = IR_100000/10)

all_covid$date <- as.Date(all_covid$date)

##Plot via ggplot
#ggplot(data = covid_canada_long) +
ggplot(data = all_covid) +
  geom_col(mapping = aes(
    x = date,
    y = IR_ten,
    #fill = case_type
    ),
    size = 0.8,
    fill = "grey",
    alpha = 0.5) +
  geom_line(mapping = aes(
    x = date,
    y = IR_ten,
    colour = case_type),
    size = 0.8,
    alpha = 0.5) +
  scale_colour_manual(values = c("green4", "chocolate2", "royalblue"),
                      name = "Population",
                      #for presentation figure
                      labels = c("Prison staff", "Canadian population", "People living in prisons")
                                 ) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "3 months",
               date_minor_breaks = "1 month", 
               date_labels = "%b \n%y") +
  labs(x = "Date",
       #y = "Weekly COVID-19 Incidence per 10,000 Individuals"
       #for presentation figure
       y = "New COVID-19 cases in \n10,000 people each week"
       ) +
  theme_minimal()+
  theme(legend.position = c(0.05, 0.95), #place legend top left corner
        legend.justification = c("left", "top"),  #top left
        legend.box.just = "left", #justify text in the legend box
        legend.box.background = element_rect(colour = "white", #white outline for legend box
                                             fill = "white"),  #white background for legend box
        legend.margin = margin(6, 6, 6, 6),  #size of the margins on the box
        text = element_text(size = 24,
                            family = "serif")) #for Times New Roman font
  
 
 
##################################################################
####POPULATION VS STAFF VS INMATES By PROVINCE
###Data Cleaning
##Provincial data
prov_data <- read_excel("Canada COVID-19 Population_Dataset_AUG18(1).xlsx", 
                        sheet = "by province", col_types = c("text", 
                                                             "date", "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "text"))
View(prov_data)

#make sure date is a date, and add epiweek variable stargting on Monday
prov_data$date <- as.Date(prov_data$date) 
prov_data <- prov_data %>%                
  mutate(epiweek = yearweek(date, week_start = 1))

#define dataset as a time series object
prov_data <- tsibble(prov_data, 
                     index = epiweek,
                     key = province)
class(prov_data)

#rename columns so we know it is dealing with the population dataset
prov_data <- prov_data %>%
  rename(new_prov_cases = new_cases,
         total_prov_cases = total_cases,
         prov_case_IR = case_IR,
         prov_rolling_IR_ave = rolling_IR_ave,
         prov_rolling_ave = rolling_ave)

#quickly plot weekly incidence to check it out
ggplot(prov_data, aes(x = date, 
                      y = new_prov_cases,
                      colour = province)) +
  geom_line()

#and cases per 100,000
ggplot(prov_data, aes(x = date, 
                      y = prov_case_IR,
                      colour = province)) +
  geom_line()

##Staff dataset
#drop NAs and add epiweek column
head(staff_data_raw)
staff_data_raw <- staff_data_raw %>%
  drop_na(date) %>%
  mutate(epiweek = yearweek(date, week_start = 1))

#group and get counts by epiweek
staff_data_week_prov <- staff_data_raw %>%
  group_by(epiweek, province) %>%               
  tally() %>%
  #rename the count column
  rename(new_staff_cases = n) 

staff_data_week_prov <- tsibble(staff_data_week_prov, 
                                index = epiweek, 
                                key = province)

##Inmate dataset
#drop NAs and add epiweek column
head(inmate_cases_raw)
inmate_cases_raw <- inmate_cases_raw %>%
  drop_na(date) %>%
  mutate(epiweek = yearweek(date, week_start = 1))

#group and get counts by epiweek
inmate_data_week_prov <- inmate_cases_raw %>%
  group_by(epiweek, province) %>%
  tally() %>%
  #rename count column
  rename(new_inmate_cases = n)

inmate_data_week_prov <- tsibble(inmate_data_week_prov, 
                                 index = epiweek,
                                 key = province)
head(inmate_data_week_prov)

###Merge the three datasets together
#Checking the length of each dataset
#we should only end up with as many rows as in pop dataset
nrow(prov_data)              #1272 rows
nrow(staff_data_week_prov)   #642 rows
nrow(inmate_data_week_prov)  #324 rows (not as good, which is why we need the full dataset)

#full_join staff data to canada pop data
covid_prov <- prov_data %>%
  select(staff_pop, inmate_pop, prov_case_IR, epiweek, date) %>%
  full_join(staff_data_week_prov, by = c("epiweek", "province"))

#full_join inmate data to other two datasets
covid_prov <- covid_prov %>%
  #select(staff_pop, inmate_pop, prov_case_IR, epiweek, date, new_staff_cases, province) %>%
  full_join(inmate_data_week_prov, by = c("epiweek", "province")) %>%
  drop_na(date)
head(covid_prov)
nrow(covid_prov) #1272 rows so we are good to go!

#calculate IR per 100,000 for staff and inmate
#based on estimated staff and inmate population numbers
covid_prov <- covid_prov %>%
  mutate(staff_IR_prov = new_staff_cases/staff_pop*100000,
         inmate_IR_prov = new_inmate_cases/inmate_pop*100000)

###Plotting it
##Convert to long format
covid_prov_long <- covid_prov %>%
  #keep the variables we are interested in (incidence rates and epiweek)
  select(epiweek, date, province, prov_case_IR, staff_IR_prov, inmate_IR_prov) %>%
  #change data to long format, selecting the three IR columns to alter
  pivot_longer(
    cols = c(`prov_case_IR`, `staff_IR_prov`, `inmate_IR_prov`),
    #move column names to the "case_type" column
    names_to = "case_type",
    #move cell values to the "IR" column
    values_to = "IR"
  )
head(covid_prov_long)

#rename the "case_type" levels to make it easier to understand
covid_prov_long <- covid_prov_long %>%
  mutate(case_type = recode(case_type,
                            #convert OLD = NEW
                            "prov_case_IR" = "General population",
                            "staff_IR_prov" = "CSC staff",
                            "inmate_IR_prov" = "Incarcerated population"))

#replace all NAs with 0s and add IR per 1,000
covid_prov_long <- covid_prov_long %>%
  mutate(IR = replace_na(IR, 0),
         IR_1000 = IR/100,
         IR_ten = IR/10)
head(covid_prov_long)

##Plot via ggplot
ggplot(data = covid_prov_long) +
  geom_col(mapping = aes(
    x = date,
    y = IR_ten,
    #fill = case_type
  ),
  size = 0.8,
  fill = "grey",
  alpha = 0.5) +
  geom_line(mapping = aes(
    x = date,
    y = IR_ten,
    colour = case_type),
    size = 0.5,
    alpha = 0.7) +
  #plot for each province
  facet_wrap(~province, 
             ncol = 2, 
             scales = 'free'
             ) +
  #scale_fill_manual(values = c("darksalmon", "royalblue", "darkseagreen"),
  #name = "Population") +
  scale_colour_manual(values = c("green4", "chocolate2", "royalblue"),
                      name = "Population") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "3 months",
               date_minor_breaks = "1 month", 
               date_labels = "%b-%y") +
  labs(x = "Date",
       y = "Weekly COVID-19 Incidence per 10,000 Individuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), #angle the date labels so they fit
        legend.position = "top", #legend at the top
        text = element_text(size = 16,
                            family = "serif")) #for Times New Roman font



###############################################################
###############################################################
####CUMULATIVE CASES FOR CANADA, STAFF, AND INMATES
###Bring in and clean the data
cum_data <- read_excel("covid_canada_dataset.xlsx", 
                       sheet = "cumulative_all weeks", col_types = c("text", 
                                                                     "text", "date", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric"))
View(cum_data)
head(cum_data)
cum_data$date <- as.Date(cum_data$date)

cum_data <- cum_data %>%
  mutate(cum_case_ten = cum_case_100000/10)

###Plot it
ggplot(data = cum_data) +
  geom_line(mapping = aes(
    x = date,
    y = cum_case_ten,
    colour = case_type)) +
  scale_colour_manual(values = c("green4", "chocolate2", "royalblue"),
                      name = "Population",
                      #for presentation figure
                      labels = c("Prison staff", "Canadian population", "People living in prisons")
                      ) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "3 months",
               date_minor_breaks = "1 month", 
               date_labels = "%b-%y") +
  labs(x = "Date",
       #y = "Cumulative COVID-19 Cases per 10,000 Individuals"
       #for presentation figure
       y = "Total COVID-19 cases \nin 10,000 people"
       ) +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95), #place legend top left corner
        legend.justification = c("left", "top"),  #top left
        legend.box.just = "left", #justify text in the legend box
        legend.box.background = element_rect(colour = "white", #white outline for legend box
                                             fill = "white"),  #white background for legend box
        legend.margin = margin(6, 6, 6, 6),  #size of the margins on the box
        axis.text.x = element_text(angle = 60, hjust = 1),
        text = element_text(size = 24,
                            family = "serif")) #for Times New Roman font



##############################################################
test2.dates <- unique(test2$date)
hist(as.Date(test2$date), breaks = "days")
test2$date <- as.Date(test2$date)

test2.dates <- as.data.frame(test2.dates) %>%
  rename(date = test2.dates)
test2.dates$date <- as.Date(test2.dates$date)


ggplot(test2, aes(x = date)) +
  geom_histogram(binwidth = 1) +
  scale_x_date(date_breaks = "1 day")

test3 <- left_join(all_dates,
                  select(test2.dates, date),
                  by = "date")
head(test3)
View(test2.dates)

head(test)

library(calendR)
all_dates <- seq(as.Date("2020-03-30"), 
                 as.Date("2023-03-26"), 
                 by = "days")

test <- inmate_cases_raw %>%
  group_by(date) %>%
  tally()

summary(test$n)
head(test)
tail(test)

all_dates <- as.data.frame(all_dates)
class(all_dates)
all_dates <- all_dates %>%
  rename(date = dates)
head(all_dates)

test <- left_join(all_dates,
                  select(test, date, n),
                  by = "date")
head(test)

ggplot(data = test) +
  geom_point(mapping = aes(x = date,
                           y = n)) +
  geom_line(mapping = aes(x = date,
                          y = n)) +
  scale_fill_discrete(na.value = "red")


head(test2.dates)

head(test)
nrow(test)


test4 <- left_join(test2.dates,
                   select(test, date, n),
                   by = "date")
View(test4)
test4 <- test4 %>%
  mutate(n = replace_na(n, 0))
View(test4)



