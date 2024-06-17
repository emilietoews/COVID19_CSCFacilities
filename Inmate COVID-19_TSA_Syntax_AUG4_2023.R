##### TIME SERIES ANALYSIS #####

#### ALL DATA ####

### DATA CLEANING ###
##Load packages
library(dplyr)
library(tidyr)
library(naniar)

##Import dataset for incarcerated individuals
library(readxl)
inmate_data <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023_Original copy.xlsx", 
                          sheet = "Inmate Cases", col_types = c("date", 
                                                                "text", "text", "text", "numeric", 
                                                                "numeric"))
View(inmate_data)

staff_data_raw <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023_Original copy.xlsx", 
                             sheet = "Staff Cases Raw", col_types = c("text",
                                                                      "text",
                                                                      "date"))
View(staff_data_raw)

##Weekly incidence overall (will need to un-group "inmate_data" to do this)

##Weekly incidence by facility
#add epiweek variable
inmate_data <- inmate_data %>%
  mutate(epiweek = yearweek(date, week_start = 1))

ggplot(inmate_data, aes(x = epiweek, y = new_inmate_cases)) +
  geom_line() +
  facet_wrap(~ facility, ncol = 5) +
  labs(x = "Date", y = "Weekly COVID-19 incidence in CSC incarcerated population") +
  theme_classic()

###Join staff and inmate data
##Get weekly staff incidence from raw dataset
staff_data_week <- staff_data_raw %>%
  group_by(date, facility) %>%   #group and tally up by weekly incidence and facility
  tally()

#remove NAs
nrow(staff_data_week)   #4,338
staff_data_week <- staff_data_week %>%
  drop_na(date)                  
nrow(staff_data_week)   #4,335

#rename "n" as "new_staff_cases"
staff_data_week <- staff_data_week %>%
  rename(new_staff_cases = n)   

#add a week of the year column
staff_data_week <- staff_data_week %>%    
  mutate(epiweek = yearweek(date, week_start = 1))

#plot it and take a look
ggplot(staff_data_week, aes(x = epiweek, y = new_staff_cases)) +
  geom_line() +
  facet_wrap(~ facility, ncol = 5) +
  labs(x = "Date", y = "Weekly COVID-19 incidence in CSC staff") +
  theme_classic()

##join based on "facility" AND "epiweek"
unique(staff_data_week$facility) #checking the unique values of facility names
unique(inmate_data$facility)
nrow(staff_data_week)            #7,336
nrow(inmate_data)                #8,918

#merge datasets
csc_data <- inmate_data %>%
  full_join(staff_data_week, by = c("epiweek", "date", "facility"))
head(csc_data)
nrow(csc_data) #12,288 rows
View(csc_data)

###Data Cleaning for new CSC dataset
#change all NAs to 0 for new_staff_cases
csc_data <- csc_data %>%
  mutate(new_staff_cases = replace_na(new_staff_cases, 0))
head(csc_data)
as.numeric(csc_data$new_staff_cases)

#Add column denoting where data is missing for inmate infections
csc_data <- csc_data %>%
  mutate(na = if_else(is.na(inmate_cases) == TRUE, "1", "0"))

#change NAs to 0 for new_inmate_cases
csc_data <- csc_data %>%
  mutate(new_inmate_cases = replace_na(new_inmate_cases, 0))
head(csc_data)
as.numeric(csc_data$new_inmate_cases)

#Convert to long format for visualization
csc_data_long <- csc_data %>%
  select(date, facility, new_inmate_cases, epiweek, new_staff_cases, na) %>% #subset the dataset to just these columns
  pivot_longer(cols = c(new_inmate_cases, new_staff_cases),  #specify the columns you want to turn into one long one
               names_to = "population",
               values_to = "value")

#Plot it!
ggplot(csc_data_long, aes(x = epiweek, y = value,
                          group = population, colour = population)) +
  facet_wrap(~ facility, ncol = 5) +
  geom_line() +
  theme_classic()
  
  
  



#number of complete rows (using "naniar" package)
pct_complete_case(csc_data)           #only 8% of rows are complete
pct_complete_case(csc_data)/100*11935 #that is only 954 rows

#export "staff_data" and "csc_data" to their own Excel files for future work
library(rio)
export(csc_data, "CSC Dataset_Full_AUG2_2023.xlsx")

#Epidemic Curve for IIs
inmate_data$date <- as.Date(inmate_data$date) #make sure the "date" column is actually dates
nrow(inmate_data) #8554
inmate_data <- inmate_data %>%
  distinct(.keep_all = TRUE) #removing the one duplicate from the dataset
inmate_data <- tsibble(inmate_data, key = facility, index = date)
class(inmate_data)














