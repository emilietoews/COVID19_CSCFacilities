#### SUMMARY STATISTICS ####

library(ggplot2)
library(janitor)     #for tabyl function
library(readxl)
library(dplyr)
library(tidyverse)   #includes ggplot2 and other data management tools
library(incidence2)  #epicurves
library(aweek)       #working with dates/epiweeks
library(i2extras)    #supplement to incidence2
library(forcats)     #working with factors
library(RColorBrewer)#palettes

staff_data_raw <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023_Original copy.xlsx", 
                             sheet = "Staff Cases Raw", col_types = c("text", 
                                                                      "text", "text", "text", "text", "text", 
                                                                      "text", "text", "text", "text", "numeric", 
                                                                      "date"))
View(staff_data_raw)

csc_raw <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023.xlsx", 
                      sheet = "All Cases Raw", col_types = c("date", 
                                                             "text", "text", "text", "text", "text", 
                                                             "text", "text", "text", "text", "text", 
                                                             "text", "numeric"))
View(csc_raw)

library(readxl)
agg_data <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023.xlsx", 
                       sheet = "Aggregate Cases by Facility")
View(agg_data)

###STAFF###
##Descriptive tables
staff_data_raw %>% tabyl(facility) %>%  #Proportion of cases coming out of each facility
  adorn_pct_formatting()                #puts in % with 1 decimal-place

staff_data_raw %>% tabyl(region) %>%    #Proportion of cases in each CSC region
  adorn_pct_formatting()

staff_data_raw %>% tabyl(province) %>%  #Proportion of cases by province
  adorn_pct_formatting()

csc_raw %>%
  tabyl(facility, case_type)

csc_raw %>%
  tabyl(design, case_type) #%>%
  #adorn_percentages("col")
#inmate cells total 9445
#inmate dorms total 2469
#inmate both total  4348
#staff cells total  9232
#staff dorms total  2617
#staff both total   3488

#period prevalence (PP) by facility design
PP_design <- agg_data %>%
  group_by(design) %>%
  summarise(N_inmates = sum(approx_inmate),
            inmate_cases = sum(inmate_cases),
            PP_inmates = (sum(inmate_cases)/sum(approx_inmate)/159*1000),
            N_staff = sum(approx_staff),
            staff_cases = sum(staff_cases),
            PP_staff = sum(staff_cases)/sum(approx_staff)/159*1000)
View(PP_design)

#period prevalence by security level
PP_security <- agg_data %>%
  group_by(security) %>%
  summarise(N_inmates = sum(approx_inmate),
            inmate_cases = sum(inmate_cases),
            PP_inmates = (sum(inmate_cases)/sum(approx_inmate)/159*1000),
            N_staff = sum(approx_staff),
            staff_cases = sum(staff_cases),
            PP_staff = sum(staff_cases)/sum(approx_staff)/159*1000)
View(PP_security)

#period prevalence by population gender
PP_gender <- agg_data %>%
  group_by(pop) %>%
  summarise(N_inmates = sum(approx_inmate),
            inmate_cases = sum(inmate_cases),
            PP_inmates = (sum(inmate_cases)/sum(approx_inmate)/159*1000),
            N_staff = sum(approx_staff),
            staff_cases = sum(staff_cases),
            PP_staff = sum(staff_cases)/sum(approx_staff)/159*1000)
View(PP_gender)

#period prevalence by facility type
PP_facility <- agg_data %>%
  group_by(fac_type) %>%
  summarise(N_inmates = sum(approx_inmate),
            inmate_cases = sum(inmate_cases),
            PP_inmates = (sum(inmate_cases)/sum(approx_inmate)/159*1000),
            N_staff = sum(approx_staff),
            staff_cases = sum(staff_cases),
            PP_staff = sum(staff_cases)/sum(approx_staff)/159*1000)
View(PP_facility)


csc_raw %>%
  tabyl(security, case_type) %>%
  adorn_percentages("col")

csc_raw %>%
  tabyl(pop, case_type) %>%
  adorn_percentages("col")

csc_raw %>%
  tabyl(type, case_type) %>%
  adorn_percentages("col")

csc_raw %>%
  tabyl(rrc, case_type) %>%
  adorn_percentages("col")


###OTHER FIGURES
##Bar chart with totals by facility using csc_raw
class(csc_raw$facility) #character class
csc_raw %>%
  tabyl(facility)       #number and proportions of cases in each facility

#change order of facilities to be in regional order
csc_raw <- csc_raw %>%
  mutate(
    facility = fct_relevel(facility,
                           "Atlantic Institution",
                           "Dorchester (Med)",
                           "Dorchester (Min)",
                           "Shepody Healing Centre",
                           "Nova Institution for Women",
                           "Springhill Institution",
                           "Archambault Institution (Med)",
                           "Archambault Institution (Min)",
                           "Cowansville Institution",
                           "Donnacona Institution",
                           "Drummond Institution",
                           "Federal Training Centre",
                           "Joliette Institution",
                           "La Macaza Institution",
                           "Port-Cartier Institution",
                           "Regional Mental Health Centre",
                           "Regional Reception Centre",
                           "Bath Institution",
                           "Beaver Creek Institution (Med)",
                           "Beaver Creek Institution (Min)",
                           "Collins Bay Institution",
                           "Grand Valley Institution for Women",
                           "Joyceville Institution",
                           "Millhaven Institution",
                           "Warkworth Institution",
                           "Bowden Institution and Annex",
                           "Drumheller Institution",
                           "Edmonton Institution",
                           "Edmonton Institution for Women",
                           "Grande Cache Institution",
                           "Grierson Institution",
                           "Pê Sâkâstêw Centre",
                           "Okimaw Ohci Healing Lodge",
                           "Regional Psychiatric Centre",
                           "Saskatchewan Penitentiary (Max and Med)",
                           "Saskatchewan Penitentiary (Min)",
                           "Willow Cree Healing Center",
                           "Stony Mountain Institution (Med)",
                           "Stony Mountain Institution (Min)",
                           "Fraser Valley",
                           "Kent Institution",
                           "Kwikwexwelhp Healing Village",
                           "Matsqui Institution",
                           "Mission Institution (Med)",
                           "Mission Institution (Min)",
                           "Mountain Institution",
                           "Pacific Institution",
                           "William Head Institution")
  )
levels(csc_raw$facility)

#use geom_bar so bar height is the number of rows in the data
ggplot(csc_raw) +
  geom_bar(aes(y = fct_rev(facility),
               fill = case_type),
           width = 0.7,
           alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(y = "Facility", x = "Total COVID-19 Cases") +
  scale_fill_manual(values = c("mediumseagreen", "royalblue"),
                      name = "Population",
                      labels = c("Incarcerated Individuals", "CSC Staff")) +
  theme(text = element_text(size = 16,
                            family = "serif")) #for Times New Roman font


##Bar Chart by Province
ggplot(csc_raw) +
  geom_bar(aes(y = province,
               fill = case_type),
           width = 0.7) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(y = "Region", x = "Total COVID-19 Cases") +
  scale_fill_manual(values = c("chocolate2", "green4"),
                    name = "Population",
                    labels = c("Incarcerated population", "CSC staff"))


###Cumulative cases (weekly)
##Bring in and clean the data
cum_data <- read_excel("covid_canada_dataset.xlsx", 
                       sheet = "cumulative_all weeks", col_types = c("text", 
                                                                     "text", "date", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "text"))
View(cum_data)
cum_data$date <- as.Date(cum_data$date)

###Plot it
ggplot(data = cum_data) +
  geom_col(mapping = aes(
    x = date,
    y = cum_case_1000),
    fill = "grey90") +
  geom_line(mapping = aes(
    x = date,
    y = cum_case_1000,
    colour = case_type)) +
  #geom_point(mapping = aes(
    #x = date,
    #y = cum_case_1000,
    #fill = factor(approx)),
    #size = 0.8) +
  scale_colour_manual(values = c("chocolate2", "royalblue", "green4"),
                      name = "Population") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "3 months",
               date_minor_breaks = "1 month", 
               date_labels = "%b-%y") +
  labs(x = "Date",
       y = "Cumulative Weekly COVID-19 Cases per 1,000 Individuals") +
  theme_minimal()

###Summary stats for cumulative cases
cases_by_pop <- cum_data %>%   
  group_by(case_type) %>%
  summarise(mean(cum_case_1000))
cases_by_pop

inc_by_pop <- cum_data %>%   
  group_by(case_type) %>%
  summarise(mean(IR))
inc_by_pop

hist(cum_data$IR_1000)


##Testing out some linear regression
CSC_Staff_COVID19_Data_JUNE26_2023 <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023.xlsx", 
                                                 sheet = "Aggregate Cases by Facility", 
                                                 col_types = c("text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric"))
View(CSC_Staff_COVID19_Data_JUNE26_2023)
csc_agg <- CSC_Staff_COVID19_Data_JUNE26_2023

#define variables of interest
binary_vars <- c("pop", "rrc")

csc_agg <- csc_agg %>%
  mutate(across(
    .cols = all_of(c())
  ))

##univariate tests
#by facility design
class(design_agg$all_inc_wk) #checking that it's numeric

boxplot(all_inc_wk ~ design, data = csc_agg) #boxplot of weekly incidence for facility design
design_agg <- csc_agg %>%
  filter(design != "both") #filter out the "both" cases
all_inc_by_design <- lm(all_inc_wk ~ design, data = design_agg)
summary(all_inc_by_design) #not-significant
boxplot(all_inc_wk ~ design, data = design_agg) #boxplot of just dorms vs cells
all_inc_by_design2 <- lm(all_inc_wk ~ design, data = csc_agg)
summary(all_inc_by_design2)

#by population gender
boxplot(all_inc_wk ~ pop, data = csc_agg)
all_inc_by_pop <- lm(all_inc_wk ~ pop, data = csc_agg)
summary(all_inc_by_pop) #compared with fm, m pops had significantly lower COVID-19 rate over the time period

#by security level
boxplot(all_inc_wk ~ security, data = csc_agg)
sec_agg <- csc_agg %>%
  filter(security != "multi")  #filter out the facilities with multiple security levels
all_inc_by_sec <- lm(all_inc_wk ~ security, data = sec_agg)
summary(all_inc_by_sec) #not significant, compared with max as indicator
boxplot(all_inc_wk ~ security, data = sec_agg)
all_inc_by_sec2 <- lm(all_inc_wk ~ security, data = csc_agg)
summary(all_inc_by_sec2) #not significant, compared with max as indicator

#by rrc or not
boxplot(all_inc_wk ~ rrc, data = csc_agg)
inc_rrc <- lm(all_inc_wk ~ rrc, data = csc_agg)
summary(inc_rrc) #not significant

##Multivariate
inc_lm <- lm(all_inc_wk ~ design + pop + security + rrc, data = csc_agg)
summary(inc_lm)

#add interaction term between design and security level
inc2_lm <- lm(all_inc_wk ~ design + pop + security + rrc + design*security, data = csc_agg)
summary(inc2_lm)


###PERIOD PREVALENCE
library(epiR)
##Living and working in CSC facilities (exposure) vs not
#15,044 cases (13,028 non-cases) in prisons out of 28,072 exposed
#total person time in prisons is 159 weeks*28072 individuals
#4581340 cases (34,348,562 non-cases) in general population of 38929902 not exposed (not quite accurate)
IRR_data <- matrix(c(15044, 4463448, 4581340, 6189854418), #set up a 2x2 matrix
                   nrow = 2, byrow = TRUE)          #2 rows
rownames(IRR_data) <- c("prison", "not prison");            #label columns
colnames(IRR_data) <- c("C19+", "C19-");    #label exposure (rows)
IRR_data
IRR_data <- as.table(IRR_data)

IRR_test <- epi.2by2(IRR_data,
                     method = "cohort.time",
                     conf.level = 0.95, 
                     units = 10000,
                     outcome = "as.columns")
summary(IRR_test)$ARate.strata.wald


###INCIDENCE RATE
#Check normality of the distribution
cum_data %>%
  ggplot(aes(x = IR_1000, fill = case_type)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~case_type)

cum_data %>%
  ggplot(aes(x = case_type, y = IR_1000, fill = case_type)) +
  geom_boxplot() +
  geom_jitter(colour = "black", size = 0.4, alpha = 0.7)

#incidence rate is very skewed to the left (around 0)

##Median and IQR overall
cum_data$IR_1000 <- as.numeric(cum_data$IR_1000)
cum_data <- cum_data %>%
  mutate(IR_ten = IR_1000*10)

cum_data %>%
  group_by(case_type) %>%
  summarise(
    median = median(IR_ten),
    IQR = quantile(IR_ten),
    max = max(IR_ten),
    min = min(IR_ten)
  )

##By time periods
#building a time periods column
cum_data <- cum_data %>%
  mutate(period = case_when(
    date < "2020-03-12" ~ "1",  #if date is less than "2020-03-01", then period = "1"
    date < "2020-04-24" ~ "2",  #builds downward, eliminating the cases in the above section
    date < "2020-06-09" ~ "3",
    date < "2021-01-18" ~ "4",
    date < "2021-08-25" ~ "5",
    date < "2022-01-20" ~ "6",
    date < "2022-05-14" ~ "7",
    date < "2023-02-12" ~ "8"
  ))

#median and IQR for each time period
cum_IQR <- cum_data %>%
  group_by(case_type, period) %>%
  summarise(
    median = median(IR_1000),
    IQR = quantile(IR_1000)
  )
print(cum_IQR, n = 135)

#make new column per 10,000
cum_IQR <- as.data.frame(cum_IQR)
cum_IQR <- cum_IQR %>%
  mutate(med_ten = median*10,
         IQR_ten = IQR*10)
View(cum_IQR)

##Calculate median/IQR for easing vs tightening overall
head(cum_data)
cum_data <- cum_data %>%
  mutate(restrictions = case_when(
    date < "2020-03-12" ~ "No",  #if date is less than "2020-03-01", then period = "1"
    date < "2020-04-24" ~ "Yes",  #builds downward, eliminating the cases in the above section
    date < "2020-06-09" ~ "No",
    date < "2021-01-18" ~ "Yes",
    date < "2021-08-25" ~ "No",
    date < "2022-01-20" ~ "Yes",
    date < "2022-05-14" ~ "No",
    date < "2023-02-12" ~ "No"
  ))

cum_IQR <- cum_data %>%
  group_by(case_type, restrictions) %>%
  summarise(
    median = median(IR_1000),
    IQR = quantile(IR_1000)
  )
print(cum_IQR, n = 135)

cum_IQR <- as.data.frame(cum_IQR)
cum_IQR <- cum_IQR %>%
  mutate(med_ten = median*10,
         IQR_ten = IQR*10)
View(cum_IQR)


