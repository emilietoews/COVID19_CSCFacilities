### DATASET CLEANING ###
library(dplyr)
library(tidyr)
library(naniar)

##Import datasets
library(readxl)

#When each staff case was reported to CSC
staff_data_raw <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023_Original copy.xlsx", 
                             sheet = "Staff Cases Raw", col_types = c("text",
                                                                      "text",
                                                                      "date"))
View(staff_data_raw)

#Facility descriptions and every calendar date between first reported case and last
staff_empty <- read_excel("CSC_Staff COVID19 Data_JUNE26_2023_Original copy.xlsx", 
                          sheet = "Staff Cases Empty", col_types = c("date", 
                                                                     "text", "text", "text", "text", "text", 
                                                                     "text", "text"))
View(staff_empty)

#Inmate case data
####WORK THOUGH THE DATA CLEANING AND MERGING OF INMATE DATA HERE ALSO

##Get count data for staff COVID-19 infections
#Group by date AND facility name
staff_data <- staff_data_raw %>%
  group_by(date, facility) %>%
  tally()
staff_data 
nrow(staff_data)  #has 4,338 rows
names(staff_data)

#rename "n" as "new_staff_cases"
staff_data <- staff_data %>%
  rename(new_staff_cases = n)
names(staff_data)

#drop rows with missing dates
nrow(staff_data)              #4,338 rows
staff_data <- staff_data %>%
  drop_na(date)
nrow(staff_data)              #4,335 rows (removed 3 due to missingness)

##Join staff cases and empty datasets
#checking the lengths of each dataset before joining
length(unique(staff_data$facility)) #checking the unique values of facility names
length(unique(staff_empty$facility))
nrow(staff_empty) #53,350 rows. This should be the number of rows in our final staff dataset

#Left join based on "facility" and "date"
staff_data <- left_join(staff_empty, staff_data, by = c("date", "facility"))
summary(staff_data)
nrow(staff_data) #53,350 rows
View(staff_data)

##Change all NAs to 0
staff_data <- staff_data %>%
  mutate(new_staff_cases = replace_na(new_staff_cases, 0))
head(staff_data)
as.numeric(staff_data$new_staff_cases)
summary(staff_data)

#export "staff_data" to its own Excel files for future work
library(rio)
export(staff_data, "Staff Dataset_Reformatted_AUG2_2023.xlsx")


##Preparing full dataset
csc_data$date <- as.Date(csc_data$date) #make sure "date" is in date format

#add in a column denoting what calendar week each case is in, with weeks starting on Monday
#helps to resolve some of the differences in date between II and staff case reporting
csc_data <- csc_data %>%
  mutate(epiweek = yearweek(date, week_start = 1))
View(csc_data)
