### WASH WANTS Assessment Data Cleaning Script - Cholera Tool
### REACH Yemen
### V4
### 02/06/2020

rm(list=ls())

### Download custom packages
# devtools::install_github("mabafaba/cleaninginspectoR", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/clog", force = T, build_vignettes = T)
# devtools::install_github("agualtieri/dataqualitycontrol", force = T, build_vignettes = T)
# devtools::install_github("mrdwab/koboloadeR", force = T, build_vignettes = T) 
# devtools::install_github("mabafaba/hypegrammaR", ref = "develop", force = T, build_opts = c(), build_vignettes = T) 
# devtools::install_github("agualtieri/koboAPI") 





### Load required library
library("cleaninginspectoR")
library("clog")
library("dataqualitycontrol")
library("tidyverse")
library("openxlsx")



### Change date of data cleaning
current_date <- Sys.Date()

### Import dataset from Kobo server throught koboloader API
#datasets <- kobo_datasets(user = "reach_yemen:KOBOyemREACH2017", api = "kobohr")
#kobo_data_downloader("408888", "reach_yemen:KOBOyemREACH2017", api = "kobohr")

### Load choices and questionnaire
choices <- read.csv("./data/choices.csv")
questions<- read.csv("./data/questions.csv")


### Load data and rename variables
data <- read.xlsx("./data/WASH Cholera Key Informant Questionnaire_test.xlsx", sheet = "sheet 1")

names(data)[names(data) == "_uuid"] <- "uuid"
names(data)[names(data) == "_index"] <- "index"

### Base check: remove sensitive information from dataset before starting the analysis - package used: dataqualitycontrol
data <- anonymise_dataset(data, c("deviceid", "_submission_time", "x_Note", "x_focalpoint_code", "_id", "_submission_time", "_validation_status"))


### Preparing dataset
#### Remove column names and replace
#names(data_408888) <- gsub(pattern = "G0_End/", replacement = "", x = names(data_408888))


#### Merging enumerator name and surname
#data$enum_id <- as.character(paste(data$g_enum_last_name_EN, data$g_enum_name_EN, sep = " "))


### First check: numeric variables and other - package used: cleaninginspectoR
issues <- inspect_all(data, "uuid") 

issue_table <- issues

issue_table <- issue_table[!grepl("'other' response. may need recoding.", issue_table$issue_type),]
issue_table <- issue_table[!grepl("Potentially sensitive information.", issue_table$issue_type),]



if(nrow(issue_table)>=1) {
  
  issue_table <- issue_table %>% 
                  mutate(uuid=data[.$index,"uuid",drop=TRUE], ngo=data[.$index,"g_enum_agency"], area = data[.$index, "g_sub_district", drop = TRUE])
  
  issue_table$new_vaue <- " "
  issue_table$fix <- "Checked with partner"
  issue_table$checked_by <- "ON"
  
  issue_log <- data.frame(uuid = issue_table$uuid, 
                          agency = issue_table$ngo, 
                          area = issue_table$area, 
                          variable = issue_table$variable,
                          issue = issue_table$issue_type, 
                          old_value = issue_table$value, 
                          new_value = issue_table$new_vaue, 
                          fix = issue_table$fix, 
                          checked_by = issue_table$checked_by)
  
} else {
  
  issue_log <- data.frame(uuid = as.character(),
                          agency = as.character(),
                          area = as.character(),
                          variable = as.character(),
                          issue = as.character(),
                          old_value = as.character(),
                          new_value = as.character(),
                          fix = as.character(),
                          checked_by = as.character())
  
  
  
  
  
  
}

#write.csv(issues, paste0("./output/issues_",current_date,".csv"), row.names = F)
#browseURL(paste0("./output/issues_",current_date,".csv"))


## Second check: soft constraints

### Issues with accessing water but no constraints
water_needs <- data %>% select("uuid", "g_enum_agency", "g_sub_district", "w_watersmell", "w_accessproblem") %>%
                mutate(access_check = ifelse((is.na(data$w_accessproblem) & (data$w_watersmell == "none" | data$w_watersmell == "few" | data$w_watersmell == "half")),1,0)) %>%
                filter(access_check == 1)

if(nrow(water_needs)>=1) {
  
  water_needs$issue_type <- "The community KI reported a lack of access to water but no access constraint were highlighted."
  water_needs$checked_by <- "NG"
  water_needs$new_value <- " "
  water_needs$fix <- "Checked with partner"
  water_needs$variable <- "w_waterneeds"
  
  
  access_log <- data.frame(uuid = water_needs$uuid, 
                            agency = water_needs$g_enum_agency, 
                            area = water_needs$g_sub_district, 
                            variable = water_needs$variable,
                            issue = water_needs$issue_type, 
                            old_value = water_needs$w_waterneeds, 
                            new_value = water_needs$new_value, 
                            fix = water_needs$fix, 
                            checked_by = water_needs$checked_by)
  
  
} else {
  
  access_log <- data.frame(uuid = as.character(),
                            agency = as.character(),
                            area = as.character(),
                            variable = as.character(),
                            issue = as.character(),
                            old_value = as.character(),
                            new_value = as.character(),
                            fix = as.character(),
                            checked_by = as.character()) 
  
  print("No issues realted to access to water. The dataset seems clean.")}


### Issues with accessing soap but no constraints
soap_needs <- data %>% select("uuid", "g_enum_agency", "g_sub_district", "h_have_soap", "h_soap_problem") %>%
  mutate(soap_check = ifelse((is.na(data$h_soap_problem) & (data$h_have_soap == "none" | data$h_have_soap == "few" | data$h_have_soap == "half")),1,0)) %>%
  filter(soap_check == 1)

if(nrow(soap_needs)>=1) {
  
  soap_needs$issue_type <- "The community KI reported a lack of access to soap but no access constraint were highlighted."
  soap_needs$checked_by <- "NG"
  soap_needs$new_value <- " "
  soap_needs$fix <- "Checked with partner"
  soap_needs$variable <- "h_have_soap"
  
  
  soap_log <- data.frame(uuid = soap_needs$uuid, 
                           agency = soap_needs$g_enum_agency, 
                           area = soap_needs$g_sub_district, 
                           variable = soap_needs$variable,
                           issue = soap_needs$issue_type, 
                           old_value = soap_needs$h_have_soap, 
                           new_value = soap_needs$new_value, 
                           fix = soap_needs$fix, 
                           checked_by = soap_needs$checked_by)
  
  
} else {
  
soap_log <- data.frame(uuid = as.character(),
                           agency = as.character(),
                           area = as.character(),
                           variable = as.character(),
                           issue = as.character(),
                           old_value = as.character(),
                           new_value = as.character(),
                           fix = as.character(),
                           checked_by = as.character()) 
  
  print("No issues realted to access to soap. The dataset seems clean.")}

### Check times
source("./R/check_time.R")
time_stamp <- select(data, "uuid", "start", "end", "g_enum_agency", "g_sub_district")
check_time <- check_time(time_stamp, 5, 40)


names(check_time)[names(check_time) == "index"] <- "uuid"

check_time$g_enum_agency <- data$g_enum_agency[match(check_time$uuid, data$uuid)]
check_time$g_sub_district <- data$g_sub_district[match(check_time$uuid, data$uuid)]

if(nrow(check_time) >= 1){
  
  check_time$new_value <- " "
  check_time$fix <- "Checked with partner"
  check_time$checked_by <- "ON"
  check_time$issue_type <- "The survey was completed in less than 10 minutes or more than 40 minutes"
  check_time$variable <- "Lenght of survey"
  
  check_time_log <- data.frame(uuid = check_time$uuid, 
                               agency = check_time$g_enum_agency, 
                               area = check_time$g_sub_district, 
                               variable = check_time$variable, 
                               issue = check_time$issue_type, 
                               old_value = check_time$value, 
                               new_value = check_time$new_value, 
                               fix = check_time$fix, 
                               checked_by = check_time$checked_by)
  
} else {
  
  check_time_log <- data.frame(uuid = as.character(),
                               agency = as.character(),
                               area = as.character(),
                               variable = as.character(),
                               issue = as.character(),
                               old_value = as.character(),
                               new_value = as.character(),
                               fix = as.character(),
                               checked_by = as.character())
  
  
  print("The lenghts of the survey are within acceptable values. No cleaning needed.") }

#### Check for shortest path
count_na <- function(x) sum(is.na(x))

data$CountNa <- rowSums(apply(is.na(data), 2, as.numeric))

shortest_path <- data %>% select("uuid", "g_enum_agency", "g_sub_district", "CountNa")
shortest_path <- shortest_path %>% filter(CountNa > 75)



if(nrow(shortest_path)>=1) {
  
  shortest_path$issue_type <- "The majority of entries are NAs"
  shortest_path$checked_by <- "NG"
  shortest_path$new_value <- " "
  shortest_path$fix <- "Checked with partner"
  shortest_path$variable <- "Count of all variables"
  
  
  shortest_path_log <- data.frame(uuid = shortest_path$uuid, 
                         agency = shortest_path$g_enum_agency, 
                         area = shortest_path$g_sub_district, 
                         variable = shortest_path$variable,
                         issue = shortest_path$issue_type, 
                         old_value = shortest_path$CountNa, 
                         new_value = shortest_path$new_value, 
                         fix = shortest_path$fix, 
                         checked_by = shortest_path$checked_by)
  
  
} else {
  
  shortest_path_log <- data.frame(uuid = as.character(),
                         agency = as.character(),
                         area = as.character(),
                         variable = as.character(),
                         issue = as.character(),
                         old_value = as.character(),
                         new_value = as.character(),
                         fix = as.character(),
                         checked_by = as.character()) 
  
  print("No enumerators seems to have taken the shortest path")}


### Final: building cleaning log
cleaning_log <- plyr::rbind.fill(issue_log,
                                 access_log,
                                 soap_log,
                                 shortest_path_log)


final_log <- list("cleaning_log" = cleaning_log,
                  "Timestamp checks"= check_time_log)

write.xlsx(final_log, paste0("./output/WASH_WANTS Cholera_cleaning log_",current_date,".xlsx"))
browseURL(paste0("./output/WASH_WANTS Cholera_cleaning log_",current_date,".xlsx"))

                           

### Stuff not done anymore
#data <- as_tibble(select(data, "g_governorate", "g_district", "g_sub_district", "g_resident", "g_orgaidloop_count", c(starts_with("g_orgaidloop["), "uuid")))

#data <- as_tibble(select(data, -c(starts_with("g_orgaidloop["))))

#data$chlorine_dist_year <- questions$label..english[match(data$w_chlorineyear, questions$name)]

