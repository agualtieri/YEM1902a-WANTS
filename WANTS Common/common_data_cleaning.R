### WASH WANTS Assessment Data Cleaning Script - Common Tool
### REACH Yemen
### V5
### 02/06/2020

rm(list=ls())

### Download custom packages
# devtools::install_github("mabafaba/cleaninginspectoR", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/clog", force = T, build_vignettes = T)
# devtools::install_github("agualtieri/dataqualitycontrol", force = T, build_vignettes = T)
# devtools::install_github("agualtieri/koboAPI", force = T)


### Load required library
library("cleaninginspectoR")
library("clog")
library("dataqualitycontrol")
library("tidyverse")
library("openxlsx")
library("stringr")
#library("googleLanguageR")

### Load source
source("./R/moveme.R")
#source("./R/translation.R")
source("./R/check_time.R")
source("./R/add_locations.R")


### Change date of data cleaning
current_date <- Sys.Date()


### Load data an rename uuid variable
data <- read.xlsx("./data/REACH_YEM_Common KI Kobo_WANTS_Test.xlsx", sheet = "Sheet1")


names(data)[names(data) == "_uuid"] <- "data_uuid"
names(data)[names(data) == "_index"] <- "index"


### Add Location names
#data <- add.location(data)


### Base check: remove sensitive information from dataset before starting the analysis - package used: dataqualitycontrol
data <- anonymise_dataset(data, c("deviceid", "_submission_time", "_tags", "x_Note", "__version__", "_validation_status", "_id", "g_enum_last_name", "g_enum_name"))


#### Stuff to translate
# trans_data <- translate.others.arabic(data, c("g_location_other", "g_position_KI", "w_treatmethod_other", "h_hygieneitem_other", "s_disposetrash_other"))
# trans_child <- translate.others.arabic(child, "g_Name_of_Organization")


### First check: numeric variables and other - package used: cleaninginspectoR
issues <- inspect_all(data, "data_uuid")

issue_table <- issues

issue_table <- issue_table[!grepl("'other' response. may need recoding.", issue_table$issue_type),]
issue_table <- issue_table[!grepl("Potentially sensitive information.", issue_table$issue_type),]

if(nrow(issue_table)>=1) {
  
  issue_table <- issue_table %>% 
    mutate(uuid=data[.$index,"uuid",drop=TRUE], ngo=data[.$index,"g_enum_agency"], area = data[.$index, "g_sub_district", drop = TRUE])
  
  issue_table$new_vaue <- " "
  issue_table$fix <- "Checked with partner"
  issue_table$checked_by <- "NG"
  
  issue_log <- data.frame(uuid = issue_table$data_uuid, 
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




### Second check: soft constraints
#### 1. The KI reported issues accessing to water but no accessing issues were highlighted
water_checks <- data %>% select(c("data_uuid", "g_enum_agency", "g_sub_district", "w_waterneeds", "w_wateraccess")) %>% 
                mutate(water_check = ifelse(((data$w_wateraccess == "no") & (data$w_waterneeds == "none" | data$w_waterneeds == "few")),1,0)) %>%
                filter(water_check == 1)


if(nrow(water_checks)>=1) {
  
  water_checks$issue_type <- "The community KI reported household having issues with water but no accessing issues were highlighted."
  water_checks$checked_by <- "NG"
  water_checks$new_value <- " "
  water_checks$fix <- "Checked with partner"
  water_checks$variable <- "w_waterneeds"
  
  
  water_log <- data.frame(uuid = water_checks$data_uuid, 
                           agency = water_checks$g_enum_agency, 
                           area = water_checks$g_sub_district, 
                           variable = water_checks$variable,
                           issue = water_checks$issue_type, 
                           old_value = water_checks$w_waterneeds, 
                           new_value = water_checks$new_value, 
                           fix = water_checks$fix, 
                           checked_by = water_checks$checked_by)
  
  
} else {
  
  water_log <- data.frame(uuid = as.character(),
                           agency = as.character(),
                           area = as.character(),
                           variable = as.character(),
                           issue = as.character(),
                           old_value = as.character(),
                           new_value = as.character(),
                           fix = as.character(),
                           checked_by = as.character()) 
  
  print("No issues realted to access to water. The dataset seems clean.")
  
  }

#### 2. The KI reported household not having issues with water and having functional water facility but not enough soap
sanitation_checks <- data %>% select(c("data_uuid", "g_enum_agency", "g_sub_district", "w_waterneeds", "h_handwashing", "h_have_soap")) %>% 
  mutate(sanitation_check = ifelse(((data$w_waterneeds == "none" | data$w_waterneeds == "few" | data$w_waterneeds == "half") & 
                                 (data$h_handwashing == "most" | data$h_handwashing == "everyone") &
                                 (data$h_have_soap == "none" | data$h_have_soap == "few")),1,0)) %>%
                                  filter(sanitation_check == 1)


if(nrow(sanitation_checks)>=1) {
  
  sanitation_checks$issue_type <- "The community KI reported household  not having issues with water nor with sanitation facilities but not having soap."
  sanitation_checks$checked_by <- "NG"
  sanitation_checks$new_value <- " "
  sanitation_checks$fix <- "Checked with partner"
  sanitation_checks$variable <- "w_waterneeds"
  
  
  sanitation_log <- data.frame(uuid = sanitation_checks$data_uuid, 
                           agency = sanitation_checks$g_enum_agency, 
                           area = sanitation_checks$g_sub_district, 
                           variable = sanitation_checks$variable,
                           issue = sanitation_checks$issue_type, 
                           old_value = sanitation_checks$w_waterneeds, 
                           new_value = sanitation_checks$new_value, 
                           fix = sanitation_checks$fix, 
                           checked_by = sanitation_checks$checked_by)
  
  
} else {
  
  sanitation_log <- data.frame(uuid = as.character(),
                           agency = as.character(),
                           area = as.character(),
                           variable = as.character(),
                           issue = as.character(),
                           old_value = as.character(),
                           new_value = as.character(),
                           fix = as.character(),
                           checked_by = as.character()) 
  
  print("No issues realted to sanitation facilities. The dataset seems clean.")}


#### 3. Check if people who reported having issues accessing soap they also report particular constraints
soap_checks <- data %>% select(c("data_uuid", "g_enum_agency", "g_sub_district", "h_have_soap", "h_soapaccess")) %>% 
  mutate(soap_check = ifelse(((data$h_soapaccess == "no") & (data$h_have_soap == "none" | data$h_have_soap == "few")),1,0)) %>%
                                filter(soap_check == 1)


if(nrow(soap_checks)>=1) {
  
  soap_checks$issue_type <- "The community KI reported households having issues with accessing soap but no access constraints were reported."
  soap_checks$checked_by <- "NG"
  soap_checks$new_value <- " "
  soap_checks$fix <- "Checked with partner"
  soap_checks$variable <- "h_soapaccess"
  
  
  soap_log <- data.frame(uuid = soap_checks$data_uuid, 
                               agency = soap_checks$g_enum_agency, 
                               area = soap_checks$g_sub_district, 
                               variable = soap_checks$variable,
                               issue = soap_checks$issue_type, 
                               old_value = soap_checks$h_soapaccess, 
                               new_value = soap_checks$new_value, 
                               fix = soap_checks$fix, 
                               checked_by = soap_checks$checked_by)
  
  
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

#### 4. Check if people reported didn't have any issues accessing soap but soap was not available in the community in the past 30 days
soap2_checks <- data %>% select(c("data_uuid", "g_enum_agency", "g_sub_district", "h_soapaccess", "h_barsoap")) %>% 
                mutate(soap2_check = ifelse(((data$h_soap_problem == "no") & (data$h_barsoap == "not_accessible")),1,0)) %>%
                filter(soap2_check == 1)

if(nrow(soap2_checks)>=1) {
  
  soap2_checks$issue_type <- "The community KI reported households not having issues accessing soap but soap was reported to innaccessible during the past 30-days."
  soap2_checks$checked_by <- "NG"
  soap2_checks$new_value <- " "
  soap2_checks$fix <- "Checked with partner"
  soap2_checks$variable <- "h_barsoap"
  
  
  soap2_log <- data.frame(uuid = soap2_checks$data_uuid, 
                         agency = soap2_checks$g_enum_agency, 
                         area = soap2_checks$g_sub_district, 
                         variable = soap2_checks$variable,
                         issue = soap2_checks$issue_type, 
                         old_value = soap2_checks$h_barsoap, 
                         new_value = soap2_checks$new_value, 
                         fix = soap2_checks$fix, 
                         checked_by = soap2_checks$checked_by)
  
  
} else {
  
  soap2_log <- data.frame(uuid = as.character(),
                         agency = as.character(),
                         area = as.character(),
                         variable = as.character(),
                         issue = as.character(),
                         old_value = as.character(),
                         new_value = as.character(),
                         fix = as.character(),
                         checked_by = as.character()) 
  
  print("No issues realted to availability of soap. The dataset seems clean.")}


#### 5. Check if people reported having enough soap in the community but soap wasn't available during the past 30-days
soap3_checks <- data %>% select(c("data_uuid", "g_enum_agency", "g_sub_district", "h_have_soap", "h_barsoap")) %>% 
                mutate(soap3_check = ifelse(((data$h_soapaccess == "half" | data$h_soapaccess == "everyone") & (data$h_barsoap == "not_accessible")),1,0)) %>%
                filter(soap3_check == 1)

if(nrow(soap3_checks)>=1) {
  
  soap3_checks$issue_type <- "The community KI reported the majority of the households having soap but soap wasn't available ni the past 30-days."
  soap3_checks$checked_by <- "NG"
  soap3_checks$new_value <- " "
  soap3_checks$fix <- "Checked with partner"
  soap3_checks$variable <- "h_have_soap"
  
  
  soap3_log <- data.frame(uuid = soap3_checks$data_uuid, 
                          agency = soap3_checks$g_enum_agency, 
                          area = soap3_checks$g_sub_district, 
                          variable = soap3_checks$variable,
                          issue = soap3_checks$issue_type, 
                          old_value = soap3_checks$h_have_soap, 
                          new_value = soap3_checks$new_value, 
                          fix = soap3_checks$fix, 
                          checked_by = soap3_checks$checked_by)
  
  
} else {
  
  soap3_log <- data.frame(uuid = as.character(),
                          agency = as.character(),
                          area = as.character(),
                          variable = as.character(),
                          issue = as.character(),
                          old_value = as.character(),
                          new_value = as.character(),
                          fix = as.character(),
                          checked_by = as.character()) 
  
  print("No issues realted to accessability of soap. The dataset seems clean.")
  }

#### 6. Check if people reported waste and trash frequently visible but gargabe is supposed to be collected frequently
garbage_checks <- data %>% select(c("data_uuid", "g_enum_agency", "g_sub_district", "s_visibletrash", "s_trashcollected")) %>% 
                mutate(garbage_check = ifelse(((data$s_visibletrash == "most" | data$s_visibletrash == "everyone") & 
                                           (data$s_trashcollected == "every_Day" | data$s_trashcollected == "once_week")),1,0)) %>%
                filter(garbage_check == 1)


if(nrow(garbage_checks)>=1) {
  
  garbage_checks$issue_type <- "The community KI reported the trash to be visible in the street but waste collection should happen frequently"
  garbage_checks$checked_by <- "NG"
  garbage_checks$new_value <- " "
  garbage_checks$fix <- "Checked with partner"
  garbage_checks$variable <- "s_visibletrash"
  
  
  garbage_log <- data.frame(uuid = garbage_checks$data_uuid, 
                          agency = garbage_checks$g_enum_agency, 
                          area = garbage_check$g_sub_district, 
                          variable = garbage_checks$variable,
                          issue = garbage_checks$issue_type, 
                          old_value = garbage_checks$s_visibletrash, 
                          new_value = garbege_checks$new_value, 
                          fix = garbage_checks$fix, 
                          checked_by = garbage_checks$checked_by)
  
  
} else {
  
  garbage_log <- data.frame(uuid = as.character(),
                          agency = as.character(),
                          area = as.character(),
                          variable = as.character(),
                          issue = as.character(),
                          old_value = as.character(),
                          new_value = as.character(),
                          fix = as.character(),
                          checked_by = as.character()) 
  
  print("No issues realted to accessability of soap. The dataset seems clean.")
}

### Check times
time_stamp <- select(data, "data_uuid", "start", "end", "g_enum_agency", "g_sub_district")
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

shortest_path <- data %>% select("data_uuid", "g_enum_agency", "g_sub_district", "CountNa")
shortest_path <- shortest_path %>% filter(CountNa > 110)


if(nrow(shortest_path)>=1) {
  
  shortest_path$issue_type <- "The majority of entries are NAs"
  shortest_path$checked_by <- "NG"
  shortest_path$new_value <- " "
  shortest_path$fix <- "Checked with partner"
  shortest_path$variable <- "Count of all variables"
  
  
  shortest_path_log <- data.frame(uuid = shortest_path$data_uuid, 
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




### Final steps
#### Save dataset for cleaning
cleaning_log <- plyr::rbind.fill(issue_log,
                                 water_log,
                                 sanitation_log,
                                 soap_log,
                                 soap2_log,
                                 soap3_log,
                                 garbage_log,
                                 shortest_path_log
                                 )


final_log <- list("cleaning_log" = cleaning_log,
                  "Timestamp checks"= check_time_log)

write.xlsx(final_log, paste0("./output/WASH_WANTS Common_cleaning log_",current_date,".xlsx"))
browseURL(paste0("./output/WASH_WANTS Common_cleaning log_",current_date,".xlsx"))

