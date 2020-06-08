# WASH Common Tool - Data Cleaning script
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V1
# 02/06/2020

rm(list=ls())
today <- Sys.Date()

## Download necessary packages
# devtools::install_github("mabafaba/clog", force = T)
# devtools::install_github("agualtieri/cleaninginspectoR", force = T)


## Install packages
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("openxlsx")

## Load libraries
require(tidyverse)
require(dataqualitycontrol)
require(cleaninginspectoR)
require(data.table)
require(openxlsx)
require(reshape2)
require(clog)

# browseVignettes("clog")

## Source
source("./R/add_locations.R")
source("./R/moveme.R")

## Upload data to be cleaned and fix some header names - the path may need to be updated based on where you stored your files
response <- read.xlsx("./data/REACH_YEM_Common KI Kobo_WANTS_Test.xlsx")

names(response)[names(response) == "_index"] <- "index"
names(response)[names(response) == "_uuid"] <- "uuid"

## Upload updated cleaning log file - - the path may need to be updated based on where you stored your files
cleaning_log <- read.xlsx("./output/WASH_Common Tool_Master Cleaning Log.xlsx")


## Apply cleaning log
my_log <- cleaninglog(ids = cleaning_log$uuid,
                      variables = cleaning_log$variable,
                      new_values = cleaning_log$new_value,
                      name = cleaning_log$fix,
                      change = cleaning_log$change,
                      data_id_column_name = "uuid")

clean_data <- clog_clean(response, my_log)

## Save cleaned data
### DB Version
write.xlsx(clean_data, paste0("./output/db/WASH_Common Tool Final DB_",today,".xlsx"))

### Final
final <- clean_data %>% anonymise_dataset(c("start", "end", "today", "g_enum_last_name", "g_enum_name", "g_enum_agency", "g_enum_agency_other",
                                            "x_focalpoint_code", "X_id", "X_submission_time", "X_validation_status"))

final <- add.location(final)

final <- final[moveme(names(final), "country_name after date_survey; country_id after country_name; a1_governorate_name after country_id; a2_district_name after g_governorate;
               a3_sub_district_name after g_district")]

write.xlsx(final, paste0("./output/final/WASH_Common Tool Final_",today,".xlsx"))

