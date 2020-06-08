### WASH WANTS Assessment Data Analysis Script - Cholera Tool
### REACH Yemen
### V2
### 03/06/2020

rm(list=ls())

### Download custom packages
# devtools::install_github("ellieallien/hypegrammaR", force = T)
# devtools::install_github("mabafaba/composR", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/xlsformfill", force = T, build_vignettes = T)

### Load required library
library("tidyverse")
library("hypegrammaR")
library("xlsformfill")
library("openxlsx")
library("lubridate")
library("zoo")

### Load source
source("./R/moveme.R")
source("./R/remove_minmax.R")
source("./R/functions/functions.R")
source("./R/functions/from_hyperanalysis_to_datamerge.R")


### Change date of data analysis
current_date <- Sys.Date()

### Load data
response <- read.xlsx("./data/WASH Cholera Key Informant Questionnaire_test.xlsx", sheet = "sheet 1")

names(response)[names(response) == "_index"] <- "index"
names(response)[names(response) == "_uuid"] <- "uuid"


### Load questionnaire
questions <- read.csv("./data/questions.csv", stringsAsFactors = F, check.names = F)
choices <- read.csv("./data/choices.csv", stringsAsFactors = F, check.names = F)
external_choices <- read.csv("./data/external_choices.csv", stringsAsFactors = F, check.names = F)

questionnaire <- load_questionnaire(response,
                                    questions,
                                    choices,
                                    choices.label.column.to.use = "label::english")



### Load Data Analysis Plan
dap <- load_analysisplan("./analysis/data/dap/wash_cholera_analysis_district.csv")

### Create extra variables
### The DAP asks for the latest date of distribution but when aggregating this type of information is invaluable as distribution happens per cummunity.
### The analysis below will answer the question "When the majority of distrubtion happened?" it does so by taking the mode by group and selecting the top 1.


### Latest chlorine, jerry cans, and soap distribution
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

### Chlorine distribution
chlo_date <- select(response, c("uuid", "g_district", "w_chlorineyear", "w_chlorinemonth"))
chlo_date <- chlo_date %>% mutate(chlo_date = as.yearmon(paste(w_chlorinemonth, w_chlorineyear)))

chlo_mode <- aggregate(chlo_date$chlo_date, list(g_district = chlo_date$g_district, most_date_chlo = chlo_date$chlo_date), getmode)
chlo_mode <- chlo_mode %>% group_by(g_district) %>% top_n(1, x)
chlo_mode$x <- NULL


### Soap distribution
soap_date <- select(response, c("uuid", "g_district", "h_soapyear", "h_soapmonth"))
soap_date <- soap_date %>% mutate(soap_date = as.yearmon(paste(h_soapmonth, h_soapyear)))

soap_mode <- aggregate(soap_date$soap_date, list(g_district = soap_date$g_district, most_date_soap = soap_date$soap_date), getmode)
soap_mode <- soap_mode %>% group_by(g_district) %>% top_n(1, x)
soap_mode$x <- NULL

### Jerry can distribution
jerry_date <- select(response, c("uuid", "g_district", "w_jerrycanyear", "w_jerrycanmonth"))
jerry_date <- jerry_date %>% mutate(jerry_date = as.yearmon(paste(w_jerrycanmonth, w_jerrycanyear)))

jerry_mode <- aggregate(jerry_date$jerry_date, list(g_district = jerry_date$g_district, most_date_jerry = jerry_date$jerry_date), getmode)
jerry_mode <- jerry_mode %>% group_by(g_district) %>% top_n(1, x)
jerry_mode$x <- NULL


# Ranking of treating method
# The code below takes the aggregate mean for each filtering option
filter_rank <- select(response, c("uuid", "g_district", "w_treatrank_stand", "w_treatrank_boiling", "w_treatrank_sun", "w_treatrank_chlorine", "w_treatrank_filter"))


filter_rank_final <- aggregate(filter_rank[,3:7], list(g_district = filter_rank$g_district), mean, na.rm=TRUE)

#### Join everything
external_analysis <- list(chlo_mode, soap_mode, jerry_mode, filter_rank_final) %>% reduce(left_join, by = "g_district")

### Label full dataset before running the analysis
#response_ren <- response

#response_ren <- response_ren[moveme(names(response_ren), "uuid first")]
#response_ren[15:131] <- choices$`label::english`[match(unlist(response_ren[15:131]), choices$name)]




### Launch Analysis Script
analysis <- from_analysisplan_map_to_output(data = response,
                                            analysisplan = dap,
                                            weighting = NULL,
                                            questionnaire = questionnaire,
                                            labeled = TRUE)

## SUMMARY STATS LIST ##
summary.stats.list <- analysis$results


## SUMMARY STATS LIST FORMATTED 
summarystats <- summary.stats.list %>%
  #lapply((map_to_labeled),questionnaire) %>% 
  resultlist_summary_statistics_as_one_table

write.csv(summarystats, paste0("./output/summarystats_final_",current_date,".csv"))



### Load the results and lunch data merge function
final_analysis <- read.csv(paste0("./output/summarystats_final_",current_date,".csv"), stringsAsFactors = F)

final_melted_analysis <- from_hyperanalysis_to_datamerge(final_analysis)

#### Multiply everything by 100, round everything up, and replace NAs with 0
final_dm <- cbind(final_melted_analysis[1], sapply(final_melted_analysis[-1],function(x) x*100))

final_dm[,-1] <- round(final_dm[,-1],0)

final_dm[is.na(final_dm)] <- 0

## Divide the only numeric value by 100 to bring it back to the correct value
final_dm$g_population.NA <- final_dm$g_population.NA/100


### Join indicators not analyzed by hypegrammaR
names(final_dm)[names(final_dm) == "independent.var.value"] <- "g_district"
data_merge <- left_join(final_dm, external_analysis, by = "g_district")

#write.xlsx(data_merge, paste0("./output/governorate_data_merge_",today,".xlsx"))
#browseURL(paste0("./output/governorate_data_merge_",today,".xlsx"))


### Add maps to the final data merge and save it as .csv file
#data_merge$`@map` <- paste0("./analysis/maps/YEM_CCCM_",data_merge$governorate,".pdf")

#names(data_merge) <- tolower(names(data_merge))

write.csv(data_merge, paste0("./output/governorate_data_merge_",current_date,".csv"), row.names = F)                    
browseURL(paste0("./analysis/output/cccm_governorate_full_merge_",current_date,".csv"))


