### WASH WANTS Assessment Data Analysis Script - Common Tool
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
response <- read.xlsx("./data/REACH_YEM_Common KI Kobo_WANTS_Test.xlsx", sheet = "Sheet1")

names(response)[names(response) == "_index"] <- "index"
names(response)[names(response) == "_uuid"] <- "uuid"


### Load questionnaire
questions <- read.csv("./data/questions.csv", check.names = F)
choices <- read.csv("./data/choices.csv", check.names = F)
external_choices <- read.csv("./data/external_choices.csv", check.names = F)

questionnaire <- load_questionnaire(response,
                                    questions,
                                    choices,
                                    choices.label.column.to.use = "english")


### Load Data Analysis Plan
dap <- load_analysisplan("./analysis/data/dap/wash_common_analysis_plan.csv")


### Create extra variables
### The DAP asks for the latest date of distribution but when aggregating this type of information is invaluable as distribution happens per cummunity.
### The analysis below will answer the question "When the majority of distrubtion happened?" it does so by taking the mode by group and selecting the top 1.


# Ranking of treating method
# The code below takes the aggregate mean for each filtering option
filter_rank <- select(response, c("uuid", "g_district", "w_treatrank_stand", "w_treatrank_boiling", "w_treatrank_sun", "w_treatrank_chlorine", "w_treatrank_filter"))


filter_rank_final <- aggregate(filter_rank[,3:7], list(g_district = filter_rank$g_district), mean, na.rm=TRUE)

#### Join everything
#external_analysis <- list(chlo_mode, soap_mode, jerry_mode, filter_rank_final) %>% reduce(left_join, by = "g_district")

### Label full dataset before running the analysis
#response_ren <- response

#response_ren <- response_ren[moveme(names(response_ren), "uuid first")]
#response_ren[15:131] <- choices$`label::english`[match(unlist(response_ren[15:131]), choices$name)]




### Launch Analysis Script
analysis <- from_analysisplan_map_to_output(data = response,
                                            analysisplan = dap,
                                            weighting = NULL,
                                            questionnaire = questionnaire)
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
final_dm$g_orgaid.NA <- final_dm$g_orgaid.NA/100


### Join indicators not analyzed by hypegrammaR
names(final_dm)[names(final_dm) == "independent.var.value"] <- "g_district"
data_merge <- left_join(final_dm, filter_rank_final, by = "g_district")

#write.xlsx(data_merge, paste0("./output/governorate_data_merge_",today,".xlsx"))
#browseURL(paste0("./output/governorate_data_merge_",today,".xlsx"))


### Add maps to the final data merge and save it as .csv file
#data_merge$`@map` <- paste0("./analysis/maps/YEM_CCCM_",data_merge$governorate,".pdf")

#names(data_merge) <- tolower(names(data_merge))

write.csv(data_merge, paste0("./output/governorate_data_merge_",current_date,".csv"), row.names = F)                     
browseURL(paste0("./analysis/output/cccm_governorate_full_merge_",current_date,".csv"))


