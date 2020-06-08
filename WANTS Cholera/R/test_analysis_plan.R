### Test using data analysis plan

rm(list=ls())

## Load required library
library("tidyverse")
library("hypegrammaR")
library("xlsformfill")
library("openxlsx")

### Load source
source("./R/moveme.R")
source("./R/remove_minmax.R")
source("./R/multiple_response.R")
source("./R/cleanHead.R")
source("./R/md_to_custom_table.R")


### Add some fake data
questions <- read.csv("./data/questions.csv", stringsAsFactors = F, check.names = F)
choices <- read.csv('./data/choices_v2.csv', stringsAsFactors = F, check.names = F)
choices_ext <- read.csv("./data/external_choices.csv", stringsAsFactors = F, check.names = F)

response <- xlsform_fill(questions,
                         choices,
                         1000)

response_locations <- xlsform_fill(questions,
                                   choices_ext,
                                   1000)



response$g_district <- response_locations$g_district
response$g_sub_district <- response_locations$g_sub_district
response$g_location <- response_locations$g_location

names(response) <- gsub("/", ".", names(response))

### Create a fake sampling frame
sampling_frame <- xlsform_generate_samplingframe(choices, "district")

### Load questionnaire
questionnaire <- load_questionnaire(response,
                                    questions,
                                    choices,
                                    choices.label.column.to.use = "label::english")

### Analysis plan
analysis_plan <- read.csv("./data/analysis_plan.csv", 
                          stringsAsFactors=F, check.names=F)

### Response analysis
response_analysis_output <- from_analysisplan_map_to_output(data = response,
                                                            #weighting = weight.function,
                                                            analysisplan = analysis_plan,
                                                            questionnaire = questionnaire,
                                                            labeled = T,
                                                            verbose = T)
