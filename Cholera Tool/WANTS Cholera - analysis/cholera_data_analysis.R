### WASH WANTS Assessment Data Analysis Script - Cholera Tool
### REACH Yemen
### V1
### 10/09/2019

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

### Load source
source("./R/moveme.R")
source("./R/remove_minmax.R")


### Change date of data analysis
current_date <- Sys.Date()

### Load cleaned data, questionnaire, and choices sheet
# response_old <- read.csv("./data/V2/WANTS_Common_Tool_2019-09-03_cleaned.csv", stringsAsFactors = F)
# response_loops <- read.csv("./data/WANTS_Common_Tool_2019-09-03_cleaned_loops.csv", stringsAsFactors = F)
# questions <- read.csv("./data/questions.csv", stringsAsFactors = F)
# choices <- read.csv("./data/choices.csv", stringsAsFactors = F)

### Add some fake data
response <- xlsform_fill(questions = read.csv("./data/questions.csv"),
                         choices = read.csv('./data/choices.csv'),
                         3000)

response_locations <- xlsform_fill(questions = read.csv("./data/questions.csv"),
                                   choices= read.csv("./data/external_choices.csv"),
                                   3000)



response$g_district <- response_locations$g_district
response$g_sub_district <- response_locations$g_sub_district
response$g_location <- response_locations$g_location

names(response) <- gsub("/", ".", names(response))

write.csv(response, paste0("./output/WASH_cholera tool_dummy data_",current_date,".csv"))

response <- filter(response, response$g_governorate == substr(response$g_district, start = 1, stop = 4))



questionnaire <- load_questionnaire(response,
                                    questions = "./data/questions.csv",
                                    choices = "./data/choices.csv",
                                    choices.label.column.to.use = "label..english")


### Analysis Demographics
#### RQ 1: What is the demographic of the community

##### Type of setting
setting_type <- response %>% map_to_result("g_urbanrural",
                                           "g_governorate", 
                                            #questionnaire,
                                            map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

setting_type_table <- map_to_table(setting_type)
map_to_file(setting_type_table, paste0("./output/demographics/setting_type_",current_date,".csv"))

##### Status of community residents
community_residents <- response %>% map_to_result("g_resident",
                                                  "g_governorate",
                                                  map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

community_residents_table <- map_to_table(community_residents)
map_to_file(community_residents_table, paste0("./output/demographics/community_residents_",current_date,".csv"))

##### Population size
pop_size <- response %>% map_to_result("g_population",
                                       "g_governorate",
                                       map_to_case("group_difference", "numerical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

pop_size_table <- map_to_table(pop_size)
map_to_file(pop_size_table, paste0("./output/demographics/pop_size_",current_date,".csv"))


##### KI Position - needs recoding

##### Assistance provided in the community
assistance <- response %>% map_to_result("g_assistance",
                                         "g_governorate",
                                         map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

assistance_table <- map_to_table(assistance)
map_to_file(assistance_table, paste0("./output/demographics/assistance_",current_date,".csv"))


##### Number of organization involved in providing aid
n_org <- response %>% map_to_result("g_orgaid",
                                    "g_governorate",
                                     map_to_case("group_difference", "numerical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

n_org_table <- map_to_table(n_org)
map_to_file(n_org_table, paste0("./output/demographics/n_org_",current_date,".csv"))

#### Type of org involved in providing aid
org_type <- response %>% map_to_result("g_orgtype",
                                    "g_governorate",
                                    map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

org_type_table <- map_to_table(org_type)
map_to_file(org_type_table, paste0("./output/demographics/org_type_",current_date,".csv"))

#### Type of aid provided by org
org_aid <- response %>% map_to_result("g_org_aid",
                                       "g_governorate",
                                       map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()


org_aid_table <- map_to_table(org_aid)
map_to_file(org_aid_table, paste0("./output/demographics/org_aid_",current_date,".csv"))

### Analysis Water
#### RQ1: What is the proportion of communities accessing Improved Water Sources?
drink_source <- response %>% map_to_result("w_drinksource",
                                      "g_governorate",
                                      map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

drink_source_table <- map_to_table(drink_source)
map_to_file(drink_source_table, paste0("./output/water/org_aid_",current_date,".csv"))

#### RQ2: What are issues with drinking water that communities experience?
water_appear <- response %>% map_to_result("w_waterappear",
                                           "g_governorate",
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

water_appear_table <- map_to_table(water_appear)
map_to_file(water_appear_table, paste0("./output/water/water_appear_",current_date,".csv"))

water_smell <- response %>% map_to_result("w_watersmell",
                                           "g_governorate",
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

water_smell_table <- map_to_table(water_smell)
map_to_file(water_smell_table, paste0("./output/water/water_smell_",current_date,".csv"))

water_needs <- response %>% map_to_result("w_waterneeds",
                                          "g_governorate",
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

water_needs_table <- map_to_table(water_needs)
map_to_file(water_needs_table, paste0("./output/water/water_needs_",current_date,".csv"))

water_access_yesno <- response %>% map_to_result("w_wateraccess",
                                          "g_governorate",
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

water_access_yesno_table <- map_to_table(water_access_yesno)
map_to_file(water_access_yesno_table, paste0("./output/water/water_access_yesno_",current_date,".csv"))

water_access_issues <- response %>% map_to_result("w_accessproblem",
                                                 "g_governorate",
                                                 map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

water_access_issues_table <- map_to_table(water_access_issues)
map_to_file(water_access_issues_table, paste0("./output/water/water_access_issues_",current_date,".csv"))

#### RQ3: What are measures undertaken at household level to treat/make drinking water safe? 
treat_water <- response %>% map_to_result("w_treatwater",
                                           "g_governorate",
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

treat_water_table <- map_to_table(treat_water)
map_to_file(treat_water_table, paste0("./output/water/treat_water_",current_date,".csv"))

treat_method <- response %>% map_to_result("w_treatmethod",
                                          "g_governorate",
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

treat_method_table <- map_to_table(treat_method)
map_to_file(treat_method_table, paste0("./output/water/treat_method_",current_date,".csv"))

##### Composing the ranking
treating_methods_ranks <- response %>% group_by(g_governorate) %>% summarise_at(vars(starts_with("w_treatrank")), funs(mean(., na.rm = T)))
write.csv(treating_methods_ranks, paste0("./output/water/treating_methods_sub_",current_date,".csv"), row.names = F)

treatmethods_ranks_overall <- response %>% summarise_at(vars(starts_with("w_treatrank")), funs(mean(., na.rm = T)))
write.csv(treatmethods_ranks_overall, paste0("./output/water/treating_methods_over_",current_date,".csv"), row.names = F)

#####

who_treats <- response %>% map_to_result("w_treatwater_who",
                                        "g_governorate",
                                        map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

who_treats_table <- map_to_table(who_treats)
map_to_file(who_treats_table, paste0("./output/water/who_treat_",current_date,".csv"))


whynotreat <- response %>% map_to_result("w_whynot_treat",
                                         "g_governorate",
                                         map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

whynotreat_table <- map_to_table(whynotreat)
map_to_file(whynotreat_table, paste0("./output/water/whynotreat_",current_date,".csv"))


chlorine_dist <- response %>% map_to_result("w_chlorinedistribution",
                                            "g_governorate",
                                             map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

chlorine_dist_table <- map_to_table(chlorine_dist)
map_to_file(chlorine_dist_table, paste0("./output/water/chlorine_dist_",current_date,".csv"))

jerry_dist <- response %>% map_to_result("w_jerrycandistribution",
                                         "g_governorate",
                                         map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

jerry_dist_table <- map_to_table(jerry_dist)
map_to_file(jerry_dist_table, paste0("./output/water/jerry_dist_",current_date,".csv"))

jerry_own <- response %>% map_to_result("w_jerrycan",
                                         "g_governorate",
                                         map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

jerry_own_table <- map_to_table(jerry_own)
map_to_file(jerry_own_table, paste0("./output/water/jerry_own_",current_date,".csv"))

##### Chlorine and Jerrycans distribution TBD

### Analysis Health
#### RQ1: What proportion of communities has adequate access to soap?
have_soap <- response %>% map_to_result("h_have_soap",
                                        "g_governorate",
                                        map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

have_soap_table <- map_to_table(have_soap)
map_to_file(have_soap_table, paste0("./output/health/have_soap_",current_date,".csv"))

soap_access <- response %>% map_to_result("h_soapaccess",
                                          "g_governorate",
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

soap_access_table <- map_to_table(soap_access)
map_to_file(soap_access_table, paste0("./output/health/soap_access_",current_date,".csv"))

soap_problem <- response %>% map_to_result("h_soap_problem",
                                          "g_governorate",
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

soap_problem_table <- map_to_table(soap_problem)
map_to_file(soap_problem_table, paste0("./output/health/soap_problem_",current_date,".csv"))


soap_distribution <- response %>% map_to_result("h_soapdistribution",
                                                "g_governorate",
                                                map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

soap_distribution_table <- map_to_table(soap_distribution)
map_to_file(soap_distribution_table, paste0("./output/health/soap_distribution_",current_date,".csv"))

campaign <- response %>% map_to_result("h_campaign",
                                                "g_governorate",
                                                map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

campaign_table <- map_to_table(campaign)
map_to_file(campaign_table, paste0("./output/health/campaign_",current_date,".csv"))

staff <- response %>% map_to_result("h_staff",
                                    "g_governorate",
                                    map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

staff_table <- map_to_table(staff)
map_to_file(staff_table, paste0("./output/health/staff_",current_date,".csv"))


#### Soap distribution dates TBD

### Analysis Sanitation
#### RQ1: What proportion of communities has access to functioning latrines?
latrine_access <- response %>% map_to_result("s_latrineaccess",
                                    "g_governorate",
                                    map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

latrine_access_table <- map_to_table(latrine_access)
map_to_file(latrine_access_table, paste0("./output/sanitation/latrine_access_",current_date,".csv"))

latrine_type <- response %>% map_to_result("s_latrinetype",
                                             "g_governorate",
                                             map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

latrine_type_table <- map_to_table(latrine_type)
map_to_file(latrine_type_table, paste0("./output/sanitation/latrine_type_",current_date,".csv"))

latrine_issues <- response %>% map_to_result("s_latrineissues",
                                           "g_governorate",
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

latrine_issues_table <- map_to_table(latrine_issues)
map_to_file(latrine_issues_table, paste0("./output/sanitation/latrine_issues_",current_date,".csv"))

accessgroups_yesno <- response %>% map_to_result("s_accessgroupyesno",
                                             "g_governorate",
                                             map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

accessgroups_yesno_table <- map_to_table(accessgroups_yesno)
map_to_file(accessgroups_yesno_table, paste0("./output/sanitation/accessgroups_yesno_",current_date,".csv"))

accessgroups <- response %>% map_to_result("s_accessgroup",
                                           "g_governorate",
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

accessgroups_table <- map_to_table(accessgroups)
map_to_file(accessgroups_table, paste0("./output/sanitation/accessgroups_",current_date,".csv"))

#### RQ2: What proportion of communities face severe environmental hygiene problems?
garbage_collection <- response %>% map_to_result("s_garbage_collection",
                                                 "g_governorate",
                                                  map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

garbage_collection_table <- map_to_table(garbage_collection)
map_to_file(garbage_collection_table, paste0("./output/sanitation/garbage_collection_",current_date,".csv"))

garbage_responsible <- response %>% map_to_result("s_garbage_responsible",
                                                 "g_governorate",
                                                 map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

garbage_responsible_table <- map_to_table(garbage_responsible)
map_to_file(garbage_responsible_table, paste0("./output/sanitation/garbage_responsible_",current_date,".csv"))

garbage_disposal <- response %>% map_to_result("s_garbage_disposal",
                                                  "g_governorate",
                                                  map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

garbage_disposal_table <- map_to_table(garbage_disposal)
map_to_file(garbage_disposal_table, paste0("./output/sanitation/garbage_disposal_",current_date,".csv"))

#### RQ3: Do people have knowledge on cholera, and how to treat it?
ors <- response %>% map_to_result("s_ors",
                                               "g_governorate",
                                               map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

ors_table <- map_to_table(ors)
map_to_file(ors_table, paste0("./output/sanitation/ors_",current_date,".csv"))

ors_who <- response %>% map_to_result("s_ors_who",
                                  "g_governorate",
                                  map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

ors_who_table <- map_to_table(ors_who)
map_to_file(ors_who_table, paste0("./output/sanitation/ors_who_",current_date,".csv"))

orc <- response %>% map_to_result("s_orc",
                                  "g_governorate",
                                  map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(questionnaire) %>% remove_minmax()

orc_table <- map_to_table(orc)
map_to_file(orc_table, paste0("./output/sanitation/orc_",current_date,".csv"))

### Save final analysis - multiple sheets
demographics_list <- list("Setting type" = setting_type_table, "Population size" = pop_size_table, "Type of residents" = community_residents_table,  
                          "Assistance provided" = assistance_table, "Number of NGOs" = n_org_table, "Type of aid provided" = org_aid_table)

write.xlsx(demographics_list, paste0("./output/WASH_WANTS_cholera tool_",current_date,"_demographics.xlsx"), asTable = T)

water_list <- list("Drinking water source" = drink_source_table, "Water appearance" = water_appear_table, "Water smell" = water_smell_table,
                   "Water treated" = treat_water_table, "Water treatement method" = treat_method_table, "Treating method ranking" = treatmethods_ranks_overall,
                   "Who treats water" = who_treats_table, "Why water is not treated" = whynotreat_table, "Chlorine distribution" = chlorine_dist_table, 
                   "Community has jerry cans" = jerry_own_table, "Jerry cans distributed" = jerry_dist_table)
  
write.xlsx(water_list, paste0("./output/WASH_WANTS_cholera tool_",current_date,"_water.xlsx"), asTable = T)

health_list <- list("Community has soap" = have_soap_table, "Community has access to soap" = soap_access_table, "Issues with accessing soap" = soap_problem_table, "Soap distribution" = soap_distribution_table,
                    "Higyiene campaing in the community" = campaign_table, "Trained staff" = staff_table)

write.xlsx(health_list, paste0("./output/WASH_WANTS_cholera tool_",current_date,"_health.xlsx"), asTable = T)

sanitation_list <- list("Access to latrines" = latrine_access_table, "Type of latrines" = latrine_type_table, "Issues with latrine" = latrine_issues_table,
                        "Have vulnerable groups access to latrines" = accessgroups_yesno_table, "Type of group with no access" = accessgroups_table,
                        "Garbage collection" = garbage_collection_table, "Who is responsible for garbage" = garbage_responsible_table, "Garbage disposal" = garbage_disposal_table,
                        "ORS" = ors_table,  "Who is able to prepare ORS" = ors_who_table, "ORC" = orc_table)

write.xlsx(sanitation_list, paste0("./output/WASH_WANTS_cholera tool_",current_date,"_sanitation.xlsx"), asTable = T)

#### BigASS file
bigASSlist <- list("Setting type" = setting_type_table, "Population size" = pop_size_table, "Type of residents" = community_residents_table,  
                    "Assistance provided" = assistance_table, "Number of NGOs" = n_org_table, "Type of aid provided" = org_aid_table,
                    "Drinking water source" = drink_source_table, "Water appearance" = water_appear_table, "Water smell" = water_smell_table,
                    "Water treated" = treat_water_table, "Water treatement method" = treat_method_table, "Treating method ranking" = treatmethods_ranks_overall,
                    "Who treats water" = who_treats_table, "Why water is not treated" = whynotreat_table, "Chlorine distribution" = chlorine_dist_table, 
                    "Community has jerry cans" = jerry_own_table, "Jerry cans distributed" = jerry_dist_table,
                    "Community has soap" = have_soap_table, "Community has access to soap" = soap_access_table, "Issues with accessing soap" = soap_problem_table, "Soap distribution" = soap_distribution_table,
                    "Higyiene campaing in the community" = campaign_table, "Trained staff" = staff_table,
                    "Access to latrines" = latrine_access_table, "Type of latrines" = latrine_type_table, "Issues with latrine" = latrine_issues_table,
                    "Have vulnerable groups access to latrines" = accessgroups_yesno_table, "Type of group with no access" = accessgroups_table,
                    "Garbage collection" = garbage_collection_table, "Who is responsible for garbage" = garbage_responsible_table, "Garbage disposal" = garbage_disposal_table,
                    "ORS" = ors_table,  "Who is able to prepare ORS" = ors_who_table, "ORC" = orc_table)

write.xlsx(bigASSlist, paste0("./output/WASH_WANTS_cholera tool_",current_date,"_complete_analysis.xlsx"), asTable = T)

#### VLOOKUP dataset 
##### Add prefix
colnames(setting_type_table) <- paste(colnames(setting_type_table), "setting_type", sep = "_")


vlookup <- response %>% select("g_governorate")

vlookup <- cbind(demographics_list)
