### WASH WANTS Assessment Data Analysis Script - Common Tool
### REACH Yemen
### V2
### 08/09/2019

rm(list=ls())

### Download custom packages
# devtools::install_github("agualtieri/hypegrammaR", force = T)
# devtools::install_github("mabafaba/composR", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/xlsformfill", force = T, build_vignettes = T)

### Load required library
library("tidyverse")
library("hypegrammaR")
library("xlsformfill")
library("openxlsx")

### Load source
source("./R/moveme.R")
source("./R/remove_minmax.R")
source("./R/multiple_response.R")


### Change date of data analysis
current_date <- Sys.Date()

### Load cleaned data, questionnaire, and choices sheet
# response_old <- read.csv("./data/V2/WANTS_Common_Tool_2019-09-03_cleaned.csv", stringsAsFactors = F)
# response_loops <- read.csv("./data/WANTS_Common_Tool_2019-09-03_cleaned_loops.csv", stringsAsFactors = F)
# questions <- read.csv("./data/questions.csv", stringsAsFactors = F)
# choices <- read.csv("./data/choices.csv", stringsAsFactors = F)
 
### Add some fake data
questions <- read.csv("./data/questions.csv", stringsAsFactors = F, check.names = F)
choices <- read.csv('./data/choices_v2.csv', stringsAsFactors = F, check.names = F)
choices_ext <- read.csv("./data/external_choices.csv", stringsAsFactors = F, check.names = F)

response <- xlsform_fill(questions,
                         choices,
                         10000)

response_locations <- xlsform_fill(questions,
                                   choices_ext,
                                   10000)



response$g_district <- response_locations$g_district
response$g_sub_district <- response_locations$g_sub_district
response$g_location <- response_locations$g_location

names(response) <- gsub("/", ".", names(response))


response <- filter(response, response$g_governorate == substr(response$g_district, start = 1, stop = 4))


# write.csv(response, paste0("./output/WASH_WANTS_Common Tool_dummy data_",current_date,".csv"))



survey <- load_questionnaire(response,
                            questions,
                            choices,
                            choices.label.column.to.use = "label::english")




### To put in cleaning
#names(response) <- gsub("W1_Water.", "", names(response))

### Attach location infos to loops
# names(response)[names(response) == "X_index"] <- "index_merge"
# names(response_loops)[names(response_loops) == "X_parent_index"] <- "index_merge"

# loops <- response %>% select(c("g_governorate", "g_district", "g_sub_district", "index_merge", "X_uuid")) %>% right_join(response_loops, "index_merge")


### Subset dataset to work with the full 333 districts list (check if needed)
#response_split <- response %>% split.data.frame(response$g_governorate) 

#analysis_split <- purrr::map(response_split, ~map_to_result(.x, dependent.var = "g_org_aid",
                                                            #independent.var = "g_district",
                                                           # questionnaire = survey,
                                                            #case = map_to_case("group_difference", "categorical", "categorical")))

#analysis_split_NoMinMax <- purrr::map(analysis_split, ~remove_minmax(.x))

#write.csv(response_split$YE17, "./output/test_dataset.csv")


#tables_split <- purrr::map(analysis_split_NoMinMax, ~map_to_table(.x))

#map_to_file(tables_split$YE17, "./output/TEST_table.csv")


### Analysis Demographics
#### RQ 1: What is the demographic of the community

##### Type of setting
setting_type <- map_to_result(response, 
                              "g_urbanrural",
                              "g_district",
                              questionnaire = survey,
                              map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()



setting_type_table <- map_to_table(setting_type)
map_to_file(setting_type_table, paste0("./output/demographics/setting_type_",current_date,".csv"))

##### Status of community residents
community_residents <- response %>% map_to_result("g_resident",
                                                  "g_district",
                                                  questionnaire = survey,
                                                  map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

community_residents_table <- map_to_table(community_residents)
map_to_file(community_residents_table, paste0("./output/demographics/community_residents_",current_date,".csv"))

##### Population size
pop_size <- response %>% map_to_result("g_population",
                                       "g_district",
                                       questionnaire = survey,
                                       map_to_case("group_difference", "numerical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

pop_size_table <- map_to_table(pop_size)
map_to_file(pop_size_table, paste0("./output/demographics/pop_size_",current_date,".csv"))

##### Type of organization
org_type <- response %>% map_to_result("g_orgtype",
                                       "g_district",
                                       questionnaire = survey,
                                       map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

org_type_table <- map_to_table(org_type)
map_to_file(org_type_table, paste0("./output/demographics/org_type_",current_date,".csv"))



### Type of org providing aid
org_aid <- response %>% map_to_result(dependent.var = "g_org_aid",
                                      independent.var = "g_district",
                                      questionnaire = survey,
                                      case = map_to_case("group_difference", "categorical", "categorical"))  %>% map_to_labeled(survey) %>% remove_minmax()


 
org_aid_table <- map_to_table(org_aid)
map_to_file(org_aid_table, paste0("./output/demographics/org_aid_",current_date,".csv"))




### Analysis Water
#### RQ1: What is the proportion of communities accessing Improved Water Sources?
main_source <- response %>% map_to_result("w_mainsource",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

main_source_table <- map_to_table(main_source)
map_to_file(main_source_table, paste0("./output/wash/main_source_",current_date,".csv"))

secondary_yes <- response %>% map_to_result("w_secondsourceyesno",
                                                  "g_governorate",
                                            questionnaire = survey,
                                                  map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

secondary_yes_table <- map_to_table(secondary_yes)
map_to_file(secondary_yes_table, paste0("./output/wash/secondary_yes_",current_date,".csv"))

secondary_source <- response %>% map_to_result("w_secondsource",
                                              "g_governorate",
                                              questionnaire = survey,
                                              map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

secondary_source_table <- map_to_table(secondary_source)
map_to_file(secondary_source_table, paste0("./output/wash/secondary_drink_",current_date,".csv"))

drink_source <- response %>% map_to_result("w_drinksource",
                                           "g_governorate",
                                           questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

drink_source_table <- map_to_table(drink_source)
map_to_file(drink_source_table, paste0("./output/wash/drink_source_",current_date,".csv"))


#### RQ2: What are issues with drinking water that communities experience?
water_issues <- response %>% map_to_result("w_waterappear",
                                           "g_governorate",
                                           questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

water_issues_table <- map_to_table(water_issues)
map_to_file(water_issues_table, paste0("./output/wash/water_issues_",current_date,".csv"))

water_appear <- response %>% map_to_result("w_waterappear",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

water_appear_table <- map_to_table(water_appear)
map_to_file(water_appear_table, paste0("./output/wash/water_appear_",current_date,".csv"))

water_smell <- response %>% map_to_result("w_watersmell",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

water_smell_table <- map_to_table(water_smell)
map_to_file(water_smell_table, paste0("./output/wash/water_smell_",current_date,".csv"))


#### RQ3: What is the proportion of communities that are accessing adequate/sufficient quantities of water?
water_needs <- response %>% map_to_result("w_waterneeds",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

water_needs_table <- map_to_table(water_needs)
map_to_file(water_needs_table, paste0("./output/wash/water_needs_",current_date,".csv"))


#### RQ4: What is the proportion of communities in which fetching water constitutes a problem?
water_coping <- response %>% map_to_result("w_watercoping",
                                           "g_governorate",
                                           questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()


water_coping_table <- map_to_table(water_coping)
map_to_file(water_coping_table, paste0("./output/wash/water_coping_",current_date,".csv"))



water_access <- response %>% map_to_result("w_wateraccess",
                                           "g_governorate",
                                           questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

water_access_table <- map_to_table(water_access)
map_to_file(water_access_table, paste0("./output/wash/water_access_",current_date,".csv"))

water_access_problem <- response %>% map_to_result("w_accessproblem",
                                                   "g_governorate",
                                                   questionnaire = survey,
                                                   map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

water_access_problem_table <- map_to_table(water_access_problem)
map_to_file(water_access_problem_table, paste0("./output/wash/water_access_problem_",current_date,".csv"))

fetch_water <- response %>% map_to_result("w_fetchwater",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

fetch_water_table <- map_to_table(fetch_water)
map_to_file(fetch_water_table, paste0("./output/wash/fetch_water_",current_date,".csv"))

#### RQ5: What are measures undertaken at household level to treat/make drinking water safe? 
treat_water <- response %>% map_to_result("w_treatwater",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

treat_water_table <- map_to_table(treat_water)
map_to_file(treat_water_table, paste0("./output/wash/treat_water_",current_date,".csv"))

treat_method <- response %>% map_to_result("w_treatmethod",
                                           "g_governorate",
                                           questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

treat_method_table <- map_to_table(treat_method)
map_to_file(treat_method_table, paste0("./output/wash/treat_method_",current_date,".csv"))


##### Composing the ranking
treating_methods_ranks <- response %>% group_by(g_governorate) %>% summarise_at(vars(starts_with("w_treatrank")), funs(mean(., na.rm = T)))
write.csv(treating_methods_ranks, paste0("./output/wash/treating_methods_sub_",current_date,".csv"), row.names = F)

treatmethods_ranks <- response %>% summarise_at(vars(starts_with("w_treatrank")), funs(mean(., na.rm = T)))
write.csv(treatmethods_ranks, paste0("./output/wash/treating_methods_over_",current_date,".csv"), row.names = F)



whynotreat <- response %>% map_to_result("w_whynot_treat",
                                         "g_governorate",
                                         questionnaire = survey,
                                         map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

whynotreat_table <- map_to_table(whynotreat)
map_to_file(whynotreat_table, paste0("./output/wash/fetch_water_",current_date,".csv"))

### Analysis Health
#### RQ1: What percentage of KIs reports that communities have access to functioning health facilities (and soap)
handwashing_access <- response %>% map_to_result("h_handwashing",
                                                 "g_governorate",
                                                 questionnaire = survey,
                                                 map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

handwashing_access_table <- map_to_table(handwashing_access)
map_to_file(handwashing_access_table, paste0("./output/health/hadnwashing_access_",current_date,".csv"))

havesoap <- response %>% map_to_result("h_have_soap",
                                       "g_governorate",
                                       questionnaire = survey,
                                       map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

havesoap_table <- map_to_table(havesoap)
map_to_file(havesoap_table, paste0("./output/health/havesoap_",current_date,".csv"))

soapaccess <- response %>% map_to_result("h_soapaccess",
                                         "g_governorate",
                                         questionnaire = survey,
                                         map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

soapaccess_table <- map_to_table(soapaccess)
map_to_file(soapaccess_table, paste0("./output/health/soapaccess_",current_date,".csv"))

soapproblem <- response %>% map_to_result("h_soap_problem",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

soapproblem_table <- map_to_table(soapproblem)
map_to_file(soapproblem_table, paste0("./output/health/soapproblem_",current_date,".csv"))

#### RQ2: What percentage of KIs reports that communities have access to basic hygiene NFIs?
soap <- response %>% map_to_result("h_barsoap",
                                   "g_governorate",
                                   questionnaire = survey,
                                   map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

soap_table <- map_to_table(soap)
map_to_file(soap_table, paste0("./output/health/soap_",current_date,".csv"))

jerrycan <- response %>% map_to_result("h_jerrycan",
                                       "g_governorate",
                                       questionnaire = survey,
                                       map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

jerrycan_table <- map_to_table(jerrycan)
map_to_file(jerrycan_table, paste0("./output/health/jerrycan_",current_date,".csv"))


sanitarypads <- response %>% map_to_result("h_sanitarypad",
                                       "g_governorate",
                                       questionnaire = survey,
                                       map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

sanitarypads_table <- map_to_table(sanitarypads)
map_to_file(sanitarypads_table, paste0("./output/health/sanitarypads_",current_date,".csv"))

daipers <- response %>% map_to_result("h_daipers",
                                           "g_governorate",
                                      questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

daipers_table <- map_to_table(daipers)
map_to_file(daipers_table, paste0("./output/health/daipers_",current_date,".csv"))

washingpowder <- response %>% map_to_result("h_washingpowder",
                                      "g_governorate",
                                      questionnaire = survey,
                                      map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

washingpowder_table <- map_to_table(washingpowder)
map_to_file(washingpowder_table, paste0("./output/health/washingpowder_",current_date,".csv"))

washingbasin <- response %>% map_to_result("h_washingbasin",
                                            "g_governorate",
                                           questionnaire = survey,
                                            map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

washingbasin_table <- map_to_table(washingbasin)
map_to_file(washingbasin_table, paste0("./output/health/washingbasin_",current_date,".csv"))

toothpaste <- response %>% map_to_result("h_toothpaste",
                                           "g_governorate",
                                         questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

toothpaste_table <- map_to_table(toothpaste)
map_to_file(toothpaste_table, paste0("./output/health/toothpaste_",current_date,".csv"))

toothbrush <- response %>% map_to_result("h_toothbrush",
                                         "g_governorate",
                                         questionnaire = survey,
                                         map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

toothbrush_table <- map_to_table(toothbrush)
map_to_file(toothbrush_table, paste0("./output/health/toothbrush_",current_date,".csv"))

chlorine <- response %>% map_to_result("h_chlorine",
                                         "g_governorate",
                                       questionnaire = survey,
                                         map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

chlorine_table <- map_to_table(chlorine)
map_to_file(chlorine_table, paste0("./output/health/chlorine_",current_date,".csv"))


hygiene_item <- response %>% map_to_result("h_hygieneitem",
                                           "g_governorate",
                                           questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

hygiene_item_table <- map_to_table(hygiene_item)
map_to_file(hygiene_item_table, paste0("./output/health/hygiene_item_",current_date,".csv"))

hygiene_item_access <- response %>% map_to_result("h_hygieneitem_access",
                                                  "g_governorate",
                                                  questionnaire = survey,
                                                  map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

hygiene_item_access_table <- map_to_table(hygiene_item_access)
map_to_file(hygiene_item_access_table, paste0("./output/health/hygiene_item_access_",current_date,".csv"))

#### RQ3: How is the access to health facilities for community members?
healthfacility <- response %>% map_to_result("h_healthfacility",
                                                  "g_governorate",
                                             questionnaire = survey,
                                                  map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

healthfacility_table <- map_to_table(healthfacility)
map_to_file(healthfacility_table, paste0("./output/health/healthfacility_",current_date,".csv"))

distancehealth <- response %>% map_to_result("h_distancehealth",
                                             "g_governorate",
                                             questionnaire = survey,
                                             map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

distancehealth_table <- map_to_table(distancehealth)
map_to_file(distancehealth_table, paste0("./output/health/distancehealth_",current_date,".csv"))


healthbarr_yesno <- response %>% map_to_result("h_barrieryesno",
                                             "g_governorate",
                                             questionnaire = survey,
                                             map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

healthbarr_yesno_table <- map_to_table(healthbarr_yesno)
map_to_file(healthbarr_yesno_table, paste0("./output/health/healthbarr_yesno_",current_date,".csv"))


healthbarrier<- response %>% map_to_result("h_healthbarrier",
                                               "g_governorate",
                                           questionnaire = survey,
                                               map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

healthbarrier_table <- map_to_table(healthbarrier)
map_to_file(healthbarrier_table, paste0("./output/health/healthbarrier_",current_date,".csv"))

### Sanitation Analysis
#### RQ1: What proportion of communities has access to functioning health?
latrineaccess <- response %>% map_to_result("s_latrineaccess",
                                           "g_governorate",
                                           questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

latrineaccess_table <- map_to_table(latrineaccess)
map_to_file(latrineaccess_table, paste0("./output/sanitation/latrineaccess_",current_date,".csv"))


latrinetype <- response %>% map_to_result("s_latrinetype",
                                            "g_governorate",
                                          questionnaire = survey,
                                            map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

latrinetype_table <- map_to_table(latrinetype)
map_to_file(latrinetype_table, paste0("./output/sanitation/latrinetype_",current_date,".csv"))


latrineissues <- response %>% map_to_result("s_latrineissues",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

latrineissues_table <- map_to_table(latrineissues)
map_to_file(latrineissues_table, paste0("./output/sanitation/latrineissues_",current_date,".csv"))


accessgroupyesno <- response %>% map_to_result("s_accessgroupyesno",
                                            "g_governorate",
                                            questionnaire = survey,
                                            map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

accessgroupyesno_table <- map_to_table(accessgroupyesno)
map_to_file(accessgroupyesno_table, paste0("./output/sanitation/accessgroupyesno_",current_date,".csv"))


accessgroup <- response %>% map_to_result("s_accessgroup",
                                               "g_governorate",
                                          questionnaire = survey,
                                               map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

accessgroup_table <- map_to_table(accessgroup)
map_to_file(accessgroup_table, paste0("./output/sanitation/accessgroup_",current_date,".csv"))


visibletrash<- response %>% map_to_result("s_visibletrash",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

visibletrash_table <- map_to_table(visibletrash)
map_to_file(visibletrash_table, paste0("./output/sanitation/visibletrash_",current_date,".csv"))


disposetrash <- response %>% map_to_result("s_disposetrash",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

disposetrash_table <- map_to_table(disposetrash)
map_to_file(disposetrash_table, paste0("./output/sanitation/disposetrash_",current_date,".csv"))


trashcollected <- response %>% map_to_result("s_trashcollected",
                                           "g_governorate",
                                           questionnaire = survey,
                                           map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

trashcollected_table <- map_to_table(trashcollected)
map_to_file(trashcollected_table, paste0("./output/sanitation/trashcollected_",current_date,".csv"))


humanfaeces <- response %>% map_to_result("s_humanfaeces",
                                             "g_governorate",
                                          questionnaire = survey,
                                             map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

humanfaeces_table <- map_to_table(humanfaeces)
map_to_file(humanfaeces_table, paste0("./output/sanitation/humanfaeces_",current_date,".csv"))

stagnantwater <- response %>% map_to_result("s_stagnantwater",
                                          "g_governorate",
                                          questionnaire = survey,
                                          map_to_case("group_difference", "categorical", "categorical")) %>% map_to_labeled(survey) %>% remove_minmax()

stagnantwater_table <- map_to_table(stagnantwater)
map_to_file(stagnantwater_table, paste0("./output/sanitation/stagnantwater_",current_date,".csv"))


### Save final analysis - multiple sheets
population_list <- list("Setting type" = setting_type_table, "Population size" = pop_size_table, "Population type" = community_residents_table,
                        "Type of organization" = org_type_table, "Type of assistance provided" = org_aid_table)
write.xlsx(population_list, paste0("./output/WASH_WANTS_common tool_",current_date,"_demographics.xlsx"), asTable = T)

wash_list <- list("Main source of water" = main_source_table, "Presence of secondary source" = secondary_yes_table, "Secondary source of water" = secondary_source_table,
                  "Main drinking water source" = drink_source_table, "Issues with water" = water_issues_table, "Water appearance" = water_appear_table, "Water smell" = water_smell_table,
                  "Water needs" = water_needs_table, "Water access" = water_access_table, "Problems with accessing water" = water_access_problem_table, "Water coping strategies" = water_coping_table,
                  "Water collection" = fetch_water_table, "Water treatment" = treat_water_table, "Treating method" = treat_method_table, "Why water not treated" = whynotreat_table,
                  "Water treatment rank" = treatmethods_ranks)
write.xlsx(wash_list, paste0("./output/WASH_WANTS_common tool_",current_date,"_water.xlsx"), asTable = T)
  
health_list <- list("Handwashing facility" = handwashing_access_table, "Presence of soap" = havesoap_table, "Access to soap" = soapaccess_table,
                    "Issues with accessing soap" = soapproblem_table, "Soap bars" = soap_table, "Jerry cans" = jerrycan_table, "Sanitary pads" = sanitarypads_table,
                    "Daipers" = daipers_table, "Washing powder" = washingpowder_table, "Washing basin" = washingbasin_table, "Toothpaste" = toothpaste_table, "Toothbrush" = toothbrush_table,
                    "Chlorine" = chlorine_table, "Hygiene items" = hygiene_item_table, "Access to hygiene items" = hygiene_item_access_table,
                    "Health facility" = healthfacility_table, "Distance from health facility" = distancehealth_table, "Presence of barriers to health acces" = healthbarr_yesno_table,
                    "Type of barriers to access health" = healthbarrier_table)

write.xlsx(health_list, paste0("./output/WASH_WANTS_common tool_",current_date,"_health.xlsx"), asTable = T)
  
sanitation_list <- list("Access to latrines" = latrineaccess_table, "Type of latrines" = latrinetype_table, "Issues with latrines" = latrineissues_table, "Groups no access to latrines" = accessgroupyesno_table,
                        "Which group" = accessgroup_table, "Visible trash" = visibletrash_table, "Dispose of trash" = disposetrash_table, "Trash collected" = trashcollected_table,
                        "Presence of faeces" = humanfaeces_table, "Presence of stagnant water" = stagnantwater_table)
  
write.xlsx(sanitation_list, paste0("./output/WASH_WANTS_common tool_",current_date,"_sanitation.xlsx"), asTable = T)

### Save final analysis - one bigASS sheet
bigASSlist <- list("Setting type" = setting_type_table, "Population size" = pop_size_table, "Population type" = community_residents_table, "Type of organization" = org_type_table, "Type of assistance provided" = org_aid_table,
                   "Main source of water" = main_source_table, "Presence of secondary source" = secondary_yes_table, "Secondary source of water" = secondary_source_table,
                   "Main drinking water source" = drink_source_table, "Issues with water" = water_issues_table, "Water appearance" = water_appear_table, "Water smell" = water_smell_table,
                   "Water needs" = water_needs_table, "Water access" = water_access_table, "Problems with accessing water" = water_access_problem_table, "Water coping strategies" = water_coping_table,
                   "Water collection" = fetch_water_table, "Water treatment" = treat_water_table, "Treating method" = treat_method_table, "Why water not treated" = whynotreat_table,
                   "Water treatment rank" = treatmethods_ranks, "Access to latrines" = latrineaccess_table, "Type of latrines" = latrinetype_table, "Issues with latrines" = latrineissues_table, "Groups no access to latrines" = accessgroupyesno_table,
                   "Which group" = accessgroup_table, "Visible trash" = visibletrash_table, "Dispose of trash" = disposetrash_table, "Trash collected" = trashcollected_table,
                   "Presence of faeces" = humanfaeces_table, "Presence of stagnant water" = stagnantwater_table) 
         

write.xlsx(bigASSlist, paste0("./output/WASH_WANTS_common tool_",current_date,"_complete_analysis.xlsx"), asTable = T)
