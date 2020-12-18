### Table 1 Code ###
### November 25, 2020  ###
################################################################################ 
# Comparing county-level data to PUMA-level data                               # 
# Authors:                                                                     #
#     - Marissa Reitsma                                                        # 
#     - Anneke Claypool                                                        # 
#     - Jeremy Goldhaber-Fiebert                                               #
################################################################################


rm(list = ls())
## Load libraries
library(sccosmoData)
library(ggplot2)
library(data.table)
library(tidyverse)
library(gridExtra)
library(grid)
library(stringi)
library(lubridate)
library(plotly)
library(ggalt)
library(TTR)
library(tidyquant)
library(scales)
library(dplyr)

source(paste0(getwd(),"/","./R/03_data_functions.R"))

date_data <- "2020-11-30" #Date of the data pull for county-level data

GLOBAL_CA_DATA_PATH            <- paste0(getwd(), "/Data/")
GLOBAL_CA_TEST_DATA_FILE       <- paste0(GLOBAL_CA_DATA_PATH, "positives_county_race_",date_data,".csv") #This dataset is not publicly available
GLOBAL_CA_CASE_DATA_FILE       <- paste0(GLOBAL_CA_DATA_PATH, "cases_county_race_",date_data,".csv") #This dataset is not publicly available
GLOBAL_CA_DEATH_DATA_FILE      <- paste0(GLOBAL_CA_DATA_PATH, "cases_county_race_",date_data,".csv") #This dataset is not publicly available

#PUMA-level data
GLOBAL_CA_PUMA_DATA_PATH       <- paste0(getwd(), "/Data/")
GLOBAL_CA_PUMA_DATA_FILE       <- paste0(GLOBAL_CA_PUMA_DATA_PATH, "puma_level_data.csv") #This dataset is not publicly available

#CSV output path
GLOBAL_CA_OUTPUT_PATH <- paste0(getwd(), "/Figures and Tables/")
GLOBAL_CA_COUNTY_SUMMARY_DATA_TABLE1_CSV <- paste0(GLOBAL_CA_OUTPUT_PATH, "table1_summary_by_race_CA_",date_data,".csv")

#Define date limits for the manuscript
first_data_date <- ymd("2020-03-22")
last_data_date <- ymd("2020-10-03")

### Define the parameters

country <- "United States of America"
state<- "California"
v_counties_calib<- c(
  "Los Angeles County",
  "San Diego County",
  "Riverside County",
  "Orange County",
  "San Bernardino County",
  "Alameda County",
  "Santa Clara County",
  "San Francisco County",
  "San Mateo County",
  "Kern County",
  "Tulare County",
  "Santa Barbara County",
  "Fresno County",
  "Contra Costa County",
  "Sacramento County",
  "Imperial County",
  "Ventura County",
  "San Joaquin County",
  "Stanislaus County",
  "Solano County",
  "Monterey County",
  "Marin County",
  "Merced County",
  "Placer County",
  "San Luis Obispo County",
  "Greater Sacramento County",
  "Central Sierra County",
  "San Joaquin Valley County",
  "Northern California County",
  "Napa_Sonoma County",
  "Northern Sacramento Valley County",
  "SantaCruz_SanBenito County"
)

v_counties_group_calib <-c(
  "Greater Sacramento",
  "Central Sierra",
  "San Joaquin Valley",
  "Northern California",
  "Napa_Sonoma",
  "Northern Sacramento Valley",
  "SantaCruz_SanBenito"
)

#####################
## Population by County and Race
#####################

### Figure out proportion of population for each group
#Find pop and pop proportion for each age group
df_pop_age_us <- get_population_ages_us(reload = FALSE)
df_pop_age_us <- as.data.frame(df_pop_age_us)

l_counties_data <- get_counties_data(country, state, v_counties_calib, v_counties_group_calib, regenerate_flag = FALSE)

pop_group_counties <- l_counties_data$df_pop_state_cty_age %>%
  group_by(county, age) %>%
  slice_head() %>%
  ungroup

pop_group_counties$c_code <- NULL
pop_group_counties$s_code <- NULL
pop_group_counties$lx <- NULL
pop_group_counties$dx <- NULL
pop_group_counties$deaths <- NULL
colnames(pop_group_counties)[6]<- "age_pop"

df_pop_age_us <- rbind(df_pop_age_us, pop_group_counties)

df_pop_race_ca <- df_pop_age_us %>%
  filter(country=="United States of America" & state == "California") %>%
  pivot_longer(names_to = "race", values_to = "tot_pop_race",
               cols = c(tot_black_pop,
                        tot_hisp_pop,
                        tot_white_pop,
                        tot_other_pop)) %>%
  filter(age == 0) %>%
  mutate(race = replace(race, race == "tot_black_pop", "Black")) %>%
  mutate(race = replace(race, race == "tot_hisp_pop", "Hispanic")) %>%
  mutate(race = replace(race, race == "tot_white_pop", "White")) %>%
  mutate(race = replace(race, race == "tot_other_pop", "Other")) %>%
  group_by(county, race, tot_pop_race) %>%
  summarise(per_race_pop = sum(tot_pop_race/ tot_pop)) %>%
  ungroup(race) %>%
  mutate(race_pop_per_cumul = cumsum(per_race_pop)) %>%
  as.data.table()

#####################
## Cases-- Case Series using Episode Date
#####################

cases_case_series_ca<- read.csv(GLOBAL_CA_CASE_DATA_FILE)

cases_case_series_ca_inc_age1 <- cases_case_series_ca %>%
  filter(county != "California" & !is.na(county)) %>%
  mutate(date = dmy(date)) %>%
  add_column(data_type = "CDPH Episode Data") %>%
  mutate(county = paste0(county, " County")) %>%
  rename(incident_cases_all = incident_cases) %>% 
  pivot_longer(names_to = "race", values_to = "incident_cases",
               cols = c(incident_cases_all, 
                        incident_cases_Black, 
                        incident_cases_Hispanic, 
                        incident_cases_White, 
                        incident_cases_Other, 
                        incident_cases_Unknown)) %>%
  mutate(race = gsub('incident_cases_', '', race)) %>%
  select(c(county, 
           date,
           race, 
           data_type, 
           incident_cases)) %>%
  as.data.table()

cases_case_series_ca_inc_age2 <- cases_case_series_ca %>%
  filter(county != "California" & !is.na(county)) %>%
  mutate(date = dmy(date)) %>%
  add_column(data_type = "CDPH Episode Data") %>%
  mutate(county = paste0(county, " County")) %>%
  rename(cumulative_cases_all = cumulative_cases) %>%
  pivot_longer(names_to = "race", values_to = "cumulative_cases",
               cols = c(cumulative_cases_all,
                        cumulative_cases_Black,
                        cumulative_cases_Hispanic,
                        cumulative_cases_White,
                        cumulative_cases_Other,
                        cumulative_cases_Unknown)) %>%
  mutate(race = gsub('cumulative_cases_', '', race)) %>% 
  select(c(county, 
           date, 
           race, 
           data_type, 
           cumulative_cases)) %>%
  as.data.table()

cases_case_series_ca_inc_age <- merge(cases_case_series_ca_inc_age1, cases_case_series_ca_inc_age2)


###########
### Total Test series ###
###########

test_series_ca <- read.csv(GLOBAL_CA_TEST_DATA_FILE)

test_series_ca <- test_series_ca %>%
  filter(county != "California" & county != "" & !is.na(county)) %>%
  mutate(date = dmy(date)) %>%
  select(c(county, 
           date, 
           conclusive_tests, conclusive_tests_to_date, 
           conclusive_tests_Hispanic, concl_tests_to_date_Hispanic,
           conclusive_tests_Black, concl_tests_to_date_Black,
           conclusive_tests_White, concl_tests_to_date_White,
           conclusive_tests_Other, concl_tests_to_date_Other,
           conclusive_tests_Unknown, concl_tests_to_date_Unknown,
           conclusive_tests_Missing, concl_tests_to_date_Missing,
           total_tests, total_tests_to_date, 
           total_tests_Hispanic, total_tests_to_date_Hispanic,
           total_tests_Black, total_tests_to_date_Black,
           total_tests_White, total_tests_to_date_White,
           total_tests_Other, total_tests_to_date_Other,
           total_tests_Unknown, total_tests_to_date_Unknown,
           total_tests_Missing, total_tests_to_date_Missing)) 


test_series_ca_concl_test_race <- test_series_ca %>%
  filter(county != "California" & !is.na(county)) %>%
  add_column(data_type = "CDPH Test Data") %>%
  mutate(county = paste0(county, " County")) %>%
  rename(conclusive_tests_all = conclusive_tests) %>%
  pivot_longer(names_to = "race", values_to = "conclusive_tests",
               cols = c(conclusive_tests_all,
                        conclusive_tests_Black,
                        conclusive_tests_Hispanic,
                        conclusive_tests_White,
                        conclusive_tests_Other,
                        conclusive_tests_Unknown)) %>%
  mutate(race = gsub('conclusive_tests_', '', race)) %>% 
  select(c(county, 
           date, 
           conclusive_tests, 
           race, 
           data_type)) %>%
  as.data.table()

test_series_ca_tot_tests_race <- test_series_ca %>%
  filter(county != "California" & !is.na(county)) %>%
  add_column(data_type = "CDPH Test Data") %>%
  mutate(county = paste0(county, " County")) %>%
  rename(total_tests_all = total_tests) %>% 
  pivot_longer(names_to = "race", values_to = "total_tests",
               cols = c(total_tests_all, 
                        total_tests_Black, 
                        total_tests_Hispanic, 
                        total_tests_White, 
                        total_tests_Other, 
                        total_tests_Unknown)) %>%
  mutate(race = gsub('total_tests_', '', race)) %>%
  select(c(county, 
           date, 
           total_tests, 
           race, 
           data_type)) %>%
  as.data.table()

test_series_ca_test_race <-merge(test_series_ca_concl_test_race,test_series_ca_tot_tests_race)


####################################
## Death data by County
####################################  
death_series_ca <- read.csv(GLOBAL_CA_DEATH_DATA_FILE)

death_series_ca_racea <- death_series_ca %>%
  filter(county != "California" & !is.na(county)) %>%
  mutate(date = dmy(date)) %>%
  add_column(data_type = "CDPH Case Series") %>%
  mutate(county = paste0(county, " County")) %>%
  rename(incident_deaths_all = incident_deaths) %>% 
  pivot_longer(names_to = "race", values_to = "incident_deaths",
               cols = c(incident_deaths_all, 
                        incident_deaths_Black, 
                        incident_deaths_Hispanic, 
                        incident_deaths_White, 
                        incident_deaths_Other,
                        incident_deaths_Unknown)) %>%
  mutate(race = gsub('incident_deaths_', '', race)) %>% 
  select(c(county, date, incident_deaths, race, data_type)) %>%
  as.data.table()

death_series_ca_raceb <- death_series_ca %>%
  filter(county != "California" & !is.na(county)) %>%
  mutate(date = dmy(date)) %>%
  add_column(data_type = "CDPH Case Series") %>%
  mutate(county = paste0(county, " County")) %>%
  rename(cumulative_deaths_all = cumulative_deaths) %>% 
  pivot_longer(names_to = "race", values_to = "cumulative_deaths",
               cols = c(cumulative_deaths_all, 
                        cumulative_deaths_Black, 
                        cumulative_deaths_Hispanic, 
                        cumulative_deaths_White, 
                        cumulative_deaths_Other,
                        cumulative_deaths_Unknown)) %>%
  mutate(race = gsub('cumulative_deaths_', '', race)) %>% 
  select(c(county, date, cumulative_deaths, race, data_type)) %>%
  as.data.table()

death_series_ca_inc_race <- merge(death_series_ca_racea, death_series_ca_raceb)

death_series_ca2 <- death_series_ca %>%
  filter(county != "California" & !is.na(county)) %>%
  mutate(date = dmy(date)) %>%
  add_column(data_type = "CDPH Case Series") %>%
  mutate(county = paste0(county, " County")) %>%
  as.data.table()


### Combine all County States ####

### Total and incidence in each group, each level ###
incidence_per_county <- cases_case_series_ca_inc_age %>%
  group_by(county, race) %>%
  filter(date >= first_data_date & date <= last_data_date) %>%
  summarise(cumulative_incident_cases_calc = sum(incident_cases))

tests_per_county <- test_series_ca_test_race %>%
  group_by(county, race) %>%
  filter(date >= first_data_date & date <= last_data_date) %>%
  summarise(conclusive_tests_to_date = sum(conclusive_tests), total_tests_to_date = sum(total_tests))

death_per_county <- death_series_ca_inc_race %>%
  group_by(county, race) %>%
  filter(date >= first_data_date & date <= last_data_date) %>%
  summarise(cumulative_deaths = sum(incident_deaths))

pop_per_county <- df_pop_race_ca %>%
  group_by(county, race) %>%
  rename(population = tot_pop_race) %>%
  select(county, race, population) %>%
  data.frame()

tot_pop_per_county <- pop_per_county %>%
  group_by(county) %>%
  summarise(total_population = sum(population)) %>%
  mutate(race = "all") %>%
  rename(population = total_population) %>%
  select(county, race, population) %>%
  data.frame()

pop_per_county <- rbind(pop_per_county, tot_pop_per_county) %>%
  arrange(county)

all_county_stats <- left_join(incidence_per_county, tests_per_county)
all_county_stats <- left_join(all_county_stats, death_per_county)
all_county_stats <- left_join(all_county_stats, pop_per_county)

all_county_stats2 <- all_county_stats %>%
  mutate(cases_incidence = (cumulative_incident_cases_calc/population)*100000,
         test_incidence = (total_tests_to_date/population)*100000,
         death_incidence = (cumulative_deaths/population)*100000,
         test_positivity_rate = cumulative_incident_cases_calc/ total_tests_to_date,
         death_rate =  cumulative_deaths/cumulative_incident_cases_calc) %>%
  data.table()

df_ca_summary_data <- all_county_stats2%>%
  filter(county != "Mexico_oroutof_Cali County" & county != "UNASSIGNED County")%>%
  group_by(race) %>%
  summarise(total_population = sum(population, na.rm = T),
            cases = sum(cumulative_incident_cases_calc),
            case_rate = (sum(cumulative_incident_cases_calc)/sum(population, na.rm = T))*100000,
            tests = sum(total_tests_to_date),
            test_rate = (sum(total_tests_to_date)/sum(population, na.rm = T))*100000,
            deaths = sum(cumulative_deaths, na.rm = T),
            death_rate = (sum(cumulative_deaths, na.rm = T)/sum(population, na.rm = T))*100000)

df_ca_summary_data$case_percent <- df_ca_summary_data$cases/df_ca_summary_data$cases[df_ca_summary_data$race == "all"]
df_ca_summary_data$test_percent <- df_ca_summary_data$tests/df_ca_summary_data$tests[df_ca_summary_data$race == "all"]
df_ca_summary_data$death_percent <- df_ca_summary_data$deaths/df_ca_summary_data$deaths[df_ca_summary_data$race == "all"]

#Add in Geocode data from PUMA
puma_series_ca <- read.csv(GLOBAL_CA_PUMA_DATA_FILE) %>%
  filter(month == "March-September") %>%
  group_by(race_grp) %>%
  summarise(total_cases_geo = sum(tot_cases),
            total_tests_geo = sum(tot_tests))

puma_series_ca2 <- puma_series_ca %>%
  mutate(race_grp = sub("Hispanic/Latinx","Hispanic", race_grp)) %>%
  mutate(race_grp = sub("Non-Hispanic Black", "Black", race_grp)) %>%
  mutate(race_grp = sub("Non-Hispanic White", "White", race_grp)) %>%
  mutate(race_grp = sub("ALL", "all", race_grp)) %>%
  rename(race = race_grp) %>%
  data.table()

df_ca_summary_data1 <- left_join(df_ca_summary_data, puma_series_ca2) %>%
  mutate(cases_geo_percent = total_cases_geo/cases) %>%
  mutate(tests_geo_percent = total_tests_geo/tests)
  
#Reformat to match Table 1
ordered_names <- c("all", "Hispanic", "Black", "White", "Other", "Unknown")

df_ca_summary_data2 <- df_ca_summary_data1 %>%
            mutate(race =  factor(race, levels = ordered_names)) %>%
            arrange(race) %>%
            select(race,total_population, cases, case_percent, case_rate, tests, test_percent, 
                   test_rate, deaths, death_rate, death_percent, total_cases_geo, 
                   cases_geo_percent, total_tests_geo, tests_geo_percent) %>%
            t() %>% 
            data.frame()
  

write.csv(df_ca_summary_data2, GLOBAL_CA_COUNTY_SUMMARY_DATA_TABLE1_CSV)


