rm(list = ls())

## Load packages
library(tidyverse)
library(data.table)
library(tigris)
library(viridis)
library(ggplot2)
library(acs)
library(usmap)
library(zoo)
library(grid)
library(gridExtra)
library(cowplot)
library(MMWRweek)
library(scales)

puma_mapping <- fread("./puma_mapping_2010.csv")
puma_mapping <- puma_mapping[STATEFP==46 & COUNTYFP==113, COUNTYFP:=102]

data(fips_codes)
fips_codes <- as.data.table(fips_codes)
setnames(fips_codes, c("state_code", "county_code"), c("STATEFP", "COUNTYFP"))
fips_codes <- fips_codes[STATEFP==46 & COUNTYFP==113, COUNTYFP:=102]
fips_codes <- fips_codes[, STATEFP_num:=as.numeric(STATEFP)]
fips_codes <- fips_codes[, COUNTYFP_num:=as.numeric(COUNTYFP)]

setnames(puma_mapping, c("STATEFP", "COUNTYFP"), c("STATEFP_num", "COUNTYFP_num"))
puma_mapping <- merge(puma_mapping, fips_codes, by = c("STATEFP_num", "COUNTYFP_num"))
puma_mapping <- puma_mapping[, id:=paste0(as.numeric(STATEFP), "_", as.numeric(PUMA))]
puma_mapping <- unique(puma_mapping[,.(STATEFP, COUNTYFP, PUMA, id, state_name, county)])
puma_mapping <- puma_mapping[!(id=="46_200" & county == "Shannon County")]

# Clean up list of puma names downloaded from https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Names.pdf?#
puma_name_list <- fread("./puma_name_list.csv")
puma_name_list <- puma_name_list[, puma_name:=paste(Name1, Name2, Name3, Name4, Name5, Name6, Name7, Name8, Name9, Name10, Name11, Name12, Name13, Name14, sep=" ")]
puma_name_list <- puma_name_list[, .(STATEFP, PUMA5CE, puma_name)]
puma_name_list <- puma_name_list[, puma_name:=trimws(puma_name)]
setnames(puma_name_list, "PUMA5CE", "PUMA")
puma_name_list <- puma_name_list[, id:=paste0(as.numeric(STATEFP), "_", as.numeric(PUMA))]

puma_mapping <- merge(puma_mapping, puma_name_list[,.(id, puma_name)], by = "id")

puma_mapping_out <- NULL
for (i in c("ALL", "Hispanic/Latinx", "Non-Hispanic Black", "Non-Hispanic White", "Other", "Unknown")) {
  temp <- copy(puma_mapping)
  temp <- temp[, race_grp:=i]
  puma_mapping_out<-rbind(puma_mapping_out, temp, fill = T)
}

puma_mapping_out <- puma_mapping_out[STATEFP=="06"]
puma_mapping_out <- puma_mapping_out[, STATEFIP:=as.numeric(STATEFP)]

out_race <- fread("./puma_estimates_race_withse_insurance_multiocc.csv")

tests <- fread("./stan_puma_race_tests_Oct6.csv")
tests <- tests[date>="2020-03-22"]
tests <- tests[race_ethnicity=="non-Hispanic Black", race_grp:="Non-Hispanic Black"]
tests <- tests[race_ethnicity=="Other", race_grp:="Other"]
tests <- tests[race_ethnicity=="non-Hispanic White", race_grp:="Non-Hispanic White"]
tests <- tests[race_ethnicity=="Hispanic/Latino", race_grp:="Hispanic/Latinx"]
tests <- tests[race_ethnicity=="Unknown", race_grp:="Unknown"]
tests <- tests[, month:=months(date)]
tests <- tests[, number:=sum(number), by = c("PUMA5CE", "race_ethnicity", "month")]
setnames(tests, "number", "tests")
tests <- unique(tests[,.(tests, PUMA5CE, race_grp, month)])

cases <- fread("./stan_puma_race_cases_epiweek_Oct6.csv")
cases <- cases[epiweek>=13 & epiweek<=40]
cases <- cases[rep(seq_len(nrow(cases)), 7)]
cases <- cases[, id := seq_len(.N), by = c("PUMA5CE", "epiweek", "number", "race_ethnicity")]
cases <- cases[, number:=number/7]
cases <- cases[, date:=MMWRweek2Date(MMWRyear = 2020, MMWRweek = epiweek, MMWRday = id), by=seq_len(nrow(cases))]
cases <- cases[race_ethnicity=="Hispanic/Latino", race_grp:="Hispanic/Latinx"]
cases <- cases[race_ethnicity=="Other", race_grp:="Other"]
cases <- cases[race_ethnicity=="non-Hispanic Black", race_grp:="Non-Hispanic Black"]
cases <- cases[race_ethnicity=="non-Hispanic White", race_grp:="Non-Hispanic White"]
cases <- cases[race_ethnicity=="Unknown", race_grp:="Unknown"]
cases <- cases[, month:=months(date)]
cases <- cases[, cases:=sum(number), by = c("PUMA5CE", "race_ethnicity", "month")]
cases <- unique(cases[,.(cases, PUMA5CE, race_grp, month)])

setnames(cases, "PUMA5CE", "PUMA")
setnames(tests, "PUMA5CE", "PUMA")

cases <- merge(cases, tests, by = c("PUMA", "race_grp", "month"), all = T)

square <- as.data.table(expand.grid(month = c("March", "April", "May", "June", "July", "August", "September", "October"),
                                    race_grp=c("Hispanic/Latinx", "Non-Hispanic Black", "Non-Hispanic White", "Other", "Unknown"),
                                    PUMA = unique(out_race$PUMA)))
cases <- merge(cases, square, by = c("race_grp", "month", "PUMA"), all = T)
cases <- cases[is.na(cases), cases:=0]
cases <- cases[is.na(tests), tests:=0]

all_cases <- copy(cases)
all_cases <- all_cases[, cases:=sum(cases), by = c("PUMA", "month")]
all_cases <- all_cases[, tests:=sum(tests), by = c("PUMA", "month")]
all_cases <- all_cases[, race_grp:="ALL"]
all_cases <- unique(all_cases)

cases <- rbind(cases, all_cases)

cases <- merge(cases, out_race[,.(PUMA, race_grp, tot_persons_point, crowd_essential_point_perc)], by = c("PUMA", "race_grp"), all = T)

setnames(cases, c("cases", "tests"), c("tot_cases", "tot_tests"))

cases <- unique(cases[,.(PUMA, race_grp, tot_persons_point, tot_cases, tot_tests, month, crowd_essential_point_perc)])

all_cases <- copy(cases)
all_cases <- all_cases[, tot_cases:=sum(tot_cases, na.rm=T), by = c("PUMA", "race_grp")]
all_cases <- all_cases[, tot_tests:=sum(tot_tests, na.rm=T), by = c("PUMA", "race_grp")]
all_cases <- all_cases[, month:="March-September"]
all_cases <- unique(all_cases)
cases <- rbind(cases, all_cases)

cases <- cases[, tpr:=tot_cases/tot_tests]
cases <- cases[, test_rate:=(tot_tests/tot_persons_point)*100000]
cases <- cases[, case_rate:=(tot_cases/tot_persons_point)*100000]

cases <- merge(cases, unique(puma_mapping_out[,.(PUMA, puma_name, race_grp)]), by = c("PUMA", "race_grp"))

write.csv(cases, "./puma_level_data.csv", na = "", row.names = F)

cases <- fread("./puma_level_data.csv")

merged_county_map <- fread("./grouped_county_assignments.csv")
merged_county_map <- merged_county_map[,.(county, county_mod)]
merged_county_map <- merged_county_map[, county:=paste0(county, " County")]
merged_county_map <- merged_county_map[, county_mod:=paste0(county_mod, " County")]

puma_mapping_out <- merge(puma_mapping_out, merged_county_map, by = "county", all = T)
puma_mapping_out <- unique(puma_mapping_out[,.(id, PUMA, puma_name, race_grp, county_mod)])
puma_mapping_out[, n_counties:=.N, by = c("PUMA", "race_grp")]

cases <- merge(cases, puma_mapping_out, by = c("PUMA", "race_grp", "puma_name"), allow.cartesian = T)
cases <- cases[, tot_cases:=tot_cases/n_counties]
cases <- cases[, tot_tests:=tot_tests/n_counties]

cases <- cases[, .(PUMA, race_grp, tot_persons_point, puma_name, tot_cases, tot_tests, month, crowd_essential_point_perc, id, county_mod, n_counties)]

write.csv(cases, "./county_level_data_split_mod.csv", na = "", row.names = F)

