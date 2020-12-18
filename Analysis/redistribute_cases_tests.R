rm(list = ls())

library(data.table)

setwd("./")

county_data <- fread("cases_tests_county_march_oct2020-11-30.csv")
county_data <- county_data[race_grp!="ALL"]
setnames(county_data, "county", "county_mod")
puma_data <- fread("./county_level_data_split_mod.csv")
puma_data <- puma_data[race_grp!="ALL"]

for (i in c("tests", "cases")) {
  ## Among those with known county & race, calculate proportions: this approach fixes the distribution before any redistributing happens
  puma_data <- puma_data[!(race_grp%in%c("ALL", "Unknown")),
                         paste0("known_puma_unknown_race_", i):=get(paste0("tot_", i))/sum(get(paste0("tot_", i)), na.rm=T), by = c("PUMA", "county_mod", "month")]
  puma_data <- puma_data[!(race_grp%in%c("ALL", "Unknown")),
                         paste0("unknown_puma_known_race_", i):=get(paste0("tot_", i))/sum(get(paste0("tot_", i)), na.rm=T), by = c("race_grp", "month", "county_mod")]
  puma_data <- puma_data[!(race_grp%in%c("ALL", "Unknown")),
                         paste0("unknown_puma_unknown_race_", i):=get(paste0("tot_", i))/sum(get(paste0("tot_", i)), na.rm=T), by = c("month", "county_mod")]
  
  # Aggregate to county so we can figure out how many county cases to redistribute
  puma_data <- puma_data[, paste0("county_", i, "_from_puma"):=sum(get(paste0("tot_", i))), by = c("county_mod", "month", "race_grp")]
  
  # Make column of unknown race/ethnicity but known PUMA to redistribute
  puma_data <- puma_data[race_grp=="Unknown", paste0("known_puma_unknown_race_toredist_", i):=get(paste0("tot_", i))]
  puma_data <- puma_data[, paste0("known_puma_unknown_race_toredist_", i):=mean(get(paste0("known_puma_unknown_race_toredist_", i)), na.rm=T),
                         by = c("month", "PUMA")]
  
  # Merge in county data *Note for testing this is only going to work for big counties
  setnames(county_data, c(paste0("total_", i)), c(paste0("county_", i)))
  puma_data <- merge(county_data[,c("county_mod", "race_grp", "month", paste0("county_",i)), with = F], puma_data, by = c("county_mod", "month", "race_grp"))
  
  # Calculate unknown puma, known race to redistribute as difference between county and puma data by race/county/month
  puma_data <- puma_data[, paste0("unknown_puma_known_race_toredist_", i):=get(paste0("county_", i)) - get(paste0("county_", i, "_from_puma"))]
  
  # Calculate unknown puma, unknown race to redistribute 
  puma_data <- puma_data[race_grp=="Unknown", paste0("unknown_puma_unknown_race_toredist_", i):=get(paste0("county_", i)) - get(paste0("county_", i, "_from_puma"))]
  puma_data <- puma_data[, paste0("unknown_puma_unknown_race_toredist_", i):=mean(get(paste0("unknown_puma_unknown_race_toredist_", i)), na.rm=T), by = c("county_mod", "month")]
  
  # Redistribute!
  puma_data <- puma_data[, paste0("final_", i):=get(paste0("tot_", i)) +
             (get(paste0("known_puma_unknown_race_", i))*get(paste0("known_puma_unknown_race_toredist_", i))) +
             (get(paste0("unknown_puma_known_race_", i))*get(paste0("unknown_puma_known_race_toredist_", i))) +
             (get(paste0("unknown_puma_unknown_race_", i))*get(paste0("unknown_puma_unknown_race_toredist_", i)))]
}

puma_data <- puma_data[,.(PUMA, race_grp, tot_persons_point, puma_name, final_cases, final_tests, month)]
puma_data <- puma_data[, final_cases:=sum(final_cases, na.rm=T), by = c("month", "race_grp", "PUMA")]
puma_data <- puma_data[, final_tests:=sum(final_tests, na.rm=T), by = c("month", "race_grp", "PUMA")]
puma_data <- unique(puma_data)
puma_data <- puma_data[race_grp!="Unknown"]

all_race <- copy(puma_data)
all_race <- all_race[, final_cases:=sum(final_cases), by = c("month", "PUMA")]
all_race <- all_race[, final_tests:=sum(final_tests), by = c("month", "PUMA")]
all_race <- all_race[, tot_persons_point:=sum(tot_persons_point), by = c("month", "PUMA")]
all_race <- all_race[, race_grp:="ALL"]
all_race <- unique(all_race)
puma_data <- rbind(puma_data, all_race)

puma_data <- puma_data[, tpr:=final_cases/final_tests]
puma_data <- puma_data[, test_rate:=(final_tests/tot_persons_point)*100000]
puma_data <- puma_data[, case_rate:=(final_cases/tot_persons_point)*100000]
setnames(puma_data, c("final_tests", "final_cases"), c("tot_tests", "tot_cases"))

write.csv(puma_data, "./redistributed_final.csv", na = "", row.names = F)
