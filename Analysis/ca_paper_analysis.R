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

## ACS ANALYSIS
df <- fread("./usa_00014.csv")

## Cut unnecessary variables
df <- df[,c("REPWTP", "RACED", "HISPAND",
            "EMPSTATD", "LABFORCE", "GQ"):=NULL]

## Essential worker status
essential_list <- readRDS("./est_occ_crit.RDS")
essential_list <- unique(essential_list)

## Deal with codes that are < 6 digits
df <- df[, OCCSOC:=trimws(OCCSOC, which = "both")]
df <- df[, OCCSOC:=sub("0$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("X$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("Y$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("0$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("X$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("Y$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("X$", "", OCCSOC)]
df <- df[OCCSOC=="", OCCSOC:="0"]

df <- merge(df, essential_list, by = "OCCSOC", all.x=T)
df <- df[OCCSOC=="99992", OCCSOC:="0"]

df <- df[is.na(Critical), Critical:=0] # Not critical if not working
df <- df[EMPSTAT!=1, Critical:=0] # Not critical if not working
df <- df[, hh_essential := max(Critical, na.rm=T), by = c("SERIAL")] # Household level characteristic

df <- df[, crit_nowfh:=ifelse(Critical>0 & low_wfh_binary=="LWFH", Critical, 0)]
df <- df[, crit_nowfh_highpp:=ifelse(Critical>0 & low_wfh_binary=="LWFH" & high_pp_binary=="HPP", Critical, 0)]

df <- df[, hh_essential_nowfh := max(crit_nowfh, na.rm=T), by = c("SERIAL")] # Household level characteristic
df <- df[, hh_essential_nowfh_highpp := max(crit_nowfh_highpp, na.rm=T), by = c("SERIAL")] # Household level characteristic

df <- df[, c("EMPSTAT", "OCCSOC", "Occupation Description", "Critical", "occtitle", "OCC"):=NULL]

rm(essential_list)

## BIPOC Status
df <- df[RACE==1 & HISPAN == 0, race_grp:="Non-Hispanic White"]
df <- df[HISPAN %in% c(1, 2, 3, 4), race_grp:="Hispanic/Latinx"]
df <- df[is.na(race_grp) & RACE == 2, race_grp:="Non-Hispanic Black"]
df <- df[is.na(race_grp), race_grp:="Other"]

df <- df[, c("RACE", "HISPAN"):=NULL]


for (i in unique(df$PUMA)) {
  temp <- df[PUMA==i]
  write.csv(temp, paste0("./PUMA_microdata/", i, ".csv"))
}

rm(df, i)

## Loop through states and collapse
files <- list.files("./PUMA_microdata/")

out_race <- NULL
for (f in files) {
  temp <- fread(paste0("./PUMA_microdata/", f))
  temp <- temp[, race_grp:="ALL"]
  temp2 <- fread(paste0("./PUMA_microdata/", f))
  temp <- rbind(temp, temp2)
  rm(temp2)
  temp <- temp[, c("V1", "MULTYEAR", "SERIAL", "PERNUM"):=NULL]
  temp <- melt(temp, id.vars = c("hh_essential", "NUMPREC", "PUMA", "ROOMS", "STATEFIP", "race_grp", "HINSCAID", "HCOVANY", "AGE"))
  
  temp <- temp[, tot_persons_rep:=sum(value), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[ROOMS < NUMPREC, crowd_rep:=sum(value), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[, crowd_rep:=mean(crowd_rep, na.rm=T), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[is.na(crowd_rep), crowd_rep:=0]
  temp <- temp[ROOMS < NUMPREC, crowd_essential_rep:=sum(value*hh_essential), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[, crowd_essential_rep:=mean(crowd_essential_rep, na.rm=T), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[is.na(crowd_essential_rep), crowd_essential_rep:=0]
  temp <- temp[, essential_rep:=sum(value*hh_essential), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[HCOVANY==2, insured_rep:=sum(value), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[, insured_rep:=mean(insured_rep, na.rm=T), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[is.na(insured_rep), insured_rep:=0]
  temp <- temp[HCOVANY==2 & HINSCAID!=2, insured_not_medicaid_rep:=sum(value), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[, insured_not_medicaid_rep:=mean(insured_not_medicaid_rep, na.rm=T), by = c("PUMA", "STATEFIP", "variable", "race_grp")]
  temp <- temp[is.na(insured_not_medicaid_rep), insured_not_medicaid_rep:=0]
    
  # Make a variable with the point estimates using the main weights
  temp <- temp[variable=="PERWT", tot_persons_point:=tot_persons_rep]
  temp <- temp[variable=="PERWT", essential_point:=essential_rep]
  temp <- temp[variable=="PERWT", crowd_essential_point:=crowd_essential_rep]
  temp <- temp[variable=="PERWT", crowd_point:=crowd_rep]
  temp <- temp[variable=="PERWT", insured_point:=insured_rep]
  temp <- temp[variable=="PERWT", insured_not_medicaid_point:=insured_not_medicaid_rep]
  
  # Fill the point estimate for SDR calculation
  temp <- temp[, tot_persons_point:=mean(tot_persons_point, na.rm=T), by = c("PUMA", "STATEFIP", "race_grp")]
  temp <- temp[, essential_point:=mean(essential_point, na.rm=T), by = c("PUMA", "STATEFIP", "race_grp")]
  temp <- temp[, crowd_essential_point:=mean(crowd_essential_point, na.rm=T), by = c("PUMA", "STATEFIP", "race_grp")]

  temp <- temp[, crowd_point:=mean(crowd_point, na.rm=T), by = c("PUMA", "STATEFIP", "race_grp")]
  temp <- temp[, insured_point:=mean(insured_point, na.rm=T), by = c("PUMA", "STATEFIP", "race_grp")]
  temp <- temp[, insured_not_medicaid_point:=mean(insured_not_medicaid_point, na.rm=T), by = c("PUMA", "STATEFIP", "race_grp")]
  
  # Collapse the dataset
  temp <- unique(temp[,.(PUMA, STATEFIP, variable, tot_persons_rep, crowd_rep, crowd_essential_rep, essential_rep,
                         tot_persons_point, essential_point, crowd_essential_point, crowd_point, insured_rep,
                         insured_point, insured_not_medicaid_rep, insured_not_medicaid_point, race_grp)])
  
  # Make percent vars
  for (i in c("crowd", "crowd_essential", "essential", "insured", "insured_not_medicaid")) {
    for (j in c("rep", "point")) {
      temp <- temp[, paste0(i, "_", j, "_perc"):=get(paste0(i, "_", j))/get(paste0("tot_persons_", j))]
    }
  }
  
  # Make squared error
  for (i in c("crowd", "essential", "crowd_essential",  "insured", "insured_not_medicaid")) {
    temp <- temp[, paste0(i, "_square_error_num") := (get(paste0(i, "_rep"))-get(paste0(i, "_point")))^2]
    temp <- temp[, paste0(i, "_square_error_perc") := (get(paste0(i, "_rep_perc"))-get(paste0(i, "_point_perc")))^2]
  }
  
  # Remove the point estimate row
  temp <- temp[variable!="PERWT"]
  
  # Calculate SE and CI
  for (i in c("crowd", "essential", "crowd_essential",  "insured", "insured_not_medicaid")) {
    temp <- temp[, paste0(i, "_se"):=sqrt(sum(get(paste0(i, "_square_error_num")))*4/80), by = c("STATEFIP", "PUMA", "race_grp")]
    temp <- temp[, paste0(i, "_perc_se"):=sqrt(sum(get(paste0(i, "_square_error_perc")))*4/80), by = c("STATEFIP", "PUMA", "race_grp")]
  }
  
  temp <- unique(temp[, .(PUMA, STATEFIP, tot_persons_point, essential_point, crowd_essential_point, crowd_point, crowd_essential_point_perc,
                          essential_point_perc, crowd_point_perc, crowd_se, essential_se, crowd_essential_se, crowd_perc_se,
                          essential_perc_se, crowd_essential_perc_se, insured_point, insured_se, insured_point_perc, insured_perc_se,
                          insured_not_medicaid_point, insured_not_medicaid_point_perc, insured_not_medicaid_perc_se,
                          insured_not_medicaid_se, race_grp)])
  
  out_race <- rbind(out_race, temp, fill = T)
}

write.csv(out_race, "./puma_estimates_race_withse_insurance_multiocc.csv", na = "", row.names = F)

