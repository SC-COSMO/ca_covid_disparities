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
for (i in c("ALL", "Hispanic/Latinx", "Non-Hispanic Black", "Non-Hispanic White", "Other")) {
  temp <- copy(puma_mapping)
  temp <- temp[, race_grp:=i]
  puma_mapping_out<-rbind(puma_mapping_out, temp, fill = T)
}

puma_mapping_out <- puma_mapping_out[STATEFP=="06"]
puma_mapping_out <- puma_mapping_out[, STATEFIP:=as.numeric(STATEFP)]

out_race <- fread("./puma_estimates_race_withse_insurance_multiocc.csv")
cases <- fread("./redistributed_final.csv")

cases <- merge(cases, out_race[,.(PUMA, crowd_essential_point_perc, race_grp)], by = c("PUMA", "race_grp"))
cases <- cases[race_grp!="ALL"]

cases <- cases[month%in%c("April", "May", "June", "July", "August", "September")]

cases <- cases[, tpr_topcoded:=ifelse(tpr>.4, .4, tpr)]
cases <- cases[, crowd_essential_topcoded:=ifelse(crowd_essential_point_perc>.35, .35, crowd_essential_point_perc)]
cases <- cases[, test_rate_topcoded:=ifelse(test_rate>15000, 15000, test_rate)]
cases <- cases[, case_rate_topcoded:=ifelse(case_rate>1500, 1500, case_rate)]

cases <- cases[, topcode_indic:=ifelse(test_rate>15000 |
                                         tpr>.4,
                                       "Topcoded", "Not Topcoded")]

cases <- cases[, month:=factor(month, levels = c("March", "April", "May", "June", "July", "August", "September"))]

cases$row_id <- sample(factor(rep(1:5, length.out=nrow(cases))))

county_df <- merge(cases, puma_mapping_out, by = c("PUMA", "race_grp"), allow.cartesian = T)
regions <- fread("./county_to_region_appendix.csv")
county_df <- merge(county_df, regions, by = "county")
county_df <- unique(county_df[,.(PUMA, race_grp, tot_persons_point, tpr_topcoded, month, test_rate_topcoded,
                                 topcode_indic, region_map, crowd_essential_topcoded, row_id)])
county_df <- county_df[, region_map:=factor(region_map, levels = c("North", "Bay", "Central", "Upper Southern", "Lower Southern"))]

county_df <- county_df[race_grp == "Hispanic/Latinx", race_grp:="Latino"]
county_df <- county_df[race_grp == "Non-Hispanic Black", race_grp:="Black"]
county_df <- county_df[race_grp == "Non-Hispanic White", race_grp:="White"]

county_df <- county_df[, race_grp:=factor(race_grp, levels = c("Latino", "Black", "White", "Other"))]


pdf("./figure4_final.pdf", width = 16, height = 14)
ggplot(data = county_df[month%in%c("May", "July", "September") & !is.na(tpr_topcoded)], aes(x = tpr_topcoded, y = race_grp)) +
  geom_point(data = county_df[month%in%c("May", "July", "September") & !is.na(tpr_topcoded) & row_id==1],
             position = position_jitter(width = 0, height =0.4),
             aes(color = test_rate_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +
  geom_point(data = county_df[month%in%c("May", "July", "September") & !is.na(tpr_topcoded) & row_id==2],
             position = position_jitter(width = 0, height =0.4),
             aes(color = test_rate_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +
  geom_point(data = county_df[month%in%c("May", "July", "September") & !is.na(tpr_topcoded) & row_id==3],
             position = position_jitter(width = 0, height =0.4),
             aes(color = test_rate_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +
  geom_point(data = county_df[month%in%c("May", "July", "September") & !is.na(tpr_topcoded) & row_id==5],
             position = position_jitter(width = 0, height =0.4),
             aes(color = test_rate_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +
  geom_point(data = county_df[month%in%c("May", "July", "September") & !is.na(tpr_topcoded) & row_id==4],
             position = position_jitter(width = 0, height =0.4),
             aes(color = test_rate_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +
  
  theme_bw() + 
  scale_color_gradientn(colors = c( "#03045e", "#0077b6", "#00b4d8", "#90e0ef")) +
  scale_size_continuous(range = c(.01, 8), breaks = c(10, 60, 110)) +
  labs(x = "Test Positivity Rate", y = "", size = "Population\n(Thousands)", color = "Test Rate\n(Per 100,000)", shape = "Topcoding") +
  scale_shape_manual(values = c(16, 18)) +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.key.width = unit(2, "cm")) +
  guides(shape = guide_legend(override.aes = list(size=6))) +
  facet_grid(region_map~month) +
  theme(legend.position = "bottom", legend.box = "vertical")

dev.off()


