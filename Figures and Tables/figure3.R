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

out_race <- fread("./puma_estimates_race_withse_insurance_multiocc.csv")
cases <- fread("./redistributed_final.csv")

cases <- merge(cases, out_race[,.(PUMA, crowd_essential_point_perc, race_grp)], by = c("PUMA", "race_grp"))
cases <- cases[race_grp!="ALL"]

cases <- cases[month%in%c("April", "May", "June", "July", "August", "September")]

cases <- cases[, tpr_topcoded:=ifelse(tpr>.4, .4, tpr)]
cases <- cases[, crowd_essential_topcoded:=ifelse(crowd_essential_point_perc>.3, .3, crowd_essential_point_perc)]
cases <- cases[, test_rate_topcoded:=ifelse(test_rate>15000, 15000, test_rate)]
cases <- cases[, case_rate_topcoded:=ifelse(case_rate>1500, 1500, case_rate)]

cases <- cases[, topcode_indic:=ifelse(crowd_essential_point_perc>.3 |
                                        tpr>.4,
                                       "Topcoded", "Not Topcoded")]

cases <- cases[, month:=factor(month, levels = c("March", "April", "May", "June", "July", "August", "September"))]

cases$row_id <- sample(factor(rep(1:5, length.out=nrow(cases))))

months <- c("April", "May", "June", "July", "August", "September")
months <- c("May", "July", "September")

cases <- cases[race_grp == "Hispanic/Latinx", race_grp:="Latino"]
cases <- cases[race_grp == "Non-Hispanic Black", race_grp:="Black"]
cases <- cases[race_grp == "Non-Hispanic White", race_grp:="White"]

cases <- cases[, race_grp:=factor(race_grp, levels = c("Latino", "Black", "White", "Other"))]

pdf("./figure3_final.pdf", width = 16, height = 8)

ggplot(data = cases[month%in%months & !is.na(tpr_topcoded)], aes(x = tpr_topcoded, y = race_grp)) +
  geom_point(data = cases[month%in%months& !is.na(tpr_topcoded) & row_id==1],
             position = position_jitter(width = 0, height =0.4),
             aes(color = crowd_essential_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +
  geom_point(data = cases[month%in%months & !is.na(tpr_topcoded) & row_id==2],
             position = position_jitter(width = 0, height =0.4),
             aes(color = crowd_essential_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +
  geom_point(data = cases[month%in%months & !is.na(tpr_topcoded) & row_id==3],
             position = position_jitter(width = 0, height =0.4),
             aes(color = crowd_essential_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +
  geom_point(data = cases[month%in%months & !is.na(tpr_topcoded) & row_id==5],
             position = position_jitter(width = 0, height =0.4),
             aes(color = crowd_essential_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +
  geom_point(data = cases[month%in%months & !is.na(tpr_topcoded) & row_id==4],
             position = position_jitter(width = 0, height =0.4),
             aes(color = crowd_essential_topcoded,
                 size = tot_persons_point/1000,
                 shape = as.factor(topcode_indic)), alpha = .6) +

  theme_bw() + 
  scale_color_gradientn(colors = c( "#577590", "#43AA8B", "#90BE6D", "#F9C74F","#F8961E", "#F3722C",  "#F94144"), breaks = c(0, .1, .2, .3),
                        labels = scales::percent) +
  scale_size_continuous(range = c(.01, 8), breaks = c(10, 60, 110)) +
  labs(x = "Test Positivity Rate", y = "", size = "Population\n(Thousands)", color = "Household\nExposure Risk    ", shape = "Topcoding") +
  scale_shape_manual(values = c(16, 18)) +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.key.width = unit(2, "cm")) +
  guides(shape = guide_legend(override.aes = list(size=6))) +
  facet_wrap(~month) +
  theme(legend.position = "bottom", legend.box = "vertical")

dev.off()

