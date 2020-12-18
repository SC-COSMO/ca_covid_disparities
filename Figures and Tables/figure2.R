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
cases <- cases[race_grp=="ALL"]

cases <- cases[month%in%c("April", "May", "June", "July", "August", "September")]

cases <- cases[, tpr_topcoded:=ifelse(tpr>.15, .15, tpr)]
cases <- cases[, crowd_essential_topcoded:=ifelse(crowd_essential_point_perc>.3, .3, crowd_essential_point_perc)]
cases <- cases[, test_rate_topcoded:=ifelse(test_rate>15000, 15000, test_rate)]
cases <- cases[, case_rate_topcoded:=ifelse(case_rate>1500, 1500, case_rate)]

cases <- cases[, topcode_indic:=ifelse(crowd_essential_point_perc>.3 |
                                         tpr>.15 |
                                         test_rate > 15000,
                                       "Topcoded", "Not Topcoded")]

cases <- cases[, month:=factor(month, levels = c("March", "April", "May", "June", "July", "August", "September"))]

pdf("./figure2_final.pdf", width = 16, height = 8)

ggplot(data = cases[month%in%c("May", "July", "September") &
                      !is.na(tpr_topcoded)], aes(x = test_rate_topcoded, y = crowd_essential_topcoded)) +
  geom_point(data = cases[month%in%c("May", "July", "September") & !is.na(tpr_topcoded)],
             aes(color = tpr_topcoded,
                 shape = as.factor(topcode_indic)), alpha = .7, size = 5) +
  theme_bw() + 
  scale_color_gradientn(colors = c( "#577590", "#43AA8B", "#90BE6D", "#F9C74F","#F8961E", "#F3722C",  "#F94144"), breaks = c(0, .05, .1, .15, .2),
                        labels = scales::percent) +
  labs(x = "Test Rate (Per 100,000)", y = "Household Exposure Risk", size = "Population (Thousands)", color = "TPR", shape = "Topcoding",
       title = "All Race/Ethnicity Combined") +
  scale_shape_manual(values = c(16, 18)) +
  theme(text = element_text(size = 20)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 5000, 10000, 15000)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.key.width = unit(2, "cm")) +
  guides(shape = guide_legend(override.aes = list(size=6))) +
  facet_wrap(~month, ncol =6) +
  theme(legend.position = "bottom", legend.box = "vertical")

dev.off()

