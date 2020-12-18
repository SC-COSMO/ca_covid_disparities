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

df <- merge(cases, out_race[,.(PUMA, crowd_essential_point_perc, race_grp)], by = c("PUMA", "race_grp"))

county_df <- merge(df, puma_mapping_out, by = c("PUMA", "race_grp", "puma_name"), allow.cartesian = T)

map_df <- fread("./puma_map_race.csv")
map_df <- unique(map_df[,.(id, long, lat, order, hole, piece, group)])

indic_df <- copy(df[month == "March-September"])
indic_df <- indic_df[race_grp!="ALL", pct_race:=tot_persons_point/sum(tot_persons_point), by = "PUMA"]
indic_df <- indic_df[, max_pct_race:=max(pct_race, na.rm=T), by = "PUMA"]
small_df <- indic_df[max_pct_race==pct_race]
small_df <- small_df[,.(race_grp, PUMA)]
setnames(small_df, "race_grp", "max_pct_race_grp")
indic_df <- merge(indic_df, small_df, by = "PUMA")
indic_df <- indic_df[, id:=paste0("6_", PUMA)]
indic_df <- indic_df[race_grp=="ALL"]

for (i in c("crowd_essential_point_perc",
            "case_rate", "test_rate", "tpr")) {
  quant_cut <- quantile(indic_df[[i]], .95)
  #quant_cut_bottom <- quantile(county_df[[i]], .025)
  indic_df <- indic_df[, paste0(i, "_topcoded"):=get(i)]
  indic_df <- indic_df[get(i)>quant_cut, paste0(i, "_topcoded"):=quant_cut]
  #county_df <- county_df[get(i)<quant_cut_bottom, paste0(i, "_topcoded"):=quant_cut_bottom]
  indic_df <- indic_df[, paste0(i, "_topcoded_indic"):=ifelse(get(i)>quant_cut, 1, 0)]
}

map_df <- merge(map_df, indic_df, by = "id", allow.cartesian = T)
map_df <- map_df[, drop:=ifelse(id=="6_3768" & lat<33.5, 1, 0)]
map_df <- map_df[drop == 0]
map_df <- map_df[, drop:=ifelse(id=="6_7501" & long < -122.8, 1, 0)]
map_df <- map_df[drop == 0]

county_outlines <- readRDS("./county_map.RDS")
county_outlines <- county_outlines[county_outlines$STATEFP=="06",]
county_outlines_dt <- fortify(county_outlines, region = "COUNTYFP")
county_outlines_dt <- merge(county_outlines_dt, unique(county_df[,.(county, COUNTYFP)]), by.y="COUNTYFP", by.x="id", all.x=T)
county_outlines_dt <- as.data.table(county_outlines_dt)

county_outlines_dt <- county_outlines_dt[, drop:=ifelse(county=="Los Angeles County" & lat<33.5, 1, 0)]
county_outlines_dt <- county_outlines_dt[drop == 0]
# county_outlines_dt <- county_outlines_dt[, drop:=ifelse(id=="6_7501" & long < -122.8, 1, 0)]
# county_outlines_dt <- county_outlines_dt[drop == 0]
# county_outlines_dt <- county_outlines_dt[, drop:=ifelse(id=="6_11106" & lat<33.5, 1, 0)]
# county_outlines_dt <- county_outlines_dt[drop == 0]

map_df <- map_df[max_pct_race_grp=="Non-Hispanic White", max_pct_race_grp:="White"]
map_df <- map_df[max_pct_race_grp=="Non-Hispanic Black", max_pct_race_grp:="Black"]
map_df <- map_df[max_pct_race_grp=="Hispanic/Latinx", max_pct_race_grp:="Latino"]
map_df <- map_df[max_pct_race_grp=="Other", max_pct_race_grp:="Other"]

# map_df <- map_df[crowd_essential_point_perc_topcoded>.12, crowd_essential_point_perc_topcoded:=.12]
# map_df <- map_df[crowd_essential_point_perc_topcoded>.12, crowd_essential_point_perc_topcoded_indic:=1]

out_plots <- NULL
counter <- 1

temp <- ggplot(data = map_df[id%in%pumas_keep]) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = max_pct_race_grp, alpha = max_pct_race)) +
  theme_void() +
  theme(text = element_text(size = 14), legend.text = element_text(size = 12)) +
  coord_equal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual(values = c("Latino" = "#985BE3", "White" = "#D8B12A", "Other" = "#53B1B6", "Black" = "#F16935")) +
  scale_alpha_continuous(limits = c(min(map_df$max_pct_race), max(map_df$max_pct_race)), labels = scales::percent) + 
  labs(title = " ", fill = "", alpha = "") +
  guides(fill=guide_legend(nrow=4,byrow=TRUE)) +
  guides(alpha=guide_legend(nrow=4,byrow=TRUE)) +
  theme(legend.key.size = unit(.5, "cm"))

out_plots[[counter]] <- get_legend(temp)

counter <- counter+1

temp <- ggplot(data = map_df[id%in%pumas_keep]) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = crowd_essential_point_perc_topcoded)) +
  theme_void() +
  theme(text = element_text(size = 14), legend.text = element_text(size = 12)) +
  coord_equal() +
  scale_fill_distiller(limits = c(min(map_df$crowd_essential_point_perc_topcoded), max(map_df$crowd_essential_point_perc_topcoded)),
                       palette = "Greens", direction = 1, labels = scales::percent) +
  labs(fill = "") +
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"))

out_plots[[counter]] <- get_legend(temp)

counter <- counter+1

temp <- ggplot(data = map_df[id%in%pumas_keep]) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = test_rate_topcoded)) +
  theme_void() +
  theme(text = element_text(size = 14), legend.text = element_text(size = 12)) +
  coord_equal() +
  scale_fill_distiller(limits = c(min(map_df$test_rate_topcoded), max(map_df$test_rate_topcoded)), palette = "Blues", direction = 1) +
  labs(fill = "") +
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"))

out_plots[[counter]] <- get_legend(temp)

counter <- counter+1

temp <- ggplot(data = map_df[id%in%pumas_keep]) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = case_rate_topcoded)) +
  theme_void() +
  theme(text = element_text(size = 14), legend.text = element_text(size = 12)) +
  coord_equal() +
  scale_fill_distiller(limits = c(min(map_df$case_rate_topcoded), max(map_df$case_rate_topcoded)), palette = "Reds", direction = 1) +
  labs(fill = "") +
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"))


out_plots[[counter]] <- get_legend(temp)
counter <- counter+1

regions <- fread("./county_to_region_subset.csv")
regions <- regions[region!="Central Valley"]

for (i in c("California", "Los Angeles")) {
  #if (i != "California") {
  pumas_keep <- unique(puma_mapping$id[puma_mapping$county%in%regions$county[regions$region==i]])
  # } else {
  #   pumas_keep <- unique(puma_mapping$id)
  # 
  # }
  
  out_plots[[counter]] <- ggplot(data = map_df[id%in%pumas_keep]) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = max_pct_race_grp, alpha = max_pct_race)) +
    geom_polygon(data = county_outlines_dt[county%in%regions$county[regions$region==i]], aes(x = long, y = lat, group = group), color = "gray", fill = NA) +
    theme_void() +
    theme(text = element_text(size = 14), legend.text = element_text(size = 12)) +
    coord_equal() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("Latino" = "#985BE3", "White" = "#D8B12A", "Other" = "#53B1B6", "Black" = "#F16935")) +
    scale_alpha_continuous(limits = c(min(map_df$max_pct_race), max(map_df$max_pct_race))) + 
    labs(title = paste0(i))
  
  counter <- counter+1
  
  out_plots[[counter]] <- ggplot(data = map_df[id%in%pumas_keep]) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = crowd_essential_point_perc_topcoded)) +
    geom_polygon(data = county_outlines_dt[county%in%regions$county[regions$region==i]], aes(x = long, y = lat, group = group), color = "gray", fill = NA) +
    theme_void() +
    theme(text = element_text(size = 14), legend.text = element_text(size = 12)) +
    coord_equal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5),) +
    scale_fill_distiller(limits = c(min(map_df$crowd_essential_point_perc_topcoded), max(map_df$crowd_essential_point_perc_topcoded)), palette = "Greens", direction = 1) +
    labs(title = " ")
  
  counter <- counter+1
  
  out_plots[[counter]] <- ggplot(data = map_df[id%in%pumas_keep]) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = test_rate_topcoded)) +
    geom_polygon(data = county_outlines_dt[county%in%regions$county[regions$region==i]], aes(x = long, y = lat, group = group), color = "gray", fill = NA) +
    theme_void() +
    theme(text = element_text(size = 14), legend.text = element_text(size = 12)) +
    coord_equal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5),) +
    scale_fill_distiller(limits = c(min(map_df$test_rate_topcoded), max(map_df$test_rate_topcoded)), palette = "Blues", direction = 1) +
    labs(title = " ")
  
  counter <- counter+1
  
  out_plots[[counter]] <- ggplot(data = map_df[id%in%pumas_keep]) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = case_rate_topcoded)) +
    geom_polygon(data = county_outlines_dt[county%in%regions$county[regions$region==i]], aes(x = long, y = lat, group = group), color = "gray", fill = NA) +
    theme_void() +
    theme(text = element_text(size = 14), legend.text = element_text(size = 12)) +
    coord_equal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5),) +
    scale_fill_distiller(limits = c(min(map_df$case_rate_topcoded), max(map_df$case_rate_topcoded)), palette = "Reds", direction = 1) +
    labs(title = " ")
  counter <- counter+1
}


pdf("./figure1_redistributed.pdf", height = 10, width = 20)
grid.arrange(out_plots[[1]], out_plots[[2]], out_plots[[3]], out_plots[[4]],
             out_plots[[5]], out_plots[[6]], out_plots[[7]], out_plots[[8]],
             out_plots[[9]], out_plots[[10]], out_plots[[11]], out_plots[[12]], ncol = 4,
             layout_matrix = rbind(c(NA, NA, 1, 1, NA, NA, 2, 2, NA, NA, 3, 3, NA, NA, 4, 4, NA, NA),
                                   c(NA, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, NA),
                                   c(NA, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, NA),
                                   c(NA, NA, 9, 9, NA, NA, 10, 10, NA, NA, 11, 11, NA, NA, 12, 12, NA, NA)))
dev.off()
