library(dplyr)
library(readxl)
library(ggplot2)
library(ISOweek)
library(stringr)
library(sf)
library(ggpubr)
library(lme4)
library(lmerTest)
library(maptools)
library(RColorBrewer)
library(sp)
library(classInt)
library(plyr)
library(broom)
library(rgdal)

#clean environment
rm(list = ls())

#set current working directory to the one this script is in (when in RStudio)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)

#load plot themes
source("plot_theme.R")

#load the COVID-19 data
case_dat = read.csv( "../data/symptom_talk_and_covid19_case_data.csv")
mortality_dat = read.csv("../data/symptom_talk_and_excess_mortality_data.csv")

#load the map data
shapefile = readOGR(dsn=path.expand('../../COVID19_data_exploration/data/NUTS_RG_01M_2021_3035.shp'), layer="NUTS_RG_01M_2021_3035")

#check the data
head(shapefile@data)

#reshape the data for ggplot2 using the Brom package
mapdata = tidy(shapefile, region = "NUTS_ID")

#check the data (the region names are missing)
head(mapdata)

#recover NUTS variables 
temp_df = data.frame(NUTS_ID = shapefile@data$NUTS_ID, 
                     NUTS_NAME = shapefile@data$NUTS_NAME,
                     CNTR_CODE = shapefile@data$CNTR_CODE,
                     LEVL_CODE = shapefile@data$LEVL_CODE)

#remanme the columns for merging
names(temp_df) = c("id", "NUTS_NAME", "CNTR_CODE", "LEVL_CODE")

#merge dataframes
clean_mapdata = join(mapdata, temp_df, by = "id")

################################################################################################################################################

### HYPOTHESIS TEST - CHANGES IN SYMPTOM TALK PREDICTING COVID-19 CASES ###

#plot and save case loads by changes in symptom talk
cases_by_symptom_talk = ggplot(data = case_dat, aes(x = percent_change_symptom_talk_past_3_mean, y = cases_in_week_per10k, colour = country)) + 
                          geom_point() + 
                          scale_x_continuous(limits = c(-100, 200), breaks = seq(-100, 200, 50)) +
                          scale_color_hue(labels = c("France", "Germany", "Italy", "Poland", "Spain", "The Netherlands", "United Kingdom")) +
                          xlab("Percent change in symptom talk frequency\n(average of Week 3 in 2017-2019 v. Week 3 in 2020)") +
                          ylab("COVID-19 cases per 10,000 residents") + 
                          labs(colour ="Country") + 
                          avenir_theme

ggsave("../plots/cases_by_symptom_talk.jpg", cases_by_symptom_talk, height = 5, width = 5)

#run model for pre-registered hypotheses (symptom talk change over past three years)
case_mod_past_3 = lmer(cases_in_week_per10k ~ 1 + percent_change_symptom_talk_past_3_mean + (1 | country), data = case_dat)
summary(case_mod_past_3)

#run model for symptom talk change over past two years (removing region with infinite value)
case_mod_past_2 = lmer(cases_in_week_per10k ~ 1 + percent_change_symptom_talk_past_2_mean + (1 | country), data = subset(case_dat, case_dat$nuts_region != "ITF2"))
summary(case_mod_past_2)

### HYPOTHESIS TEST - CHANGES IN SYMPTOM TALK PREDICTING COVID-19 CASES ###

#plot and excess mortality  by changes in symptom talk
mortality_by_symptom_talk = ggplot(data = mortality_dat, aes(x = percent_change_symptom_talk_past_3_mean, y = percentage_increase_mortality, colour = country)) + 
                              geom_point() + 
                              scale_x_continuous(limits = c(-100, 200), breaks = seq(-100, 200, 50)) +
                              scale_color_hue(labels = c("France", "Germany", "Italy", "Poland", "Spain", "The Netherlands", "United Kingdom")) +
                              xlab("Percent change in symptom talk frequency\n(average of Week 3 in 2017-2019 v. Week 3 in 2020)") +
                              ylab("Percent increase in mortality") + 
                              labs(colour ="Country") + 
                              avenir_theme

ggsave("../plots/mortality_by_symptom_talk.jpg", mortality_by_symptom_talk, height = 5, width = 5)

#run model for pre-registered hypotheses (mortality change over past three years)
mortality_mod_past_3 = lmer(percentage_increase_mortality ~ 1 + percent_change_symptom_talk_past_3_mean + (1 | country), data = mortality_dat)
summary(mortality_mod_past_3)

################################################################################################################################################

### PLOT THE MORTALITY DATA BY COUNTRY ###

#load mortality data for plotting
country_map = read.csv("../../COVID19_data_exploration/data/country_mortality_plotting_data_weeks_11_to_15.csv")

#plot the data for specific weeks
country_map_w14 = subset(country_map, country_map$WEEK == "W14")

#save the data for week 14
eu_plot_14 = ggplot() +   
              geom_polygon(data = country_map_w14, 
                           aes(x = long, y = lat, group = group, fill = percentage_increase), color = "#FFFFFF", size = 0.1) + 
              scale_fill_gradient(low = "khaki1", high = "red3", breaks = seq(0, 500, 100), labels = c("0%", "100%", "200%", "300%", "400%", "500%")) +
              theme_classic() + 
              theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line = element_blank()) +
              theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line = element_blank()) +
              labs(fill="Percentage increase\nin mortality rate") +
              coord_fixed(1)

ggsave("../plots/eu_mortality_data_2020_week_14.jpg", eu_plot_14, width = 10, height = 7.5)

################################################################################################################################################

### PLOT COVID-19 CASE LOADS BY COUNTRY ###

#aggregate all case data from Germany to the NUTS 1 level (excess mortality exists only at the NUTS 1 level)
case_dat$nuts_1_region =substr(case_dat$nuts_region, 1, 3)
case_dat$nuts_region_plot = ifelse(case_dat$country == "Germany", 
                                   case_dat$nuts_1_region, 
                                   case_dat$nuts_region)

case_dat_merge = case_dat %>% group_by(nuts_region_plot) %>% dplyr::summarise(cases_in_week_per10k = mean(cases_in_week_per10k))
country_map_cases = merge(country_map_w14, case_dat_merge[ , c("nuts_region_plot", "cases_in_week_per10k")], by.x = "nuts_region", by.y = "nuts_region_plot")

#plot and save the data
eu_plot_cases = ggplot() +   
                  geom_polygon(data = country_map_cases, 
                               aes(x = long, y = lat, group = group, fill = cases_in_week_per10k), color = "#FFFFFF", size = 0.1) + 
                  scale_fill_gradient(low = "khaki1", high = "red3") +
                  theme_classic() + 
                  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line = element_blank()) +
                  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line = element_blank()) +
                  labs(fill="COVID-19 cases\nper 10,000 residents") +
                  coord_fixed(1)

ggsave("../plots/eu_covid19_cases.jpg", eu_plot_cases, width = 10, height = 7.5)

################################################################################################################################################

### PLOT SYMPTOM TALK BY COUNTRY ###

#aggregate all symptom talk data from Germany to the NUTS 1 level (excess mortality exists only at the NUTS 1 level)
case_dat$nuts_1_region =substr(case_dat$nuts_region, 1, 3)
case_dat$nuts_region_plot = ifelse(case_dat$country == "Germany", 
                                   case_dat$nuts_1_region, 
                                   case_dat$nuts_region)

case_dat_merge = case_dat %>% group_by(nuts_region_plot) %>% dplyr::summarise(percent_change_symptom_talk_past_3_mean = mean(percent_change_symptom_talk_past_3_mean))
country_map_cases = merge(country_map_w14, case_dat_merge[ , c("nuts_region_plot", "percent_change_symptom_talk_past_3_mean")], by.x = "nuts_region", by.y = "nuts_region_plot")

#plot and save the data
eu_symptom_talk = ggplot() +   
                    geom_polygon(data = country_map_cases, 
                                 aes(x = long, y = lat, group = group, fill = percent_change_symptom_talk_past_3_mean), color = "#FFFFFF", size = 0.1) + 
                    scale_fill_gradient(low = "khaki1", high = "red3") +
                    theme_classic() + 
                    theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line = element_blank()) +
                    theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line = element_blank()) +
                    labs(fill="Percentage change in\nsymptom talk") +
                    coord_fixed(1)

ggsave("../plots/eu_symptom_talk_change.jpg", eu_symptom_talk, width = 10, height = 7.5)

################################################################################################################################################

