library(dplyr)
library(readxl)
library(ggplot2)
library(ISOweek)
library(stringr)
library(sf)
library(ggpubr)

#set current working directory to the one this script is in (when in RStudio)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)

#load the COVID-19 data
dat = read.csv("../../COVID19_data_exploration/data/EUROPE_COVID19_master.csv", sep = ";")

#load the NUTS geographical data
nuts_dat = read_excel("../../COVID19_data_exploration/data/NUTS2021.xlsx")

#load mortality data (https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Weekly_death_statistics#Almost_1.2_million_additional_deaths_in_2020-2021_compared_to_2016-2019)
mortality_dat = read.csv("../../COVID19_data_exploration/data/EUROPE_mortality_master.csv")

################################################################################################################################################

### PLOT THEMES ###

#load local functions for making pastelle colours
source("colour_functions.R")

#fonts
quartzFonts(texture_helv  = c('Helvetica-Bold',
                              'Helvetica-Bold',
                              'Helvetica-Oblique',
                              'Helvetica-BoldOblique'))

#theme
header_size = 10
axis_size = 10
x_axis_size = 8

#theme for scatter plots
point_plot = theme(text=element_text(size=header_size,family='texture_helv'),
                   axis.text.x = element_text(color='black',size = x_axis_size, vjust = 1),
                   axis.text.y = element_text(color='black',size = axis_size),
                   axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                   axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
                   panel.background = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.major.y = element_line(color = '#e7e7e7'),
                   legend.key = element_blank()
)

point_plot2 = theme(text=element_text(size=header_size,family='texture_helv'),
                    axis.text.x = element_text(color='black',size = x_axis_size, vjust = 1, angle = 45),
                    axis.text.y = element_text(color='black',size = axis_size),
                    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                    axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
                    panel.background = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(color = '#e7e7e7'),
                    legend.key = element_blank()
)

point_plot3 = theme(axis.text.x = element_text(color='black',size = x_axis_size, vjust = 1, angle = 45),
                    axis.text.y = element_text(color='black',size = axis_size),
                    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                    axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
                    panel.background = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(color = '#e7e7e7'),
                    legend.key = element_blank()
)

#set the colors for each country
France = pastellize("pink", .75)
Germany = pastellize("black", .6)
Italy = pastellize("darkgreen", .6)
Netherlands = pastellize("orange", 1)
Poland = pastellize("red", .75)
Spain = pastellize("gold", .75)
UK = pastellize("navy", .7)
country_cols = c(France, Germany, Italy, Netherlands, Poland, Spain, UK)

################################################################################################################################################

### DATA CLEANING ###

#convert factor data to characters
dat$nuts3_id = as.character(dat$nuts3_id)
dat$nuts2_id = as.character(dat$nuts2_id)

#convert the date to a datetime object (first fix the '2121' error)
dat$date = gsub('-2121', '-2021', dat$date)
dat$date_clean = as.Date(dat$date, format = "%d-%b-%Y")

################################################################################################################################################

### DATA CREATION ###

#create a NUTS 2 region variable (remove the last character, which specifies the NUTS 3 region)
dat$nuts2_clean = substr(dat$nuts3_id,1,nchar(dat$nuts3_id)-1)

#add the NUTS 2 region data for Greece and Poland (where there was no NUTS 3 data)
dat$nuts2_clean = ifelse(dat$country == "Greece", dat$nuts2_id,
                         ifelse(dat$country == "Poland", dat$nuts2_id, dat$nuts2_clean))

#this provides data for 275 regions
length(table(dat$nuts2_clean))

################################################################################################################################################

### DATA EXPLORATION ###

#subset the dataset to only those countries for which Twitter data was collected
table(dat$country)
twitter_dat_countries = c("England (UK)", "Scotland (UK)", "Germany", "France", "Italy", "Spain", "Poland", "Netherlands")
twitter_countries = subset(dat, dat$country %in% twitter_dat_countries)

#aggregate data from NUTS regions
nuts2_day_dat = twitter_countries %>% 
                  group_by(date_clean, nuts2_clean) %>%
                  dplyr::summarize(nuts_2_population = sum(population),
                            cumulative_cases = sum(cases),
                            daily_cases = sum(cases_daily))

#check how many NUTS 2 regions exist for these countries (there are 166)
length(unique(nuts2_day_dat$nuts2_clean))

#create a country variable (remove the last character, which specifies the NUTS 3 region)
nuts2_day_dat$country_code = substr(nuts2_day_dat$nuts2_clean,1,2)
nuts2_day_dat$country = ifelse(nuts2_day_dat$country_code == "DE", "Germany",
                               ifelse(nuts2_day_dat$country_code == "ES", "Spain",
                                      ifelse(nuts2_day_dat$country_code == "FR", "France",
                                             ifelse(nuts2_day_dat$country_code == "IT", "Italy",
                                                    ifelse(nuts2_day_dat$country_code == "NL", "Netherlands",
                                                           ifelse(nuts2_day_dat$country_code == "PL", "Poland",
                                                                  ifelse(nuts2_day_dat$country_code == "UK", "United Kingdom", nuts2_day_dat$country_code)))))))
table(nuts2_day_dat$country_code)
table(nuts2_day_dat$country)

#create a daily cases per 10,000 variable
nuts2_day_dat$daily_cases_per10k = (nuts2_day_dat$daily_cases / nuts2_day_dat$nuts_2_population) * 10000

### FURTHER DATA CLEANING ###

#check how many daily cases are negative (an error resulting from corrections to the data), and remove them
nrow(subset(nuts2_day_dat, nuts2_day_dat$daily_cases < 0)) / nrow(subset(nuts2_day_dat, nuts2_day_dat$daily_cases >= 0)) * 100
nuts2_day_dat = subset(nuts2_day_dat, nuts2_day_dat$daily_cases >= 0)

#remove the date "2020-04-08" from all Dutch observations (there was a week skipped in the data so the daily cases is inaccurate when added)
nuts2_day_dat = subset(nuts2_day_dat, !(nuts2_day_dat$date_clean == as.Date("2020-04-08") & nuts2_day_dat$country_code == "NL"))

### PLOT THE DATA ###

#reduce the data to the first four months
nuts2_day_dat_first_six = subset(nuts2_day_dat, nuts2_day_dat$date_clean < as.Date("2020-07-01"))

#replace outliers
nuts2_day_dat_first_six$daily_cases_per10k = ifelse(nuts2_day_dat_first_six$daily_cases_per10k > 50, 30, nuts2_day_dat_first_six$daily_cases_per10k)

#plot the data for daily cases per 10,000 residents and save it
cases_by_time = ggplot(data = nuts2_day_dat_first_six, aes(x = date_clean, y = daily_cases_per10k, color = country)) +
                  geom_point(alpha = .5, size = 1) + 
                  scale_color_manual(values = country_cols) + 
                  ylab("Daily cases per 10,000 residents") +
                  xlab("2020") +
                  guides(colour=guide_legend(title="Country")) +
                  point_plot

ggsave("../plots/cases_by_time_2020_first_half.jpg", cases_by_time, width = 10, height = 7.5)

### ### ###

#find the last date in the dataset where a region has no cases ("2020-03-19" with the exception of Corsica)
no_cases = subset(nuts2_day_dat_first_six, nuts2_day_dat_first_six$cumulative_cases == 0)

#get the first appearance of each country in the dataset
countries = c("France", "Germany", "Italy", "Netherlands", "Poland", "Spain", "United Kingdom")
for (c in countries){
  cdat = subset(nuts2_day_dat, nuts2_day_dat$country == c)
  first_date = min(cdat$date_clean)
  print(paste0("Country: ", c, " | First date: ", first_date))
}

#examine the data of different countries
italy_dat = subset(nuts2_day_dat, nuts2_day_dat$country == "Italy")
france_dat = subset(nuts2_day_dat, nuts2_day_dat$country == "France")
poland_dat = subset(nuts2_day_dat, nuts2_day_dat$country == "Poland")

#get the number of regions with no cases by data
no_cases_FEB1 = subset(nuts2_day_dat, nuts2_day_dat$date_clean == as.Date("2020-02-01") & nuts2_day_dat$cumulative_cases == 0)
nrow(no_cases_FEB1)

no_cases_FEB25 = subset(nuts2_day_dat, nuts2_day_dat$date_clean == as.Date("2020-02-25") & nuts2_day_dat$cumulative_cases == 0)
nrow(no_cases_FEB25)

no_cases_MAR20 = subset(nuts2_day_dat, nuts2_day_dat$date_clean == as.Date("2020-03-20") & nuts2_day_dat$cumulative_cases == 0)
nrow(no_cases_MAR20)

### ### ###

#get the date on which all regions of a country are present in a dataset
for (c in countries){
  
  #subset to country data
  cdat = subset(nuts2_day_dat, nuts2_day_dat$country == c)
  
  print(paste0("Country: ", c))
  
  #get the total number of regions in the country
  total_regions = length(table(cdat$nuts2_clean))
  
  #get region counts by date for the country
  for (date in unique(cdat$date_clean)){
    
    #subset by date
    date_dat = subset(cdat, cdat$date_clean == date)
    
    #get the total number of regions in the country for that date
    total_regions_cur_date = length(table(date_dat$nuts2_clean))
    
    #check if it matches the total regions for the country
    if (total_regions_cur_date == total_regions){
      print(paste0("Date: ", as.Date(date, origin = "1970-01-01"), " | ", "Total regions: ", total_regions_cur_date))
      break
    }
  }
}

### ### ###

#get the date on which all regions of a country are present in a dataset
for (c in countries){
  
  #subset to country data
  cdat = subset(nuts2_day_dat, nuts2_day_dat$country == c)
  
  print(paste0("Country: ", c))
  
  #get the total number of regions in the country
  total_regions = length(table(cdat$nuts2_clean))
  
  #get region counts by date for the country
  for (date in unique(cdat$date_clean)){
    
    #subset by date
    date_dat = subset(cdat, cdat$date_clean == date)
    
    #get the total number of regions in the country for that date
    total_regions_cur_date = length(table(date_dat$nuts2_clean))
    
    #check if it matches the total regions for the country
    if (total_regions_cur_date == total_regions){
      print(paste0("Date: ", as.Date(date, origin = "1970-01-01"), " | ", "Total regions: ", total_regions_cur_date))
      break
    }
  }
}

################################################################################################################################################

### GET FIRST WEEK OF FULL DATA FOR EACH COUNTRY ###

#create a total dataframe and counter
total_data_list = list()
counter = 1

#get the date on which all regions of a country are present in a dataset
for (c in countries){
  
  #subset to country data
  cdat = subset(nuts2_day_dat, nuts2_day_dat$country == c)
  
  #track progress
  print(paste0("Country: ", c))
  
  #get the total number of regions in the country
  total_regions = length(table(cdat$nuts2_clean))
  
  #get region counts by date for the country
  for (date in unique(cdat$date_clean)){
    
    #subset by date
    date_dat = subset(cdat, cdat$date_clean == date)
    
    #get the total number of regions in the country for that date
    total_regions_cur_date = length(table(date_dat$nuts2_clean))
    
    #check if it matches the total regions for the country
    if (total_regions_cur_date == total_regions){
      
      #track progress
      print(paste0("Date: ", as.Date(date, origin = "1970-01-01"), " | ", "Total regions: ", total_regions_cur_date))
      
      #subset the country dataset to include the first week of data after all country data became available
      time_span = date + 7
      span_subset = subset(cdat, cdat$date_clean >= date & cdat$date_clean < time_span)
      
      #the data from Spain should start on 25 February 2020, the same date as the next earliest country, Italy (the data from the previous month is too sparse with almost no cases)
      if (c == "Spain"){
        spain_start = as.Date("2020-02-25", format="%Y-%m-%d")
        time_span = spain_start + 7
        span_subset = subset(cdat, cdat$date_clean >= spain_start & cdat$date_clean < time_span)
        
      }
      
      #add to the total data list and counter
      total_data_list[[counter]] = span_subset
      counter = counter + 1
      
      break
    }
  }
}

#combine the data
total_data = do.call(rbind, total_data_list)

### ### ###

library(dplyr)

#get the total case counts for each region for each day
first_week_data = total_data %>% group_by(nuts2_clean) %>% dplyr::summarise(country = first(country),
                                                                     country_code = first(country_code),
                                                                     start_date = as.Date(min(date_clean), origin = "1970-01-01"),
                                                                     end_date = as.Date(max(date_clean), origin = "1970-01-01"),
                                                                     cases_in_week = sum(daily_cases),
                                                                     nuts_2_population = min(nuts_2_population),
                                                                     total_cases_reported = max(cumulative_cases))

#create a variable that is the number of cases per 10,000 residents
first_week_data$cases_in_week_per10k = (first_week_data$cases_in_week / first_week_data$nuts_2_population) * 10000

### ### ###

#plot the data for weekly cases per 10,000 residents by date of data appearance and save it
cases_by_start_date = ggplot(data = first_week_data, aes(x = start_date, y = cases_in_week_per10k, color = country)) +
                        geom_point(alpha = .5, size = 1) + 
                        scale_color_manual(values = country_cols) + 
                        ylab("Case per 10,000 residents in first week of data availability") +
                        xlab("2020") +
                        scale_x_date(limits = c(min(first_week_data$start_date), max(first_week_data$end_date)),
                                     breaks = c(as.Date("2020-02-25"), as.Date("2020-03-03"), as.Date("2020-03-10"), as.Date("2020-03-20"), as.Date("2020-03-06")),
                                     labels = c("FEB 25", "MAR 3", "MAR 6", "MAR 10", "MAR 20")) +
                        guides(colour=guide_legend(title="Country")) + 
                        point_plot3

ggsave("../plots/cases_per_10k_residents_first_week_of_data.jpg", cases_by_start_date, width = 10, height = 7.5)

################################################################################################################################################

### SAVE DATA ###

#save the data
write.csv(first_week_data, "../data/centred_first_week_COVID_19_case_data.csv", row.names = FALSE)

################################################################################################################################################

### MORTALITY DATA EXPLORATION ###

#examine the excess mortality data
table(mortality_dat$DATAFLOW)
table(mortality_dat$unit)
table(mortality_dat$geo)

#get a list of all the regions to be analysed
regions = unique(first_week_data$nuts2_clean)
length(regions)

#convert the region variable and date period to a character string
mortality_dat$nuts_region = as.character(mortality_dat$geo)
mortality_dat$TIME_PERIOD = as.character(mortality_dat$TIME_PERIOD)

#add a week to the ISO 8601 week, and a separate week variable
mortality_dat$TIME_PERIOD_FULL = paste0(mortality_dat$TIME_PERIOD, "-1")
mortality_dat$WEEK = unlist(lapply(strsplit(mortality_dat$TIME_PERIOD, "-"), '[[', 2))

### ### ###

#use data from only the seven countries being studied
mortality_dat_subset = subset(mortality_dat, mortality_dat$nuts_region %in% regions)

#check to make sure all regions are present
length(unique(mortality_dat_subset$nuts_region))
length(unique(first_week_data$nuts2_clean))

#find the missing regions
'%!in%' <- function(x,y)!('%in%'(x,y))
for (g in regions){
  if (g %!in% unique(mortality_dat_subset$nuts_region)){
    print(g)
  }
}

#data from Germany is only available at the NUTS 1 (state) level; there are 16
german_states = c("DE1", "DE2", "DE3", "DE4", "DE5", "DE6", "DE7", "DE8", "DE9", "DEA", "DEB", "DEC", "DED", "DEE", "DEF", "DEG")

#get the German data
mortality_dat_germany = subset(mortality_dat, mortality_dat$nuts_region %in% german_states)

#combine the two datasets
mortality_final = rbind(mortality_dat_subset, mortality_dat_germany)

### ### ###

#convert the IS0 8601 week to a regular date
mortality_final$week_start = ISOweek2date(mortality_final$TIME_PERIOD_FULL)
table(mortality_final$TIME_PERIOD_FULL)
table(mortality_final$week_start)

#list of weeks to include in analyses (https://www.epochconverter.com/weeks/2020)
weeks_to_keep = c("2016-W01-1", "2016-W02-1", "2016-W03-1", "2016-W04-1", "2016-W05-1", "2016-W06-1", "2016-W07-1",
                  "2016-W08-1", "2016-W09-1", "2016-W10-1", "2016-W11-1", "2016-W12-1", "2016-W13-1", "2016-W14-1",
                  "2016-W15-1", "2016-W16-1", "2016-W17-1", "2016-W18-1", "2016-W19-1", "2016-W20-1", "2016-W21-1",
                  "2017-W01-1", "2017-W02-1", "2017-W03-1", "2017-W04-1", "2017-W05-1", "2017-W06-1", "2017-W07-1",
                  "2017-W08-1", "2017-W09-1", "2017-W10-1", "2017-W11-1", "2017-W12-1", "2017-W13-1", "2017-W14-1",
                  "2017-W15-1", "2017-W16-1", "2017-W17-1", "2017-W18-1", "2017-W19-1", "2017-W20-1", "2017-W21-1",
                  "2018-W01-1", "2018-W02-1", "2018-W03-1", "2018-W04-1", "2018-W05-1", "2018-W06-1", "2018-W07-1",
                  "2018-W08-1", "2018-W09-1", "2018-W10-1", "2018-W11-1", "2018-W12-1", "2018-W13-1", "2018-W14-1",
                  "2018-W15-1", "2018-W16-1", "2018-W17-1", "2018-W18-1", "2018-W19-1", "2018-W20-1", "2018-W21-1",
                  "2019-W01-1", "2019-W02-1", "2019-W03-1", "2019-W04-1", "2019-W05-1", "2019-W06-1", "2019-W07-1",
                  "2019-W08-1", "2019-W09-1", "2019-W10-1", "2019-W11-1", "2019-W12-1", "2019-W13-1", "2019-W14-1",
                  "2019-W15-1", "2019-W16-1", "2019-W17-1", "2019-W18-1", "2019-W19-1", "2019-W20-1", "2019-W21-1",
                  "2020-W01-1", "2020-W02-1", "2020-W03-1", "2020-W04-1", "2020-W05-1", "2020-W06-1", "2020-W07-1",
                  "2020-W08-1", "2020-W09-1", "2020-W10-1", "2020-W11-1", "2020-W12-1", "2020-W13-1", "2020-W14-1",
                  "2020-W15-1", "2020-W16-1", "2020-W17-1", "2020-W18-1", "2020-W19-1", "2020-W20-1", "2020-W21-1")

#drop dates that will not be used 
mortality_final = subset(mortality_final, mortality_final$TIME_PERIOD_FULL %in% weeks_to_keep)

### ### ###

#create a dataset with average deaths per week by region for 2016-2019
mortality_2016_2019 = subset(mortality_final, mortality_final$TIME_PERIOD_FULL %in% weeks_to_keep[1:84]) 
mortality_2016_2019_summary = mortality_2016_2019 %>% group_by(nuts_region, WEEK) %>% summarise(mean_weekly_deaths = as.integer(mean(OBS_VALUE)))

#add a time period variable to this dataset
mortality_2016_2019_summary$time_period = "2016-2019"

#combine this dataframe with a dataframe with 2020 statistics (long form)
mortality_final_2020 = subset(mortality_final, mortality_final$TIME_PERIOD_FULL %in% weeks_to_keep[85:length(weeks_to_keep)])
mortality_final_2020$mean_weekly_deaths = mortality_final_2020$OBS_VALUE
mortality_final_2020$time_period = "2020"
rownames(mortality_final_2020) = NULL
mortality_final_2020_sub =  mortality_final_2020[, c("nuts_region", "WEEK", "mean_weekly_deaths", "time_period")]
mortality_final_long = rbind(as.data.frame(mortality_2016_2019_summary), mortality_final_2020_sub)

#combine this dataframe with a dataframe with 2020 statistics (long form)
mortality_final_2020_sub$weekly_deaths_2020 = mortality_final_2020_sub$mean_weekly_deaths
mortality_2016_2019_summary$mean_weekly_deaths_2016_2019 = mortality_2016_2019_summary$mean_weekly_deaths

mortality_increase_dat = merge(mortality_final_2020_sub[, c("nuts_region", "WEEK", "weekly_deaths_2020")],
                               mortality_2016_2019_summary[, c("nuts_region", "WEEK", "mean_weekly_deaths_2016_2019")],
                               by = c('nuts_region', 'WEEK'), all=TRUE)

#create change and percentage change in mortality variables
mortality_increase_dat$mortality_change = mortality_increase_dat$weekly_deaths_2020 - mortality_increase_dat$mean_weekly_deaths_2016_2019
mortality_increase_dat$percentage_increase = (mortality_increase_dat$mortality_change / mortality_increase_dat$mean_weekly_deaths_2016_2019) * 100
hist(mortality_increase_dat$mortality_change, breaks = 50)
hist(mortality_increase_dat$percentage_increase, breaks = 50)

################################################################################################################################################

### PLOT THE DATA BY COUNTRY ###

library(maptools)
library(RColorBrewer)
library(sp)
library(classInt)
library(rgal)
library(plyr)
library(broom)
library(rgdal)

#load the map data
shapefile = readOGR(dsn=path.expand('../data/NUTS_RG_01M_2021_3035.shp'), layer="NUTS_RG_01M_2021_3035")

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

### ### ###

country_codes = c("DE", "FR", "UK", "IT", "ES", "NL", "PL")

plots = c()
counter = 1

#plot all countries at once
for (country in country_codes){
  
  #subset to the country data
  if (country == "DE"){
    country_week_dat = subset(clean_mapdata, clean_mapdata$CNTR_CODE == country & clean_mapdata$LEVL_CODE == 1)
  } else{
    country_week_dat = subset(clean_mapdata, clean_mapdata$CNTR_CODE == country & clean_mapdata$LEVL_CODE == 2)
  }
  
  #remove French overseas regions (and Corsica for plotting)
  if (country == "FR"){
    country_week_dat = subset(country_week_dat, country_week_dat$id %!in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRM0"))
  }
  
  #remove the Canary Islands (for plotting)
  if (country == "ES"){
    country_week_dat = subset(country_week_dat, country_week_dat$id %!in% c("ES70"))
  } 
  
  #select a week to plot for the mortality data - week 13 of 2020 is the beginning of the first wave in Europe
  specific_week = "W15"
  mortality_week_dat = subset(mortality_increase_dat, mortality_increase_dat$nuts_region %in% unique(country_week_dat$id) & mortality_increase_dat$WEEK == specific_week)
  
  #join the datasets to plot
  mortality_week_dat$id = mortality_week_dat$nuts_region
  country_map = join(country_week_dat, mortality_week_dat, by="id")
  
  #plot the data
  country_plot =  ggplot() +   
    geom_polygon(data = country_map, 
                 aes(x = long, y = lat, group = group, fill = percentage_increase), color = "#FFFFFF", size = 0.25) + 
    scale_fill_gradient(low = "khaki1", high = "red3") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    ggtitle(country) + 
    labs(fill="Percentage increase\nin death rate") +
    coord_fixed(1)
  
  #add the plot to the list
  plots[[counter]] = country_plot
  
  counter = counter + 1
  
}

#show the data
ggarrange(plotlist = plots, widths = c(3,3))

### ### ###

#create a dataset for each country
spain_dat = subset(clean_mapdata, clean_mapdata$CNTR_CODE == "ES" & clean_mapdata$LEVL_CODE == 2)
italy_dat = subset(clean_mapdata, clean_mapdata$CNTR_CODE == "IT" & clean_mapdata$LEVL_CODE == 2)
uk_dat = subset(clean_mapdata, clean_mapdata$CNTR_CODE == "UK" & clean_mapdata$LEVL_CODE == 2)

#select a week to plot for the mortality data - week 13 of 2020 is the beginning of the first wave in Europe
specific_week = "W14"
mortality_dat_spain = subset(mortality_increase_dat, mortality_increase_dat$nuts_region %in% unique(spain_dat$id) & mortality_increase_dat$WEEK == "W14")
mortality_dat_italy = subset(mortality_increase_dat, mortality_increase_dat$nuts_region %in% unique(italy_dat$id) & mortality_increase_dat$WEEK == "W13")
mortality_dat_uk = subset(mortality_increase_dat, mortality_increase_dat$nuts_region %in% unique(uk_dat$id) & mortality_increase_dat$WEEK == "W16")

#join the datasets to plot
mortality_dat_spain$id = mortality_dat_spain$nuts_region
mortality_dat_italy$id = mortality_dat_italy$nuts_region
mortality_dat_uk$id = mortality_dat_uk$nuts_region

spain_map = join(spain_dat, mortality_dat_spain, by="id")
italy_map = join(italy_dat, mortality_dat_italy, by="id")
uk_map = join(uk_dat, mortality_dat_uk, by="id")

#plot and save Spain data (first remove the Canary Islands and  Balearic Islands)
spain_map = subset(spain_map, spain_map$id %!in% c("ES70", "ES53"))
spain_plot =  ggplot() +   
                geom_polygon(data = spain_map, 
                             aes(x = long, y = lat, group = group, fill = percentage_increase), color = "#FFFFFF", size = 0.25) + 
                scale_fill_gradient(low = "khaki1", high = "red3", limits = c(0, 520), breaks = c(0,100,200,300,400,500), labels = c("0%", "100%", "200%", "300%", "400%", "500%")) +
                theme_classic() + 
                theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line = element_blank()) +
                theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line = element_blank()) +
                ggtitle("Spain: deaths in week 14 of 2020\n(compared to 2016-2019 week 14 average)") + 
                labs(fill="Percentage\nincrease\nin deaths") +
                coord_fixed(1)

ggsave("../plots/spain_mortality_data_2020_week_14.jpg", spain_plot, width = 5, height = 5)

#plot and save the Italy data
italy_plot =  ggplot() +
                geom_polygon(data = italy_map, 
                             aes(x = long, y = lat, group = group, fill = percentage_increase), color = "#FFFFFF", size = 0.25) + 
                scale_fill_gradient(low = "khaki1", high = "red3", breaks = c(0,100,200,300), labels = c("0%", "100%", "200%", "300%")) +
                theme_classic() + 
                theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line = element_blank()) +
                theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line = element_blank()) +
                ggtitle("Italy: deaths in week 13 of 2020\n(compared to 2016-2019 week 13 average)") + 
                labs(fill="Percentage\nincrease\nin deaths") +
                coord_fixed(1)

ggsave("../plots/italy_mortality_data_2020_week_13.jpg", italy_plot, width = 5, height = 5)

#plot and save the UK data
uk_plot =  ggplot() +
            geom_polygon(data = uk_map, 
                         aes(x = long, y = lat, group = group, fill = percentage_increase), color = "#FFFFFF", size = 0.25) + 
            scale_fill_gradient(low = "khaki1", high = "red3", limits = c(0,320), breaks = c(0,100,200,300), labels = c("0%", "100%", "200%","300%")) +
            theme_classic() + 
            theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line = element_blank()) +
            theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line = element_blank()) +
            ggtitle("UK: deaths in week 16 of 2020\n(compared to 2016-2019 week 16 average)") + 
            labs(fill="Percentage\nincrease\nin deaths") +
            coord_fixed(1)

ggsave("../plots/uk_mortality_data_2020_week_16.jpg", uk_plot, width = 5, height = 5)

################################################################################################################################################

### SAVE DATA ###

#load the map data
shapefile = readOGR(dsn=path.expand('../data/NUTS_RG_01M_2021_3035.shp'), layer="NUTS_RG_01M_2021_3035")

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

#subset create subset of the country data
all_country_dat = subset(clean_mapdata, clean_mapdata$CNTR_CODE %in% c("FR", "UK", "IT", "ES", "NL", "PL") & clean_mapdata$LEVL_CODE == 2)
germany_dat = subset(clean_mapdata, clean_mapdata$CNTR_CODE == "DE" & clean_mapdata$LEVL_CODE == 1)
all_country_dat = rbind(all_country_dat, germany_dat)

#drop overseas territories
all_country_dat = subset(all_country_dat, all_country_dat$id %!in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRM0", "ES70"))

#select a week to plot for the mortality data - week 13 of 2020 is the beginning of the first wave in Europe
specific_weeks = c("W11", "W12", "W13", "W14", "W15")
mortality_week_dat = subset(mortality_increase_dat, mortality_increase_dat$nuts_region %in% unique(all_country_dat$id) & mortality_increase_dat$WEEK %in% specific_weeks)

#join the datasets to plot
mortality_week_dat$id = mortality_week_dat$nuts_region
country_map = join(all_country_dat, mortality_week_dat, by="id")

#check the data
ggplot() + geom_polygon(data = country_map, aes(x = long, y = lat, group = group), color = "#FFFFFF", size = 0.25)

#add country data to the mortality data
mortality_week_dat$country = ifelse(startsWith(mortality_week_dat$nuts_region, "DE"), "Germany",
                                    ifelse(startsWith(mortality_week_dat$nuts_region, "FR"), "France",
                                           ifelse(startsWith(mortality_week_dat$nuts_region, "IT"), "Italy",
                                                  ifelse(startsWith(mortality_week_dat$nuts_region, "ES"), "Spain",
                                                         ifelse(startsWith(mortality_week_dat$nuts_region, "NL"), "The Netherlands",
                                                                ifelse(startsWith(mortality_week_dat$nuts_region, "PL"), "Poland",
                                                                       ifelse(startsWith(mortality_week_dat$nuts_region, "UK"), "United Kingdom", NA)))))))
#save the data
write.csv(mortality_week_dat, "../data/country_mortality_data_weeks_11_to15.csv", row.names = FALSE)
write.csv(country_map, "../data/country_mortality_plotting_data_weeks_11_to_15.csv", row.names = FALSE)

################################################################################################################################################

### COMBINE DATA ###

#read the COVID-19 data
mortality_week_dat = read.csv("../data/country_mortality_data_weeks_11_to15.csv")
first_week_data = read.csv("../data/centred_first_week_COVID_19_case_data.csv")

#subset the mortality data to the week of interest
mortality_week14_dat = subset(mortality_week_dat, mortality_week_dat$WEEK  == "W14")

#read the Twitter symptom talk data
france_dat = read.csv("../data/France.csv")
france_dat = subset(france_dat, startsWith(france_dat$nuts_2_region, "FR"))

germany_dat = read.csv("../data/Germany.csv")
germany_dat = subset(germany_dat, startsWith(germany_dat$nuts_2_region, "DE"))

italy_dat = read.csv("../data/Italy.csv")
italy_dat = subset(italy_dat, startsWith(italy_dat$nuts_2_region, "IT"))

poland_dat = read.csv("../data/Poland.csv")
poland_dat = subset(poland_dat, startsWith(poland_dat$nuts_2_region, "PL"))

spain_dat = read.csv("../data/Spain.csv")
spain_dat = subset(spain_dat, startsWith(spain_dat$nuts_2_region, "ES"))

netherlands_dat = read.csv("../data/The_Netherlands.csv")
netherlands_dat = subset(netherlands_dat, startsWith(netherlands_dat$nuts_2_region, "NL"))

united_kingdom_dat = read.csv("../data/United_Kingdom.csv")
united_kingdom_dat = subset(united_kingdom_dat, startsWith(united_kingdom_dat$nuts_2_region, "UK"))

#combine the dataframes
symp_dat = do.call("rbind", list(france_dat, germany_dat, italy_dat, poland_dat, spain_dat, netherlands_dat, united_kingdom_dat))

### ### ###

#not in function
'%!in%' = function(x,y)!('%in%'(x,y))

#get Week 3 data (the week to be compared over time)
symp_dat_week3 = subset(symp_dat, symp_dat$week == "W3")

#get the average symptom mention rate for 2017, 2018, and 2019
symp_dat_week3_non_covid19 = subset(symp_dat_week3, symp_dat_week3$year != 2020)
symp_dat_week3_non_covid19 = symp_dat_week3_non_covid19 %>% group_by(nuts_2_region) %>% dplyr::summarise(mean_symp_talk_week3_2017_to_2019 = mean(sick_talk_proportion),
                                                                                                         country = first(country), 
                                                                                                         mean_tweets_week3_2017_to_2019 = mean(num_tweets))

#get the average symptom mention rate for 2018 and 2019
symp_dat_week3_non_covid19_2018_2019 = subset(symp_dat_week3, symp_dat_week3$year %in% c(2018, 2019))
symp_dat_week3_non_covid19_2018_2019 = symp_dat_week3_non_covid19_2018_2019 %>% group_by(nuts_2_region) %>% dplyr::summarise(mean_symp_talk_week3_2018_2019 = mean(sick_talk_proportion),
                                                                                                                             country = first(country), 
                                                                                                                             mean_tweets_week3_2018_2019 = mean(num_tweets))

#combine with data from 2020
symp_dat_week3_covid19 = subset(symp_dat_week3, symp_dat_week3$year == 2020)
symp_dat_week3_covid19 = merge(symp_dat_week3_covid19, symp_dat_week3_non_covid19, by = c("country", "nuts_2_region"))
symp_dat_week3_covid19 = merge(symp_dat_week3_covid19, symp_dat_week3_non_covid19_2018_2019, by = c("country", "nuts_2_region"))

#create percentage change in regional illness symptom mention variable
symp_dat_week3_covid19$percent_change_symptom_talk_past_3_mean = (symp_dat_week3_covid19$sick_talk_proportion - symp_dat_week3_covid19$mean_symp_talk_week3_2017_to_2019) / symp_dat_week3_covid19$mean_symp_talk_week3_2017_to_2019 
symp_dat_week3_covid19$percent_change_symptom_talk_past_3_mean = round(symp_dat_week3_covid19$percent_change_symptom_talk_past_3_mean * 100, 2)

symp_dat_week3_covid19$percent_change_symptom_talk_past_2_mean = (symp_dat_week3_covid19$sick_talk_proportion - symp_dat_week3_covid19$mean_symp_talk_week3_2018_2019) / symp_dat_week3_covid19$mean_symp_talk_week3_2018_2019 
symp_dat_week3_covid19$percent_change_symptom_talk_past_2_mean = round(symp_dat_week3_covid19$percent_change_symptom_talk_past_2_mean * 100, 2)

### ### ###

#compare regional data for symptom talk and COVID-19 cases
setdiff(symp_dat_week3_covid19$nuts_2_region, first_week_data$nuts2_clean)
nrow(symp_dat_week3_covid19)
nrow(first_week_data)

#drop regions that do not have COVID-19 case data (Wales, Northern Ireland, and the Balearic Islands)
symp_dat_week3_covid19_sub1 = subset(symp_dat_week3_covid19, symp_dat_week3_covid19$nuts_2_region %!in% c("ES53", "UKL1", "UKL2", "UKN0"))
setdiff(symp_dat_week3_covid19_sub1$nuts_2_region, first_week_data$nuts2_clean)
nrow(symp_dat_week3_covid19_sub1)
nrow(first_week_data)

#compare regional data for symptom talk and excess mortality; data missing from Corsica, all German NUTS 2 regions , and 
setdiff(symp_dat_week3_covid19$nuts_2_region, mortality_week14_dat$nuts_region)
nrow(symp_dat_week3_covid19)      
nrow(mortality_week14_dat)

#drop regions that do not have excess mortality data (Wales, Northern Ireland, the Balearic Islands, the Canary Islands, and Corsica)
symp_dat_week3_covid19_sub2 = subset(symp_dat_week3_covid19, symp_dat_week3_covid19$nuts_2_region %!in% c("ES53", "ES70", "UKL1", "UKL2", "UKN0", "FRM0"))
mortality_week14_dat_sub = subset(mortality_week14_dat, mortality_week14_dat$nuts_region %!in% c("ES53", "ES70", "UKL1", "UKL2", "UKN0", "FRM0"))

#aggregate all symptom talk data from Germany to the NUTS 1 level (excess mortality exists only at the NUTS 1 level)
symp_dat_week3_covid19_sub2$nuts_1_region = substr(symp_dat_week3_covid19_sub2$nuts_2_region, 1, 3)
symp_dat_week3_covid19_sub2$nuts_2_region = ifelse(symp_dat_week3_covid19_sub2$country == "Germany", 
                                                   symp_dat_week3_covid19_sub2$nuts_1_region, 
                                                   symp_dat_week3_covid19_sub2$nuts_2_region)

symp_dat_week3_covid19_final = symp_dat_week3_covid19_sub2 %>% 
                                group_by(nuts_2_region) %>% 
                                  dplyr::summarise(country = first(country),
                                                   year = first(year),
                                                   week = first(week),
                                                   sick_talk_proportion = mean(sick_talk_proportion),
                                                   num_tweets = mean(num_tweets),
                                                   mean_symp_talk_week3_2017_to_2019 = mean(mean_symp_talk_week3_2017_to_2019),
                                                   mean_tweets_week3_2017_to_2019 = mean(mean_tweets_week3_2017_to_2019),
                                                   percent_change_symptom_talk_past_3_mean = mean(percent_change_symptom_talk_past_3_mean),
                                                   mean_symp_talk_week3_2018_2019 = mean(mean_symp_talk_week3_2018_2019),
                                                   mean_tweets_week3_2018_2019 = mean(mean_tweets_week3_2018_2019),
                                                   percent_change_symptom_talk_past_2_mean = mean(percent_change_symptom_talk_past_2_mean),
                                                   nuts_1_region = first(nuts_1_region))
                                                                      
#rename and drop variables in datasets before combining them
mortality_week14_dat_sub$mortality_week = mortality_week14_dat_sub$WEEK
mortality_week14_dat_sub$percentage_increase_mortality = mortality_week14_dat_sub$percentage_increase
mortality_week14_dat_sub = mortality_week14_dat_sub[ , c("nuts_region", "mortality_week", "weekly_deaths_2020", "mean_weekly_deaths_2016_2019",
                                                         "mortality_change", "percentage_increase_mortality")]

symp_dat_week3_covid19_final$nuts_region = symp_dat_week3_covid19_final$nuts_2_region
symp_dat_week3_covid19_final$symptom_talk_year = symp_dat_week3_covid19_final$year
symp_dat_week3_covid19_final$symptom_talk_week = symp_dat_week3_covid19_final$week
symp_dat_week3_covid19_final = symp_dat_week3_covid19_final[ , c("country", "nuts_region", "symptom_talk_year", "week", "sick_talk_proportion",
                                                                 "num_tweets", "mean_symp_talk_week3_2017_to_2019", "mean_tweets_week3_2017_to_2019",
                                                                 "mean_symp_talk_week3_2018_2019", "mean_tweets_week3_2018_2019",
                                                                 "percent_change_symptom_talk_past_3_mean", "percent_change_symptom_talk_past_2_mean")]

first_week_data_final = first_week_data
first_week_data_final$nuts_region = first_week_data_final$nuts2_clean
first_week_data_final$start_date_cases = first_week_data_final$start_date
first_week_data_final$end_date_cases = first_week_data_final$end_date
first_week_data_final = first_week_data_final[ , c("nuts_region", "start_date_cases", "end_date_cases", "cases_in_week",
                                                   "nuts_2_population", "total_cases_reported", "cases_in_week_per10k")]

symp_dat_week3_covid19_sub1$nuts_region = symp_dat_week3_covid19_sub1$nuts_2_region
symp_dat_week3_covid19_sub1$symptom_talk_year = symp_dat_week3_covid19_sub1$year
symp_dat_week3_covid19_sub1$symptom_talk_week = symp_dat_week3_covid19_sub1$week
symp_dat_week3_covid19_sub1 = symp_dat_week3_covid19_sub1[ , c("country", "nuts_region", "symptom_talk_year", "week", "sick_talk_proportion",
                                                                 "num_tweets", "mean_symp_talk_week3_2017_to_2019", "mean_tweets_week3_2017_to_2019",
                                                                 "mean_symp_talk_week3_2018_2019", "mean_tweets_week3_2018_2019",
                                                                 "percent_change_symptom_talk_past_3_mean", "percent_change_symptom_talk_past_2_mean")]


#combine symptom talk data with the COVID-19 case rate and mortality data (separate datasets with the Twitter symptom talk data)
final_symp_talk_case_data = merge(symp_dat_week3_covid19_sub1, first_week_data_final, by = "nuts_region")
final_symp_talk_mortality_data = merge(symp_dat_week3_covid19_final, mortality_week14_dat_sub, by = "nuts_region")

### ### ###

#save the dataframes
write.csv(final_symp_talk_case_data, "../data/symptom_talk_and_covid19_case_data.csv", row.names = FALSE)
write.csv(final_symp_talk_mortality_data, "../data/symptom_talk_and_excess_mortality_data.csv", row.names = FALSE)
