# import importation script. File must be in your working directory 
source("importation_func.r")

# uncomment two lines of code to create these two folders if you do not have them in your working directory
# dir.create('Data')
# dir.create('Figures')

setwd("/Users/fanokye/MUN/Amy_Paper")
path_out = getwd()

################------------- Load Canada (All provinces) Travel Related Data -------------################

# individual-level data for travel-related cases hosted on Github
covid_data_2020_travel_rel = read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2020.csv', fill=TRUE)
covid_data_2021_travel_rel = read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2021_1.csv', fill = TRUE)
covid_data_2021b_travel_rel = read.csv('https://raw.githubusercontent.com/ishaberry//Covid19Canada/master/retired_datasets/individual_level/cases_2021_2.csv', fill = TRUE)

################------------- Clean Travel Related Data Uusng Custom function -------------################

# apply the custom function above to clean the columns in each data frame
covid_data_2020_travel_rel <- clean_column(covid_data_2020_travel_rel, c('locally_acquired','travel_history_country'))
covid_data_2021_travel_rel <- clean_column(covid_data_2021_travel_rel, c('locally_acquired','travel_history_country'))
covid_data_2021b_travel_rel <- clean_column(covid_data_2021b_travel_rel, c('locally_acquired','travel_history_country'))

################------------- Load Covid-19 Time Series Data Collected For All Canada -------------################

# load covid-19 reported data for all provinces in Canada
covid_data <- read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/active_timeseries_prov.csv', fill = TRUE)

# subset only data after June 28, 2021
covid_data = covid_data[as.Date(covid_data$date_active, format = "%d-%m-%Y") <= as.Date("27-06-2021",format="%d-%m-%Y"), ]

################------------- Provincial Active Cases -------------################

# provincial weekly active cases
provincial_active_cases <- provincial_weekly_active_cases(covid_data)

# provincial weekly active cases per 10k people
provincial_active_cases_per_10k <- provincial_weekly_active_cases_per_10k(covid_data)

# create merged data frame of active case
canada_merged_active <- cbind(provincial_active_cases_per_10k, subset(provincial_active_cases,select =  c("NL_new","NS_new","NB_new","PEI_new","YT_new","NWT_new")))

# create dates with 7 days interval ie weekly
week = cut(as.Date(canada_merged_active$date), "week")

# append weekly date to Canada_data
canada_merged_active <- cbind(canada_merged_active,week)

# aggregate and sum data weekly
canada_merged_active_week = aggregate(canada_merged_active[,2:20], by = list(canada_merged_active$week),FUN = sum)

# rename the first column as week
# canada_week = data.frame(week = canada_week[,1],canada_week[,2:20])
colnames(canada_merged_active_week)[1] <- "week"

# convert week column to datetime
canada_merged_active_week$week = as.Date(canada_merged_active_week$week)

# Begin the week of March 2, 2021 (remove first 6 weeks)
canada_merged_active_week <- tail(canada_merged_active_week,-6)

# save data as a csv file
fileName = paste(path_out, 'canada_merged_active_week.csv',sep = '/')
write.csv(canada_merged_active_week,fileName)

################ ############################################
######## Create Importation Data For Each Province ##########
################ ############################################
travel_data_canada_week = data.frame(row = 1:69)
travel_data_contacts_week = data.frame(row = 1:69)
provinces = c("NL","Nova Scotia","PEI","New Brunswick","Yukon","NWT")
for (prov in provinces)
  {
  travel_data <- covid_importation_data(covid_data_2020_travel_rel, covid_data_2021_travel_rel, covid_data_2021b_travel_rel, canada_merged_active_week, prov)
  # No values after May 31
  travel_data[67:69,2:5] = rep(NA,12)
  dom <- travel_data$domestic
  int <- travel_data$international
  no_rep <- travel_data$not_reported
  week <- travel_data$week
  if (prov == 'NL')
    {
    travel_data_contacts_week[,'NL'] <- travel_data$close_contact
    }
  if (prov == 'Nova Scotia')
    {
    travel_data_contacts_week[,'NS'] <- travel_data$close_contact
    }
  if (prov == 'PEI')
    {
    travel_data_contacts_week[,'PEI'] <- travel_data$close_contact
    }
  if (prov == 'New Brunswick')
    {
    travel_data_contacts_week[,'NB'] <- travel_data$close_contact
    }
  if (prov == 'NWT')
    {
    travel_data_contacts_week[,'NWT'] <- travel_data$close_contact
    }
  if (prov == 'Yukon')
    {
    travel_data_contacts_week[,'YT'] <- travel_data$close_contact
    }
  
  df <- as.data.frame(list(dom,int,no_rep))
  colnames(df) <- c(paste(prov,"dom",sep = '_'), paste(prov,"int",sep = '_'),paste(prov,"nr",sep = '_') )
  travel_data_canada_week <- cbind(travel_data_canada_week,df)
  }
  
# rename columns
names(travel_data_canada_week)[names(travel_data_canada_week) %in% c("Nova Scotia_dom","Nova Scotia_int", "Nova Scotia_nr", "New Brunswick_dom", "New Brunswick_int","New Brunswick_nr")] <- 
  c("NS_dom","NS_int","NS_nr","NB_dom","NB_int","NB_nr")
# assign row to week and rename
travel_data_canada_week$row <- week
travel_data_contacts_week$row <- week
# rename row to week
names(travel_data_canada_week)[names(travel_data_canada_week) %in% 'row'] <- 'week'
names(travel_data_contacts_week)[names(travel_data_contacts_week) %in% 'row'] <- 'week'

# merge canada_merged_active_week and Canada_week to have all information in one file
Canada_week_merged <- cbind(canada_merged_active_week, travel_data_canada_week[,-1])

# save files
fileName1 = paste(path_out, 'Canada_week_b.csv',sep = '/')
fileName2 = paste(path_out, 'Contacts_week.csv',sep = '/')
fileName3 = paste(path_out, 'Canada_week_m.csv',sep = '/')
write.csv(travel_data_canada_week,fileName1)
write.csv(travel_data_contacts_week,fileName2)
write.csv(Canada_week_merged,fileName3)



