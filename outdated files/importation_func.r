
###############################################################################################################
# function below fixes the unique values in each of the two columns (locally_acquired & travel_history_country) 
# by passing each data frame and the two columns as a list. 
###############################################################################################################
clean_column <- function(data_frame, list_columns_to_clean) 
  {
  for (name_of_column_to_clean in list_columns_to_clean)
  {
    # replace all missing values in each variable with "Not Reported"
    data_frame[,name_of_column_to_clean][is.na(data_frame[,name_of_column_to_clean])] <- "Not Reported"
    data_frame[,name_of_column_to_clean][data_frame[,name_of_column_to_clean] == ""] <- "Not Reported"
    # if the column name is 'locally_acquired' and 'close' (case insensitive) is contained in any of the column values, 
    # replace with "Close Contact" otherwise keep the old value.
    if (grepl('locally_acquired', name_of_column_to_clean, ignore.case = T)) {
      data_frame[,name_of_column_to_clean] <- ifelse(grepl("close", data_frame[,name_of_column_to_clean], ignore.case = T), "Close Contact", data_frame[,name_of_column_to_clean])
    }else{
      # if the column name is 'travel_history_country' and 'Not' (case insensitive) is contained in any of the column values, 
      # replace with "Not Reported" otherwise keep the old value.
      data_frame[,name_of_column_to_clean] <- ifelse(grepl("Not", data_frame[,name_of_column_to_clean], ignore.case = T), "Not Reported", data_frame[,name_of_column_to_clean])
    }
  }
  # remove extra column created
  data_frame[,1] <- NULL
  
  return(data_frame)
  }

#########################################################################################################
# Weekly new cases in all provinces - originally this was done on new cases then switched to active cases
#########################################################################################################
provincial_weekly_active_cases <- function(data_frame)
  {
  provinces = c("Alberta","BC","Saskatchewan","Manitoba","Ontario","Quebec","Nova Scotia","PEI","New Brunswick","NL","Nunavut","NWT","Yukon")
  active_case_list = list()
  for (prov in provinces) 
  {
    active_case <- diff(c(0,data_frame[data_frame[,'province'] == prov,3]))
    active_case_list <- append(active_case_list ,list(active_case))
  }
  df <- as.data.frame(active_case_list)
  date <- data.frame(date=as.Date(data_frame[data_frame[, 'province']=="Alberta",2],format="%d-%m-%Y"))
  df <- cbind(date, df)
  colnames(df) <- c("date","AB_new","BC_new","SK_new","MB_new","ON_new","QC_new","NS_new","PEI_new","NB_new","NL_new","NV_new","NWT_new","YT_new")
  return(df)
}

#############################################################################
# Weekly active cases in all provinces per 10K people province sizes based on 
#############################################################################
provincial_weekly_active_cases_per_10k <- function(data_frame)
  {
  provinces = c("Alberta","BC","Saskatchewan","Manitoba","Ontario","Quebec","Nova Scotia","PEI","New Brunswick","NL","Nunavut","NWT","Yukon")
  active_case_list = list()
  for (prov in provinces) 
  {
    # there was a change in how active cases were reported in Quebec on July 17, 2020.
    # hence,  we scale active cases prior to above date
    if(grepl("Quebec", data_frame[,'province'] == prov, ignore.case = T))
    {
      pop = 8164361
      QC_active_case <- data_frame[data_frame[,'province'] == prov,]
      change_index = which(QC_active_case[,'active_cases_change'] == min(QC_active_case[,'active_cases_change']))
      QC_scale = QC_active_case[change_index,][,'active_cases'] / QC_active_case[change_index - 1,][,'active_cases']
      # Rescale active cases prior to July 17, 2020:
      QC_active_case[,'active_cases'][1 : change_index - 1] <- QC_scale * QC_active_case[,'active_cases'][1 : change_index - 1]
      QC_active_case = 1e4 * QC_active_case[,'active_cases'] / pop
      active_case_list <- append(active_case_list ,list(QC_active_case))
    }
    else
    {
      # provincial population
      if (prov == 'Alberta')
      { pop = 4067175}
      if (prov == 'BC')
      { pop = 4648055}
      if (prov == 'Saskatchewan')
      { pop = 1098352}
      if (prov == 'Manitoba')
      { pop = 1278365}
      if (prov == 'Ontario')
      { pop = 13448494}
      if (prov == 'Nova Scotia')
      { pop = 923598}
      if (prov == 'PEI')
      { pop = 142907}
      if (prov == 'New Brunswick')
      { pop = 747101}
      if (prov == 'NL')
      { pop = 519716}
      if (prov == 'Nunavut')
      { pop = 35944}
      if (prov == 'NWT')
      { pop = 41786}
      if (prov == 'Yukon')
      { pop = 35874}
      active_case <- 1e4 * data_frame[data_frame[,'province'] == prov,6] / pop
      active_case_list <- append(active_case_list ,list(active_case))
    }
  }
  
  df <- as.data.frame(active_case_list)
  date <- data.frame(date=as.Date(data_frame[data_frame[, 'province']=="Alberta",2],format="%d-%m-%Y"))
  df <- cbind(date,df)
  colnames(df) <- c("date","AB","BC","SK","MB","ON","QC","NS","PEI","NB","NL","NV","NWT","YT")
  return(df)
  }

#############################################################################
# this function  Travel-related cases
#############################################################################
covid_importation_data = function(data2020, data2021, data2021b, merged_active, province_name)
  {
  # subset only data for the specified province
  covid_data_2020_travel_rel <- data2020[data2020[, 'province'] == province_name,]
  covid_data_2021_travel_rel <- data2021[data2021[, 'province'] == province_name,]
  covid_data_2021b_travel_rel <- data2021b[data2021b[, 'province'] == province_name,]
  
  # merge all the data to create a singe file
  covid_travel_data = rbind(covid_data_2020_travel_rel, covid_data_2021_travel_rel, covid_data_2021b_travel_rel)
  
  # convert format of date_report to datetime
  covid_travel_data$date_report = as.Date(covid_travel_data$date_report, format="%d-%m-%Y")
  
  # subset only data that is travel-related and not from close contacts
  covid_travel_rel = covid_travel_data[covid_travel_data$travel_yn == 1 & covid_travel_data$locally_acquired != "Close Contact",]
  
  # subset only dates that is not travel-related but has close contacts
  covid_contacts_dates = covid_travel_data[covid_travel_data$travel_yn != 1 & covid_travel_data$locally_acquired == "Close Contact",]$date_report
  
  # subset dates with both close contacts and travel-related i.e. mutually exclusive for all 
  covid_travel_contacts = covid_travel_data[covid_travel_data$travel_yn == 1 & covid_travel_data$locally_acquired == "Close Contact",]$date_report
  
  # subset dates where travel history country is Canada
  travel_Canada_date = covid_travel_data$date_report[covid_travel_data$travel_history_country == "Canada"]
  
  # subset dates with no information on travel history country
  travel_Not_Reported_date = covid_travel_data$date_report[covid_travel_data$travel_history_country == "Not Reported"]
  
  # subset date for international travel ie where travel history country is different from Canada
  travel_Int_date = covid_travel_data$date_report[covid_travel_data$travel_history_country != "Not Reported" & covid_travel_data$travel_history_country != "Canada"]
  
  ########---------- Domestic Data--------###############
  # create dates with 7 days interval i.e weekly
  week = cut(as.Date(travel_Canada_date), "week")
  
  # create data frame for travels to Canada
  travel_Canada_df = data.frame(date = travel_Canada_date, number = rep(1,length(travel_Canada_date)), week = as.Date(week))
  
  # aggregate domestic weekly travels within Canada 
  domestic = aggregate(travel_Canada_df$number, by = list(travel_Canada_df$week),FUN = sum)
  fileName = paste(path_out, 'domestic.csv',sep = '/')
  write.csv(domestic,fileName)
  
  #######---------- International Data--------###############
  # create international data for all provinces but NWT which has no Covid information on international travels 
  if (province_name != "NWT")
  {
    # create dates with 7 days interval ie weekly
    week = cut(as.Date(travel_Int_date), "week")
    
    # create data frame for international travels 
    travel_Int_df = data.frame(date = travel_Int_date, number = rep(1,length(travel_Int_date)), week = as.Date(week))
    
    # aggregate weekly international travels 
    international = aggregate(travel_Int_df$number, by = list(travel_Int_df$week),FUN = sum)
    fileName = paste(path_out, 'international.csv',sep = '/')
    write.csv(international,fileName)
  }
  
  ########---------- Not reported Data--------###############
  # create dates with 7 days interval i.e weekly
  if ( province_name != "NWT"){
    week = cut(as.Date(travel_Not_Reported_date), "week")
    
    # create data frame for international travels 
    travel_Not_Reported_df = data.frame(date = travel_Not_Reported_date, number = rep(1,length(travel_Not_Reported_date)), week = as.Date(week))
    
    # aggregate weekly data with unreported travel history  
    not_reported = aggregate(travel_Not_Reported_df$number, by = list(travel_Not_Reported_df$week),FUN = sum)
    fileName = paste(path_out, 'not_reported.csv',sep = '/')
    write.csv(not_reported,fileName)
  }
  
  ########---------- Close Contact Data--------###############
  week = cut(as.Date(covid_contacts_dates), "week")
  
  # create data frame for contacts 
  covid_contacts_df = data.frame(date = covid_contacts_dates, number = rep(1,length(covid_contacts_dates)), week = as.Date(week))
  
  # aggregate weekly data for contacts  
  contacts = aggregate(covid_contacts_df$number, by = list(covid_contacts_df$week),FUN = sum)
  fileName = paste(path_out, 'contacts.csv',sep = '/')
  write.csv(contacts,fileName)
  
  #########---------- Merge Data Frames--------###############
  # create new data frame same as the size of 'canada_week' and populate with weekly reports for domestic, international and not reported. 
  travel_data = data.frame(week = merged_active[,'week'], domestic = rep(0,length(merged_active[,'week'])), international = rep(0,length(merged_active[,'week'])), 
                           close_contact = rep(0,length(merged_active[,'week'])), not_reported = rep(0,length(merged_active[,'week'])))
  
  # Match with the weeks for Canada cases
  for ( i in seq(length(domestic[,1])))
    {
    domloc = which(merged_active[,'week'] == domestic$Group.1[i])
    travel_data$domestic[domloc] = domestic$x[i]
    }
  
  for ( i in seq(length(contacts[,1])))
    {
    contatloc = which(merged_active[,'week'] == contacts$Group.1[i])
    travel_data$close_contact[contatloc] = contacts$x[i]
    }
  
  for ( i in seq(length(not_reported[,1])))
    {
    reploc = which(merged_active[,'week'] == not_reported$Group.1[i])
    travel_data$not_reported[reploc] = not_reported$x[i]
    }
      
  if ( province_name != "NWT")
    {
    for ( i in seq(length(international[,1])))
      {
      loc = which(merged_active[,'week'] == international$Group.1[i])
      travel_data$international[loc] = international$x[i]
      }
    }
  
  fileName = paste(path_out, 'travel_data.csv',sep = '/')
  write.csv(travel_data,fileName)
  
  return(travel_data)
  }





