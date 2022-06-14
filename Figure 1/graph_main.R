
# --------------------------------------------------------------------------------
# import required functions and libraries for the plots of travel-rated cases
# --------------------------------------------------------------------------------

library(scales)
library(ggplot2)
library(patchwork)
source('nl_graph.r')
source('ns_graph.r')
source('nb_graph.r')
source('nwt_graph.r')
source('pei_graph.r')
# set colors 
col1 = "black"
col2 = "#FF6666"
cb = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
setwd('/Users/fanokye/MUN/Amy_Paper/')
# -------------------------------------------------------------------------------------------
# load data
# -------------------------------------------------------------------------------------------

contacts_week <- read.csv("Contacts_week.csv")
canada_week <- read.csv("Canada_week_m.csv")

# Remove 1st column, contains no useful info
canada_week = canada_week[,-1]
contacts_week = contacts_week[,-1]

# # almost zero cases were related to domestic origin travel across provinces prior to the week of July 20th 2020
# hence, we exclude these dates
canada_week = canada_week[-(1:20),]
contacts_week = contacts_week[-(1:20),]

# sum all row cases to know total active cases reported per week for each province
sum_total = rowSums(canada_week[,2:14])

# subset to exclude last 3 weeks for the purpose of statistical models built
total_activecases_per_week = head(sum_total,-3)
week = as.Date(tail(canada_week$week,-3), format = "%Y-%m-%d")

# -------------------------------------------------------------------------------------------
# Uncomment any of the codes below to visualize the plot for the provinces 
# -------------------------------------------------------------------------------------------

nl_plots(canada_week, contacts_week)
# ns_plots(canada_week, contacts_week)
# nwt_plots(canada_week, contacts_week)
# pei_plots(canada_week, contacts_week)

# -------------------------------------------------------------------------------------------
