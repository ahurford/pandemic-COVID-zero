ns_plots <- function(canada_week, contacts_week)
  {
    # -------------------------------------------------------------------------------------------
    # No data for travel-related cases after May 31 - remove last three
    # weeks of predictor variable data
    # -------------------------------------------------------------------------------------------
    
    ON = head(canada_week$ON,-3)
    AB = head(canada_week$AB,-3)
    BC = head(canada_week$BC,-3)
    SK = head(canada_week$SK,-3)
    MB = head(canada_week$MB,-3)
    QC = head(canada_week$QC,-3)
    NS = head(canada_week$NS,-3)
    NB = head(canada_week$NB,-3)
    NWT = head(canada_week$NWT,-3)
    YT = head(canada_week$YT,-3)
    NL = head(canada_week$NL,-3)
    PEI = head(canada_week$PEI,-3)
    
    ns_nr <- head(canada_week[,"NS_nr"],-3)
    ns_dom <- head(canada_week[,'NS_dom'],-3)
    ns_int <- head(canada_week[,'NS_int'],-3)
    ns_new = head(canada_week[,'NS_new'],-3)
    ns_contacts <- head(contacts_week[,'NS'],-3)
    ns_total <- ns_nr + ns_dom + ns_int
    all_ns_reported_contacts <- ns_nr + ns_dom + ns_int + ns_contacts
    prov <- 'Nova Scotia'
    week = as.Date(tail(canada_week$week,-3), format = "%Y-%m-%d")
    
    # -------------------------------------------------------------------------------------------
    # Nova Scotia experienced community outbreak, hence no travel-related cases were recorded
    # in the last 6 weeks, therefore we exclude the date
    # -------------------------------------------------------------------------------------------
    
    ON2 = head(ON,-6)
    QC2 = head(QC,-6)
    NB2 = head(NB,-6)
    AB2 = head(AB,-6)
    MB2 = head(MB,-6)
    BC2 = head(BC,-6)
    NS2 = head(NS,-6)
    NS_total2 = head(ns_total,-6)
    Total2 = head(total_activecases_per_week, -6)

    # -------------------------------------------------------------------------------------------
    # we fit multiple generalized linear models (glms) with poisson error distribution using total 
    # number of travel-related cases as the response variable and total active cases across 
    # provinces as predictor(s)
    
    # The Akaike information criterion (AIC) is an estimator of out-of-sample prediction error. 
    # It is used often when we are unable to test the model’s performance on a test set 
    # as practiced in standard machine learning task.
    
    # The province associated with the best performing model in terms of least AIC value is chosen
    # as a predictor and combined with remaining provinces to build 
    # -------------------------------------------------------------------------------------------
    print(replicate(15,paste0('---')))
    print('Buiulding GLM For Nova Scotia Travel-Related Cases')
    print(replicate(15,paste0('---')))

    aiclist <- c()
    provincial_cases <- cbind(Total2, ON2, QC2, NB2, AB2, MB2, BC2)
    province <- c('Total', 'ON', 'QC', 'NB', 'AB', 'MB', 'BC')
    for (i in 1 : ncol(provincial_cases))
      {
      var_name <- colnames(provincial_cases)[i]
      mod <- glm(NS_total2 ~ 0 + provincial_cases[,var_name], family = "poisson")
      print(paste0( 'Model AIC For ', var_name,': ',  mod$aic))
      aiclist[[var_name]] <- mod$aic
    }
    print('')
    # province associated with the minimum aic
    prov_with_min_aic <- names(aiclist)[which.min(aiclist)]
    print(paste0('The province with the least AIC is: ',prov_with_min_aic))
    print('Pair cases from this chosen province with the rest as predictors in a new model')
    print('')
    # extract cases of the province and use in combination with other provinces in the subsequent models
    for (i in 1:ncol(provincial_cases)){
      if (grepl(prov_with_min_aic, colnames(provincial_cases)[i], ignore.case = T))
      {
        sel_prov_cases <-  provincial_cases[,prov_with_min_aic]
      }
    }
    # remove selected province from list of new predictors to combine with it
    modified_aic_list <- c()
    selected_provinces <- colnames(provincial_cases)[! colnames(provincial_cases) %in% c(prov_with_min_aic, 'Total2')]
    selected_prov_cases <- provincial_cases[,selected_provinces]
    
    for (i in 1 : ncol(selected_prov_cases))
      {
        var_name <- colnames(selected_prov_cases)[i]
        mod_combo <- glm(NS_total2 ~ 0 + selected_prov_cases[,var_name], family = "poisson")
        print(paste0( 'Model AIC For ', prov_with_min_aic,' & ', var_name,': ',  mod_combo$aic))
        modified_aic_list[[var_name]] <- mod_combo$aic
    }
    print('')
    print('Comparing the AIC from all the models, it appears combining predictors does no better.')
    print('')
    # use the model with the province with best AIC as chosen model 
    best_mod <- glm(NS_total2 ~ 0 + provincial_cases[,prov_with_min_aic], family = "poisson")
    
    # -------------------------------------------------------------------------------------------
    
    # create data frame to store actual cases and model predictions
    ns_df = data.frame(week = head(week,-6), actual = NS_total2, predicted = best_mod$fitted.values)
    
    # -------------------------------------------------------------------------------------------
    
    week = c(week,week,week, week)
    value <- c(ns_nr, ns_dom, ns_int, ns_contacts)
    variable <- c(rep("d",length(week)),rep("c",length(week)),rep("b",length(week)), rep("a", length(week)))
    value2 <- c(ns_new, ns_new, ns_new, ns_new)
    value3 <- rep(max(ns_dom + ns_int + ns_contacts + ns_nr), 4 * length(NS))
    value4 <- rep(ns_total,4)
    
    #  create data frame with information on travel-related cases
    data = data.frame(week = week, value = value, variable = variable, value2 = value2, value3 = value3, value4 = value4)
    
    # subset data where the new cases are greater than the total cases and replace with the total reported cases
    data$value2[data$value2>max(all_ns_reported_contacts)] = max(all_ns_reported_contacts)
    
    # -------------------------------------------------------------------------------------------
    # Visualizations of the travel-related cases
    # -------------------------------------------------------------------------------------------
    
    # visualize the data frame by ploting the week against the total cases
    nl_tot_graph <-  ggplot(ns_df,aes(week,actual)) +
      geom_line(aes(y=predicted),color=cb[2]) +
      geom_point(aes(y=actual),color=cb[2]) +
      scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%b %Y"))+
      xlab("") +
      ylab("total travel-related\n(weekly)")+
      ggtitle(prov)+
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 90, size=rel(0.9)), legend.title = element_blank(),
      legend.text = element_text(size = rel(1)),plot.title = element_text(size = rel(.9)),axis.title = element_text(size = rel(.8)))
    png("nl_active_tot.png")
    print(nl_tot_graph)
    dev.off()
    
    nl_conta_graph <- ggplot(data,aes(x=week,y = value,color = variable)) +
      geom_area(aes(fill = variable,alpha = variable),linetype = "blank") +
      geom_line(aes(y = value2),col="black")+
      geom_line(aes(y = value4),col="black",linetype="dashed")+
      geom_line(aes(y = value3),col = "white",size = 2)+
      scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%b %Y"))+
      scale_fill_manual(values = c(cb[3], cb[3],cb[3],"black"))+
      scale_alpha_manual(values = c(0.3,0.7,1, 0))+
      ylim(0,max(all_ns_reported_contacts))+
      xlab("") +
      ylab("")+
      ggtitle(prov)+
      theme_classic() + theme(legend.position = "none",axis.text.x = element_text(angle = 90, size=rel(0.9)), 
      legend.title = element_blank(),legend.text=element_text(size=rel(1)),plot.title=element_text(size=rel(.9)))
      
    png("ns_active_contacts.png")
    print(nl_conta_graph)
    dev.off()
    
    # print(gNs_tot)
    # print(gns_contacts)
  }
   
    
