library(readr)
library(scales)
library(ggplot2)
library(patchwork)

col1="black"
col2="#FF6666"
cb =c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

model_plots <- function(travel , prov)
  {
  # -------------------------------------------------------------------------------------------
  # extract predictors from data: active cases for each province
  # -------------------------------------------------------------------------------------------

  ON = travel$ON_active 
  QC = travel$QC_active
  NB = travel$NB_active 
  AB = travel$AB_active 
  MB = travel$MB_active
  BC = travel$BC_active 
  NS = travel$NS_active 
  SK =  travel$SK_active
  
  if (prov %in% c('NL','NS','NB','PEI'))
    {
      resp_name <- paste(prov,"travel",sep = '_')
      response = travel[,resp_name]
      print(paste0('You are working with ', prov, ' Travel-Related Cases As Response variable '))}
  else{print('Chosen Province Has No Response Variable For Travel-Rated Cases !')}
  
  predictors <- cbind(ON, QC, NB, AB, MB, BC, NS, SK)
  
  if (prov %in% colnames(predictors))
    {
    selected_predictors <- colnames(predictors)[! colnames(predictors) %in% c(prov)]
    predictors <- predictors[,selected_predictors]
  }
  else{
    predictors <- predictors
  }
  
  week = as.Date(travel$date, format = "%Y-%m-%d")
  
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
  print(replicate(11,paste0('---')))
  print(paste0('Buiulding GLM For ',prov, ' Travel-Related Cases'))
  print(replicate(11,paste0('---')))
  print('')
  # ---------------- First Model Fitting For Best Predictor ------------------------------------
  # fit models for only 'AB', 'MB', 'SK' and select the province with the least AIC
  # -------------------------------------------------------------------------------------------
  global_aic_list <- c()
  aiclist <- c()
  for (i in 1 : ncol(predictors))
    {
      var_name <- colnames(predictors)[i]
      if (var_name %in% c('AB', 'MB', 'SK'))
        {
          mod <- glm(response ~ 0 + predictors[,var_name], family = "poisson")
          print(paste0( 'Model AIC For ', var_name,': ',  mod$aic))
          aiclist[[var_name]] <- mod$aic
        }
    }
  
  # province associated with the minimum aic
  first_prov_with_min_aic <- names(aiclist)[which.min(aiclist)]
  print(paste0('The province to use as prior with the least AIC is: ',first_prov_with_min_aic))
  print('')
  
  first_best_predictor <- predictors[,first_prov_with_min_aic]
  mod <- glm(response ~ 0 + first_best_predictor, family = "poisson")
  global_aic_list[[first_prov_with_min_aic]] <- mod$aic
  
  # we select some specific provinces in addition to the first best province obtained from above
  selected_predictors <- subset(predictors, select = c("ON", "QC", "NS", "NB", "BC", first_prov_with_min_aic)) 
  
  for (i in 1 : ncol(selected_predictors)) 
    {
      variable_name <- colnames(selected_predictors)[i]
      if (variable_name %in% c("ON","QC","NS"))
        {
        print(paste0( 'Model Combinations For ', variable_name))
        print('')
        
        if (grepl("ON",variable_name, ignore.case = T))
          {
          for (i in 1 : ncol(selected_predictors))
            {
              if(i == 1)
                {
                  var <- colnames(selected_predictors)[i]
                  mod <- glm(response ~ 0 + selected_predictors[,var], family = "poisson")
                  print(paste0( 'Model AIC For ', var,': ',  mod$aic))
                  global_aic_list[[var]] <- mod$aic
                }
              if(i == 2)
                {
                  var <- colnames(selected_predictors)[i]
                  mod <- glm(response ~ 0 + selected_predictors[,variable_name] + selected_predictors[,var], family = "poisson")
                  print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                  global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                }
              if(i == 3)
                {
                  var <- colnames(selected_predictors)[i]
                  mod <- glm(response ~ 0 + selected_predictors[,variable_name] + selected_predictors[,var], family = "poisson")
                  print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                  global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                }
              if(i == 4)
                {
                  var <- colnames(selected_predictors)[i]
                  mod <- glm(response ~ 0 + selected_predictors[,variable_name] + selected_predictors[,var], family = "poisson")
                  print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                  global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                }
              if(i == 5)
                {
                  var <- colnames(selected_predictors)[i]
                  mod <- glm(response ~ 0 + selected_predictors[,variable_name] + selected_predictors[,var], family = "poisson")
                  print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                  global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                }
              if(i == 6)
                {
                  var <- colnames(selected_predictors)[i]
                  mod <- glm(response ~ 0 + selected_predictors[,variable_name] + selected_predictors[,var], family = "poisson")
                  print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                  global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                }
            }
            trio_selected_predictors <- subset(predictors, select = c("ON", "QC", "NS", "BC" ))
            for (i in 1 : ncol(trio_selected_predictors))
              { 
                if(i > 2)
                  {
                    var <- colnames(trio_selected_predictors)[i]
                    mod <- glm(response ~ 0 + trio_selected_predictors[,variable_name] + trio_selected_predictors[,'QC']  + trio_selected_predictors[,var], family = "poisson")
                    print(paste0( 'Model AIC For ', variable_name, ', ', 'QC' ,' & ', var,': ',  mod$aic))
                    global_aic_list[[paste(variable_name,'QC',var,sep = '_')]] <- mod$aic
                  }
              }
            final_selected_predictors <- subset(predictors, select = c("ON", first_prov_with_min_aic, "NS", "BC","NB" ))
            for (i in 1 : ncol(final_selected_predictors))
            { 
              if(i > 2)
              {
                var <- colnames(final_selected_predictors)[i]
                mod <- glm(response ~ 0 + final_selected_predictors[,variable_name] + final_selected_predictors[,first_prov_with_min_aic]  + final_selected_predictors[,var], family = "poisson")
                print(paste0( 'Model AIC For ', variable_name, ', ', first_prov_with_min_aic ,' & ', var,': ',  mod$aic))
                global_aic_list[[paste(variable_name,first_prov_with_min_aic,var,sep = '_')]] <- mod$aic
              }
            }
        } 
      
        else if (grepl("QC",variable_name, ignore.case = T))
          {
            qc_selected_predictors <- subset(predictors, select = c("QC", "BC", "NS", "NB",first_prov_with_min_aic ))
            for (i in 1 : ncol(qc_selected_predictors))
              { 
                if(i == 1)
                  { 
                    var <- colnames(qc_selected_predictors)[i]
                    mod <- glm(response ~ 0 + qc_selected_predictors[,var], family = "poisson")
                    print(paste0( 'Model AIC For ', var,': ',  mod$aic))
                    global_aic_list[[var]] <- mod$aic
                  }
                if(i == 2)
                {
                  var <- colnames(qc_selected_predictors)[i]
                  mod <- glm(response ~ 0 + qc_selected_predictors[,variable_name] + qc_selected_predictors[,var], family = "poisson")
                  print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                  global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                }
                if(i == 3)
                {
                  var <- colnames(qc_selected_predictors)[i]
                  mod <- glm(response ~ 0 + qc_selected_predictors[,variable_name] + qc_selected_predictors[,var], family = "poisson")
                  print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                  global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                }
                if(i == 4)
                {
                  var <- colnames(qc_selected_predictors)[i]
                  mod <- glm(response ~ 0 + qc_selected_predictors[,variable_name] + qc_selected_predictors[,var], family = "poisson")
                  print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                  global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                }
              } 
            
            final_qc_selected_predictors <- subset(predictors, select = c("QC", first_prov_with_min_aic, "BC","NS","NB" ))
            for (i in 1 : ncol(final_qc_selected_predictors))
            { 
              if(i == 2)
                {
                mod <- glm(response ~ 0 + final_qc_selected_predictors[,variable_name] + final_qc_selected_predictors[,first_prov_with_min_aic], family = "poisson")
                print(paste0( 'Model AIC For ', variable_name, ' & ', first_prov_with_min_aic,': ',  mod$aic))
                global_aic_list[[paste(variable_name, first_prov_with_min_aic,sep = '_')]] <- mod$aic
                }
              if(i > 2)
              {
                var <- colnames(final_qc_selected_predictors)[i]
                mod <- glm(response ~ 0 + final_qc_selected_predictors[,variable_name] + final_qc_selected_predictors[,first_prov_with_min_aic]  + final_qc_selected_predictors[,var], family = "poisson")
                print(paste0( 'Model AIC For ', variable_name, ', ', first_prov_with_min_aic ,' & ', var,': ',  mod$aic))
                global_aic_list[[paste(variable_name, first_prov_with_min_aic,var,sep = '_')]] <- mod$aic
              }
            }
          }
        else if(grepl("NS",variable_name, ignore.case = T))
          {
            ns_selected_predictors <- subset(predictors, select = c("NS", "BC",first_prov_with_min_aic ))
            for (i in 1 : ncol(ns_selected_predictors))
              { 
                if(i == 1)
                  { 
                    var <- colnames(ns_selected_predictors)[i]
                    mod <- glm(response ~ 0 + ns_selected_predictors[,var], family = "poisson")
                    print(paste0( 'Model AIC For ', var,': ',  mod$aic))
                    global_aic_list[[var]] <- mod$aic
                  }
                else if(i == 2)
                  { 
                    var <- colnames(ns_selected_predictors)[i]
                    mod <- glm(response ~ 0 + ns_selected_predictors[,variable_name] + ns_selected_predictors[,var], family = "poisson")
                    print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                    global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                  }
                else 
                  {
                    var <- colnames(ns_selected_predictors)[i]
                    mod <- glm(response ~ 0 + ns_selected_predictors[,variable_name] + ns_selected_predictors[,var], family = "poisson")
                    print(paste0( 'Model AIC For ', variable_name, ' & ', var,': ',  mod$aic))
                    global_aic_list[[paste(variable_name, var,sep = '_')]] <- mod$aic
                  }
              }
          }
      }
    }
  print('')
  print(paste0( 'Model Fits For NB, BC, & ', variable_name))
  print('')
  final_selected_predictors <- subset(predictors, select = c('NB', 'BC', first_prov_with_min_aic ))
  for (i in 1 : ncol(final_selected_predictors))
    {
      var <- colnames(final_selected_predictors)[i]
      mod <- glm(response ~ 0 + predictors[,var], family = "poisson")
      print(paste0( 'Model AIC For ', var,': ',  mod$aic))
      global_aic_list[[var]] <- mod$aic
    }
  
  print('')

  # province associated with the minimum aic
  best_prov_combination <- names(global_aic_list)[which.min(global_aic_list)]
  print(paste0('The best province combinations to use as predictors in final model: ', best_prov_combination))
  print('')
  
  # # build chosen model with the three predictors 
  best_mod <- glm(response ~ 0 + QC + AB + NB, family = "poisson")

  # extract coefficients
  coef_predictor_1 <- coef(best_mod)[1]
  coef_predictor_2 <- coef(best_mod)[2]
  coef_predictor_3 <- coef(best_mod)[3]
   
  # prediction values from model
  predicted = exp(coef_predictor_1 * QC + coef_predictor_2 * AB + coef_predictor_3 * NB)

  # create data frame to store actual cases and model predictions
  df = data.frame(week = week, true_values = response, predictions = predicted)

  # visualize the data frame by ploting the week against the total cases
  trav_graph <- ggplot(df, aes(week, true_values)) +
    geom_area()+
    geom_line(aes(y = true_values),color = col1) +
    geom_line(aes(y = predictions),color = col2) +
    scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%b %Y"))+
    xlab("") +
    ylab("total travel-related\n(weekly)")+
    ggtitle(prov)+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, size=rel(0.9)), legend.title = element_blank(),
          legend.text = element_text(size = rel(1)),plot.title = element_text(size = rel(.9)),axis.title = element_text(size = rel(.8)))

  png(paste(prov,"travel.png",sep = '_'))
  print(trav_graph)
  dev.off()

  }
# Executing Function With Data

active <- read.csv("travel.csv")
model_plots(active,prov = 'NL')

