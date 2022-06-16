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
  print(replicate(15,paste0('---')))
  print(paste0('Buiulding GLM For ',prov, ' Travel-Related Cases'))
  print(replicate(15,paste0('---')))
  
  # ---------------- First Model Fitting For Best Predictor ------------------------------------
  aiclist <- c()
  for (i in 1 : ncol(predictors))
  {
    var_name <- colnames(predictors)[i]
    mod <- glm(response ~ 0 + predictors[,var_name], family = "poisson")
    print(paste0( 'Model AIC For ', var_name,': ',  mod$aic))
    aiclist[[var_name]] <- mod$aic
  }
  
  # province associated with the minimum aic
  first_prov_with_min_aic <- names(aiclist)[which.min(aiclist)]
  print(paste0('The province with the least AIC is: ',first_prov_with_min_aic))
  print('')
  
  first_best_predictor <- predictors[,first_prov_with_min_aic]
  
  # ---------------- Second Model Fitting For Best Predictor ------------------------------------
  
  # extract cases of the province and use in combination with other provinces in the subsequent models
  if (first_prov_with_min_aic %in% colnames(predictors))
  {
    selected_predictors <- colnames(predictors)[! colnames(predictors) %in% c(prov,first_prov_with_min_aic)]
    predictors <- predictors[,selected_predictors]
  }
  else{
    predictors <- predictors
  }
  
  modified_aic_list <- c()
  for (i in 1 : ncol(predictors))
  {
    var_name <- colnames(predictors)[i]
    mod <- glm(response ~ 0 + first_best_predictor + predictors[,var_name], family = "poisson")
    print(paste0( 'Model AIC For ', first_prov_with_min_aic, ' & ', var_name,': ',  mod$aic))
    modified_aic_list[[var_name]] <- mod$aic
  }
  
  # province associated with the minimum aic
  second_prov_with_min_aic <- names(modified_aic_list)[which.min(modified_aic_list)]
  print(paste0('The province with the least AIC is: ',second_prov_with_min_aic))
  print('')
  
  second_best_predictor <- predictors[,second_prov_with_min_aic]
  
  # ---------------- Second Model Fitting For Best Predictor ------------------------------------
  
  # extract cases of the province and use in combination with other provinces in the subsequent models
  if (second_prov_with_min_aic %in% colnames(predictors))
  {
    selected_predictors <- colnames(predictors)[! colnames(predictors) %in% c(prov,first_prov_with_min_aic, second_prov_with_min_aic)]
    predictors <- predictors[,selected_predictors]
  }
  else{
    predictors <- predictors
  }
  
  print('Combine this chosen predictor with others in a new model')
  print('')
  modified_aic_list <- c()
  for (i in 1 : ncol(predictors))
  {
    var_name <- colnames(predictors)[i]
    mod <- glm(response ~ 0 + first_best_predictor + second_best_predictor+ predictors[,var_name], family = "poisson")
    print(paste0( 'Model AIC For ', first_prov_with_min_aic,', ' ,second_prov_with_min_aic,' & ', var_name,': ',  mod$aic))
    modified_aic_list[[var_name]] <- mod$aic
  }
  
  
  # province associated with the minimum aic
  third_prov_with_min_aic <- names(modified_aic_list)[which.min(modified_aic_list)]
  print(paste0('The province with the least AIC is: ',third_prov_with_min_aic))
  print('')
  
  third_best_predictor <- predictors[,third_prov_with_min_aic]
  
  print(paste0('Best Three Chosen Provinces As Predictors For Our Model Are ',first_prov_with_min_aic,', ',
               second_prov_with_min_aic,' & ',third_prov_with_min_aic))
  
  # build chosen model with the three predictors 
  best_mod <- glm(response ~ 0 + first_best_predictor + second_best_predictor + third_best_predictor, family = "poisson")
  
  # extract coefficients
  coef_predictor_1 <- coef(best_mod)[1]
  coef_predictor_2 <- coef(best_mod)[2]
  coef_predictor_3 <- coef(best_mod)[3]
  
  # prediction values from model
  predicted = exp(coef_predictor_1 * first_best_predictor + coef_predictor_2 * second_best_predictor + coef_predictor_3 * third_best_predictor)
  
  # create data frame to store actual cases and model predictions
  df = data.frame(week = week, true_values = response, predictions = predicted)

  # visualize the data frame by ploting the week against the total cases
  trav_graph <- ggplot(df, aes(week, actual)) +
    geom_line(aes(y = true_values),color = cb[1]) +
    geom_line(aes(y = predictions),color = cb[2]) +
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

