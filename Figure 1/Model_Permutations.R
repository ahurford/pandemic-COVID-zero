library(readr)
library(scales)
library(ggplot2)
library(patchwork)
library(gtools)
library(sets)
library(reticulate)
sys <- import('sys',convert=TRUE);


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
    date = as.Date(travel$date, format = "%Y-%m-%d")
    
    df <- cbind(ON, QC, NB, AB, MB, BC, NS, SK)
    
    if (prov %in% c('NL','NS','NB','PEI'))
      {
        resp_name <- paste(prov,"travel",sep = '_')
        response = travel[,resp_name]
        print(paste0('You are working with ', prov, ' Travel-Related Cases As Response variable '))
      }
    else
      {
        print('Chosen Province Has No Response Variable For Travel-Rated Cases !')
      }
    
    if (prov %in% colnames(df))
      {
        sel_features <- colnames(df)[! colnames(df) %in% c(prov)]
        df_features <- df[,sel_features]
      }
    else
      {
        df_features <- df[,colnames(df)]
        sel_features <- colnames(df_features)
      }
    
    features = colnames(df_features)
    ###--------import python file---------###
    source_python("permute_variables.py")
    permuted_variables <- variable_permutations(feature_list = features)
    # exclude the empty list
    permuted_variables <- permuted_variables[-1]
    ###-----------------------------------###
    
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
    print(paste0('Buiulding Total of ',length(permuted_variables), ' GLM For ',prov, ' Travel-Related Cases'))
    print(replicate(11,paste0('---')))
    print('')
    # ---------------- First Model Fitting For Best Predictor ------------------------------------
    # fit models for only 'AB', 'MB', 'SK' and select the province with the least AIC
    # -------------------------------------------------------------------------------------------
    global_aic_list <- c()
    
    for (predictors in permuted_variables)
      {
        if (length(predictors) == 1)
          { 
            mod <- glm(response ~ 0 + df_features[,predictors[1]], family = "poisson")
            print(paste0( 'Model AIC For ', predictors,': ',  mod$aic))
            global_aic_list[[predictors]] <- mod$aic
            print('')
          }
      
        else if ( length(predictors) == 2)
          { 
            mod <- glm(response ~ 0 + df_features[,predictors[1]] + df_features[,predictors[2]], family = "poisson")
            print(paste0( 'Model AIC For ', predictors[1], ' & ', predictors[2],': ',  mod$aic))
            global_aic_list[[paste(predictors[1], predictors[2],sep = '_')]] <- mod$aic
            print('')
          }
            
        else if ( length(predictors) == 3)
          { 
            mod <- glm(response ~ 0 + df_features[,predictors[1]] + df_features[,predictors[2]] + df_features[,predictors[3]], family = "poisson")
            print(paste0( 'Model AIC For ', predictors[1],' ,', predictors[2] ,' & ', predictors[3],': ',  mod$aic))
            global_aic_list[[paste(predictors[1], predictors[2],predictors[3],sep = '_')]] <- mod$aic
            print('')
          }
        
        else if (length(predictors) == 4)
          { 
            mod <- glm(response ~ 0 + df_features[,predictors[1]] + df_features[,predictors[2]] + 
                         df_features[,predictors[3]] + df_features[,predictors[4]], family = "poisson")
            print(paste0( 'Model AIC For ', predictors[1],' ,', predictors[2] ,' ,', predictors[3],' & ', predictors[4],': ',  mod$aic))
            global_aic_list[[paste(predictors[1], predictors[2],predictors[3],predictors[4],sep = '_')]] <- mod$aic
            print('')
          }    
        else if (length(predictors) == 5)
          { 
            mod <- glm(response ~ 0 + df_features[,predictors[1]] + df_features[,predictors[2]] + 
                         df_features[,predictors[3]] + df_features[,predictors[4]] + df_features[,predictors[5]], family = "poisson")
            print(paste0( 'Model AIC For ', predictors[1],' ,', predictors[2] ,' ,', predictors[3],' ,', predictors[4],' & ', predictors[5],': ',  mod$aic))
            global_aic_list[[paste(predictors[1], predictors[2],predictors[3],predictors[4],predictors[5],sep = '_')]] <- mod$aic
            print('')
          }   
      
        else if (length(predictors) == 6)
          { 
            mod <- glm(response ~ 0 + df_features[,predictors[1]] + 
                         df_features[,predictors[2]] + df_features[,predictors[3]] + df_features[,predictors[4]] + 
                         df_features[,predictors[5]] + df_features[,predictors[6]], family = "poisson")
            print(paste0( 'Model AIC For ', predictors[1],' ,', predictors[[2]] ,' ,', predictors[[3]],' ,', predictors[[4]],' ,', predictors[[5]],' & ', predictors[[6]],': ',  mod$aic))
            global_aic_list[[paste(predictors[1], predictors[2],predictors[3],predictors[4],predictors[5],predictors[6],sep = '_')]] <- mod$aic
            print('')
          }    
      
        else
          {
            mod <- glm(response ~ 0 + df_features[,predictors[1]] + df_features[,predictors[2]] + 
                         df_features[,predictors[3]] + df_features[,predictors[4]] + df_features[,predictors[5]] + 
                         df_features[,predictors[6]] + df_features[,predictors[7]], family = "poisson")
            print(paste0( 'Model AIC For ', predictors[1],' ,', predictors[2] ,' ,', predictors[3],' ,', predictors[4],' ,', predictors[5],' ,', predictors[6],' & ', predictors[7],': ',  mod$aic))
            global_aic_list[[paste(predictors[1], predictors[2],predictors[3],predictors[4],predictors[5],predictors[6],predictors[7],sep = '_')]] <- mod$aic
            print('')
          }
      }
    
    # province associated with the minimum aic
    best_prov_combination <- names(global_aic_list)[which.min(global_aic_list)]
    print(paste0('The best province combination is ',best_prov_combination,' with AIC value of: ',global_aic_list[which.min(global_aic_list)]))
    print('')
    
    # split the chosen predictors into a list
    chosen_predictors <- as.list(strsplit(best_prov_combination, "_")[[1]])
    
    # build final model from chosen best predictors 
    if (length(chosen_predictors) == 1)
      { 
        print(paste0( 'Building Final Model With ', length(chosen_predictors),' predictor.'))
        best_mod <- glm(response ~ 0 + df_features[,chosen_predictors[[1]]], family = "poisson")
        # create data frame to store actual cases and model predictions
        # extract coefficients
        coef_predictor_1 <- coef(best_mod)[1]
        
        # prediction values from model
        predicted = exp(coef_predictor_1 * df_features[,chosen_predictors[[1]]])
        
        df = data.frame(week = date, true_values = response, predictions = predicted)
      }
    else if (length(chosen_predictors) == 2)
      { 
        print(paste0( 'Building Final Model With ', length(chosen_predictors),' predictors'))
        best_mod <- glm(response ~ 0 + df_features[,chosen_predictors[[1]]] + df_features[,chosen_predictors[[2]]], family = "poisson")
        # create data frame to store actual cases and model predictions
        # extract coefficients
        coef_predictor_1 <- coef(best_mod)[1]
        coef_predictor_2 <- coef(best_mod)[2]
        
        # prediction values from model
        predicted = exp(coef_predictor_1 * df_features[,chosen_predictors[[1]]] + coef_predictor_2 * df_features[,chosen_predictors[[2]]])
        
        df = data.frame(week = date, true_values = response, predictions = predicted)
      }
    else if (length(chosen_predictors) == 3)
      { 
        print(paste0( 'Building Final Model With ', length(chosen_predictors),' predictors'))
        best_mod <- glm(response ~ 0 + df_features[,chosen_predictors[[1]]] + df_features[,chosen_predictors[[2]]] + 
                          df_features[,chosen_predictors[[3]]], family = "poisson")
        # create data frame to store actual cases and model predictions
        # extract coefficients
        coef_predictor_1 <- coef(best_mod)[1]
        coef_predictor_2 <- coef(best_mod)[2]
        coef_predictor_3 <- coef(best_mod)[3]
        
        # prediction values from model
        predicted = exp(coef_predictor_1 * df_features[,chosen_predictors[[1]]] + coef_predictor_2 * df_features[,chosen_predictors[[2]]] + 
                          coef_predictor_3 * df_features[,chosen_predictors[[3]]])
        
        df = data.frame(week = date, true_values = response, predictions = predicted)
      }
    else if (length(chosen_predictors) == 4)
      {     
        print(paste0( 'Building Final Model With ', length(chosen_predictors),' predictors'))
        best_mod <- glm(response ~ 0 + df_features[,chosen_predictors[[1]]] + df_features[,chosen_predictors[[2]]] + 
                          df_features[,chosen_predictors[[3]]] + df_features[,chosen_predictors[[4]]], family = "poisson")
        # create data frame to store actual cases and model predictions
        # extract coefficients
        coef_predictor_1 <- coef(best_mod)[1]
        coef_predictor_2 <- coef(best_mod)[2]
        coef_predictor_3 <- coef(best_mod)[3]
        coef_predictor_4 <- coef(best_mod)[4]
        
        # prediction values from model
        predicted = exp(coef_predictor_1 * df_features[,chosen_predictors[[1]]] + coef_predictor_2 * df_features[,chosen_predictors[[2]]] + 
                          coef_predictor_3 * df_features[,chosen_predictors[[3]]] + coef_predictor_4 * df_features[,chosen_predictors[[4]]])
        
        df = data.frame(week = date, true_values = response, predictions = predicted)
      }
    else if (length(chosen_predictors) == 5)
      { 
        print(paste0( 'Building Final Model With ', length(chosen_predictors),' predictors'))
        best_mod <- glm(response ~ 0 + df_features[,chosen_predictors[[1]]] + df_features[,chosen_predictors[[2]]] + 
                          df_features[,chosen_predictors[[3]]] + df_features[,chosen_predictors[[4]]] + df_features[,chosen_predictors[[5]]], family = "poisson")
        # create data frame to store actual cases and model predictions
        # extract coefficients
        coef_predictor_1 <- coef(best_mod)[1]
        coef_predictor_2 <- coef(best_mod)[2]
        coef_predictor_3 <- coef(best_mod)[3]
        coef_predictor_4 <- coef(best_mod)[4]
        coef_predictor_5 <- coef(best_mod)[5]
        
        
        # prediction values from model
        predicted = exp(coef_predictor_1 * df_features[,chosen_predictors[[1]]] + coef_predictor_2 * df_features[,chosen_predictors[[2]]] + 
                          coef_predictor_3 * df_features[,chosen_predictors[[3]]] + coef_predictor_4 * df_features[,chosen_predictors[[4]]] + 
                          coef_predictor_5 * df_features[,chosen_predictors[[5]]])
        
        df = data.frame(week = date, true_values = response, predictions = predicted)
      }
    else if (length(chosen_predictors) == 6)
      { 
        print(paste0( 'Building Final Model With ', length(chosen_predictors),' predictors'))
        best_mod <- glm(response ~ 0 + df_features[,chosen_predictors[[1]]] + df_features[,chosen_predictors[[2]]] + 
                          df_features[,chosen_predictors[[3]]] + df_features[,chosen_predictors[[4]]] + 
                          df_features[,chosen_predictors[[5]]] + df_features[,chosen_predictors[[6]]], family = "poisson")
        # extract coefficients
        coef_predictor_1 <- coef(best_mod)[1]
        coef_predictor_2 <- coef(best_mod)[2]
        coef_predictor_3 <- coef(best_mod)[3]
        coef_predictor_4 <- coef(best_mod)[4]
        coef_predictor_5 <- coef(best_mod)[5]
        coef_predictor_6 <- coef(best_mod)[6]
        
        # prediction values from model
        predicted = exp(coef_predictor_1 * df_features[,chosen_predictors[[1]]] + coef_predictor_2 * df_features[,chosen_predictors[[2]]] + 
                          coef_predictor_3 * df_features[,chosen_predictors[[3]]] + coef_predictor_4 * df_features[,chosen_predictors[[4]]] + 
                          coef_predictor_5 * df_features[,chosen_predictors[[5]]] + coef_predictor_6 * df_features[,chosen_predictors[[6]]])
        
        df = data.frame(week = date, true_values = response, predictions = predicted)
      }
    else 
      { 
        print(paste0( 'Building Final Model With ', length(chosen_predictors),' predictors'))
        best_mod <- glm(response ~ 0 + df_features[,chosen_predictors[[1]]] + df_features[,chosen_predictors[[2]]] + 
                          df_features[,chosen_predictors[[3]]] + df_features[,chosen_predictors[[4]]] + 
                          df_features[,chosen_predictors[[5]]] + df_features[,chosen_predictors[[6]]] + df_features[,chosen_predictors[[7]]], family = "poisson")
        
        # extract coefficients
        coef_predictor_1 <- coef(best_mod)[1]
        coef_predictor_2 <- coef(best_mod)[2]
        coef_predictor_3 <- coef(best_mod)[3]
        coef_predictor_4 <- coef(best_mod)[4]
        coef_predictor_5 <- coef(best_mod)[5]
        coef_predictor_6 <- coef(best_mod)[6]
        coef_predictor_7 <- coef(best_mod)[7]
        
        # prediction values from model
        predicted = exp(coef_predictor_1 * df_features[,chosen_predictors[[1]]] + coef_predictor_2 * df_features[,chosen_predictors[[2]]] + 
                          coef_predictor_3 * df_features[,chosen_predictors[[3]]] + coef_predictor_4 * df_features[,chosen_predictors[[4]]] + 
                          coef_predictor_5 * df_features[,chosen_predictors[[5]]] + coef_predictor_6 * df_features[,chosen_predictors[[6]]] +
                          coef_predictor_7 * df_features[,chosen_predictors[[7]]])
        
        df = data.frame(week = date, true_values = response, predictions = predicted)
      }
    # visualize the data frame by ploting the week against the total cases
    trav_graph <- ggplot(df, aes(week, true_values)) +
      geom_area()+
      geom_line(aes(y = true_values),color = 'gold2') +
      geom_line(aes(y = predictions),color = 'red') +
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

active <- read.csv("travel.csv")
model_plots(active,prov = 'PEI')
