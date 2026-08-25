################################################
# This file contains the Effect Decomposition Function
################################################

# add functionality to set breaks manually (for categorical variables!)
library(grf)
library(Counterfactual)

#' Function to perform effect decomposition
#' 
#' In order to use groups different to the levels in a categorical variable
#' (e.g. use some sort of regrouping or aggregating of groups), 
#' the regrouped variable should be added to the data frame BEFORE
#' calling the function.
#' 
#' In a first step, the data is randomly split into aux and main sample.
#' This function then makes use of the causal_forest function from the grf 
#' package to predict treatment effects. The causal forest is trained on the 
#' aux-data sample, whereas the main sample is used to derive the effect 
#' decomposition. Then groups of interest are derived from the variable to 
#' be split at. The function loops through all possible groups and compares 
#' treatment effect distributions. To do so it uses counterfactual from 
#' Counterfactual to estimate the counterfactual distribution as well as 
#' total effect, structure effect and composition effect.
#' 
#' @param data data be be used for effect decomposition
#' @param split_var variable of interest, whose effect on the treatment effect should be analyzed.
#' @param var_exclude names of vaariables to be excluded from decomposition. Needed for excluding variables form highly skewed groups.
#' @param name_recoded name of recoded variable
#' @param type type of split_var, either "cont" or "cat"
#' @param n_sim number of random splits
#' @param n_groups number of groups that should be developed for continuous variables.
#'                 will be obsolete in case type = "cat"
#' @param breaks in case manual breaks should be set, breaks parameter can be used, if NULL, quantiles are used
#' @param alpha significance level of the derived CIs 
#'            (needs to be taken *2 in order to get confidence level accounting for splitting uncertainty)
#' @param quantiles points where distributions are estimated and evaluated
#' @param nreg number of quantile regression used to estimate counterfactual distribution
#' @param downsample_ratio only take random subsample of 1/downsample_ratio of data, defaults to 10
#' @param  cores number of cores used for decomposition, defaults to 2
#' 
#' @return list of data frames containing results for te,se,ce and quantiles of distributions
#' 
effect_decomposition = function(data, outcome_var, split_var, var_exclude = NULL, name_recoded = NULL, type, n_sim, n_groups,  breaks = NULL,
                                alpha = 0.05, quantiles = c(1:99)/100, nreg = 100, downsample_ratio = 10, cores =2, crossfit_nuisances = FALSE){
  
  set.seed(5678)
  # define result names
  # store effects
  total_effect_df       = c()
  structure_effect_df   = c()
  composition_effect_df = c()
  # store results for quantile plots
  quantiles_1_df              = c()
  quantiles_0_df              = c()
  quantiles_counterfactual_df = c()
  
  # remove splitting variable from all variables used for counterfactual analysis
  vars = c("sex", "wtgain", "mage", "meduc", "mrace",                   
           "ormoth", "mar", "paternity",               
           "fage", "feduc", "frace", "orfath",                
           "monpre", "nprevis", "priorterm", "dlivord",                 
           "wic", "gestat",                  
           "prepreg_diabetes", "gest_diabetes", "prepreg_hypertension", "gest_hypertension",       
           "preterm", "infertility_treatment", "fertility_drugs",         
           "reproductive_assistance", "cesarean", "cesarean_n",               
           "chlamydia", 
           "payment", "bmi",                     
           "interval_last_livebirth", "interval_last_otherbirth", "mothers_height", "birmon",                  
           "birthplace")
  vars_counterfactual = vars[!vars %in% split_var]
  # exclude variables
  vars_counterfactual = vars_counterfactual[!vars_counterfactual %in% var_exclude]
  # remove sex as it is already part of standardization
  if (identical(outcome_var, "bw_standardized")) { 
    vars_counterfactual <- setdiff(vars_counterfactual, c("sex"))
  }
  #variables for forest
  vars_forest <- vars[!vars %in% c("gestat")]
  # remove sex if already accounted for in birthweight standardization
  if (identical(outcome_var, "bw_standardized")) {
    vars_forest = setdiff(vars_forest, "sex")}
  
  # in case no manually recoded variable is used, use quantiles for continuous and groups for categorical variables
  if (is.null(name_recoded)){
    if(type == "cont"){
      # get groups to be used for all splits
      if(is.null(breaks)){
        breaks              = quantile(data[,split_var], seq(0,1,1/n_groups))
      }
      data$interval_label = cut(data[,split_var], breaks = breaks, include.lowest = TRUE)
      data$interval_int   = cut(data[,split_var], breaks = breaks, include.lowest = TRUE, labels = FALSE)
      n_groups = length(levels(data$interval_label))
      #store group names
      group_names = levels(data$interval_label)
    } else if (type =="cat"){
      # data interval label will be original splitting var
      data$interval_label = as.factor(data[,split_var])
      #store group names
      group_names = levels(data$interval_label)
      n_groups = length(levels(data$interval_label))
    }
  } else { # in case manually recoded variable is used
    # data interval label will be recoded splitting var
    data$interval_label = as.factor(data[,name_recoded])
    print(data$interval_label)
    #store group names
    group_names = levels(data$interval_label)
    n_groups = length(levels(data$interval_label))
  }
  
  # recode categorical variables of interest to factors
  cat_features = c("sex", "meduc", "mrace",                   
                   "ormoth", "mar", "paternity",               
                   "feduc", "frace", "orfath",                
                   "wic",                   
                   "prepreg_diabetes", "gest_diabetes", "prepreg_hypertension", "gest_hypertension",       
                   "preterm", "infertility_treatment", "fertility_drugs",         
                   "reproductive_assistance", "cesarean",              
                   "chlamydia",            
                   "payment",                     
                   "birmon", "birthplace")
  data[,cat_features] = lapply(data[,cat_features], factor)
  
  # donwsampling of data
  train_data = data %>% slice(., sample(nrow(.),nrow(.)/downsample_ratio))
  
  for (rep in 1:n_sim){
    print(paste0(">>>>>>>>>>>>>> run: ", rep, "  >>>>>>>>>>>>>"))
    ################# Prediction of Treatment Effect ##############
    
    #split sample in half
    idx         = sample(c(TRUE,FALSE),nrow(train_data), replace = TRUE)
    aux_sample  = train_data[idx,]
    main_sample = train_data[!idx,]
    
    # Prepare data
    W_train <- aux_sample$tobacco
    Y_train <- aux_sample[[outcome_var]]
    X_train <- aux_sample %>% dplyr::select(all_of(vars_forest))
    X_train_encoded <- model.matrix(~ 0 + ., X_train) %>% data.frame()
    
    n <- nrow(X_train_encoded)
    W.hat <- rep(NA, n)
    Y.hat <- rep(NA, n)
    
    if (crossfit_nuisances) {
      print(paste0(">>>>>>>>>>>>>> Crossfitting nuisances for run ", rep, "  >>>>>>>>>>>>>"))
      folds <- sample(rep(1:5, length.out = n))
      
      for (k in 1:5) {
        print(paste0(">>>>>>>>>>>>>> fold ", k, "  >>>>>>>>>>>>>"))
        train.idx <- which(folds != k)
        test.idx <- which(folds == k)
        
        forest.W <- regression_forest(X_train_encoded[train.idx, ], W_train[train.idx])
        forest.Y <- regression_forest(X_train_encoded[train.idx, ], Y_train[train.idx])
        
        W.hat[test.idx] <- predict(forest.W, X_train_encoded[test.idx, ])$predictions
        Y.hat[test.idx] <- predict(forest.Y, X_train_encoded[test.idx, ])$predictions
      }
      
    } else {

      W.hat <- NULL
      Y.hat <- NULL
    }
    
    # Fit causal forest using nuisance predictions
    cf <- causal_forest(X_train_encoded, Y_train, W_train,
                        W.hat = W.hat,
                        Y.hat = Y.hat,
                        honesty = TRUE)
    
    # predict cate on main sample 
    newdata = main_sample %>% dplyr::select(all_of(vars_forest))
    newdata <- model.matrix(~ 0 + ., newdata) %>% data.frame()
    tau.hat <- predict(cf, newdata = newdata, estimate.variance = TRUE)
    
    main_sample$tau = tau.hat$predictions
    
    
    ############ Decomposition ##################
    # loop over groups (leave out group 1, which will be reference group)
    for (i in seq(2,n_groups,1)) {
      print(paste0(">>>>>>>>>>>>>> group comparison: ", i, "  >>>>>>>>>>>>>"))
      if (is.null(name_recoded)){
        if(type == "cont"){
          #recode groups
          main_sample$group <- ifelse(main_sample$interval_int == 1, 0, ifelse(main_sample$interval_int == i, 1, -1))
        } else if(type =="cat"){
          main_sample$group <- ifelse(main_sample$interval_label == group_names[1], 0, ifelse(main_sample$interval_label == group_names[i], 1, -1))
        }
      } else {
        main_sample$group <- ifelse(main_sample$interval_label == group_names[1], 0, ifelse(main_sample$interval_label == group_names[i], 1, -1))
      }
      
      # filter out unused obs
      main_sample_cut = main_sample[main_sample$group != -1,]
      # now we are left with the right data for each iteration
      # estimate counterfactual effect
      counterfac <- counterfactual(as.formula(paste("tau", paste(vars_counterfactual, collapse=" + "), sep=" ~ ")),
                                   data = main_sample_cut,
                                   group=group, treatment=TRUE, quantiles=quantiles,
                                   method="logit",  alpha = alpha, nreg= nreg, weightedboot = TRUE,
                                   printdeco=FALSE, decomposition = TRUE,
                                   sepcore = TRUE, ncore=cores)
      
      # store results
      total_effect_df = rbind(total_effect_df, c( c("effect", i-1),(counterfac$resTE)[,1]))
      total_effect_df = rbind(total_effect_df, c(c("upper", i-1),(counterfac$resTE)[,3]))
      total_effect_df = rbind(total_effect_df, c(c("lower", i-1),(counterfac$resTE)[,4]))
      
      structure_effect_df = rbind(structure_effect_df, c(c("effect", i-1),(counterfac$resSE)[,1]))
      structure_effect_df = rbind(structure_effect_df, c(c("upper", i-1),(counterfac$resSE)[,3]))
      structure_effect_df = rbind(structure_effect_df, c(c("lower", i-1),(counterfac$resSE)[,4]))
      
      composition_effect_df = rbind(composition_effect_df, c(c("effect", i-1),(counterfac$resCE)[,1]))
      composition_effect_df = rbind(composition_effect_df, c(c("upper", i-1),(counterfac$resCE)[,3]))
      composition_effect_df = rbind(composition_effect_df, c(c("lower", i-1),(counterfac$resCE)[,4]))
      
      # store results for quantile plots
      #group 1
      quantiles_1_df = rbind(quantiles_1_df, c( c("distr", i-1),(counterfac$model_quantile_ref1)[,1]))
      quantiles_1_df = rbind(quantiles_1_df, c( c("upper", i-1),(counterfac$model_quantile_ref1)[,3]))
      quantiles_1_df = rbind(quantiles_1_df, c( c("lower", i-1),(counterfac$model_quantile_ref1)[,4]))
      # reference group 0
      quantiles_0_df = rbind(quantiles_0_df, c( c("distr", i-1),(counterfac$model_quantile_ref0)[,1]))
      quantiles_0_df = rbind(quantiles_0_df, c( c("upper", i-1),(counterfac$model_quantile_ref0)[,3]))
      quantiles_0_df = rbind(quantiles_0_df, c( c("lower", i-1),(counterfac$model_quantile_ref0)[,4]))
      # counterfactual distribution
      quantiles_counterfactual_df = rbind(quantiles_counterfactual_df, 
                                          c( c("distr", i-1),(counterfac$model_quantile_counter)[,1]))
      quantiles_counterfactual_df = rbind(quantiles_counterfactual_df, 
                                          c( c("upper", i-1),(counterfac$model_quantile_counter)[,3]))
      quantiles_counterfactual_df = rbind(quantiles_counterfactual_df, 
                                          c( c("lower", i-1),(counterfac$model_quantile_counter)[,4]))
    }
  }
  return(list(total_effect             = as.data.frame(total_effect_df), 
              structure_effect         = as.data.frame(structure_effect_df), 
              composition_effect       = as.data.frame(composition_effect_df), 
              quantiles_1              = as.data.frame(quantiles_1_df),
              quantiles_0              = as.data.frame(quantiles_0_df), 
              quantiles_counterfactual = as.data.frame(quantiles_counterfactual_df),
              groups                   = group_names))
}


