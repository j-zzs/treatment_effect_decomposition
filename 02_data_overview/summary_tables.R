# load required libraries
library(tidyverse)
library(caret)
library(xtable)
source("./data_preparation/load_data.R")

outpath = "./data_overview/output/"


########### functions #############

# 1/2 flags to binary flag, now 1 means yes, 0 no (1 male, female 0)
binary_recode = function(x){
  if (is.numeric(x)){
    return(abs(x-2))  
  } else {
    return(ifelse(x == "Y", 1, 0))
  }
}

# function to derive standardized difference
std_diff = function(x,y){
  x_mean = mean(x, na.rm = TRUE)
  y_mean = mean(y, na.rm = TRUE)
  
  var_x = var(x, na.rm = TRUE)
  var_y = var(y, na.rm = TRUE)
  
  norm_diff = (x_mean - y_mean)/ (sqrt(var_x + var_y))
  return(norm_diff)
}


data_preparation = function(data, relevant_vars){
  # recode binary flags
  flags = c("mar", "gest_diabetes", "prepreg_diabetes",
            "gest_hypertension", "prepreg_hypertension", "eclamp",
            "preterm", "cesarean","paternity")
  data[,flags] = lapply(data[,flags], binary_recode)
  
  # recode sex of child
  data$sex = ifelse(data$sex == "M", 1, 0)
  
  # recode birht place
  data$birthplace = ifelse(data$birthplace == 1, 1, 0)
  
  # recode variables of interest to factors
  cat_features = c("meduc", "mrace", "ormoth", 
                   "restatus", "frace", "orfath", "birmon",
                   "payment")
  data[,cat_features] = lapply(data[,cat_features], factor)
  
  # recode cigar, since 99 = NA are not discarded automatically
  data$cig_0 = ifelse(data$cig_0 == 99, NA, data$cig_0)
  data$cig_1 = ifelse(data$cig_1 == 99, NA, data$cig_1)
  data$cig_2 = ifelse(data$cig_2 == 99, NA, data$cig_2)
  data$cig_3 = ifelse(data$cig_3 == 99, NA, data$cig_3)
  
  # add prepregnancy smoking flag
  data$smoke_prepreg = ifelse(data$cig_0 >0, 1, 0)
  
  # add LBW flag
  data$LBW = ifelse(data$birwt <2500, 1, 0)
  
  # recode bmi
  data$bmi = as.numeric(gsub(",", ".", data$bmi))
  
  # delete first birth from interval since last live birth
  data$interval_last_livebirth = ifelse(data$interval_last_livebirth == 888, NA,
                                        ifelse(data$interval_last_livebirth == 999, NA, data$interval_last_livebirth))
  # ov relevant variables 
  data = data %>% select(relevant_vars)
  
  # one hot encoding
  dmy = dummyVars("~ . ", data = data)
  data = data.frame(predict(dmy, newdata = data))
  return(data)
}


smoking_ov = function(data_combined, relevant_vars_renaming){
  
  relevant_vars = unique(gsub("[-^0-9]|[[:punct:]]", "", relevant_vars_renaming))
  #load clear names of variables
  renaming = read.csv2("./code/summary_tables/renaming.csv")
  renaming = renaming %>% filter(column %in% relevant_vars_renaming)
  
  smokers = data_combined %>% filter(tobacco ==1)
  nonsmokers = data_combined %>% filter(tobacco ==0)
  
  # get mean and sd smokers
  mean_data = smokers %>%  summarise_all(funs( mean(., na.rm = TRUE))) %>% gather(column, Mean)
  sd_data = smokers %>%  summarise_all(funs( sd(., na.rm = TRUE))) %>% gather(column, "Standard Deviation")
  data_smoker = mean_data %>% left_join(sd_data, by = "column")
  # get mean and sd non smokers
  mean_data = nonsmokers %>%  summarise_all(funs( mean(., na.rm = TRUE))) %>% gather(column, Mean)
  sd_data = nonsmokers %>%  summarise_all(funs( sd(., na.rm = TRUE))) %>% gather(column, "Standard Deviation")
  data_nonsmoker = mean_data %>% left_join(sd_data, by = "column")
  
  #get normalized difference (instead of t stat)
  norm_diff = data_combined %>%
    summarise_each(funs(std_diff(.[tobacco == 1], .[tobacco == 0])), vars = - tobacco) %>% 
    gather(column, "norm_diff")
  
  # join results
  data_overview = data_nonsmoker %>% 
    left_join(data_smoker, by = "column") %>% 
    left_join(norm_diff, by = "column")
  data_overview = renaming %>% left_join(data_overview, by = "column")
  data_overview = data_overview %>% select(-column)
  
  return(data_overview)
}


data_ov = function(years, relevant_vars, relevant_vars_renaming){
  
  relevant_vars = unique(gsub("\\.[0-9]$", "", relevant_vars_renaming))
  #load clear names of variables
  renaming = read.csv2("./code/summary_tables/renaming.csv")
  data_overview = renaming %>% filter(column %in% relevant_vars_renaming)
  
  for (year in years){
    
    # load prepared dataframe
    data <- load_data(year)
    data = data.frame(data)
    
    data = data_preparation(data, relevant_vars)
    
    # get mean and sd for entire population
    mean_data = data %>%  summarise_all(funs( mean(., na.rm = TRUE))) %>% gather(column, Mean)
    sd_data = data %>%  summarise_all(funs( sd(., na.rm = TRUE))) %>% gather(column, "Standard Deviation")
    data_total = mean_data %>% left_join(sd_data, by = "column")
    
    # rename data
    data_overview = data_overview %>% left_join(data_total, by = "column")
    rm(data)
  }
  data_overview = data_overview %>% select(-column)
  return(data_overview)
}

############## overview by smoking status

# specify relevant years and variables
years = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
relevant_vars = c("birwt", "LBW", "apgar5" ,"sex", "gestat",
                  "tobacco", "cig_0", "cig_1", "cig_2", "cig_3",
                  "wtgain", "nprevis",
                  "monpre", "dlivord", "dtotord", "gest_diabetes", 
                  "prepreg_diabetes","gest_hypertension", "prepreg_hypertension",
                  "eclamp", "mage", "mar", "mrace",
                  "meduc", "ormoth", "restatus", "bmi", "cesarean", "paternity",
                  "priorterm", "preterm", "payment", "birthplace", 
                  "interval_last_livebirth", "smoke_prepreg")


# load data 
data_combined = combine_data(years)
setDF(data_combined)

data_combined = data_preparation(data_combined, relevant_vars)

child_ov = smoking_ov(data_combined = data_combined, 
                      relevant_vars_renaming = c("birwt", "LBW", "apgar5", "sex", "gestat", "cesarean", "birthplace"))
child_ov.table= xtable(child_ov)
print(child_ov.table, 
      file = file.path(outpath, "child_table_smoker_norm_diff.tex"),
      hline.after = NULL,
      NA.string="-", 
      floating = FALSE, 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      only.contents = TRUE)


mother_ov = smoking_ov(data_combined = data_combined,
                       relevant_vars_renaming = c("mage","mar","mrace.1","mrace.2",
                                                  "mrace.3","mrace.4", "meduc.1", 
                                                  "meduc.2", "meduc.3", "meduc.4",
                                                  "meduc.5","meduc.6", "ormoth.0",
                                                  "ormoth.1", "ormoth.2", "ormoth.3",
                                                  "ormoth.4", "payment.1", "payment.2",
                                                  "payment.3", "restatus.1", 
                                                  "restatus.2", "restatus.3", "restatus.4"))
mother_ov.table= xtable(mother_ov)
print(mother_ov.table, 
      file = file.path(outpath, "mother_table_smoker_norm_diff.tex"),
      hline.after = NULL,
      NA.string="-", 
      floating = FALSE, 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      only.contents = TRUE)


risk_ov = smoking_ov(data_combined = data_combined,
                     relevant_vars_renaming = c("tobacco", "smoke_prepreg", "cig_0", "cig_1", "cig_2", "cig_3",
                                                "bmi", "wtgain", "nprevis", "monpre", 
                                                "dlivord", "dtotord", "priorterm", "preterm", "interval_last_livebirth",
                                                "gest_diabetes", "prepreg_diabetes", "gest_hypertension", 
                                                "prepreg_hypertension","eclamp"))
risk_ov.table= xtable(risk_ov)
print(risk_ov.table, 
      file = file.path(outpath, "risk_table_smoker_norm_diff.tex"),
      hline.after = NULL,
      NA.string="-", 
      floating = FALSE, 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      only.contents = TRUE)


############### data overview by years



child_ov = data_ov(years = c(2011, 2013), 
                   relevant_vars = relevant_vars,
                   relevant_vars_renaming = c("birwt", "LBW", "apgar5", "sex", "gestat", "cesarean", "birthplace"))
child_ov.table= xtable(child_ov)
print(child_ov.table, 
      file = file.path(outpath, "child_table_years.tex"),
      hline.after = NULL,
      NA.string="-", 
      floating = FALSE, 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      only.contents = TRUE)


mother_ov = data_ov(years = c(2011, 2013, 2015, 2018), 
                    relevant_vars = relevant_vars,
                    relevant_vars_renaming = c("mage","mar","mrace.1","mrace.2",
                                               "mrace.3","mrace.4", "meduc.1", 
                                               "meduc.2", "meduc.3", "meduc.4",
                                               "meduc.5","meduc.6", "ormoth.0",
                                               "ormoth.1", "ormoth.2", "ormoth.3",
                                               "ormoth.4", "payment.1", "payment.2",
                                               "payment.3", "restatus.1", 
                                               "restatus.2", "restatus.3", "restatus.4"))
mother_ov.table= xtable(mother_ov)
print(mother_ov.table, 
      file = file.path(outpath, "mother_table_years.tex"),
      hline.after = NULL,
      NA.string="-", 
      floating = FALSE, 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      only.contents = TRUE)


risk_ov = data_ov(years = c(2011, 2013, 2015, 2018), 
                  relevant_vars = relevant_vars,
                  relevant_vars_renaming = c("tobacco", "smoke_prepreg", "cig_0", "cig_1", "cig_2", "cig_3",
                                             "bmi", "wtgain", "nprevis", "monpre", 
                                             "dlivord", "dtotord", "priorterm", "preterm", "interval_last_livebirth",
                                             "gest_diabetes", "prepreg_diabetes", "gest_hypertension", 
                                             "prepreg_hypertension","eclamp"))
risk_ov.table= xtable(risk_ov)
print(risk_ov.table,
      file = file.path(outpath, "risk_table_years.tex"),
      hline.after = NULL,
      NA.string="-", 
      floating = FALSE, 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      only.contents = TRUE)




for (year in  c(2011, 2013, 2015, 2018)){
  
  
  # load prepared dataframe
  data <- load_data(year, "birth", "revised")
  data = data.frame(data)
  print(nrow(data))
  rm(data)
}
