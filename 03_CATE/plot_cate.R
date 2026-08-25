set.seed(12345)
setwd("C:/Users/JKutz/Documents/GitProjects/heterogeneity")

# load data loading function
source("./data_preparation/load_data.R")
# load function for cf
source("./CATE/cate_plotting_functions.R")

outpath = "./CATE/output/"

# load prepared + recoded data
data_combined = combine_data(years =  c(2011,2013,2015,2018))
setDF(data_combined)


# fix factor handling
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

data_combined[,cat_features] = lapply(data_combined[,cat_features], factor)
data_combined = data_combined %>% slice(., sample(nrow(.),nrow(.)/10))



##################### estimate + plot results #########################

covariates <- c(
  "sex", "wtgain", "mage", "meduc", "mrace",                   
  "ormoth", "mar", "paternity", "fage", "feduc", "frace", 
  "orfath", "monpre", "nprevis", "priorterm", "dlivord",                 
  "wic", "prepreg_diabetes", 
  "gest_diabetes", "prepreg_hypertension", "gest_hypertension",       
  "preterm", "infertility_treatment", "fertility_drugs",         
  "reproductive_assistance", "cesarean", "cesarean_n",              
  "chlamydia",  "gonorrhea", "syphilis",
  "hepatitis_c", "hepatitis_b",            
  "payment", "bmi", "interval_last_livebirth", 
  "interval_last_otherbirth", "mothers_height", "birmon",                  
  "birthplace"
)


####### plot CATE bw_std
result_bwstd <- train_causal_forest(
  data = data_combined,
  treatment = "tobacco",
  outcome = "bw_standardized",
  covariates = covariates,
  num.trees = 2000
)

saveRDS(
  result_bwstd,
  file = file.path(outpath, "result_bwstd.rds")
)

plot_result_bwstd <- plot_causal_forest_results(
  data = data_combined[result_bwstd$test_indices,],
  predictions = result_bwstd$test_predictions,
  ate = result_bwstd$ate,
  plot_title = "Effect of Maternal Smoking on Standardized Birth Weight",
  plot_path = outpath,
  plot_filename = "CATE_bw_std_CI.png",
  plot_width_cm = 24,
  plot_height_cm = 12
)

rm(result_bwstd, plot_result_bwstd)
gc()


####### plot CATE apgar
result_apgar <- train_causal_forest(
  data = data_combined,
  treatment = "tobacco",
  outcome = "apgar5",
  covariates = covariates,
  num.trees = 2000
)

saveRDS(
  result_bwstd,
  file = file.path(outpath, "result_apgar.rds")
)


plot_result_apgar <- plot_causal_forest_results(
  data = data_combined[result_apgar$test_indices,],
  predictions = result_apgar$test_predictions,
  ate = result_apgar$ate,
  plot_title = "Effect of Maternal Smoking on APGAR Score",
  plot_path = outpath,
  plot_filename = "apgar_CATE_CI.png",
  plot_width_cm = 24,
  plot_height_cm = 12
)


rm(result_apgar, plot_result_apgar)
gc()


####### plot CATE LBW
result_lbw <- train_causal_forest(
  data = data_combined,
  treatment = "tobacco",
  outcome = "LBW",
  covariates = covariates,
  num.trees = 2000
)


saveRDS(
  result_bwstd,
  file = file.path(outpath, "result_lbw.rds")
)


plot_result_lbw <- plot_causal_forest_results(
  data = data_combined[result_lbw$test_indices,],
  predictions = result_lbw$test_predictions,
  ate = result_lbw$ate,
  plot_title = "Effect of Maternal Smoking on Low Birth Weight Birth",
  plot_path = outpath,
  plot_filename = "CATE_lbw_CI.png",
  plot_width_cm = 24,
  plot_height_cm = 12
)


rm(result_lbw, plot_result_lbw)
gc()

####### plot CATE birthweight
result_bw <- train_causal_forest(
  data = data_combined,
  treatment = "tobacco",
  outcome = "birwt",
  covariates = covariates,
  num.trees = 2000
)


saveRDS(
  result_bwstd,
  file = file.path(outpath, "result_bw.rds")
)


plot_result_bw <- plot_causal_forest_results(
  data = data_combined[result_bw$test_indices,],
  predictions = result_bw$test_predictions,
  ate = result_bw$ate,
  plot_title = "Effect of Maternal Smoking on Birth Weight Birth",
  plot_path = outpath,
  plot_filename = "CATE_CI.png",
  plot_width_cm = 24,
  plot_height_cm = 12)


rm(result_bw, plot_result_bw)
gc()


# ate results
result_bw <- readRDS(file.path(outpath, "result_bw.rds"))
result_lbw <- readRDS(file.path(outpath, "result_lbw.rds"))
result_apgar <- readRDS(file.path(outpath, "result_apgar.rds"))
result_bwstd <- readRDS(file.path(outpath, "result_bwstd.rds"))



ate_results <- list(
  bw = result_bw,
  lbw = result_lbw,
  apgar = result_apgar,
  bwstd = result_bwstd
)

# Save to .tex file
output_path <- file.path(outpath, "ate_table.tex")
generate_ate_latex(ate_results, file = output_path)