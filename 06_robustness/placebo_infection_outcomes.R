set.seed(123456)

#### Placebo analysis
# check whether smoking has an effect on STDs

# load data loading function
source("./data_preparation/load_data.R")
source("./CATE/cate_plotting_functions.R")
library(grf)
library(margins)
library(tidyverse)
library(xtable)
library(ggplot2)

# load prepared + recoded data
data_combined = combine_data(years = c(2011,2012,2013, 2014, 2015, 2016, 2017, 2018))
setDF(data_combined)


#### run causal forests
set.seed(1234)
data_sample <- data_combined %>%
  slice_sample(prop = 1/10)


# gonnorhea
result_gon =  train_causal_forest(data = data_sample,
                    treatment = "tobacco",
                    outcome = "gonorrhea",
                    covariates = c("sex", "wtgain", "mage", "meduc", "mrace", 
                                   "ormoth", "mar", "paternity", "fage", "feduc", "frace", 
                                   "orfath", "monpre", "nprevis", "priorterm", "dlivord", 
                                   "wic", "prepreg_diabetes", "gest_diabetes", "prepreg_hypertension", "gest_hypertension", 
                                   "preterm", "infertility_treatment", "fertility_drugs", 
                                   "reproductive_assistance", "cesarean", "cesarean_n", 
                                   "payment", "bmi", "interval_last_livebirth", 
                                   "interval_last_otherbirth", "mothers_height", "birmon", 
                                   "birthplace"),
                    num.trees = 1000)

# syphilis
result_syp =  train_causal_forest(data = data_sample,
                    treatment = "tobacco",
                    outcome = "syphilis",
                    covariates = c("sex", "wtgain", "mage", "meduc", "mrace", 
                                   "ormoth", "mar", "paternity", "fage", "feduc", "frace", 
                                   "orfath", "monpre", "nprevis", "priorterm", "dlivord", 
                                   "wic", "prepreg_diabetes", "gest_diabetes", "prepreg_hypertension", "gest_hypertension", 
                                   "preterm", "infertility_treatment", "fertility_drugs", 
                                   "reproductive_assistance", "cesarean", "cesarean_n", 
                                   "payment", "bmi", "interval_last_livebirth", 
                                   "interval_last_otherbirth", "mothers_height", "birmon", 
                                   "birthplace"),
                    num.trees = 1000)

# chlamydia
result_cla =  train_causal_forest(data = data_sample,
                    treatment = "tobacco",
                    outcome = "chlamydia",
                    covariates = c("sex", "wtgain", "mage", "meduc", "mrace", 
                                   "ormoth", "mar", "paternity", "fage", "feduc", "frace", 
                                   "orfath", "monpre", "nprevis", "priorterm", "dlivord", 
                                   "wic", "prepreg_diabetes", "gest_diabetes", "prepreg_hypertension", "gest_hypertension", 
                                   "preterm", "infertility_treatment", "fertility_drugs", 
                                   "reproductive_assistance", "cesarean", "cesarean_n", 
                                   "payment", "bmi", "interval_last_livebirth", 
                                   "interval_last_otherbirth", "mothers_height", "birmon", 
                                   "birthplace"),
                    num.trees = 1000)




# hepatitis_c
result_hepc = train_causal_forest(data = data_sample,
                    treatment = "tobacco",
                    outcome = "hepatitis_c",
                    covariates = c("sex", "wtgain", "mage", "meduc", "mrace", 
                                   "ormoth", "mar", "paternity", "fage", "feduc", "frace", 
                                   "orfath", "monpre", "nprevis", "priorterm", "dlivord", 
                                   "wic", "prepreg_diabetes", "gest_diabetes", "prepreg_hypertension", "gest_hypertension", 
                                   "preterm", "infertility_treatment", "fertility_drugs", 
                                   "reproductive_assistance", "cesarean", "cesarean_n", 
                                   "payment", "bmi", "interval_last_livebirth", 
                                   "interval_last_otherbirth", "mothers_height", "birmon", 
                                   "birthplace"),
                    num.trees = 1000)


# hepatitis_b
result_hepcb = train_causal_forest(data = data_sample,
                    treatment = "tobacco",
                    outcome = "hepatitis_b",
                    covariates = c("sex", "wtgain", "mage", "meduc", "mrace", 
                                   "ormoth", "mar", "paternity", "fage", "feduc", "frace", 
                                   "orfath", "monpre", "nprevis", "priorterm", "dlivord", 
                                   "wic", "prepreg_diabetes", "gest_diabetes", "prepreg_hypertension", "gest_hypertension", 
                                   "preterm", "infertility_treatment", "fertility_drugs", 
                                   "reproductive_assistance", "cesarean", "cesarean_n", 
                                   "payment", "bmi", "interval_last_livebirth", 
                                   "interval_last_otherbirth", "mothers_height", "birmon", 
                                   "birthplace"),
                    num.trees = 1000)



# get baseline outcome means
baseline_chlamydia= mean(data_combined$chlamydia[data_combined$tobacco == 0])
baseline_hepatitis_b= mean(data_combined$hepatitis_b[data_combined$tobacco == 0])
baseline_hepatitis_c= mean(data_combined$hepatitis_c[data_combined$tobacco == 0])
baseline_syphilis= mean(data_combined$syphilis[data_combined$tobacco == 0])
baseline_gonorrhea= mean(data_combined$gonorrhea[data_combined$tobacco == 0])


# Create estimates data frame
estimates <- data.frame(
  outcome = c("Chlamydia", "Gonorrhea", "Hepatitis C", "Hepatitis B", "Syphilis"),
  estimate = c(result_cla$ate[1],
               result_gon$ate[1],
               result_hepc$ate[1],
               result_hepcb$ate[1],
               result_syp$ate[1]),
  se = c(result_cla$ate[2],
         result_gon$ate[2],
         result_hepc$ate[2],
         result_hepcb$ate[2],
         result_syp$ate[2]),
  baseline = c(baseline_chlamydia,
               baseline_gonorrhea,
               baseline_hepatitis_c,
               baseline_hepatitis_b,
               baseline_syphilis)
)

estimates <- estimates %>%
  mutate(
    sd_control = sqrt(baseline * (1 - baseline)),
    standardized_ate = estimate / sd_control
  )


# Subset and format estimates
table_df <- estimates %>%
  select(outcome, estimate, se, baseline, standardized_ate) %>%
  mutate(
    estimate = round(estimate, 5),
    se = round(se, 5),
    baseline = round(baseline, 5),
    standardized_ate = round(standardized_ate, 3)
  )

# Generate LaTeX table
latex_table <- xtable(table_df,
                      caption = "Treatment Effects on STI Testing Outcomes",
                      label = "tab:sti_effects",
                      align = c("l", "l", "r", "r", "r", "r"))

# Print LaTeX
print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)

