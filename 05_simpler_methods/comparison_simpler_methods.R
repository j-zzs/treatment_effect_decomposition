set.seed(123)

# load data
source("./data_preparation/load_data.R")


library(tidyverse)
library(grf)
library(tinytable)
library(modelsummary)


# load prepared + recoded data
data = combine_data(years = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) # 
setDF(data)

data = data %>% mutate(mar = ifelse(mar == 1, 1, 0))

downsample_ratio = 10
  

# 1. estimate CATE, stratify by quantiles of CATE, and compare treatment effect by qunatile group
train_data <- data %>% slice(sample(nrow(.), nrow(.) / downsample_ratio))

rm(data)
gc()


# Split data
idx <- sample(c(TRUE, FALSE), nrow(train_data), replace = TRUE)
aux_sample <- train_data[idx, ]
main_sample <- train_data[!idx, ]


# remove splitting variable from all variables used for counterfactual analysis
control_vars = c("sex", "wtgain", "mage", "meduc", "mrace",                   
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

# Train causal forest
W_train <- aux_sample$tobacco
Y_train <- aux_sample[["birwt"]]
X_train <- aux_sample %>% dplyr::select(all_of(control_vars))
X_train_encoded <- model.matrix(~ 0 + ., X_train) %>% data.frame()

cf <- causal_forest(X_train_encoded, Y_train, W_train, honesty = TRUE)

# Predict CATE on main sample
X_main <- main_sample %>% dplyr::select(all_of(control_vars))
X_main_encoded <- model.matrix(~ 0 + ., X_main) %>% data.frame()
tau_hat <- predict(cf, newdata = X_main_encoded)$predictions
main_sample$tau <- tau_hat


# look at quintiles

main_sample <- main_sample %>%
  mutate(
    sex = ifelse(sex == "M", 1, 0),
    
    meduc_1 = as.integer(meduc == 1),
    meduc_2 = as.integer(meduc == 2),
    meduc_3 = as.integer(meduc == 3),
    meduc_4 = as.integer(meduc == 4),
    meduc_5 = as.integer(meduc == 5),
    meduc_6 = as.integer(meduc == 6),
    
    mrace_1 = as.integer(mrace == 1),
    mrace_2 = as.integer(mrace == 2),
    mrace_3 = as.integer(mrace == 3),
    mrace_4 = as.integer(mrace == 4)
  )


# Q1 - smallest tau values
# Q5 - largest tau values
main_sample$cate_q <- cut(
  main_sample$tau,
  breaks = quantile(main_sample$tau,
                    probs = seq(0, 1, by = 0.2),
                    na.rm = TRUE),
  include.lowest = TRUE,
  labels = paste0("Q", 1:5)
)

# summary stats
vars_summary <- c(
  "mage",
  "sex",
  "wtgain",
  "bmi",
  
  "meduc_1",
  "meduc_2",
  "meduc_3",
  "meduc_4",
  "meduc_5",
  "meduc_6",
  
  "mrace_1",
  "mrace_2",
  "mrace_3",
  "mrace_4",
  
  "mar",
  "monpre",
  "nprevis",
  "dlivord"
)


summary_by_q <- main_sample %>%
  group_by(cate_q) %>%
  summarise(
    avg_cate = sprintf(
      "%.2f (%.2f)",
      mean(tau, na.rm = TRUE),
      sd(tau, na.rm = TRUE) / sqrt(n())
    ),
    
    across(
      all_of(vars_summary),
      ~ sprintf(
        "%.2f (%.2f)",
        mean(.x, na.rm = TRUE),
        sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      )
    ),
    
    
    n = as.character(n()),
    .groups = "drop"
    
  )



nice_names <- c(
  cate_q   = "CATE Quintile",
  avg_cate = "Average Estimated CATE",
  
  mage      = "Mother's Age",
  sex       = "Male",
  wtgain    = "Weight Gain",
  bmi       = "BMI",
  
  meduc_1   = "Education - up to 12th grade",
  meduc_2   = "Education - High School",
  meduc_3   = "Education - College without Degree",
  meduc_4   = "Education - Associate Degree",
  meduc_5   = "Education - Bachelor's Degree",
  meduc_6   = "Education - Master's/PhD",
  
  mrace_1   = "Race - White",
  mrace_2   = "Race - Black",
  mrace_3   = "Race - American Indian / Eskimo",
  mrace_4   = "Race - Asian / Pacific Islander",
  
  mar       = "Married",
  monpre    = "Month Prenatal Care Began",
  nprevis   = "Prenatal Visits",
  dlivord   = "Birth Order",
  n         = "N"
)
names(summary_by_q) <- nice_names[names(summary_by_q)]




table_out <- summary_by_q %>%
  pivot_longer(
    cols = -"CATE Quintile",
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = "CATE Quintile",
    values_from = "Value"
  )



tt(table_out) |>
  format_tt(digits = 2) |>
  save_tt("./simpler_methods/cate_quantiles.tex")


# 2. BLP 

# --------------------------------------------------
# BLP using grf
# --------------------------------------------------


A_aux <- aux_sample %>%
  transmute(
    mage,
    sex,
    wtgain,
    bmi,
    mar,
    monpre,
    nprevis,
    dlivord,
    meduc = relevel(factor(meduc), ref = "1"),
    mrace = relevel(factor(mrace), ref = "1")
  )

A_aux <- model.matrix(~ . , A_aux)[, -1] %>%
  as.data.frame()





blp_grf <- best_linear_projection(
  cf,
  A =  A_aux,
  target.sample = "overlap"
)


renaming <- read.csv2(
  "./code/summary_tables/renaming.csv",
  stringsAsFactors = FALSE
)


coef_map <- setNames(
  renaming$variable,
  gsub("\\.", "", renaming$column)
)
coef_map["sexM"] <- "Male Infant"


options("modelsummary_format_numeric_latex" = "plain")
modelsummary(
  list("GRF BLP" = blp_grf),
  coef_map = coef_map,
  statistic = "({std.error})",
  stars = TRUE,
  title = "Best Linear Projection of Treatment Effects",
  output = "./simpler_methods/blp_grf.tex"
)




