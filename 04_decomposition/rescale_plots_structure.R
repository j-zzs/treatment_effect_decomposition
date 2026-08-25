#################
# plot all plots on same scale
#################
# script reads in Rdata files saved from decompositions
# and plots all plots on same scale

set.seed(12345)

source("./decomposition/counterfactual_plotting.R")

outcome = "bw_std"
#outcome = "birwt"
#outcome = "apgar"

path <- file.path(
  "./decomposition/results",
  outcome,
  "/"
)

save_path <- file.path(
  path,
  "scaled"
  
)


limits_structure <- switch(
  outcome,
  "birwt"  = c(-90, 90),
  "bw_std" = c(-0.15, 0.15),
  "apgar"  = c(-0.1, 0.1),
  stop("Unknown outcome: ", outcome)
)


# run through all plots

# mothers age
var = "mage"

load(paste0(path, var, ".RData"))


plot_effects(
  structure_effect_df = res$structure_effect,
  taus = c(1:99) / 100,
  group_names = res$groups,
  plot_annotation = "Mothers Age",
  save_figure = TRUE,
  path = save_path,
  name = paste0("decomposition_", var, "_structural.png"),
  filter_low = 0.01,
  filter_high = 0.99,
  limits_structure = limits_structure,
  y_lab = "Difference in Treatment Effect (in grams)",
  plot = "structure" 
)

rm(res)



# dlivord

# decomposition for parity !note new breaks!
var = "parity"
load(paste0(path, var, ".RData"))


plot_effects(
  structure_effect_df = res$structure_effect,
  taus = c(1:99) / 100,
  group_names = c("nullipara", "primipara", "secundipara", "multipara"),
  plot_annotation = "Parity",
  save_figure = TRUE,
  path = save_path,
  name = paste0("decomposition_", var, "_structural.png"),
  filter_low = 0.01,
  filter_high = 0.99,
  limits_structure = limits_structure,
  y_lab = "Difference in Treatment Effect (in grams)",
  plot = "structure" 
)


rm(res)

# ## decomposition for sex
# var = "sex"
# load(paste0(path, var, ".RData"))
# 
# plot_effects(res$structure_effect,
#              taus= c(1:99)/100,
#              c("male", "female"),
#              "Sex of Child",
#              save_figure = TRUE,
#              path = save_path,
#              name = paste0("decomposition_",var,"_structural.png"),
#              limits_structure = limits_structure,
#              y_lab = "Difference in Treatment Effect (in grams)")
# 
# rm(res)

# decomposition for wtgain
var <- "wtgain"
load(paste0(path, var, ".RData"))

plot_effects(
  structure_effect_df = res$structure_effect,
  taus = c(1:99) / 100,
  group_names = res$groups,
  plot_annotation = "Weight Gain",
  save_figure = TRUE,
  path = save_path,
  name = paste0("decomposition_", var, "_structural.png"),
  filter_low = 0.01,
  filter_high = 0.99,
  limits_structure = limits_structure,
  y_lab = "Difference in Treatment Effect (in grams)",
  plot = "structure" 
)

rm(res)


# decomposition for wtgain recoded
var <- "wtgain_recoded"
load(paste0(path, var, ".RData"))

plot_effects(
  structure_effect_df = res$structure_effect,
  taus = c(1:99) / 100,
  group_names = c("below recommendation", "as recommended", "above recommendation"),
  plot_annotation = "Weight Gain",
  save_figure = TRUE,
  path = save_path,
  name = paste0("decomposition_", var, "_structural.png"),
  filter_low = 0.01,
  filter_high = 0.99,
  limits_structure = limits_structure,
  y_lab = "Difference in Treatment Effect (in grams)",
  plot = "structure" 
)

rm(res)


# decomposition for bmi
var <- "bmi"
load(paste0(path, var, ".RData"))

plot_effects(
  structure_effect_df = res$structure_effect,
  taus = c(1:99) / 100,
  group_names = c("Underweight", "Normal Weight", "Overweight", "Obese"),
  plot_annotation = "BMI",
  save_figure = TRUE,
  path = save_path,
  name = paste0("decomposition_", var, "_structural.png"),
  filter_low = 0.01,
  filter_high = 0.99,
  limits_structure = limits_structure,
  y_lab = "Difference in Treatment Effect (in grams)",
  plot = "structure" 
)

rm(res)

