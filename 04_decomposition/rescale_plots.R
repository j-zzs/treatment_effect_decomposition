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


limits_quant <- switch(
  outcome,
  "birwt"  = c(-300,0),
  "bw_std" = c(-0.6,0),
  "apgar"  = c(-0.2,0.1),
  stop("Unknown outcome: ", outcome)
)

limits_total <- switch(
  outcome,
  "birwt"  = c(-80,80),
  "bw_std" = c(-0.15, 0.15),
  "apgar"  = c(-0.1, 0.1),
  stop("Unknown outcome: ", outcome)
)


limits_structure <- switch(
  outcome,
  "birwt"  = c(-90, 90),
  "bw_std" = c(-0.15, 0.15),
  "apgar"  = c(-0.1, 0.1),
  stop("Unknown outcome: ", outcome)
)


limits_composition <- switch(
  outcome,
  "birwt"  = c(-80,80),
  "bw_std" = c(-0.15, 0.15),
  "apgar"  = c(-0.1, 0.1),
  stop("Unknown outcome: ", outcome)
)



# run through all plots

# mothers age
var = "mage"

load(paste0(path, var, ".RData"))

plot_effects(res$total_effect,
             res$structure_effect,
             res$composition_effect,
             taus= c(1:99)/100,
             res$quantiles_1,
             res$quantiles_0,
             res$quantiles_counterfactual,
             res$groups,
             "Mothers Age",
             save_figure = TRUE,
             path = save_path,
             name = paste0("decomposition_",var,".png"),
             filter_low = 0.01,
             filter_high = 0.99,
             limits_quant = limits_quant,
             limits_total = limits_total,
             limits_structure = limits_structure,
             limits_composition = limits_composition)

rm(res)



# dlivord

# decomposition for parity !note new breaks!
var = "parity"
load(paste0(path, var, ".RData"))

plot_effects(res$total_effect,
             res$structure_effect,
             res$composition_effect,
             taus= c(1:99)/100,
             res$quantiles_1,
             res$quantiles_0,
             res$quantiles_counterfactual,
             c("nullipara", "primipara", "secundipara", "multipara"),
             "Parity",
             save_figure = TRUE,
             path = save_path,
             name = paste0("decomposition_",var,".png"),
             filter_low = 0.01,
             filter_high = 0.99,
             limits_quant = limits_quant,
             limits_total = limits_total,
             limits_structure = limits_structure,
             limits_composition = limits_composition)


rm(res)

# ## decomposition for sex
# var = "sex"
# load(paste0(path, var, ".RData"))
# 
# plot_effects(res$total_effect,
#              res$structure_effect,
#              res$composition_effect,
#              taus= c(1:99)/100,
#              res$quantiles_1,
#              res$quantiles_0,
#              res$quantiles_counterfactual,
#              c("male", "female"),
#              "Sex of Child",
#              save_figure = TRUE,
#              path = save_path,
#              name = paste0("decomposition_",var,".png"),
#              limits_quant = limits_quant,
#              limits_total = limits_total,
#              limits_structure = limits_structure,
#              limits_composition = limits_composition)
# 
# rm(res)

# decomposition for wtgain
var = "wtgain"
load(paste0(path, var, ".RData"))

plot_effects(res$total_effect,
             res$structure_effect,
             res$composition_effect,
             taus= c(1:99)/100,
             res$quantiles_1,
             res$quantiles_0,
             res$quantiles_counterfactual,
             res$groups,
             "Weight Gain",
             save_figure = TRUE,
             path = save_path,
             name = paste0("decomposition_",var,".png"),
             filter_low = 0.01,
             filter_high = 0.99,
             limits_quant = limits_quant,
             limits_total = limits_total,
             limits_structure = limits_structure,
             limits_composition = limits_composition)

rm(res)


# decomposition for wtgain recoded
var = "wtgain_recoded"
load(paste0(path, var, ".RData"))

plot_effects(res$total_effect,
             res$structure_effect,
             res$composition_effect,
             taus= c(1:99)/100,
             res$quantiles_1,
             res$quantiles_0,
             res$quantiles_counterfactual,
             c('below recommendation', 'as recommended', 'above recommendation'),
             "Weight Gain",
             save_figure = TRUE,
             path = save_path,
             name = paste0("decomposition",var,".png"),
             filter_low = 0.01,
             filter_high = 0.99,
             limits_quant = limits_quant,
             limits_total = limits_total,
             limits_structure = limits_structure,
             limits_composition = limits_composition)

rm(res)

# decomposition for bmi
var = "bmi"
load(paste0(path, var, ".RData"))

plot_effects(res$total_effect,
             res$structure_effect,
             res$composition_effect,
             taus= c(1:99)/100,
             res$quantiles_1,
             res$quantiles_0,
             res$quantiles_counterfactual,
             c("underweight", "Normal Weight", "Overweight", "Obese"),
             "BMI",
             save_figure = TRUE,
             path = save_path,
             name = paste0("decomposition_",var,".png"),
             filter_low = 0.01,
             filter_high = 0.99,
             limits_quant = limits_quant,
             limits_total = limits_total,
             limits_structure = limits_structure,
             limits_composition = limits_composition)

rm(res)



