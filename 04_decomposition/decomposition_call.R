set.seed(5678)


# variable to allow for robustness check: non-smoker vs heavy smoker
# in case robustness == True - keep downsampling in mind, since more smokers are filtered out

# # standardized birth weight
# robustness = FALSE
# downsampling = 50
# nsim = 5
# nreg = 100
# save_path = "./decomposition/results/bw_std/"
# outcome = "bw_standardized"

# # apgar score
# robustness = FALSE
# downsampling = 70
# nsim = 5
# nreg = 50
# save_path = "./decomposition/results/apgar/"
# outcome = "apgar5"

# birth weight
robustness = FALSE
downsampling = 70
nsim = 5
nreg = 50
save_path = "./decomposition/results/birwt/"
outcome = "birwt"

# load functions
source("./decomposition/counterfactual_plotting.R")# load data loading function
source("./data_preparation/load_data.R")
source("./decomposition/effect_decomposition.R")


# load prepared + recoded data
data = combine_data(years = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))
setDF(data)


if(robustness){
  data <- data %>% 
    filter((cig_1 > 20 & cig_2 >0 & cig_3 >0 & tobacco ==1) | (tobacco ==0)) 
  # downsampling
  data_smoker = data %>% filter(tobacco ==1)
  data_nonsmoker = data %>% filter(tobacco ==0) %>% 
    slice(., sample(nrow(.),nrow(.)/80))
  data = rbind(data_smoker, data_nonsmoker)
  rm(data_smoker)
  rm(data_nonsmoker)
}



############ decomposition ################



# decomposition for mothers age
var = "mage"
res = effect_decomposition(data = data, outcome_var = outcome, split_var = var, type = "cont",
                           n_sim = nsim, n_groups = 5, quantiles = c(1:99)/100, alpha = 0.025,
                           nreg = nreg, downsample_ratio = downsampling, cores = 26,
                           var_exclude = c("eclamp", "reproductive_assistance", "fertility_drugs"))

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
             name = paste0("decomposition_full_",var,".png"),
             single = FALSE)

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
             single = TRUE)

save(res, file=paste0(save_path, var,".RData"))
rm(res)



# decomposition for parity !note new breaks!
var = "dlivord"
res = effect_decomposition(data = data %>% mutate(dlivord = dlivord - 1), outcome_var = outcome, split_var = var, type = "cont",
                           n_sim = nsim, n_groups = 3, breaks = c(0,0.5,1,2,8),
                           quantiles = c(1:99)/100, nreg = nreg, alpha = 0.025,
                           downsample_ratio = downsampling, cores = 26,
                           var_exclude = c("eclamp", "reproductive_assistance", "fertility_drugs",
                                           "priorterm", "preterm", "cesarean", "cesarean_n",
                                           "interval_last_livebirth", "interval_last_otherbirth"))

print(res$groups)

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
             name = paste0("decomposition_full_",'parity',".png"),
             single = FALSE)

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
             name = paste0("decomposition_",'parity',".png"),
             single = TRUE)

save(res, file=paste0(save_path,'parity',".RData"))
rm(res)




# decomposition for wtgain
var = "wtgain"
res = effect_decomposition(data = data, outcome_var = outcome, split_var = var, type = "cont",
                           n_sim = nsim, n_groups = 4, quantiles = c(1:99)/100, alpha = 0.025,
                           nreg = nreg, downsample_ratio = downsampling, cores = 26,
                           var_exclude = c("eclamp", "reproductive_assistance", "fertility_drugs"))

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
             name = paste0("decomposition_full_",var,".png"),
             single = FALSE)

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
             single = TRUE)

save(res, file=paste0(save_path, var,".RData"))
rm(res)



# decomposition for wtgain recoded
var = "wtgain"
res = effect_decomposition(data = data, outcome_var = outcome, split_var = var, type = "cont", name_recoded = "rec",
                           n_sim = nsim, n_groups = 4, quantiles = c(1:99)/100, alpha = 0.025,
                           nreg = nreg, downsample_ratio = downsampling, cores = 26,
                           var_exclude = c("eclamp", "reproductive_assistance", "fertility_drugs"))

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
             name = paste0("decomposition_full_recoded",var,".png"),
             single = FALSE)

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
             name = paste0("decomposition_recoded",var,".png"),
             single = TRUE)


save(res, file=paste0(save_path, var,"_recoded.RData"))
rm(res)


# decomposition for bmi
var = "bmi"
res = effect_decomposition(data = data, outcome_var = outcome, split_var = var, type = "cont", breaks = c(0,18.5,25,30,70),
                           n_sim = nsim, n_groups = 4, quantiles = c(1:99)/100, alpha = 0.025,
                           nreg = nreg, downsample_ratio = downsampling, cores = 26,
                           var_exclude = c("eclamp", "reproductive_assistance", "fertility_drugs"))

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
             name = paste0("decomposition_full_",var,".png"),
             single = FALSE)

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
             single = TRUE)

save(res, file=paste0(save_path, var,".RData"))
rm(res)