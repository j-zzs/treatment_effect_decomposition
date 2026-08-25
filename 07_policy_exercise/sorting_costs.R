# load data loading function


source("./data_preparation/load_data.R")

library(grf)
library(xtable)
library(tidyverse)
library("readxl")
library(ggpubr)

set.seed(1000)


path = "./policy_exercise/output/"

# load prepared + recoded data
data_combined = combine_data(years = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) 
setDF(data_combined)

########## effect estimation #############


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

data_combined[,cat_features] = lapply(data_combined[,cat_features], factor)
data_train = data_combined %>% slice(., sample(nrow(.),nrow(.)/50))


# on full training sample train cf
W_train <- data_train$tobacco
Y_train <- data_train$birwt
X_train <- data_train %>% select(sex, wtgain, mage, meduc, mrace,                   
                                 ormoth, mar, paternity, fage, feduc, frace, 
                                 orfath, monpre, nprevis, priorterm, dlivord,                 
                                 wic, prepreg_diabetes, 
                                 gest_diabetes, prepreg_hypertension, gest_hypertension,       
                                 preterm, infertility_treatment, fertility_drugs,         
                                 reproductive_assistance, cesarean, cesarean_n,              
                                 chlamydia,             
                                 payment, bmi, interval_last_livebirth, 
                                 interval_last_otherbirth, mothers_height, birmon,                  
                                 birthplace)

rm(data_train)
gc()

# One Hot encoding of X_train
X_train_encoded <- model.matrix(~ 0 + ., X_train)
X_train_encoded = data.frame(X_train_encoded)

# train causal forest
cf <- causal_forest(X_train_encoded, Y_train, W_train, honesty = TRUE)

rm(X_train_encoded)
rm(X_train)
rm(Y_train)
rm(W_train)
gc()

# filter for smokers only
smokers = data_combined %>% filter(tobacco ==1)
rm(data_combined)
gc()


newdata = smokers %>% select(sex, wtgain, mage, meduc, mrace,                   
                             ormoth, mar, paternity, fage, feduc, frace, 
                             orfath, monpre, nprevis, priorterm, dlivord,                 
                             wic, prepreg_diabetes, 
                             gest_diabetes, prepreg_hypertension, gest_hypertension,       
                             preterm, infertility_treatment, fertility_drugs,         
                             reproductive_assistance, cesarean, cesarean_n,              
                             chlamydia,             
                             payment, bmi, interval_last_livebirth, 
                             interval_last_otherbirth, mothers_height, birmon,                  
                             birthplace)
newdata <- model.matrix(~ 0 + ., newdata)
newdata = data.frame(newdata)

# predict cf on smokers only
tau.hat <- predict(cf, newdata = newdata)
smokers$tau = tau.hat$predictions

rm(newdata)
gc()

#### filter for smokers only
data_smokers = smokers %>% 
  mutate(lbw = ifelse(birwt <2500, 1, 0)) %>% 
  mutate(birwt_wo_smoking = birwt - tau) %>% 
  mutate(lbw_wo_smoking = ifelse(birwt_wo_smoking <2500, 1, 0)) 


############# cost analysis ##################

##### costs of LBW (Almond 2005)
cost_data = read_excel("./policy_exercise/costs_LBW_Almond.xlsx")

### add cut point info to smoking data
data_smokers = data_smokers %>% 
  mutate(bw_cut_point = ifelse(birwt < 800, 800,
                               ifelse(birwt < 1000, 1000,
                                      ifelse(birwt <1500, 1500,
                                             ifelse(birwt <2000, 2000,
                                                    ifelse(birwt <2500, 2500,
                                                           ifelse(birwt <3000, 3000, 5000)))))))


# join costs info
data_smokers = data_smokers %>% left_join(cost_data, by = c("bw_cut_point" = "weight"))


# calculate costs saved (tau* costs)
data_smokers = data_smokers %>% mutate(costs_saved_pooled = tau * pooled,
                                       costs_saved_fe = tau * fe)


########## sorting vs. random #################

# informed sorting
data_smoker_cut = data_smokers %>% 
  arrange(desc(mage), desc(dtotord), wtgain, bmi) %>% 
  slice_head(n = 10000)


data_smoker_cut %>% 
  summarise(saved_sum_pooled = sum(costs_saved_pooled, na.rm = TRUE), saved_sum_fe = sum(costs_saved_fe, na.rm = TRUE))

data_smokers %>% group_by(bw_cut_point) %>% count()


### histogram both
smoker_plot = data_smoker_cut %>% mutate(birwt_wo_smoking = birwt - tau) %>% 
  select(birwt, birwt_wo_smoking) %>% 
  gather() %>% 
  ggplot(aes(x=value, fill = key)) + 
  geom_histogram(alpha = 0.6, position = 'identity', color = "black")+
  scale_fill_grey(labels = c("Birth Weight Smoking", "Successful Smoking Cessation")) +
  xlim(0,5000) + 
  ylim(0, 1550) +
  theme_minimal() +
  geom_vline(aes(xintercept = 2500), color = "red", lty = 2) +
  xlab("Birth Weight (in grams)") +
  labs(fill="") +
  ggtitle("10,000 Smoking Pregnant Women selected based on main modifying factors")

smoker_plot

# 
# # histograms of bw beofre and after targetting
# data_smoker_cut %>% 
#   ggplot(aes(x=birwt)) + 
#   geom_histogram()+
#   xlim(0,5000) + 
#   ylim(0, 1500) +
#   theme_minimal() +
#   geom_vline(aes(xintercept = 2500), color = "red", lty = 2)
# 
# data_smoker_cut %>% 
#   mutate(birwt_wo_smoking = birwt - tau) %>% 
#   ggplot(aes(x=birwt_wo_smoking)) + 
#   geom_histogram() +
#   xlim(0,5000) + 
#   ylim(0, 1500) +
#   theme_minimal() +
#   geom_vline(aes(xintercept = 2500), color = "red", lty = 2)
# 
# 
# 
# data_smokers %>% 
#   arrange(desc(mage), desc(dtotord), wtgain, bmi) %>% 
#   slice_head(n = 10000) %>% 
#   group_by(lbw, lbw_wo_smoking) %>% 
#   count()


# random selection
set.seed(1000)
data_random = data_smokers %>% 
  sample_n(10000)

data_random %>% 
  summarise(saved_sum_pooled = sum(costs_saved_pooled, na.rm = TRUE), saved_sum_fe = sum(costs_saved_fe, na.rm = TRUE))


plot_random = data_random %>% 
  select(birwt, birwt_wo_smoking) %>% 
  gather() %>% 
  ggplot(aes(x=value, fill = key)) + 
  geom_histogram(alpha = 0.6, position = 'identity', color = "black")+
  scale_fill_grey(labels = c("Birth Weight Smoking", "Successful Smoking Cessation")) +
  xlim(0,5000) + 
  ylim(0, 1550) +
  theme_minimal() +
  geom_vline(aes(xintercept = 2500), color = "red", lty = 2) +
  xlab("Birth Weight (in grams)") +
  labs(fill="") +
  ggtitle("10,000 Smoking Pregnant Women selected at random")

plot_random


# 
# data_random %>% 
#   ggplot(aes(x=birwt)) + 
#   geom_histogram() +
#   xlim(0,5000) + 
#   ylim(0, 1500) +
#   theme_minimal() +
#   geom_vline(aes(xintercept = 2500), color = "red", lty = 2)
# 
# data_random %>% 
#   #mutate(birwt_wo_smoking = birwt - tau) %>% 
#   ggplot(aes(x=birwt_wo_smoking)) + 
#   geom_histogram() +
#   xlim(0,5000) + 
#   theme_minimal() +
#   geom_vline(aes(xintercept = 2500), color = "red", lty = 2)
# 
# data_random %>% 
#   group_by(lbw, lbw_wo_smoking) %>%
#   count()
# 



######### plot effect of sorting vs random #######
plot <- ggarrange(smoker_plot, plot_random,
                  ncol = 2, nrow = 1, 
                  common.legend = TRUE, legend="bottom")

annotate_figure(plot, top = text_grob("Effect of Smoking Cessation Programs", face = "bold", size = 14))

ggsave(paste0(path, "sorting_effect.png"), width = 16, height = 8.5)


############### cost saved OV ##################
# Assuming data_smoker_cut and data_random are your data frames
sum_smoker_cut <- data_smoker_cut %>% summarise(saved_sum_pooled = sum(costs_saved_pooled, na.rm = TRUE))
sum_random <- data_random %>% summarise(saved_sum_pooled = sum(costs_saved_pooled, na.rm = TRUE))

# Combine the summaries into a single data frame
summary_df <- data.frame(
  Group = c("Smoker Cut", "Random"),
  Total_Costs_Saved_Pooled = c(sum_smoker_cut$saved_sum_pooled, sum_random$saved_sum_pooled)
)

# Create the xtable object
xtable_summary <- xtable(summary_df, caption = "Overview of Costs Saved (Pooled)", label = "tab:costs_saved_overview")

# Print the xtable object to the console
print(xtable_summary, 
      type = "latex", 
      include.rownames = FALSE, 
      file="./policy_exercise/output/costs_saved.tex",
      hline.after = NULL,
      NA.string="-")


##### LBW numbers overview ########
data_random_summary  = data_random %>% 
  group_by(lbw, lbw_wo_smoking) %>%
  count() %>%
  rename(count_random = n)

data_smoker_cut_summary  = data_smoker_cut %>% 
  group_by(lbw, lbw_wo_smoking) %>%
  count() %>%
  rename(count_smoker_cut  = n)

# Combine the two data frames
summary_ov_data <- full_join(data_random_summary, data_smoker_cut_summary, 
                             by = c("lbw", "lbw_wo_smoking"))

# Replace NA with 0 in count columns
summary_ov_data[is.na(summary_ov_data)] <- 0

# Export to LaTeX using xtable
latex_table <- xtable(summary_ov_data, caption = "Comparison of Data Random and Smoker Cut",
                      label = "tab:comparison")

print(latex_table, type = "latex", include.rownames = FALSE,
      file="./policy_exercise/output/lbw_ov.tex")

