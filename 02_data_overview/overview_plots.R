set.seed(12345)


# load data loading function
source("./data_preparation/load_data.R")
library(tidyverse)

savepath = "./data_overview/output/"

data_combined = combine_data(years = c(2011,2012,2013,2014,2015,2016,2017,2018))
setDF(data_combined)

# get group means
mu_bwstd = data_combined%>% 
  mutate(tobacco = as.factor(tobacco)) %>%  
  group_by(tobacco) %>% 
  summarize(grp.mean = mean(birwt))



# get group means
mu_bw = data_combined %>% 
  mutate(tobacco = as.factor(tobacco)) %>%  
  group_by(tobacco) %>% 
  summarize(grp.mean = mean(birwt))



# get group means
mu_std = data_combined %>%
  mutate(tobacco = as.factor(tobacco)) %>%
  group_by(tobacco) %>%
  summarize(grp.mean = mean(bw_standardized))


################ birthweight standardized


#density plot by smoking status
data_combined %>% 
  filter(bw_standardized < 7) %>% 
  mutate(tobacco = as.factor(tobacco)) %>% 
  ggplot(aes(x=bw_standardized, color = tobacco)) +
  geom_density(adjust = 3, n = 100, linewidth = 1.05) +
  geom_vline(data = mu_std, aes(xintercept=grp.mean, color= tobacco),
             linetype="dashed", linewidth = 1.05)+
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Standardized Birth Weight", y= "Density", color = "Smoking Status") +
  scale_color_manual(values=c("black", "red"), labels = c("Non-Smoker", "Smoker"))+
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),         # Increase axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Increase axis title size
    legend.title = element_text(size = 12),     # Increase legend title size
    legend.text = element_text(size = 10),      # Increase legend text size
    legend.box.spacing = unit(0.2, "cm"),       # Adjust spacing around legend
    legend.key.size = unit(1.5, "lines"),       # Adjust size of legend color key
    plot.title = element_text(size = 16, face = "bold")  # Increase plot title size
  )

ggsave("density_standardized_bw_smoking_status_new.png",
       plot = last_plot(),
       device = "png",
       path = savepath,
       scale = 1,
       width = 24,
       height = 12,
       units = "cm",
       dpi = 300,
       limitsize = TRUE
)


################# apgar score

# barplot with relative occurance
data_combined %>% 
  mutate(tobacco = as.factor(tobacco)) %>% 
  ggplot(aes(x=apgar5, color = tobacco)) +
  geom_bar(aes(y = ..prop..),fill = "white",position="dodge") +
  scale_color_manual(values=c("black", "red"), labels = c("Non-Smoker", "Smoker"))+
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "5-minute Apgar Score", y= "Relative Frequency", color ="Smoking Status") +
  theme_minimal()

ggsave("apgar_bar_density.png",
       plot = last_plot(),
       device = "png",
       path = savepath,
       scale = 1,
       width = 24,
       height = 12,
       units = "cm",
       dpi = 300,
       limitsize = TRUE
)


################### birth weight


# density plot by smoking status
data_combined %>% 
  filter(birwt < 5500) %>% 
  mutate(tobacco = as.factor(tobacco)) %>% 
  ggplot(aes(x=birwt, color = tobacco)) +
  geom_density(adjust = 1, n = 100, linewidth = 1.025) +
  geom_vline(data = mu_bw, aes(xintercept=grp.mean, color= tobacco),
             linetype="dashed", linewidth = 1.025)+
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Birth Weight (in grams)", y= "Density", color = "Smoking Status") +
  scale_color_manual(values=c("black", "red"), labels = c("Non-Smoker", "Smoker"))+
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),         # Increase axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Increase axis title size
    legend.title = element_text(size = 12),     # Increase legend title size
    legend.text = element_text(size = 10),      # Increase legend text size
    legend.box.spacing = unit(0.2, "cm"),       # Adjust spacing around legend
    legend.key.size = unit(1.5, "lines"),       # Adjust size of legend color key
    plot.title = element_text(size = 16, face = "bold")  # Increase plot title size
  )

ggsave("density_smoking_status_new.png",
       plot = last_plot(),
       device = "png",
       path = savepath,
       scale = 1,
       width = 24,
       height = 12,
       units = "cm",
       dpi = 300,
       limitsize = TRUE
)
