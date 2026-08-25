## data loading functions
library(data.table)
# file includes several function for re-coding some variables and generating new ones
# load_data loads one data frame per year and cleans/adapts data directly
# combine_data combine frames form different years to one large frame 
PATH = "./analysis_data/"

### data cleaning
recode_hisp <- function(data) {
  # Recode Hispanic origin.
  # Combine Cuban, Puerto Rican, and other Central and South American countries into category 2.
  data$ormoth <- ifelse(data$ormoth %in% c(0, 1), 1,
                        ifelse(data$ormoth %in% c(2, 3, 4), 2,
                               ifelse(data$ormoth == 5, 5, 5)))
  data$orfath <- ifelse(data$orfath %in% c(0, 1), 1,
                        ifelse(data$orfath %in% c(2, 3, 4), 2,
                               ifelse(data$orfath == 5, 5, 5)))
  return(data)
}

recode_educ <- function(data) {
  # Recode education level.
  # Resulting levels:
  # 1 - less than HS
  # 2 - HS or GED
  # 3 - some college, no degree
  # 4 - associate degree
  # 5 - Bachelor
  # 6 - Master, PhD
  data$meduc = ifelse(data$meduc %in% c(1, 2), 1,
                             ifelse(data$meduc ==3, 2,
                                    ifelse(data$meduc == 4, 3,
                                           ifelse(data$meduc ==5, 4,
                                                  ifelse(data$meduc ==6, 5, 6)))))
  return(data)
}

recode_frace <- function(data) {
  # Recode fathers race.
  # Combine races Asian (only), NHOPI (only), More than one race into category 4 (Other).
  data$frace <- ifelse(data$frace %in% c(1, 2, 3), data$frace, 4)
  return(data)
}

recode_bmi <- function(data) {
  # Recode BMI: Replace commas with periods and convert to numeric
  data$bmi <- as.numeric(gsub(",", ".", data$bmi))
  return(data)
}


wtgain_recommended <- function(data) {
  # Determine weight gain recommendation based on BMI and weight gain.
  data$rec <- ifelse(data$bmi < 18.5, 
                     ifelse(data$wtgain < 28, -1, ifelse(data$wtgain < 40, 0, 1)),
                     ifelse(data$bmi < 25, 
                            ifelse(data$wtgain < 25, -1, ifelse(data$wtgain < 35, 0, 1)),
                            ifelse(data$bmi < 30, 
                                   ifelse(data$wtgain < 15, -1, ifelse(data$wtgain < 25, 0, 1)),
                                   ifelse(data$wtgain < 11, -1, ifelse(data$wtgain < 20, 0, 1)))))
  
  return(data)
}


# recode factors to "Y" and "U/N/X"
recode_factors = function(data){
  data$paternity = ifelse((data$paternity == "Y"), "Y", "U/N/X")
  data$fertility_drugs = ifelse((data$fertility_drugs == "Y"), "Y", "N/X")
  data$reproductive_assistance = ifelse((data$reproductive_assistance == "Y"), "Y", "N/X")
  return(data)
}


process_infections <- function(dt, infection_cols) {
  # Ensure dt is a data.table
  if (!data.table::is.data.table(dt)) {
    stop("Input must be a data.table.")
  }
  # Create the infection column
  dt[, infection := as.integer(rowSums(.SD == "Y") > 0), .SDcols = infection_cols]
  
  # Binarize the specified columns
  for (col in infection_cols) {
    dt[, (col) := as.integer(get(col) == "Y")]
  }
  return(dt)
}

load_data = function(year, path = PATH){
  # load data
  file_path <- paste0(path, "birth_data_", year, ".csv")
  data = fread(file_path)
  
  # clean data
  # discard foreign residents
  data = data %>% filter(restatus !=4)
  # recode bmi to numeric
  data = recode_bmi(data)
  # add wtgain recommendations
  data = wtgain_recommended(data)
  #recode hispanic origin
  data = recode_hisp(data)
  # recode education
  data = recode_educ(data)
  if (year >= 2017){
    data = recode_frace(data)
  }
  #recode factors
  data = recode_factors(data)
  # handle infecitons
  infection_columns <- c("gonorrhea", "syphilis", "chlamydia", "hepatitis_c", "hepatitis_b")
  data <- process_infections(data, infection_columns)

  return(data)

}



combine_data <- function(years = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) {
  # Initialize an empty data frame to store combined data
  data_combined <- data.frame()
  
  # Loop through each year in the input list of years
  for (year in years) {
    # Read in data for the current year
    data <- load_data(year)
    
    # Add a column indicating the year
    data$year <- year
    
    # Combine the current year's data with the existing combined data
    data_combined <- rbind(data_combined, data)
  }
  
  
  # add standardized bw
  ref_bw = data_combined %>%
    filter(tobacco == 0) %>% 
    filter(prepreg_diabetes == "N") %>% 
    filter(prepreg_hypertension == "N") %>% 
    filter(gest_diabetes == "N") %>% 
    filter(gest_hypertension == "N") %>% 
    filter(eclamp == "N") %>%
    filter(gonorrhea == 0) %>%
    filter(syphilis == 0) %>%
    filter(chlamydia == 0) %>%
    filter(hepatitis_b == 0) %>%
    filter(hepatitis_c == 0) %>%
    group_by(gestat, sex) %>% 
    summarize(bw_mean = mean(birwt), sd_bw = sd(birwt), count = n())
  
  data_combined = data_combined %>% 
    merge(ref_bw, by = c("gestat", "sex")) %>% 
    mutate(bw_standardized = (birwt - bw_mean)/sd_bw ) %>% 
    select(-c(bw_mean, sd_bw, count))
  
  # add lbw indicator
  data_combined = data_combined %>% mutate(LBW = ifelse(birwt < 2500, 1,0))
  
  # Return the combined data frame
  return(data_combined)
}




