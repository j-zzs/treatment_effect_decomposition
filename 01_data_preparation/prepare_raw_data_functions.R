library(data.table)
library(tidyverse)
library(readxl)

################### define functions ##################33

# filter for single births
filter_singletons <- function(df) {
  df <- subset(df, twins == 1)
  return(df)
}

# filter for relevant variables
filter_vars <- function(df, var) {
  df <- df[, var, with = FALSE]
  return(df)
}

# rename columns with new consistent column names
rename <- function(df, name_new){
  colnames(df) <- name_new
  return(df)
}

# relabel missings
relabel_na <- function(df,missing1,missing2){
  for (i in 1:ncol(df)){
    if (is.na(missing1[i]) == FALSE){
      df[[i]] <- ifelse((df[[i]] == missing1[i]), NA, df[[i]])
    } 
    if(is.na(missing2[i]) == FALSE){
      df[[i]] <- ifelse((df[[i]] == missing2[i]), NA, df[[i]])
    }
  }
  return(df)
}

filter_revision <- function(df){
  df <- subset(df, revision == "A")
  return(df)
}

# recode fathers race to adapt to drift
recode_race <- function(df){
  df$frace = ifelse(df$frace == 5, 4,
                    ifelse(df$frace ==6, 9, df$frace))
  return(df)
}

# works for all years with revised format (2011+)
data_prep_revised <- function(data, year, path= "./data_preparation/revised_variables.xlsx"){
  year = as.character(year)
  # read variable info sheet
  rel_var <- read_excel(path= path,
                        sheet = year)
  # only keep complete cases
  rel_var <- rel_var[!is.na(rel_var$colname),]
  # filter revision
  if (year< 2014){
    data = filter_revision(data)
  }
  # filter for relevant variables
  data = filter_vars(data, rel_var$colname)
  # rename columns
  data = rename(data, rel_var$colname_new)
  # filter singletons
  data = filter_singletons(data)
  # relabel missings
  data = relabel_na(data, rel_var$missings_1, rel_var$missings_2) 
  # omit NA
  data = na.omit(data)
  # relabel tobacco
  data$tobacco <- ifelse(data$tobacco == "N", 0, 1)
  # relabel smoking
  data$cig_0 <- ifelse(data$cig_0 == 99, NA, data$cig_0)
  data$cig_1 <- ifelse(data$cig_1 == 99, NA, data$cig_1)
  data$cig_2 <- ifelse(data$cig_2 == 99, NA, data$cig_2)
  data$cig_3 <- ifelse(data$cig_3 == 99, NA, data$cig_3)
  return(data)
}


############### automtated data prep ####################

data_preparation <- function(year, path_var, path_data, path_output){
  if  (year %in% seq(2011,2017,1)){
    print(">>>>>>>> read data and prepare >>>>>>>>>>")
    birth_data <- fread(paste0(path_data, "natl",year,".csv"), header = TRUE)
    birth_data = data_prep_revised(data = birth_data, year = year, path = path_var)
    print(">>>>> write to file >>>>>>>")
    write.csv2(birth_data, paste0(path_output,"birth_data_",year,".csv"), row.names = FALSE)
    rm(birth_data)
  } else if (year == 2018){
    print(">>>>>>>> read data and prepare >>>>>>>>>>")
    birth_data <- fread(paste0(path_data, "natl",year,"us.csv"), header = TRUE)
    birth_data = data_prep_revised(data = birth_data, year = year, path = path_var)
    print(">>>>> write to file >>>>>>>")
    write.csv2(birth_data, paste0(path_output,"birth_data_",year,".csv"), row.names = FALSE)
    rm(birth_data)
  }
}

