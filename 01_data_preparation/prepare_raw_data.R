#####################################
#
# file to clean raw data according to needs 
#
# removes unused information + recodes NAs
#
#################

source('./data_preparation/prepare_raw_data_functions.R')

#specify paths
path_var = "./data_preparation/revised_variables.xlsx"
path_data ="./raw_data/"
path_output = "./analysis_data/"



# function call
year = 2011
data_preparation(year = year, 
                 path_var = path_var, 
                 path_data = path_data, 
                 path_output = path_output)
gc()

year = 2012
data_preparation(year = year, 
                 path_var = path_var, 
                 path_data = path_data, 
                 path_output = path_output)

gc()


year = 2013
data_preparation(year = year, 
                 path_var = path_var, 
                 path_data = path_data, 
                 path_output = path_output)


gc()


year = 2014
data_preparation(year = year, 
                 path_var = path_var, 
                 path_data = path_data, 
                 path_output = path_output)

gc()

year = 2015
data_preparation(year = year, 
                 path_var = path_var, 
                 path_data = path_data, 
                 path_output = path_output)

gc()

year = 2016
data_preparation(year = year, 
                 path_var = path_var, 
                 path_data = path_data, 
                 path_output = path_output)

gc()

year = 2017
data_preparation(year = year, 
                 path_var = path_var, 
                 path_data = path_data, 
                 path_output = path_output)

gc()

year = 2018
data_preparation(year = year, 
                 path_var = path_var, 
                 path_data = path_data, 
                 path_output = path_output)




