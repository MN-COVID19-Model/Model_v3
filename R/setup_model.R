######################################################################
## Project: MN COVID-19 Model version 3.0
## Date: May 1, 2020
## Authors: Eva A. Enns, Shalini Kulasingam, and the UMN COVID-19 Modeling Team
##          University of Minnesota School of Public Health
## More information: https://www.sph.umn.edu/research/projects/covid-19-model-code/ 
##                   covid19model@umn.edu
## © 2020 Regents of the University of Minnesota. 
## This software is licensed via the GNU General Public License 3.0.
######################################################################


############ Model Setup Script ############

library(ggplot2)
library(plyr)
source("index_gen.R")
source("lambda.R")
source("covid_19_model_function_v3.R")
source("parameters_v3.R")
source("convert_parms.R")
source("process_output_v3.R")
source("solve_model.R")
source("get_initial_conditions.R")

load("../data/mixing_matrix_MN_sym.rda")
load("../data/2020_03_23_MN_Init_Cases.rda")
load("../data/MN_population_data.rda")

