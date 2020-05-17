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

########### Script to run Scenarios ###########

## Make sure working directory is set to MNCOVID19/R ##
# i.e. modify and run the following code, if necessary:
# setwd("<My Path>/MNCOVID19/R")


## Load necessary packages, source files, and load data ##
source("setup_model.R")


## Intervention scenarios ##
#   1 = Unmitigted spread; ICU capacity not expanded (ICU capacity = 235 beds)
#   2 = Social distancing from March 23rd to 27th (5 days); 
#        starting on March 28th stay-at-home executive order enacted for 14 days, 
#        then continued social distancing until May 1 (21 days);
#        ICU capacity expanded to 2,200 beds (defaut input value)
#   3 = Scenario 2 + continued social distancing for vulnerable populations until 30 days past peak deaths
#   4 = Extend Scenario 3 stay-at-home order from 14 days to 42 days; 
#       lifting of stay-at-home is followed by 21 days social distancing for all; 
#       vulnerable populations continue to social distance until 30 days past peak
#   5 = Extend Scenario 4 stay-at-home order from 42 days to 51 days (May 18th)
#   6 = Extend Scenario 4 stay-at-home order from 51 days to 65 days
#   99 = Stay-at-home for entire epidemic (for comparison purposes)

# Scenarios to be run
#scn_vec <- c("1", "2", "3", "4", "5", "6", "99")
scn_vec <- c("1", "2", "3", "4", "5", "6")

# Intializing lists to store the raw model output and processed model output
lst_out_raw <- list()
lst_out <- list()

# Initializing a matrix to store printed summary output from model
summary_out <- matrix(nrow = length(scn_vec), ncol = 8)
colnames(summary_out) <- c("strategy", 
                           "n_deaths", 
                           "n_deaths_may30",
                           "day_icu_cap_reached",
                           "max_icu_demand",
                           "Rt_est",
                           "day_peak_infections",
                           "additional_vulnerable_sd_days")

# Loop through each scenario and run the model
for (i in 1:length(scn_vec)) {
  
  # set parameters to their default base case values 
  parms <- parameters(n_icu_beds = )
  
  i_scenario <- scn_vec[i]
  
  if (i_scenario == "1") {
    # original number of ICU beds before any additional capacity was added
    parms$n_icu_beds <- 235
    
  } else if (i_scenario == "2") {
    parms$start_time_social_distancing <- 1 # March 23nd
    parms$end_time_social_distancing <- 1+5+14+21 # March 23rd to 27th, then 5 weeks more
    parms$start_time_sip <- 6 # March 27th
    parms$end_time_sip <- 6+14 # March 27th then 2 weeks more
    
  } else if (i_scenario == "3") {
    parms$start_time_social_distancing <- 1
    parms$end_time_social_distancing <- 1+5+14+21
    parms$start_time_sip <- 6
    parms$end_time_sip <- 6+14
    parms$start_time_60plus_distancing <- 1+5+14+21 # Vulnerable population defined by age 60+
    parms$end_time_60plus_distancing <- -Inf
    parms$sixty_plus_days_past_peak <- 30
    
  } else if (i_scenario == "4") {
    parms$start_time_social_distancing <- 1
    parms$end_time_social_distancing <- 1+5+42+21
    parms$start_time_sip <- 6
    parms$end_time_sip <- 6+42
    parms$start_time_60plus_distancing <- 1+5+42+21
    parms$end_time_60plus_distancing <- -Inf
    parms$sixty_plus_days_past_peak <- 30
    
  } else if (i_scenario == "5") {
    parms$start_time_social_distancing <- 1
    parms$end_time_social_distancing <- 1+5+51+21
    parms$start_time_sip <- 6
    parms$end_time_sip <- 6+51
    parms$start_time_60plus_distancing <- 1+5+51+21
    parms$end_time_60plus_distancing <- -Inf
    parms$sixty_plus_days_past_peak <- 30

  } else if (i_scenario == "6") {
    parms$start_time_social_distancing <- 1
    parms$end_time_social_distancing <- 1+5+65+21
    parms$start_time_sip <- 6
    parms$end_time_sip <- 6+65
    parms$start_time_60plus_distancing <- 1+5+65+21
    parms$end_time_60plus_distancing <- -Inf
    parms$sixty_plus_days_past_peak <- 30
        
  } else if (i_scenario == "99") {
    parms$start_time_social_distancing <- 1
    parms$end_time_social_distancing <- 1+5+365
    parms$start_time_sip <- 6
    parms$end_time_sip <- 6+365
    parms$start_time_60plus_distancing <- 1+5+365
    parms$end_time_60plus_distancing <- -Inf
    parms$sixty_plus_days_past_peak <- 30
    
  }
  
  ## NOTE: To estimate maximum ICU demand ##
  # UNCOMMENT line below to generate ICU demand estimates for all scenarios (under unconstrainted ICU capacity)
  #parms$n_icu_beds <- Inf
  
  # Specify time horizon of model output (in days); default = 365 (1 year)
  # Day 1 = March 22, 2020
  times <- seq(1, 365, by = parms$timestep)
  
  # Run model
  m_out_raw <- solve_model(parms$init_vec, 
                           times = times,
                           func = covid_19_model_function,
                           parms = parms)
  
  # Store the raw output from each strategy in a list
  lst_out_raw <- c(lst_out_raw, list(m_out_raw))
  
  # Process output matrix "out" to extract more data
  out <- process_output(m_out_raw,parms)
  lst_out <- c(lst_out,list(out))
  
  ## Store select summary outputs ##
  
  # deaths
  n_deaths <- round(out[nrow(out), "cumulative_deaths"], 0)
  pct_deaths <- round(100 * n_deaths / parms$N, 2)
  n_deaths_may30 <- round(out[70,"cumulative_deaths"],0) # day 70 is may 30
  
  # healthcare demand
  day_icu_cap_reached <- which(out[, "ICU_bed_demand"] >= parms$n_icu_beds)[1]
  max_icu_demand <- round(max(out[, "ICU_bed_demand"]), 0)
  
  # infections
  day_peak_infections <- which.max(out[, "prevalent_infections"])
  
  # Rt estimation (first 20 days)
  lm_Rt <- lm(log(out[1:20,"cumulative_infections"])~out[1:20,"Time"])
  avg_exp_dur <- parms$n_exposed_states/(parms$exposed_transition_rate/parms$timestep)
  avg_inf_dur <- parms$n_infected_states/(parms$infected_transition_rate/parms$timestep)
  Rt_est <- round((1+lm_Rt$coefficients[[2]]*avg_inf_dur)*(1+lm_Rt$coefficients[[2]]*avg_exp_dur),2)
  
  # Scenario features
  # number of additional days distancing for vulnerable (aged 60+)
  n_add_vulnerable_sd_days <- ifelse(parms$sixty_plus_days_past_peak >= 0,
                                     round(max(which(m_out_raw[, "sd60p"] == 1)) - 
                                             max(which(m_out_raw[, "sd"] == 1 | m_out_raw[, "sip"] == 1)),0),
                                     NA)
  
  # Print output summary
  print(paste0("Scenario: ", i_scenario))
  print(paste0("Overall deaths (rounded): ", 100 * round(n_deaths / 100, 0), " (", round(100 * n_deaths / parms$N, 2), "%)"))
  print(paste0("Number of deaths by end of May (rounded): ", 100 * round(n_deaths_may30 / 100, 0)))
  print(paste0("Week ICU capacity reached: ", round(day_icu_cap_reached / 7, 0)))
  if (is.infinite(parms$n_icu_beds)){
    print(paste0("Max ICU demand (unconstrained capacity): ", 100 * round(max_icu_demand / 100, 0)))
  }else{
    print(paste0("Max ICU demand (with ",parms$n_icu_beds," bed capacity): ", 100 * round(max_icu_demand / 100, 0)))
  }
  print(paste0("Rt (first 20 days): ", round(Rt_est,2)))
  print(paste0("Week of peak infections: ", round(day_peak_infections / 7, 0)))
  print(paste0("Additional weeks that vulnerable groups social distance: ",round(n_add_vulnerable_sd_days/7,0)))
  print("--------------------")
  
  # Store summary results in matrix
  summary_out[i,] <- c(i_scenario, n_deaths, n_deaths_may30, 
                       day_icu_cap_reached, max_icu_demand, 
                       Rt_est, day_peak_infections, 
                       n_add_vulnerable_sd_days)
}

names(lst_out) <- paste0("strat_", scn_vec)
names(lst_out_raw) <- paste0("strat_", scn_vec)

## Create data.frame for plotting
df_ls <- lapply(1:length(lst_out), FUN = function(x) {
  df <- as.data.frame(lst_out[[x]][, 1:6])
  df$t <- 1:nrow(lst_out[[x]])
  df$strategy <- scn_vec[x]
  return(df)
})
df_out <- do.call(rbind, df_ls)


## Function for basic plots of model outputs over time
plot_func <- function(var_name) {
  var <- sym(var_name)
  plot <- ggplot(df_out, aes(x = t / 7, y = !! var, color = strategy)) + geom_path() +
    ylab(var_name) + xlab("Time (weeks)") +
    ggtitle(paste0(var_name)) +
    theme(plot.title = element_text(hjust = 0.5))
  return(plot)
}


## Generate plots
plot_func("ICU_bed_demand")
plot_func("cumulative_deaths")
plot_func("prevalent_infections")
plot_func("cumulative_infections")
plot_func("daily_deaths")
plot_func("prevalent_hospitalizations")


## UNCOMMENT to print matrix of summary output measures
#print(summary_out)
