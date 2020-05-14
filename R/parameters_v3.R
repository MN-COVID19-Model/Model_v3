#' @title Model parameter generation
#'
#' Parameter Generation for MN Covid-19 model
#'
#' input values of parameters or use defaults to generate list of parameters
#' that is used by main function.
#' 
#' @param age_groups vector which lists the starting age of each age group.
#' @param beta probability that contact with an infectious person will cause an infection.
#' @param n_infected_states number of infected tunnel states.
#' @param n_exposed_states number of exposed tunnel states.
#' @param n_days_incubation average number of days spent in the exposed states.
#' @param n_days_infectious average number of days spent in the infectious states.
#' @param exposed_transition_rate rate at which population moves through the exposed tunnel states. This value is NA by default,
#'  and if it is not supplied directly, the value is calculated internally in accordance with timestep, n_exposed_states, and n_days_incubation.
#' @param infected_transition_rate rate at which population moves through the exposed tunnel states. This value is NA by default,
#'  and if it is not supplied directly, the value is calculated internally in accordance with timestep, n_infected_states, and n_days_infectious.
#' @param n_days_rec_hosp average number of days it takes to recover if in the hospital.
#' @param n_days_rec_ICU average number of days it takes to recover if in the ICU (w/ bed).
#' @param init_cases_detected percentage of cases that are assumed to have been detected, as of day 0 in the model. 
#' Used in inital condition calculation.
#' @param n_icu_beds number of ICU beds available in Minnesota.
#' @param relative_risk_mort_co relative risk of mortality if a person has 1 or more comorbidities.
#' @param start_time_social_distancing day of simulation at which social distancing measures start.
#' @param start_time_sip day of simulation at which shelter in place starts.
#' @param start_time_60plus_distancing day of simulation at which targeted social distancing measures start for individuals age 60+.
#' @param start_time_behavior_change day of simulation at which population changes behavior independent of enacted policies to start socially distancing.
#' @param end_time_60plus_distancing day of simulation at which targeted social distancing measures end for individuals age 60+.
#' @param end_time_social_distancing day of simulation at which social distancing measures end.
#' @param end_time_sip day of simulation at which shelter in place ends.
#' @param end_time_behavior_change day of simulation at which population ceases policy-independent social distancing.
#' @param sip_contact_reduction as a decimal, the percentage reduction in contact rates when shelter in place is in effect.
#' @param social_distancing_contact_reduction as a decimal, the percentage reduction in contact rates when social distancing is in effect
#' This reduction only applies to relevant entries of the mixing matrix.
#' @param sixty_plus_contact_reduction as a decimal, the percentage reduction in contact rates when 60+ social distancing is in effect. 
#' This reduction only applies to relevant entries of the mixing matrix.
#' @param behavior_change_contact_reduction as a decimal, the percentage reduction in contact rates when policy-indepedent behavioral changes are in effect
#' @param sip_days_past_peak number of days shelter in place will be in effect past the chosen peak (infections, hospitalizations, deaths)
#' @param social_distancing_days_past_peak number of days social distancing will be in effect past the chosen peak (infections, hospitalizations, deaths)
#' @param sixty_plus_days_past_peak number of days 60+ social distancing will be in effect past the chosen peak (infections, hospitalizations, deaths)
#' @param behavior_change_days_past_peak number of days a long-lasting behavior change will be in effect past the chosen peak (infections, hospitalizations, deaths)
#' @param str_peak_type string indicating which peak type will be used in calculations
#' @param v_strat_status a vector that will be used in the solve_model() function which indicates which social distancing strategies are being used
#' @param prop_asymptomatic vector that contains the proportion of each age groups which is asymptomatic
#' @param p_h_80 probability of being hospitalized for individuals 80 years of age or older
#' @param p_dying_home_80 probability of dying at home for 70+ year old people who have a symptomatic infection
#' @param timestep length of timesteps in days used by the solver function, should be 1 or less, ideally it should be 0.05 or less to ensure 
#' accurate calculations 
#'
#' @return
#' list of parameters
#'
#'@export
#'
parameters <- function(init_cases_detected =  0.02143008,  # calibrated value 5/11/2020
                       n_icu_beds = 2200,
                       start_time_social_distancing = Inf,
                       start_time_sip = Inf,
                       start_time_60plus_distancing = Inf,
                       start_time_behavior_change = Inf,
                       end_time_60plus_distancing = -Inf,
                       end_time_social_distancing = -Inf,
                       end_time_sip = -Inf,
                       end_time_behavior_change = -Inf,
                       sip_contact_reduction = 0.5511887, # calibrated value 5/11/2020
                       social_distancing_contact_reduction = 0.3755247, # calibrated value 5/11/2020
                       sixty_plus_contact_reduction = 0.5,
                       behavior_change_contact_reduction = 0.1,
                       sip_days_past_peak = -Inf,
                       social_distancing_days_past_peak = -Inf,
                       sixty_plus_days_past_peak = -Inf,
                       behavior_change_days_past_peak = -Inf,
                       prop_asymptomatic = matrix(0.4116304, nrow = 2, ncol = 9), # calibrated value 5/11/2020
                       p_h_80 =  0.1032184, # calibrated value 5/11/2020
                       p_dying_home_80 = 0.1392806) # calibrated value 5/11/2020
{
  
  #initialize model parameters
  age_groups <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
  beta <- 0.0295
  n_infected_states <- 3
  n_exposed_states <- 2
  n_days_incubation <- 5.2
  n_days_infectious <- 7.8
  exposed_transition_rate <- NA
  infected_transition_rate <- NA
  n_days_rec_hosp <- 11
  n_days_rec_ICU <- 8
  str_peak_type <- c("deaths")
  v_strat_status <- c("sd" = 1, "sip" = 1,"sd60p" = 1, "bec" = 1)
  timestep <- 0.05 #note, time step must be 1 or less
  relative_risk_mort_co <- 1
  
  # proportion of symptomatic infections being hospitalized
  prop_hosp <- c(0.01837648,
                 0.07925855, 
                 0.19718760,
                 0.32917865,
                 0.43336529,
                 0.51230425,
                 0.60051135,
                 0.74768297,
                 1) * p_h_80
  
  # proportion of hospitalised cases requiring ventilation
  prop_ICU <- c(0.1304,
                0.1196,
                0.1351,
                0.1711,
                0.2219,
                0.2719,
                0.2962,
                0.2703,
                0.1877)
  
  ## MN demographic data ##
   
  # total population
  N = sum(population["pop"])
  
  ## proportion in each age group is calculated using demographic data from minnesota
  n_age_groups <- length(age_groups)
  N_by_age <- rep(0, n_age_groups)
  
  for (k in 1:(length(age_groups) - 1)) {
    N_by_age[k] <- sum(population[(age_groups[k]):(age_groups[k + 1] - 1), 2])
  }
  N_by_age[n_age_groups] <- sum(population[(age_groups[n_age_groups]):nrow(population), 2])
  
  # age distribution
  age_prop <- N_by_age / N

  ## Comorbidities
  comorbidity_groups <- c("c0", "c1")
  ncg <- length(comorbidity_groups)
  # prevalence of at least one comorbidity by age group
  # from MN All-Payer-Claims dataset, 2017
  comorbidity_prop_by_age <- c(1.2,2.3,3.8,6.9,12.5,21.4,30.1,43.6,55.9)/100
  
  ## Mixing matrix
  mixing_matrix <- mixing_matrix
  
  ## Exposed and Infected compartment dynamics
  # exposed compartments
  if (is.na(exposed_transition_rate)) {
     exposed_transition_rate <- n_exposed_states / n_days_incubation 
  }
  # infected compartments
  if (is.na(infected_transition_rate)) {
    infected_transition_rate <- n_infected_states / n_days_infectious
  }
  
  v_exp_str <- paste0("E", 1:n_exposed_states)
  v_inf_str <- paste0("I", 1:n_infected_states)
  v_asym_inf_str <- paste0("AI", 1:n_infected_states) # asymptomatic or mild infectious states
  epi_groups <- c("S", v_exp_str, v_asym_inf_str, v_inf_str, "H", "ICU", "R", "D", "HD", "CH")
  n_epi_groups<- length(epi_groups)
  
  
  ## Names for output objects
  nam <- c(outer(outer(comorbidity_groups, epi_groups, FUN = "paste"), age_groups , FUN = "paste"))
  
  
  ## Create indexing data frame
  df_ind <- expand.grid(ie = 1:n_epi_groups, # number of classes
                        ic = 0:(ncg - 1), # number of comorbidity groups
                        ia = 0:(n_age_groups - 1)) # number of age groups
  df_ind$ie_str <- mapvalues(x = df_ind$ie,
                             from = c(1:n_epi_groups),
                             to = c("S", v_exp_str, v_asym_inf_str, v_inf_str, "H", "ICU", "R", "D", "HD", "CH"))

  df_ind$index <- get_ind(eg = df_ind[, "ie"], cg = df_ind[, "ic"], ag = df_ind[, "ia"],
                          ncg = ncg, neg = n_epi_groups, nag = n_age_groups)
  
  # make parameter matrices for each parameter which is age or comorbidity dependent
  
  # n_days_rec_hosp is the number of days a person will spend in the hospital if admitted
  n_days_rec_hosp = matrix(n_days_rec_hosp, nrow = ncg, ncol = n_age_groups)
  
  # n_days_rec_ICU is the number of days someone will spend in the ICU if they have a bed
  n_days_rec_ICU = matrix(n_days_rec_ICU, nrow = ncg, ncol = n_age_groups)
  
  # icu_death_rate_1 is the rate at which people in the ICU die if they have a bed
  icu_death_rate_1 = matrix(0, nrow = ncg, ncol = n_age_groups)

  prob_icu_death <- c(0.0005, 
                      0.0424,	
                      0.1002,
                      0.1738,
                      0.2633,
                      0.3686,
                      0.4897,
                      0.6267,
                      0.7795)
  
  icu_death_rate_1[1,] <- -log(1-prob_icu_death)
  
  icu_death_rate_1[2,] <- icu_death_rate_1[1,]*relative_risk_mort_co
  prob_icu_death <- 1-exp(-1*icu_death_rate_1)
  
  # assume that people who need an icu bed but don't get one, will always die
  prob_icu_death_no_bed <- matrix(1,nrow = 2, ncol = 9 )
  
  # prop_hosp_die is the proportion of hospitalized people who are hospitalized 
  # for COVID-19 and die of the same cause
  prop_hosp_die <- matrix(0, nrow = 2, ncol = 9)
  h_death_rate <- matrix(0, nrow = 2, ncol = 9)
  
  prop_hosp_die[1,] <- c(2.86E-09,
                         2.76E-06,
                         0.0003,
                         0.0043,
                         0.0186,
                         0.0365,
                         0.0538,
                         0.0957,
                         0.2874)
  
  prop_hosp_die[2,] <- prop_hosp_die[1,]
  h_death_rate <- -log(1-prop_hosp_die)
  h_death_rate[2,] <- h_death_rate[1,]*relative_risk_mort_co
  prop_hosp_die <- 1-exp(-1*h_death_rate)

  # prop_inf_die is the proportion of people people who are symptomatic who do 
  # not get hospitalized but die
   prop_inf_die <- matrix(0, nrow = 2, ncol = 9)
   prop_inf_die[, 8] <- p_dying_home_80  # For now, in this setting, assume people aged 70 and up die at the same rates as people aged 80 and up
   prop_inf_die[, 9] <- p_dying_home_80
   prop_inf_die[2,8:9] <- -log(1-prop_inf_die[2,8:9])*relative_risk_mort_co
   prop_inf_die[2,8:9] <- 1 - exp(-1*prop_inf_die[2,8:9])
  
   # proportion of cumulative infections in age age group when infections reach 20,000 
   # used in initial conditions only
   prop_inf_by_age <- c(0.08930854, 
                        0.26353432,
                        0.15794502,
                        0.15717893,
                        0.14297132,
                        0.11012012,
                        0.04463342,
                        0.02264691,
                        0.01166143)
  
  #hash table for hashing indexes
  e = new.env()
  
  parms <- list("beta" = beta,
                "n_exposed_states" = n_exposed_states,
                "n_infected_states" = n_infected_states,
                "n_days_incubation" = n_days_incubation,
                "n_days_infectious" = n_days_infectious,
                "exposed_transition_rate" = exposed_transition_rate,
                "infected_transition_rate" = infected_transition_rate,
                "n_days_rec_ICU" = n_days_rec_ICU,
                "n_days_rec_hosp" = n_days_rec_hosp,
                "init_cases_detected"=init_cases_detected,
                "N" = N,
                "N_by_age" = N_by_age,
                "n_icu_beds" = n_icu_beds,
                "n_age_groups" = n_age_groups,
                "n_epi_groups" = n_epi_groups,
                "n_co_groups" = ncg,
                "prop_hosp" = prop_hosp,
                "prop_ICU" = prop_ICU,
                "start_time_social_distancing" = start_time_social_distancing,
                "start_time_sip" = start_time_sip,
                "start_time_60plus_distancing"=start_time_60plus_distancing,
                "start_time_behavior_change" = start_time_behavior_change,
                "end_time_60plus_distancing"=end_time_60plus_distancing,
                "end_time_social_distancing" = end_time_social_distancing,
                "end_time_sip" = end_time_sip,
                "end_time_behavior_change" = end_time_behavior_change,
                "sixty_plus_contact_reduction" = sixty_plus_contact_reduction,
                "sip_days_past_peak" = sip_days_past_peak,
                "social_distancing_days_past_peak" = social_distancing_days_past_peak,
                "sixty_plus_days_past_peak" = sixty_plus_days_past_peak,
                "behavior_change_days_past_peak" = behavior_change_days_past_peak,
                "str_peak_type" = str_peak_type,
                "age_groups" = age_groups,
                "mixing_matrix" = mixing_matrix,
                "timestep" = timestep,
                "sip_contact_reduction" = sip_contact_reduction,
                "social_distancing_contact_reduction" = social_distancing_contact_reduction,
                "sixty_plus_contact_reduction" = sixty_plus_contact_reduction,
                "behavior_change_contact_reduction" = behavior_change_contact_reduction,
                "age_prop" = age_prop,
                "comorbidity_prop_by_age" = comorbidity_prop_by_age,
                "v_strat_status" = v_strat_status,
                "df_ind" = df_ind,
                "v_exp_str" = v_exp_str,
                "v_inf_str" = v_inf_str,
                "v_asym_inf_str" = v_asym_inf_str,
                "prop_asymptomatic" = prop_asymptomatic,
                "prop_inf_die" = prop_inf_die,
                "prop_inf_by_age" = prop_inf_by_age,
                "prop_hosp_die" = prop_hosp_die,
                "prob_icu_death" = prob_icu_death,
                "prob_icu_death_no_bed" = prob_icu_death_no_bed,
                "hash_table" = e)

  
  init_vec <- get_initial_conditions(parms, m_init_cases)
  names(init_vec) <- nam
  
  parms$init_vec <- init_vec
  
  parms <- convert_parms(parms)
  
  return(parms)
  
}
