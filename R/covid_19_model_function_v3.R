#' @title Model Function for MN covid-19 model
#'
#' function contains a list of discrete differential equations
#' uses given parameter and timestep to calculate difference in each timestep at specified time
#'
#' states are
#' S-sucsceptible
#' E-pre-infectious, E1-Em
#' AI- asymptomatic infectious or mild infection, AI1-AIn
#' I-infectious, I1-In
#' H- person in hospital
#' ICU-person in ICU
#' R-recovered
#' D-deaths
#'
#' @param t current time
#' @param init_pop current state of model, i.e. number of people in each state, comorbidity, and age group
#' @param parms list of parameters
#' @param v_days_since_peak vector of the number of days since the last peak in infections, hospitalizations, and deaths
#'
#' @return vector of the difference in each model state at specified time step
#'
#' @export
covid_19_model_function <- function(t, init_pop, parms, v_days_since_peak) {
  
  # Initialize all parameters
  beta <- parms$beta
  n_days_rec_ICU <- parms$n_days_rec_ICU
  N <- parms$N
  nag <- parms$n_age_groups
  neg <- parms$n_epi_groups
  ncg <- parms$n_co_groups
  nes <- parms$n_exposed_states
  nis <- parms$n_infected_states
  stsd <- parms$start_time_social_distancing
  stsip <- parms$start_time_sip
  st60p <- parms$start_time_60plus_distancing
  stbec <- parms$start_time_behavior_change
  et60p <- parms$end_time_60plus_distancing
  etsd <- parms$end_time_social_distancing
  etsip <- parms$end_time_sip
  etbec <- parms$end_time_behavior_change
  et_peak_sd <- parms$social_distancing_days_past_peak
  et_peak_sip <- parms$sip_days_past_peak
  et_peak_60p <- parms$sixty_plus_days_past_peak
  et_peak_bec <- parms$behavior_change_days_past_peak
  str_peak_type <- parms$str_peak_type
  prop_hosp <- parms$prop_hosp
  prop_ICU <- parms$prop_ICU
  n_days_rec_hosp <- parms$n_days_rec_hosp
  mixing_matrix <- parms$mixing_matrix
  time <- t
  df_ind <- parms$df_ind
  v_exp_str <- parms$v_exp_str
  v_inf_str <- parms$v_inf_str
  v_asym_inf_str <- parms$v_asym_inf_str
  v_strat_status <- parms$v_strat_status
  prop_asym <- parms$prop_asymptomatic
  prop_inf_die <- parms$prop_inf_die
  prob_icu_death <- parms$prob_icu_death
  prob_hosp_death <-parms$prop_hosp_die
  prob_icu_death_no_bed <- parms$prob_icu_death_no_bed
  
  ## Check for ICU over-capacity
  
  # Calculate total number of people in ICU
  v_ind_icu <- parms$hash_table[["icu_index"]]
  if (is.null(v_ind_icu)) {
    v_ind_icu <- df_ind[df_ind$ie_str == "ICU", "index"]
    parms$hash_table[["icu_index"]] <- v_ind_icu
  }
  n_icu_t <- sum(init_pop[v_ind_icu])
  if (is.na(n_icu_t)) {
    
    print(init_pop[v_ind_icu])
    stop(paste0("ICU numbers are negative!!: time= ", time))
  }
  
  # Calculate the proportion of people who need an ICU but cannot access one
  p_icu_overflow <- 0
  if (n_icu_t > parms$n_icu_beds) {
    p_icu_overflow <- (n_icu_t - parms$n_icu_beds) / n_icu_t
  }
#  p_icu_overflow <- 0
  ## Calculate prob of transitioning through exposed and infected states
  p_trans_exp <- 1 - exp(-1 * parms$exposed_transition_rate)
  p_trans_inf <- 1 - exp(-1 * parms$infected_transition_rate)
  
  ## Calculate probability of recovering from hospital
  p_h_exit <- 1 - exp(- 1 / parms$n_days_rec_hosp)

  # Calculate probability of exiting ICU and proportion of exits that are deaths
   p_icu_exit <- 1 - exp(- 1 / parms$n_days_rec_ICU)
  # if someone needs an icu bed but one is not available, they spend a fixed time, 1 day, in the icu compartment
   p_icu_exit_nobed <- 1 - exp( - parms$timestep )

  ## Calculate number of contacts per day by age group based on current social distancing practices
  
  ##using current time, check if social distancing practices are in place
  if (!(str_peak_type %in% c("deaths", "hospitalizations", "infections"))) {
    stop("Error in COVID model function: invalid peak stype specified for lifting restrctions decision.")
  }
  
  if ((time >= stsd) && (time < etsd | et_peak_sd > v_days_since_peak[str_peak_type]) && (v_strat_status["sd"] == 1)) {
    social_distancing_op <- 1
  } else {
    social_distancing_op <- 0
  }
  
  if ((time >= stsip) && (time < etsip | et_peak_sip > v_days_since_peak[str_peak_type]) && (v_strat_status["sip"] == 1)) {
    sip_op <- 1
  } else{
    sip_op <- 0
  }
  
  if ((time >= st60p) && (time < et60p | time < (st60p + et_peak_60p) | v_days_since_peak[str_peak_type] < et_peak_60p) && (v_strat_status["sd60p"] == 1)) {
    social_distancing_60_op <- 1
  } else {
    social_distancing_60_op <- 0
  }
  
  if ((time >= stbec) && (time < etbec | time < (stbec + et_peak_bec) | v_days_since_peak[str_peak_type] < et_peak_bec) && (v_strat_status["bec"] == 1)) {
    behavior_change_op <- 1
  } else {
    behavior_change_op <- 0
  }
  
  ## Adjust contact rate of each age group based on what social distancing practices are in place
  if (sip_op) {
    #for now assume in reduces contacts by 80%
    mixing_matrix <- mixing_matrix * (1 - parms$sip_contact_reduction)
  }
  else if (social_distancing_op) {
    #for now assume social distances reduces contacts by fifty percent
    mixing_matrix <- mixing_matrix * (1 - parms$social_distancing_contact_reduction)
  } else {
    
    if (social_distancing_60_op) {
      # reduces interactions between 60+ individuals and all other groups
      
      mixing_matrix[7,] <- mixing_matrix[7,] * (1 - parms$sixty_plus_contact_reduction)
      mixing_matrix[8,] <- mixing_matrix[8,] * (1 - parms$sixty_plus_contact_reduction)
      mixing_matrix[9,] <- mixing_matrix[9,] * (1 - parms$sixty_plus_contact_reduction)
      
      mixing_matrix[1:6, 7] <- mixing_matrix[1:6, 7] * (1 - parms$sixty_plus_contact_reduction)
      mixing_matrix[1:6, 8] <- mixing_matrix[1:6, 8] * (1 - parms$sixty_plus_contact_reduction)
      mixing_matrix[1:6, 9] <- mixing_matrix[1:6, 9] * (1 - parms$sixty_plus_contact_reduction)
    }
    
    if (behavior_change_op) {
      # lasting overall reduction in population contacts
      if (social_distancing_60_op == 0) {
        mixing_matrix <- mixing_matrix * (1 - parms$behavior_change_contact_reduction)
      }
      
      if (social_distancing_60_op == 1) {
        mixing_matrix[1:6, 1:6] <- mixing_matrix[1:6, 1:6] * (1 - parms$behavior_change_contact_reduction)
      }
      
    }
  }
  
  # Call calculate_lambda() to get force of infection for current time step
  lambda <- calculate_lambda(mixing_matrix, init_pop, parms)
  
  ##initilize vector of differences##
  d <- rep(0, length(parms$init_vec))
  
  ##define transition rates with equations##
  # foreach(agg=0:(nag-1)) %dopar% {
  for (agg in 0:(nag - 1)) {
    for (cag in 0:(ncg - 1)) {
      ind_key <- paste0(agg, cag)
      v_ind <- parms$hash_table[[ind_key]]
      if (is.null(v_ind)) {
        v_S_ind <- df_ind[df_ind$ie_str == "S" & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_E_ind <- df_ind[df_ind$ie_str %in% v_exp_str & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_AI_ind <- df_ind[df_ind$ie_str %in% v_asym_inf_str & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_I_ind <- df_ind[df_ind$ie_str %in% v_inf_str & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_H_ind <- df_ind[df_ind$ie_str == "H" & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_ICU_ind <- df_ind[df_ind$ie_str == "ICU" & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_R_ind <- df_ind[df_ind$ie_str == "R" & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_D_ind <- df_ind[df_ind$ie_str == "D" & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_HD_ind <- df_ind[df_ind$ie_str == "HD" & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_CH_ind <- df_ind[df_ind$ie_str == "CH" & df_ind$ia == agg & df_ind$ic == cag, "index"]
        v_ind <- list("v_S_ind" = v_S_ind,
                      "v_E_ind" = v_E_ind,
                      "v_AI_ind" = v_AI_ind,
                      "v_I_ind" = v_I_ind,
                      "v_H_ind" = v_H_ind,
                      "v_ICU_ind" = v_ICU_ind,
                      "v_R_ind" = v_R_ind,
                      "v_D_ind" = v_D_ind,
                      "v_HD_ind" = v_HD_ind,
                      "v_CH_ind" = v_CH_ind)
        parms$hash_table[[ind_key]] <- v_ind
      }
      
      ## dS
      v_S_ind <- v_ind$v_S_ind
      d[v_S_ind] <- -beta * lambda[agg + 1] * init_pop[v_S_ind]
      
      ## dE1...dEm
      v_E_ind <- v_ind$v_E_ind
      # E1
      d[v_E_ind[1]] <- beta * lambda[agg + 1] * init_pop[v_S_ind] - p_trans_exp * init_pop[v_E_ind[1]]
      # E2,...Em
      if (nes > 1) {
        d[v_E_ind[2:nes]] <- 
          p_trans_exp * init_pop[v_E_ind[1:(nes - 1)]] - # incoming E_(i-1) --> E_i
          p_trans_exp * init_pop[v_E_ind[2:nes]] # exiting E_i --> E_(i+1)
      }
      
      ## dAI1...dAIn
      v_AI_ind <- v_ind$v_AI_ind
      # AI1
      x <- prop_asym[cag + 1, agg + 1] * p_trans_exp * init_pop[v_E_ind[nes]] - p_trans_inf * init_pop[v_AI_ind[1]]
      d[v_AI_ind[1]] <- prop_asym[cag + 1, agg + 1] * p_trans_exp * init_pop[v_E_ind[nes]] - p_trans_inf * init_pop[v_AI_ind[1]]
      # AI2,...AIn
      if (nis > 1) {
        d[v_AI_ind[2:nis]] <-
          p_trans_inf * (init_pop[v_AI_ind[1:(nis - 1)]] - # incoming I_(i-1) --> I_i
                           init_pop[v_AI_ind[2:nis]]) # exiting I_i --> I_(i+1)
      }
      
      ## dI1...dIn
      v_I_ind <- v_ind$v_I_ind
      # I1
      d[v_I_ind[1]] <- (1 - prop_asym[cag + 1, agg + 1]) * p_trans_exp * init_pop[v_E_ind[nes]] - p_trans_inf * init_pop[v_I_ind[1]]
      # I2,...In
      if (nis > 1) {
        d[v_I_ind[2:nis]] <-
          p_trans_inf * (init_pop[v_I_ind[1:(nis - 1)]] - # incoming I_(i-1) --> I_i
                           init_pop[v_I_ind[2:nis]]) # exiting I_i --> I_(i+1)
      }
      
      ## H (hospitalization)
      v_H_ind <- v_ind$v_H_ind
      
      d[v_H_ind] <- 
        p_trans_inf * init_pop[v_I_ind[nis]] * prop_hosp[agg + 1] * (1 - prop_ICU[agg + 1]) - # incoming from infected state
        p_h_exit[cag + 1, agg + 1] * init_pop[v_H_ind] 
      
      ## ICU
      v_ICU_ind <- v_ind$v_ICU_ind
      # weighted average of exit probabilities (due to potential ICU overwhelm)
      p_icu_exit_overall <- (1 - p_icu_overflow) * p_icu_exit[cag + 1, agg + 1] + p_icu_overflow * p_icu_exit_nobed 
      # dICU
      d[v_ICU_ind] <-
        p_trans_inf * init_pop[v_I_ind[nis]] * prop_hosp[agg + 1] * prop_ICU[agg + 1] - # incoming from infected state
        p_icu_exit_overall * init_pop[v_ICU_ind] # ICU overall probability of exiting
      
      ## R (recovery)
      v_R_ind <- v_ind$v_R_ind
      # weighted average of recovery flow (due to potential ICU overwhelm)
      p_icu2rec_overall <- (1 - p_icu_overflow) * p_icu_exit[cag + 1, agg + 1] * (1-prob_icu_death[cag+1,agg+1]) +
        p_icu_overflow * p_icu_exit_nobed * (1-prob_icu_death_no_bed[cag+1,agg+1]) 

      # dR
      d[v_R_ind] <-
        (1 - prop_inf_die[cag + 1, agg + 1]) * p_trans_inf * init_pop[v_I_ind[nis]] * (1 - prop_hosp[agg + 1]) + # incoming directly from infectious, no hospitalization
        p_trans_inf * init_pop[v_AI_ind[nis]] + # incoming from aysmptomatic infection
        (1 - prob_hosp_death[cag+1,agg+1])*p_h_exit[cag + 1,agg + 1] * init_pop[v_H_ind] +
        p_icu2rec_overall * init_pop[v_ICU_ind] # recovering from ICU
      
      ## D (death)
      v_D_ind <- v_ind$v_D_ind
      # dD
      d[v_D_ind] <-
        p_icu_exit[cag + 1, agg + 1] * (1 - p_icu_overflow) * prob_icu_death[cag+1,agg+1] * init_pop[v_ICU_ind] + # incoming from ICU, with bed
        p_icu_exit_nobed * p_icu_overflow * prob_icu_death_no_bed[cag+1,agg+1] * init_pop[v_ICU_ind] + # incoming from ICU, without bed
        (prop_inf_die[cag + 1, agg + 1]) * p_trans_inf * init_pop[v_I_ind[nis]] * (1 - prop_hosp[agg + 1]) + # incoming from Infectious state
        prob_hosp_death[cag+1,agg+1]*p_h_exit[cag + 1,agg + 1] * init_pop[v_H_ind] 
      
      ## HD (home death)
      v_HD_ind <- v_ind$v_HD_ind
      # dHD
      d[v_HD_ind] <- 
        prop_inf_die[cag + 1, agg + 1] * p_trans_inf * init_pop[v_I_ind[nis]] * (1 - prop_hosp[agg + 1]) 
      
      ## CH (cumulative hospitalizations)
      v_CH_ind <- v_ind$v_CH_ind
      # dCH
      d[v_CH_ind] <- 
        p_trans_inf * init_pop[v_I_ind[nis]] * prop_hosp[agg + 1] * (1 - prop_ICU[agg + 1])
    }
  }
  
  return(list("d" = d,
              "v_strat_active" = c("sd" = social_distancing_op,
                                   "sip" = sip_op,
                                   "sd60p" = social_distancing_60_op,
                                   "bec" = behavior_change_op)))
}
