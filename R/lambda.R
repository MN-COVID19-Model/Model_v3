#' @title Calculate Lambda
#'
#' takes contact matrix, current state of model, and parameter set and calculates the number of contacts
#' each age group have with infected people
#'
#' @param mixing_matrix- matrix which has the number of contacts age group i has with age group j e.g. mixing_matrix[1,2]
#' is the number of contacts age group 1 has with age group 2, 
#' @param init_pop- current state of the model
#' @param parms- list of parameters
#'
#' @return
#' a vector which has the number of contacts a person in each age group has with an infected person each day
#'
#' @export
calculate_lambda <- function(mixing_matrix, init_pop,parms) {

  df_ind <- parms$df_ind
  v_exp_str <- parms$v_exp_str
  v_inf_str <- parms$v_inf_str
  v_asym_inf_str <- parms$v_asym_inf_str
  
  # Calculate number of people in each age group who are infected and living
  n_inf_by_age <- rep(0, parms$n_age_groups)
  n_alive_by_age <- rep(0, parms$n_age_groups) #parms$N_by_age

  for (i in 1:parms$n_age_groups) {
    ind_key <- paste0("lambda", i)
    v_ind <- parms$hash_table[[ind_key]]
    if (is.null(v_ind)) {
      # Here we assume that people wth asymptomatic or mild infections are just as infectious as people with symptomatic and severe infections
      v_ind_inf_by_age <- df_ind[df_ind$ie_str %in% c(v_asym_inf_str, v_inf_str) & df_ind$ia == (i - 1), "index"]
      v_ind_D_by_age <- df_ind[df_ind$ie_str == "D" & df_ind$ia == (i - 1), "index"]
      v_ind <- list("v_ind_inf_by_age" = v_ind_inf_by_age,
                    "v_ind_D_by_age" = v_ind_D_by_age)
      parms$hash_table[[ind_key]] <- v_ind
    }
    n_inf_by_age[i] <- sum(init_pop[v_ind$v_ind_inf_by_age])
    n_alive_by_age[i] <- parms$N_by_age[i] - sum(init_pop[v_ind$v_ind_D_by_age])
  }
  
  # Calculate prevalence of infectious disease in each age group
  p_inf_by_age <- n_inf_by_age / n_alive_by_age

  # Calculate how many contacts a person in each age group has with other age groups and how many are infectious
  n_inf_contacts <- rep(0, parms$n_age_groups)

  for(k in 1:(parms$n_age_groups)) {
    n_inf_contacts[k] <- sum(mixing_matrix[k,] * p_inf_by_age)
  }

  return(lambda = n_inf_contacts)
}
