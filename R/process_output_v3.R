#' @title Process Output
#'
#' takes raw output from solve function and calculates cumulative infections, prevalent infections, daily deaths, cumulative deaths,
#' number of people in the hospital, number of people in the ICU for each timestep
#'
#' @param out- matrix of raw output from solve functio
#' @param parms- list of parameters
#'
#' @return
#' the same matrix that was inputted but with columns at the start for each calculated output
#'
#' @export
process_output <- function(out,parms){

  neg <- parms$n_epi_groups
  nag <- parms$n_age_groups
  ncg <- parms$n_co_groups
  ts <- parms$timestep
  nes <- parms$n_exposed_states
  nis <- parms$n_infected_states
  df_ind <- parms$df_ind
  v_exp_str <- parms$v_exp_str
  v_inf_str <- parms$v_inf_str
  v_asym_inf_str <- parms$v_asym_inf_str

  ## extract prevalent infections by summing over E+AI+I+ICU+H 
  prevalent_infections <- rowSums(out[, df_ind[df_ind$ie_str %in% c(v_exp_str, v_asym_inf_str, v_inf_str, "H", "ICU"), "index"]])

  ## extract cumulative infections by summing over  E+AI+I+ICU+H+R+D
  cumulative_infections <- rowSums(out[, df_ind[df_ind$ie_str %in% c(v_exp_str, v_asym_inf_str, v_inf_str, "H", "ICU", "R", "D"), "index"]])

  ## extract cumulative deaths by summing over deaths in all age groups and co-groups
  cumulative_deaths <- rowSums(out[, df_ind[df_ind$ie_str == "D", "index"]])

  ## extract prevalent hospitalizations by adding people in ICU or H
  prevalent_hospitalizations <- rowSums(out[, df_ind[df_ind$ie_str %in% c("H", "ICU"), "index"]])
  
  ## extract demand for ICU beds by adding people in the ICU in all age and co groups
  ICU_bed_demand <- rowSums(out[, df_ind[df_ind$ie_str == "ICU", "index"]])

  ## calculate daily deaths given cumulative deaths
  daily_deaths <- c(0, diff(cumulative_deaths))
  
  ## extract cumulative home deaths by summing over deaths in all age groups and co-groups
  cumulative_home_deaths <- rowSums(out[, df_ind[df_ind$ie_str == "HD", "index"]])
  
  ## extract cumulative hospitalizations by summing over deaths in all age groups and co-groups
  cumulative_hospitalizations <- rowSums(out[, df_ind[df_ind$ie_str == "CH", "index"]])
  
  out <- cbind(prevalent_infections = prevalent_infections,
               cumulative_infections = cumulative_infections, 
               daily_deaths = daily_deaths,
               cumulative_deaths = cumulative_deaths, 
               ICU_bed_demand = ICU_bed_demand,
               prevalent_hospitalizations = prevalent_hospitalizations,
               cumulative_home_deaths = cumulative_home_deaths, 
               cumulative_hospitalizations = cumulative_hospitalizations,
               out)
  return(out)
}
