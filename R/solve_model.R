#' @title Solve Model
#' 
#' takes initial conditions, sequence of times, model function, 
#' and parameter list and calculates the state of the model at each day timestep
#' 
#' @param y initial state of model
#' @param times vector of times to calculate model state at
#' @param func function which generates differences between model states, \code{\link{covid_19_main_function}}
#' @param parms list of parameters
#' @param return_full default = 0 for integer day output; set to 1 if returning full output matrix by time step
#' 
#' @return 
#' matrix of raw results, each row is 1 timestep, each column is a different model state
#' 
#' @export
solve_model <- function(y, times, func, parms, return_full = 0) {
  
  # parameters
  timestep <- parms$timestep
  v_strat_status <- parms$v_strat_status
  df_ind <- parms$df_ind
  v_exp_str <- parms$v_exp_str
  v_asym_inf_str <- parms$v_asym_inf_str
  v_inf_str <- parms$v_inf_str
  
  
  # traversing through the time sequence
  model_state <- y
  y["Time"] <- 1.0

  # initialize the output matrix
  out <- matrix(0, 
                nrow = ((length(times) - 1) * timestep + 1), 
                ncol = length(names(y)) + length(v_strat_status))
  
  colnames(out) <- c(names(y), names(v_strat_status))
  
  out_full <- matrix(0, nrow = (length(times)), ncol = length(names(y)) + length(v_strat_status))
  colnames(out_full) <- c(names(y), names(v_strat_status))

  # add the first row to the matrix
  out[1, ] <- out_full[1, ] <- c(y, 0 * v_strat_status)
  
  # vector of days since peak
  v_days_since_peak <- c("infections" = -Inf, "hospitalizations" = -Inf, "deaths" = -Inf)
  
  # indices for calculating infections, hospitalizations, and deaths
  ind_inf <- df_ind[df_ind$ie_str %in% c(v_exp_str, v_asym_inf_str, v_inf_str, "H", "ICU"), "index"]
  ind_hosp <- df_ind[df_ind$ie_str %in% c("H", "ICU"), "index"]
  ind_death <- df_ind[df_ind$ie_str %in% c("D"), "index"]
  
  for (i in 1:(length(times) - 1)) {
    
    ts <- times[i]
    
    # for each step, call the model function to get the differentials
    model_state <- as.vector(model_state)
    ls_res <- func(ts, model_state, parms, v_days_since_peak)
    new_state <- ls_res$d
    v_strat_active <- ls_res$v_strat_active
    new_state <- as.vector(new_state)
    
    # update the model state and add time 
    model_state <- model_state + new_state
    output_row <- model_state
    output_row["Time"] <- times[i + 1]
    
    out_full[i + 1, ] <- c(output_row, v_strat_active)
    
    # update abbreviated output if at an integer time point
    tnext <- times[i + 1]
    if (tnext == as.integer(tnext)) {
      # add state to the output matrix
      out[tnext, ] <- c(output_row, v_strat_active)

      # Update counter of days since peak
      # differences in infections, hospitalizations, and deaths
      if (tnext == 1) {
        v_days_since_peak["infections"] <- 0
        v_days_since_peak["hospitalizations"] <- 0
        daily_deaths <- diff(out[1, ind_death])
        v_days_since_peak["deaths"] <- 0
      } else  {
        v_days_since_peak["infections"] <- tnext - which.max(rowSums(out[1:tnext, ind_inf]))
        v_days_since_peak["hospitalizations"] <- tnext - which.max(rowSums(out[1:tnext, ind_hosp]))
        daily_deaths <- diff(rowSums(out[1:tnext, ind_death]))
        v_days_since_peak["deaths"] <- tnext - which.max(c(0, daily_deaths))
      }
    }
  }
  
  # return the matrix
  out[, "Time"] <- out[, "Time"] - 1
  out_full[, "Time"] <- out_full[, "Time"] - 1
  
  if(return_full == 1) {
    return(out_full)
  } else {
    return(out)
  }
}