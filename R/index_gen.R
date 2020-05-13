#' Generate Indices
#'
#' takes vector of epigroups, comorbidity groups, and age groups and generates the indices that will reference the matching state in the
#' vector of model states. Note that the number of exposed and infectious compartments used in the model is a variable that is specified in
#' the file 'parameters_v3.R'. The get_ind function is called in the parameters() function to create a data.frame for indexing that is used throughout
#' the rest of the model.
#'
#' @param eg integer indicating which epigroup from list defined in parameter function
#' c("S","E1","E2","E3","E4","I1","I2","I3","I4","I5","I6","I7","H","ICU","R","D","HD", "CH")
#' e.g. 1 would be "S", 8 would be "I3"
#' @param cg integer indicating which comorbidity group from list defined in parameter function, starting from 0
#' c("c0,c1")
#' e.g. 0 would be "c0"
#' @param ag integer indicating which age group from list defined in parameter function
#' c(0,10,20,30,40,50,60,70,80)
#' e.g. index 0 would be "0-9" age group, and index 5 would be "50-59" age group
#' @param ncg total number of comorbidity groups
#' @param neg total number of epigroups
#' @param nag total number of age groups
#'
#' @return
#' a vector of integers which reference the matching state in the vector of model states
#'
#' @export
get_ind <- function(eg, cg, ag, ncg = 2, neg = 61, nag = 9){

  if( any(eg < 1 | eg > neg) ) { stop('epigroup index out of bounds')}
  if( any(cg < 0 | cg > ncg) ) { stop('comorbidity index out of bounds')}
  if( any(ag < 0 | ag > nag) ) { stop('age index out of bounds')}

    return(eg*ncg + cg + ag*neg*ncg - (ncg-1))
}
