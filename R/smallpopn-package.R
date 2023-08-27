#' Package 'smallpopn'
#' 
#' Functions needed for paper "How big does a population
#' need to be before demographers can ignore individual-level
#' randomness in demographic events?"
#'
#' @section Functions:
#'
#' - `Lx` - Life table values for West model life table.
#' - `microsim` - Function to carry out demographic microsimulation.
#' - `mx` - Mortality rates for West model life table.
#' - `propn_age_fert` - Age distribution of fertility from Booth standard
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp evalCpp
#' @useDynLib smallpopn, .registration = TRUE
## usethis namespace: end
NULL
