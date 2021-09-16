
#' Demographic Microsimulation
#'
#' Given an initial population, and a set of
#' fertility and mortality rates,
#' randomly generate subsequent population, births,
#' and deaths.
#'
#' All dimensions of \code{initial_popn}, \code{fertility_rates},
#' and \code{mortality_rates} with the same
#' name should have the same length:
#' for instance, all dimensions called "age" should have the same
#' length. This includes the "age" dimension of \code{fertility_rates}.
#' Rates outside the reproductive ages should be set to zero.
#'
#' The "triangle" dimension holds Lexis triangles, and
#' must have labels "Lower" and "Upper".
#'
#' The "sex" dimension can be length 1, 2, or more.
#'
#' @param initial_popn An array with dimensions "age",
#' and "sex".
#' @param fertility_rates An array with dimensions
#' "age", "triangle", "sex", and "time".
#' @param mortality_rates An array with dimensions
#' "age", "triangle", "sex", and "time".
#' @param step The length of one age-time step.
#' Defaults to 5.
#' @param dominant_sex Which sex provides the exposure
#' term for birth rates. Defaults to "Female".
#'
#' @return A list of arrays with names
#' "population", "births", and "deaths".
#'
#' @export
microsim <- function(initial_popn,
                     fertility_rates,
                     mortality_rates,
                     step = 5L,
                     dominant_sex = "Female") {
    ## check names of dimensions
    stopifnot(identical(names(dimnames(initial_popn)),
                        c("age", "sex")))
    stopifnot(identical(names(dimnames(fertility_rates)),
                        c("age", "triangle", "sex", "time")))
    stopifnot(identical(names(dimnames(mortality_rates)),
                        c("age", "triangle", "sex", "time")))
    stopifnot(identical(dimnames(initial_popn),
                        dimnames(fertility_rates)[c(1L, 3L)]))
    stopifnot(identical(dimnames(fertility_rates),
                        dimnames(mortality_rates)))
    stopifnot(identical(dimnames(mortality_rates)$triangle,
                        c("Lower", "Upper")))
    ## extract lengths of dimensions
    n_age <- dim(initial_popn)[[1L]]
    n_sex <- dim(initial_popn)[[2L]]
    n_period <- dim(fertility_rates)[[4L]]
    ## tidy length_step
    step <- as.integer(step)
    stopifnot(identical(length(step), 1L))
    stopifnot(step >= 1L)
    ## obtain (0-based) index for dominant sex
    stopifnot(identical(length(dominant_sex), 1L))
    i_dom_sex <- match(dominant_sex, dimnames(initial_popn)$sex) - 1L
    if (is.na(i_dom_sex))
        stop(gettextf("dimnames for \"%s\" dimension do not include dominant sex [\"%s\"]",
                      "sex", dominant_sex))
    ## run simulation
    microsim_inner(initial_popn,
                   fertility_rates,
                   mortality_rates,
                   n_age,
                   n_sex,
                   n_period,
                   step,
                   i_dom_sex)
}
