
#' Mortality rates mx from the West model life table
#'
#' Mortality rates mx, by age, Lexis triangle, sex, and life table
#' level, for the West model life table.
#'
#' For a description of life tables, and West model life tables,
#' see \url{http://demographicestimation.iussp.org}.
#'
#' Above age 5, values for the upper Lexis triangle
#' are identical to values for the lower Lexis triangle,
#' for the same age, sex, and lifetable level.
#' Below age 5, the triangles are weighted
#' averages of mortality rates for ages 0 and 1-4.
#'
#' @seealso \code{\link{Lx}}
#' 
#' @source Modified from values obtained from the
#' \code{\link[demogR]{cdmltw}} function
#' in package \code{demogR}.
"mx"
