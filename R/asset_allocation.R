#' Get an amount of bonds to include in your portfolio
#'
#' Suggests an amount of bonds to include in your portfolio based on
#' the total value of your portfolio, the annual amount you'd like to withdraw,
#' and the number of years until withdrawals begin.
#'
#' @param intercept Optional intercept for efficient bond allocation. Must be between 0 and 1. Defaults determined by experimentation.
#' @param slope Optional slope for efficient bond allocation. Must be between -1 and 0. Defaults determined by experimentation.
#' @inheritParams get_years_until_depleted
#'
#' @return An amount of bonds to include in your portfolio
#' @export
#'
#' @examples
#' get_amount_bonds(100000, 20000, 10)
get_amount_bonds <- function(total_amount = numeric(), annual_amount = numeric(), years_waiting = numeric(), constant = 1.01, return_rate = 0.05, ...) {

  years_until_depleted <- get_years_until_depleted(total_amount, annual_amount, years_waiting, constant, return_rate)

  years_bonds <- get_years_bonds(years_waiting, years_until_depleted, ...)

  years_bonds * annual_amount
}

get_years_bonds <- function(years_waiting, years_until_depleted, ...) {
  if(years_until_depleted < years_waiting) {
    stop("The years until depleted must be greater than the years waiting")
  }

  max(
    0,
    get_years_bonds_integral(
      years_until_depleted, ...
    ) - get_years_bonds_integral(
      years_waiting, ...
    )
  )
}

get_years_bonds_integral <- function(year, intercept = 0.8, slope = -0.036) {
  if(intercept < 0 | 1 <= intercept) {
    stop("The intercept must be between 0 and 1: (0, 1]")
  }

  if(slope < -1 | 0 < slope) {
    stop("The slope must be between -1 and 0: (-1, 0)")
  }

  # e.g. integral of 0.87-0.06y
  ## equals zero at 29
  mid <- -intercept/slope
  # years must be between 0 and the peak of the integral (quadratic)
  ## which happens at the lines zero
  year_range <- min(max(0, year), mid)
  intercept * year_range + slope / 2 * year_range^2
}
