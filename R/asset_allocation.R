#' Allocate Assets
#'
#' @param total_amount numeric
#' @param annual_amount numeric
#' @param years_waiting numeric
#' @param constant optional constant to adjust depletion calculation
#' @param return_rate optional rate of return
#'
#' @return list
#' @export
#'
#' @examples
#' allocate_assets(100000, 20000, 10)
allocate_assets <- function(total_amount = numeric(), annual_amount = numeric(), years_waiting = numeric(), constant = 1.01, return_rate = 0.05) {

  years_until_depleted <- get_years_until_depleted(total_amount, annual_amount, years_waiting, constant, return_rate)

  years_bonds <- get_years_bonds(years_waiting, years_until_depleted)

  amount_bonds <- years_bonds * annual_amount

  percent_bonds <- amount_bonds / total_amount

  list(
    amount_bonds = amount_bonds,
    percent_bonds = percent_bonds
  )
}

get_years_bonds <- function(years_waiting, years_until_depleted) {
  if(years_until_depleted < years_waiting) {
    stop("The years until depleted must be greater than the years waiting")
  }

  max(
    0,
    get_years_bonds_integral(
      years_until_depleted
    ) - get_years_bonds_integral(
      years_waiting
    )
  )
}

get_years_bonds_integral <- function(year, intercept = 0.87, slope = -0.06) {
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
