#' Allocate Assets
#'
#' @param total_amount numeric
#' @param annual_amount numeric
#' @param years_waiting numeric
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

# return is about 5% (0.05) each year = return
# the annual amount needed when available = annual_amount
# the total amount available when needed = total_amount
# the number of years until the annual amount is needed = years_waiting
# the years of money already saved = total_amount / annual_amount = years_saved
# less money is needed in later years because unneeded money will grow ~5% a year
# the money needed each year is modeled by:
## years_saved = 1.01 * exp(-return * years_waiting)
### because the return is constant,
### the total amount depends only on the years_waiting
## this equation gives the amount needed for a single year, not multiple years
## the total amount needed for multiple years is modeled by
## the indefinite integral of the equation:
### f(y) = years_saved = -1.01 * exp(-return * years_waiting) / return
## as the years until needed approaches infinity, the years_saved approaches 0
get_years_saved_at_year <- function(constant, return_rate, years_waiting) {
  -constant * exp(-return_rate * years_waiting) / return_rate
}

# if you have to wait before using the money
# how much money do you need before it's infinite?
## the definite integral from the years_waiting to Inf is:
### f(Inf) - f(years_waiting)
### f(Inf) = 0
get_years_saved_infinite <- function(constant, return_rate, years_waiting) {
  0 - get_years_saved_at_year(constant, return_rate, years_waiting)
}

# if you have some years saved
## how many years do you need to wait before it's infinite?
# years_saved = f(Inf) - f(years_waiting)
## years_saved = 0 - -1.01 * exp(-return_rate * years_waiting) / return_rate
### years_saved = 1.01 * exp(-return_rate * years_waiting) / return_rate
# solve for years_waiting:
## years_waiting = log(withdrawal_rate * return_rate / 1.01) / return_rate
get_years_waiting_infinite <- function(constant, return_rate, years_saved) {
  log(constant / (return_rate * years_saved)) / return_rate
}

# if you have some years saved and some years to wait
## how many years will it last once you start using it?
## years_saved = f(years_effective) - f(years_waiting)
## when solving for years_effective, you need to take the log of both sides
### the difference of the terms below is on one side
## a negative value can't be logged and means that the final value will be infinite
# solve for years until depleted:
get_years_until_depleted <- function(total_amount = numeric(), annual_amount = numeric(), years_waiting, constant = 1.01, return_rate = 0.05) {
  years_saved <- get_years_saved(total_amount, annual_amount)

  amount_needed_waiting <- exp(-return_rate * years_waiting)
  amount_generated_savings <- return_rate * years_saved / constant

  if(amount_generated_savings > amount_needed_waiting) {
    Inf
  } else {
    log(
      amount_needed_waiting - amount_generated_savings
    ) / -return_rate
  }
}

get_years_saved <- function(total_amount, annual_amount) {
  total_amoung / annual_amount
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
