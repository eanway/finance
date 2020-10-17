#' Get years until depletion
#'
#' Calculate how many years a portfolio will last until it is fully depleted
#'
#' @param total_amount Total amount in the portfolio. Numeric greater than 0.
#' @param annual_amount Annual amount to be withdrawn from the portfolio. Numeric greater than 0.
#' @param years_waiting Years until the annual amount will start being withdrawn. Numeric greater than or equal to 0.
#' @param constant Optional constant to reduce short-term returns. Default determined from experimentation.
#' @param return_rate Optional long term rate of return. Default determined from experimentation.
#'
#' @return positive numeric or Inf
#' @export
#'
#' @examples
#' get_years_until_depleted(10000, 1000, 5)
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
  total_amount / annual_amount
}

#' Get total amount needed to theoretically last forever
#'
#' Given a goal annual amount and years waiting, what total amount of money
#' is needed to theoretically last forever?
#'
#' @param annual_amount Annual amount to be withdrawn from the portfolio. Numeric greater than 0.
#' @param years_waiting Years until the annual amount will start being withdrawn. Numeric greater than or equal to 0.
#' @param constant Optional constant to reduce short-term returns. Default determined from experimentation.
#' @param return_rate Optional long term rate of return. Default determined from experimentation.
#'
#' @return Goal total amount
#' @export
#'
#' @examples
get_goal_total_amount <- function(annual_amount, years_waiting, constant = 1.01, return_rate = 0.05) {
  annual_amount * get_years_saved_infinite(years_waiting, constant, return_rate)
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
get_years_saved_at_year <- function(years_waiting, constant, return_rate) {
  -constant * exp(-return_rate * years_waiting) / return_rate
}

# if you have to wait before using the money
# how much money do you need before it's infinite?
## the definite integral from the years_waiting to Inf is:
### f(Inf) - f(years_waiting)
### f(Inf) = 0
get_years_saved_infinite <- function(years_waiting, constant, return_rate) {
  0 - get_years_saved_at_year(years_waiting, constant, return_rate)
}

#' Growth with contributions
#'
#' Growth of an account with both growth and contributions over time.
#' http://www.moneychimp.com/articles/finworks/fmbasinv.htm
#'
#' @param principle The initial amount
#' @param contribution The annual contribution
#' @param years The number of years of growth and contribution
#' @param return_rate The rate of return each year
#'
#' @return The final amount
#' @export
#'
#' @examples
growth_and_contributions <- function(principle, contribution, years, return_rate = 0.05) {
  return_plus_one <- 1 + return_rate
  principle * (return_plus_one) ^ years +
    contribution * (return_plus_one ^ (years + 1) - return_plus_one) / return_rate
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
