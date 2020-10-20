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

growth_and_contributions_prime <- function(principle, contribution, years, return_rate = 0.05) {
  return_plus_one <- 1 + return_rate
  (principle * (return_plus_one) ^ years +
     contribution * return_plus_one ^ (years + 1) / return_rate) *
    log(return_plus_one)
}

#' Lowest contribution with growth and contribution
#'
#' What is the lowest contribution rate that will reach a given final amount
#' given a starting and final amount, years, and return rate?
#'
#'
#' @param principle The initial amount
#' @param years The number of years of growth and contribution
#' @param final_amount The final amount after years of growth and contribution
#' @param return_rate The rate of return each year
#'
#' @return A contribution rate
#' @export
#'
#' @examples
lowest_contribution <- function(principle, years, final_amount, return_rate = 0.05) {
  return_plus_one <- 1 + return_rate
  return_rate * (final_amount - principle * return_plus_one ^ years) /
    (return_plus_one * (return_plus_one ^ years - 1))
}

#' Fastest goal total amount
#'
#' How quickly can you reach the goal total amount, given a starting amount,
#' maximum annual contribution, current age, and goal annual amount.
#'
#' Numerical approximation using Newton's method.
#' https://math.stackexchange.com/questions/925838/constructing-a-while-loop-in-r-for-newtons-method
#'
#' @param principle The initial total amount
#' @param maximum_contribution The maximum annual contribution possible
#' @param age Your current age
#' @param goal_annual_amount The goal annual amount to withdraw in retirement
#' @param constant Optional constant to reduce short-term returns. Default determined from experimentation.
#' @param return_rate Optional long term rate of return. Default determined from experimentation.
#'
#' @return A number of years until growth and contributions reaches goal growth
#' @export
#'
#' @examples
fastest_goal <- function(principle, maximum_contribution, age, goal_annual_amount, constant = 1.01, return_rate = 0.05) {
  guess <- 0
  while(abs(g_goal(guess, principle, maximum_contribution, age, goal_annual_amount, constant, return_rate)) > 0.1) {
    guess <- guess -
      g_goal(guess, principle, maximum_contribution, age, goal_annual_amount, constant, return_rate) /
      g_goal_prime(guess, principle, maximum_contribution, age, goal_annual_amount, constant, return_rate)
  }
  guess
}

g_goal <- function(years, principle, maximum_contribution, age, goal_annual_amount, constant, return_rate) {
  get_goal_total_amount(goal_annual_amount, 65 - age - years, constant, return_rate) -
    growth_and_contributions(principle, maximum_contribution, years, return_rate)
}

g_goal_prime <- function(years, principle, maximum_contribution, age, goal_annual_amount, constant, return_rate) {
  get_goal_total_amount(goal_annual_amount, 65 - age - years, constant, return_rate) *
    return_rate -
    growth_and_contributions_prime(principle, maximum_contribution, years, return_rate)
}

#' Get bridge amount at age
#'
#' Get the amount needed to bridge your early retirement until age 65 based on
#' a withdrawal rate.
#'
#' @param age Current age
#' @param goal_annual_amount Goal amount to withdraw each year in early retirement
#' @param return_rate Return rate of the bridge account
#'
#' @return
#' @export
#'
#' @examples
bridge_amount <- function(age, goal_annual_amount, return_rate = 0.04) {
  return_rate_plus_one <- return_rate + 1
  goal_annual_amount / (return_rate * return_rate_plus_one ^ (65 - age)) * (
    return_rate_plus_one ^ (65 - age + 1) - return_rate_plus_one
  )
}

#' Early retirement planning
#'
#' What proportion of contributions should go to retirement and early retirement
#' (bridge) funds?
#'
#' Determined numerically with Newton's method. Assumes that reaching
#' early retirement goals occurs at the same time as reaching retirement goals.
#' Also assumes that annual withdrawals in early retirement and retirement are
#' the same.
#'
#' @param principle_retirement Starting principle amount in retirement funds
#' @param principle_bridge Starting principle amount in early retirement bridge funds
#' @param contribution Total contribution to be split between retirement and bridge funds
#' @param age Current age
#' @param goal_annual_amount Goal amount to withdraw annually when retired (early and late)
#' @param constant_retirement A constant to adjust retirement returns
#' @param constant_bridge A constant to adjust bridge returns
#' @param return_rate_retirement The rate of return of retirement funds
#' @param return_rate_bridge The rate of return of bridge funds, usually lower than retirement
#'
#' @return A proportion of contributions that should go towards retirement
#' @export
#'
#' @examples
retire_early <- function(principle_retirement, principle_bridge, contribution, age, goal_annual_amount, constant_retirement = 1.01, return_rate_retirement = 0.05, return_rate_bridge = 0.04) {
  fewest_years_to_retire <- fastest_goal(
    principle_retirement, contribution, age, goal_annual_amount
  )

  lowest_contribution_to_retire <- lowest_contribution(
    goal_annual_amount, 65 - age, get_goal_total_amount(48000, 0)
  )

  if(age + fewest_years_to_retire > 65 | lowest_contribution_to_retire > contribution) {
    warning("Contribution is too low or goal annual amount is too high to retire by 65") %>%
      stop()
  }

  guess <- 0.5

  while(
    abs(
      g_bridge(
        guess, principle_retirement, principle_bridge, contribution, age,
        goal_annual_amount, constant_retirement,
        return_rate_retirement, return_rate_bridge
      )
    ) > 0.1
  ) {
    guess <- guess -
      g_bridge(
        guess, principle_retirement, principle_bridge, contribution, age,
        goal_annual_amount, constant_retirement,
        return_rate_retirement, return_rate_bridge
      ) /
      g_bridge_prime(
        guess, principle_retirement, principle_bridge, contribution, age,
        goal_annual_amount, constant_retirement,
        return_rate_retirement, return_rate_bridge
      )
  }
  guess
}

g_bridge <- function(contribution_percent_retirement, principle_retirement, principle_bridge, contribution, age, goal_annual_amount, constant_retirement, return_rate_retirement, return_rate_bridge) {
  contribution_retirement <- contribution_percent_retirement * contribution
  contribution_bridge <- contribution - contribution_retirement


  years_to_goal <- fastest_goal(
    principle_retirement, contribution_retirement, age, goal_annual_amount,
    constant_retirement, return_rate_retirement
  )

  years_to_bridge <- get_years_to_bridge(contribution_bridge, principle_bridge, age, goal_annual_amount, return_rate_bridge)

  years_to_goal - years_to_bridge
}

get_years_to_bridge <- function(contribution_bridge, principle_bridge, age, goal_annual_amount, return_rate_bridge) {
  return_bridge_plus_one <- return_rate_bridge + 1

  log(
    (
      return_bridge_plus_one ^ 65 * (goal_annual_amount + contribution_bridge)
    ) /
    (
      goal_annual_amount * return_bridge_plus_one ^ age +
        contribution_bridge * return_bridge_plus_one ^ 65 +
        principle_bridge * return_rate_bridge * return_bridge_plus_one ^ 64
    )
  ) /
  log(return_bridge_plus_one)
}

g_bridge_prime <- function(contribution_percent_retirement, principle_retirement, principle_bridge, contribution, age, goal_annual_amount, constant_retirement, return_rate_retirement, return_rate_bridge) {
  contribution_retirement <- contribution_percent_retirement * contribution

  years_to_goal <- fastest_goal(
    principle_retirement, contribution_retirement, age, goal_annual_amount,
    constant_retirement, return_rate_retirement
  )

  goal_prime <- g_goal_prime(
    years_to_goal, principle_retirement, contribution_retirement, age,
    goal_annual_amount, constant_retirement, return_rate_retirement
  )

  bridge_prime <- 1

  goal_prime - bridge_prime
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
