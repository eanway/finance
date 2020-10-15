#' Plan retirement allocation
#'
#' Predict and plan suggested retirement allocation using the bond allocation model.
#'
#' @param birth_year Year of birth
#' @param age Current age
#' @param starting_amount Current amount of money saved for retirement
#' @param annual_withdrawal Annual amount desired to be withdrawn in retirement
#' @param limit_change Should changes in the asset allocation be limited to contributions and withdrawals?
#' @param contribution_or_withdrawal_amount A vector of amount to contribute or withdraw
#' @param bond_rates A vector of changes in bond values over time
#' @param stock_rates A vector of changes in stock values over time
#'
#' @return A graph of the theoretical performance of the bond allocation model
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' plan_retirement_allocation(1991, 21, 1000, 40000, TRUE, rep(6000, 10), rep(1.02, 10), rep(1.05, 10))
plan_retirement_allocation <- function(birth_year, age, starting_amount, annual_withdrawal, limit_change, contribution_or_withdrawal_amount, bond_rates, stock_rates) {

  n_periods <- length(contribution_or_withdrawal_amount)

  if(n_periods != length(bond_rates) | n_periods != length(stock_rates)) {
    stop("The length of contribution/withdrawal, bond, and stock vectors should be the same")
  }

  current_year <- birth_year + age
  vec_periods <- 1:n_periods
  new_total <- starting_amount
  current_years_until_available <- 65 - age
  vec_years_waiting <- max(0, current_years_until_available - vec_periods)
  current_bond_value <- get_amount_bonds(
    total_amount = new_total,
    annual_amount = annual_withdrawal,
    years_waiting = vec_years_waiting[1]
  ) %>%
    min(new_total)
  current_stock_value <- new_total - current_bond_value

  df_main <- data.frame(
    period = vec_periods,
    year = current_year:(current_year + n_periods - 1),
    vec_years_waiting,
    bond_rates,
    stock_rates,
    starting_bond_value = NA_real_,
    starting_stock_value = NA_real_,
    new_total = NA_real_,
    goal_bond_value = NA_real_,
    goal_stock_value = NA_real_,
    difference_bonds = NA_real_,
    change_bonds = NA_real_,
    change_stocks = NA_real_,
    ending_bond_value = NA_real_,
    ending_stock_value = NA_real_
  )

  for (i_period in vec_periods) {
    current_bond_value <- current_bond_value * bond_rates[i_period]
    current_stock_value <- current_stock_value * stock_rates[i_period]
    current_years_waiting <- vec_years_waiting[i_period]
    income_or_expense <- contribution_or_withdrawal_amount[i_period]

    new_total <- current_bond_value + current_stock_value + income_or_expense

    if(new_total < 0) {
      break
    }

    goal_bond_value <-get_amount_bonds(
        total_amount = new_total,
        annual_amount = annual_withdrawal,
        years_waiting = current_years_waiting
      ) %>%
      min(new_total)

    goal_stock_value <- new_total - goal_bond_value
    difference_bonds <- goal_bond_value - current_bond_value
    if(limit_change) {
      if(income_or_expense > 0) {
        # income
        change_bonds <- min(income_or_expense, difference_bonds)
        change_bonds <- max(change_bonds, 0)
      } else {
        # expense
        change_bonds <- max(income_or_expense, difference_bonds)
        change_bonds <- min(change_bonds, 0)
      }
      change_stocks <- income_or_expense - change_bonds
    } else {
      change_bonds <- difference_bonds
      change_stocks <- goal_stock_value - current_stock_value
    }

    i_row <- with(df_main, period == i_period)
    df_main[i_row, 6:13] <- c(
      current_bond_value,
      current_stock_value,
      new_total,
      goal_bond_value,
      goal_stock_value,
      difference_bonds,
      change_bonds,
      change_stocks
    )

    current_bond_value <- current_bond_value + change_bonds
    current_stock_value <- current_stock_value + change_stocks
    df_main[i_row, 14:15] <- c(
      current_bond_value,
      current_stock_value
    )
  }
  df_main
}
