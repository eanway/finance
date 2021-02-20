#' Income percentile
#'
#' @return Table of income percentiles
#' @export
#'
#' @examples
get_income_percentile <- function(age, income) {
  loess_percent <- loess(
    percent ~ age + income, data = income_trajectories, span = 0.5
  )

  predict(loess_percent, data.frame(age = age, income = income)) %>%
    unname()
}

#' Get future income trajectory
#'
#' Assumes income will increase from until 40 then plateau
#'
#' @param current_age Current age
#' @param current_income Current income
#'
#' @return Vector of future incomes
#' @export
#'
#' @examples
get_future_income <- function(current_age, current_income, retirement_age = 70) {
  income_pct <- get_income_percentile(current_age, current_income)
  loess_income <- loess(
    income ~ age + percent, data = income_trajectories, span = 0.5
  )

  predict(
    loess_income,
    data.frame(age = current_age:retirement_age, percent = income_pct)
  )
}

#' Tax efficient deferrals
#'
#' @param income Current income
#' @param age Current age
#' @param income_trajectories Table of income trajectories
#' @param tax_brackets Table of tax brackets
#'
#' @return
#' @export
#'
#' @examples
tax_efficient_deferrals <- function(
  current_age, future_income,
  income_trajectories = income_trajectories_2021,
  tax_brackets = tax_brackets_2021
) {

}
