#' Income percentile
#'
#' Estimates income percentile given current age and income.
#'
#' @return Estimated income percentile
#' @export
#'
#' @examples
#' get_income_percentile(25, 30000)
get_income_percentile <- function(age, income) {
  predict(
    smooth_percent_by_income_age, data.frame(age = age, income = income)
  ) %>%
    unname()
}

#' Get future income trajectory
#'
#' Estimates future income until retirement from current age and income.
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

  vec_age_to_retirement <- current_age:retirement_age

  predict(
    smooth_income_by_percent_age,
    data.frame(age = vec_age_to_retirement, percent = income_pct)
  ) %>%
    setNames(vec_age_to_retirement)
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
