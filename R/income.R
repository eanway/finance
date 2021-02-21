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
  scam::predict.scam(
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

  scam::predict.scam(
    smooth_income_by_percent_age,
    data.frame(age = vec_age_to_retirement, percent = income_pct)
  ) %>%
    setNames(vec_age_to_retirement)
}
