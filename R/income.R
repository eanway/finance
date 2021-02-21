#' Income percentile
#'
#' Estimates income percentile given current age and income.
#' Used to estimate future earnings.
#'
#' @param current_age Current age
#' @param current_income Current annual income
#'
#' @return Estimated income percentile
#' @export
#'
#' @examples
#' get_income_percentile(25, 30000)
get_income_percentile <- function(current_age, current_income) {
  scam::predict.scam(
    smooth_percent_by_income_age,
    data.frame(age = current_age, income = current_income)
  ) %>%
    unname()
}

#' Get lifetime income
#'
#' Estimates lifetime income based on current age and income.
#' Used to estimate social security benefits.
#'
#' @param current_age Current age
#' @param current_income Current annual income
#' @param starting_age Age when started working
#' @param retirement_age Age when stopped working
#'
#' @return
#' @export
#'
#' @examples
#' get_lifetime_income(25, 30000)
get_lifetime_income <- function(
  current_age, current_income, starting_age = 18, retirement_age = 70
) {
  income_pct <- get_income_percentile(current_age, current_income)

  vec_age_to_retirement <- starting_age:retirement_age

  scam::predict.scam(
    smooth_income_by_percent_age,
    data.frame(age = vec_age_to_retirement, percent = income_pct)
  ) %>%
    setNames(vec_age_to_retirement)
}

#' Get future income trajectory
#'
#' Estimates future income until retirement from current age and income.
#' Used to estimate total lifetime income to determine optimal tax brackets
#' for tax-deferral.
#'
#' @param current_age Current age
#' @param current_income Current income
#' @param retirement_age Age when stopped working
#'
#' @return Vector of future incomes
#' @export
#'
#' @examples
#' get_future_income(25, 30000)
get_future_income <- function(current_age, current_income, retirement_age = 70) {
  lifetime_income <- get_lifetime_income(
    current_age, current_income, retirement_age = 70
  )

  lifetime_income[names(lifetime_income) > current_age]
}
