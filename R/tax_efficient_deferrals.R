#' Get maximum tax bracket
#'
#' Finds the maximum tax bracket that will be paid with future incomes
#'
#' @param future_income A vector of future incomes
#' @param tax_brackets A table of tax brackets
#'
#' @return
#' @export
#'
#' @examples
#' get_max_tax_bracket(c(10000, 50000), tax_brackets)
get_max_tax_bracket <- function(future_income, tax_brackets) {
  tax_brackets %>%
    dplyr::filter(cumulative_income_over < max(future_income)) %>%
    dplyr::arrange(cumulative_income_over) %>%
    dplyr::pull(marginal_tax_rate) %>%
    max()
}

#' Get total income over a threshold
#'
#' Calculate total income over a threshold
#'
#' @param income_threshold A double
#' @param future_income A vector of future incomes
#'
#' @return A double
#' @export
#'
#' @examples
#' get_total_income_over(10000, c(15000, 20000))
get_total_income_over <- function(income_threshold, future_income) {
  sum(future_income - income_threshold)
}

#' Tax bracket in/over which to defer taxes
#'
#' @param future_income A vector of future income until retirement
#' @param tax_brackets A table of tax brackets
#' @param ss_benefit A double of annual Social Security benefit in retirement
#' @param years_savings A double of years of savings needed to withdraw
#' in retirement
#'
#' @return A tax bracket from tax_brackets
#' @export
#'
#' @examples
#' deferral_tax_bracket(get_future_income(25, 30000), tax_brackets, 2000, 22)
deferral_tax_bracket <- function(
  future_income, tax_brackets, ss_benefit, years_savings
) {
  tax_brackets %>%
    dplyr::mutate(
      total_income_over = purrr::map_dbl(
        cumulative_income_over, get_total_income_over, future_income = future_income
      ),
      taxable_income_over = cumulative_income_over - ss_benefit / 2,
      taxable_savings_needed = taxable_income_over * years_savings
    ) %>%
    dplyr::filter(total_income_over > taxable_savings_needed) %>%
    dplyr::pull(marginal_tax_rate) %>%
    max()
}

#' Tax efficient deferrals
#'
#' Estimate the tax bracket in/above which you should defer taxes
#'
#' @param current_age Current age
#' @param current_income Current annual income
#' @param ss_bends A table of social security bends and benefit rates
#' @param rmds A vector of required minimum distributions
#' @param tax_brackets A table of tax brackets and tax rates
#'
#' @return
#' @export
#'
#' @examples
#' tax_efficient_deferrals(25, 30000, ss_bends, rmds, tax_brackets)
tax_efficient_deferrals <- function(
  current_age, current_income, ss_bends, rmds, tax_brackets
) {
  future_income <- get_future_income(current_age, current_income)

  lifetime_income <- get_lifetime_income(current_age, current_income)

  ss_benefit <- social_security_benefit(lifetime_income, ss_bends) * 12

  constant_withdrawal <- find_constant_withdrawal_rate(rmds)

  years_withdrawals <- find_years_constant_withdrawals(constant_withdrawal)

  starting_amount <- sum(1 / 1.03^(0:years_withdrawals))

  deferral_tax_bracket(future_income, tax_brackets, ss_benefit, starting_amount)
}
