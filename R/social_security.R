#' Social Security Bends
#'
#' Calculate social security (SS) benefits from average monthly income and
#' SS bends
#'
#' @param monthly_income Monthly income from top 35 average annual incomes
#' @param ss_bends Data frame of social security bends: income thresholds and
#' benefit percents
#'
#' @return
#' @export
#'
#' @examples
#' social_security_bends(2000, ss_bends)
social_security_bends <- function(monthly_income, ss_bends) {
  income_thresholds <- ss_bends$income_threshold
  marginal_income <- diff(income_thresholds)
  percent_benefit <- ss_bends$percent_benefit

  remaining_income <- monthly_income
  benefit <- 0

  for(bend in 1:length(percent_benefit)) {
    bend_threshold <- ifelse(
      bend <= length(marginal_income), marginal_income[bend], Inf
    )
    bend_income <- min(bend_threshold, remaining_income)

    remaining_income <- remaining_income - bend_income
    benefit <- benefit + bend_income * percent_benefit[bend]
  }
  benefit
}

#' Calculate Social Security benefits
#'
#' @param lifetime_income Lifetime income by year
#' @param ss_bends Data frame of social security bends: income thresholds and
#' benefit percents
#'
#' @return
#' @export
#'
#' @examples
#' calc_social_security_benefit(get_lifetime_income(25, 30000), ss_bends)
calc_social_security_benefit <- function(lifetime_income, ss_bends) {
  sort(lifetime_income, decreasing = TRUE) %>%
    "["(1:35) %>%
    mean() %>%
    "/"(12) %>%
    social_security_bends(ss_bends)
}

#' Estimate Social Security benefit
#'
#' @param current_age Current age
#' @param current_income Current annual income
#' @param ss_bends Data frame of social security bends: income thresholds and
#' benefit percents
#'
#' @return
#' @export
#'
#' @examples
#' social_security_benefit(25, 30000, ss_bends)
social_security_benefit <- function(current_age, current_income, ss_bends) {
  lifetime_income <- get_lifetime_income(current_age, current_income)

  ss_benefit <- calc_social_security_benefit(lifetime_income, ss_bends) * 12
}

#' Adjust taxable income for Social Security benefits
#'
#' Reduces taxable income by half of social security benefits
#'
#' @param taxable_income Taxable income
#' @param social_security_benefit Social Security benefit
#'
#' @return
#' @export
#'
#' @examples
#' adjust_taxable_income_for_social_security(30000, 12000)
adjust_taxable_income_for_social_security <- function(
  taxable_income, social_security_benefit
) {
  taxable_income - social_security_benefit / 2
}
