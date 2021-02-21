#' Income trajectory data
#'
#' Data of income by age and income percentile
#'
#' @docType data
#'
#' @usage data(income_trajectory)
#'
#' @format An object of class 'lm'
#'
#' @keywords datasets
#'
#' @source \href{https://dqydj.com/income-percentile-by-age-calculator/}{DQYDJ}
#'
#' @examples
#' data(income_trajectories)
#' lm(income ~ age + percent, income_trajectories)
"income_trajectories"

#' Smoothed income percentile data
#'
#' Smoothed shape-constrained additive model (SCAM) of
#' income percentiles by income and age
#'
#' @docType data
#'
#' @usage data(smooth_percent_by_income_age)
#'
#' @format An object of class 'scam', 'glm', 'lm'
#'
#' @keywords datasets
#'
#' @source \href{https://dqydj.com/income-percentile-by-age-calculator/}{DQYDJ}
#'
#' @examples
#' data(smooth_percent_by_income_age)
#' predict(
#'   smooth_percent_by_income_age,
#'   data.frame(income = 30000, age = 25)
#' )
"smooth_percent_by_income_age"

#' Smoothed income trajectory data
#'
#' Smoothed shape-constrained additive model (SCAM) of
#' income by age and income percentile
#'
#' @docType data
#'
#' @usage data(smooth_income_by_percent_age)
#'
#' @format An object of class 'scam', 'glm', 'lm'
#'
#' @keywords datasets
#'
#' @source \href{https://dqydj.com/income-percentile-by-age-calculator/}{DQYDJ}
#'
#' @examples
#' data(smooth_income_by_percent_age)
#' predict(
#'   smooth_income_by_percent_age,
#'   data.frame(percent = 0.5, age = 25)
#' )
"smooth_income_by_percent_age"

#' Required minimum distributions (RMDs)
#'
#' Required minimum distributions for deferred taxable income in retirement in
#' 2021
#'
#' @docType data
#'
#' @usage data(rmds)
#'
#' @format A double vector
#'
#' @keywords datasets
#'
#' @source \href{https://www.irs.gov/publications/p590b#en_US_2019_publink1000231258}{RMDs}
#'
#' @examples
#' data(rmds)
"rmds"

#' Social security bends
#'
#' Dataframe of social security bends and percent benefits for 2021
#'
#' @docType data
#'
#' @usage data(ss_bends)
#'
#' @format An object of class 'data.frame'
#'
#' @keywords datasets
#'
#' @source \href{https://www.ssa.gov/oact/COLA/piaformula.html}{PIA formula}
#'
#' @examples
#' data(ss_bends)
"ss_bends"

#' Tax brackets
#'
#' Dataframe of tax brackets and tax rates for 2020
#'
#' @docType data
#'
#' @usage data(tax_brackets)
#'
#' @format An object of class 'data.frame'
#'
#' @keywords datasets
#'
#' @source \href{https://www.irs.gov/newsroom/irs-provides-tax-inflation-adjustments-for-tax-year-2020}{IRS 2020}
#'
#' @examples
#' data(tax_brackets)
"tax_brackets"
