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
