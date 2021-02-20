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
