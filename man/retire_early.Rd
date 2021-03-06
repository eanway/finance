% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_portfolio.R
\name{retire_early}
\alias{retire_early}
\title{Early retirement planning}
\usage{
retire_early(
  principle_retirement,
  principle_bridge,
  contribution,
  age,
  goal_annual_amount,
  constant_retirement = 1.01,
  return_rate_retirement = 0.05,
  return_rate_bridge = 0.04
)
}
\arguments{
\item{principle_retirement}{Starting principle amount in retirement funds}

\item{principle_bridge}{Starting principle amount in early retirement bridge funds}

\item{contribution}{Total contribution to be split between retirement and bridge funds}

\item{age}{Current age}

\item{goal_annual_amount}{Goal amount to withdraw annually when retired (early and late)}

\item{constant_retirement}{A constant to adjust retirement returns}

\item{return_rate_retirement}{The rate of return of retirement funds}

\item{return_rate_bridge}{The rate of return of bridge funds, usually lower than retirement}

\item{constant_bridge}{A constant to adjust bridge returns}
}
\value{
A proportion of contributions that should go towards retirement
}
\description{
What proportion of contributions should go to retirement and early retirement
(bridge) funds?
}
\details{
Determined numerically with Newton's method. Assumes that reaching
early retirement goals occurs at the same time as reaching retirement goals.
Also assumes that annual withdrawals in early retirement and retirement are
the same.
}
