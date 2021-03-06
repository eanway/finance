% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_portfolio.R
\name{retirement_summary}
\alias{retirement_summary}
\title{Retirement summary}
\usage{
retirement_summary(
  year,
  age,
  goal_annual_amount,
  contribution,
  principle_retirement,
  principle_bridge,
  constant_retirement = 1.01,
  return_rate_retirement = 0.05,
  return_rate_bridge = 0.04
)
}
\arguments{
\item{year}{Current year}

\item{age}{Current age}

\item{goal_annual_amount}{Amount to withdraw in retirement}

\item{contribution}{Amount added to retirement or bridge accounts each year}

\item{principle_retirement}{Current amount in retirement accounts}

\item{principle_bridge}{Current amount in bridge (early retirement) accounts}

\item{constant_retirement}{Optional constant for retirement growth}

\item{return_rate_retirement}{Optional return rate for retirement growth}

\item{return_rate_bridge}{Optional return rate for bridge growth}
}
\value{

}
\description{
A text summary of (early) retirement prospects
}
