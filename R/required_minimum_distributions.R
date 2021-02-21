#' Find Maximum Withdrawal
#'
#' Withdraws the maximum of required minimum distributions (RMDs) or a constant
#' withdrawal amount then grows the account by a constant growth rate.
#'
#' @param account_balance The starting account balance
#' @param rmds A vector of required minimum distributions (between 0 and 100)
#' @param constant_withdrawal A constant amount to withdraw each year
#' @param growth_rate An annual growth rate for the account balance
#'
#' @return
#' @export
#'
#' @examples
#' find_max_withdrawal(20:10)
find_max_withdrawal <- function(
  rmds, account_balance = 1, constant_withdrawal = 0, growth_rate = 1.03
) {
  max_withdrawal <- 0

  for(rmd in rmds) {
    withdrawal_rmd <- account_balance / rmd
    withdrawal <- max(withdrawal_rmd, constant_withdrawal)
    max_withdrawal <- max(max_withdrawal, withdrawal)
    account_balance <- account_balance %>%
      `-`(withdrawal) %>%
      `*`(growth_rate)
  }
  max_withdrawal
}

#' Find Constant Withdrawal Rate
#'
#' Finds a constant withdrawal rate that can be applied to a starting account
#' balance when required minimum distributions (RMDs) begin. This value enables
#' you to predict income from RMDs throughout retirement and keep the taxable
#' income below a goal tax threshold
#'
#' @param rmds A vector of required minimum distributions (0-100)
#' @param growth_rate A constant growth rate
#' @param increment The precision of the final withdrawal rate
#'
#' @return
#' @export
#'
#' @examples
#' find_constant_withdrawal_rate(20:10)
find_constant_withdrawal_rate <- function(
  rmds, growth_rate = 1.03, increment = 1e-04
) {
  under_rmds <- TRUE

  withdrawal_rate <- -increment

  while (under_rmds) {
    withdrawal_rate <- withdrawal_rate + increment

    max_withdrawal <- find_max_withdrawal(
      rmds, account_balance = 1, constant_withdrawal = withdrawal_rate,
      growth_rate = growth_rate
    )

    under_rmds <- withdrawal_rate < max_withdrawal
  }

  withdrawal_rate
}

#' Find the number of years possible with constant withdrawals
#'
#' Finds the number of years of constant withdrawals from a growing account
#' before the account reaches 0.
#'
#' @param withdrawal_amount Annual withdrawal amount
#' @param account_balance Starting account balance
#' @param growth_rate Annual growth rate
#'
#' @return
#' @export
#'
#' @examples
#' find_years_constant_withdrawals(0.04)
find_years_constant_withdrawals <- function(
  withdrawal_amount, account_balance = 1, growth_rate = 1.03
) {
  years <- 0
  while(account_balance > 0) {
    years <- years + 1
    account_balance <- (account_balance - withdrawal_amount) * growth_rate
  }
  years
}
