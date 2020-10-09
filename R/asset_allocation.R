#' Allocate Assets
#'
#' @param total_amount numeric
#' @param annual_amount numeric
#' @param years_until_available numeric
#'
#' @return list
#' @export
#'
#' @examples
#' allocate_assets(100000, 20000, 10)
allocate_assets <- function(total_amount = numeric(), annual_amount = numeric(), years_until_available = numeric()) {

  amount_needed_until_available <- get_amount_needed_until_available(annual_amount, years_until_available)

  ratio_total_annual <- total_amount / annual_amount

  ratio_total_annual_asymptote <- get_ratio_total_annual_asymptote(years_until_available)

  ratio_total_annual_infinite <- 20

  years_until_available_asymptote <- get_years_until_available_asymptote(ratio_total_annual)

  is_infinite <- ratio_total_annual >= ratio_total_annual_asymptote | years_until_available >= years_until_available_asymptote

  percent_bonds_asymptote <- get_percent_bonds(14.5)

  amount_bonds_asymptote <- annual_amount * percent_bonds_asymptote

  if(is_infinite) {
    years_until_depleted <- Inf

    percent_bonds <- percent_bonds_asymptote - get_percent_bonds(years_until_available)

  } else {
    years_until_depleted <- get_years_until_depleted(ratio_total_annual, years_until_available)

    percent_bonds <- get_percent_bonds(years_until_depleted) - get_percent_bonds(years_until_available)
  }

  amount_bonds <- percent_bonds * annual_amount

  total_amount_infinite_min <- annual_amount * ratio_total_annual_asymptote

  annual_amount_infinite_max <- total_amount / ratio_total_annual_asymptote

  amount_bonds_infinite_max <- annual_amount_infinite_max * percent_bonds_asymptote

  model <-
    structure(
      list(
        total_amount = total_amount,
        annual_amount = annual_amount,
        years_until_available = years_until_available,
        amount_needed_until_available = amount_needed_until_available,
        ratio_total_annual = ratio_total_annual,
        ratio_total_annual_asymptote = ratio_total_annual_asymptote,
        years_until_available_asymptote = years_until_available_asymptote,
        is_infinite = is_infinite,
        ratio_total_annual_infinite = ratio_total_annual_infinite,
        years_until_depleted = years_until_depleted,
        total_amount_infinite_min = total_amount_infinite_min,
        annual_amount_infinite_max = annual_amount_infinite_max,
        percent_bonds = percent_bonds,
        amount_bonds = amount_bonds,
        amount_bonds_infinite_max = amount_bonds_infinite_max
      ),
      class = "model"
    )

  return(model)
}

get_amount_needed_until_available <- function(annual_amount, years_until_available) {
  annual_amount * (-20 * exp(-0.05 * years_until_available) + 20)
}

get_ratio_total_annual_asymptote <- function(years_until_available) {
  20 / exp(years_until_available) ^ (1/20)
}

get_years_until_available_asymptote <- function(ratio_total_annual) {
  20 * log(20 / ratio_total_annual)
}

get_years_until_depleted <- function(ratio_total_annual, years_until_available) {
  20 * log(-20 / (ratio_total_annual - 20 * exp(-years_until_available / 20)))
}

get_percent_bonds <- function(year) {
  # integral of 0.87-0.06y
  # equals zero at 14.5
  year_range <- min(max(0, year), 14.5)
  0.87*year_range - 0.03 * year_range^2
}
