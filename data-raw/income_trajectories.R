library(tidyr)
library(dplyr)
library(akima)
library(rgl)

get_percent <- function(name) {
  sub("pct_(.+)", "\\1", name) %>%
    as.numeric()
}

income_trajectories <- read.csv("data-raw/income_trajectories.csv") %>%
  pivot_longer(-age, values_to = "income") %>%
  mutate(percent = get_percent(name)) %>%
  select(age, percent, income) %>%
  bind_rows(data.frame(age = unique(.$age), percent = 0, income = 0)) %>%
  arrange(age, percent, income)

# Percent ####
loess_percent <- income_trajectories %>%
  loess(percent ~ age + income, data = ., span = 0.5)

persp(
  predict(
    loess_percent,
    expand.grid(
      age = unique(income_trajectories$age),
      income = seq(
        min(income_trajectories$income), max(income_trajectories$income), 10000
      )
    )
  ),
  xlab = "age", ylab = "income", zlab = "percent",
  theta = 60
)

loess_income <- loess(
  income ~ age + percent, data = income_trajectories, span = 0.5
)

persp(
  predict(loess_income, expand.grid(age = 18:65, percent = seq(0, 1, 0.1))),
  xlab = "age", ylab = "percent", zlab = "income",
  theta = 30
)

usethis::use_data(income_trajectories, overwrite = TRUE)
