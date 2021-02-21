library(tidyr)
library(dplyr)
library(akima)
library(rgl)
library(tibble)
library(mgcv)
library(plotly)
library(car)
library(scam)

get_percent <- function(name) {
  sub("pct_(.+)", "\\1", name) %>%
    as.numeric()
}

income_trajectories <- read.csv("data/income_trajectories.csv") %>%
  pivot_longer(-age, values_to = "income") %>%
  mutate(percent = get_percent(name)) %>%
  select(age, percent, income) %>%
  bind_rows(data.frame(age = unique(.$age), percent = 0, income = 0)) %>%
  arrange(age, percent, income)

# Test ####

# Percent ####
scatter3d(
  x = income_trajectories$age, y = income_trajectories$income,
  z = income_trajectories$percent,
  xlab = "age", ylab = "income", zlab = "percent",
  fit = "smooth", model.summary = TRUE
)

# Income ####
test_income <- income_trajectories %>%
  pivot_wider(names_from = percent, values_from = income) %>%
  column_to_rownames("age") %>%
  as.matrix()

persp(
  test_income,
  xlab = "age", ylab = "percent", zlab = "income",
  theta = 30,
  ticktype = "detailed"
)

# Loess ####

# Percent ####
loess_percent <- income_trajectories %>%
  loess(percent ~ age + income, data = ., span = 0.15, degree = 1)

loess_percent

df_percent <- expand.grid(
  age = unique(income_trajectories$age),
  income = seq(
    min(income_trajectories$income), max(income_trajectories$income), 10000
  )
) %>%
  arrange(age, income)

persp(
  predict(
    loess_percent,
    df_percent
  ),
  xlab = "age", ylab = "income", zlab = "percent",
  theta = 60,
  ticktype = "detailed"
)

# Income ####
loess_income <- loess(
  income ~ age + percent, data = income_trajectories, span = 0.1, degree = 1
)

summary(loess_income)

persp(
  predict(loess_income, expand.grid(age = 18:65, percent = seq(0, 1, 0.1))),
  xlab = "age", ylab = "percent", zlab = "income",
  theta = 30,
  ticktype = "detailed"
)

# GAM ####

# Percent ####
gam_percent <- income_trajectories %>%
  gam(percent ~ s(age, income), data = .)

summary(gam_percent)

persp(
  z = matrix(
    predict(gam_percent), ncol = length(unique(income_trajectories$age))
  ),
  xlab = "income", ylab = "age", zlab = "percent",
  theta = -30,
  ticktype = "detailed"
)

income_percentile <- predict(gam_percent, data.frame(age = 29, income = 60900))

income_percentile

# Income ####
gam_income <- income_trajectories %>%
  gam(income ~ te(age, percent), data = .)

summary(gam_income)

persp(
  z = matrix(
    predict(gam_income), ncol = length(unique(income_trajectories$age))
  ),
  xlab = "income", ylab = "age", zlab = "percent",
  theta = -30,
  ticktype = "detailed"
)

predict(gam_income, data.frame(age = 29, percent = income_percentile))

# Scam ####

# Percent ####
scam_percent <- income_trajectories %>%
  filter(percent != 0, percent != 0.99) %>%
  scam(percent ~ s(age, income, bs = "tesmi2"), data = .)

summary(scam_percent)

smooth_percent_by_income_age <- scam_percent

persp(
  z = matrix(
    predict(scam_percent), ncol = length(unique(income_trajectories$age))
  ),
  xlab = "income", ylab = "age", zlab = "percent",
  theta = -30,
  ticktype = "detailed"
)

income_percentile <- predict(scam_percent, data.frame(age = 29, income = 60900))

income_percentile

# Income ####
scam_income <- income_trajectories %>%
  filter(percent != 0, percent != 0.99) %>%
  scam(income ~ s(age, percent, bs = "tesmi2"), data = .)

summary(scam_income)

smooth_income_by_percent_age <- scam_income

persp(
  z = matrix(
    predict(scam_income), ncol = length(unique(income_trajectories$age))
  ),
  xlab = "percent", ylab = "age", zlab = "income",
  theta = -30,
  ticktype = "detailed"
)

predict(scam_income, data.frame(age = 29, percent = income_percentile))

plot(predict(scam_income, data.frame(age = 29:65, percent = income_percentile)))

plot(predict(scam_income, data.frame(age = 29, percent = seq(0, 1, 0.1))))

# Export ####
usethis::use_data(income_trajectories, overwrite = TRUE)

usethis::use_data(smooth_percent_by_income_age, overwrite = TRUE)

usethis::use_data(smooth_income_by_percent_age, overwrite = TRUE)
