df_rmds <- read.csv("data/rmds.csv")

rmds <- df_rmds$required_minimum_distributions

usethis::use_data(rmds, overwrite = TRUE)
