cat("Make data tables")

library(tidyverse)
library(kableExtra)

data_table_dir <- here::here("data-table")
data_dir <- here::here("data")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

calc_gmts <- function(data, group_var) {
  r <- function(n) signif(n, 3)
  data %>%
    filter(!is.na(titre)) %>%
    group_by(virus, !!rlang::sym(group_var)) %>%
    summarise(
      logmean = mean(log(titre)),
      logmean_se = sd(log(titre)) / sqrt(n()),
      gmt = exp(logmean),
      gmt_low = exp(logmean - qnorm(0.975) * logmean_se),
      gmt_high = exp(logmean + qnorm(0.975) * logmean_se),
      gmt_est = glue::glue(
        "{r(gmt)} ({r(gmt_low)}, {r(gmt_high)})"
      ),
      .groups = "drop"
    )
}

save_csv <- function(data, name) {
  write_csv(data, file.path(data_table_dir, paste0(name, ".csv")))
  data
}

# Script ======================================================================

data <- read_data()

notcontrol <- data %>%
  filter(group_binary == "Not control") %>%
  calc_gmts("group_binary") %>%
  rename(group = group_binary)

four_groups <- data %>%
  calc_gmts("group_full") %>%
  rename(group = group_full)

all_gmts <- bind_rows(four_groups, notcontrol)

all_gmts %>%
  save_csv("gmt-long") %>%
  select(virus, Group = group, gmt_est) %>%
  pivot_wider(names_from = virus, values_from = gmt_est) %>%
  save_csv("gmt-wide-virus")

all_gmts %>%
  select(Virus = virus, group, gmt_est) %>%
  pivot_wider(names_from = group, values_from = gmt_est) %>%
  save_csv("gmt-wide-group")
