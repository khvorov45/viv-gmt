cat("Make data tables")

library(tidyverse)
library(kableExtra)

data_table_dir <- here::here("data-table")
data_dir <- here::here("data")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

calc_gmts <- function(data, group_vars, titre_var, result_var_name) {
  r <- function(n) signif(n, 3)
  res <- rlang::sym(result_var_name)
  res_low <- rlang::sym(paste0(result_var_name, "_low"))
  res_high <- rlang::sym(paste0(result_var_name, "_high"))
  res_est <- rlang::sym(paste0(result_var_name, "_est"))
  data %>%
    filter(!is.na(!!rlang::sym(titre_var))) %>%
    group_by(!!!rlang::syms(group_vars)) %>%
    summarise(
      logmean = mean(!!rlang::sym(titre_var)),
      logmean_se = sd(!!rlang::sym(titre_var)) / sqrt(n()),
      !!res := exp(logmean),
      !!res_low := exp(logmean - qnorm(0.975) * logmean_se),
      !!res_high := exp(logmean + qnorm(0.975) * logmean_se),
      !!res_est := glue::glue(
        r(!!res), " (", r(!!res_low), ", ", r(!!res_high), ")"
      ),
      .groups = "drop"
    )
}

calc_all_gmts <- function(data,
                          group_vars = c("virus", "timepoint"),
                          titre_var = "logtitre",
                          result_var_name = "gmt") {
  notcontrol <- data %>%
    filter(group_binary == "Not control") %>%
    calc_gmts(c(group_vars, "group_binary"), titre_var, result_var_name) %>%
    rename(group = group_binary)

  four_groups <- data %>%
    calc_gmts(c(group_vars, "group_full"), titre_var, result_var_name) %>%
    rename(group = group_full)

  bind_rows(four_groups, notcontrol)
}

save_csv <- function(data, name) {
  write_csv(data, file.path(data_table_dir, paste0(name, ".csv")))
  data
}

save_csvs <- function(data, name) {
  res_est <- rlang::sym(paste0(name, "_est"))
  data %>%
    save_csv(glue::glue("{name}-long")) %>%
    select(virus, contains("timepoint"), Group = group, !!res_est) %>%
    pivot_wider(names_from = virus, values_from = !!res_est) %>%
    save_csv(glue::glue("{name}-wide-virus"))

  data %>%
    select(Virus = virus, contains("timepoint"), group, !!res_est) %>%
    pivot_wider(names_from = group, values_from = !!res_est) %>%
    save_csv(glue::glue("{name}-wide-group"))
}

# Script ======================================================================

data <- read_data()
data_diff <- read_data("data-diff")

all_gmts <- calc_all_gmts(data)
all_diff <- calc_all_gmts(data_diff, "virus", "logtitre_diff", "foldchange")

save_csvs(all_gmts, "gmt")
save_csvs(all_diff, "foldchange")

bind_rows(
  mutate(all_gmts, facet = paste(virus, timepoint, sep = "-")) %>%
    select(facet, group, value = gmt_est),
  mutate(all_diff, facet = paste(virus, "Fold change", sep = "-")) %>%
    select(facet, group, value = foldchange_est),
) %>%
  arrange(facet) %>%
  pivot_wider(names_from = "facet", values_from = "value") %>%
  save_csv("combined")
