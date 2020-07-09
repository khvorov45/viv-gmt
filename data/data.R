cat("Extract data for analysis")

library(tidyverse)

data_raw_dir <- here::here("data-raw")
data_dir <- here::here("data")

# Functions ===================================================================

save_csv <- function(data, name) {
  write_csv(data, file.path(data_dir, paste0(name, ".csv")))
  data
}

# Script ======================================================================

data <- readxl::read_excel(
  file.path(data_raw_dir, "GMT calculation.xlsx"),
  range = "A1:L59"
) %>%
  mutate(
    group_full = recode(
      Group_No,
      "1" = "Control", "2" = "Pre-arm",
      "3" = "Post-arm", "4" = "Pre-leg"
    ),
    group_binary = if_else(Group_No == "1", "Control", "Not control"),
    gender = recode(Gender, "1" = "Male", "2" = "Female")
  ) %>%
  select(id = ID, group_full, group_binary, gender, everything()) %>%
  select(-Group_No, -Gender, -Group_Bi) %>%
  mutate(across(c(-id, -group_full, -group_binary, -gender), as.character)) %>%
  pivot_longer(
    c(-id, -group_full, -group_binary, -gender),
    names_to = "virus", values_to = "titre"
  ) %>%
  mutate(
    titre = if_else(titre == "<10", "5", titre),
    titre = as.integer(titre),
    logtitre = log(titre),
    timepoint = if_else(str_detect(virus, "BL"), "Baseline", "1 Month"),
    virus = str_replace(virus, "_[B|1][L|M]$", "")
  ) %>%
  arrange(id) %>%
  save_csv("data")

data %>%
  select(-titre) %>%
  pivot_wider(names_from = "timepoint", values_from = "logtitre") %>%
  mutate(logtitre_diff = `1 Month` - Baseline) %>%
  select(-Baseline, -`1 Month`) %>%
  save_csv("data-diff")
