cat("Plot the data")

library(tidyverse)

data_plot_dir <- here::here("data-plot")
data_table_dir <- here::here("data-table")

# Functions ===================================================================

source(file.path(data_dir, "read_data.R"))

save_plot <- function(plot, name, width, height) {
  ggdark::ggsave_dark(
    file.path(data_plot_dir, paste0(name, ".pdf")), plot,
    width = width, height = height, units = "cm"
  )
}

# Script ======================================================================

data <- read_data()

data_ext <- bind_rows(
  rename(data, group = group_full) %>% select(-group_binary),
  rename(data, group = group_binary) %>%
    select(-group_full) %>%
    filter(group == "Not control")
)

# Check that we didn't duplicate anything
stopifnot(
  nrow(data_ext) ==
    nrow(data) + nrow(filter(data, group_binary == "Not control"))
)

gmt <- read_csv(
  file.path(data_table_dir, "gmt-long.csv"),
  col_types = cols(
    group = col_factor(c("Control", "Pre-arm", "Pre-leg", "Not control"))
  )
)

gmt_plot <- data_ext %>%
  filter(!is.na(titre)) %>%
  ggplot(aes(timepoint, titre)) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  theme(
    strip.background = element_blank(),
    panel.spacing.x = unit("0", "null"),
    axis.text.x = element_text(angle = 40, hjust = 1),
    strip.placement = "outside",
    axis.title.x = element_blank()
  ) +
  facet_grid(virus ~ group, switch = "x") +
  scale_y_log10("Titre", breaks = 5 * 2^(0:10)) +
  geom_line(aes(group = id), color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.5, color = "gray50", shape = "x") +
  geom_errorbar(
    data = gmt,
    aes(timepoint, gmt, ymin = gmt_low, ymax = gmt_high),
    width = 0.5
  ) +
  geom_point(
    data = gmt,
    aes(timepoint, gmt),
    size = 2
  )

save_plot(gmt_plot, "gmt", 15, 15)
