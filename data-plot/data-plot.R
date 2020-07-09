cat("Plot the data")

library(tidyverse)

data_plot_dir <- here::here("data-plot")
data_table_dir <- here::here("data-table")

# Functions ===================================================================

save_plot <- function(plot, name) {
  ggdark::ggsave_dark(
    file.path(data_plot_dir, "gmt.pdf"), plot,
    width = 15, height = 10, units = "cm"
  )
}

# Script ======================================================================

gmt <- read_csv(
  file.path(data_table_dir, "gmt-long.csv"),
  col_types = cols(
    group = col_factor(c("Control", "Pre-arm", "Pre-leg", "Not control"))
  )
)

gmt_plot <- gmt %>%
  ggplot(aes(group, gmt)) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  theme(
    strip.background = element_blank(),
    panel.spacing.x = unit("0", "null"),
    axis.text.x = element_text(angle = 40, hjust = 1)
  ) +
  facet_wrap(~virus, nrow = 2) +
  scale_y_log10("GMT", breaks = 5 * 2^(0:10)) +
  xlab("Group") +
  geom_errorbar(aes(ymin = gmt_low, ymax = gmt_high), width = 0.5) +
  geom_point()

save_plot(gmt_plot)
