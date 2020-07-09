read_data <- function(name = "data") {
  suppressWarnings(
    read_csv(
      glue::glue("data/{name}.csv"),
      col_types = cols(
        id = col_integer(),
        titre = col_integer(),
        group_full = col_factor(c("Control", "Pre-arm", "Post-arm", "Pre-leg")),
        group_binary = col_factor(c("Control", "Not control")),
        timepoint = col_factor(c("Baseline", "1 Month"))
      )
    )
  )
}
