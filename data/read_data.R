read_data <- function() {
  read_csv(
    "data/data.csv",
    col_types = cols(
      id = col_integer(),
      titre = col_integer(),
      group_full = col_factor(c("Control", "Pre-arm", "Post-arm", "Pre-leg")),
      group_binary = col_factor(c("Control", "Not control"))
    )
  )
}
