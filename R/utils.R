#' @importFrom readr read_csv
NULL

pmx_list <- function(){
  data <- read_csv(system.file("checklists/qc-checklists(pharmacometrics).csv", package = "ghqc"), show_col_types = FALSE)
  columns_to_keep <- !is.na(as.vector(data[1, ]))
  data <- data[, columns_to_keep]

  data_list <- lapply(data, function(col) {
    col <- as.character(col)
    col[!is.na(col)]
  })
}
