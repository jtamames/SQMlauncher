#' Read KEGG abundance table
#' @export
read_kegg <- function(project_path) {

  file <- file.path(project_path,
                    "results",
                    "13.Kegg",
                    "kegg_abundances.tsv")

  if (!file.exists(file)) {
    stop("KEGG results not found.")
  }

  read.delim(file, check.names = FALSE)
}

