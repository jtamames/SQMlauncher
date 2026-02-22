#' Check process status
#' @export
check_status <- function(proc) {
  if (proc$is_alive()) {
    "Running"
  } else {
    "Finished"
  }
}

#' Read process output
read_log <- function(proc) {
  paste(proc$read_output_lines(), collapse = "\n")
}

