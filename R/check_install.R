#' Check SqueezeMeta installation
#' @export
check_squeezemeta <- function() {
  sm <- Sys.which("SqueezeMeta.pl")
  if (sm == "") {
    stop("SqueezeMeta.pl not found in PATH. Install SqueezeMeta and add it to PATH.")
  }
  return(sm)
}

