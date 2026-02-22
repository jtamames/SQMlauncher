#' Create SqueezeMeta project directory
create_project <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  normalizePath(path)
}

