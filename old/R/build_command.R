#' Build SqueezeMeta command
#' @export
build_sm_command <- function(project,
                             sample_file,
                             results_dir,
                             mode = "sequential",
                             threads = 8) {

  sm <- check_squeezemeta()

  if (!file.exists(sample_file)) {
    stop("Sample file not found.")
  }

 if (!dir.exists(input_dir)) {
    stop("Input directory (-f) does not exist.")
  }


  args <- c(
    "-m", mode,
    "-p", project,
    "-s", normalizePath(sample_file),
    "-f", normalizePath(input_dir),
    "-t", threads
  )

  paste(sm, paste(args, collapse = " "))
}

