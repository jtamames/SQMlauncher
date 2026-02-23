#' Build SqueezeMeta command
#' @export
build_sm_command <- function(program,
                             project,
                             sample_file,
                             input_dir,
                             mode = "sequential",
                             threads = 8,
                             run_trimmomatic = FALSE) {

  # Detect executable
  exe <- switch(program,
                "SqueezeMeta.pl" = check_squeezemeta(),
                "sqm_reads.pl" = "sqm_reads.pl",
                "sqm_longreads.pl" = "sqm_longreads.pl",
                stop("Unknown program selected")
  )

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

  if (run_trimmomatic && program == "SqueezeMeta.pl") {
    args <- c(args, "--cleaning")
  }

  paste(exe, paste(args, collapse = " "))
}
