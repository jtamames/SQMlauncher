#' Run SqueezeMeta locally using processx
#'
#' @param samples_file Path to samples file (-s)
#' @param input_dir Directory with input reads (-f)
#' @param project_name Project name (-p)
#' @param workdir Working directory where project folder will be created
#' @param mode Execution mode (-m)
#' @param threads Number of threads (-t)
#'
#' @return A list containing:
#'   - process: processx object
#'   - log_file: path to log file
#' @export

run_squeezemeta <- function(program,
                            samples_file,
                            input_dir,
                            project_name,
                            workdir,
                            mode = "coassembly",
                            threads = 8,
                            run_trimmomatic = FALSE,
                            assembler = "megahit",
                            mapper = "bowtie",
                            mapping_options = NULL) {

  # -----------------------------
  # Validation
  # -----------------------------

  if (!file.exists(samples_file)) {
    stop("Samples file does not exist")
  }

  if (!dir.exists(input_dir)) {
    stop("Input directory does not exist")
  }

  if (!dir.exists(workdir)) {
    stop("Working directory does not exist")
  }

  if (project_name == "") {
    stop("Project name cannot be empty")
  }

  project_path <- file.path(workdir, project_name)

  if (dir.exists(project_path)) {
    stop("Project directory already exists. Please choose another project name.")
  }

  # -----------------------------
  # Locate SqueezeMeta
  # -----------------------------

  sm <- Sys.which(program)

  if (sm == "") {
    stop(paste(program, "not found in PATH"))
  }

  # -----------------------------
  # Build arguments
  # -----------------------------

  args <- c(
    "-s", normalizePath(samples_file),
    "-f", normalizePath(input_dir),
    "-p", project_name,
    "-t", as.character(threads)
  )
  
  # Solo añadir -m si es SqueezeMeta
  if (program == "SqueezeMeta.pl") {
    args <- c(args, "-m", mode)
  }
  
  if (program == "SqueezeMeta.pl" && run_trimmomatic) {
    args <- c(args, "--cleaning")
  }

  if (program == "SqueezeMeta.pl" && !is.null(assembler) && assembler != "") {
    args <- c(args, "-a", assembler)
  }
  
  if (!is.null(mapper) && mapper != "") {
  args <- c(args, "-map", mapper)
 }

 # Mapping options
 
 if (!is.null(mapping_options) && mapping_options != "") {
   args <- c(args, "-mapping_options", mapping_options)
 }
  
  # -----------------------------
  # Prepare log file
  # -----------------------------

  log_file <- file.path(workdir, paste0(project_name, "_run.log"))

  # Create empty log file
  file.create(log_file)

  message("--------------------------------------------------")
  message("Running SqueezeMeta")
  message("Working directory: ", normalizePath(workdir))
  message("Log file: ", log_file)
  message("Command:")
  message(sm, " ", paste(args, collapse = " "))
  message("--------------------------------------------------")

  # -----------------------------
  # Launch process
  # -----------------------------

  p <- processx::process$new(
    command = sm,
    args = args,
    stdout = "|",
    stderr = "|",
    supervise = TRUE,
    wd = normalizePath(workdir)
  )

  return(list(
    process = p,
    log_file = log_file
  ))
}

