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
                            cleaning_parameters = NULL,
                            assembler = "megahit",
                            assembly_options = NULL,
                            min_contig_length = 200,
                            use_singletons = FALSE,
                            no_cog = FALSE,
                            no_kegg = FALSE,
                            no_pfam = TRUE,
                            eukaryotes = FALSE,
                            doublepass = FALSE,
                            extdb = NULL,
                            mapper = "bowtie",
                            mapping_options = NULL,
                            no_bins = FALSE,
		            only_bins = FALSE,
                            binners = "concoct,metabat2"
                            ) {

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
  
  if (no_cog) {
    args <- c(args, "--nocog")
  }

  if (no_kegg) {
    args <- c(args, "--nokegg")
  }

  if (no_pfam) {
    args <- c(args, "--nopfam")
  }

  if (eukaryotes) {
    args <- c(args, "--euk")
  }

  if (doublepass) {
    args <- c(args, "--D")
  }

  if (!is.null(extdb) && extdb != "") {
    args <- c(args, "-extdb", extdb)
  }

  # Solo añadir -m si es SqueezeMeta
  if (program == "SqueezeMeta.pl") {
    args <- c(args, "-m", mode)
  }
  
  if (program == "SqueezeMeta.pl" && run_trimmomatic) {
    args <- c(args, "--cleaning")
      if (!is.null(cleaning_parameters) && cleaning_parameters != "") {
     args <- c(args, "-cleaning_options", cleaning_parameters)
  }

  }

  if (program == "SqueezeMeta.pl" && !is.null(assembler) && assembler != "") {
    args <- c(args, "-a", assembler)
  }
  
   # Assembly options
  if (!is.null(assembly_options) && assembly_options != "") {
    args <- c(args, "-assembly_options", assembly_options)
  }

  # Min contig length
  if (!is.null(min_contig_length)) {
    args <- c(args, "-c", as.character(min_contig_length))
  }

  # Singletons
  if (use_singletons) {
    args <- c(args, "--singletons")
 
    if (!is.null(mapper) && mapper != "") {
    args <- c(args, "-map", mapper)
   }
    }

 # Mapping options
 
 if (!is.null(mapping_options) && mapping_options != "") {
   args <- c(args, "-mapping_options", mapping_options)
 }
  
 # ---- BINNING ----

if (no_bins) {
  args <- c(args, "--nobins")

} else {

  if (only_bins) {
    args <- c(args, "--onlybins")
  }

  if (!is.null(binners) && length(binners) > 0) {
    args <- c(args, "-binners", paste(binners, collapse = ","))
  }
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

