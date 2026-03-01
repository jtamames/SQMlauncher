# R/builtin_profiles.R

get_builtin_profiles <- function() {
  list(
    list(
      name = "Standard Metagenome",
      description = "Balanced configuration",
      parameters = list(
        threads = 8,
        assembler = "megahit",
        mode = "coassembly",
        mapper= "bowtie",
        consensus = 50,
        assembly_options = "",
        skip_binning = FALSE
      )
    ),
    list(
      name = "Nanopore Metagenome",
      description = "For working with nanopore data",
      parameters = list(
        threads = 8,
        assembler = "canu",
        mode = "coassembly",
        mapper= "minimap2-ont",
        consensus = 20,
        assembly_options = "stopOnLowCoverage=2 minInputCoverage=0",
        skip_binning = FALSE
      )
    )
  )
}

get_profile_by_name <- function(name) {
  profiles <- get_builtin_profiles()
  # Buscamos el índice donde el nombre coincida exactamente
  idx <- which(sapply(profiles, function(p) p$name == name))
  
  if (length(idx) == 0) return(NULL)
  
  return(profiles[[idx[1]]])
}
