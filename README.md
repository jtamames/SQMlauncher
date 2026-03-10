# SQMlauncher

R/Shiny interface to run and monitor [SqueezeMeta](https://github.com/jtamames/SqueezeMeta) pipelines (SqueezeMeta, SQM_reads, SQM_longreads) locally from your browser.

---

## Requirements

- [SqueezeMeta](https://github.com/jtamames/SqueezeMeta) installed via conda and available in your PATH
- The conda environment for SqueezeMeta must be active before installing or running the app

---

## Installation

We provide an installation script that handles all dependencies automatically. It uses `mamba` if available, falling back to `conda` otherwise.

**1. Activate your SqueezeMeta conda environment:**

```bash
conda activate SqueezeMeta   # or your environment name
```

**2. Download and run the installation script:**

```bash
bash install_sqmlauncher.sh
```

The script will:
- Install all required R/Shiny dependencies from conda-forge (precompiled binaries)
- Clone this repository into your conda environment
- Install the `squeezeMetaR` R package
- Add a convenience `sqm()` function to your `~/.Rprofile`

### Manual installation

If you prefer to install manually, first install the Shiny dependencies from conda-forge:

```bash
conda install -c conda-forge r-httpuv r-shiny r-shinyjs r-shinyfiles r-remotes
```

Then install the package from R:

```r
remotes::install_local("/path/to/SQMlauncher/", 
                       force = TRUE, build = TRUE, 
                       build_vignettes = FALSE, upgrade = "never")
```

---

## Running the app

If you used the installation script, simply open R and run:

```r
sqm()
```

Or manually:

```r
library(squeezeMetaR)
lazyLoad(file.path(find.package("squeezeMetaR"), "R", "squeezeMetaR"), envir = globalenv())
run_app()
```

The interface will open in your default web browser.

---

## Features

- **Execution profiles** — pre-defined profiles for common use cases:
  - *Standard Metagenome*: default settings for Illumina/short-read data
  - *Nanopore Metagenome*: optimized settings for long-read data, with automatic adjustment of consensus and assembly parameters
- **Real-time monitoring** — live log updates and non-blocking pipeline execution via `processx` and `shinyjs`
- **Path verification** — checks for `SqueezeMeta.pl` on launch and warns if the environment is not correctly configured

---

## Troubleshooting

**The app fails to start or the Run button does not respond:**
- Make sure your SqueezeMeta conda environment is active
- Verify all dependencies are installed by re-running `install_sqmlauncher.sh`
- Check the R console for error messages
- Ensure you have write permissions in the Working Directory selected in the app

**R cannot find `run_app` after `library(squeezeMetaR)`:**

This is a known issue in some conda environments where the package namespace does not load correctly. Use the `lazyLoad` workaround shown above, or reinstall using the provided script which configures this automatically via `~/.Rprofile`.
