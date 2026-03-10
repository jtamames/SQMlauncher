#!/bin/bash
# =============================================================================
# install_sqmlauncher.sh
# Installation script for SQMlauncher
#
# USAGE:
#   1. Activate your conda environment first:
#        conda activate SqueezeMeta   (or your environment name)
#   2. Then run this script:
#        bash install_sqmlauncher.sh
# =============================================================================

set -e  # Exit on any error

# ------------------------------------------------------------------------------
# Check that a conda environment is active
# ------------------------------------------------------------------------------
if [ -z "$CONDA_PREFIX" ]; then
    echo "ERROR: No conda environment is active."
    echo "Please activate your conda environment first, e.g.:"
    echo "   conda activate SqueezeMeta"
    exit 1
fi

CONDA_ENV_NAME=$(basename "$CONDA_PREFIX")
R_LIB_DIR="$CONDA_PREFIX/lib/R/library"
REPO_DIR="$CONDA_PREFIX/SQMlauncher"

echo "=============================================="
echo " SQMlauncher Installation"
echo "=============================================="
echo " Active conda environment : $CONDA_ENV_NAME"
echo " R library directory      : $R_LIB_DIR"
echo " Repository directory     : $REPO_DIR"
echo "=============================================="

# ------------------------------------------------------------------------------
# 1. Install R dependencies from conda-forge
#    (precompiled binaries -- avoids Perl/automake compilation issues)
# ------------------------------------------------------------------------------
echo ""
echo "[1/3] Installing R dependencies from conda-forge..."

# Use mamba if available (faster), fall back to conda on failure
PKGS="r-httpuv r-shiny r-shinyjs r-shinyfiles r-remotes"
if command -v mamba &> /dev/null; then
    echo "    mamba detected, trying mamba for faster installation..."
    if ! mamba install -y -c conda-forge $PKGS; then
        echo "    mamba failed, falling back to conda..."
        conda install -y -c conda-forge $PKGS
    fi
else
    echo "    mamba not found, using conda..."
    conda install -y -c conda-forge $PKGS
fi

# ------------------------------------------------------------------------------
# 2. Clone repo and install package
# ------------------------------------------------------------------------------
echo ""
echo "[2/3] Cloning and installing squeezeMetaR..."

# Clone or update the repository
if [ -d "$REPO_DIR" ]; then
    echo "    Repository already exists, updating..."
    git -C "$REPO_DIR" pull
else
    git clone https://github.com/jtamames/SQMlauncher.git "$REPO_DIR"
fi

# Remove problematic .RData file if present
if [ -f "$REPO_DIR/R/.RData" ]; then
    rm "$REPO_DIR/R/.RData"
    echo "    Removed problematic .RData file"
fi

# Install the package into the active conda environment's R library
Rscript -e "
  remotes::install_local(
    '$REPO_DIR',
    lib           = '$R_LIB_DIR',
    force         = TRUE,
    build         = TRUE,
    build_vignettes = FALSE,
    upgrade       = 'never'
  )
"

# ------------------------------------------------------------------------------
# 3. Configure ~/.Rprofile with sqm() launcher function
# ------------------------------------------------------------------------------
echo ""
echo "[3/3] Configuring ~/.Rprofile..."

RPROFILE="$HOME/.Rprofile"
MARKER="# >>> SQMlauncher >>>"

if ! grep -q "$MARKER" "$RPROFILE" 2>/dev/null; then
cat >> "$RPROFILE" << 'EOF'

# >>> SQMlauncher >>>
# Launches SQMlauncher loading the namespace correctly
sqm <- function() {
  library(squeezeMetaR)
  lazyLoad(
    file.path(find.package("squeezeMetaR"), "R", "squeezeMetaR"),
    envir = globalenv()
  )
  run_app()
}
# <<< SQMlauncher <<<
EOF
    echo "    ~/.Rprofile updated"
else
    echo "    ~/.Rprofile already configured, no changes made"
fi

# ------------------------------------------------------------------------------
# Done
# ------------------------------------------------------------------------------
echo ""
echo "=============================================="
echo " Installation completed successfully"
echo "=============================================="
echo ""
echo " To use SQMlauncher, open R and run:"
echo ""
echo "   sqm()"
echo ""
echo " Or manually:"
echo ""
echo "   library(squeezeMetaR)"
echo "   lazyLoad(file.path(find.package('squeezeMetaR'), 'R', 'squeezeMetaR'), envir = globalenv())"
echo "   run_app()"
echo ""
