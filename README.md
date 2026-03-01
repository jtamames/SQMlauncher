# squeezeMetaR

R/Shiny interface to run and monitor SqueezeMeta locally.

Installation Guide: SQMlauncher (squeezeMetaR)
This guide provides step-by-step instructions to set up the environment and install SQMlauncher, the graphical user interface for the SqueezeMeta metagenomics pipeline.
1. Prerequisites
Before installing the application, ensure your system meets the following requirements:
R Installation: Download and install the latest version of R from CRAN.
RStudio (Recommended): For an optimal experience, install RStudio Desktop.
SqueezeMeta: Since this app is a launcher, the SqueezeMeta suite must be installed on your system and available in your system's PATH.
Development Tools: You will need the devtools package to install the application directly from GitHub.
Open your R console or RStudio and run:
install.packages("devtools")

2. Installation
The application is maintained as an R package named squeezeMetaR. You can install the latest version from the official repository using the following command:
# Install from GitHub
devtools::install_github("jtamames/SQMlauncher")

During installation, R may ask to update existing dependencies. It is recommended to update them to ensure compatibility with the Shiny environment.
3. Launching the Application
Once the installation is complete, you can start the GUI by loading the library and calling the launch function.
Load the library:
library(squeezeMetaR)

2.  **Start the app**:
    ```r
run_app()

The interface will open in your default web browser or within the RStudio Viewer pane.
4. Configuration and Usage Notes
Path Verification: Upon launching, the application checks for the existence of SqueezeMeta.pl. Ensure your environment variables are correctly configured if the program is not detected.
Execution Profiles: The launcher includes pre-defined profiles:
Standard Metagenome: Default settings for Illumina/short-read data.
Nanopore Metagenome: Optimized settings for long-read data (automatically adjusts consensus and assembly parameters).
Real-time Monitoring: The application utilizes the processx and shinyjs packages to provide live log updates and non-blocking execution of the pipeline.
5. Troubleshooting
If the application fails to start or the "Run" button does not respond:
Verify that all dependencies are installed by running devtools::install_github("jtamames/SQMlauncher") again.
Check the R console for error messages.
Ensure you have write permissions in the selected Working Directory within the app.
