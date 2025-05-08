# Quantifying omitted variables influence in REDD+

This repository accompanies the article: *Quantifying the potential influence of omitted variables is essential for robust causal inference: an illustration using the controversy concerning REDD+ project impacts*.

## ⚙️ Prerequisites

Before proceeding, ensure the following are installed:

-   [RStudio](https://posit.co/download/rstudio-desktop/) (recommended) — integrated development environment for running and managing this project
-   [Git](https://git-scm.com/downloads) — for cloning the repository
-   [Rtools](https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html) — **Windows only**, required for compiling packages from source

------------------------------------------------------------------------

### Step 1: Ensure R 4.5.0 is Installed

This project requires **R version 4.5.0** for compatibility with the `renv` environment.

#### Check your R version

If you already have R installed, run:

``` r
R.version.string
```

If it does not show version `4.5.0`, follow the instructions below.

#### Install R 4.5.0 using rig

[`rig`](https://github.com/r-lib/rig) is a cross-platform R version manager maintained by the R community.

Follow the official installation instructions here:\
👉 <https://github.com/r-lib/rig>

After installing `rig`, open a terminal (Mac, Linux) or PowerShell (Windows) and run the following to install R 4.5.0 and set it as system default, :

``` bash
rig add 4.5.0
rig default 4.5.0
```

Now restart RStudio to ensure it uses R 4.5.0. In RStudio, go to:\
`Tools > Global Options > General > R version`, and confirm it is using the correct version.

------------------------------------------------------------------------

### Step 2: Clone the Repository

Open a terminal appropriate to your system:

-   **Windows**: Git Bash, PowerShell, or Command Prompt
-   **macOS/Linux**: Your system's default terminal

Navigate to your home directory (or another writable directory of your choice):

``` bash
cd ~
```

Optionally, create a folder for your projects:

``` bash
mkdir -p projects
cd projects
```

Then clone the repository:

``` bash
git clone https://github.com/guizar/quantifying-omitted-variables-influence-in-redd.git
```

Your cloned repository is now at:

```         
~/projects/quantifying-omitted-variables-influence-in-redd
```

------------------------------------------------------------------------

### Step 3: Download Data

Project data is hosted on the [OSF platform](https://osf.io/r9ygh/).

1.  Download the data archive:\
    [full_data_2024-08-11.zip](https://osf.io/r9ygh/files/osfstorage/680251c8f9c5af4839d5655d)

2.  Create the directory to hold the data:

``` bash
cd ~/projects/quantifying-omitted-variables-influence-in-redd
mkdir data/full_data_2024-08-11
```

3.  Extract the **contents** of the downloaded zip file (`full_data_2024-08-11.zip`) into the `data/full_data_2024-08-11/` directory. Make sure the files are directly inside `full_data_2024-08-11/`, not in a nested folder. You should see paths like:

```         
~/projects/quantifying-omitted-variables-influence-in-redd/data/full_data_2024-08-11/844_PER.csv
```

------------------------------------------------------------------------

### Step 4: Open the Project in RStudio (Recommended)

For reproducibility and ease of use across all platforms (Windows, macOS, Linux):

1.  Open **RStudio**.
2.  Go to `File > Open Project` and open the `.Rproj` file located in the root of this repository (e.g., `quantifying-omitted-variables-influence-in-redd.Rproj`).
3.  This will automatically set the working directory to the project root, enabling all scripts to run with **relative paths**.

If you're not using RStudio, you can manually navigate to the cloned directory in your terminal and start R from there:

``` bash
cd ~/projects/quantifying-omitted-variables-influence-in-redd
R
```

------------------------------------------------------------------------

### Step 5: Set Up the Project Environment with `renv`

This project uses [`renv`](https://rstudio.github.io/renv/) for reproducible package management.

To initialize the correct package environment:

``` r
install.packages("renv")
renv::restore()
```

This will install all required packages as recorded in the `renv.lock` file.

------------------------------------------------------------------------

### Step 6: Run the Analysis

Once the environment is set up, run:

``` r
source("00_wrapper.R")
```

This wrapper script runs all required analysis scripts in sequence.

------------------------------------------------------------------------

### Optional: Run Exploratory Matching Analysis

By default, the exploratory matching script (`02_exploratory_matching.R`) is **not run** because it is computationally intensive and **not required** to reproduce the core results of the paper.

To include it in your analysis, open `00_wrapper.R` and modify the following line:

``` r
run_exploratory_matching <- FALSE
```

Change it to:

``` r
run_exploratory_matching <- TRUE
```
