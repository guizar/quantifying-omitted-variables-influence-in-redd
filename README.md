# Quantifying Omitted Variables Influence in REDD+

This project accompanies the article: *Quantifying the potential influence of omitted variables is essential for robust causal inference: an illustration using the controversy concerning REDD+ project impacts*.

## 🚀 Quick Start with Docker (All Platforms)

This setup works on **Windows**, **macOS**, and **Linux**.

------------------------------------------------------------------------

### ✅ Step 1: Install Docker

If you haven't already, install Docker from the official website:

👉 <https://docs.docker.com/get-docker/>

-   On **Windows**, install "Docker Desktop".
-   After installation, **restart your computer** if prompted.

------------------------------------------------------------------------

### ✅ Step 2: Start Docker

Before running any commands:

-   On **Windows**, **you must open Docker Desktop** manually.
    -   Click the Start Menu, search for **Docker Desktop**, and open it.
    -   Wait until the Docker whale icon in the system tray says "**Docker is running**".
-   On **macOS/Linux**, Docker usually starts automatically, but confirm it is running if unsure.

------------------------------------------------------------------------

### ✅ Step 3: Open a Terminal or Command Line

-   On **Windows**: Open **Command Prompt** or **PowerShell** (press `Win + R`, type `cmd`, or search for PowerShell).
-   On **macOS/Linux**: Open the **Terminal** app.

------------------------------------------------------------------------

### ✅ Step 4: Run These Commands

Copy and paste each of the following lines into your terminal:

``` bash
docker pull deforestationreview/deforestation_image:anon
```

``` bash
docker run -d -p 8787:8787 --name deforestation_container -e USER=rstudio -e PASSWORD=bayes deforestationreview/deforestation_image:anon
```

> 💡 The first command downloads the image. The second starts the container and opens RStudio in your browser.

------------------------------------------------------------------------

### ✅ Step 5: Access RStudio

Open your browser and go to:

👉 <http://localhost:8787>

You now have a fully configured RStudio environment with all dependencies pre-loaded.

------------------------------------------------------------------------

## ▶️ Running the Analysis

Once logged into RStudio:

1.  In the **Files** pane (bottom left), click on the folder called `code`.
2.  Click on the file `00_wrapper.R`.
3.  Click `Source` (top-right of the editor) to run the full analysis.

Or use this in the R console:

``` r
source("code/00_wrapper.R")
```

------------------------------------------------------------------------

## ⚙️ Optional: Run the Exploratory Matching Analysis

The script `02_exploratory_matching.R` is **not run by default** because it is computationally intensive.

To include it:

1.  Open `code/00_wrapper.R`
2.  Change:

``` r
run_exploratory_matching <- FALSE
```

to:

``` r
run_exploratory_matching <- TRUE
```

3.  Save the file, then rerun:

``` r
source("code/00_wrapper.R")
```

------------------------------------------------------------------------

You're all set!

If you encounter issues, consult the [Docker Get Started Guide](https://docs.docker.com/get-started/).
