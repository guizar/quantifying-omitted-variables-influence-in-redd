# Quantifying Omitted Variables Influence in REDD+

This project accompanies the article: *Unobserved confounders cannot explain over-crediting in avoided deforestation carbon projects*.

## Code 

All the analytical steps, from selecting controls from the pool of observations to the final analyses, are provided at:
https://github.com/guizar/quantifying-omitted-variables-influence-in-redd/

## Data

Available at https://osf.io/r9ygh

## Running the Analysis

1.  Make sure to unzip the `full_data_2024-08-11.zip` file in the `data` folder
2.  In the **Files** pane (bottom left), click on the folder called `code`.
2.  Click on the file `00_wrapper.R`.
3.  Click `Source` (top-right of the editor) to run the full analysis.

Or use this in the R console:

``` r
source("code/00_wrapper.R")
```

------------------------------------------------------------------------

## Optional: Run the Exploratory Matching Analysis

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

## Reproducible code + data with Docker

We will update this repo with a Docker image with standardised code, data and computing environment to reproduce the paper's analyses
