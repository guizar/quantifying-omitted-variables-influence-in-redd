# Unobserved confounders cannot explain over-crediting in avoided deforestation carbon projects

Code repository for the article: *[Unobserved confounders cannot explain over-crediting in avoided deforestation carbon projects](https://www.nature.com/articles/s41559-026-03049-7)*.

## Data

Available at https://osf.io/r9ygh

## Running the analyses 

1.  Make sure to unzip the `full_data_2024-08-11.zip` file in the `data` folder
2.  Head over to `code/00_wrapper.R`.
3.  Source scripts sequentially to run the full analysis.

Or use this in the R console:

``` r
source("code/00_wrapper.R")
```

------------------------------------------------------------------------

## Optional: Run the exploratory matching analysis

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

We will update this repo with a Docker image with standardised code, data and computing environment to reproduce the paper's analyses.
