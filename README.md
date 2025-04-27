# Quantifying omitted variables influence in REDD+

Code and data repository for the article *Quantifying the potential influence of omitted variables is essential for robust causal inference: an illustration using the controversy concerning REDD+ project impacts*. 

**Data**

This project is hosted on the OSF platform: https://osf.io/r9ygh/

**Reproducing the analyses**

1. Clone this repo

1. Download the data [zip](https://osf.io/r9ygh/files/osfstorage/680251c8f9c5af4839d5655d) file. Create a new folder `quantifying-omitted-variables-influence-in-redd /data/full_data_2024-08-11/`;  place the contents of the zip file inside the newly created folder.

2. Edit the `00_wrapper.R` script: Before running this script, you'll need to specify the location of the project GitHub folder on your machine. This is done by defining the `PATH_FROM_HOME` variable. This variable indicates the directory where the GitHub folder is located, starting from either the HOME (Unix) or USERPROFILE (Windows) folder.

For example, if the project folder (`quantifying-omitted-variables-influence-in-redd`) is located in `home/Documents/Github`, you would define `PATH_FROM_HOME` as:

```R
PATH_FROM_HOME <- c('Documents', 'Github')
```

Note that the path must be defined as a character vector, following the `file.path` function convention.

3. Run the code: The code can be executed in the order provided in the `00_wrapper.R` file.

Please note: The script `02_exploratory_matching.R` is time-consuming to run as it executes multiple matching runs per project. It is not required to reproduce the main analysis in the paper, so it can be skipped.x
