# Required packages
required_packages <- c(
  "tidyverse", "dplyr", "knitr", "magrittr", "doParallel", 
  "ggplot2", "qqplotr", "broom", "arrangements", "pander", 
  "rcartocolor", "patchwork", "ggbreak", "boot", "randomForest", 
  "MatchIt", "nlme", "scales", "sf", "rnaturalearth", 
  "rnaturalearthdata", "countrycode", "lwgeom"
)

# Install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
}


install_if_missing(required_packages)


# List package versions
package_versions <- sapply(required_packages, function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    as.character(packageVersion(pkg))
  }
})

# Print package versions
print(package_versions)