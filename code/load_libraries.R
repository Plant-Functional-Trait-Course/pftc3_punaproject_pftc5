### LIBRARIES ----
## CRAN packages
# custom function that checks whether package is installed and installs a package that isn't before loading it
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
# packages available at CRAN and needed here
package_vec <- c(
  "here",
  "remotes",
  "osfr",
  "readxl",
  "tidyverse",
  "lubridate",
  #"tpl",
  "janitor",
  "writexl",
  "vegan",
  "broom",
  "dataDownloader"
)
# install/load packages as needed
sapply(package_vec, install.load.package)

## non-CRAN packages
if("ggvegan" %in% rownames(installed.packages()) == FALSE){ # check if package is installed
  remotes::install_github("gavinsimpson/ggvegan")
}
library(ggvegan)

if("PFTCFunctions" %in% rownames(installed.packages()) == FALSE){ # check if package is installed
  remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
}
library(PFTCFunctions)

### HELPER FUNCTION ----
pn <- . %>% print(n = Inf)

