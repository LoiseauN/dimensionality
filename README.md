# dimensionality

Research Compendium of the paper _Mouillot et al. (2021) The dimensionality and
structure of species trait spaces. Submitted to Ecology Letters_.



## Goal

- Quantify dimensionality of functional space
- Quantify robustness
- Compute cluster and unique species



## Content



This repository is structured as follow:

- :file_folder: &nbsp;[**data/**](https://github.com/loiseaun/dimensionality/tree/master/data):
contains all data required to reproduce analyses and figures

- :file_folder: &nbsp;[**R/**](https://github.com/loiseaun/dimensionality/tree/master/R):
contains R functions developed especially for this project

- :file_folder: &nbsp;[**man/**](https://github.com/loiseaun/dimensionality/tree/master/man):
contains documentation of R functions

- :file_folder: &nbsp;[**analyses/**](https://github.com/loiseaun/dimensionality/tree/master/analyses):
contains R scripts to reproduce all the analyses/figures

- :file_folder: &nbsp;[**outputs/**](https://github.com/loiseaun/dimensionality/tree/master/outputs):
contains all the results stored in the `.rds` format

- :page_facing_up: &nbsp;[**make.R**](https://github.com/loiseaun/dimensionality/tree/master/make.R):
master R script to run the entire project by calling each R script stored in the **analyses/** folder

- :page_facing_up: &nbsp;[**DESCRIPTION**](https://github.com/loiseaun/dimensionality/tree/master/DESCRIPTION):
contains project metadata (dependencies, etc.)



## Notes



- All required packages will be installed (if necessary)
- All required packages and R functions will be loaded
- Figures will be stored in `figures/`

:warning: Analyses require several cores and some computations take time.



## Usage

Clone the repository and run this command in R/RStudio:

```r
source("make.R")
```


Cheers!
