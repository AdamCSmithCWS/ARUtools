
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ARUtools <img src="inst/figures/ARUtools.png" align="right" width="120" />

<!-- badges: start -->
<!-- badges: end -->

The goal of ARUtools is to facilitate the processing of ARU data. It is
very much a development version now.

## Installation

You can install the development version of ARUtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("dhope/ARUtools")
```

## Overview

Currently ARUtools can perform 3 main functions:

### Process metadata

For a given folder structure of ARU recordings from BarLT or Songmeters,
the `clean_metadata` function is meant to process the files, extract
locations, and calculate the time to sunrise and sunset.

``` r
library(ARUtools)
path_to_folder <- "~/path/to/ARU/data/SM4"
cleaned_metadata <- clean_metadata(type = "SM4", folder_base = "path_to_folder")
```

### Select samples
