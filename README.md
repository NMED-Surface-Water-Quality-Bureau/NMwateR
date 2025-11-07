# NMwateR
R package for water quality data management, analysis, and assessment. Used to support work conducted by the New Mexico Environment Department (NMED).

## Badges

<!-- badges: start -->

[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/NMED-Surface-Water-Quality-Bureau/NMwateR/graphs/commit-activity)
[![GitHub
license](https://img.shields.io/github/license/NMED-Surface-Water-Quality-Bureau/NMwateR)](https://github.com/NMED-Surface-Water-Quality-Bureau/NMwateR/blob/main/LICENSE)
[![GitHub
issues](https://img.shields.io/github/issues-raw/NMED-Surface-Water-Quality-Bureau/NMwateR)]((https://github.com/NMED-Surface-Water-Quality-Bureau/NMwateR/issues))
[![Github all
releases](https://img.shields.io/github/downloads/NMED-Surface-Water-Quality-Bureau/NMwateR/total)](https://github.com/NMED-Surface-Water-Quality-Bureau/NMwateR/releases)
<!-- badges: end -->

## Installation

To install the current version use the code below to install from
GitHub. The use of “force = TRUE” ensures the package is installed even
if already present. If the package `remotes` is missing the code below
will install it.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("NMED-Surface-Water-Quality-Bureau/NMwateR", force=TRUE)
```
[Describe vignette when completed]

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("NMED-Surface-Water-Quality-Bureau/NMwateR", force=TRUE, build_vignettes=TRUE)
```

## Purpose

NMwateR provides various functions that support water quality
analyses frequently undertaken by the NMED’s [Surface Water Quality Bureau Program](https://www.env.nm.gov/surface-water-quality/).

## Status

In development.
