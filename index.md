# NMwateR

NMwaterR is an R package for water quality data management, analysis,
and assessment. These functions support Integrated Reporting conducted
by the New Mexico Environment Department (NMED) [Surface Water Quality
Bureau Program](https://www.env.nm.gov/surface-water-quality/).

The package is intended to make assessment analyses faster and more
consistent. The work starts with loading the necessary files from SQUID
(NMED’s in-house databse) into
[`Data_Prep()`](https://nmed-surface-water-quality-bureau.github.io/NMwateR/reference/Data_Prep.md).
Next comes \>10 water quality analysis functions depending on the
parameters and designated uses being assessed. Outputs from these
analyses are input into the
[`assessment()`](https://nmed-surface-water-quality-bureau.github.io/NMwateR/reference/assessment.md)
function to make initial Integrated Reporting conclusions.

## Badges

## Installation

To install the current version use the code below to install from
GitHub. The use of “force = TRUE” ensures the package is installed even
if already present. If the package `remotes` is missing the code below
will install it.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("NMED-Surface-Water-Quality-Bureau/NMwateR", force=TRUE)
```

A vignette is included in the package that is an example assessment
workflow using real NMED data. To build the vingette, the
`build_vignettes = TRUE` argument needs to be included. Copy and paste
the code below to install the package with the vignette. Once installed,
the vignette can be accessed either by running `?NMwateR` in the RStudio
console and navigating via the Help menu or by running
`browseVignettes("NMwateR")`.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("NMED-Surface-Water-Quality-Bureau/NMwateR", force=TRUE, build_vignettes=TRUE)
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on the
[Issues](https://github.com/NMED-Surface-Water-Quality-Bureau/NMwateR/issues)
page. For questions and other discussion, please reach out to Meredith
Zeigler (NMED; <Meredith.Zeigler@env.nm.gov>) or Benjamin Block (Tetra
Tech; <Ben.Block@tetratech.com>).

## Status

In development.
