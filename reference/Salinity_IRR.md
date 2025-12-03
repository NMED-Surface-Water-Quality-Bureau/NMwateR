# Analysis of salinity data against IRR standards

This function compares salinity data against water quality standards for
irrigation (IRR) use. For more information, see the NMED Consolidated
Assessment and Listing Methodology (CALM) guidance manual.

## Usage

``` r
Salinity_IRR(Chem_table, DU_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

## Value

A dataframe containing analyzed salinity data compared to IRR water
quality criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
df_Salinity_IRR <- Salinity_IRR(Chem_table = df_Chem_combined
, DU_table = df_DU_processed)} # }
```
