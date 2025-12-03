# Analysis of toxics data against IRR standards

This function compares toxics data against water quality standards for
irrigation (IRR) use. For more information, see the NMED Consolidated
Assessment and Listing Methodology (CALM) guidance manual.

## Usage

``` r
Toxics_IRR(Chem_table, Criteria_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- Criteria_table:

  Quality controlled criteria table from Data_Prep function.

## Value

A dataframe containing analyzed toxics data compared to IRR water
quality criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
df_Toxics_IRR <- Toxics_IRR(Chem_table = df_Chem_combined
, Criteria_table = df_Criteria)} # }
```
