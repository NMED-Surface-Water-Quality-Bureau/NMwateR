# Analysis of toxics data against LW standards

This function compares toxics data against water quality standards for
livestock watering (LW) use. For more information, see the NMED
Consolidated Assessment and Listing Methodology (CALM) guidance manual.

## Usage

``` r
Toxics_LW(Chem_table, Criteria_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- Criteria_table:

  Quality controlled criteria table from Data_Prep function.

## Value

A dataframe containing analyzed toxics data compared to LW water quality
criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
df_Toxics_LW <- Toxics_LW(Chem_table = df_Chem_combined
, Criteria_table = df_Criteria)} # }
```
