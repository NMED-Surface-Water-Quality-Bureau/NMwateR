# Analysis of conventionals data against LW standards

This function compares conventional water chemistry data against water
quality standards for livestock watering (LW) use. For more information,
see the NMED Consolidated Assessment and Listing Methodology (CALM)
guidance manual.

## Usage

``` r
Conventionals_LW(Chem_table, DU_table, Criteria_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

- Criteria_table:

  Quality controlled criteria table from Data_Prep function.

## Value

A dataframe containing analyzed conventionals data compared to LW water
quality criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
df_Conventionals_LW <- Conventionals_LW(Chem_table = df_Chem_combined
, DU_table = df_DU_processed
, Criteria_table = df_Criteria)} # }
```
