# Analysis of conventionals data against ALU standards

This function compares conventional water chemistry data against water
quality standards for aquatic life use (ALU). For more information, see
the NMED Consolidated Assessment and Listing Methodology (CALM) guidance
manual.

## Usage

``` r
Conventionals_ALU(Chem_table, DU_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

## Value

A dataframe containing analyzed conventionals data compared to ALU water
quality criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
df_Conv_ALU <- Conventionals_ALU(Chem_table = df_Chem_combined
, DU_table = df_DU_processed)} # }
```
