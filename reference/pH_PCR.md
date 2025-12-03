# Analysis of pH data against PCR standards

This function compares pH data against water quality standards for
primary contact recreation (PCR) use. For more information, see the NMED
Consolidated Assessment and Listing Methodology (CALM) guidance manual.

## Usage

``` r
pH_PCR(Chem_table, DU_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

## Value

A dataframe containing analyzed pH data compared to PCR water quality
criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
df_pH_PCR <- pH_PCR(Chem_table = df_Chem_combined
, DU_table = df_DU_processed)} # }
```
