# Analysis of bacteria data against PCR and SCR standards

This function compares bacteria data against water quality standards for
primary contact recreation (PCR) and secondary contact recreation (SCR)
uses. For more information, see the NMED Consolidated Assessment and
Listing Methodology (CALM) guidance manual.

## Usage

``` r
Bacteria_PCR_SCR(Chem_table, DU_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

## Value

A dataframe containing analyzed bacteria data compared to PCR and SCR
water quality criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
df_Bacteria_PCR_SCR <- Bacteria_PCR_SCR(Chem_table = df_Chem_combined
, DU_table = df_DU_processed)} # }
```
