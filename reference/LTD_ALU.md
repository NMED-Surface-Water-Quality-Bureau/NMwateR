# Analysis of long-term deployment data against ALU standards

This function compares conventional long-term deployment water chemistry
data against water quality standards for aquatic life use (ALU). For
more information, see the NMED Consolidated Assessment and Listing
Methodology (CALM) guidance manual.

## Usage

``` r
LTD_ALU(Chem_table, DU_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

## Value

A dataframe containing analyzed conventional LTD data compared to ALU
water quality criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
df_LTD_ALU <- LTD_ALU(Chem_table = df_Chem_combined
, DU_table = df_DU_processed)} # }
```
