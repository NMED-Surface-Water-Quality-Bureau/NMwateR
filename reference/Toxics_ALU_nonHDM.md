# Analysis of non hardness dependent toxics data against ALU standards

This function compares non hardness dependent toxics data against water
quality standards for aquatic life use (ALU). For more information, see
the NMED Consolidated Assessment and Listing Methodology (CALM) guidance
manual.

## Usage

``` r
Toxics_ALU_nonHDM(Chem_table, DU_table, Criteria_table)
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

A dataframe containing analyzed non hardness dependent toxics data
compared to ALU water quality criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
df_Tox_ALU_nHDM <- Toxics_ALU_nonHDM(Chem_table = df_Chem_combined
, DU_table = df_DU_processed
, Criteria_table = df_Criteria)} # }
```
