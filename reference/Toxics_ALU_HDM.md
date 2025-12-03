# Analysis of hardness dependent toxics data against ALU standards

This function compares hardness dependent toxics data against water
quality standards for aquatic life use (ALU). For more information, see
the NMED Consolidated Assessment and Listing Methodology (CALM) guidance
manual.

## Usage

``` r
Toxics_ALU_HDM(Chem_table, DU_table, Criteria_table)
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

A list of two dataframes. The first contains analyzed hardness dependent
toxics data compared to LW water quality criteria. The second, labeled
"Indiv_Res" is an intermediate file used for QA/QC purposes.

## Examples

``` r
if (FALSE) { # \dontrun{
Toxics_ALU_HDM_list <- Toxics_ALU_HDM(Chem_table = df_Chem_combined
, DU_table = df_DU_processed
, Criteria_table = df_Criteria)
df_Toxics_ALU_HDM <- Toxics_ALU_HDM_list$Toxics_ALU_HDM
df_Toxics_ALU_HDM_Indiv_Res <- Toxics_ALU_HDM_list$Toxics_ALU_HDM_Indiv_Res} # }
```
