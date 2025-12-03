# Analysis of copper data from LANL against site-specific ALU standards

This function compares copper data from Los Alamos National Laboratory
(LANL) against site-specific water quality standards for certain aquatic
life uses (ALU). For more information, see the NMED Consolidated
Assessment and Listing Methodology (CALM) guidance manual and New Mexico
SQWS.

## Usage

``` r
SS_Copper_ALU(DU_LANL_Stations_table, LANL_WQ_data)
```

## Arguments

- DU_LANL_Stations_table:

  Stations DU table for LANL monitoring locations.

- LANL_WQ_data:

  Water quality data table provided by LANL to NMED.

## Value

A list of three dataframes. The first contains analyzed LANL copper data
compared to site-specific water quality criteria. The second, labeled
"Indiv_Res" is an intermediate file used for QA/QC purposes. The third,
labeled "Insuff_Res" is a dataset of copper criteria with missing
predictor parameters for copper calculations.

## Examples

``` r
if (FALSE) { # \dontrun{
SS_Copper_ALU_list <- SS_Copper_ALU(DU_LANL_Stations_table = example_DU_LANL_Sites
, LANL_WQ_data = example_LANL_WQ_data)} # }
```
