# Analysis of stream nutrient data against ALU standards

This function compares stream nutrient data against water quality
standards for aquatic life use (ALU). For more information, see the NMED
Consolidated Assessment and Listing Methodology (CALM) guidance manual.

## Usage

``` r
Nutrients_Streams(Chem_table, DU_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

## Value

A list of two dataframes. The first contains analyzed stream nutrient
data compared to ALU water quality criteria. The second, labeled
"Indiv_Res" is an intermediate file used for QA/QC purposes.

## Examples

``` r
df_Chem_combined <- NMwateR::example_chemistry_processed
df_DU_processed <- NMwateR::example_DU_processed

Nutrients_Streams_list <- Nutrients_Streams(Chem_table = df_Chem_combined
, DU_table = df_DU_processed)
#> Joining with `by = join_by(WATER_ID, WATER_NAME, PROJECT_NAME, STATION,
#> STATION_NAME, SAMPLING_EVENT_TYPE, DATE, TIME, CHR_UID, MEASUREMENT_num,
#> ASSESSABILITY_QUALIFIER_CODE)`
#> Joining with `by = join_by(WATER_ID)`
#> Joining with `by = join_by(WATER_ID)`

df_Nutrients_Streams <- Nutrients_Streams_list$Nutrients_Streams
df_Nutrients_Streams_Indiv_Res <- Nutrients_Streams_list$Nutrients_Streams_Indiv_Res
```
