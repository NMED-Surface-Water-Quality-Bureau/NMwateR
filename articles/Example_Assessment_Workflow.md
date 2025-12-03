# Example WQ Assessment Workflow

## Introduction

This vignette is intended as an example water quality assessment
workflow for the New Mexico Environment Department. Assessments occur at
a “project” level which is specific to the NMED Surface Water Quality
Bureau’s sampling schema. All assessments within a project are conducted
on an assessment unit (AU) basis.

The workflow follow four steps:

1.  Specify project-specific and generic SQUID input files
2.  Prepare and QA/QC the data
3.  Conduct water quality analyses
4.  Make assessments

Interim and final products can be exported from RStudio at any point
using a variety of functions such as
[`readr::write_csv()`](https://readr.tidyverse.org/reference/write_delim.html)
or [`utils::write.table()`](https://rdrr.io/r/utils/write.table.html).

### Contact information

If you need assistance or discover a code error, please reach out to
Meredith Zeigler (Meredith.Zeigler@env.nm.gov) or Benjamin Block
(Ben.Block@tetratech.com). You can also submit an issue on GitHub:
<https://github.com/NMED-Surface-Water-Quality-Bureau/NMwateR/issues>.

## Specify Input Files

Project-specific and generic SQUID input files are required for the
subsequent analyses. Use [`head()`](https://rdrr.io/r/utils/head.html)
or [`View()`](https://rdrr.io/r/utils/View.html) on the dataframe to
explore the contents. Search the package for the example table using
`??[fill_in_example_dataframe_name_here]` to get an explanation of each
field name.

**Criteria Table**

- *Required*

- *Description*: Table containing water quality criteria for SQUID
  analysis.

- *Dig deeper*: `??example_criteria_table`

**Parameter Table**

- *Required*

- *Description*: Table containing parameters for SQUID analyses.

- *Dig deeper*: `??example_parameter_table`

**RStudio Export**

- *Required*

- *Description*: RStudio export from SQUID that contains grab water
  chemistry data for a particular project.

- *Dig deeper*: `??example_SQUID_RStudio_table`

**Stations DU Export**

- *Required*

- *Description*: Stations DU (designated use) export from SQUID that
  contains DUs and AU-specific water quality criteria.

- *Dig deeper*: `??example_SQUID_DU_table`

**Long-term Deployment Export**

- *Optional*

- *Description*: Long-term deployment export from SQUID.

- *Dig deeper*: `??example_SQUID_LTD_table`

**Lake Profile Export**

- *Optional*

- *Description*: Lake profile data export from SQUID.

- *Dig deeper*: `??example_SQUID_LakeProfile_table`

**LANL Stations DU Export**

- *Optional*

- *Description*: Specific to LANL stations only. Stations DU (designated
  use) export from SQUID that contains DUs and AU-specific water quality
  criteria.

- *Dig deeper*: `??example_LANL_DU_table`

**LANL Water Quality Data Export**

- *Optional*

- *Description*: Specific to LANL stations only. Contains grab water
  chemistry provided by LANL.

- *Dig deeper*: `??example_LANL_WQ_table`

### Run the following to load example files:

``` r
# specify input files
data(example_criteria_table)
data(example_parameter_table)
data(example_SQUID_RStudio_table)
data(example_SQUID_DU_table)
data(example_SQUID_LTD_table)
data(example_SQUID_LakeProfile_table)
data(example_LANL_DU_table)
data(example_LANL_WQ_table)
```

## Data Preparation

Data exported from SQUID cannot immediately be used in analyses but
rather must first go through QA/QC protocols and formatting. The
[`NMwateR::Data_Prep()`](https://nmed-surface-water-quality-bureau.github.io/NMwateR/reference/Data_Prep.md)
function does all of this for the user. Note that the first four
arguments are required, whereas the LTD and lake profile datasets are
optional.

**Special user instruction**: Inspect the initial Stations DU table. For
lake stations, change the value(s) in the TEMP_WQC column to the 6T3
value unless there is a segment-specific criterion.

``` r
# Data Prep
my_data_list <- Data_Prep(criteria_table = example_criteria_table
                          , parameter_table = example_parameter_table
                          , SQUID_RStudio_table = example_SQUID_RStudio_table
                          , SQUID_DU_table = example_SQUID_DU_table
                          , SQUID_LTD_table = example_SQUID_LTD_table
                          , SQUID_LakeProfile_table = example_SQUID_LakeProfile_table)

# Pull out dataframes
df_Chem_combined <- my_data_list$Chem_Combined
df_DU_processed <- my_data_list$DU_Processed
df_Criteria <- my_data_list$Criteria_Formatted
```

## Water Quality Analyses

The following are a series of water quality analyses that can be used
depending on the type of data available from the SQUID files used in
[`NMwateR::Data_Prep()`](https://nmed-surface-water-quality-bureau.github.io/NMwateR/reference/Data_Prep.md).
A simple solution is to run all water quality analyses (although see
note below). Alternatively, the user can determine which functions to
run by identify all unique characteristics and designated uses in the
data by reviewing the `CHARACTERISTIC_NAME` , `CHR_UID` , and `DU`
fields. Lastly, the
[`NMwateR::Conventionals_ALU()`](https://nmed-surface-water-quality-bureau.github.io/NMwateR/reference/Conventionals_ALU.md)
function must be run to ensure that the assessment function
([`NMwateR::assessment()`](https://nmed-surface-water-quality-bureau.github.io/NMwateR/reference/assessment.md))
works (all others are optional depending on data availability).

**Note**: In some cases, the results from these analyses are empty
(i.e., dataframes with zero observations) due to all of the input data
not meeting CALM guidance requirements. These dataframes should not be
included in the assessment step.

For more information, see the NMED Consolidated Assessment and Listing
Methodology (CALM) guidance manual.

### Conventionals (ALU)

This function compares conventional water chemistry data against water
quality standards for aquatic life use (ALU).

``` r
df_Conv_ALU <- Conventionals_ALU(Chem_table = df_Chem_combined
                                 , DU_table = df_DU_processed)
```

### Bacteria (PCR/SCR)

This function compares bacteria data against water quality standards for
primary contact recreation (PCR) and secondary contact recreation (SCR)
uses.

``` r
df_Bacteria_PCR_SCR <- Bacteria_PCR_SCR(Chem_table = df_Chem_combined
                                        , DU_table = df_DU_processed)
```

### Conventionals (LW)

This function compares conventional water chemistry data against water
quality standards for livestock watering (LW) use.

``` r
df_Conventionals_LW <- Conventionals_LW(Chem_table = df_Chem_combined
                                        , DU_table = df_DU_processed
                                        , Criteria_table = df_Criteria)
```

### Long-term deployment (ALU)

This function compares conventional long-term deployment water chemistry
data against water quality standards for aquatic life use (ALU).

``` r
df_LTD_ALU <- LTD_ALU(Chem_table = df_Chem_combined
                      , DU_table = df_DU_processed)
```

### Nutrients (Lakes)

This function compares lake nutrient data against water quality
standards for aquatic life use (ALU).

``` r
Nutrients_Lakes_list <- Nutrients_Lakes(Chem_table = df_Chem_combined
                                        , DU_table = df_DU_processed)

df_Nutrients_Lakes <- Nutrients_Lakes_list$Nutrients_Lakes # primary file
df_Nutrients_Lakes_Indiv_Res <- Nutrients_Lakes_list$Nutrients_Lakes_Indiv_Res # intermediate file
```

### Nutrients (Streams)

This function compares stream nutrient data against water quality
standards for aquatic life use (ALU).

``` r
Nutrients_Streams_list <- Nutrients_Streams(Chem_table = df_Chem_combined
                                        , DU_table = df_DU_processed)

df_Nutrients_Streams <- Nutrients_Streams_list$Nutrients_Streams # primary file
df_Nutrients_Streams_Indiv_Res <- Nutrients_Streams_list$Nutrients_Streams_Indiv_Res # intermediate file
```

### pH (PCR)

This function compares pH data against water quality standards for
primary contact recreation (PCR) use.

``` r
df_pH_PCR <- pH_PCR(Chem_table = df_Chem_combined
                    , DU_table = df_DU_processed)
```

### Salinity (IRR)

This function compares salinity data against water quality \#’ standards
for irrigation (IRR) use.

``` r
df_Salinity_IRR <- Salinity_IRR(Chem_table = df_Chem_combined
                                , DU_table = df_DU_processed)
```

### Toxics nonHDM (ALU)

This function compares non hardness-dependent toxics data against water
quality standards for aquatic life use (ALU).

``` r
df_Tox_ALU_nHDM <- Toxics_ALU_nonHDM(Chem_table = df_Chem_combined
                                     , DU_table = df_DU_processed
                                     , Criteria_table = df_Criteria)
```

### Toxics HDM (ALU)

This function compares hardness dependent toxics data against water
quality standards for aquatic life use (ALU).

``` r
Toxics_ALU_HDM_list <- Toxics_ALU_HDM(Chem_table = df_Chem_combined
                                    , DU_table = df_DU_processed
                                    , Criteria_table = df_Criteria)


df_Toxics_ALU_HDM <- Toxics_ALU_HDM_list$Toxics_ALU_HDM # primary file
df_Toxics_ALU_HDM_Indiv_Res <- Toxics_ALU_HDM_list$Toxics_ALU_HDM_Indiv_Res # intermediate file
```

### Toxics (DWS)

This function compares toxics data against water quality standards for
drinking water supply (DWS) use.

``` r
df_Toxics_DWS <- Toxics_DWS(Chem_table = df_Chem_combined
                            , Criteria_table = df_Criteria)
```

### Toxics (HH)

This function compares toxics data against water quality standards for
human health (HH) under the aquatic life use (ALU).

``` r
df_Toxics_HH <- Toxics_HH(Chem_table = df_Chem_combined
                          , Criteria_table = df_Criteria)
```

### Toxics (IRR)

This function compares toxics data against water quality standards for
irrigation (IRR) use.

``` r
df_Toxics_IRR <- Toxics_IRR(Chem_table = df_Chem_combined
                            , Criteria_table = df_Criteria)
```

### Toxics (LW)

This function compares toxics data against water quality standards for
livestock watering (LW) use.

``` r
df_Toxics_LW <- Toxics_LW(Chem_table = df_Chem_combined
                          , Criteria_table = df_Criteria)
```

### Toxics (WH)

This function compares toxics data against water quality standards for
wildlife habitat (WH) use.

``` r
df_Toxics_WH <- Toxics_WH(Chem_table = df_Chem_combined
                          , Criteria_table = df_Criteria)
```

### Turbidity (ALU)

This function compares turbidity data against water quality standards
for aquatic life use (ALU).

``` r
Turbidity_ALU_list <- Turbidity_ALU(Chem_table = df_Chem_combined
                                    , DU_table = df_DU_processed
                                    , Criteria_table = df_Criteria)

df_Turbidity_ALU <- Turbidity_ALU_list$Turbidity_ALU # primary file
df_Turbidity_ALU_Indiv_Res <- Turbidity_ALU_list$Turbidity_ALU_Indiv_Res # intermediate file
```

### Site-specific copper (ALU)

This function compares copper data from Los Alamos National Laboratory
(LANL) against site-specific water quality standards for certain aquatic
life uses (ALU).

``` r
SS_Copper_ALU_list <- SS_Copper_ALU(DU_LANL_Stations_table = example_LANL_DU_table
                                  , LANL_WQ_data = example_LANL_WQ_table)

df_SS_Copper_ALU <- SS_Copper_ALU_list$df_SS_Copper_ALU # primary file
df_SS_Copper_ALU_Indiv_Res <- SS_Copper_ALU_list$df_SS_Copper_ALU_Indiv_Res # intermediate file
df_SS_Copper_ALU_Insuff_Res <- SS_Copper_ALU_list$df_SS_Copper_ALU_Insuff_Res # insufficient data file
```

## Assessment

The assessment is the culmination of all the water quality analyses and
utilizes the logic from the SWQS and CALM documents. The
[`NMwateR::assessment()`](https://nmed-surface-water-quality-bureau.github.io/NMwateR/reference/assessment.md)
function produces a list of four dataframes. First, the indiviual
results dataframe contains IR categories assigned to each
AU/DU/parameter combination. The DU results dataframe assigns IR
categories to each AU/DU combination based on the individual results.
The AU results dataframe assigns IR categories to each AU based on the
DUs that were assessed. Lastly, the upload report is for NMED to upload
into SQUID to be transferred to ATTAINS.

``` r
assessment_list <- assessment(Conventionals_ALU_table = df_Conv_ALU
                       , Bacteria_PCR_SCR_table = df_Bacteria_PCR_SCR
                       , Conventionals_LW_table = df_Conventionals_LW
                       , LTD_ALU_table = df_LTD_ALU
                       , Nutrients_Lakes_table = df_Nutrients_Lakes
                       , Nutrients_Streams_table = df_Nutrients_Streams
                       , pH_PCR_table = df_pH_PCR
                       , Salinity_IRR_table = df_Salinity_IRR
                       , SS_Copper_ALU_table = df_SS_Copper_ALU
                       , Toxics_ALU_nonHDM_table = df_Tox_ALU_nHDM
                       , Toxics_ALU_HDM_table = df_Toxics_ALU_HDM
                       , Toxics_DWS_table = df_Toxics_DWS
                       , Toxics_HH_table = df_Toxics_HH
                       , Toxics_IRR_table = df_Toxics_IRR
                       , Toxics_LW_table = df_Toxics_LW
                       , Toxics_WH_table = df_Toxics_WH
                       , Turbidity_ALU_table = df_Turbidity_ALU)

df_Assess_Indiv_Res <- assessment_list$Assess_Indiv_Res # Individual results
df_Assess_DU_Res <- assessment_list$Assess_DU_Res # DU results
df_Assess_AU_Res <- assessment_list$Assess_AU_Res # AU results
df_Assess_Upload_Report <- assessment_list$Assess_Upload_Report # SQUID upload report
```
