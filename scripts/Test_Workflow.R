#### R script used to test NMED assessment workflow
# Developed by Ben Block, Tetra Tech; Ben.Block@tetratech.com
## Hannah Ferriby, Tetra Tech; Hannah.Ferriby@tetratech.com
### and Kateri Salk, Tetra Tech; Kateri.SalkGundersen@tetratech.com
# Date created: 11/07/2025
# Date last updated: 11/13/2025
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.5.2 (2025-10-31 ucrt) -- "[Not] Part in a Rumble"

# Libraries needed
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(lubridate)
# library(rLakeAnalyzer)
library(devtools)

# specify input files ####
data(example_criteria_table)
data(example_parameter_table)
data(example_SQUID_RStudio_table)
data(example_SQUID_DU_table)
data(example_SQUID_LTD_table)
data(example_SQUID_LakeProfile_table)

# load all
devtools::load_all()

# Data Prep ####
my_data_list <- Data_Prep(criteria_table = example_criteria_table
                          , parameter_table = example_parameter_table
                          , SQUID_RStudio_table = example_SQUID_RStudio_table
                          , SQUID_DU_table = example_SQUID_DU_table
                          , SQUID_LTD_table = example_SQUID_LTD_table
                          , SQUID_LakeProfile_table = example_SQUID_LakeProfile_table)

## Pull out dataframes ####
df_Chem_combined <- my_data_list$Chem_Combined
df_DU_processed <- my_data_list$DU_Processed
df_Criteria <- my_data_list$Criteria_Formatted

# cleanup
rm(example_criteria_table, example_parameter_table, example_SQUID_RStudio_table
   , example_SQUID_DU_table, example_SQUID_LTD_table
   , example_SQUID_LakeProfile_table, my_data_list)

# Water quality analyses ####
## Conventionals ALU ####
df_Conv_ALU <- Conventionals_ALU(Chem_table = df_Chem_combined
                                 , DU_table = df_DU_processed)

## Toxics ALU (nonHDM) ####
# Hardness-dependent metals excluded
df_Tox_ALU_nHDM <- Toxics_ALU_nonHDM(Chem_table = df_Chem_combined
                                     , DU_table = df_DU_processed
                                     , Criteria_table = df_Criteria)

## Toxics ALU (HDM) ####
# Hardness-dependent metals only
Toxics_ALU_HDM_list <- Toxics_ALU_HDM(Chem_table = df_Chem_combined
                                    , DU_table = df_DU_processed
                                    , Criteria_table = df_Criteria)


df_Toxics_ALU_HDM <- Toxics_ALU_HDM_list$Toxics_ALU_HDM
df_Toxics_ALU_HDM_Indiv_Res <- Toxics_ALU_HDM_list$Toxics_ALU_HDM_Indiv_Res

# cleanup
rm(Toxics_ALU_HDM_list)

## Conventionals LW ####
df_Conventionals_LW <- Conventionals_LW(Chem_table = df_Chem_combined
                                        , DU_table = df_DU_processed
                                        , Criteria_table = df_Criteria)

## Salinity IRR ####
df_Salinity_IRR <- Salinity_IRR(Chem_table = df_Chem_combined
                                , DU_table = df_DU_processed)

## Toxics HH ####
df_Toxics_HH <- Toxics_HH(Chem_table = df_Chem_combined
                          , Criteria_table = df_Criteria)

## Site-specific copper ####
# Only used for LANL data
SS_Copper_ALU_list <- SS_Copper_ALU(DU_LANL_Stations_table = example_DU_LANL_Sites
                                  , LANL_WQ_data = example_LANL_WQ_data)

## Toxics DWS ####
df_Toxics_DWS <- Toxics_DWS(Chem_table = df_Chem_combined
                            , Criteria_table = df_Criteria)

## Toxics IRR ####
df_Toxics_IRR <- Toxics_IRR(Chem_table = df_Chem_combined
                            , Criteria_table = df_Criteria)

## Toxics WH ####
df_Toxics_WH <- Toxics_WH(Chem_table = df_Chem_combined
                          , Criteria_table = df_Criteria)

## Toxics LW ####
df_Toxics_LW <- Toxics_LW(Chem_table = df_Chem_combined
                          , Criteria_table = df_Criteria)

## Bacteria PCR/SCR ###
df_Bacteria_PCR_SCR <- Bacteria_PCR_SCR(Chem_table = df_Chem_combined
                                        , DU_table = df_DU_processed)

## pH PCR ####
df_pH_PCR <- pH_PCR(Chem_table = df_Chem_combined
                    , DU_table = df_DU_processed)

## Turbidity ALU ####
Turbidity_ALU_list <- Turbidity_ALU(Chem_table = df_Chem_combined
                                    , DU_table = df_DU_processed
                                    , Criteria_table = df_Criteria)

df_Turbidity_ALU <- Turbidity_ALU_list$Turbidity_ALU
df_Turbidity_ALU_Indiv_Res <- Turbidity_ALU_list$Turbidity_ALU_Indiv_Res

## LTD ALU ####
df_LTD_ALU <- LTD_ALU(Chem_table = df_Chem_combined
                      , DU_table = df_DU_processed)

## Nutrients (Lakes) ####
Nutrients_Lakes_list <- Nutrients_Lakes(Chem_table = df_Chem_combined
                                        , DU_table = df_DU_processed)

df_Nutrients_Lakes <- Nutrients_Lakes_list$Nutrients_Lakes
df_Nutrients_Lakes_Indiv_Res <- Nutrients_Lakes_list$Nutrients_Lakes_Indiv_Res

Nutrients_Streams

## Nutrients (Streams) ####
Nutrients_Streams_list <- Nutrients_Streams(Chem_table = df_Chem_combined
                                        , DU_table = df_DU_processed)

df_Nutrients_Streams <- Nutrients_Streams_list$Nutrients_Streams
df_Nutrients_Streams_Indiv_Res <- Nutrients_Streams_list$Nutrients_Streams_Indiv_Res
