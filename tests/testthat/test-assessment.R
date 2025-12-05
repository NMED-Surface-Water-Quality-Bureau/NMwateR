test_that("Workflow works", {
  df_Chem_combined <- NMwateR::example_chemistry_processed
  df_Criteria <- NMwateR::example_criteria_processed
  df_DU_processed <- NMwateR::example_DU_processed
  example_LANL_DU_table <- NMwateR::example_LANL_DU_table
  example_LANL_WQ_table <- NMwateR::example_LANL_WQ_table

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
  #Only used for LANL data
  SS_Copper_ALU_list <- SS_Copper_ALU(DU_LANL_Stations_table = example_LANL_DU_table
                                      , LANL_WQ_data = example_LANL_WQ_table)

  df_SS_Copper_ALU <- SS_Copper_ALU_list$df_SS_Copper_ALU

  rm(SS_Copper_ALU_list, example_LANL_DU_table, example_LANL_WQ_table)

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

  ## LTD ALU ####
  df_LTD_ALU <- LTD_ALU(Chem_table = df_Chem_combined
                        , DU_table = df_DU_processed)

  ## Nutrients (Lakes) ####
  Nutrients_Lakes_list <- Nutrients_Lakes(Chem_table = df_Chem_combined
                                          , DU_table = df_DU_processed)

  df_Nutrients_Lakes <- Nutrients_Lakes_list$Nutrients_Lakes

  # cleanup
  rm(Nutrients_Lakes_list)

  ## Nutrients (Streams) ####
  Nutrients_Streams_list <- Nutrients_Streams(Chem_table = df_Chem_combined
                                              , DU_table = df_DU_processed)

  df_Nutrients_Streams <- Nutrients_Streams_list$Nutrients_Streams

  # cleanup
  rm(Nutrients_Streams_list)

  # Assessment ####
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

  df_Assess_AU_Res <- assessment_list$Assess_AU_Res

  sum_n_Samps_code <- sum(df_Assess_AU_Res$n_Samples, na.rm = TRUE)
  sum_n_Exceed_code <- sum(df_Assess_AU_Res$n_Exceed, na.rm = TRUE)
  Count_Cat2_code <- sum(df_Assess_AU_Res$Overall_Category == "2")
  Count_Cat3_code <- sum(df_Assess_AU_Res$Overall_Category == "3")
  Count_Cat5_code <- sum(df_Assess_AU_Res$Overall_Category == "5")

  # dev values taken from run of R code that was developed by Tetra Tech
  # dev code ran on 12/05/2025 using Chama project
  sum_n_Samps_dev <- 6699
  sum_n_Exceed_dev <- 78
  Count_Cat2_dev <- 943
  Count_Cat3_dev <- 147
  Count_Cat5_dev <- 701

  testthat::expect_equal(sum_n_Samps_code, sum_n_Samps_dev)
  testthat::expect_equal(sum_n_Exceed_code, sum_n_Exceed_dev)
  testthat::expect_equal(Count_Cat2_code, Count_Cat2_dev)
  testthat::expect_equal(Count_Cat3_code, Count_Cat3_dev)
  testthat::expect_equal(Count_Cat5_code, Count_Cat5_dev)

})
