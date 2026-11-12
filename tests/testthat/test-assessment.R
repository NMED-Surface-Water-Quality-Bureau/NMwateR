test_that("Workflow works", {
  df_Chem_combined <- NMwateR::example_chemistry_processed
  df_Criteria <- NMwateR::example_criteria_processed
  df_DU_processed <- NMwateR::example_DU_processed

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
  sum_n_Samps_dev <- 6689
  sum_n_Exceed_dev <- 78
  Count_Cat2_dev <- 941
  Count_Cat3_dev <- 147
  Count_Cat5_dev <- 701

  testthat::expect_equal(sum_n_Samps_code, sum_n_Samps_dev)
  testthat::expect_equal(sum_n_Exceed_code, sum_n_Exceed_dev)
  testthat::expect_equal(Count_Cat2_code, Count_Cat2_dev)
  testthat::expect_equal(Count_Cat3_code, Count_Cat3_dev)
  testthat::expect_equal(Count_Cat5_code, Count_Cat5_dev)

})

test_that("verified results rebuild upload decisions and preserve reviewer notes", {
  input <- tibble::tibble(
    WATER_ID = c("AU1", "AU1", "AU2"), WATER_NAME = "Test river",
    DU = c("ColdWAL", "LW", "PC"), CHR_UID = c(1, 1, 2),
    CHARACTERISTIC_NAME = c("Metal", "Metal", "Bacteria"),
    n_Samples = 4, Method = "Count", Rationale = "Data Sufficient",
    Exceed = c("Yes", "No", "No"), n_Exceed = c(2, 0, 0)
  )
  original <- assessment(input)
  verified <- original$Assess_DU_Res
  unchanged <- assessment(Verified_results_table = verified)
  expect_equal(unchanged, original)

  verified$Individual_Category <- c("2", "3c", "5")
  verified$AV_Notes <- c("Excluded duplicate sample after review.", NA, "Verified lab result.")
  verified$PARAM_COMMENT <- c(NA, "Reviewer parameter comment.", "")
  verified$ASSESSMENT_RATIONALE <- c("Reviewer rationale.", NA, "")
  result <- assessment(Verified_results_table = verified)
  report <- result$Assess_Upload_Report
  expect_equal(result$Assess_DU_Res$Use_Category, c("2", "3", "5"))
  expect_equal(result$Assess_AU_Res$Overall_Category, c("2", "5"))
  expect_equal(report$PARAM_ATTAINMENT_CODE,
               c("meeting criteria", "not enough information", "not meeting criteria"))
  expect_equal(report$STATE_IR_CAT_CODE, c("2", "2", "5"))
  expect_equal(report$ASSESSMENT_RATIONALE[1],
               "Reviewer rationale. Assessment verification notes: Excluded duplicate sample after review.")
  expect_equal(report$PARAM_COMMENT[2], "Reviewer parameter comment.")
  expect_match(report$PARAM_COMMENT[1], "Determination: Fully Supporting", fixed = TRUE)
  expect_match(report$PARAM_COMMENT[1], "Excluded duplicate sample after review.", fixed = TRUE)
  expect_match(report$ASSESSMENT_RATIONALE[3], "Verified lab result.", fixed = TRUE)
  expect_equal(nrow(result$Assess_Upload_Report_FS), 1L)
  expect_equal(nrow(result$Assess_Upload_Report_NS), 1L)
  expect_equal(nrow(result$Assess_Upload_Report_NA), 1L)
  expect_identical(result$Assess_Upload_Report_NS$PARAM_COMMENT, report$PARAM_COMMENT[3])

  minimal <- verified[c("WATER_ID", "WATER_NAME", "DU", "CHR_UID",
                         "CHARACTERISTIC_NAME", "Individual_Category")]
  minimal$Individual_Category[1] <- NA_character_
  expect_equal(nrow(assessment(Verified_results_table = minimal)$Assess_Upload_Report_NA), 2L)
  expect_equal(nrow(assessment(Verified_results_table = minimal[0, ])$Assess_Upload_Report), 0L)
  expect_error(assessment(input, Verified_results_table = verified), "alone")
  expect_error(assessment(Verified_results_table = original$Assess_AU_Res), "missing")
  expect_error(assessment(Verified_results_table = rbind(verified, verified[1, ])), "duplicate")
  verified$Individual_Category[1] <- "typo"
  expect_error(assessment(Verified_results_table = verified), "unsupported")
  verified$Individual_Category[1] <- "2"
  verified$DU[1] <- ""
  expect_error(assessment(Verified_results_table = verified), "nonblank DU")
})

test_that("upload rationales use the selected result for each AU/use/parameter", {
  input <- tibble::tibble(
    WATER_ID = "AU1", WATER_NAME = "Test river",
    DU = c("ColdWAL", "ColdWAL", "LW", "PC", "ColdWAL"),
    CHR_UID = c(1, 1, 1, 2, 3),
    CHARACTERISTIC_NAME = c("Metal", "Metal", "Metal", "Bacteria", "Chloride"),
    SAMPLE_FRACTION = "Total", n_Samples = c(4, 8, 5, 1, 20),
    n_Samples_assessable = c(4, 6, 5, 1, 20),
    Method = c("First method", "Acute and chronic", "Count", "Count", "Percent"),
    Rationale = c("Data Sufficient", "Data Sufficient", "Data Sufficient",
                  "Data Insufficient", "Data Sufficient"),
    Exceed = c("No", "Yes", "No", "Yes", "No"),
    n_Exceed = c(0, NA, 0, 1, NA),
    n_Exceed_Acute = c(NA, 2, NA, NA, NA),
    n_Exceed_Chronic = c(NA, 3, NA, NA, NA),
    pct_Exceed = c(NA, NA, NA, NA, 5)
  )
  result <- assessment(input)
  report <- result$Assess_Upload_Report
  expect_equal(nrow(report), 4L)
  expect_identical(report$ASSESSMENT_RATIONALE, report$PARAM_COMMENT)
  expect_false(anyNA(report$PARAM_COMMENT))
  metal <- subset(report, PARAM_NAME == "Metal" & USE_NAME == "Coldwater Aquatic Life")
  expect_match(metal$PARAM_COMMENT, "acute exceedance count: 2; chronic exceedance count: 3", fixed = TRUE)
  expect_match(metal$PARAM_COMMENT, "assessable samples: 6", fixed = TRUE)
  expect_match(metal$PARAM_COMMENT, "Exceedance count: not available", fixed = TRUE)
  expect_false(grepl("First method", metal$PARAM_COMMENT, fixed = TRUE))
  livestock <- subset(report, USE_NAME == "Livestock Watering")
  expect_match(livestock$PARAM_COMMENT, "Exceedance count: 0", fixed = TRUE)
  expect_match(livestock$PARAM_COMMENT, "Determination: Fully Supporting", fixed = TRUE)
  expect_match(result$Assess_Upload_Report_NA$PARAM_COMMENT,
               "Data sufficiency: Data Insufficient", fixed = TRUE)
  expect_match(subset(report, PARAM_NAME == "Chloride")$PARAM_COMMENT,
               "Percent exceedance: 5", fixed = TRUE)
  for (suffix in c("FS", "NS", "NA")) {
    split <- result[[paste0("Assess_Upload_Report_", suffix)]]
    expect_identical(names(split), names(report))
    expect_true(all(split$PARAM_COMMENT %in% report$PARAM_COMMENT))
  }
})

test_that("HH rationales use the linked DU and nutrient counts remain unavailable", {
  input <- tibble::tibble(
    WATER_ID = "AU1", WATER_NAME = "Test river", DU = c("ColdWAL", "HH"),
    CHR_UID = c(1, 2), CHARACTERISTIC_NAME = c("Temperature", "Metal"),
    n_Samples = 4, Method = "Count", Rationale = "Data Sufficient",
    Exceed = "No", n_Exceed = 0
  )
  nutrients <- tibble::tibble(WATER_ID = "AU1", WATER_NAME = "Test river",
                             DU = "ColdWAL", Category = "3c")
  result <- assessment(input, Nutrients_Streams_table = nutrients)
  report <- result$Assess_Upload_Report
  expect_match(subset(report, PARAM_NAME == "Metal")$PARAM_COMMENT,
               "designated use: Coldwater Aquatic Life (ColdWAL)", fixed = TRUE)
  nutrient <- subset(report, PARAM_NAME == "Plant Nutrients")$PARAM_COMMENT
  expect_match(nutrient, "Method: Stream Nutrients Assessment", fixed = TRUE)
  expect_match(nutrient, "Exceedance count: not available", fixed = TRUE)
  expect_match(nutrient, "Determination: Not Assessed", fixed = TRUE)
  empty <- assessment(input[0, ])
  expect_length(empty$Assess_Upload_Report$PARAM_COMMENT, 0L)
})
