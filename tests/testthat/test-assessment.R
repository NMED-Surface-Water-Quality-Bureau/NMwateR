test_that("nutrient-only assessments count 1 or 2 as a fully supporting use (issue #6)", {
  nutrients <- tibble::tibble(
    WATER_ID = c("AU1", "AU2", "AU3"), WATER_NAME = "Test waterbody",
    DU = "ColdWAL", Category = c("1 or 2", "3b", "4 or 5")
  )

  for (table in c("Nutrients_Lakes_table", "Nutrients_Streams_table")) {
    result <- do.call(assessment, setNames(list(nutrients), table))
    expect_equal(result$Assess_DU_Res$Individual_Category, nutrients$Category,
                 info = table)
    expect_equal(result$Assess_DU_Res$Use_Category, c("2", "3", "5"), info = table)
    expect_equal(result$Assess_AU_Res$Overall_Category, c("2", "3", "5"), info = table)

    report <- result$Assess_Upload_Report
    expect_true(all(is.na(report$STATE_IR_CAT_CODE)), info = table)
    expect_equal(result$Assess_Upload_Report_FS$ASSESSMENT_UNIT_ID,
                 c("AU1", "AU2", "AU3"), info = table)
    expect_equal(result$Assess_Upload_Report_FS$USE_ATTAINMENT_CODE,
                 c("F", "X", "F"), info = table)
    expect_equal(result$Assess_Upload_Report_3C3B$PARAM_STATE_IR_CAT, "3B", info = table)
    expect_equal(result$Assess_Upload_Report_NS$PARAM_STATE_IR_CAT, "5A", info = table)
    expect_equal(result$Assess_Upload_Report_NS$ASSESSMENT_UNIT_ID, "AU3", info = table)
  }
})

test_that("fully supporting nutrients respect use category precedence (issue #6)", {
  input <- tibble::tibble(
    WATER_ID = c("AU1", "AU2", "AU3"), WATER_NAME = "Test waterbody",
    DU = "ColdWAL", CHR_UID = 1, CHARACTERISTIC_NAME = "Metal",
    n_Samples = c(1, 4, 4), Method = "Count",
    Rationale = c("Data Insufficient", "Data Sufficient", "Data Sufficient"),
    Exceed = c("No", "Yes", "No"), n_Exceed = c(0, 2, 0)
  )
  nutrients <- tibble::tibble(
    WATER_ID = input$WATER_ID, WATER_NAME = input$WATER_NAME,
    DU = "ColdWAL", Category = "1 or 2"
  )

  for (table in c("Nutrients_Lakes_table", "Nutrients_Streams_table")) {
    result <- do.call(assessment, c(list(Conventionals_ALU_table = input),
                                   setNames(list(nutrients), table)))
    uses <- unique(result$Assess_DU_Res[c("WATER_ID", "DU", "Use_Category")])
    expect_equal(uses$Use_Category, c("2", "5", "2"), info = table)
    expect_equal(result$Assess_AU_Res$Overall_Category, c("2", "5", "2"), info = table)
    expect_equal(nrow(result$Assess_DU_Res), 6L, info = table)

    # The final verification run must apply the same category precedence.
    verified <- assessment(Verified_results_table = result$Assess_DU_Res)
    expect_equal(verified$Assess_DU_Res, result$Assess_DU_Res, info = table)
    expect_equal(verified$Assess_Upload_Report, result$Assess_Upload_Report, info = table)
  }
})

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

  Count_Cat2_code <- sum(df_Assess_AU_Res$Overall_Category == "2")
  Count_Cat3_code <- sum(df_Assess_AU_Res$Overall_Category == "3")
  Count_Cat5_code <- sum(df_Assess_AU_Res$Overall_Category == "5")

  # The AU summary now has one conclusion per AU. The former expectations
  # counted repeated parameter rows and sample statistics in that summary.
  testthat::expect_equal(nrow(df_Assess_AU_Res), 58L)
  testthat::expect_equal(anyDuplicated(df_Assess_AU_Res$WATER_ID), 0L)
  testthat::expect_equal(Count_Cat2_code, 27L)
  testthat::expect_equal(Count_Cat3_code, 6L)
  testthat::expect_equal(Count_Cat5_code, 25L)

})

test_that("verified results rebuild decisions and retain reviewer evidence", {
  input <- tibble::tibble(
    WATER_ID = c("AU1", "AU1", "AU2"), WATER_NAME = "Test river",
    DU = c("ColdWAL", "LW", "PC"), CHR_UID = c(1, 1, 2),
    CHARACTERISTIC_NAME = c("Metal", "Metal", "Bacteria"),
    n_Samples = 4, Method = "Count", Rationale = "Data Sufficient",
    Exceed = c("Yes", "No", "No"), n_Exceed = c(2, 0, 0)
  )
  original <- assessment(input)
  verified <- original$Assess_DU_Res
  expect_equal(assessment(Verified_results_table = verified), original)

  verified$Individual_Category <- c("2", "3c", "5")
  verified$AV_Notes <- c("Excluded duplicate sample after review.", NA, "Verified lab result.")
  verified$PARAM_COMMENT <- c(NA, "Reviewer parameter comment.", "")
  verified$ASSESSMENT_RATIONALE <- c("Reviewer rationale.", NA, "")
  result <- assessment(Verified_results_table = verified)
  expect_equal(result$Assess_DU_Res$Use_Category, c("2", "3", "5"))
  expect_equal(result$Assess_AU_Res$Overall_Category, c("2", "5"))
  expect_equal(result$Assess_DU_Res$AV_Notes, verified$AV_Notes)
  expect_equal(result$Assess_Upload_Report_3C3B$PARAM_COMMENT, "Reviewer parameter comment.")
  expect_match(result$Assess_Upload_Report_NS$ASSESSMENT_RATIONALE,
               "Verified lab result.", fixed = TRUE)
  expect_true(all(is.na(result$Assess_Upload_Report_FS$PARAM_NAME)))
  expect_equal(nrow(result$Assess_Upload_Report_FS), 3L)
  expect_equal(nrow(result$Assess_Upload_Report_NS), 1L)
  expect_equal(nrow(result$Assess_Upload_Report_3C3B), 1L)
  expect_false("Assess_Upload_Report_NA" %in% names(result))

  minimal <- verified[c("WATER_ID", "WATER_NAME", "DU", "CHR_UID",
                         "CHARACTERISTIC_NAME", "Individual_Category")]
  minimal$Individual_Category[1] <- NA_character_
  expect_equal(assessment(Verified_results_table = minimal)$Assess_Upload_Report_FS$USE_ATTAINMENT_CODE,
               c("X", "F", "F"))
  empty <- assessment(Verified_results_table = minimal[0, ])
  expect_equal(nrow(empty$Assess_Upload_Report), 0L)
  expect_equal(ncol(empty$Assess_Upload_Report), 46L)
  expect_error(assessment(input, Verified_results_table = verified), "alone")
  expect_error(assessment(Verified_results_table = original$Assess_AU_Res), "missing")
  expect_error(assessment(Verified_results_table = rbind(verified, verified[1, ])), "duplicate")
  verified$Individual_Category[1] <- "typo"
  expect_error(assessment(Verified_results_table = verified), "unsupported")
  verified$Individual_Category[1] <- "2"
  verified$DU[1] <- ""
  expect_error(assessment(Verified_results_table = verified), "nonblank DU")
})

test_that("parameter evidence follows selection and rationale is shared by AU", {
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
  metal <- result$Assess_Upload_Report_NS
  expect_match(metal$PARAM_COMMENT, "acute exceedance count: 2; chronic exceedance count: 3", fixed = TRUE)
  expect_match(metal$PARAM_COMMENT, "assessable samples: 6", fixed = TRUE)
  expect_match(metal$PARAM_COMMENT, "Exceedance count: not available", fixed = TRUE)
  expect_false(grepl("First method", metal$PARAM_COMMENT, fixed = TRUE))
  monitoring <- result$Assess_Upload_Report_3C3B
  expect_match(monitoring$PARAM_COMMENT, "Data sufficiency: Data Insufficient", fixed = TRUE)
  expect_identical(metal$ASSESSMENT_RATIONALE, monitoring$ASSESSMENT_RATIONALE)
  expect_true(all(is.na(result$Assess_Upload_Report_FS$ASSESSMENT_RATIONALE)))
  # Fully supporting detail stays in intermediate results, not parameter uploads.
  expect_equal(subset(result$Assess_DU_Res, CHARACTERISTIC_NAME == "Chloride")$pct_Exceed, 5)
  for (suffix in c("FS", "NS", "3C3B")) {
    split <- result[[paste0("Assess_Upload_Report_", suffix)]]
    expect_identical(names(split), names(result$Assess_Upload_Report))
    expect_true(all(split$PARAM_COMMENT %in% result$Assess_Upload_Report$PARAM_COMMENT))
  }
})

test_that("HH is linked to its DU and nutrient evidence remains available", {
  input <- tibble::tibble(
    WATER_ID = "AU1", WATER_NAME = "Test river", DU = c("ColdWAL", "HH"),
    CHR_UID = c(1, 2), CHARACTERISTIC_NAME = c("Temperature", "Metal"),
    n_Samples = 4, Method = "Count", Rationale = "Data Sufficient",
    Exceed = c("No", "Yes"), n_Exceed = c(0, 2)
  )
  nutrients <- tibble::tibble(WATER_ID = "AU1", WATER_NAME = "Test river",
                             DU = "ColdWAL", Category = "3c")
  result <- assessment(input, Nutrients_Streams_table = nutrients)
  expect_match(result$Assess_Upload_Report_NS$PARAM_COMMENT,
               "designated use: Coldwater Aquatic Life (ColdWAL)", fixed = TRUE)
  nutrient <- result$Assess_Upload_Report_3C3B
  expect_equal(nutrient$PARAM_NAME, "Nutrients")
  expect_equal(nutrient$USE_ATTAINMENT_CODE, "N")
  expect_match(nutrient$PARAM_COMMENT, "Method: Stream Nutrients Assessment", fixed = TRUE)
  expect_match(nutrient$PARAM_COMMENT, "Exceedance count: not available", fixed = TRUE)
  expect_match(nutrient$PARAM_COMMENT, "Determination: Not Assessed", fixed = TRUE)
  empty <- assessment(input[0, ])
  expect_length(empty$Assess_Upload_Report$PARAM_COMMENT, 0L)
})
