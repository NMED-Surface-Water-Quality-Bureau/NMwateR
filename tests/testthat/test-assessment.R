test_that("low-n excursions use counts as well as the analysis decision flag", {
  input <- tibble::tibble(
    WATER_ID = paste0("AU", 1:7), WATER_NAME = "Test waterbody", DU = "ColdWAL",
    CHR_UID = 1, CHARACTERISTIC_NAME = "Copper", n_Samples = 3,
    Rationale = c(rep("Data Insufficient", 6), "Data Sufficient"),
    Exceed = c(rep("No", 5), "Yes", "No"),
    n_Exceed = c(1, NA, NA, 0, NA, NA, 1),
    n_Exceed_Acute = c(NA, 1, NA, 0, NA, NA, NA),
    n_Exceed_Chronic = c(NA, NA, 1, 0, NA, NA, NA))
  expect_silent(result <- assessment(Conventionals_ALU_table = input))
  expect_equal(result$Assess_DU_Res$Individual_Category,
               c("3c", "3c", "3c", "3b", "3b", "3c", "2"))
  expect_equal(result$Assess_Upload_Report_3C3B$PARAM_STATE_IR_CAT,
               c("3C", "3C", "3C", "3B", "3B", "3C"))
  verified <- result$Assess_DU_Res
  verified$Individual_Category[1] <- "3b"
  expect_silent(rerun <- assessment(Verified_results_table = verified))
  expect_equal(rerun$Assess_DU_Res$Individual_Category[1], "3b")
})

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
  expect_silent({
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
  # Exercise the workflow's CSV import with complete processed example data.
  av_file <- tempfile(fileext = ".csv")
  on.exit(unlink(av_file), add = TRUE)
  readr::write_csv(assessment_list$Assess_DU_Res, av_file)
  verified_results <- readr::read_csv(av_file,
    col_types = readr::cols(.default = readr::col_character()))
  final <- assessment(Verified_results_table = verified_results)
  expect_equal(final$Assess_Upload_Report, assessment_list$Assess_Upload_Report)
  })
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
  expect_equal(result$Assess_Upload_Report_3C3B$PARAM_COMMENT,
               "Reviewer parameter comment. changed during AV from 2 to 3C.")
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
  expect_equal(metal$PARAM_COMMENT, "5A for Metal - 2/6 acute and 3/6 chronic excs.")
  expect_false(grepl("First method", metal$PARAM_COMMENT, fixed = TRUE))
  monitoring <- result$Assess_Upload_Report_3C3B
  expect_equal(monitoring$PARAM_COMMENT, "3C for primary contact Bacteria - 1/1 excs.")
  expect_identical(metal$ASSESSMENT_RATIONALE, monitoring$ASSESSMENT_RATIONALE)
  expect_true(all(result$Assess_Upload_Report_FS$ASSESSMENT_RATIONALE == metal$ASSESSMENT_RATIONALE))
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
  expect_equal(result$Assess_Upload_Report_NS$PARAM_COMMENT, "5A for Metal - 2/4 excs.")
  nutrient <- result$Assess_Upload_Report_3C3B
  expect_equal(nutrient$PARAM_NAME, "Nutrients")
  expect_equal(nutrient$USE_ATTAINMENT_CODE, "N")
  expect_equal(nutrient$PARAM_COMMENT, "3C for Plant Nutrients - delta DO data unavailable.")
  empty <- assessment(input[0, ])
  expect_length(empty$Assess_Upload_Report$PARAM_COMMENT, 0L)
})

selection_input <- function(categories) {
  tibble::tibble(
    WATER_ID = "AU1", WATER_NAME = "Test lake", DU = "ColdWAL",
    CHR_UID = 2849, CHARACTERISTIC_NAME = "Temperature, water",
    Individual_Category = categories, Rationale = "Data Sufficient",
    n_Exceed = 1, n_Samples = 4, Method = "Count")
}

test_that("DU and SQUID selection follow impaired > 3C > FS > 3B > 3A", {
  # Feed categories through the analysis path as well, since verified input
  # deliberately requires unique AU/use/characteristic records.
  pairs <- list(c("5", "3c"), c("5c", "3c"), c("4 or 5", "3c"),
                c("3c", "2"), c("3c", "1 or 2"),
                c("2", "3b"), c("1 or 2", "3b"), c("3b", "3a"))
  for (pair in pairs) {
    input <- selection_input(pair)
    selected <- NMwateR:::.assessment_select_result(
      input, c("WATER_ID", "DU", "CHR_UID", "CHARACTERISTIC_NAME"), "Individual_Category")
    expect_equal(selected$Individual_Category, pair[1], info = paste(pair, collapse = " > "))
    expect_equal(NMwateR:::.assessment_select_result(
      input[2:1, ], c("WATER_ID", "DU", "CHR_UID", "CHARACTERISTIC_NAME"),
      "Individual_Category"), selected)
  }

  input <- selection_input(c("2", "3c"))
  input$CHR_UID <- c(2849, 999920)
  input$CHARACTERISTIC_NAME <- c("Temperature, water", "4T3")
  result <- assessment(Verified_results_table = input)
  expect_true(all(result$Assess_DU_Res$Use_Category == "3"))
  expect_equal(result$Assess_Upload_Report_3C3B$PARAM_NAME, "Temperature")
  expect_equal(result$Assess_Upload_Report_3C3B$PARAM_STATE_IR_CAT, "3C")
  expect_equal(result$Assess_Upload_Report_3C3B$USE_ATTAINMENT_CODE, "F")
})

test_that("blank names and profile fractions do not duplicate DU results", {
  input <- tibble::tibble(
    WATER_ID = "NM-2114_00", WATER_NAME = rep(c("Abiquiu Reservoir", "  "), 4),
    DU = c(rep("ColdWAL", 6), "PC", "PC"),
    CHR_UID = c(985, 985, 1648, 1648, 2849, 2849, 1648, 1648),
    CHARACTERISTIC_NAME = c(rep("Dissolved oxygen (DO)", 2), rep("pH", 2),
                            rep("Temperature, water", 2), rep("pH", 2)),
    SAMPLE_FRACTION = rep(c(NA, "Thermocline Average"), 4),
    n_Samples = rep(c(4, 20), 4), Rationale = "Data Sufficient", Exceed = "No")
  input$WATER_NAME[4] <- NA_character_
  result <- assessment(Conventionals_ALU_table = input)
  expect_equal(nrow(result$Assess_Indiv_Res), 8L)
  expect_equal(nrow(result$Assess_DU_Res), 4L)
  expect_true(all(result$Assess_DU_Res$WATER_NAME == "Abiquiu Reservoir"))
  expect_true(all(result$Assess_DU_Res$n_Samples == 20))
  expect_equal(assessment(Verified_results_table = result$Assess_DU_Res)$Assess_Upload_Report,
               result$Assess_Upload_Report)
  reversed <- assessment(Conventionals_ALU_table = input[8:1, ])
  expect_equal(reversed$Assess_DU_Res, result$Assess_DU_Res)
  expect_equal(reversed$Assess_Upload_Report, result$Assess_Upload_Report)
})

test_that("category ties select sufficient data then most exceedances then most samples", {
  input <- selection_input(rep("5", 5))
  input$CHR_UID <- seq_len(nrow(input))
  input$CHARACTERISTIC_NAME <- c("Temperature, water", "Maximum - Temperature, water",
                                 "4T3", "Temperature", "Temperature")
  input$Rationale <- c("Data Sufficient; Low N Samples", rep("Data Sufficient", 4))
  input$n_Exceed <- c(99, 2, 3, 3, 3)
  input$n_Samples <- c(100, 100, 10, 20, 20)
  input$Method <- c("Low N", "Fewer exceedances", "Fewer samples", "A strongest", "Z strongest")
  result <- assessment(Verified_results_table = input)
  ns <- result$Assess_Upload_Report_NS
  expect_equal(nrow(ns), 1L)
  expect_match(ns$PARAM_COMMENT, "3/20 excs", fixed = TRUE)
  expect_false(grepl("99/100|2/100|3/10", ns$PARAM_COMMENT))
  # Also exercise the analytical grouping path, including a final lexical tie.
  analysis <- input
  analysis$CHR_UID <- 2849
  analysis$CHARACTERISTIC_NAME <- "Temperature, water"
  analysis$Individual_Category <- NULL
  analysis$Exceed <- "Yes"
  first <- assessment(Conventionals_ALU_table = analysis)
  expect_equal(first$Assess_DU_Res$Method, "A strongest")
  for (order in list(5:1, c(3, 5, 1, 4, 2))) {
    reordered <- assessment(Verified_results_table = input[order, ])
    expect_equal(reordered$Assess_Upload_Report, result$Assess_Upload_Report)
    reordered_analysis <- assessment(Conventionals_ALU_table = analysis[order, ])
    expect_equal(reordered_analysis$Assess_DU_Res, first$Assess_DU_Res)
    expect_equal(reordered_analysis$Assess_Upload_Report, first$Assess_Upload_Report)
  }
})

test_that("acute and chronic counts break ties without double counting", {
  input <- selection_input(rep("5", 3))
  input$n_Exceed <- NA_real_
  input$n_Exceed_Acute <- c(2, 4, NA)
  input$n_Exceed_Chronic <- c(3, 0, NA)
  input$n_Samples <- c(10, 8, 100)
  input$Method <- c("Overlapping acute/chronic", "Most known exceedances", "No count available")
  input$Individual_Category <- NULL
  input$Exceed <- "Yes"
  result <- assessment(Conventionals_ALU_table = input)
  expect_equal(result$Assess_DU_Res$Method, "Most known exceedances")
  expect_equal(result$Assess_DU_Res$n_Exceed_Acute, 4)
  expect_equal(assessment(Conventionals_ALU_table = input[3:1, ])$Assess_DU_Res,
               result$Assess_DU_Res)
})

upload_fixture <- function(categories, names = rep("Temperature, water", length(categories)),
                           water_ids = rep("AU1", length(categories)),
                           uses = rep("ColdWAL", length(categories))) {
  tibble::tibble(WATER_ID = water_ids, WATER_NAME = "Test waterbody", DU = uses,
                 CHR_UID = seq_along(categories), CHARACTERISTIC_NAME = names,
                 Individual_Category = categories)
}

test_that("FS initializes every AU/use exactly once as F or X", {
  input <- upload_fixture(c("2", "3b", "5", "3b", "3c"),
                           water_ids = c("AU1", "AU1", "AU2", "AU3", "AU4"))
  result <- assessment(Verified_results_table = input)
  fs <- result$Assess_Upload_Report_FS
  expect_equal(fs$ASSESSMENT_UNIT_ID, paste0("AU", 1:4))
  expect_equal(fs$USE_ATTAINMENT_CODE, c("F", "F", "X", "F"))
  expect_equal(fs$PARAM_ATTAINMENT_CODE,
               c("meeting criteria", "meeting criteria", "not enough information", "meeting criteria"))
  for (field in c("PARAM_NAME", "PARAM_STATUS_NAME", "PARAM_POLLUTANT_INDICATOR",
                  "PARAM_STATE_IR_CAT")) {
    expect_true(all(is.na(fs[[field]])), info = field)
  }
  expect_equal(result$Assess_Upload_Report_NS$USE_ATTAINMENT_CODE, "N")
  monitoring <- result$Assess_Upload_Report_3C3B
  expect_equal(monitoring$ASSESSMENT_UNIT_ID, c("AU3", "AU4"))
  expect_equal(monitoring$PARAM_STATE_IR_CAT, c("3B", "3C"))
  expect_equal(monitoring$USE_ATTAINMENT_CODE, c("X", "F"))
})

test_that("SQUID aliases consolidate with the selected category precedence", {
  input <- upload_fixture(
    c("2", "5", "5c", "3c", "5", "5", "5", "5", "5", "5", "4 or 5"),
    c("Temperature, water", "Maximum - Temperature, water", "4T3",
      "Dissolved oxygen (DO)", "Minimum - Dissolved oxygen (DO)",
      "pH", "Maximum - pH", "Specific conductance", "Maximum - Specific conductance",
      "Escherichia coli", "Plant Nutrients"))
  result <- assessment(Verified_results_table = input)
  ns <- result$Assess_Upload_Report_NS
  expect_setequal(ns$PARAM_NAME,
                  c("Temperature", "Dissolved oxygen", "pH", "Specific Conductance", "E. coli", "Nutrients"))
  expect_equal(nrow(ns), 6L)
  expect_equal(subset(ns, PARAM_NAME == "Temperature")$PARAM_STATE_IR_CAT, "5C")
  expect_equal(subset(ns, PARAM_NAME == "Temperature")$PARAM_PRIORITY_RANKING, "Medium")
  expect_true(all(ns$PARAM_PRIORITY_RANKING[ns$PARAM_STATE_IR_CAT == "5A"] == "High"))
  expect_true(all(ns$PARAM_TARGET_TMDL_DATE == 2030L))
  expect_true(all(ns$PARAM_STATUS_NAME == "Cause"))
  expect_true(all(ns$PARAM_POLLUTANT_INDICATOR == "Y"))
  expect_equal(nrow(result$Assess_Upload_Report_3C3B), 0L)
  expect_equal(result$Assess_DU_Res$CHARACTERISTIC_NAME, input$CHARACTERISTIC_NAME)
})

test_that("fractions and segment-specific phosphorus remain distinct", {
  input <- upload_fixture(rep("5", 5),
                           c("Aluminum", "Aluminum", "Plant Nutrients", "Total Phosphorus", "Copper"))
  input$CHR_UID[1:2] <- c(549001, 549002)
  input$SAMPLE_FRACTION <- c("Dissolved", "Total", NA, NA, "Dissolved")
  result <- assessment(Verified_results_table = input)
  expect_setequal(result$Assess_Upload_Report_NS$PARAM_NAME,
                  c("Aluminum, Dissolved", "Aluminum, Total Recoverable", "Nutrients",
                    "Phosphorus, Total", "Copper, Dissolved"))
  expect_equal(nrow(result$Assess_DU_Res), 5L)
})

test_that("all 3C rows for an impaired use carry N and 3B is suppressed", {
  input <- upload_fixture(c("3c", "5", "3b", "3c", "3b"),
                           c("Dissolved oxygen (DO)", "Temperature, water", "Copper",
                             "Plant Nutrients", "Copper"),
                           water_ids = c("AU1", "AU1", "AU1", "AU2", "AU2"))
  result <- assessment(Verified_results_table = input)
  monitoring <- result$Assess_Upload_Report_3C3B
  expect_equal(monitoring$USE_ATTAINMENT_CODE, c("N", "F"))
  expect_true(all(monitoring$PARAM_STATE_IR_CAT == "3C"))
  expect_true(all(monitoring$PARAM_STATUS_NAME == "Monitored"))
  expect_true(all(monitoring$PARAM_ATTAINMENT_CODE == "meeting criteria"))
  expect_true(all(is.na(monitoring$PARAM_PRIORITY_RANKING)))
  expect_true(all(is.na(monitoring$PARAM_TARGET_TMDL_DATE)))
  expect_false(any(result$Assess_Upload_Report_NS$PARAM_STATE_IR_CAT %in% c("3C", "3B")))
})

test_that("multiple stations cannot create conflicting use records", {
  input <- tibble::tibble(
    WATER_ID = rep(c("W_NM-208-04.B", "W_NM-208-01"), each = 2),
    WATER_NAME = c("Station A", "Station B", "Station C", "Station D"),
    DU = "PC", CHR_UID = 2287, CHARACTERISTIC_NAME = "Escherichia coli",
    n_Samples = c(1, 4, 4, 4), Rationale = c("Data Insufficient", rep("Data Sufficient", 3)),
    Exceed = "No")
  result <- assessment(Bacteria_PCR_SCR_table = input)
  expect_equal(nrow(result$Assess_Upload_Report_FS), 2L)
  expect_true(all(result$Assess_Upload_Report_FS$USE_ATTAINMENT_CODE == "F"))
  expect_equal(nrow(result$Assess_Upload_Report_3C3B), 0L)
})

test_that("delistings require explicit reasons and stay out of NS", {
  input <- upload_fixture(c("2", "2", "5"),
                           c("Temperature, water", "Escherichia coli", "pH"))
  input$DELISTING_REASON <- c("Original basis for listing was incorrect.", NA, NA)
  input$Delist_eligible <- "Yes"
  result <- assessment(Verified_results_table = input)
  fs <- result$Assess_Upload_Report_FS
  expect_equal(nrow(fs), 2L)
  delist <- subset(fs, !is.na(PARAM_NAME))
  expect_equal(delist$PARAM_NAME, "Temperature")
  expect_equal(delist$PARAM_STATUS_NAME, "Cause")
  expect_equal(delist$PARAM_ATTAINMENT_CODE, "meeting criteria")
  expect_equal(delist$USE_ATTAINMENT_CODE, "F")
  expect_equal(delist$DELISTING_REASON, input$DELISTING_REASON[1])
  expect_true(is.na(delist$PARAM_STATE_IR_CAT))
  expect_equal(result$Assess_Upload_Report_NS$PARAM_NAME, "pH")
  expect_true(all(is.na(result$Assess_Upload_Report_NS$DELISTING_REASON)))
})

test_that("exports share the full schema and support explicit cycle and run dates", {
  input <- upload_fixture(c("5A", "3C"), c("Temperature, water", "Plant Nutrients"))
  result <- assessment(Verified_results_table = input, Cycle = 2030,
                        Assessment_date = "2028-11-18", Target_TMDL_year = 2029)
  report <- result$Assess_Upload_Report
  expect_equal(ncol(report), 46L)
  expect_true(all(report$CYCLE == 2030L & report$CYCLE_LAST_ASSESSED == 2030L))
  expect_true(all(report$USE_ASMT_DATE == as.integer(as.Date("2028-11-18") - as.Date("1899-12-30"))))
  expect_true(all(is.na(report$STATE_IR_CAT_CODE)))
  expect_equal(result$Assess_Upload_Report_NS$PARAM_TARGET_TMDL_DATE, 2029L)
  parts <- result[c("Assess_Upload_Report_FS", "Assess_Upload_Report_NS", "Assess_Upload_Report_3C3B")]
  expect_equal(sum(vapply(parts, nrow, integer(1))), nrow(report))
  expect_equal(report, dplyr::bind_rows(parts))
  for (part in parts) expect_identical(names(part), names(report))
  expect_error(assessment(Verified_results_table = input, Cycle = 2029), "even-numbered")
  expect_error(assessment(Verified_results_table = input, Assessment_date = "bad date"), "valid date")
  expect_error(assessment(Verified_results_table = input, Target_TMDL_year = NA), "single year")
  expect_equal(assessment(Verified_results_table = result$Assess_DU_Res, Cycle = 2030,
                           Assessment_date = "2028-11-18", Target_TMDL_year = 2029)$Assess_Upload_Report,
               report)
})

test_that("staged uploads retain final use attainment regardless of monitoring row order", {
  input <- upload_fixture(
    c("2", "5", "3c", "3b", "2", "3b", "3c"),
    c("Temperature, water", "pH", "Dissolved oxygen (DO)", "Copper",
      "Escherichia coli", "Copper", "Plant Nutrients"),
    water_ids = c(rep("NM-2116.A_002", 4), "NM-2116.A_000", "AU_X", "AU_3C"))
  input$DELISTING_REASON <- c("Verified temperature delisting.", rep(NA, 3),
                              "Verified E. coli delisting.", NA, NA)
  result <- assessment(Verified_results_table = input)
  fs <- result$Assess_Upload_Report_FS
  ns <- result$Assess_Upload_Report_NS
  monitoring <- result$Assess_Upload_Report_3C3B
  use_keys <- c("ASSESSMENT_UNIT_ID", "USE_NAME")
  parameter_keys <- c(use_keys, "PARAM_NAME")

  baseline <- subset(fs, is.na(PARAM_NAME))
  expect_equal(nrow(baseline), 4L)
  expect_equal(subset(baseline, ASSESSMENT_UNIT_ID == "AU_X")$USE_ATTAINMENT_CODE, "X")
  expect_true(all(baseline$USE_ATTAINMENT_CODE[baseline$ASSESSMENT_UNIT_ID != "AU_X"] == "F"))
  expect_setequal(stats::na.omit(fs$PARAM_NAME), c("Temperature", "E. coli"))
  expect_equal(ns$PARAM_NAME, "pH")
  expect_equal(ns$USE_ATTAINMENT_CODE, "N")
  expect_setequal(monitoring$PARAM_STATE_IR_CAT, c("3C", "3B"))
  expect_equal(nrow(dplyr::inner_join(subset(fs, !is.na(PARAM_NAME)), ns,
                                      by = parameter_keys)), 0L)

  # Model SQUID's last-use-code-wins behavior in the intended upload sequence.
  # Reverse monitoring rows as well: no impaired use may revert from N to F.
  for (order in list(seq_len(nrow(monitoring)), rev(seq_len(nrow(monitoring))))) {
    uploaded <- dplyr::bind_rows(fs, ns, monitoring[order, ]) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(use_keys))) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()
    expect_equal(subset(uploaded, ASSESSMENT_UNIT_ID == "NM-2116.A_002")$USE_ATTAINMENT_CODE, "N")
    expect_equal(subset(uploaded, ASSESSMENT_UNIT_ID == "NM-2116.A_000")$USE_ATTAINMENT_CODE, "F")
    expect_equal(subset(uploaded, ASSESSMENT_UNIT_ID == "AU_X")$USE_ATTAINMENT_CODE, "X")
    expect_equal(subset(uploaded, ASSESSMENT_UNIT_ID == "AU_3C")$USE_ATTAINMENT_CODE, "F")
  }
  expect_equal(result$Assess_Upload_Report, dplyr::bind_rows(fs, ns, monitoring))
})

test_that("an impaired SQUID alias suppresses a conflicting FS delisting", {
  input <- upload_fixture(c("2", "5"),
                           c("Temperature, water", "Maximum - Temperature, water"))
  input$DELISTING_REASON <- c("Proposed delisting before consolidation.", NA)
  result <- assessment(Verified_results_table = input)
  expect_true(all(is.na(result$Assess_Upload_Report_FS$PARAM_NAME)))
  expect_equal(result$Assess_Upload_Report_NS$PARAM_NAME, "Temperature")
  expect_equal(result$Assess_Upload_Report_NS$PARAM_ATTAINMENT_CODE, "not meeting criteria")
  expect_true(all(is.na(result$Assess_Upload_Report_NS$DELISTING_REASON)))
})

rod_input <- function(category = "5", parameter = "Escherichia coli", use = "PC") {
  tibble::tibble(WATER_ID = "AU1", WATER_NAME = "Test waterbody", DU = use,
                 CHR_UID = 1, CHARACTERISTIC_NAME = parameter,
                 Individual_Category = category, n_Samples = 4,
                 n_Samples_assessable = 4, n_Exceed = 3,
                 Rationale = "Data Sufficient", Method = "No more than one exceedance")
}

test_that("copper delisting rationale uses the client's ROD wording", {
  input <- rod_input("2", "Copper", "ColdWAL")
  input$SAMPLE_FRACTION <- "Dissolved"
  input$n_Exceed <- 0
  input$n_Exceed_Acute <- 0
  input$n_Exceed_Chronic <- 0
  input$DELISTING_REASON <- assessment_delisting_reasons()$DELISTING_REASON[1]
  expect_silent(result <- assessment(Verified_results_table = input))
  delist <- subset(result$Assess_Upload_Report_FS, !is.na(PARAM_NAME))
  expected <- "Delist for Copper, Dissolved - 0/4 acute and 0/4 chronic excs."
  expect_equal(delist$PARAM_COMMENT, expected)
  expect_true(all(result$Assess_Upload_Report$ASSESSMENT_RATIONALE == expected))
  expect_equal(assessment(Verified_results_table = result$Assess_DU_Res)$Assess_Upload_Report,
               result$Assess_Upload_Report)
  input$Individual_Category <- "delist"
  explicit <- assessment(Verified_results_table = input)$Assess_Upload_Report_FS
  expect_equal(explicit$PARAM_COMMENT[!is.na(explicit$PARAM_NAME)], expected)
  input$Individual_Category <- "2"
  input$DELISTING_REASON <- NA_character_
  input$ROD_Include <- TRUE
  ordinary <- assessment(Verified_results_table = input)$Assess_Upload_Report
  expect_match(ordinary$ASSESSMENT_RATIONALE[1], "^2 for Copper")
  input$DELISTING_REASON <- "Verified reason."
  input$PARAM_COMMENT <- "Reviewer statement."
  custom <- assessment(Verified_results_table = input)$Assess_Upload_Report
  expect_equal(custom$PARAM_COMMENT[!is.na(custom$PARAM_NAME)], "Reviewer statement.")
})

test_that("grab and toxic statements use compact counts and SQUID categories", {
  input <- rod_input()
  result <- assessment(Verified_results_table = input)
  expect_equal(result$Assess_Upload_Report_NS$PARAM_COMMENT, "5A for primary contact E. coli - 3/4 excs.")
  expect_true(all(result$Assess_Upload_Report$ASSESSMENT_RATIONALE == "5A for primary contact E. coli - 3/4 excs."))

  input <- rod_input("5", "Total Phosphorus", "ColdWAL")
  input$n_Exceed <- 7
  input$n_Samples <- input$n_Samples_assessable <- 12
  input$pct_Exceed <- 58
  input$Criteria_Type <- "SSC"
  expect_equal(assessment(Verified_results_table = input)$Assess_Upload_Report_NS$PARAM_COMMENT,
               "5A for total phosphorus - 7/12 excs (58%) of SSC total phosphorus criterion.")

  input <- rod_input("5c", "Aluminum", "ColdWAL")
  input$SAMPLE_FRACTION <- "Total"
  input$n_Samples <- input$n_Samples_assessable <- 2
  input$n_Exceed <- NA_real_
  input$n_Exceed_Acute <- 1
  input$n_Exceed_Chronic <- 2
  expect_equal(assessment(Verified_results_table = input)$Assess_Upload_Report_NS$PARAM_COMMENT,
               "5C for TR Al - 1/2 acute and 2/2 chronic excs, further data collection needed.")

  input$Individual_Category <- "4A"
  cause <- assessment(Verified_results_table = input)$Assess_Upload_Report_NS
  expect_equal(cause$PARAM_STATE_IR_CAT, "4A")
  expect_match(cause$PARAM_COMMENT, "4A for TR Al", fixed = TRUE)
  expect_true(is.na(cause$PARAM_TARGET_TMDL_DATE))
})

test_that("AU rationale orders parameter statements and carries narratives once", {
  input <- dplyr::bind_rows(rod_input("2", "Copper", "ColdWAL"),
                            rod_input("3c", "Dissolved oxygen (DO)", "ColdWAL"),
                            rod_input("5", "Escherichia coli", "PC"),
                            rod_input("2", "Temperature, water", "ColdWAL"))
  input$CHR_UID <- 1:4
  input$DELISTING_REASON <- c(NA, NA, NA, "Verified reason.")
  input$AV_Notes <- c("Subwatershed narrative.", "Subwatershed narrative.", NA, NA)
  result <- assessment(Verified_results_table = input)
  rationale <- unique(result$Assess_Upload_Report$ASSESSMENT_RATIONALE)
  expect_length(rationale, 1L)
  expect_equal(rationale,
    paste("5A for primary contact E. coli - 3/4 excs.",
          "3C for DO - 3/4 excs.", "Delist for temperature - 3/4 excs.", "Subwatershed narrative."))
  expect_false(grepl("Copper|retained|added|delisted|Assessment unit:", rationale, ignore.case = TRUE))
  expect_equal(result$Assess_Upload_Report_NS$PARAM_COMMENT, "5A for primary contact E. coli - 3/4 excs.")
  expect_false(grepl("Subwatershed", result$Assess_Upload_Report_NS$PARAM_COMMENT))
  expect_equal(assessment(Verified_results_table = result$Assess_DU_Res)$Assess_Upload_Report,
               result$Assess_Upload_Report)
})

test_that("new outside fully supporting data require an explicit ROD flag", {
  input <- rod_input("2", "Copper", "ColdWAL")
  expect_true(all(is.na(assessment(Verified_results_table = input)$Assess_Upload_Report$ASSESSMENT_RATIONALE)))
  input$ROD_Include <- TRUE
  result <- assessment(Verified_results_table = input)
  expect_equal(result$Assess_Upload_Report_FS$ASSESSMENT_RATIONALE, "2 for Copper - 3/4 excs.")
  expect_true(all(is.na(result$Assess_Upload_Report_FS$PARAM_NAME)))
  input$ASSESSMENT_RATIONALE <- "Reviewed AU rationale."
  input$AV_Notes <- "HAB narrative."
  expect_equal(assessment(Verified_results_table = input)$Assess_Upload_Report_FS$ASSESSMENT_RATIONALE,
               "Reviewed AU rationale. HAB narrative.")
})

test_that("LTD aliases and grab evidence form one temperature statement", {
  grab <- rod_input("5", "Maximum - Temperature, water", "ColdWAL")
  grab$Exceed <- "Yes"
  ltd <- grab
  ltd$LTD_Exceedance_Years <- "2023"
  ltd$n_Exceed <- 1
  fourt3 <- ltd
  fourt3$CHR_UID <- 999920
  fourt3$CHARACTERISTIC_NAME <- "4T3"
  six <- fourt3
  six$CHR_UID <- 999921
  six$CHARACTERISTIC_NAME <- "6T3"
  result <- assessment(Conventionals_ALU_table = grab, LTD_ALU_table = dplyr::bind_rows(ltd, fourt3, six))
  statement <- result$Assess_Upload_Report_NS$PARAM_COMMENT
  expect_length(statement, 1L)
  expect_match(statement, "4T3 exceeded in 2023", fixed = TRUE)
  expect_match(statement, "6T3 exceeded in 2023", fixed = TRUE)
  expect_match(statement, "LTD tmax WQC excs in 2023", fixed = TRUE)
  expect_match(statement, "3/4 excs", fixed = TRUE)
  expect_true("LTD_Exceedance_Years" %in% names(result$Assess_DU_Res))
  expect_equal(assessment(Verified_results_table = result$Assess_DU_Res)$Assess_Upload_Report,
               result$Assess_Upload_Report)
})

test_that("nutrient indicators survive assessment and inform ROD statements", {
  stream <- tibble::tibble(WATER_ID = "AU1", WATER_NAME = "Test river", DU = "ColdWAL",
                           Category = "3c", Enrichment_causal = "Yes", Enrichment_DODELTA = "No",
                           Char_Exceeding = "Total Nitrogen,Total Phosphorus")
  result <- assessment(Nutrients_Streams_table = stream)
  expect_equal(result$Assess_Upload_Report_3C3B$PARAM_COMMENT,
               "3C for Plant Nutrients - TN and TP median threshold excs, no delta DO exc.")
  expect_equal(result$Assess_DU_Res$Char_Exceeding, stream$Char_Exceeding)
  lake <- stream
  lake$Category <- "4 or 5"
  lake$Enrichment_response <- "Yes"
  lake$Char_Exceeding <- "Total Nitrogen,Total Phosphorus,Dissolved oxygen (DO)"
  lake$DOsat_flag <- "Flag"
  expect_equal(assessment(Nutrients_Lakes_table = lake)$Assess_Upload_Report_NS$PARAM_COMMENT,
    "5A for Plant Nutrients - causal (TN and TP) enrichment indicator excs, response (DO) enrichment indicator excs, DO saturation >120% flagged.")
  stream$Category <- "Not Assessable: Waterbody Type Mismatch"
  result <- assessment(Nutrients_Streams_table = stream)
  expect_match(result$Assess_Upload_Report_FS$ASSESSMENT_RATIONALE,
    "Nutrients AP not applicable due to waterbody type mismatch, however note TN and TP median threshold excs", fixed = TRUE)
  expect_equal(nrow(result$Assess_Upload_Report_NS), 0L)
})

test_that("missing evidence never invents years durations or nutrient constituents", {
  input <- rod_input("3c", "Minimum - Dissolved oxygen (DO)", "ColdWAL")
  input$Analysis_Source <- "LTD_ALU"
  statement <- assessment(Verified_results_table = input)$Assess_Upload_Report_3C3B$PARAM_COMMENT
  expect_equal(statement, "3C for DO - LTD min DO <WQC.")
  expect_false(grepl("2023|hours|days|4T3", statement))
  input <- rod_input("3c", "Plant Nutrients", "ColdWAL")
  statement <- assessment(Verified_results_table = input)$Assess_Upload_Report_3C3B$PARAM_COMMENT
  expect_false(grepl("TN|TP|chl-a", statement))
  expect_match(statement, "delta DO data unavailable", fixed = TRUE)
})

test_that("aquatic life uses share one AU statement while keeping use-specific comments", {
  input <- dplyr::bind_rows(rod_input("5", "Dissolved oxygen (DO)", "ColdWAL"),
                            rod_input("3c", "Dissolved oxygen (DO)", "WWAL"))
  input$n_Exceed <- c(3, 1)
  result <- assessment(Verified_results_table = input)
  expect_equal(unique(result$Assess_Upload_Report$ASSESSMENT_RATIONALE), "5A for DO - 3/4 excs.")
  expect_equal(result$Assess_Upload_Report_NS$PARAM_COMMENT, "5A for DO - 3/4 excs.")
  expect_equal(result$Assess_Upload_Report_3C3B$PARAM_COMMENT, "3C for DO - 1/4 excs.")
  expect_equal(result$Assess_Upload_Report_3C3B$USE_ATTAINMENT_CODE, "F")
})

av_fixture <- function(category = "2", parameter = "Temperature, water", use = "ColdWAL") {
  tibble::tibble(WATER_ID = "AU1", WATER_NAME = "Test river", DU = use,
    CHR_UID = 1, CHARACTERISTIC_NAME = parameter, Individual_Category = category,
    Rationale = "Data Sufficient", Exceed = "No", n_Samples = 4, n_Exceed = 0)
}

test_that("client delisting defaults are suggested and can be overridden", {
  choices <- assessment_delisting_reasons()
  expect_equal(nrow(choices), 8L)
  expect_equal(choices$Suggested_Delisting_Category,
    c("2", "2", "2", NA, NA, "4A", "4B", "4C"))
  for (i in seq_len(nrow(choices))) {
    input <- av_fixture("delist")
    input$DELISTING_REASON <- choices$DELISTING_REASON[i]
    suggestion <- choices$Suggested_Delisting_Category[i]
    if (is.na(suggestion)) {
      expect_error(assessment(Verified_results_table = input), "Delisting_Category")
      input$Delisting_Category <- "3A"
    }
    result <- assessment(Verified_results_table = input)
    expect_equal(result$Assess_DU_Res$Suggested_Delisting_Category, suggestion)
    fs <- subset(result$Assess_Upload_Report_FS, !is.na(PARAM_NAME))
    expect_equal(fs$DELISTING_REASON, choices$DELISTING_REASON[i])
    if (is.na(suggestion)) {
      expect_equal(fs$PARAM_STATE_IR_CAT, "3A")
      expect_equal(fs$PARAM_ATTAINMENT_CODE, "not enough information")
      expect_equal(fs$USE_ATTAINMENT_CODE, "X")
    } else if (suggestion == "2") {
      expect_equal(fs$PARAM_ATTAINMENT_CODE, "meeting criteria")
      expect_equal(nrow(result$Assess_Upload_Report_NS), 0L)
    } else {
      expect_equal(result$Assess_Upload_Report_NS$PARAM_STATE_IR_CAT, suggestion)
      expect_equal(result$Assess_Upload_Report_NS$USE_ATTAINMENT_CODE, "N")
      expect_true(is.na(result$Assess_Upload_Report_NS$DELISTING_REASON))
    }
    expect_equal(assessment(Verified_results_table = result$Assess_DU_Res), result)

    input$Delisting_Category <- "3B"
    overridden <- assessment(Verified_results_table = input)
    expect_equal(overridden$Assess_DU_Res$Determination, "Not Assessed")
    expect_equal(overridden$Assess_DU_Res$Suggested_Delisting_Category, suggestion)
    expect_equal(subset(overridden$Assess_Upload_Report_FS, !is.na(PARAM_NAME))$PARAM_STATE_IR_CAT, "3B")
  }
  input <- av_fixture("delist")
  expect_error(assessment(Verified_results_table = input), "DELISTING_REASON")
  input$DELISTING_REASON <- "Custom verified reason"
  expect_error(assessment(Verified_results_table = input), "Delisting_Category")
  input$Delisting_Category <- "typo"
  expect_error(assessment(Verified_results_table = input), "Delisting_Category")
})

test_that("reason changes recompute defaults without overwriting explicit AV choices", {
  input <- av_fixture("delist")
  input$DELISTING_REASON <- assessment_delisting_reasons()$DELISTING_REASON[1]
  result <- assessment(Verified_results_table = input)
  edited <- result$Assess_DU_Res
  edited$DELISTING_REASON <- assessment_delisting_reasons()$DELISTING_REASON[6]
  rerun <- assessment(Verified_results_table = edited)
  expect_equal(rerun$Assess_Upload_Report_NS$PARAM_STATE_IR_CAT, "4A")
  expect_equal(rerun$Assess_DU_Res$Suggested_Delisting_Category, "4A")
  expect_true(is.na(rerun$Assess_DU_Res$Delisting_Category))
  edited$Delisting_Category <- "2"
  expect_equal(assessment(Verified_results_table = edited)$Assess_DU_Res$Determination, "Fully Supporting")
})

test_that("AV changes preserve the original decision and explain contradictory evidence", {
  input <- av_fixture()
  input$Individual_Category <- NULL
  original <- assessment(Conventionals_ALU_table = input)
  verified <- original$Assess_DU_Res
  expect_equal(verified$Original_R_Category, "2")
  verified$Individual_Category <- "5A"
  verified$PARAM_COMMENT <- "Reviewer parameter statement."
  verified$ASSESSMENT_RATIONALE <- "Reviewer AU statement."
  result <- assessment(Verified_results_table = verified)
  expect_equal(result$Assess_DU_Res$Original_R_Category, "2")
  expect_equal(result$Assess_DU_Res$Exceed, "No")
  expect_equal(result$Assess_DU_Res$n_Exceed, 0)
  for (text in list(result$Assess_DU_Res$Rationale,
                   result$Assess_Upload_Report_NS$PARAM_COMMENT,
                   result$Assess_Upload_Report_NS$ASSESSMENT_RATIONALE)) {
    expect_match(text, "changed during AV from 2 to 5A.", fixed = TRUE)
  }
  expect_equal(assessment(Verified_results_table = result$Assess_DU_Res), result)
  verified <- result$Assess_DU_Res
  verified$Individual_Category <- "2"
  reverted <- assessment(Verified_results_table = verified)
  expect_true(is.na(reverted$Assess_DU_Res$AV_Change))
  expect_equal(reverted$Assess_DU_Res$Rationale, "Data Sufficient")
  for (category in c("2", "3A", "3B", NA_character_)) {
    input <- av_fixture(category)
    input$Original_R_Category <- "5"
    result <- assessment(Verified_results_table = input)
    expect_match(result$Assess_Upload_Report_FS$ASSESSMENT_RATIONALE, "changed during AV from 5 to", fixed = TRUE)
  }
  input <- av_fixture("5A")
  input$Original_R_Category <- "5"
  expect_true(is.na(assessment(Verified_results_table = input)$Assess_DU_Res$AV_Change))
  expect_true(is.na(assessment(Verified_results_table = av_fixture())$Assess_DU_Res$Original_R_Category))
})

test_that("SQUID overrides are honored before parameter consolidation", {
  input <- dplyr::bind_rows(av_fixture("5A"), av_fixture("2", "Copper"))
  input$CHR_UID <- 1:2
  input$SQUID_PARAM_OVERRIDE <- c(" Custom SQUID name ", "Custom SQUID name")
  result <- assessment(Verified_results_table = input)
  expect_equal(result$Assess_Upload_Report_NS$PARAM_NAME, "Custom SQUID name")
  expect_equal(result$Assess_DU_Res$CHARACTERISTIC_NAME, input$CHARACTERISTIC_NAME)
  expect_equal(assessment(Verified_results_table = result$Assess_DU_Res), result)
  for (blank in c(NA_character_, "", "  ")) {
    input <- av_fixture("5A")
    input$SQUID_PARAM_OVERRIDE <- blank
    expect_equal(assessment(Verified_results_table = input)$Assess_Upload_Report_NS$PARAM_NAME, "Temperature")
  }
})

test_that("3A remains available in the monitoring upload without asserting support", {
  input <- av_fixture("3A")
  result <- assessment(Verified_results_table = input)
  expect_equal(result$Assess_DU_Res$Individual_Category, "3a")
  expect_equal(result$Assess_Upload_Report_3C3B$PARAM_STATE_IR_CAT, "3A")
  expect_equal(result$Assess_Upload_Report_3C3B$USE_ATTAINMENT_CODE, "X")
  expect_equal(result$Assess_Upload_Report_3C3B$PARAM_ATTAINMENT_CODE, "not enough information")
})

test_that("SQUID years and dates are numeric in every report including empty reports", {
  for (cycle in c(2028L, 2030L)) {
    result <- assessment(Verified_results_table = av_fixture("5A"), Cycle = cycle,
                         Assessment_date = "2028-11-18")
    expect_equal(result$Assess_Upload_Report_NS$PARAM_TARGET_TMDL_DATE, cycle + 2L)
    for (report in result[c("Assess_Upload_Report", "Assess_Upload_Report_FS",
                            "Assess_Upload_Report_NS", "Assess_Upload_Report_3C3B")]) {
      for (field in c("CYCLE", "CYCLE_LAST_ASSESSED", "USE_ASMT_DATE", "PARAM_TARGET_TMDL_DATE")) {
        expect_type(report[[field]], "integer")
      }
      expect_true(all(report$USE_ASMT_DATE == as.integer(as.Date("2028-11-18") - as.Date("1899-12-30"))))
    }
  }
})

test_that("selenium dissolved and total recoverable aliases remain separate", {
  uses <- c("DWS", "IRR", "LW", "WH", "ColdWAL", "CoolWAL", "HQColdWAL", "LAL", "MCWAL", "MWWAL", "WWAL")
  input <- dplyr::bind_rows(lapply(uses, function(use) av_fixture("5A", "Selenium", use)))
  input$SAMPLE_FRACTION <- c(rep("Dissolved", 3), rep("Total Recoverable", length(uses) - 3))
  result <- assessment(Verified_results_table = input)
  ns <- result$Assess_Upload_Report_NS
  expect_equal(sum(ns$PARAM_NAME == "Selenium, Dissolved"), 3L)
  expect_equal(sum(ns$PARAM_NAME == "Selenium, Total Recoverable"), length(uses) - 3L)
  input <- dplyr::bind_rows(av_fixture("5A", "Selenium"), av_fixture("5A", "Selenium"))
  input$CHR_UID <- c(1783001, 1783002)
  input$SAMPLE_FRACTION <- c("Dissolved", "Total Recoverable")
  result <- assessment(Verified_results_table = input)
  expect_setequal(result$Assess_Upload_Report_NS$PARAM_NAME, c("Selenium, Dissolved", "Selenium, Total Recoverable"))
  expect_equal(nrow(result$Assess_Upload_Report_NS), 2L)
})

test_that("existing criteria supply fractions without parameter-name rules", {
  input <- dplyr::bind_rows(av_fixture("5A", "Selenium", "DWS"),
    av_fixture("5A", "Selenium", "WH"), av_fixture("5A", "Selenium", "ColdWAL"),
    av_fixture("5A", "Selenium", "ColdWAL"))
  input$CHR_UID <- c(1783001, 1783002, 1783001, 1783002)
  criteria <- NMwateR::example_criteria_processed
  result <- assessment(Verified_results_table = input, Criteria_table = criteria)
  expected <- c("Dissolved", "Total Recoverable", "Dissolved", "Total Recoverable")
  # Result ordering is by AU/use, so compare using keys rather than row order.
  for (i in seq_len(nrow(input))) {
    row <- subset(result$Assess_DU_Res, DU == input$DU[i] & CHR_UID == input$CHR_UID[i])
    expect_equal(row$ASSESSMENT_FRACTION, expected[i])
  }
  expect_equal(sum(result$Assess_Upload_Report_NS$PARAM_NAME == "Selenium, Dissolved"), 2L)
  expect_equal(sum(result$Assess_Upload_Report_NS$PARAM_NAME == "Selenium, Total Recoverable"), 2L)
  expect_true(all(is.na(result$Assess_DU_Res$SAMPLE_FRACTION)))
  rerun <- assessment(Verified_results_table = result$Assess_DU_Res)
  rejoined <- assessment(Verified_results_table = result$Assess_DU_Res, Criteria_table = criteria)
  for (field in setdiff(names(result), "Assess_Indiv_Res")) {
    expect_equal(rerun[[field]], result[[field]])
    expect_equal(rejoined[[field]], result[[field]])
  }
  # The same processed criteria input also works on the initial analysis path.
  initial <- input[1, ]
  initial$Individual_Category <- NULL
  expect_equal(assessment(Toxics_DWS_table = initial, Criteria_table = criteria)$Assess_DU_Res$ASSESSMENT_FRACTION,
    "Dissolved")
})

test_that("fraction joins support arbitrary parameter names without multiplying rows", {
  input <- dplyr::bind_rows(av_fixture("5A", "A new parameter", "DWS"),
    av_fixture("5A", "A new parameter", "WH"))
  input$CHR_UID <- "new-id"
  input$SAMPLE_FRACTION <- "Total"
  criteria <- tibble::tibble(CHR_UID = "new-id", DU = c("DWS", "DWS", "WH"),
    FRACTION = c(" dissolved ", "Dissolved", "Total Recoverable"), Criteria_Type = c("Acute", "Chronic", "WH"))
  result <- assessment(Verified_results_table = input, Criteria_table = criteria)
  expect_equal(nrow(result$Assess_Indiv_Res), 2L)
  expect_equal(result$Assess_DU_Res$ASSESSMENT_FRACTION, c("Dissolved", "Total Recoverable"))
  expect_equal(result$Assess_DU_Res$SAMPLE_FRACTION, c("Total", "Total"))
  input$CHARACTERISTIC_NAME <- "Renamed parameter"
  expect_equal(assessment(Verified_results_table = input, Criteria_table = criteria)$Assess_DU_Res$ASSESSMENT_FRACTION,
    result$Assess_DU_Res$ASSESSMENT_FRACTION)
  edited <- result$Assess_DU_Res
  edited$ASSESSMENT_FRACTION[1] <- "Total Recoverable"
  expect_equal(assessment(Verified_results_table = edited, Criteria_table = criteria)$Assess_DU_Res$ASSESSMENT_FRACTION,
    c("Total Recoverable", "Total Recoverable"))
})

test_that("ambiguous fraction criteria fail instead of silently selecting a fraction", {
  input <- av_fixture("5A", "Copper")
  input$CHR_UID <- 832
  criteria <- tibble::tibble(CHR_UID = 832, DU = "ColdWAL", FRACTION = c("Dissolved", "Total Recoverable"))
  expect_error(assessment(Verified_results_table = input, Criteria_table = criteria), "conflicting FRACTION")
  expect_error(assessment(Verified_results_table = input, Criteria_table = list()), "data frame")
  expect_error(assessment(Verified_results_table = input, Criteria_table = criteria["CHR_UID"]), "missing: DU, FRACTION")
})

test_that("missing fraction metadata is not inferred from IDs or designated uses", {
  input <- av_fixture("5A", "Selenium", "WH")
  input$CHR_UID <- 1783002
  result <- assessment(Verified_results_table = input)
  expect_true(is.na(result$Assess_DU_Res$ASSESSMENT_FRACTION))
  expect_equal(result$Assess_Upload_Report_NS$PARAM_NAME, "Selenium")
  criteria <- tibble::tibble(CHR_UID = 1783002, DU = "DWS", FRACTION = "Dissolved")
  expect_equal(assessment(Verified_results_table = input, Criteria_table = criteria)$Assess_Upload_Report_NS$PARAM_NAME,
    "Selenium")
  # Existing fully qualified SQUID labels remain intact without assigning a
  # fraction to the analytical record by reading that label.
  input$CHARACTERISTIC_NAME <- "Selenium, Total Recoverable"
  qualified <- assessment(Verified_results_table = input)
  expect_true(is.na(qualified$Assess_DU_Res$ASSESSMENT_FRACTION))
  expect_equal(qualified$Assess_Upload_Report_NS$PARAM_NAME, "Selenium, Total Recoverable")
})
