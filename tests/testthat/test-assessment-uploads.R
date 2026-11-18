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
                  "PARAM_STATE_IR_CAT", "ASSESSMENT_RATIONALE")) {
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
  expect_true(all(ns$PARAM_TARGET_TMDL_DATE == "2027"))
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
                    "Phosphorus, Total", "Copper"))
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
  expect_true(all(report$CYCLE == "2030" & report$CYCLE_LAST_ASSESSED == "2030"))
  expect_true(all(report$USE_ASMT_DATE == "11/18/2028"))
  expect_true(all(is.na(report$STATE_IR_CAT_CODE)))
  expect_equal(result$Assess_Upload_Report_NS$PARAM_TARGET_TMDL_DATE, "2029")
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
