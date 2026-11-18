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
  expect_match(ns$PARAM_COMMENT, "Method: A strongest.", fixed = TRUE)
  expect_match(ns$PARAM_COMMENT, "Exceedance count: 3;", fixed = TRUE)
  expect_match(ns$PARAM_COMMENT, "Samples: 20;", fixed = TRUE)
  expect_false(grepl("Low N|Fewer exceedances|Fewer samples|Z strongest", ns$PARAM_COMMENT))
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
