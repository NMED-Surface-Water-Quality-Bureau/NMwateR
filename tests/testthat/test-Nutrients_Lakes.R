lake_concurrence_input <- function() {
  chemistry <- tidyr::expand_grid(
    DATE = as.Date(c("2024-07-01", "2024-07-22")),
    CHR_UID = c(1648, 985, 1418, 1416, 1674, 791, 986)) %>%
    dplyr::mutate(WATER_ID = "LAKE1", WATER_NAME = "Test lake", PROJECT_NAME = "Test project",
      STATION = "STATION1", STATION_NAME = "Test station", TIME = "12:00:00",
      SAMPLING_EVENT_TYPE = ifelse(CHR_UID %in% c(1648, 985, 986),
        "Lake Depth Profile Summary", "LAKE-CHEMICAL"),
      SAMPLE_TYPE = "Regular", ACTIVITY_TYPE = "Sample", SAMPLE_FRACTION = "Total",
      CHARACTERISTIC_NAME = dplyr::case_when(CHR_UID == 1648 ~ "pH",
        CHR_UID == 985 ~ "Dissolved oxygen (DO)", CHR_UID == 1418 ~ "NO2 + NO3",
        CHR_UID == 1416 ~ "TKN", CHR_UID == 1674 ~ "Phosphorus as P",
        CHR_UID == 791 ~ "Chlorophyll a", CHR_UID == 986 ~ "DO saturation"),
      MEASUREMENT_num = dplyr::case_when(CHR_UID == 1648 ~ 7,
        CHR_UID == 985 ~ 8, CHR_UID == 1418 ~ 0.05, CHR_UID == 1416 ~ 0.1,
        CHR_UID == 1674 ~ 0.01, CHR_UID == 791 ~ 0.001, CHR_UID == 986 ~ 100),
      UNITS = "mgL")
  uses <- tibble::tibble(WATER_ID = "LAKE1", DU = "ColdWAL", Waterbody = "RESERVOIR",
    ECOREGION = "21a", CHR_UID = c(1648, 1648, 985),
    Criteria_Name = c("PH_LOW", "PH_HIGH", "DO_WQC"), Criteria_Value = c(6.6, 9, 6))
  list(chemistry = chemistry, uses = uses)
}

test_that("Nutrients Lakes calc works", {
  df_Chem_combined <- NMwateR::example_chemistry_processed
  df_DU_processed <- NMwateR::example_DU_processed

  ## Nutrients (Lakes) ####
  Nutrients_Lakes_list <- Nutrients_Lakes(Chem_table = df_Chem_combined
                                          , DU_table = df_DU_processed)

  df_Nutrients_Lakes <- Nutrients_Lakes_list$Nutrients_Lakes

  n_row_code <- nrow(df_Nutrients_Lakes)
  n_unique_WATER_ID_code <- length(unique(df_Nutrients_Lakes$WATER_ID))
  n_unique_Category_code <- length(unique(df_Nutrients_Lakes$Category))

  # One conservative result per AU/use; the former extra row for Heron had
  # no category and must not duplicate its impaired station result.
  n_row_dev <- 3
  n_unique_WATER_ID_dev <- 3
  n_unique_Category_dev <- 1

  testthat::expect_equal(n_row_code, n_row_dev)
  testthat::expect_equal(n_unique_WATER_ID_code, n_unique_WATER_ID_dev)
  testthat::expect_equal(n_unique_Category_code, n_unique_Category_dev)
  expect_equal(anyDuplicated(df_Nutrients_Lakes[c("WATER_ID", "DU")]), 0L)
  reservoirs <- subset(df_Nutrients_Lakes, WATER_ID %in% c("NM-2114_00", "NM-2117_10"))
  expect_equal(nrow(reservoirs), 2L)
  expect_true(all(reservoirs$Category == "4 or 5"))

})

test_that("lake station results roll up to one conservative assessment per AU/use", {
  input <- lake_concurrence_input()
  supporting <- input$chemistry
  impaired <- supporting
  impaired$STATION <- "STATION2"
  impaired$STATION_NAME <- "Impaired station"
  impaired$MEASUREMENT_num[impaired$CHR_UID == 1674] <- 0.1
  impaired$MEASUREMENT_num[impaired$CHR_UID == 985] <- 4
  sparse <- impaired[impaired$CHR_UID == 1674, ]
  sparse$STATION <- "STATION3"
  sparse$STATION_NAME <- "Incomplete station"
  chemistry <- dplyr::bind_rows(supporting, impaired, sparse)
  result <- Nutrients_Lakes(chemistry, input$uses)
  expect_equal(nrow(result$Nutrients_Lakes), 1L)
  expect_equal(result$Nutrients_Lakes$Category, "4 or 5")
  expect_setequal(result$Nutrients_Lakes_Indiv_Res$STATION, c("STATION1", "STATION2", "STATION3"))
  expect_equal(Nutrients_Lakes(chemistry[nrow(chemistry):1, ], input$uses)$Nutrients_Lakes,
    result$Nutrients_Lakes)
  assessed <- assessment(Nutrients_Lakes_table = result$Nutrients_Lakes)
  expect_equal(nrow(assessed$Assess_DU_Res), 1L)
  expect_equal(assessed$Assess_Upload_Report_NS$PARAM_STATE_IR_CAT, "5A")
  expect_equal(assessment(Verified_results_table = assessed$Assess_DU_Res)$Assess_Upload_Report,
    assessed$Assess_Upload_Report)

  second_use <- input$uses
  second_use$DU <- "HQColdWAL"
  multiple_uses <- Nutrients_Lakes(chemistry, dplyr::bind_rows(input$uses, second_use))
  expect_equal(nrow(multiple_uses$Nutrients_Lakes), 2L)
  expect_setequal(multiple_uses$Nutrients_Lakes$DU, c("ColdWAL", "HQColdWAL"))
})

test_that("lake concurrence identifies renamed phosphorus and calculated nitrogen by ID", {
  input <- lake_concurrence_input()
  result <- Nutrients_Lakes(input$chemistry, input$uses)
  expect_equal(unique(result$Nutrients_Lakes$n_concurrentdates), 2L)
  expect_equal(unique(result$Nutrients_Lakes$concurrence_met), "Yes")
  expect_true(all(c(1415, 1674) %in% result$Nutrients_Lakes_Indiv_Res$CHR_UID))
  expect_true("Total Phosphorus" %in% result$Nutrients_Lakes_Indiv_Res$CHARACTERISTIC_NAME)
  expect_true("Total Nitrogen" %in% result$Nutrients_Lakes_Indiv_Res$CHARACTERISTIC_NAME)
  expect_equal(assessment(Nutrients_Lakes_table = result$Nutrients_Lakes)$Assess_DU_Res$Use_Category, "2")

  input$chemistry$CHARACTERISTIC_NAME <- paste("Display label", input$chemistry$CHR_UID)
  renamed <- Nutrients_Lakes(input$chemistry, input$uses)
  expect_equal(unique(renamed$Nutrients_Lakes$n_concurrentdates), 2L)
  expect_equal(unique(renamed$Nutrients_Lakes$concurrence_met), "Yes")
})

test_that("lake concurrence requires all indicators in two qualifying events", {
  for (missing_id in c(1674, 1418, 791, 1648, 985)) {
    input <- lake_concurrence_input()
    chemistry <- input$chemistry %>%
      dplyr::filter(!(DATE == as.Date("2024-07-22") & CHR_UID == missing_id))
    result <- Nutrients_Lakes(chemistry, input$uses)$Nutrients_Lakes
    expect_equal(unique(result$n_concurrentdates), 1L, info = paste("missing", missing_id))
    expect_equal(unique(result$concurrence_met), "No", info = paste("missing", missing_id))
  }
})

test_that("lake concurrence retains the seven-day sampling window", {
  for (offset in c(7, 8)) {
    input <- lake_concurrence_input()
    input$chemistry$DATE[input$chemistry$CHR_UID == 1674] <-
      input$chemistry$DATE[input$chemistry$CHR_UID == 1674] + offset
    result <- Nutrients_Lakes(input$chemistry, input$uses)$Nutrients_Lakes
    expect_equal(unique(result$n_concurrentdates), if (offset == 7) 2L else 0L)
    expect_equal(unique(result$concurrence_met), if (offset == 7) "Yes" else "No")
  }
})

test_that("causal enrichment with no response uses the standard full-support label", {
  input <- lake_concurrence_input()
  input$chemistry$MEASUREMENT_num[input$chemistry$CHR_UID == 1674] <- 0.1
  result <- Nutrients_Lakes(input$chemistry, input$uses)
  expect_equal(result$Nutrients_Lakes$Enrichment_causal, "Yes")
  expect_equal(result$Nutrients_Lakes$Enrichment_response, "No")
  expect_equal(result$Nutrients_Lakes$Enrichment_chlorophyll, "No")
  expect_equal(result$Nutrients_Lakes$Category, "1 or 2")
  assessed <- assessment(Nutrients_Lakes_table = result$Nutrients_Lakes)
  expect_equal(assessed$Assess_DU_Res$Determination, "Fully Supporting")
  expect_equal(assessed$Assess_DU_Res$Use_Category, "2")
})
