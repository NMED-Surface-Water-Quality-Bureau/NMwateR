test_that("Expect Correct Outputs", {
  example_criteria_table <- NMwateR::example_criteria_table
  example_parameter_table <- NMwateR::example_parameter_table
  example_SQUID_RStudio_table <- NMwateR::example_SQUID_RStudio_table
  example_SQUID_DU_table <- NMwateR::example_SQUID_DU_table
  example_SQUID_LTD_table <- NMwateR::example_SQUID_LTD_table
  example_SQUID_LakeProfile_table <- NMwateR::example_SQUID_LakeProfile_table

  my_data_list <- Data_Prep(criteria_table = example_criteria_table
                          , parameter_table = example_parameter_table
                          , SQUID_RStudio_table = example_SQUID_RStudio_table
                          , SQUID_DU_table = example_SQUID_DU_table
                          , SQUID_LTD_table = example_SQUID_LTD_table
                          , SQUID_LakeProfile_table = example_SQUID_LakeProfile_table)

  # cleanup
  rm(example_criteria_table, example_parameter_table, example_SQUID_RStudio_table
     , example_SQUID_DU_table, example_SQUID_LTD_table, example_SQUID_LakeProfile_table)

  df_Chem_combined <- my_data_list$Chem_Combined
  df_DU_processed <- my_data_list$DU_Processed
  df_Criteria <- my_data_list$Criteria_Formatted

  example_chemistry_processed <- NMwateR::example_chemistry_processed
  example_criteria_processed <- NMwateR::example_criteria_processed
  example_DU_processed <- NMwateR::example_DU_processed

  # chem test
  example_chemistry_processed_num <- sum(example_chemistry_processed$MEASUREMENT_num)
  df_Chem_combined_num <- sum(df_Chem_combined$MEASUREMENT_num)

  testthat::expect_equal(df_Chem_combined_num, example_chemistry_processed_num)

  # criteria test
  example_criteria_processed_num <- sum(example_criteria_processed$Magnitude_Numeric)
  df_Criteria_num <- sum(df_Criteria$Magnitude_Numeric)

  testthat::expect_equal(df_Criteria_num, example_criteria_processed_num)

  # DU test
  example_DU_processed_num <- sum(example_DU_processed$Criteria_Value)
  df_DU_processed_num <- sum(as.numeric(df_DU_processed$Criteria_Value))

  testthat::expect_equal(df_DU_processed_num, example_DU_processed_num)

})

test_that("Check missing fields criteria_table", {
  # example_criteria_table <- NMwateR::example_criteria_table
  example_parameter_table <- NMwateR::example_parameter_table
  example_SQUID_RStudio_table <- NMwateR::example_SQUID_RStudio_table
  example_SQUID_DU_table <- NMwateR::example_SQUID_DU_table

  testthat::expect_error(my_data_list <- Data_Prep(parameter_table = example_parameter_table
                                    # , criteria_table = example_criteria_table
                                      , SQUID_RStudio_table = example_SQUID_RStudio_table
                                      , SQUID_DU_table = example_SQUID_DU_table
                                      , SQUID_LTD_table = NULL
                                      , SQUID_LakeProfile_table = NULL))

  example_criteria_table <- NMwateR::example_criteria_table
  example_criteria_table_v2 <- example_criteria_table %>%
    dplyr::select(-c(CHR_UID, CHR_NAME))

  testthat::expect_error(my_data_list <- Data_Prep(parameter_table = example_parameter_table
                          , criteria_table = example_criteria_table_v2
                            , SQUID_RStudio_table = example_SQUID_RStudio_table
                            , SQUID_DU_table = example_SQUID_DU_table
                            , SQUID_LTD_table = NULL
                            , SQUID_LakeProfile_table = NULL))


})

test_that("Check missing fields parameter_table", {
  example_criteria_table <- NMwateR::example_criteria_table
  # example_parameter_table <- NMwateR::example_parameter_table
  example_SQUID_RStudio_table <- NMwateR::example_SQUID_RStudio_table
  example_SQUID_DU_table <- NMwateR::example_SQUID_DU_table

  testthat::expect_error(my_data_list <- Data_Prep(criteria_table = example_criteria_table
                                      # , parameter_table = example_parameter_table
                                      , SQUID_RStudio_table = example_SQUID_RStudio_table
                                      , SQUID_DU_table = example_SQUID_DU_table
                                      , SQUID_LTD_table = NULL
                                      , SQUID_LakeProfile_table = NULL))

  example_parameter_table <- NMwateR::example_parameter_table
  example_parameter_table_v2 <- example_parameter_table %>%
    dplyr::select(-c(CHR_UID, CHR_NAME))

  testthat::expect_error(my_data_list <- Data_Prep(criteria_table = example_criteria_table
                                     , parameter_table = example_parameter_table_v2
                                     , SQUID_RStudio_table = example_SQUID_RStudio_table
                                     , SQUID_DU_table = example_SQUID_DU_table
                                     , SQUID_LTD_table = NULL
                                     , SQUID_LakeProfile_table = NULL))

})

test_that("Check missing fields SQUID_RStudio_table", {
  example_criteria_table <- NMwateR::example_criteria_table
  example_parameter_table <- NMwateR::example_parameter_table
  # example_SQUID_RStudio_table <- NMwateR::example_SQUID_RStudio_table
  example_SQUID_DU_table <- NMwateR::example_SQUID_DU_table

  testthat::expect_error(my_data_list <- Data_Prep(criteria_table = example_criteria_table
                                    , parameter_table = example_parameter_table
                                  # , SQUID_RStudio_table = example_SQUID_RStudio_table
                                    , SQUID_DU_table = example_SQUID_DU_table
                                    , SQUID_LTD_table = NULL
                                    , SQUID_LakeProfile_table = NULL))

  example_SQUID_RStudio_table <- NMwateR::example_SQUID_RStudio_table
  example_SQUID_RStudio_table_v2 <- example_SQUID_RStudio_table %>%
    dplyr::select(-c(WATER_ID, WATER_NAME))

  testthat::expect_error(my_data_list <- Data_Prep(criteria_table = example_criteria_table
                                , parameter_table = example_parameter_table
                                , SQUID_RStudio_table = example_SQUID_RStudio_table_v2
                                , SQUID_DU_table = example_SQUID_DU_table
                                , SQUID_LTD_table = NULL
                                , SQUID_LakeProfile_table = NULL))

})

test_that("Check missing fields SQUID_DU_table", {
  example_criteria_table <- NMwateR::example_criteria_table
  example_parameter_table <- NMwateR::example_parameter_table
  example_SQUID_RStudio_table <- NMwateR::example_SQUID_RStudio_table
  # example_SQUID_DU_table <- NMwateR::example_SQUID_DU_table

  testthat::expect_error(my_data_list <- Data_Prep(criteria_table = example_criteria_table
                                    , parameter_table = example_parameter_table
                                    , SQUID_RStudio_table = example_SQUID_RStudio_table
                                    # , SQUID_DU_table = example_SQUID_DU_table
                                    , SQUID_LTD_table = NULL
                                    , SQUID_LakeProfile_table = NULL))

  example_SQUID_DU_table <- NMwateR::example_SQUID_DU_table
  example_SQUID_DU_table_v2 <- example_SQUID_DU_table %>%
    dplyr::select(-c(WATER_ID, WATER_NAME))

  testthat::expect_error(my_data_list <- Data_Prep(criteria_table = example_criteria_table
                               , parameter_table = example_parameter_table
                               , SQUID_RStudio_table = example_SQUID_RStudio_table
                               , SQUID_DU_table = example_SQUID_DU_table_v2
                               , SQUID_LTD_table = NULL
                               , SQUID_LakeProfile_table = NULL))

})

test_that("Check missing fields SQUID_LTD_table", {
  example_criteria_table <- NMwateR::example_criteria_table
  example_parameter_table <- NMwateR::example_parameter_table
  example_SQUID_RStudio_table <- NMwateR::example_SQUID_RStudio_table
  example_SQUID_DU_table <- NMwateR::example_SQUID_DU_table
  example_SQUID_LTD_table <- NMwateR::example_SQUID_LTD_table

  example_SQUID_LTD_table_v2 <- example_SQUID_LTD_table %>%
    dplyr::select(-c(ASSESSMENT_UNIT_ID, ASSESSABILITY_QUALIFIER_CODE))

  testthat::expect_error(my_data_list <- Data_Prep(criteria_table = example_criteria_table
                                , parameter_table = example_parameter_table
                                , SQUID_RStudio_table = example_SQUID_RStudio_table
                                , SQUID_DU_table = example_SQUID_DU_table
                                , SQUID_LTD_table = example_SQUID_LTD_table_v2
                                , SQUID_LakeProfile_table = NULL))

})

test_that("Check missing fields SQUID_LakeProfile_table", {
  example_criteria_table <- NMwateR::example_criteria_table
  example_parameter_table <- NMwateR::example_parameter_table
  example_SQUID_RStudio_table <- NMwateR::example_SQUID_RStudio_table
  example_SQUID_DU_table <- NMwateR::example_SQUID_DU_table
  example_SQUID_LakeProfile_table <- NMwateR::example_SQUID_LakeProfile_table

  example_SQUID_LakeProfile_table_v2 <- example_SQUID_LakeProfile_table %>%
    dplyr::select(-c(WATER_ID, MLOC_ID))

  testthat::expect_error(my_data_list <- Data_Prep(criteria_table = example_criteria_table
                              , parameter_table = example_parameter_table
                              , SQUID_RStudio_table = example_SQUID_RStudio_table
                              , SQUID_DU_table = example_SQUID_DU_table
                              , SQUID_LTD_table = NULL
                  , SQUID_LakeProfile_table = example_SQUID_LakeProfile_table_v2))

})
