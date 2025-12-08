#' Assessment of water quality data using NMED SWQS and CALM logic
#'
#' This function uses the exports from the various water quality analyses and
#' assesses the results using logic from the New Mexico Surface Water Quality
#' Standards (SWQS) and the NMED Consolidated Assessment and Listing Methodology
#' (CALM) guidance manual.
#'
#' @param Conventionals_ALU_table WQ analysis export from Conventionals_ALU() function.
#' @param Bacteria_PCR_SCR_table WQ analysis export from Bacteria_PCR_SCR() function.
#' @param Conventionals_LW_table WQ analysis export from Conventionals_LW() function.
#' @param LTD_ALU_table WQ analysis export from LTD_ALU() function.
#' @param Nutrients_Lakes_table WQ analysis export from Nutrients_Lakes() function.
#' @param Nutrients_Streams_table WQ analysis export from Nutrients_Streams() function.
#' @param pH_PCR_table WQ analysis export from pH_PCR() function.
#' @param Salinity_IRR_table WQ analysis export from Salinity_IRR() function.
#' @param SS_Copper_ALU_table WQ analysis export from SS_Copper_ALU() function.
#' @param Toxics_ALU_nonHDM_table WQ analysis export from Toxics_ALU_nonHDM() function.
#' @param Toxics_ALU_HDM_table WQ analysis export from Toxics_ALU_HDM() function.
#' @param Toxics_DWS_table WQ analysis export from Toxics_DWS() function.
#' @param Toxics_HH_table WQ analysis export from Toxics_HH() function.
#' @param Toxics_IRR_table WQ analysis export from Toxics_IRR() function.
#' @param Toxics_LW_table WQ analysis export from Toxics_LW() function.
#' @param Toxics_WH_table WQ analysis export from Toxics_WH() function.
#' @param Turbidity_ALU_table WQ analysis export from Turbidity_ALU() function.
#'
#' @returns A list of four dataframes. Indiviual results are IR categories assigned
#' to each AU/DU/parameter combination. DU results are IR categories assigned to
#' each AU/DU combination. AU results are IR categories assigned to each AU. Lastly,
#' the upload report is for NMED to upload into SQUID to be transferred to ATTAINS.
#' @examples
#' df_Chem_combined <- NMwateR::example_chemistry_processed
#' df_Criteria <- NMwateR::example_criteria_processed
#' df_DU_processed <- NMwateR::example_DU_processed
#' example_LANL_DU_table <- NMwateR::example_LANL_DU_table
#' example_LANL_WQ_table <- NMwateR::example_LANL_WQ_table
#'
#' # Water quality analyses
#' ## Conventionals ALU
#' df_Conv_ALU <- Conventionals_ALU(Chem_table = df_Chem_combined
#'                                  , DU_table = df_DU_processed)
#'
#' ## Toxics ALU (nonHDM)
#' # Hardness-dependent metals excluded
#' df_Tox_ALU_nHDM <- Toxics_ALU_nonHDM(Chem_table = df_Chem_combined
#'                                      , DU_table = df_DU_processed
#'                                      , Criteria_table = df_Criteria)
#'
#' ## Toxics ALU (HDM)
#' # Hardness-dependent metals only
#' Toxics_ALU_HDM_list <- Toxics_ALU_HDM(Chem_table = df_Chem_combined
#'                                       , DU_table = df_DU_processed
#'                                       , Criteria_table = df_Criteria)
#'
#'
#' df_Toxics_ALU_HDM <- Toxics_ALU_HDM_list$Toxics_ALU_HDM
#'
#' # cleanup
#' rm(Toxics_ALU_HDM_list)
#'
#' ## Conventionals LW
#' df_Conventionals_LW <- Conventionals_LW(Chem_table = df_Chem_combined
#'                                         , DU_table = df_DU_processed
#'                                         , Criteria_table = df_Criteria)
#'
#' ## Salinity IRR ####
#' df_Salinity_IRR <- Salinity_IRR(Chem_table = df_Chem_combined
#'                                 , DU_table = df_DU_processed)
#'
#' ## Toxics HH ####
#' df_Toxics_HH <- Toxics_HH(Chem_table = df_Chem_combined
#'                           , Criteria_table = df_Criteria)
#'
#' ## Site-specific copper
#' #Only used for LANL data
#' SS_Copper_ALU_list <- SS_Copper_ALU(DU_LANL_Stations_table = example_LANL_DU_table
#'                                     , LANL_WQ_data = example_LANL_WQ_table)
#'
#' df_SS_Copper_ALU <- SS_Copper_ALU_list$df_SS_Copper_ALU
#'
#' rm(SS_Copper_ALU_list, example_LANL_DU_table, example_LANL_WQ_table)
#'
#' ## Toxics DWS
#' df_Toxics_DWS <- Toxics_DWS(Chem_table = df_Chem_combined
#'                             , Criteria_table = df_Criteria)
#'
#' ## Toxics IRR
#' df_Toxics_IRR <- Toxics_IRR(Chem_table = df_Chem_combined
#'                             , Criteria_table = df_Criteria)
#'
#' ## Toxics WH
#' df_Toxics_WH <- Toxics_WH(Chem_table = df_Chem_combined
#'                          , Criteria_table = df_Criteria)
#'
#' ## Toxics LW
#' df_Toxics_LW <- Toxics_LW(Chem_table = df_Chem_combined
#'                           , Criteria_table = df_Criteria)
#'
#' ## Bacteria PCR/SCR
#' df_Bacteria_PCR_SCR <- Bacteria_PCR_SCR(Chem_table = df_Chem_combined
#'                                         , DU_table = df_DU_processed)
#'
#' ## pH PCR
#' df_pH_PCR <- pH_PCR(Chem_table = df_Chem_combined
#'                     , DU_table = df_DU_processed)
#'
#' ## Turbidity ALU
#' Turbidity_ALU_list <- Turbidity_ALU(Chem_table = df_Chem_combined
#'                                     , DU_table = df_DU_processed
#'                                    , Criteria_table = df_Criteria)
#'
#' df_Turbidity_ALU <- Turbidity_ALU_list$Turbidity_ALU
#'
#' ## LTD ALU
#' df_LTD_ALU <- LTD_ALU(Chem_table = df_Chem_combined
#'                       , DU_table = df_DU_processed)
#'
#' ## Nutrients (Lakes)
#' Nutrients_Lakes_list <- Nutrients_Lakes(Chem_table = df_Chem_combined
#'                                         , DU_table = df_DU_processed)
#'
#' df_Nutrients_Lakes <- Nutrients_Lakes_list$Nutrients_Lakes
#'
#' # cleanup
#' rm(Nutrients_Lakes_list)
#'
#' ## Nutrients (Streams) ####
#' Nutrients_Streams_list <- Nutrients_Streams(Chem_table = df_Chem_combined
#'                                             , DU_table = df_DU_processed)
#'
#' df_Nutrients_Streams <- Nutrients_Streams_list$Nutrients_Streams
#'
#' # cleanup
#' rm(Nutrients_Streams_list)
#'
#' # Assessment ####
#' assessment_list <- assessment(Conventionals_ALU_table = df_Conv_ALU
#'                               , Bacteria_PCR_SCR_table = df_Bacteria_PCR_SCR
#'                               , Conventionals_LW_table = df_Conventionals_LW
#'                               , LTD_ALU_table = df_LTD_ALU
#'                               , Nutrients_Lakes_table = df_Nutrients_Lakes
#'                               , Nutrients_Streams_table = df_Nutrients_Streams
#'                               , pH_PCR_table = df_pH_PCR
#'                               , Salinity_IRR_table = df_Salinity_IRR
#'                               , SS_Copper_ALU_table = df_SS_Copper_ALU
#'                               , Toxics_ALU_nonHDM_table = df_Tox_ALU_nHDM
#'                               , Toxics_ALU_HDM_table = df_Toxics_ALU_HDM
#'                               , Toxics_DWS_table = df_Toxics_DWS
#'                               , Toxics_HH_table = df_Toxics_HH
#'                               , Toxics_IRR_table = df_Toxics_IRR
#'                               , Toxics_LW_table = df_Toxics_LW
#'                               , Toxics_WH_table = df_Toxics_WH
#'                               , Turbidity_ALU_table = df_Turbidity_ALU)
#' df_Assess_Indiv_Res <- assessment_list$Assess_Indiv_Res
#' df_Assess_DU_Res <- assessment_list$Assess_DU_Res
#' df_Assess_AU_Res <- assessment_list$Assess_AU_Res
#' df_Assess_Upload_Report <- assessment_list$Assess_Upload_Report
#'
#' @export
#'
assessment <- function(Conventionals_ALU_table
                       , Bacteria_PCR_SCR_table = NULL
                       , Conventionals_LW_table = NULL
                       , LTD_ALU_table = NULL
                       , Nutrients_Lakes_table = NULL
                       , Nutrients_Streams_table = NULL
                       , pH_PCR_table = NULL
                       , Salinity_IRR_table = NULL
                       , SS_Copper_ALU_table = NULL
                       , Toxics_ALU_nonHDM_table = NULL
                       , Toxics_ALU_HDM_table = NULL
                       , Toxics_DWS_table = NULL
                       , Toxics_HH_table = NULL
                       , Toxics_IRR_table = NULL
                       , Toxics_LW_table = NULL
                       , Toxics_WH_table = NULL
                       , Turbidity_ALU_table = NULL){

  # QC ####
  # Collect arguments into a named list
  args_list <- list(
    Conventionals_ALU_table = Conventionals_ALU_table,
    Bacteria_PCR_SCR_table = Bacteria_PCR_SCR_table,
    Conventionals_LW_table = Conventionals_LW_table,
    LTD_ALU_table = LTD_ALU_table,
    Nutrients_Lakes_table = Nutrients_Lakes_table,
    Nutrients_Streams_table = Nutrients_Streams_table,
    pH_PCR_table = pH_PCR_table,
    Salinity_IRR_table = Salinity_IRR_table,
    SS_Copper_ALU_table = SS_Copper_ALU_table,
    Toxics_ALU_nonHDM_table = Toxics_ALU_nonHDM_table,
    Toxics_ALU_HDM_table = Toxics_ALU_HDM_table,
    Toxics_DWS_table = Toxics_DWS_table,
    Toxics_HH_table = Toxics_HH_table,
    Toxics_IRR_table = Toxics_IRR_table,
    Toxics_LW_table = Toxics_LW_table,
    Toxics_WH_table = Toxics_WH_table,
    Turbidity_ALU_table = Turbidity_ALU_table)

  # Identify present and missing tables
  present <- names(args_list)[!sapply(args_list, is.null)]
  missing <- names(args_list)[sapply(args_list, is.null)]

  # QC: Remove zero-row tables
  zero_obs <- c()
  for (nm in present) {
    if (nrow(args_list[[nm]]) == 0) {
      zero_obs <- c(zero_obs, nm)
    }
  }

  present <- setdiff(present, zero_obs)

  # Messages for user
  cat("Tables provided: ", ifelse(length(present) > 0
                                  , paste(present, collapse = ", ")
                                  , "None"), "\n")
  cat("Tables missing: ", ifelse(length(missing) > 0
                                 , paste(missing, collapse = ", "), "None"), "\n")
  cat("Tables removed (zero observations): ", ifelse(length(zero_obs) > 0
                                , paste(zero_obs, collapse = ", "), "None"), "\n")

  # Final product: filtered data list
  data_list <- args_list[present]

  # Format data ####
  ## Pull out nutrient files ####
  # safely extract a single DF by name pattern; returns NULL if absent
  get_by_pattern <- function(lst, pattern) {
    nm <- grep(pattern, names(lst), value = TRUE)
    if (length(nm) == 0) return(NULL)
    lst[[nm[1]]]
  }
  df_Nutr_Lakes   <- get_by_pattern(data_list, "^Nutrients_Lake")
  df_Nutr_Streams <- get_by_pattern(data_list, "^Nutrients_Stream")

  # Remove nutrients before merging
  remove_names <- grep("^(Nutrients_Lake|Nutrients_Stream)", names(data_list)
                       , value = TRUE)

  if (length(remove_names) > 0) {
    data_list <- data_list[!names(data_list) %in% remove_names]
  }

  ## Merge ####
  # If no non-nutrient files remain, create an empty results DF with expected columns
  if (length(data_list) == 0) {
    df_AnalysisResults <- tibble::tibble(
      WATER_ID = character(), DU = character(), CHR_UID = numeric(),
      CHARACTERISTIC_NAME = character(), SAMPLE_FRACTION = character(),
      n_Samples = numeric(), n_Samples_assessable = numeric(), Method = character(),
      Rationale = character(), Exceed = character(), n_Exceed = numeric(),
      n_Exceed_Acute = numeric(), n_Exceed_Chronic = numeric(), pct_Exceed = numeric()
    )
  } else {
    # Extract column names from each data frame
    all_field_names <- lapply(data_list, names)
    unique_field_names <- unique(unlist(all_field_names))
    print(unique_field_names)

    # Loop through each data frame and add missing columns
    data_list_aligned <- lapply(data_list, function(df) {
      missing_cols <- setdiff(unique_field_names, names(df))
      df[missing_cols] <- NA
      df <- df[unique_field_names]
      df
    })

    data_list_aligned_chr <- lapply(data_list_aligned, function(df) {
      dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
    })

    # Combine all data frames into one
    df_AnalysisResults <- dplyr::bind_rows(data_list_aligned_chr) %>%
      tibble::as_tibble() %>%
      readr::type_convert()  # re-parse numerics, dates, etc.
  }

  # confirm all results
  sort(unique(df_AnalysisResults$R_Script_Name))

  ## Format ####
  rownames(df_AnalysisResults) <- NULL

  # Define expected columns
  expected_cols <- c("WATER_ID", "WATER_NAME", "DU", "CHR_UID", "CHARACTERISTIC_NAME"
                     , "SAMPLE_FRACTION", "n_Samples", "n_Samples_assessable"
                     , "Method", "Rationale", "Exceed", "n_Exceed"
                     , "n_Exceed_Acute", "n_Exceed_Chronic", "pct_Exceed")

  # Add missing columns with NA
  missing_cols <- setdiff(expected_cols, names(df_AnalysisResults))
  for (col in missing_cols) {
    df_AnalysisResults[[col]] <- NA
  }

  # Select and reorder columns
  df_AnalysisResults_v2 <- df_AnalysisResults %>%
    dplyr::select(dplyr::all_of(expected_cols)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(n_Samples_assessable = dplyr::case_when((!is.na(n_Samples_assessable))
                                            ~ n_Samples_assessable
                                            , (is.na(n_Samples_assessable))
                                            ~ n_Samples))

  ### Human Health ####
  # Relabel Human Health results to correct ALU based on AU.
  ALUs <- c("ColdWAL", "CoolWAL", "HQColdWAL", "LAL", "MCWAL", "MWWAL", "WWAL")

  df_ALUs <- df_AnalysisResults_v2 %>%
    dplyr::filter(DU %in% ALUs) %>%
    dplyr::select(WATER_ID, DU) %>%
    dplyr::distinct()

  df_nonHH <- df_AnalysisResults_v2 %>%
    dplyr::filter(DU != "HH")

  df_HH_fixed <- df_AnalysisResults_v2 %>%
    dplyr::filter(DU == "HH") %>%
    dplyr::select(-c(DU)) %>%
    dplyr::left_join(., df_ALUs, by = "WATER_ID")

  df_AnalysisResults_v3 <- rbind(df_nonHH, df_HH_fixed)
  unique(df_AnalysisResults_v3$DU)

  # cleanup
  rm(df_AnalysisResults, df_AnalysisResults_v2, ALUs, df_ALUs, df_nonHH
     , df_HH_fixed)

  # Individual Parameter Determinations ####
  # Fully Supporting, Not Supporting, and Not Assessed.
  df_indiv_results <- df_AnalysisResults_v3 %>%
    dplyr::mutate(Determination = dplyr::case_when((Rationale == "Data Insufficient")
                                     ~ "Not Assessed"
                                     , (Rationale == "Data Sufficient; Low N Samples")
                                     ~ "Not Assessed"
                                     , (Exceed == "Yes") ~ "Not Supporting"
                                     , (Exceed == "No") ~ "Fully Supporting"
                                     , TRUE ~ "Error - New logic necessary")
           , Individual_Category = dplyr::case_when((Determination == "Not Assessed"
                                              & Exceed == "No") ~ "3b"
                                             , (Determination == "Not Assessed"
                                                & Exceed == "Yes") ~ "3c"
                                             , (Determination == "Not Supporting"
                                                & CHR_UID == 1977) ~ "5c"
                                             , (Determination == "Not Supporting")
                                             ~ "5" # could be updated to be "4 or 5"
                                             , (Determination == "Fully Supporting")
                                             ~ "2" # could be updated to "1 or 2"
                                             , TRUE ~ NA))

  # Integrate nutrient results ####
  # Fallback empty nutrient frames if missing
  empty_nutr <- tibble::tibble(
    WATER_ID = character(),
    WATER_NAME = character(),
    DU = character(),
    Category = character()
  )
  if (is.null(df_Nutr_Lakes))   df_Nutr_Lakes   <- empty_nutr
  if (is.null(df_Nutr_Streams)) df_Nutr_Streams <- empty_nutr

  # cleanup
  rm(empty_nutr)

  df_Nutr_Lakes_v2 <- df_Nutr_Lakes %>%
    dplyr::select(WATER_ID, WATER_NAME, DU, Category) %>%
    dplyr::mutate(CHARACTERISTIC_NAME = "Plant Nutrients"
           , Method = "Lake Nutrients Assessment"
           , Determination = dplyr::case_when((Category == "1 or 2") ~ "Fully Supporting"
                                       , (Category == "3a"
                                          | Category == "3b"
                                          | Category == "3c") ~ "Not Assessed"
                                       , (Category == "4 or 5") ~ "Not Supporting")) %>%
    dplyr::rename(Individual_Category = Category)

  df_Nutr_Streams_v2 <- df_Nutr_Streams %>%
    dplyr::select(WATER_ID, WATER_NAME, DU, Category) %>%
    dplyr::mutate(CHARACTERISTIC_NAME = "Plant Nutrients"
           , Method = "Stream Nutrients Assessment"
           # If the stream was not an assessable waterbody type, assign the category as NA at this stage.
           , Category = dplyr::case_when((Category == "Not Assessable: Waterbody Type Mismatch")
                                  ~ NA_character_
                                  , (grepl("1 or 2", Category)) ~ "1 or 2"
                                  , TRUE ~ Category)
           , Determination = dplyr::case_when((Category == "1 or 2") ~ "Fully Supporting"
                                       , (Category == "3a"
                                          | Category == "3b"
                                          | Category == "3c") ~ "Not Assessed"
                                       , (Category == "4 or 5"
                                          | Category == "5c") ~ "Not Supporting")) %>%
    dplyr::rename(Individual_Category = Category)

  missing_cols <- setdiff(names(df_indiv_results), names(df_Nutr_Lakes_v2))
  df_Nutr_Lakes_v2[missing_cols] <- NA
  df_Nutr_Streams_v2[missing_cols] <- NA

  df_indiv_results_v2 <- rbind(df_indiv_results, df_Nutr_Lakes_v2
                               , df_Nutr_Streams_v2)

  # cleanup
  rm(df_Nutr_Lakes, df_Nutr_Lakes_v2, df_Nutr_Streams, df_Nutr_Streams_v2
     , missing_cols, df_indiv_results)

  # DU Determinations ####
  df_DU_results <- df_indiv_results_v2 %>%
    dplyr::ungroup() %>%
    dplyr::group_by(WATER_ID, WATER_NAME,  DU, CHR_UID, CHARACTERISTIC_NAME) %>%
    dplyr::mutate(n = dplyr::n()
           # This seciton could be updated to be more refined
           # See NMED CALM pg. 45 (NMED IR Categories)
           , is_2 = sum(ifelse(Individual_Category == "2"
                               | Individual_Category == "1 or 2", 1, 0))
           , is_3 = sum(ifelse(Individual_Category == "3a"
                               | Individual_Category == "3b"
                               | Individual_Category == "3c", 1, 0))
           , is_5 = sum(ifelse(Individual_Category == "4 or 5"
                               | Individual_Category == "5"
                               | Individual_Category == "5c", 1, 0))
           #If n > 1, choose worse category
           , new_Individual_Category = dplyr::case_when((n > 1 & is_5 == 1) ~"5"
                                                 , (n > 1 & is_5 == 0
                                                    & is_2 > 0) ~ "2"
                                                 , TRUE ~ Individual_Category)) %>%
    dplyr::filter(Individual_Category == new_Individual_Category) %>%
    dplyr::select(!c(Individual_Category, n, is_2, is_3, is_5)) %>%
    dplyr::rename(Individual_Category = new_Individual_Category) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(WATER_ID, DU) %>%
    dplyr::mutate(cat_5_present = length(Individual_Category[Individual_Category=="5"
                                                             | Individual_Category=="5c"
                                                             | Individual_Category=="4 or 5"])
                  , cat_2_present = length(Individual_Category[Individual_Category=="2"])
                  , Use_Category = dplyr::case_when((cat_5_present > 0) ~ "5"
                                             , (cat_5_present == 0
                                                & cat_2_present > 0) ~ "2"
                                             , (cat_5_present == 0
                                                & cat_2_present == 0) ~ "3"
                                             , TRUE ~ NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(WATER_ID, DU)

  # AU Determinations ####
  df_AU_results <- df_DU_results %>%
    dplyr::group_by(WATER_ID, WATER_NAME) %>%
    dplyr::mutate(cat_5_present = length(Use_Category[Use_Category=="5"]),
                  cat_2_present = length(Use_Category[Use_Category=="2"]),
                  Overall_Category = dplyr::case_when((cat_5_present > 0) ~ "5"
                                               , (cat_5_present == 0
                                                  & cat_2_present > 0) ~ "2"
                                               , (cat_5_present == 0
                                                  & cat_2_present == 0) ~ "3"
                                               , TRUE ~ NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(WATER_ID)

  # Upload Report ####
  df_upload <- df_AU_results %>%
    dplyr::rename(ASSESSMENT_UNIT_ID = WATER_ID
           , AU_NAME_REF_ONLY = WATER_NAME
           , PARAM_NAME = CHARACTERISTIC_NAME
           , PARAM_STATE_IR_CAT = Individual_Category
           , STATE_IR_CAT_CODE = Overall_Category) %>%
    dplyr::mutate(USE_NAME = dplyr::case_when(DU == "ColdWAL"   ~ "Coldwater Aquatic Life"
                                , DU == "CoolWAL"   ~ "Coolwater Aquatic Life"
                                , DU == "DWS"       ~ "Domestic Water Supply"
                                , DU == "HQColdWAL" ~ "High Quality Coldwater Aquatic Life"
                                , DU == "IRR"       ~ "Irrigation"
                                , DU == "LAL"       ~ "Limited Aquatic Life"
                                , DU == "LW"        ~ "Livestock Watering"
                                , DU == "MCWAL"     ~ "Marginal Coldwater Aquatic Life"
                                , DU == "MWWAL"     ~ "Marginal Warmwater Aquatic Life"
                                , DU == "PC"        ~ "Primary Contact"
                                , DU == "PWS"       ~ "Public Water Supply"
                                , DU == "SC"        ~ "Secondary Contact"
                                , DU == "WWAL"      ~ "Warmwater Aquatic Life"
                                , DU == "WH"        ~ "Wildlife Habitat"
                                , TRUE              ~ NA)) %>%
    dplyr::mutate(PARAM_ATTAINMENT_CODE = dplyr::case_when((Determination == "Fully Supporting")
                                             ~ "meeting criteria"
                                             , (Determination == "Not Supporting")
                                             ~ "not meeting criteria"
                                             , (Determination == "Not Assessed")
                                             ~ "not enough information"))


  req_cols <- c("ASSESSMENT_UNIT_ID", "AU_NAME_REF_ONLY", "CYCLE", "CYCLE_LAST_ASSESSED"
                , "ASSESSMENT_COMMENT", "ASSESSMENT_RATIONALE", "USE_NAME", "USE_ATTAINMENT_CODE"
                , "USE_ASMT_DATE", "PARAM_NAME", "PARAM_STATUS_NAME", "PARAM_ATTAINMENT_CODE"
                , "PARAM_POLLUTANT_INDICATOR", "PARAM_TARGET_TMDL_DATE", "PARAM_EXPECTED_TO_ATTAIN"
                , "PARAM_PRIORITY_RANKING", "DELISTING_REASON", "PARAM_STATE_IR_CAT"
                , "STATE_IR_CAT_CODE", "AGENCY_CODE", "YEAR_LAST_MONITORED", "TROPHIC_STATUS"
                , "USE_AGENCY_CODE", "USE_TREND", "USE_THREATENED", "USE_ASMT_BASIS"
                , "USE_MONITORING_START", "USE_MONITORING_END", "USE_ASSESSOR_NAME", "USE_COMMENT"
                , "USE_ASMT_TYPE", "USE_ASMT_CONFIDENCE", "USE_ASMT_METHOD_CODE"
                , "USE_ASMT_METHOD_CONTEXT", "USE_ASMT_METHOD_NAME", "PARAM_USE_NAME"
                , "PARAM_TREND", "PARAM_COMMENT", "PARAM_AGENCY_CODE", "PARAM_YEAR_LISTED"
                , "PARAM_CONSENT_DECREE_CYCLE", "PARAM_ALT_LISTING_ID", "SOURCE_PARAM_NAME"
                , "SOURCE_NAME", "SOURCE_CONFIRMED", "SOURCE_COMMENT")

  # missing fields to add as NA
  setdiff(req_cols, names(df_upload))
  missing_cols <- setdiff(req_cols, names(df_upload))
  df_upload_v2 <- df_upload
  df_upload_v2[missing_cols] <- NA
  df_upload_v2 <- df_upload_v2[req_cols]

  # Export data ####
  return(list(
    Assess_Indiv_Res = df_indiv_results_v2,
    Assess_DU_Res = df_DU_results,
    Assess_AU_Res = df_AU_results,
    Assess_Upload_Report = df_upload_v2))

} # END ~ Function
