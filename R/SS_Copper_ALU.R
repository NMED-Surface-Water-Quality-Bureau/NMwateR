#' Analysis of copper data from LANL against site-specific ALU standards
#'
#' This function compares copper data from Los Alamos National Laboratory (LANL)
#' against site-specific water quality standards for certain aquatic life uses
#' (ALU). For more information, see the NMED Consolidated Assessment and
#' Listing Methodology (CALM) guidance manual and New Mexico SQWS.
#'
#' @param DU_LANL_Stations_table Stations DU table for LANL monitoring locations.
#' @param LANL_WQ_data Water quality data table provided by LANL to NMED.
#'
#' @returns A list of three dataframes. The first contains analyzed LANL copper
#' data compared to site-specific water quality criteria. The second, labeled
#' "Indiv_Res" is an intermediate file used for QA/QC purposes. The third, labeled
#' "Insuff_Res" is a dataset of copper criteria with missing predictor parameters
#' for copper calculations.
#'
#' @examples
#' \dontrun{
#' SS_Copper_ALU_list <- SS_Copper_ALU(DU_LANL_Stations_table = example_DU_LANL_Sites
#' , LANL_WQ_data = example_LANL_WQ_data)}
#' @export
#'
SS_Copper_ALU <- function(DU_LANL_Stations_table
                       , LANL_WQ_data){

  # QC ####
  # QC messages for required files
  if (missing(DU_LANL_Stations_table)) {
    stop(paste0("Error: 'DU_LANL_Stations_table' is required but was not provided. ",
                "This is a stations DU table for LANL sites specifically."
    ))}

  if (missing(LANL_WQ_data)) {
    stop(paste0("Error: 'LANL_WQ_data' is required but was not provided. ",
                "This is a table of water quality data from LANL."
    ))}

  # Fix "u" field (throws R Package error)
  names(LANL_WQ_data) <- gsub("\u00B5", "u", names(LANL_WQ_data), fixed = TRUE)
  names(LANL_WQ_data) <- gsub("<c2><b5>", "u", names(LANL_WQ_data), fixed = TRUE)
  # names(LANL_WQ_data) <- gsub("Reported Value[[:space:]]*\\(if \"<\", leave blank\\)",
  #                   "reported_value",
  #                   names(LANL_WQ_data))

  # Format data ####
  RFunctionName <- "SS_Copper_ALU"

  ALUs <- c("ColdWAL", "LAL", "MCWAL", "MWWAL")

  #Characteristics in LANL data
  LANL_WQ_data %>% dplyr::select(`Parameter Name`) %>% unique()

  df_LANL_v2 <- LANL_WQ_data %>%
    #remove last row, table info
    dplyr::filter(dplyr::row_number() <= dplyr::n() - 1) %>%
    dplyr::rename(WATER_ID = `AU ID`,
                  WATER_NAME = `AU / Waterbody Name`,
                  STATION = `Station ID`,
                  STATION_NAME = `Station Name`,
                  PROJECT_NAME = `Submitter / Data Source`,
                  CHARACTERISTIC_NAME = `Parameter Name`,
                  MEASUREMENT_num = `Reported Value` ,
                  UNITS = Unit,
                  SAMPLING_EVENT_TYPE = `Sample Media`,
                  SAMPLE_FRACTION = `Sample Fraction`,
                  LESS_THAN_YN = `Less than detection limit?`) %>%
    dplyr::mutate(MEASUREMENT = as.character(MEASUREMENT_num),
                  `Sample Date and Time` = lubridate::mdy_hm(`Sample Date and Time`),
                  DATE = as.Date(`Sample Date and Time`),
                  TIME = format(`Sample Date and Time`, "%H:%M:%S"),
                  SAMPLING_EVENT_TYPE = dplyr::case_when(SAMPLING_EVENT_TYPE == "W"
                                                         | SAMPLING_EVENT_TYPE == "Wa" ~
                                                           'Water'),
                  SAMPLE_TYPE = SAMPLING_EVENT_TYPE,
                  LESS_THAN_YN = ifelse(LESS_THAN_YN == "Yes", "Y", "N"),
                  CHR_UID = dplyr::case_when(CHARACTERISTIC_NAME == 'Dissolved Organic Carbon'
                                             ~ 2174,
                                             CHARACTERISTIC_NAME == 'Copper' ~
                                               832,
                                             CHARACTERISTIC_NAME == 'Hardness' ~
                                               4528,
                                             CHARACTERISTIC_NAME == 'Acidity or Alkalinity of a solution' ~
                                               1648,
                                             CHARACTERISTIC_NAME == 'Total Organic Carbonl' ~
                                               NA,
                                             T ~ NA),
                  CHARACTERISTIC_NAME = ifelse(CHARACTERISTIC_NAME == 'Acidity or Alkalinity of a solution',
                                               'pH', CHARACTERISTIC_NAME),
                  ACTIVITY_TYPE = NA,
                  FLT_10UG = NA,
                  ASSESSABILITY_QUALIFIER_CODE = NA,
                  ACT_START_DATE = NA,
                  ACT_END_DATE = NA) %>%
    dplyr::select(!c(`Sample Date and Time`, `Sample Detection Limit (SDL)`,
                     `SDL Units`, `Analysis Date and Time`, `Analytical Method`,
                     `Dilution Factor`, `Lab Name`, `SSWQC Copper Chronic (ug/L)`,
                     `Submitter Qualifier Code`, `Lab Qualifier Code`,
                     `SSWQC Copper Acute (ug/L)`)) %>%
    #filter out unnecessary characteristics
    dplyr::filter(!is.na(CHR_UID))

  df_DU_v2 <- DU_LANL_Stations_table %>%
    dplyr::select(WATER_ID, DU) %>%
    dplyr::distinct() %>%
    dplyr::filter(DU %in% ALUs)

  #Combine df_Chem and formatted LANL data
  df_Chem_v2 <- dplyr::left_join(df_LANL_v2
                                 , df_DU_v2, by = "WATER_ID")

  # Toxics ALU ####
  CHR_UID_ToxicALU <- 832 # copper

  ## Trim chem data ####
  df_Chem_v3 <- df_Chem_v2 %>%
    dplyr::filter(CHR_UID %in% CHR_UID_ToxicALU # Hardness-dependent metals
                  | CHR_UID == 4528 # Total Hardness
                  | CHR_UID == 2174 # DOC
                  | CHR_UID == 1648) # pH

  ## Adjust units ####
  # Per SWQS, metals should be ug/L not mg/L
  df_Chem_v4 <- df_Chem_v3 %>%
    dplyr::select(-c(MEASUREMENT)) %>% # not used; holdover from previous
    dplyr::mutate(MEASUREMENT_num = dplyr::case_when((CHR_UID %in% CHR_UID_ToxicALU
                                                      & UNITS == "mgL")
                                                     ~ MEASUREMENT_num*1000
                                                     , TRUE ~ as.numeric(MEASUREMENT_num))
                  , UNITS = dplyr::case_when((CHR_UID %in% CHR_UID_ToxicALU) ~ "ugL"
                                             , TRUE ~ "mgL"))

  ## SS Copper AUs ####
  # AUs for segment-specific copper criteria (Pajarito Plateau)
  water_ids <- c("NM-9000.A_054", "NM-9000.A_055", "NM-9000.A_046", "NM-128.A_03"
                 , "NM-9000.A_005","NM-2118.A_70", "NM-126.A_03", "NM-97.A_002"
                 , "NM-97.A_007", "NM-128.A_14","NM-128.A_10", "NM-97.A_005"
                 , "NM-97.A_003", "NM-9000.A_063", "NM-127.A_00","NM-9000.A_006"
                 , "NM-9000.A_000", "NM-9000.A_049", "NM-9000.A_043", "NM-99.A_001"
                 , "NM-97.A_006", "NM-9000.A_045", "NM-97.A_029", "NM-97.A_004"
                 , "NM-128.A_00", "NM-128.A_17", "NM-128.A_16", "NM-126.A_01"
                 , "NM-128.A_08", "NM-9000.A_040", "NM-128.A_06", "NM-9000.A_048"
                 , "NM-128.A_07", "NM-9000.A_091", "NM-128.A_15", "NM-9000.A_053"
                 , "NM-9000.A_042", "NM-9000.A_047", "NM-128.A_11", "NM-128.A_01"
                 , "NM-126.A_00", "NM-9000.A_051", "NM-128.A_02", "NM-128.A_04"
                 , "NM-128.A_05", "NM-128.A_09", "NM-9000.A_044", "NM-9000.A_052"
                 , "NM-128.A_12", "NM-128.A_13")

  applicability <- c("Acute Only", "Acute Only", "Acute Only", "Acute Only"
                     , "Acute And Chronic","Acute And Chronic", "Acute And Chronic"
                     , "Acute And Chronic", "Acute And Chronic", "Acute Only"
                     , "Acute Only", "Acute And Chronic", "Acute And Chronic"
                     , "Acute Only", "Acute And Chronic", "Acute Only", "Acute And Chronic"
                     , "Acute And Chronic", "Acute And Chronic", "Acute And Chronic"
                     , "Acute And Chronic", "Acute And Chronic", "Acute And Chronic"
                     , "Acute And Chronic", "Acute Only", "Acute Only", "Acute Only"
                     , "Acute And Chronic", "Acute Only", "Acute And Chronic"
                     , "Acute Only", "Acute And Chronic", "Acute Only", "Acute Only"
                     , "Acute Only", "Acute And Chronic", "Acute Only", "Acute And Chronic"
                     , "Acute Only", "Acute Only", "Acute And Chronic"
                     , "Acute And Chronic", "Acute Only", "Acute Only", "Acute Only"
                     , "Acute Only", "Acute And Chronic", "Acute And Chronic"
                     , "Acute Only", "Acute Only")

  # Combine into a dataframe
  df_SS_AUs <- data.frame(WATER_ID = water_ids, Applicability = applicability)

  # cleanup
  rm(applicability, water_ids)

  ## AU Loop ####
  Unique_AUIDs <- unique(df_SS_AUs$WATER_ID) %>% stats::na.omit()
  result_list <- list()
  result_indiv_list <- list()
  result_suff <- list()
  counter <- 0
  counter_v2 <- 1
  df_suff <- tibble::tibble(WATER_ID = NA, Result = NA)

  for(i in Unique_AUIDs){
    print(i) # print name of current WATER_ID

    counter <- counter + 1

    # subset chem data by WATER_ID
    df_subset <- df_Chem_v4 %>%
      dplyr::filter(WATER_ID == i)

    #If no relevant samples, skip WATER_ID
    if(nrow(df_subset)==0){
      next
    }

    # predictor data
    df_predictors <- df_subset %>%
      dplyr::filter(CHR_UID %in% c(4528, 1648, 2174)) %>%
      dplyr::group_by(WATER_ID, STATION, CHR_UID, DATE) %>%
      dplyr::reframe(MEASUREMENT = mean(MEASUREMENT_num)) %>%
      tidyr::pivot_wider(id_cols = c('WATER_ID', 'STATION', 'DATE'),
                         names_from = 'CHR_UID',
                         values_from = 'MEASUREMENT') %>%
      dplyr::rename(Hardness_mgL = any_of("4528"),
                    DOC_mgL      = any_of("2174"),
                    pH           = any_of("1648"))


    #If no relevant hardness samples, skip WATER_ID
    if(nrow(df_predictors)==0){
      next
    }

    #If insufficient dependent characteristics, mark as insufficient
    if(ncol(df_predictors) < 6) {
      chars <- df_subset %>%
        dplyr::distinct(CHARACTERISTIC_NAME) %>%
        dplyr::arrange(CHARACTERISTIC_NAME) %>%
        dplyr::pull(CHARACTERISTIC_NAME)

      df_suff <- df_subset %>%
        dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, DU) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          R_Script_Name = RFunctionName,
          Result  = "Insufficient dependent characteristics",
          n_Characteristics = list(chars)  #ist column
        )

      result_suff[[counter_v2]] <- df_suff
      counter_v2 <- counter_v2 + 1

      next
    }

    # subset chem data for copper
    df_subset_v2 <- df_subset %>%
      dplyr::filter(CHR_UID == 832)

    # subset chem data by last three years
    maxYear <- max(lubridate::year(df_subset$DATE))
    YearMinus2 <- maxYear-2

    df_subset_v3 <- df_subset_v2 %>%
      dplyr::mutate(Year = lubridate::year(DATE)) %>%
      dplyr::filter(Year >= YearMinus2)

    # join predictor data
    df_subset_v4 <- dplyr::left_join(df_subset_v3, df_predictors
                                     , by = c("WATER_ID" = "WATER_ID"
                                              , "STATION" = "STATION"
                                              , "DATE" = "DATE")) %>%
      dplyr::filter(!is.na(Hardness_mgL))

    n_Samples <- nrow(df_subset_v4)

    # Copper
    #Site-specific criteria type
    criteria <- df_SS_AUs %>%
      dplyr::filter(WATER_ID == i) %>%
      dplyr::select(Applicability) %>%
      dplyr::pull()

    #Calculate acute
    df_subset_v5 <- df_subset_v4 %>%
      dplyr::mutate(DOC_mgL = ifelse(DOC_mgL > 29.7, 29.7, DOC_mgL),
                    Hardness_mgL = ifelse(Hardness_mgL > 207, 207, Hardness_mgL),
                    Crit_Acute = exp(-22.914+1.017*log(DOC_mgL)+0.045*log(Hardness_mgL)+5.176*pH-0.261*pH^2),
                    Crit_Chronic = NA)

    #Calculate chronic where applicable
    if(criteria == 'Acute and Chronic') {
      df_subset_v5 <- df_subset_v5 %>%
        dplyr::mutate(Crit_Chronic = exp(-23.391+1.017*log(DOC_mgL)+0.045*log(Hardness_mgL)+5.176*pH-0.261*pH^2))
    }


    # create results table
    df_results <- df_subset_v5 %>%
      dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, DU,  SAMPLING_EVENT_TYPE
                    , SAMPLE_TYPE, SAMPLE_FRACTION, CHR_UID, CHARACTERISTIC_NAME, UNITS) %>%
      dplyr::distinct() %>%
      dplyr::mutate(R_Script_Name = RFunctionName
                    , n_Samples = n_Samples)

    df_results_indiv <- df_subset_v5 %>%
      dplyr::mutate(FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                         & (Crit_Acute <= MEASUREMENT_num))
                                                        ~ "Measurement is ND and MRL is greater than calculated acute criterion"
                                                        , (LESS_THAN_YN == "Y" & (Crit_Chronic <= MEASUREMENT_num))
                                                        ~ "Measurement is ND and MRL is greater than calculated chronic criterion"
                                                        , TRUE ~ NA))

    # Apply method based on Acute and Chronic criteria
    if(n_Samples == 1){
      results <- df_subset_v5 %>%
        dplyr::mutate(bad_samp_acute = dplyr::case_when(is.na(Crit_Acute) ~ 0
                                                        , (LESS_THAN_YN == "Y"
                                                           & (Crit_Acute <= MEASUREMENT_num))  ~ 0
                                                        , MEASUREMENT_num > Crit_Acute ~ 1
                                                        , TRUE ~ 0),
                      bad_samp_chronic = dplyr::case_when(SAMPLE_TYPE == "Storm" ~ 0
                                                          , is.na(Crit_Chronic) ~ 0
                                                          , (LESS_THAN_YN == "Y"
                                                             & (Crit_Chronic <= MEASUREMENT_num))  ~ 0
                                                          , MEASUREMENT_num > Crit_Chronic ~ 1
                                                          , TRUE ~ 0),
                      FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                           & (Crit_Acute <= MEASUREMENT_num))
                                                          ~ "Nonassessable"
                                                          , (LESS_THAN_YN == "Y"
                                                             & (Crit_Chronic <= MEASUREMENT_num))
                                                          ~ "Nonassessable"
                                                          , TRUE ~ NA))

      df_results$n_Samples_assessable <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                         == "Nonassessable"
                                                         , na.rm = TRUE)

      bad_tot_acute <- sum(results$bad_samp_acute)
      bad_tot_chronic <- sum(results$bad_samp_chronic)

      df_results$Method <- "No acute exceedance AND no more than one
                                chronic exceedance"
      df_results$Rationale <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                     , "Data Sufficient"
                                     , "Data Insufficient")
      df_results$Exceed <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >=1
                                  , 'Yes', 'No')
      df_results$n_Exceed_Acute <- bad_tot_acute
      df_results$n_Exceed_Chronic <- bad_tot_chronic

    } else if (n_Samples == 2 | n_Samples == 3){
      results <- df_subset_v5 %>%
        dplyr::mutate(bad_samp_acute = dplyr::case_when(is.na(Crit_Acute) ~ 0
                                                        , (LESS_THAN_YN == "Y"
                                                           & (Crit_Acute <= MEASUREMENT_num))  ~ 0
                                                        , MEASUREMENT_num > Crit_Acute ~ 1
                                                        , TRUE ~ 0)
                      , bad_samp_chronic = dplyr::case_when(SAMPLE_TYPE == "Storm" ~ 0
                                                            , is.na(Crit_Chronic) ~ 0
                                                            , (LESS_THAN_YN == "Y"
                                                               & (Crit_Chronic <= MEASUREMENT_num))  ~ 0
                                                            , MEASUREMENT_num > Crit_Chronic ~ 1
                                                            , TRUE ~ 0),
                      FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                           & (Crit_Acute <= MEASUREMENT_num))
                                                          ~ "Nonassessable"
                                                          , (LESS_THAN_YN == "Y"
                                                             & (Crit_Chronic <= MEASUREMENT_num))
                                                          ~ "Nonassessable"
                                                          , TRUE ~ NA))

      df_results$n_Samples_assessable <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                         == "Nonassessable"
                                                         , na.rm = TRUE)

      bad_tot_acute <- sum(results$bad_samp_acute)
      bad_tot_chronic <- sum(results$bad_samp_chronic)

      df_results$Method <- "No acute exceedance AND no more than one
                                chronic exceedance"
      df_results$Rationale <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                     , "Data Sufficient"
                                     , "Data Insufficient")
      df_results$Exceed <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                  , 'Yes', 'No')
      df_results$n_Exceed_Acute <- bad_tot_acute
      df_results$n_Exceed_Chronic <- bad_tot_chronic

    } else if (n_Samples >= 4 ) {
      results <- df_subset_v5 %>%
        dplyr::mutate(bad_samp_acute = dplyr::case_when(is.na(Crit_Acute) ~ 0
                                                        , (LESS_THAN_YN == "Y"
                                                           & (Crit_Acute <= MEASUREMENT_num))  ~ 0
                                                        , MEASUREMENT_num > Crit_Acute ~ 1
                                                        , TRUE ~ 0)
                      , bad_samp_chronic = dplyr::case_when(SAMPLE_TYPE == "Storm" ~ 0
                                                            , is.na(Crit_Chronic) ~ 0
                                                            , (LESS_THAN_YN == "Y"
                                                               & (Crit_Chronic <= MEASUREMENT_num))  ~ 0
                                                            , MEASUREMENT_num > Crit_Chronic ~ 1
                                                            , TRUE ~ 0),
                      FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                           & (Crit_Acute <= MEASUREMENT_num))
                                                          ~ "Nonassessable"
                                                          , (LESS_THAN_YN == "Y"
                                                             & (Crit_Chronic <= MEASUREMENT_num))
                                                          ~ "Nonassessable"
                                                          , TRUE ~ NA))

      n_Samples_assessable_value <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                    == "Nonassessable"
                                                    , na.rm = TRUE)

      df_results$n_Samples_assessable <- n_Samples_assessable_value

      bad_tot_acute <- sum(results$bad_samp_acute)
      bad_tot_chronic <- sum(results$bad_samp_chronic)

      df_results$Rationale <- ifelse(n_Samples_assessable_value >= 4
                                     , "Data Sufficient"
                                     , ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                              , "Data Sufficient"
                                              , "Data Insufficient"))

      df_results$Method <- "No acute exceedance AND no more than one chronic exceedance"
      df_results$Exceed <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                  , 'Yes', 'No')
      df_results$n_Exceed_Acute <- bad_tot_acute
      df_results$n_Exceed_Chronic <- bad_tot_chronic

    } # END ~ method if/else

    result_list[[counter]] <- df_results
    result_indiv_list[[counter]] <- df_results_indiv
  }  # END ~ AU for loop

  # combine results from for loop
  df_loop_results <- as.data.frame(do.call("rbind", result_list))
  df_loop_results_indiv <- as.data.frame(do.call("rbind", result_indiv_list))
  df_suff_results <- as.data.frame(do.call("rbind", result_suff))

  # Export data ####
  return(list(
    df_SS_Copper_ALU = df_loop_results,
    df_SS_Copper_ALU_Indiv_Res = df_loop_results_indiv,
    df_SS_Copper_ALU_Insuff_Res = df_suff_results))

} # END ~ Function
