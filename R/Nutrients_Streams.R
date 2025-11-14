Nutrients_Streams <- function(Chem_table
                              , DU_table){

  # QC ####
  # QC messages for required files
  if (missing(Chem_table)) {
    stop(paste0("Error: 'Chem_table' is required but was not provided. ",
                "This is an output from the Data_Prep function."
    ))}

  if (missing(DU_table)) {
    stop(paste0("Error: 'DU_table' is required but was not provided. ",
                "This is an output from the Data_Prep function."
    ))}

  # Nutrients AL ####
  # Only relevant criteria in DU table
  ALUs <- c("ColdWAL", "CoolWAL", "HQColdWAL", "LAL", "MCWAL", "MWWAL", "WWAL")

  # set CHR_UID to numeric for this script
  Chem_table$CHR_UID <- as.numeric(Chem_table$CHR_UID)
  DU_table$CHR_UID <- as.numeric(DU_table$CHR_UID)

  # Filter for relevant DUs, and exclude to only DO.
  # TN and TP are not included in the DU or criteria tables and will be hard-coded.
  # Note: growing season is included in the CALM but as "preferred", so no need to filter here.
  df_DU_v2 <- DU_table %>%
    # intermittent and ephemeral streams as well as large are excluded per the CALM
    # These waterbody types will be retained in the output but flagged.
    dplyr::filter(Waterbody %in% c("STREAM, PERENNIAL", "RIVER", "STREAM, INTERMITTENT", "STREAM, EPHEMERAL")) %>%
    dplyr::filter(DU %in% ALUs) %>%
    dplyr::filter(Criteria_Name %in% c("DO_WQC"))

  df_Ecoregion <- df_DU_v2 %>%
    dplyr::select(WATER_ID, ECOREGION) %>%
    # extract Level III ecoregion from the Level IV ecoregion
    dplyr::mutate(ECOREGION_L3 = substr(ECOREGION, 1, 2)) %>%
    dplyr::select(WATER_ID, ECOREGION_L3) %>%
    dplyr::distinct()

  df_DU_v2_subset <- df_DU_v2 %>%
    dplyr::select(WATER_ID, DU, Waterbody, DU, TN_SITE_CLASS, TP_SITE_CLASS, ECOREGION, ELEVATION)

  ## Trim chem data ####

  # Note: in 2025, DO concentration was removed from the method (CHR_UID = 985)
  # CHR_UIDs for TN, TP, chlorophyll, DO saturation
  # 1418 (NO2 + NO3)
  # 1416 (TKN)
  # 1674 (TP)
  # 999919 (daily delta DO)

  CHR_UID_Unique <- c(1418, 1416, 1674, 999919)

  df_Chem_v2a <- Chem_table %>%
    # filter for only lakes
    dplyr::filter(SAMPLING_EVENT_TYPE %in% c("RIVER/STREAM-CHEMICAL", "LONG TERM DEPLOYMENT")) %>%
    # filter for TN, TP, and DO
    dplyr::filter(CHR_UID %in% CHR_UID_Unique)

  df_Chem_v2 <- df_Chem_v2a %>%
    # calculate TN from TKN + NO2+NO3
    # Note: Non-detects are already reported as the detection limit (see "Y" for LESS_THAN_YN).
    # If NO2+NO3 and/or TKN is a nondetect, the detection limit should be used in the calculation.
    # This procedure preserves that workflow.
    dplyr::select(WATER_ID:TIME, CHR_UID, MEASUREMENT_num, ASSESSABILITY_QUALIFIER_CODE) %>%
    tidyr::pivot_wider(names_from = "CHR_UID", values_from = "MEASUREMENT_num") %>%
    # Note: TN (calculated) CHR_UID is 1415.
    dplyr::mutate(`1415` = `1418` + `1416`) %>%
    tidyr::pivot_longer(cols = c(`1418`, `1416`, `1415`, `1674`, `999919`),
                        names_to = "CHR_UID", values_to = "MEASUREMENT_num") %>%
    tidyr::drop_na(MEASUREMENT_num) %>%
    dplyr::mutate(CHR_UID = as.numeric(CHR_UID)) %>%
    dplyr::full_join(df_Chem_v2a, .) %>%
    # fill in TN details
    dplyr::mutate(SAMPLE_FRACTION = dplyr::case_when(CHR_UID == 1415 ~ "Total",
                                                     TRUE ~ SAMPLE_FRACTION),
                  CHARACTERISTIC_NAME = dplyr::case_when(CHR_UID == 1415 ~ "Total Nitrogen",
                                                         TRUE ~ CHARACTERISTIC_NAME),
                  UNITS = dplyr::case_when(CHR_UID == 1415 ~ "mgL",
                                           TRUE ~ UNITS)) %>%
    # remove TKN and NO2+NO3
    dplyr::filter(CHR_UID != 1416 & CHR_UID != 1418) %>%
    dplyr::mutate(CHARACTERISTIC_NAME = dplyr::case_when(CHARACTERISTIC_NAME == "Phosphorus as P" ~ "Total Phosphorus",
                                           TRUE ~ CHARACTERISTIC_NAME))

  rm(df_Chem_v2a)

  df_results <- df_Chem_v2 %>%
    dplyr::full_join(., df_DU_v2_subset, relationship = "many-to-many") %>%
    dplyr::left_join(., df_Ecoregion) %>%
    dplyr::mutate(Criteria_Value = dplyr::case_when(
      # TN
      CHR_UID == 1415 & TN_SITE_CLASS == "Flat" ~ 0.69,
      CHR_UID == 1415 & TN_SITE_CLASS == "Moderate" ~ 0.42,
      CHR_UID == 1415 & TN_SITE_CLASS == "Steep" ~ 0.3,
      # TP
      CHR_UID == 1674  & TP_SITE_CLASS == "High-Volcanic" ~ 0.105,
      CHR_UID == 1674  & TP_SITE_CLASS == "Flat-Moderate" ~ 0.061,
      CHR_UID == 1674  & TP_SITE_CLASS == "Steep" ~ 0.030,
      # DO Daily Delta
      CHR_UID == 999919  & TP_SITE_CLASS == "High-Volcanic" ~ 5.02,
      CHR_UID == 999919  & TP_SITE_CLASS == "Flat-Moderate" ~ 4.08,
      CHR_UID == 999919  & TP_SITE_CLASS == "Steep" ~ 1.79)) %>%
    dplyr::mutate(Criteria_Name = dplyr::case_when(CHR_UID == 1415 ~ "TN_WQC",
                                                   CHR_UID == 1674 ~ "TP_WQC",
                                                   CHR_UID == 999919 ~ "DODELTA_WQC")) %>%
    # TN and TP are on site medians, not on single samples.
    dplyr::group_by(WATER_ID, WATER_NAME, PROJECT_NAME, STATION, STATION_NAME,
                    SAMPLING_EVENT_TYPE, ACTIVITY_TYPE, SAMPLE_TYPE,
                    SAMPLE_FRACTION, CHR_UID, CHARACTERISTIC_NAME,
                    UNITS, DU, Waterbody, TN_SITE_CLASS, TP_SITE_CLASS,
                    Criteria_Name, Criteria_Value, ASSESSABILITY_QUALIFIER_CODE) %>%
    dplyr::mutate(MEASUREMENT_median = dplyr::case_when(CHR_UID %in% c(1415, 1674) ~ median(MEASUREMENT_num)),
                  MEASUREMENT_p75 = dplyr::case_when(CHR_UID %in% c(1415, 1674) ~ quantile(MEASUREMENT_num, 0.75, na.rm = TRUE)),
                  MEASUREMENT_IQR = dplyr::case_when(CHR_UID %in% c(1415, 1674) ~ quantile(MEASUREMENT_num, 0.75, na.rm = TRUE) - quantile(MEASUREMENT_num, 0.25, na.rm = TRUE)),
                  MEASUREMENT_outlierthreshold = dplyr::case_when(CHR_UID %in% c(1415, 1674) ~ MEASUREMENT_p75 + 3 * MEASUREMENT_IQR)) %>%
    dplyr::ungroup() %>%
    # "Exceed" determines whether median is exceeded for TN and TP or whether a sample is exceeded for DO.
    dplyr::mutate(Exceed = dplyr::case_when(MEASUREMENT_median > Criteria_Value &
                                              Criteria_Name %in% c("TN_WQC",
                                                                   "TP_WQC")
                                            ~ "Yes",
                                            MEASUREMENT_median <= Criteria_Value  &
                                              Criteria_Name %in% c("TN_WQC",
                                                                   "TP_WQC")
                                            ~ "No",
                                            # DO should be assessed as individual observation, not site median.
                                            MEASUREMENT_num > Criteria_Value &
                                              Criteria_Name %in% c("DODELTA_WQC")
                                            ~ "Yes",
                                            MEASUREMENT_num <= Criteria_Value  &
                                              Criteria_Name %in% c("DODELTA_WQC")
                                            ~ "No"),
                  # if all dates are outside the growing season, set up for a flag.
                  DO_within_gs = dplyr::case_when(ECOREGION_L3 %in% c(22, 23) &
                                                    ELEVATION >7500 &
                                                    # July 1
                                                    lubridate::yday(ACT_START_DATE) >= 182 |
                                                    # October 15
                                                    lubridate::yday(ACT_END_DATE) <= 288 ~ "Yes",
                                                  ECOREGION_L3 %in% c(22, 23) &
                                                    ELEVATION >7500 &
                                                    # July 1
                                                    lubridate::yday(ACT_START_DATE) < 182 &
                                                    # October 15
                                                    lubridate::yday(ACT_END_DATE) > 288 ~ "No",
                                                  ECOREGION_L3 %in% c(20, 21, 22, 23) &
                                                    ELEVATION <= 7500 &
                                                    # June 15
                                                    lubridate::yday(ACT_START_DATE) >= 166 |
                                                    # November 1
                                                    lubridate::yday(ACT_END_DATE) <= 305 ~ "Yes",
                                                  ECOREGION_L3 %in% c(20, 21, 22, 23) &
                                                    ELEVATION <= 7500 &
                                                    # June 15
                                                    lubridate::yday(ACT_START_DATE) < 166 &
                                                    # November 1
                                                    lubridate::yday(ACT_END_DATE) > 305 ~ "No",
                                                  ECOREGION_L3 %in% c(24, 25, 26, 79) &
                                                    # May 15
                                                    lubridate::yday(ACT_START_DATE) >= 135 |
                                                    # November 15
                                                    lubridate::yday(ACT_END_DATE) <= 319 ~ "Yes",
                                                  ECOREGION_L3 %in% c(24, 25, 26, 79) &
                                                    # May 15
                                                    lubridate::yday(ACT_START_DATE) < 135 &
                                                    # November 15
                                                    lubridate::yday(ACT_END_DATE) > 319 ~ "No"),
                  # Flag outliers as 75th percentile + 3xIQR.
                  # Note that sample size was not taken into account, and results may be skewed at small sample size.
                  MEASUREMENT_outlier = dplyr::case_when(CHR_UID %in% c(1415, 1674) & MEASUREMENT_num > MEASUREMENT_outlierthreshold ~ "Yes",
                                                         CHR_UID %in% c(1415, 1674) & MEASUREMENT_num <= MEASUREMENT_outlierthreshold ~ "No"))




  df_results_v2 <- df_results %>%
    tidyr::drop_na(WATER_NAME) %>%
    dplyr::group_by(WATER_ID, WATER_NAME, PROJECT_NAME, #STATION, STATION_NAME, # Note: group by AU rather than station?
                    SAMPLING_EVENT_TYPE, Waterbody,  DU, CHR_UID, CHARACTERISTIC_NAME,
                    UNITS, Criteria_Name, Criteria_Value, ASSESSABILITY_QUALIFIER_CODE) %>%
    dplyr::reframe(n_Samples = dplyr::n(),
                     n_Exceed = dplyr::case_when(Criteria_Name %in% c("DODELTA_WQC") ~ sum(Exceed == "Yes"),
                                                 Criteria_Name %in% c("TN_WQC", "TP_WQC") ~ sum(Exceed == "Yes")/n_Samples),

                     outlier_flag = ifelse(any(MEASUREMENT_outlier == "Yes", na.rm = TRUE), "Yes", "")) %>%
    # Note: TN and TP are assessed by median, so any number of excursions leads to non-support.
    dplyr::mutate(Exceed = dplyr::case_when(Criteria_Name %in% c("TN_WQC", "TP_WQC")
                                            & n_Exceed >= 1 & n_Samples > 3
                                            ~ "Yes",
                                            Criteria_Name %in% c("TN_WQC", "TP_WQC")
                                            & n_Exceed < 1 & n_Samples > 3
                                            ~ "No",
                                            Criteria_Name %in% c("TN_WQC", "TP_WQC")
                                            & n_Exceed >= 1 & n_Samples <= 3
                                            ~ "Limited Data, Exceedance",
                                            Criteria_Name %in% c("TN_WQC", "TP_WQC")
                                            & n_Exceed < 1 & n_Samples <= 3
                                            ~ "Limited Data, No Exceedance",
                                            Criteria_Name %in% c("DODELTA_WQC")
                                            & n_Exceed >= 1
                                            ~ "Yes",
                                            Criteria_Name %in% c("DODELTA_WQC")
                                            & n_Exceed < 1
                                            ~ "No"),
                  Delist_eligible = dplyr::case_when(Criteria_Name %in% c("TN_WQC", "TP_WQC")
                                                     & n_Samples > 3
                                                     & n_Exceed < 1
                                                     ~ "Yes",
                                                     Criteria_Name %in% c("TN_WQC", "TP_WQC")
                                                     & n_Samples <= 3
                                                     ~ "No",
                                                     Criteria_Name %in% c("TN_WQC", "TP_WQC")
                                                     & n_Exceed >= 1
                                                     ~ "No",
                                                     Criteria_Name %in% c("DODELTA_WQC")
                                                     & n_Samples >= 1
                                                     & n_Exceed < 1
                                                     ~ "Yes",
                                                     Criteria_Name %in% c("DODELTA_WQC")
                                                     & n_Exceed >= 1
                                                     ~ "No")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Enrichment_causal = dplyr::case_when(
      Criteria_Name %in% c("TN_WQC", "TP_WQC") & Exceed == "Yes" & Waterbody == "STREAM, PERENNIAL" ~ "Yes",
      Criteria_Name %in% c("TN_WQC", "TP_WQC") & Exceed == "No" & Waterbody == "STREAM, PERENNIAL"~ "No",
      Criteria_Name %in% c("TN_WQC", "TP_WQC") & Exceed == "Limited Data, No Exceedance" & Waterbody == "STREAM, PERENNIAL"~ "Limited Data, No Exceedance",
      Criteria_Name %in% c("TN_WQC", "TP_WQC") & Exceed == "Limited Data, Exceedance" & Waterbody == "STREAM, PERENNIAL"~ "Limited Data, Exceedance",
      Criteria_Name %in% c("TN_WQC", "TP_WQC") &
        Waterbody %in% c("RIVER", "STREAM, INTERMITTENT", "STREAM, EPHEMERAL") ~ "Not Assessable: Waterbody Type Mismatch"),
      Enrichment_DODELTA = dplyr::case_when(
        Criteria_Name %in% c("DODELTA_WQC") & Exceed == "Yes" ~ "Yes",
        Criteria_Name %in% c("DODELTA_WQC") & Exceed == "No" ~ "No")) %>%
    dplyr::group_by(WATER_ID, WATER_NAME, PROJECT_NAME) %>% # , STATION, STATION_NAME # Note: group by AU rather than station?
    # Set up support indicators corresponding to Appendix C
    # Yes needs to trump No
    dplyr::mutate(Enrichment_causal = ifelse(any(Enrichment_causal == "Not Assessable: Waterbody Type Mismatch", na.rm = TRUE), "Not Assessable: Waterbody Type Mismatch", ifelse(
      any(Enrichment_causal == "Yes", na.rm = TRUE), "Yes", ifelse(
        any(Enrichment_causal == "No", na.rm = TRUE), "No", ifelse(
          any(Enrichment_causal == "Limited Data, Exceedance", na.rm = TRUE), "Limited Data, Exceedance", ifelse(
            any(Enrichment_causal == "Limited Data, No Exceedance", na.rm = TRUE), "Limited Data, No Exceedance", NA
          ))))),
      Enrichment_DODELTA = ifelse(any(Enrichment_DODELTA == "Yes", na.rm = TRUE), "Yes", ifelse(
        any(Enrichment_DODELTA == "No", na.rm = TRUE), "No", NA)),
      ASSESSABILITY_QUALIFIER_CODE = ifelse(any(ASSESSABILITY_QUALIFIER_CODE == "NEITHER", na.rm = TRUE), "NEITHER", ifelse(
        any(ASSESSABILITY_QUALIFIER_CODE == "NON", na.rm = TRUE), "NON", ifelse(
          any(ASSESSABILITY_QUALIFIER_CODE == "BOTH", na.rm = TRUE), "BOTH", NA
        ))),
      Delist_eligible = ifelse(any(Delist_eligible == "No"), "No", Delist_eligible)) %>%
    # format for results output
    dplyr::ungroup() %>%
    dplyr::mutate(R_Script_Name = "Nutrients_Streams") %>%
    # keep station names
    dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, Waterbody, # STATION, STATION_NAME, # Note: group by AU rather than station?
                  CHR_UID, CHARACTERISTIC_NAME, UNITS, R_Script_Name, DU,
                  n_Samples, Criteria_Value, Exceed, n_Exceed,
                  Enrichment_causal, Enrichment_DODELTA,
                  ASSESSABILITY_QUALIFIER_CODE, outlier_flag, Delist_eligible)

  # Optional: if df_results_v2 was exported and edited, uncomment the lines below and re-upload here.
  # Please replace myDate with the date of the desired spreadsheet, if applicable.

  # df_results_v2 <- read_csv(file.path(wd, output.dir, results.dir
  #                                     , paste0("Nutrients_Stream_ALU_Results_Intermediate"
  #                                              , myDate, ".csv"))
  #                           , na = c("NA",""), trim_ws = TRUE, skip = 0
  #                           , col_names = TRUE, guess_max = 100000)


  df_results_v3 <- df_results_v2 %>%
    dplyr::group_by(WATER_ID, WATER_NAME) %>%
    dplyr::mutate(Char_Exceeding = paste(unique(CHARACTERISTIC_NAME[Exceed == "Yes"]), collapse = ","),
                  outlier_flag = ifelse(any(outlier_flag == "Yes", na.rm = TRUE), "Yes", "")) %>%
    dplyr::ungroup() %>%
    dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, Waterbody, DU,
                  Enrichment_causal, Enrichment_DODELTA,
                  ASSESSABILITY_QUALIFIER_CODE, outlier_flag, Delist_eligible,
                  Char_Exceeding) %>%
    dplyr::distinct() %>%
    # Apply flowchart in Appendix C
    dplyr::mutate(Category = dplyr::case_when(
      # Not supporting
      Enrichment_causal == "Yes" & Enrichment_DODELTA == "Yes" &
        ASSESSABILITY_QUALIFIER_CODE %in% c("BOTH", "NON") ~ "4 or 5",
      # Fully supporting
      Enrichment_causal == "No" & Enrichment_DODELTA == "No" ~ "1 or 2",
      Enrichment_causal == "No" & Enrichment_DODELTA == "Yes" ~ "1 or 2; high delta DO - check upstream AU for TN/TP",
      Enrichment_causal == "Yes" & Enrichment_DODELTA == "No" &
        ASSESSABILITY_QUALIFIER_CODE %in% c("BOTH") ~ "3c",
      # Limited data, exceedance(s)
      Enrichment_causal == "Limited Data, Exceedance" ~ "3c",
      Enrichment_causal == "Yes" & is.na(Enrichment_DODELTA) ~ "3c",
      is.na(Enrichment_causal) & Enrichment_DODELTA == "Yes" ~ "3c",
      Enrichment_causal == "Yes" & Enrichment_DODELTA == "Yes" &
        ASSESSABILITY_QUALIFIER_CODE %in% c("NEITHER") ~ "3c",
      Enrichment_causal == "Yes" & Enrichment_DODELTA == "No" &
        ASSESSABILITY_QUALIFIER_CODE %in% c("NEITHER", "NON") ~ "3c",
      # Limited data, no exceedance(s)
      Enrichment_causal == "Limited Data, No Exceedance" ~ "3b",
      Enrichment_causal == "No" & is.na(Enrichment_DODELTA) ~ "3b",
      is.na(Enrichment_causal) & Enrichment_DODELTA == "No" ~ "3b",
      # Not assessed
      Enrichment_causal == "Not Assessable: Waterbody Type Mismatch" ~ "Not Assessable: Waterbody Type Mismatch",
      is.na(Enrichment_causal) & is.na(Enrichment_DODELTA) ~ "3a",
      is.na(Enrichment_causal) & is.na(Enrichment_DODELTA) ~ "3a"
    )) %>%
    # category 3s are NA for delist eligibility
    dplyr::mutate(Delist_eligible = dplyr::case_when(Category %in%
                                                       c("3a", "3b", "3c") ~ NA,
                                              TRUE ~ Delist_eligible))

  # Export data ####
  return(list(
    Nutrients_Streams = df_results_v3,
    Nutrients_Streams_Indiv_Res = df_results_v2))


} # END ~ Function
