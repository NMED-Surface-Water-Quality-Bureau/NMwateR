#' Analysis of lake nutrient data against ALU standards
#'
#' This function compares lake nutrient data against water quality
#' standards for aquatic life use (ALU). For more information, see the NMED
#' Consolidated Assessment and Listing Methodology (CALM) guidance manual.
#'
#' @param Chem_table Compiled water chemistry data from Data_Prep function. Usually
#' contains a combination of grab, LTD, and lake profile data.
#' @param DU_table Quality controlled Stations DU table from Data_Prep function.
#'
#' @returns A list of two dataframes. The first contains analyzed lake nutrient
#' data compared to ALU water quality criteria. The second, labeled "Indiv_Res"
#' is an intermediate file used for QA/QC purposes.
#'
#' @examples
#' \dontrun{
#' Nutrients_Lakes_list <- Nutrients_Lakes(Chem_table = df_Chem_combined
#' , DU_table = df_DU_processed)
#' df_Nutrients_Lakes <- Nutrients_Lakes_list$Nutrients_Lakes
#' df_Nutrients_Lakes_Indiv_Res <- Nutrients_Lakes_list$Nutrients_Lakes_Indiv_Res)}
#'
Nutrients_Lakes <- function(Chem_table
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

  # Filter for relevant DUs, and filter to only DO and pH.
  # TN, TP, and chlorophyll are not included in the DU or criteria tables and will be hard-coded.
  # Note: DO saturation also needs to be included as a flag for lakes, calculated from profile data.
  # Note: SOP specifies lakes DO is collected only in the growing season. No need to filter here.
  df_DU_v2 <- DU_table %>%
    dplyr::filter(Waterbody %in% c("RESERVOIR", "LAKE, FRESHWATER"
                                   , "LAKE, SALINE")) %>%
    dplyr::filter(DU %in% ALUs) %>%
    dplyr::filter(Criteria_Name %in% c("PH_LOW", "PH_HIGH", "DO_WQC"))

  df_Ecoregion <- df_DU_v2 %>%
    dplyr::select(WATER_ID, ECOREGION) %>%
    # extract Level III ecoregion from the Level IV ecoregion
    dplyr::mutate(ECOREGION_L3 = substr(ECOREGION, 1, 2)) %>%
    dplyr::select(WATER_ID, ECOREGION_L3) %>%
    dplyr::distinct()

  df_DU_only <- df_DU_v2 %>%
    dplyr::select(WATER_ID, DU, Waterbody) %>%
    dplyr::distinct()


  ## Trim chem data ####

  # 1648 (pH), 985 (DO), 1418 (NO2 + NO3), 1416 (TKN), 1674 (TP), 791 (chlorophyll a)
  # 986 (DO saturation)

  CHR_UID_Unique <- c(1648, 985, 1418, 1416, 1674, 791, 986)

  df_Chem_v2a <- Chem_table %>%
    # filter for only lakes
    dplyr::filter(SAMPLING_EVENT_TYPE %in% c("LAKE-CHEMICAL"
                                             , "Lake Depth Profile Summary")) %>%
    # filter for TN, TP, chlorophyll, DO, and pH
    dplyr::filter(CHR_UID %in% CHR_UID_Unique)

  df_Chem_v2 <- df_Chem_v2a %>%
    # calculate TN from TKN + NO2+NO3
    # Note: Non-detects are already reported as the detection limit (see "Y" for LESS_THAN_YN).
    # If NO2+NO3 and/or TKN is a nondetect, the detection limit should be used in the calculation.
    # This procedure preserves that workflow.
    dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME,
                  STATION, STATION_NAME, SAMPLING_EVENT_TYPE, SAMPLE_TYPE,
                  DATE, TIME, CHR_UID, MEASUREMENT_num) %>%
    tidyr::pivot_wider(names_from = "CHR_UID", values_from = "MEASUREMENT_num") %>%
    # Note: TN (calculated) CHR_UID is 1415.
    dplyr::mutate(`1415` = `1418` + `1416`) %>%
    tidyr::pivot_longer(cols = c(`1648`, `985`, `1418`, `1416`, `1674`, `791`
                                 , `986`, `1415`),
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
    dplyr::mutate(CHARACTERISTIC_NAME = dplyr::case_when(CHARACTERISTIC_NAME == "Phosphorus as P"
                                                  ~ "Total Phosphorus",
                                           TRUE ~ CHARACTERISTIC_NAME))

  rm(df_Chem_v2a)

  # pull AU metadata from LAKE-CHEMICAL into Lake Depth Profile Summary
  df_meta <- df_Chem_v2 %>%
    dplyr::select(WATER_ID, WATER_NAME) %>%
    tidyr::drop_na() %>%
    dplyr::distinct()

  # replace missing WATER_NAME from Lake Depth Profile Summary
  df_Chem_v2 <- df_Chem_v2 %>%
    dplyr::select(-WATER_NAME) %>%
    dplyr::left_join(., df_meta)

  rm(df_meta)

  # Need to calculate "concurrence" in this step to determine full support.
  # "Concurrenty" = within 7 days
  # Minimum requirement for all parameters sampled concurrently is 2x
  df_results <- df_Chem_v2 %>%
    dplyr::full_join(., df_DU_only, relationship = "many-to-many") %>%
    dplyr::full_join(., df_DU_v2, relationship = "many-to-many") %>%
    dplyr::left_join(., df_Ecoregion) %>%
    dplyr::mutate(Criteria_Value = as.numeric(Criteria_Value)) %>%
    dplyr::mutate(Criteria_Value = dplyr::case_when(# Chlorophyll data are in mg/L, convert from ug/L)
      CHR_UID == 791 ~ 10/1000,
      # TN
      CHR_UID == 1415 & ECOREGION_L3 == 20 ~ 0.463,
      CHR_UID == 1415 & ECOREGION_L3 == 21 ~ 0.387,
      CHR_UID == 1415 & ECOREGION_L3 == 22 ~ 0.385,
      CHR_UID == 1415 & ECOREGION_L3 == 23 ~ 0.481,
      CHR_UID == 1415 & ECOREGION_L3 == 24 ~ 0.488,
      CHR_UID == 1415 & ECOREGION_L3 == 26 ~ 0.561,
      # TP
      CHR_UID == 1674 & ECOREGION_L3 == 20 ~ 0.023,
      CHR_UID == 1674 & ECOREGION_L3 == 21 ~ 0.025,
      CHR_UID == 1674 & ECOREGION_L3 == 22 ~ 0.022,
      CHR_UID == 1674 & ECOREGION_L3 == 23 ~ 0.04,
      CHR_UID == 1674 & ECOREGION_L3 == 24 ~ 0.02,
      CHR_UID == 1674 & ECOREGION_L3 == 26 ~ 0.021,
      # DO Saturation
      CHR_UID == 986 ~ 120,
      TRUE ~ Criteria_Value),
      Criteria_Name = dplyr::case_when(CHR_UID == 791 ~ "CHLOROPHYLL_WQC",
                                       CHR_UID == 1415 ~ "TN_WQC",
                                       CHR_UID == 1674 ~ "TP_WQC",
                                       CHR_UID == 986 ~ "DOSAT_WQC",
                                       TRUE ~ Criteria_Name),
      Exceed_sample = ifelse(Criteria_Name %in% c("DO_WQC","PH_LOW"),
                             ifelse(MEASUREMENT_num < Criteria_Value, 'Yes', 'No'),
                             ifelse(MEASUREMENT_num >= Criteria_Value, 'Yes', 'No')))

  # Determine whether samples on a given date were sampled concurrently
  # Required parameters
  required_chars <- c(
    "Phosphorus as P",
    "Chlorophyll a",
    "pH",
    "Dissolved oxygen (DO)",
    "Nitrogen as N"
  )


  df_concurrence <- df_results %>%
    # keep only required parameters to determine whether they were sampled concurrently
    dplyr::filter(CHARACTERISTIC_NAME %in% required_chars) %>%
    dplyr::distinct(WATER_ID, DATE, CHARACTERISTIC_NAME) %>%
    dplyr::arrange(WATER_ID, DATE) %>%
    # build clusters PER WATER_ID based on <= 7-day gaps
    dplyr::group_by(WATER_ID) %>%
    dplyr::mutate(
      gap = as.integer(DATE - lag(DATE)),
      new_group = dplyr::if_else(is.na(gap) | gap > 7, 1L, 0L),
      cluster_id = cumsum(tidyr::replace_na(new_group, 1L))
    ) %>%
    dplyr::ungroup() %>%
    # summarize cluster completeness
    dplyr::group_by(WATER_ID, cluster_id) %>%
    dplyr::reframe(
      cluster_start = min(DATE),
      cluster_end   = max(DATE),
      n_days_span   = as.integer(cluster_end - cluster_start),
      n_params = length(unique(CHARACTERISTIC_NAME)),
      has_all_required = length(setdiff(required_chars
                                        , unique(CHARACTERISTIC_NAME))) == 0)

  df_concurrence_summ <- df_concurrence %>%
    dplyr::group_by(WATER_ID) %>%
    dplyr::reframe(n_concurrentdates = length(has_all_required[has_all_required == TRUE]),
                     concurrence_met = dplyr::case_when(n_concurrentdates >= 2 ~ "Yes",
                                                        n_concurrentdates < 2 ~ "No"))


  df_results_v2 <- df_results %>%
    dplyr::left_join(., df_concurrence_summ) %>%
    dplyr::group_by(WATER_ID, WATER_NAME, PROJECT_NAME, STATION, STATION_NAME,
                    concurrence_met, n_concurrentdates, DU, CHR_UID
                    , CHARACTERISTIC_NAME, UNITS, Criteria_Name, Criteria_Value) %>%
    dplyr::reframe(n_Samples = dplyr::n(),
                     n_Exceed = sum(Exceed_sample == "Yes")) %>%
    dplyr::mutate(Exceed = dplyr::case_when(Criteria_Name %in% c("TN_WQC"
                                                                 , "TP_WQC"
                                                                 , "CHLOROPHYLL_WQC")
                                            & n_Samples <= 4 & n_Samples >= 2
                                            & n_Exceed >= 1
                                            ~ "Yes",
                                            Criteria_Name %in% c("TN_WQC"
                                                                 , "TP_WQC"
                                                                 , "CHLOROPHYLL_WQC")
                                            & n_Samples <= 4 & n_Samples >= 2
                                            & n_Exceed < 1
                                            ~ "No",
                                            Criteria_Name %in% c("TN_WQC"
                                                                 , "TP_WQC"
                                                                 , "CHLOROPHYLL_WQC")
                                            & n_Samples == 1
                                            & n_Exceed < 1
                                            ~ "Limited Data, No Exceedance",
                                            Criteria_Name %in% c("TN_WQC"
                                                                 , "TP_WQC"
                                                                 , "CHLOROPHYLL_WQC")
                                            & n_Samples == 1
                                            & n_Exceed >= 1
                                            ~ "Limited Data, Exceedance",
                                            Criteria_Name %in% c("TN_WQC"
                                                                 , "TP_WQC"
                                                                 , "CHLOROPHYLL_WQC")
                                            & n_Samples > 4
                                            & n_Exceed >= 2
                                            ~ "Yes",
                                            Criteria_Name %in% c("TN_WQC"
                                                                 , "TP_WQC"
                                                                 , "CHLOROPHYLL_WQC")
                                            & n_Samples > 4
                                            & n_Exceed < 2
                                            ~ "No",
                                            Criteria_Name %in% c("DOSAT_WQC")
                                            & n_Exceed >= 1
                                            ~ "Flag",
                                            Criteria_Name %in% c("DO_WQC"
                                                                 , "PH_LOW"
                                                                 , "PH_HIGH")
                                            & n_Exceed < 1
                                            ~ "No",
                                            Criteria_Name %in% c("DO_WQC"
                                                                 , "PH_LOW"
                                                                 , "PH_HIGH")
                                            & n_Exceed >= 1
                                            ~ "Yes"),
                  Delist_eligible = dplyr::case_when(Criteria_Name %in% c("TN_WQC"
                                                                , "TP_WQC"
                                                                , "CHLOROPHYLL_WQC")
                                                     & n_Samples >= 2
                                                     & n_Exceed < 1
                                                     ~ "Yes",
                                                     Criteria_Name %in% c("TN_WQC"
                                                                , "TP_WQC"
                                                                , "CHLOROPHYLL_WQC")
                                                     & n_Samples < 2
                                                     ~ "No",
                                                     Criteria_Name %in% c("TN_WQC"
                                                              , "TP_WQC"
                                                              , "CHLOROPHYLL_WQC")
                                                     & n_Exceed >= 1
                                                     ~ "No",
                                                     Criteria_Name %in% c("DO_WQC"
                                                                , "PH_LOW"
                                                                , "PH_HIGH")
                                                     & n_Samples >= 1
                                                     & n_Exceed < 1
                                                     ~ "Yes",
                                                     Criteria_Name %in% c("DO_WQC"
                                                                    , "PH_LOW"
                                                                    , "PH_HIGH")
                                                     & n_Exceed >= 1
                                                     ~ "No")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Enrichment_causal = dplyr::case_when(
      Criteria_Name %in% c("TN_WQC", "TP_WQC") & Exceed == "Yes" ~ "Yes",
      Criteria_Name %in% c("TN_WQC", "TP_WQC") & Exceed == "No" ~ "No",
      Criteria_Name %in% c("TN_WQC", "TP_WQC")
      & Exceed == "Limited Data, No Exceedance" ~ "Limited Data, No Exceedance",
      Criteria_Name %in% c("TN_WQC", "TP_WQC")
      & Exceed == "Limited Data, Exceedance" ~ "Limited Data, Exceedance"),
      Enrichment_response = dplyr::case_when(
        Criteria_Name %in% c("DO_WQC", "PH_LOW", "PH_HIGH") & Exceed == "Yes" ~ "Yes",
        Criteria_Name %in% c("DO_WQC", "PH_LOW", "PH_HIGH") & Exceed == "No" ~ "No"),
      Enrichment_chlorophyll = dplyr::case_when(
        Criteria_Name %in% c("CHLOROPHYLL_WQC") & Exceed == "Yes" ~ "Yes",
        Criteria_Name %in% c("CHLOROPHYLL_WQC") & Exceed == "No" ~ "No",
        Criteria_Name %in% c("CHLOROPHYLL_WQC")
        & Exceed == "Limited Data, No Exceedance" ~ "Limited Data, No Exceedance",
        Criteria_Name %in% c("CHLOROPHYLL_WQC")
        & Exceed == "Limited Data, Exceedance" ~ "Limited Data, Exceedance")) %>%
    dplyr::group_by(WATER_ID, WATER_NAME, PROJECT_NAME, STATION, STATION_NAME, DU) %>%
    # Set up support indicators corresponding to Figure 2 in Appendix D
    dplyr::mutate(Enrichment_causal = ifelse(any(Enrichment_causal == "Yes"
                                                 , na.rm = TRUE), "Yes", ifelse(
      any(Enrichment_causal == "No", na.rm = TRUE), "No", ifelse(
        any(Enrichment_causal == "Limited Data, Exceedance", na.rm = TRUE)
        , "Limited Data, Exceedance", ifelse(
          any(Enrichment_causal == "Limited Data, No Exceedance", na.rm = TRUE)
          , "Limited Data, No Exceedance", NA
        )))),
      Enrichment_response = ifelse(any(Enrichment_response == "Yes", na.rm = TRUE)
                                   , "Yes", ifelse(
        any(Enrichment_response == "No", na.rm = TRUE), "No", NA)),
      Enrichment_chlorophyll = ifelse(any(Enrichment_chlorophyll == "Yes"
                                          , na.rm = TRUE), "Yes", ifelse(
        any(Enrichment_chlorophyll == "No", na.rm = TRUE), "No", ifelse(
          any(Enrichment_chlorophyll == "Limited Data, Exceedance"
              , na.rm = TRUE), "Limited Data, Exceedance", ifelse(
            any(Enrichment_chlorophyll == "Limited Data, No Exceedance"
                , na.rm = TRUE), "Limited Data, No Exceedance", NA)))),
      DOsat_flag = ifelse(any(Exceed == "Flag", na.rm = TRUE), "Flag", "No Flag"),
      Delist_eligible = ifelse(any(Delist_eligible == "No"), "No", Delist_eligible)) %>%
    # format for results output
    dplyr::ungroup() %>%
    dplyr::mutate(R_Script_Name = "Nutrients_Lakes") %>%
    dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, STATION, STATION_NAME,
                  CHR_UID, CHARACTERISTIC_NAME, UNITS, R_Script_Name, DU,
                  n_Samples, Criteria_Value, Exceed, n_Exceed,
                  Enrichment_causal, Enrichment_response, Enrichment_chlorophyll
                  , DOsat_flag,concurrence_met, n_concurrentdates, Delist_eligible)

  df_results_v3 <- df_results_v2 %>%
    dplyr::group_by(WATER_ID, WATER_NAME) %>%
    dplyr::mutate(Char_Exceeding = paste(unique(CHARACTERISTIC_NAME[Exceed == "Yes"])
                                         , collapse = ",")) %>%
    dplyr::ungroup() %>%
    dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, DU,
                  Enrichment_causal, Enrichment_response, Enrichment_chlorophyll,
                  DOsat_flag, concurrence_met, n_concurrentdates, Delist_eligible,
                  Char_Exceeding) %>%
    dplyr::distinct() %>%
    # Apply flowchart in Figure 2 in Appendix D
    dplyr::mutate(Category = dplyr::case_when(
      # Not supporting
      Enrichment_causal == "Yes" & Enrichment_response == "Yes" ~ "4 or 5",
      Enrichment_causal == "Yes" & Enrichment_chlorophyll == "Yes"  ~ "4 or 5",
      Enrichment_causal == "No" & Enrichment_response == "Yes" &
        Enrichment_chlorophyll == "Yes"  ~ "4 or 5",
      # Fully supporting
      Enrichment_causal == "Yes" & Enrichment_response == "No" &
        Enrichment_chlorophyll == "No" & concurrence_met == "Yes" ~ "1/2",
      Enrichment_causal == "No" & Enrichment_response == "No"
      & concurrence_met == "Yes" ~ "1 or 2",
      Enrichment_causal == "No" & Enrichment_chlorophyll == "No"
      & concurrence_met == "Yes"  ~ "1 or 2",
      # Limited data, exceedance(s)
      Enrichment_causal == "Limited Data, Exceedance" |
        Enrichment_chlorophyll == "Limited Data, Exceedance" ~ "3c",
      # Limited data, no exceedance(s)
      Enrichment_causal == "Limited Data, No Exceedance"
      &  Enrichment_response == "No" &
        Enrichment_chlorophyll == "No" ~ "3b",
      Enrichment_causal == "Limited Data, No Exceedance"
      &  Enrichment_response == "No" &
        Enrichment_chlorophyll == "Limited Data, No Exceedance" ~ "3b",
      Enrichment_chlorophyll == "Limited Data, No Exceedance"
      &  Enrichment_causal == "No" &
        Enrichment_response == "No" ~ "3b",
      Enrichment_causal == "No" & Enrichment_response == "No"
      & concurrence_met == "No" ~ "3b",
      Enrichment_causal == "No" & Enrichment_chlorophyll == "No"
      & concurrence_met == "No"  ~ "3b",
      # Not assessed
      is.na(Enrichment_causal) & is.na(Enrichment_response) ~ "3a",
      is.na(Enrichment_causal) & is.na(Enrichment_chlorophyll) ~ "3a"
    )) %>%
    # category 3s are NA for delist eligibility
    dplyr::mutate(Delist_eligible = dplyr::case_when(Category
                                                     %in% c("3a", "3b", "3c") ~ NA,
                                              TRUE ~ Delist_eligible))


  # Export data ####
  return(list(
    Nutrients_Lakes = df_results_v3,
    Nutrients_Lakes_Indiv_Res = df_results_v2))

} # END ~ Function
