#' Compile data from SQUID for water quality analyses
#'
#' This function compiles multiple tables from SQUID into a single output used for
#' subsequent water quality analysis functions. The LTD and lake profile datasets
#' are optional, whereas the RStudio, stations DU, parameter, and criteria tables
#' are required for the function to run.
#'
#' @param criteria_table Criteria table is output from SQUID. Filters water quality
#' data to those in the criteria table. Has criteria to compare data against.
#' @param parameter_table Parameter table is output from SQUID. Filters water quality
#' data to those in the parameter table.
#' @param SQUID_RStudio_table RStudio query from SQUID. Entirely grab data from SQUID
#' projects.
#' @param SQUID_DU_table Stations DU table from SQUID. Has DU and AU specific water
#' quality criteria which are used in some cases.
#' @param SQUID_LTD_table Optional long-term deployment summary table from SQUID.
#' Not raw data.
#' @param SQUID_LakeProfile_table Optional lake profile table from SQUID.
#'
#' @returns A dataframe containing compiled and quality controlled water quality
#' necessary for subsequent analyses.
#'
#' @examples
#' \dontrun{
#' #' Data_Prep <- function(criteria_table = my_Criteria
#' , parameter_table = my_Parameter
#' , SQUID_RStudio_table = my_RStudio
#' , SQUID_DU_table = my_DU
#' , SQUID_LTD_table = my_LTD
#' , SQUID_LakeProfile_table = my_Profile)}
#'
Data_Prep <- function(criteria_table
                      , parameter_table
                      , SQUID_RStudio_table
                      , SQUID_DU_table
                      , SQUID_LTD_table = NULL
                      , SQUID_LakeProfile_table = NULL){

  # QC ####

  # QC messages for required files
  if (missing(criteria_table)) stop("Error: 'criteria_table' is required but was
                                    not provided. This is a SQUID output.")
  if (missing(parameter_table)) stop("Error: 'parameter_table' is required but
                                     was not provided. This is a SQUID output.")
  if (missing(SQUID_RStudio_table)) stop("Error: 'SQUID_RStudio_table' is
                                         required but was not provided. This is
                                         a project-specific RStudio output from
                                         SQUID.")
  if (missing(SQUID_DU_table)) stop("Error: 'SQUID_DU_table' is required but was
                                    not provided. This is a project-specific
                                    designated use output from SQUID.")

  # Check optional arguments separately for custom messages
  if (missing(SQUID_LTD_table)) {
    message("Note: 'SQUID_LTD_table' was not provided.
            LTD-related processing will be skipped.")
  } # End ~ if statement

  if (missing(SQUID_LakeProfile_table)) {
    message("Note: 'SQUID_LakeProfile_table' was not provided.
            Lake profile data will not be included.")
  } # End ~ if statement

  # Format Data ####
  ## Parameters ####
  # This table is a list of parameters that are actually assessed by NMED.
  # QC Warning
  required_cols <- c("CHR_UID", "CHR_NAME", "Units")

  missing_cols <- setdiff(required_cols, colnames(parameter_table))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns are missing from the
             parameters dataframe (parameter_table):",
               paste(missing_cols, collapse = ", ")))
  } # End ~ if statement

  # trim
  df_Params_Assess <- parameter_table %>%
    dplyr::select(dplyr::one_of(required_cols)) %>%
    dplyr::arrange(CHR_NAME) %>%
    dplyr::mutate(CHR_UID = as.character(CHR_UID)
                  , UNITS = dplyr::case_when((Units == "ug/l") ~ "ugL"
                                             , (Units == "MPN/100ml") ~ "MPN100mL"
                                             , (Units == "MPN/100mL") ~ "MPN100mL"
                                             , (Units == "mg/l") ~ "mgL"
                                             , (Units == "pCi/L") ~ "pCiL"
                                             , (Units == "#/ml") ~ "Num_mL"
                                             , (Units == "uS/cm") ~ "uScm"
                                             , (Units == "deg C") ~ "DegC"
                                             , (Units == "%") ~ "Pct"
                                             , (Units == "fibers/L") ~ "fibers_L"
                                             , (Units == "mg/kg fish tissue")
                                                ~ "mgKg_Fish_Tissue"
                                             , (Units == "None") ~ "none"
                                             , TRUE ~ Units)) %>%
    dplyr::select(-c(Units))

  # cleanup
  rm(parameter_table, required_cols, missing_cols)

  ## Criteria ####
  # QC Warning
  required_cols <- c("CHR_UID", "CHR_NAME", "FRACTION", "DU", "WBODY", "WGROUP"
                     , "PERSISTENT", "HD_METAL", "TOXIC", "TURBAP", "UNITS"
                     , "ECOLI_SINGLE", "SC_WQC", "TEMP_WQC", "LT", "GT", "ACUTE"
                     , "CHRONIC", "HH", "DWS", "IRR", "LW", "WH")


  missing_cols <- setdiff(required_cols, colnames(criteria_table))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns are missing from the
             criteria dataframe (criteria_table):",
               paste(missing_cols, collapse = ", ")))
  } # End ~ if statement

  # cleanup
  rm(required_cols, missing_cols)

  # Aqualtic life uses
  df_Criteria_v2 <- criteria_table %>%
    # Filter below removes all criteria found in df_DU. Redundant. Use df_DU values.
    dplyr::filter(!(CHR_UID == "2287" | CHR_UID == "2849" | CHR_UID == "1648"
                    | CHR_UID == "985" | CHR_UID == "1815" | CHR_UID == "1674"
                    | CHR_UID == "988" | CHR_UID == "1827" | CHR_UID == "1648"
                    | CHR_UID == "773")) %>%
    dplyr::mutate(CHR_UID = as.character(CHR_UID)
           , UNITS = dplyr::case_when((UNITS == "ug/l") ~ "ugL"
                                      , (UNITS == "MPN/100ml") ~ "MPN100mL"
                                      , (UNITS == "MPN/100mL") ~ "MPN100mL"
                                      , (UNITS == "mg/l") ~ "mgL"
                                      , (UNITS == "pCi/L") ~ "pCiL"
                                      , (UNITS == "#/ml") ~ "Num_mL"
                                      , (UNITS == "uS/cm") ~ "uScm"
                                      , (UNITS == "deg C") ~ "DegC"
                                      , (UNITS == "%") ~ "Pct"
                                      , (UNITS == "fibers/L") ~ "fibers_L"
                                      , (UNITS == "mg/kg fish tissue") ~ "mgKg_Fish_Tissue"
                                      , (UNITS == "None") ~ "none"
                                      , TRUE ~ UNITS)
           , Waterbody = dplyr::case_when((!is.na(WBODY)) ~ WBODY
                                          , (is.na(WBODY) & !is.na(WGROUP)) ~ WGROUP
                                          , (is.na(WBODY) & is.na(WGROUP)) ~ NA)) %>%
    dplyr::select(CHR_UID, CHR_NAME, FRACTION, DU, Waterbody, PERSISTENT, HD_METAL, TOXIC
                  , TURBAP, UNITS, ECOLI_SINGLE, SC_WQC, TEMP_WQC, LT, GT, ACUTE, CHRONIC
                  , HH) %>%
    tidyr::pivot_longer(!c(CHR_UID, CHR_NAME, FRACTION, DU, Waterbody, PERSISTENT, HD_METAL, TOXIC
                           , TURBAP, UNITS)
                        , names_to = "Criteria_Type"
                        , values_to = "Magnitude_Numeric") %>%
    dplyr::filter(!is.na(Magnitude_Numeric)) %>%
    dplyr::filter(!(DU == "LAL" & Criteria_Type == "CHRONIC"
                    & CHR_UID %in% c(1245, 1280, 7506))) %>% # remove, erroneous
    dplyr::filter(!(DU == "LAL" & Criteria_Type == "HH" & is.na(PERSISTENT))) %>% # remove, erroneous
    dplyr::mutate(HD_METAL = dplyr::case_when((CHR_UID == "1793") ~ "Y" # Silver should by HDM
                                       , TRUE ~ HD_METAL))

  # Other uses
  df_Criteria_OtherUses <- criteria_table %>%
    dplyr::mutate(CHR_UID = as.character(CHR_UID)
                  , UNITS = dplyr::case_when((UNITS == "ug/l") ~ "ugL"
                                             , (UNITS == "MPN/100ml") ~ "MPN100mL"
                                             , (UNITS == "MPN/100mL") ~ "MPN100mL"
                                             , (UNITS == "mg/l") ~ "mgL"
                                             , (UNITS == "pCi/L") ~ "pCiL"
                                             , (UNITS == "#/ml") ~ "Num_mL"
                                             , (UNITS == "uS/cm") ~ "uScm"
                                             , (UNITS == "deg C") ~ "DegC"
                                             , (UNITS == "%") ~ "Pct"
                                             , (UNITS == "fibers/L") ~ "fibers_L"
                                             , (UNITS == "mg/kg fish tissue") ~ "mgKg_Fish_Tissue"
                                             , (UNITS == "None") ~ "none"
                                             , TRUE ~ UNITS)
                  , Waterbody = dplyr::case_when((!is.na(WBODY)) ~ WBODY
                                                 , (is.na(WBODY) & !is.na(WGROUP)) ~ WGROUP
                                                 , (is.na(WBODY) & is.na(WGROUP)) ~ NA)) %>%
    dplyr::select(CHR_UID, CHR_NAME, FRACTION, Waterbody, PERSISTENT, HD_METAL, TOXIC
                  , TURBAP, UNITS, DWS, IRR, LW, WH) %>%
    tidyr::pivot_longer(!c(CHR_UID, CHR_NAME, FRACTION, Waterbody, PERSISTENT, HD_METAL
                           , TOXIC, TURBAP, UNITS)
                        , names_to = "DU"
                        , values_to = "Magnitude_Numeric") %>%
    dplyr::filter(!is.na(Magnitude_Numeric)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Criteria_Type = DU) %>%
    dplyr::mutate(CHR_UID = dplyr::case_when((CHR_NAME == "Barium") ~ "611" # fixes error in CHR_UID
                                             , TRUE ~ CHR_UID))

  # Join
  df_Criteria_v3 <- rbind(df_Criteria_v2, df_Criteria_OtherUses)

  # cleanup
  rm(criteria_table, df_Criteria_v2, df_Criteria_OtherUses)

  ## Chem data ####
  # QC Warning
  required_cols <- c("WATER_ID", "WATER_NAME", "PROJECT_NAME", "STATION"
                     , "STATION_NAME", "SAMPLING_EVENT_TYPE", "DATE", "TIME"
                     , "ACTIVITY_TYPE", "SAMPLE_TYPE", "FLT_10UG", "SAMPLE_FRACTION"
                     , "CHR_UID", "CHARACTERISTIC_NAME", "MEASUREMENT", "UNITS"
                     , "LESS_THAN_YN", "SWQB_QUALIFIER_CODE", "SDL")

  missing_cols <- setdiff(required_cols, colnames(SQUID_RStudio_table))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns are missing from the
             grab chemistry dataframe (SQUID_RStudio_table):",
               paste(missing_cols, collapse = ", ")))
  } # End ~ if statement

  # cleanup
  rm(required_cols, missing_cols)

  # trim
  df_Chem_v2 <- SQUID_RStudio_table %>%
    dplyr::filter(!(SWQB_QUALIFIER_CODE %in% c("R1", "R2", "R3", "Er", "ER"
                                               , "ER3"))) %>% # remove rejected data
    dplyr::filter(!(MEASUREMENT == "mdp" | MEASUREMENT == "MDP")) %>% # missing data point
    dplyr::mutate(DATE = as.Date(DATE, format = "%m/%d/%y")
                  , Station_Date = paste0(STATION,"_",DATE)
                  , MEASUREMENT_num = suppressWarnings(dplyr::case_when(
                    (grepl(">", MEASUREMENT)) ~ as.numeric(SDL)
                    , TRUE ~ as.numeric(MEASUREMENT)))) %>%
    dplyr::relocate(MEASUREMENT_num, .after = MEASUREMENT)

  ### QC Flood/Baseflow ####
  # In CALM and current R scripts. Need to flag for analysis.
  QC_Flood <- df_Chem_v2 %>%
    dplyr::filter(MEASUREMENT == "5 - flood flow" | SAMPLE_TYPE == "stormwater") %>%
    dplyr::select(Station_Date) %>%
    dplyr::distinct() %>%
    dplyr::pull(Station_Date)

  # Baseflow flag only for turbidity
  QC_Baseflow <- df_Chem_v2 %>%
    dplyr::filter(MEASUREMENT == "3 - moderate flow" | MEASUREMENT == "2 - low flow") %>%
    dplyr::select(Station_Date) %>%
    dplyr::distinct() %>%
    dplyr::pull(Station_Date)

  df_Chem_v3 <- df_Chem_v2 %>%
    dplyr::mutate(SAMPLE_TYPE = dplyr::case_when((Station_Date %in% QC_Flood) ~ "Storm"
                                                 , ((Station_Date %in% QC_Baseflow)
                                                    & CHR_UID == 1977) ~ "Baseflow"
                                                 , TRUE ~ SAMPLE_TYPE))

  # cleanup
  rm(QC_Flood, QC_Baseflow, df_Chem_v2)

  ### QC Dups ####
  # Check for duplicate site/date/parameter - take higher value (per Lynette)
  df_Chem_v4 <- df_Chem_v3 %>%
    dplyr::group_by(STATION, DATE, CHR_UID) %>%
    dplyr::slice_max(MEASUREMENT, with_ties = FALSE)

  # cleanup
  rm(df_Chem_v3)

  ### Trim Columns ####
  required_cols <- c("WATER_ID", "WATER_NAME", "PROJECT_NAME", "STATION"
                     , "STATION_NAME", "SAMPLING_EVENT_TYPE", "DATE", "TIME"
                     , "ACTIVITY_TYPE", "SAMPLE_TYPE", "FLT_10UG", "SAMPLE_FRACTION"
                     , "CHR_UID", "CHARACTERISTIC_NAME", "MEASUREMENT"
                     , "MEASUREMENT_num", "UNITS", "LESS_THAN_YN")

  # trim
  df_Chem_v5 <- df_Chem_v4 %>%
    dplyr::select(dplyr::one_of(required_cols))

  # cleanup
  rm(df_Chem_v4, required_cols)

  ### Filter Assess Params ####
  # specify additional parameters to keep
  nutr_params <- c("1674" #"Phosphorus as P"
                   , "2245" #"Depth, bottom"
                   , "2248" #"Depth, Secchi disk depth"
                   , "1418" #"Nitrogen, Nitrite (NO2) + Nitrate (NO3) as N"
                   , "1416" #"Total Kjeldahl nitrogen (Organic N & NH3)"
                   , "791" #"Chlorophyll a"
  ) # END ~ nutr_params

  other_params <- c("4528" # "Total Hardness"
                    , "1775" #"Salinity"
                    , "988" #"Total dissolved solids"
                    , "1827" #"Sulfate"
                    , "773" #"Chloride"
                    , "986" # DO Saturation
  ) # END ~ other_params

  # Filter
  df_Chem_v6 <- df_Chem_v5 %>%
    dplyr::filter(CHR_UID %in% nutr_params # nutrients
                  | CHR_UID %in% df_Params_Assess$CHR_UID # parameters to assess
                  | CHR_UID %in% other_params # other parameters for assessment
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(UNITS = dplyr::case_when((UNITS == "ug/l") ~ "ugL"
                                           , (UNITS == "MPN/100ml") ~ "MPN100mL"
                                           , (UNITS == "MPN/100mL") ~ "MPN100mL"
                                           , (UNITS == "mg/l") ~ "mgL"
                                           , (UNITS == "pCi/L") ~ "pCiL"
                                           , (UNITS == "#/ml") ~ "Num_mL"
                                           , (UNITS == "uS/cm") ~ "uScm"
                                           , (UNITS == "deg C") ~ "DegC"
                                           , (UNITS == "%") ~ "Pct"
                                           , (UNITS == "fibers/L") ~ "fibers_L"
                                           , (UNITS == "mg/kg fish tissue")
                                              ~ "mgKg_Fish_Tissue"
                                           , (UNITS == "None") ~ "none"
                                           , TRUE ~ UNITS))

  # cleanup
  rm(df_Chem_v5, df_Params_Assess, other_params)

  ## DU table ####
  required_cols <- c("WATER_ID", "DU", "WBODY", "ECOLI_GEOMEAN", "ECOLI_SINGLE"
                     , "TEMP_6T3", "TEMP_4T3", "TEMP_WQC", "PH_LOW", "PH_HIGH"
                     , "DO_WQC", "SC_WQC", "TP_WQC", "TDS_WQC", "SO4_WQC", "CHL_WQC"
                     , "TN_SITE_CLASS", "TP_SITE_CLASS", "ECOREGION", "ELEVATION")

  missing_cols <- setdiff(required_cols, colnames(SQUID_DU_table))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns are missing from the
             designated use dataframe (SQUID_DU_table):",
               paste(missing_cols, collapse = ", ")))
  } # End ~ if statement

  df_DU_v2 <- SQUID_DU_table %>%
    dplyr::select(dplyr::one_of(required_cols)) %>%
    dplyr::group_by(WATER_ID) %>%
    dplyr::slice_min(ELEVATION) %>% # takes minimum of all station elevations per AU
    dplyr::rename(Waterbody = WBODY) %>%
    dplyr::filter(DU != "LW" & DU != "FC" & DU != "PWS" & DU != "DWS"
                  & DU != "WH")# should not include these uses

  # cleanup
  rm(SQUID_DU_table, required_cols, missing_cols)

  ### QC Multi-ALU ####
  # add QC check to ensure that each station only has one ALU (can have other uses)
  ALUs <- c("ColdWAL", "CoolWAL", "HQColdWAL", "LAL", "MCWAL", "MWWAL", "WWAL")

  df_DU_ALU <- df_DU_v2 %>%
    dplyr::filter(DU %in% ALUs) %>%
    dplyr::mutate(ALU_Rank = dplyr::case_when((DU == "HQColdWAL") ~ 1
                                              , (DU == "ColdWAL") ~ 2
                                              , (DU == "CoolWAL") ~ 3
                                              , (DU == "MCWAL") ~ 4
                                              , (DU == "WWAL") ~ 5
                                              , (DU == "MWWAL") ~ 6
                                              , (DU == "LAL") ~ 7
                                              , TRUE ~ NA)) %>%
    dplyr::group_by(WATER_ID) %>%
    dplyr::slice_min(ALU_Rank) %>%
    dplyr::select(-c(ALU_Rank))

  df_DU_nonALU <- df_DU_v2 %>%
    dplyr::filter(!(DU %in% ALUs))

  df_DU_v3 <- rbind(df_DU_ALU, df_DU_nonALU)

  # cleanup
  rm(df_DU_v2, df_DU_ALU, df_DU_nonALU)

  ## Pivot Longer ####
  df_DU_v4 <- df_DU_v3 %>%
    tidyr::pivot_longer(!c(WATER_ID, DU, Waterbody, TN_SITE_CLASS, TP_SITE_CLASS
                           , ECOREGION, ELEVATION)
                        , names_to = "Criteria_Name"
                        , values_to = "Criteria_Value") %>%
    dplyr::filter(!is.na(Criteria_Value)) %>%
    dplyr::mutate(CHR_UID = dplyr::case_when((Criteria_Name == "CHL_WQC") ~ 773
                                             , (Criteria_Name == "DO_WQC") ~ 985
                                             , (Criteria_Name == "ECOLI_GEOMEAN"
                                                | Criteria_Name == "ECOLI_SINGLE") ~ 2287
                                             , (Criteria_Name == "PH_HIGH"
                                                | Criteria_Name == "PH_LOW") ~ 1648
                                             , (Criteria_Name == "SC_WQC") ~ 1815
                                             , (Criteria_Name == "SO4_WQC") ~ 1827
                                             , (Criteria_Name == "TDS_WQC") ~ 988
                                             , (Criteria_Name == "TEMP_WQC") ~ 2849
                                             , (Criteria_Name == "TEMP_4T3") ~ 999920
                                             , (Criteria_Name == "TEMP_6T3") ~ 999921
                                             , (Criteria_Name == "TP_WQC") ~ 1674)
                  , CHR_UID_Unique = dplyr::case_when((Criteria_Name == "ECOLI_GEOMEAN")
                                                          ~ "2287a"
                                                      , (Criteria_Name == "ECOLI_SINGLE")
                                                          ~ "2287b"
                                                      , (Criteria_Name == "PH_HIGH")
                                                          ~ "1648a"
                                                      , (Criteria_Name == "PH_LOW")
                                                          ~ "1648b"
                                                      , TRUE ~ as.character(CHR_UID))
                  , DU = dplyr::case_when((DU == "IRR Storage") ~ "IRR"
                                          , TRUE ~ DU)) %>%
    # The following changes made to match NMED expectations given SQUID format.
    dplyr::filter(!(DU %in% ALUs & grepl("ECOLI_", Criteria_Name))) %>%
    dplyr::filter(!((DU == "WWAL" | DU == "PC")
                    & (CHR_UID == 988 | CHR_UID == 1827 | CHR_UID == 773))) %>%
    dplyr::filter(!((DU == "PC" | DU == "IRR" | DU == "SC")
                    & (CHR_UID == 2849 | CHR_UID == 985| CHR_UID == 1674
                       | CHR_UID == 999920 | CHR_UID == 999921 | CHR_UID == 1815))) %>%
    dplyr::filter(!(DU == "IRR" & (CHR_UID == 1648 | CHR_UID == 2287))) %>%
    dplyr::filter(!(DU == "SC" & (CHR_UID == 1648))) %>%
    dplyr::filter(!(DU == "MWWAL" & CHR_UID == 999921))

  # cleanup
  rm(df_DU_v3)

  ## LTD Data ####
  if(exists("SQUID_LTD_table")){
    # QC Warning
    required_cols <- c("ASSESSMENT_UNIT_ID", "ASSESSMENT_UNIT_NAME", "PROJECT_NAME"
                       , "STATION_ID", "STATION_NAME", "SAMPLING_EVENT_TYPE"
                       , "ACT_START_DATE", "ACT_END_DATE", "ACTIVITY_TYPE", "CHR_UID"
                       , "CHARACTERISTIC_NAME", "MEASUREMENT", "UNITS"
                       , "ASSESSABILITY_QUALIFIER_CODE")

    missing_cols <- setdiff(required_cols, colnames(SQUID_LTD_table))

    if (length(missing_cols) > 0) {
      stop(paste("The following required columns are missing from the
             LTD dataframe (SQUID_LTD_table):",
                 paste(missing_cols, collapse = ", ")))
    } # End ~ if statement

    # cleanup
    rm(required_cols, missing_cols)

    # Format
    df_LTD_v2 <- SQUID_LTD_table %>%
      dplyr::select(ASSESSMENT_UNIT_ID, ASSESSMENT_UNIT_NAME, PROJECT_NAME
                    , STATION_ID, STATION_NAME, SAMPLING_EVENT_TYPE, ACT_START_DATE
                    , ACT_END_DATE, ACTIVITY_TYPE, CHR_UID, CHARACTERISTIC_NAME
                    , MEASUREMENT, UNITS, ASSESSABILITY_QUALIFIER_CODE) %>%
      dplyr::filter(!(MEASUREMENT == "mdp" | MEASUREMENT == "MDP")) %>% # missing data point
      dplyr::mutate(SAMPLE_TYPE = "Surface Water"
                    , DATE = as.Date(ACT_START_DATE, format = "%Y-%m-%d %H:%M:%S")
                    , DATE_START = as.Date(ACT_START_DATE
                                           , format = "%Y-%m-%d %H:%M:%S")
                    , DATE_END = as.Date(ACT_END_DATE
                                         , format = "%Y-%m-%d %H:%M:%S")
                    , TIME = format(as.POSIXct(ACT_START_DATE
                                               , format = "%Y-%m-%d %H:%M:%S")
                                    , format = "%H:%M")
                    , MEASUREMENT_num = as.numeric(MEASUREMENT)
                    , FLT_10UG = NA
                    , SAMPLE_FRACTION = NA
                    , LESS_THAN_YN = NA) %>%
      dplyr::rename(WATER_ID = ASSESSMENT_UNIT_ID
                    , WATER_NAME = ASSESSMENT_UNIT_NAME
                    , STATION = STATION_ID) %>%
      dplyr::mutate(UNITS = dplyr::case_when((UNITS == "ug/l") ~ "ugL"
                                             , (UNITS == "MPN/100ml") ~ "MPN100mL"
                                             , (UNITS == "MPN/100mL") ~ "MPN100mL"
                                             , (UNITS == "mg/l") ~ "mgL"
                                             , (UNITS == "pCi/L") ~ "pCiL"
                                             , (UNITS == "#/ml") ~ "Num_mL"
                                             , (UNITS == "uS/cm") ~ "uScm"
                                             , (UNITS == "deg C") ~ "DegC"
                                             , (UNITS == "%") ~ "Pct"
                                             , (UNITS == "fibers/L") ~ "fibers_L"
                                             , (UNITS == "mg/kg fish tissue")
                                                ~ "mgKg_Fish_Tissue"
                                             , (UNITS == "None") ~ "none"
                                             , TRUE ~ UNITS)) %>%
      dplyr::filter(CHR_UID == "999920" | CHR_UID == "999921"
                    | (CHR_UID == "985"
                       & CHARACTERISTIC_NAME == "Minimum - Dissolved oxygen (DO)")
                    | CHR_UID == "999919"
                    | CHARACTERISTIC_NAME %in% c('Maximum - Specific conductance'
                                                 , 'Minimum - pH'
                                                 , 'Maximum - pH'
                                                 , 'Maximum - Temperature, water'
                                                 , 'Maximum - Turbidity')) %>%
      dplyr::select(-c(DATE_START, DATE_END))

    unique(df_LTD_v2$CHARACTERISTIC_NAME)

    # cleanup
    rm(SQUID_LTD_table)
  } # END ~ if LTD exists

  ## Depth profile ####
  if(exists("SQUID_LakeProfile_table")){
    ### Initial cleanup ####
    # summary(df_Profile)
    SQUID_LakeProfile_table$SE_START_DATE_TIME <- as.POSIXct(SQUID_LakeProfile_table$SE_START_DATE_TIME)
    SQUID_LakeProfile_table$RES_DEPTH_HEIGHT <- as.numeric(SQUID_LakeProfile_table$RES_DEPTH_HEIGHT)
    SQUID_LakeProfile_table$MEASUREMENT <- as.numeric(SQUID_LakeProfile_table$MEASUREMENT)

    # QC Warning
    required_cols <- c("WATER_ID", "PROJECT_NAME", "MLOC_NAME", "MLOC_ID", "ACT_ID"
                       , "RES_DEPTH_HEIGHT", "CHARACTERISTIC", "MEASUREMENT"
                       , "SE_START_DATE_TIME", "PRJ_UID", "MLOC_UID")

    missing_cols <- setdiff(required_cols, colnames(SQUID_LakeProfile_table))

    if (length(missing_cols) > 0) {
      stop(paste("The following required columns are missing from the
             lake depth profile dataframe (SQUID_LakeProfile_table):",
                 paste(missing_cols, collapse = ", ")))
    } # End ~ if statement

    # cleanup
    rm(required_cols, missing_cols)

    # Create wide dataset
    df_Profile_wide <- SQUID_LakeProfile_table %>%
      dplyr::filter(!(SWQB_QUALIFIER_CODE %in% c("R1", "R2", "R3", "Er", "ER"
                                                 , "ER3"))) %>% # remove rejected data
      dplyr::select(WATER_ID, PROJECT_NAME, MLOC_NAME, MLOC_ID, ACT_ID
                    , RES_DEPTH_HEIGHT, CHARACTERISTIC, MEASUREMENT
                    , SE_START_DATE_TIME, PRJ_UID, MLOC_UID) %>%
      tidyr::pivot_wider(names_from = CHARACTERISTIC, values_from = MEASUREMENT)

    ### Calculate thermoclines ####
    df_thermoclines <- df_Profile_wide %>%
      dplyr::group_by(WATER_ID, PROJECT_NAME, MLOC_NAME, MLOC_ID,
                      SE_START_DATE_TIME, PRJ_UID, MLOC_UID) %>%
      dplyr::summarise(thermocline = rLakeAnalyzer::thermo.depth(wtr = `Temperature, water`,
                                                  depths = RES_DEPTH_HEIGHT,
                                                  mixed.cutoff = 1),
                       depth_max = max(RES_DEPTH_HEIGHT)) %>%
      # remove thermoclines < 1 m from the surface and bottom (considered transient)
      dplyr::mutate(thermocline = dplyr::case_when(thermocline < 1 ~ NA_real_,
                                      (depth_max - thermocline) < 1 ~ NA_real_,
                                      TRUE ~ thermocline),
                    depth_assess = dplyr::case_when(!is.na(thermocline)
                                                      ~ "epilimnion",
                                                    is.na(thermocline)
                                                      ~ "onethird"))
    ### Calculate grab values ####
    # relevant for temp, pH, DO, and DO saturation
    df_Profile_summ <- df_Profile_wide %>%
      dplyr::left_join(df_thermoclines)

    # if thermocline is present, summarize for the average epilimnion value
    df_Profile_epi <- df_Profile_summ %>%
      dplyr::filter(depth_assess == "epilimnion") %>%
      dplyr::filter(RES_DEPTH_HEIGHT <= thermocline) %>%
      tidyr::pivot_longer(cols = c("Salinity", "Turbidity", "Specific conductance",
                                   "Dissolved oxygen saturation",
                                   "Temperature, water", "Dissolved oxygen (DO)",
                                   "pH"),
                          names_to = "CHARACTERISTIC", values_to = "MEASUREMENT") %>%
      dplyr::group_by(WATER_ID, PROJECT_NAME, MLOC_NAME, MLOC_ID, ACT_ID
                      , SE_START_DATE_TIME,PRJ_UID, MLOC_UID, thermocline
                      , depth_max, depth_assess,CHARACTERISTIC) %>%
      dplyr::summarise(MEASUREMENT = round(mean(MEASUREMENT, na.rm = TRUE), 2))

    # if thermocline is not present, summarize for the average upper 1/3 of water column.
    df_Profile_mix <- df_Profile_summ %>%
      dplyr::filter(depth_assess == "onethird") %>%
      dplyr::filter(RES_DEPTH_HEIGHT <= (depth_max/3)) %>%
      tidyr::pivot_longer(cols = c("Salinity", "Turbidity", "Specific conductance",
                                   "Dissolved oxygen saturation",
                                   "Temperature, water","Dissolved oxygen (DO)",
                                   "pH"),
                          names_to = "CHARACTERISTIC", values_to = "MEASUREMENT") %>%
      dplyr::group_by(WATER_ID, PROJECT_NAME, MLOC_NAME, MLOC_ID, ACT_ID
                      , SE_START_DATE_TIME, PRJ_UID, MLOC_UID, thermocline
                      , depth_max, depth_assess, CHARACTERISTIC) %>%
      dplyr::summarise(MEASUREMENT = round(mean(MEASUREMENT, na.rm = TRUE), 2))

    df_Profile_summ_v2 <- rbind(df_Profile_epi, df_Profile_mix)

    ### Format ####
    df_Profile_summ_v3 <- df_Profile_summ_v2 %>%
      dplyr::ungroup() %>%
      dplyr::mutate(DATE = as.Date(SE_START_DATE_TIME, format = "%Y-%m-%d %H:%M:%S")
                    , TIME = format(as.POSIXct(SE_START_DATE_TIME
                                               , format = "%Y-%m-%d %H:%M:%S")
                                    , format = "%H:%M")
                    , MEASUREMENT_num = as.numeric(MEASUREMENT)
                    , SAMPLING_EVENT_TYPE = "Lake Depth Profile Summary"
                    , ACTIVITY_TYPE = "Lake Depth Profile Summary"
                    , SAMPLE_TYPE = "Lake Depth Profile Summary"
                    , FLT_10UG = NA
                    , SAMPLE_FRACTION = NA
                    , LESS_THAN_YN = NA
                    , WATER_NAME = NA
                    , CHR_UID = dplyr::case_when(
                      (CHARACTERISTIC == "Dissolved oxygen (DO)") ~ "985"
                      , (CHARACTERISTIC == "Dissolved oxygen saturation") ~ "986"
                      , (CHARACTERISTIC == "Salinity") ~ "1775"
                      , (CHARACTERISTIC == "Specific conductance") ~ "1815"
                      , (CHARACTERISTIC == "Temperature, water") ~ "2849"
                      , (CHARACTERISTIC == "Turbidity") ~ "1977"
                      , (CHARACTERISTIC == "pH") ~ "1648")
                    , UNITS = dplyr::case_when(
                      (CHARACTERISTIC == "Dissolved oxygen (DO)") ~ "mgL"
                      , (CHARACTERISTIC == "Dissolved oxygen saturation") ~ "Pct"
                      , (CHARACTERISTIC == "Salinity") ~ "ppt"
                      , (CHARACTERISTIC == "Specific conductance") ~ "uScm"
                      , (CHARACTERISTIC == "Temperature, water") ~ "DegC"
                      , (CHARACTERISTIC == "Turbidity") ~ "NTU"
                      , (CHARACTERISTIC == "pH") ~ "none")) %>%
      dplyr::rename(STATION = MLOC_ID
                    , STATION_NAME = MLOC_NAME
                    , CHARACTERISTIC_NAME = CHARACTERISTIC) %>%
      dplyr::select(-c(SE_START_DATE_TIME, ACT_ID, PRJ_UID, MLOC_UID, thermocline
                       , depth_max, depth_assess))

    # cleanup
    rm(df_Profile_summ_v2, df_Profile_summ, df_Profile_epi
       , df_Profile_mix, df_thermoclines, df_Profile_wide, SQUID_LakeProfile_table)
  } #END ~ if df_Profile exists

  # Combine data ####
  ## Merge ####
  # Merge logic depends on available data
  if (exists("df_Profile_summ_v3") & exists("df_LTD_v2")) {
    df_Prof_samps <- df_Profile_summ_v3 %>%
      dplyr::select(WATER_ID, STATION, DATE, CHR_UID) %>%
      dplyr::distinct() %>%
      dplyr::mutate(InOtherData = "Yes_Profile")

    df_Chem_v7 <- dplyr::left_join(df_Chem_v6, df_Prof_samps
                                   , by = c("WATER_ID" = "WATER_ID"
                                            , "STATION" = "STATION"
                                            , "DATE" = "DATE"
                                            , "CHR_UID" = "CHR_UID")) %>%
      dplyr::filter(is.na(InOtherData)) %>% #Remove lake samples overlapping profile data
      dplyr::select(-c(InOtherData))

    # cleanup
    rm(df_Prof_samps, df_Chem_v6)

    df_Chem_Combined <- rbind(df_Chem_v7, df_Profile_summ_v3) %>%
      #Need to add blank version of req column that's in LTD
      dplyr::mutate(ASSESSABILITY_QUALIFIER_CODE = NA,
                    ACT_START_DATE = NA,
                    ACT_END_DATE = NA) %>%
      rbind(df_LTD_v2) %>%
      unique()

    # cleanup
    rm(df_Chem_v7, df_LTD_v2, df_Profile_summ_v3)

  } else if (exists("df_Profile_summ_v3") & !exists("df_LTD_v2")) {
    df_Prof_samps <- df_Profile_summ_v3 %>%
      dplyr::select(WATER_ID, STATION, DATE, CHR_UID) %>%
      dplyr::distinct() %>%
      dplyr::mutate(InOtherData = "Yes_Profile")

    df_Chem_v7 <- dplyr::left_join(df_Chem_v6, df_Prof_samps
                                   , by = c("WATER_ID" = "WATER_ID"
                                            , "STATION" = "STATION"
                                            , "DATE" = "DATE"
                                            , "CHR_UID" = "CHR_UID")) %>%
      dplyr::filter(is.na(InOtherData)) %>% #Remove lake samples overlapping profile data
      dplyr::select(-c(InOtherData))

    # cleanup
    rm(df_Prof_samps, df_Chem_v6)

    df_Chem_Combined <- rbind(df_Chem_v7, df_Profile_summ_v3) %>%
      #Need to add blank version of req column that's in LTD
      dplyr::mutate(ASSESSABILITY_QUALIFIER_CODE = NA,
                    ACT_START_DATE = NA,
                    ACT_END_DATE = NA) %>%
      unique()

    # cleanup
    rm(df_Chem_v7, df_Profile_summ_v3)

  } else if (!exists("df_Profile_summ_v3") & exists("df_LTD_v2")) {
    df_Chem_Combined <- df_Chem_v6 %>%
      #Need to add blank version of req column that's in LTD
      dplyr::mutate(ASSESSABILITY_QUALIFIER_CODE = NA,
                    ACT_START_DATE = NA,
                    ACT_END_DATE = NA) %>%
      rbind(df_LTD_v2) %>%
      unique()

    # cleanup
    rm(df_Chem_v6, df_LTD_v2)

  } else {
    df_Chem_Combined <- df_Chem_v6 %>%
      #Need to add blank version of req column that's in LTD
      dplyr::mutate(ASSESSABILITY_QUALIFIER_CODE = NA,
                    ACT_START_DATE = NA,
                    ACT_END_DATE = NA)%>%
      unique()

    # cleanup
    rm(df_Chem_v6)

  } # end if/else

  ## Growing season ####
  df_GrowSeas <- df_DU_v4 %>%
    dplyr::select(WATER_ID, ECOREGION, ELEVATION) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Eco_3 = substr(ECOREGION, 1,2)
                  , DO_Site_Class = dplyr::case_when(((Eco_3 == 22 | Eco_3 == 23)
                                                      & ELEVATION >= 7500)
                                                        ~ "Mtns_Gtr7500"
                                                     , (Eco_3 == 20
                                                        | Eco_3 == 21
                                                        | Eco_3 == 22
                                                        | Eco_3 == 23)
                                                     & ELEVATION < 7500
                                                        ~ "Mtns_Less7500_Plat"
                                                     , (Eco_3 == 24
                                                        | Eco_3 == 25
                                                        | Eco_3 == 26
                                                        | Eco_3 == 79)
                                                        ~ "South_Des_Plains"
                                                     , TRUE ~ "Error")
                  , Date_Begin = dplyr::case_when(
                    (DO_Site_Class == "Mtns_Gtr7500") ~ "07-01"
                    , (DO_Site_Class == "Mtns_Less7500_Plat") ~ "06-15"
                    , (DO_Site_Class == "South_Des_Plains") ~ "05-15")
                  , Date_End = dplyr::case_when(
                    (DO_Site_Class == "Mtns_Gtr7500") ~ "10-15"
                    , (DO_Site_Class == "Mtns_Less7500_Plat") ~ "11-01"
                    , (DO_Site_Class == "South_Des_Plains") ~ "11-15")) %>%
    dplyr::select(WATER_ID, DO_Site_Class, Date_Begin, Date_End)

  df_Chem_Combined_v2 <- dplyr::left_join(df_Chem_Combined, df_GrowSeas
                                          , by = "WATER_ID") %>%
    dplyr::mutate(samp_md = format(DATE, "%m-%d") # Extract month-day from DATE
                  , begin_md = format(as.Date(Date_Begin, format = "%m-%d"), "%m-%d")
                  , end_md = format(as.Date(Date_End, format = "%m-%d"), "%m-%d")
                  , Flag_DO_GrowSeas = dplyr::case_when(((samp_md < begin_md
                                                          | samp_md > end_md)
                                                         & (CHR_UID == "986"
                                                            | CHR_UID == "985"))
                                                            ~ "FLAG")) %>%
    dplyr::filter(is.na(Flag_DO_GrowSeas)) %>% # excludes DO samps from outside growing season
    dplyr::select(-c(DO_Site_Class, Date_Begin, Date_End, samp_md, begin_md, end_md
                     , Flag_DO_GrowSeas))


  # Non-Assess Data ####
  # remove not assessable data
  df_non_assessable <- SQUID_RStudio_table %>%
    dplyr::filter(SWQB_QUALIFIER_CODE %in% c("R1", "R2", "R3", "Er", "ER", "ER3"))

  # cleanup
  rm(SQUID_RStudio_table)

  # Ensure consistent CHR_UID types
  df_Chem_Combined$CHR_UID <- as.character(df_Chem_Combined$CHR_UID)
  df_DU_v4$CHR_UID <- as.character(df_DU_v4$CHR_UID)
  df_Criteria_v3$CHR_UID <- as.character(df_Criteria_v3$CHR_UID)
  df_DU_v4$CHR_UID_Unique <- as.character(df_DU_v4$CHR_UID_Unique)

  # Export Data ####
  return(list(
    Chem_Combined = df_Chem_Combined,
    DU_Processed = df_DU_v4,
    Non_Assessable = df_non_assessable,
    Criteria_Formatted = df_Criteria_v3))

} # END ~ Function
