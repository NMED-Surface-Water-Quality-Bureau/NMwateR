Toxics_ALU_HDM <- function(Chem_table
                       , DU_table
                       , Criteria_table){

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

  if (missing(Criteria_table)) {
    stop(paste0("Error: 'Criteria_table' is required but was not provided. ",
                "This is an output from the Data_Prep function."
    ))}

  # Format data ####
  ALUs <- c("ColdWAL", "CoolWAL", "HQColdWAL", "LAL", "MCWAL", "MWWAL", "WWAL")

  df_DU_v2 <- DU_table %>%
    dplyr::select(WATER_ID, DU) %>%
    dplyr::distinct() %>%
    dplyr::filter(DU %in% ALUs)

  df_Chem_v2 <- dplyr::left_join(Chem_table, df_DU_v2, by = "WATER_ID")

  # Toxics ALU ####
  RFunctionName <- "Toxics_ALU_HDM"
  # Filter only relevant criteria
  df_Crit_v2 <- Criteria_table %>%
    dplyr::filter(DU %in% ALUs
                  & TOXIC == "Y"
                  & HD_METAL == "Y"
                  & (Criteria_Type == "ACUTE" | Criteria_Type == "CHRONIC"))

  CHR_UID_ToxicALU <- unique(df_Crit_v2$CHR_UID)

  ## Trim chem data ####
  df_Chem_v3 <- df_Chem_v2 %>%
    dplyr::filter(CHR_UID %in% CHR_UID_ToxicALU # Hardness-dependent metals
                  | CHR_UID == "4528" # Total Hardness
    )

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

  ## AU Loop ####
  Unique_AUIDs <- unique(df_Chem_v4$WATER_ID) %>% stats::na.omit()
  result_list <- list()
  result_indiv_list <- list()
  counter <- 0

  for(i in Unique_AUIDs){
    print(i) # print name of current WATER_ID

    # subset chem data by WATER_ID
    df_subset <- df_Chem_v4 %>%
      dplyr::filter(WATER_ID == i)

    #If no relevant samples, skip WATER_ID
    if(nrow(df_subset)==0){
      next
    }

    # predictor data
    df_predictors <- df_subset %>%
      dplyr::filter(CHR_UID == "4528") %>%
      dplyr::group_by(WATER_ID, STATION, DATE) %>%
      dplyr::reframe(Hardness_mgL = mean(MEASUREMENT_num)) %>%
      dplyr::mutate(Hardness_mgL = dplyr::case_when((Hardness_mgL > 400) ~ as.numeric(400) # from CALM
                                                    # Aluminum max hardness addressed below
                                                    , TRUE ~ Hardness_mgL)) %>%
      dplyr::ungroup()

    #If no relevant hardness samples, skip WATER_ID
    if(nrow(df_predictors)==0){
      next
    }

    # obtain unique constituents from WQ dataset for the WATER_ID
    my_constituents <- unique(df_subset$CHR_UID)
    my_constituents <- my_constituents[my_constituents != 4528] # Don't loop hardness

    #Cycle through each parameter to apply logic
    for(j in my_constituents){
      print(j)
      counter <- counter + 1

      # Specify CHR_UID
      filter_by <- j

      # subset chem data by CHR_UID
      df_subset_v2 <- df_subset %>%
        dplyr::filter(CHR_UID %in% filter_by)

      # subset chem data by last three years
      maxYear <- max(lubridate::year(df_subset_v2$DATE))
      YearMinus2 <- maxYear-2

      df_subset_v3 <- df_subset_v2 %>%
        dplyr::mutate(Year = lubridate::year(DATE)) %>%
        dplyr::filter(Year >= YearMinus2)

      # join hardness data
      df_subset_v4 <- dplyr::left_join(df_subset_v3, df_predictors
                                       , by = c("WATER_ID" = "WATER_ID"
                                                , "STATION" = "STATION"
                                                , "DATE" = "DATE")) %>%
        dplyr::filter(!is.na(Hardness_mgL)) # removes samples without hardness per CALM

      n_Samples <- nrow(df_subset_v4)

      #If no relevant samples, skip CHR_UID
      if(n_Samples == 0) {
        next
      }

      # calculate criteria
      if(filter_by == "549002"){
        # Aluminum
        df_subset_v5 <- df_subset_v4 %>%
          dplyr::mutate(Hardness_mgL = dplyr::case_when((Hardness_mgL > 220) ~ as.numeric(220) # from CALM
                                                        , TRUE ~ Hardness_mgL)) %>%
          dplyr::mutate(Crit_Acute = exp(1.3695*(log(Hardness_mgL)) + 1.8308)
                        , Crit_Chronic = exp(1.3695*(log(Hardness_mgL)) + 0.9161))

      } else if(filter_by == "725"){
        # Cadmium
        df_subset_v5 <- df_subset_v4 %>%
          dplyr::mutate(Crit_Acute = exp(0.9789*(log(Hardness_mgL))-3.866)
                        *(1.136672-log(Hardness_mgL)*0.041838)
                        , Crit_Chronic = exp(0.7977*(log(Hardness_mgL))-3.909)
                        *(1.101672-log(Hardness_mgL)*0.041838))

      } else if (filter_by == "810"){
        # Chromium(III)
        df_subset_v5 <- df_subset_v4 %>%
          dplyr::mutate(Crit_Acute = exp(0.819*(log(Hardness_mgL))+3.7256)*0.316
                        , Crit_Chronic = exp(0.819*(log(Hardness_mgL))+0.6848)*0.860)

      } else if (filter_by == "832"){
        # Copper
        #Not site-specific
        df_subset_v5 <- df_subset_v4 %>%
          dplyr::mutate(Crit_Acute = exp(0.9422*(log(Hardness_mgL))-1.7)*0.960
                        , Crit_Chronic = exp(0.8545*(log(Hardness_mgL))-1.702)*0.960)

      } else if (filter_by == "1215") {
        # Lead
        df_subset_v5 <- df_subset_v4 %>%
          dplyr::mutate(Crit_Acute = exp(1.273*(log(Hardness_mgL))-1.460)
                        *(1.46203-log(Hardness_mgL)*0.145712)
                        , Crit_Chronic = exp(1.273*(log(Hardness_mgL))-4.705)
                        *(1.46203-log(Hardness_mgL)*0.145712))

      } else if (filter_by == "1249"){
        # Manganese
        df_subset_v5 <- df_subset_v4 %>%
          dplyr::mutate(Crit_Acute = exp(0.3331*(log(Hardness_mgL)) + 6.4676)
                        , Crit_Chronic = exp(0.3331*(log(Hardness_mgL)) + 5.8743))

      } else if (filter_by == "1395"){
        # Nickel
        df_subset_v5 <- df_subset_v4 %>%
          dplyr::mutate(Crit_Acute = exp(0.8460*(log(Hardness_mgL))+2.255)*0.998
                        , Crit_Chronic = exp(0.8460*(log(Hardness_mgL))+0.0584)*0.997)

      } else if (filter_by == "1793") {
        # Silver (acute only)
        df_subset_v5 <- df_subset_v4 %>%
          dplyr::mutate(Crit_Acute = exp(1.72*(log(Hardness_mgL))-6.59)*0.85
                        , Crit_Chronic = NA_real_)

      } else if (filter_by == "2005") {
        # Zinc
        df_subset_v5 <- df_subset_v4 %>%
          dplyr::mutate(Crit_Acute = exp(0.9094*(log(Hardness_mgL))+0.9095)*0.978
                        , Crit_Chronic = exp(0.9094*(log(Hardness_mgL))+0.0635)*0.986)

      } # END ~ if/else criteria calc

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
    } # END ~ Parameter for loop
  } # END ~ AU for loop

  # combine results from for loop
  df_loop_results <- as.data.frame(do.call("rbind", result_list))
  df_loop_results_indiv <- as.data.frame(do.call("rbind", result_indiv_list))

  # Export data ####
  return(list(
    Toxics_ALU_HDM = df_loop_results,
    Toxics_ALU_HDM_Indiv_Res = df_loop_results_indiv))

} # END ~ Function
