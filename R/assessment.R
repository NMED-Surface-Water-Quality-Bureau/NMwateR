#' Assessment of water quality data using NMED SWQS and CALM logic
#'
#' This function uses the exports from the various water quality analyses and
#' assesses the results using logic from the New Mexico Surface Water Quality
#' Standards (SWQS) and the \href{https://www.env.nm.gov/surface-water-quality/calm/}{NMED Consolidated Assessment and Listing Methodology (CALM)} document.
#'
#' @param Conventionals_ALU_table WQ analysis export from Conventionals_ALU() function.
#' @param Bacteria_PCR_SCR_table WQ analysis export from Bacteria_PCR_SCR() function.
#' @param Conventionals_LW_table WQ analysis export from Conventionals_LW() function.
#' @param LTD_ALU_table WQ analysis export from LTD_ALU() function.
#' @param Nutrients_Lakes_table WQ analysis export from Nutrients_Lakes() function.
#' @param Nutrients_Streams_table WQ analysis export from Nutrients_Streams() function.
#' @param pH_PCR_table WQ analysis export from pH_PCR() function.
#' @param Salinity_IRR_table WQ analysis export from Salinity_IRR() function.
#' @param Toxics_ALU_nonHDM_table WQ analysis export from Toxics_ALU_nonHDM() function.
#' @param Toxics_ALU_HDM_table WQ analysis export from Toxics_ALU_HDM() function.
#' @param Toxics_DWS_table WQ analysis export from Toxics_DWS() function.
#' @param Toxics_HH_table WQ analysis export from Toxics_HH() function.
#' @param Toxics_IRR_table WQ analysis export from Toxics_IRR() function.
#' @param Toxics_LW_table WQ analysis export from Toxics_LW() function.
#' @param Toxics_WH_table WQ analysis export from Toxics_WH() function.
#' @param Turbidity_ALU_table WQ analysis export from Turbidity_ALU() function.
#' @param Verified_results_table Optional complete, verified copy of
#'   `Assess_DU_Res` from a previous run. Supply it without analysis tables for
#'   the final post-verification run; `Criteria_table` may accompany it.
#'   Edit `Individual_Category` to correct a decision;
#'   `Determination`, `Use_Category`, and `Overall_Category` are recalculated.
#'   Add `AV_Notes` for AU narratives or verification notes. Nonblank
#'   `ASSESSMENT_RATIONALE` overrides the AU's generated rationale;
#'   `PARAM_COMMENT` overrides its parameter statement.
#'   Nonblank `SQUID_PARAM_OVERRIDE` replaces the SQUID parameter name in
#'   exports. Keep `Original_R_Category` to enable automatic AV change text.
#'   For `delist`, enter `DELISTING_REASON`; `Delisting_Category` optionally
#'   overrides the suggested category and is required when no default exists.
#'   An AU summary alone is insufficient because uploads require use/parameter
#'   records. Each WATER_ID/DU/CHR_UID/CHARACTERISTIC_NAME combination must
#'   occur once; WATER_NAME and SAMPLE_FRACTION are supporting attributes.
#' @param Cycle Even-numbered assessment cycle for SQUID exports; defaults to 2028.
#' @param Assessment_date Assessment run date, as a Date or ISO date string.
#' @param Target_TMDL_year Target TMDL year for 5A/5C causes; defaults to
#'   `Cycle + 2` (2030 for cycle 2028).
#' @param Criteria_table Optional existing processed criteria table (the same
#'   table supplied to the analysis functions), containing `CHR_UID`, `DU`,
#'   and `FRACTION`. Joins the assessment fraction by characteristic ID and
#'   designated use. Repeated acute/chronic criteria with the same fraction
#'   are collapsed; conflicting fractions for an assessed key cause an error.
#'
#' @returns A list of seven dataframes. Individual results are IR categories assigned
#' to each AU/DU/parameter combination. DU results are IR categories assigned to
#' each AU/DU combination. AU results are IR categories assigned to each AU. Lastly,
#' the combined upload report and separate FS, NS, and 3C/3B reports are
#' provided for NMED to upload into SQUID and transfer to ATTAINS.
#' Ordinary FS upload records contain one row per AU/use, without a
#' parameter name. Parameter-specific causes, explicit delistings, and 3C
#' monitoring records are consolidated by SQUID parameter name. Their rationale
#' combines the AU's assessment evidence; missing statistics are not treated as
#' zero. FS provides initial F/X use records, including uses subsequently changed
#' to N by NS cause records. X means nothing is assessable; there is no separate
#' NA report.
#' 3A/3B/3C parameter records are in Assess_Upload_Report_3C3B (the existing
#' output name is retained), with 3A/3B suppressed
#' for uses having any assessable parameter. All uploads retain
#' the 46 template columns. STATE_IR_CAT_CODE is left blank for manual entry.
#' @details For assessment verification, save and edit `Assess_DU_Res`, retaining
#'   all rows intended for the final upload and the supporting statistics. Run
#'   `assessment(Verified_results_table = verified_results)` to rebuild the seven
#'   outputs. Supported verified parameter categories are `2`, `1 or 2`, `3a`,
#'   `3b`, `3c`, `5` (also `5A`), `5c`, `4A`, `4B`, `4C`, `delist`, and
#'   `4 or 5`; category letters are
#'   case-insensitive. Missing categories remain Not Assessed.
#'   Parameter selection uses impaired (5A/5C/4 or 5) > 3C > Fully Supporting
#'   (2/1 or 2) > 3B > 3A. Ties prefer Data Sufficient over Low N, then the
#'   greatest exceedance count, then the greatest sample count. Where a total
#'   exceedance count is missing, the larger known acute/chronic count is used
#'   without adding potentially overlapping excursions. Remaining ties use
#'   category detail and a fixed ordering of row values, independent of input
#'   order. Water names are filled from the AU's nonblank names after selection.
#'   Verification notes are appended once to the AU rationale, including notes
#'   supplied on fully supporting rows. Correct
#'   statistics as needed: they are retained as evidence, not used to reverse
#'   verified category decisions.
#'   Supply a nonblank DELISTING_REASON on a verified fully supporting parameter
#'   to request a parameter-specific delisting record; eligibility alone does
#'   not establish a delisting. Supporting delistings initialize F in FS; NS causes
#'   subsequently set impaired uses to N. Characteristics without an explicit SQUID alias retain their
#'   names, so additional substance names should be checked against SQUID.
#'   Alternatively, use `Individual_Category = "delist"` with a nonblank
#'   `DELISTING_REASON`. `assessment_delisting_reasons()` returns the eight
#'   reason choices and their suggested final categories. The three WQS-attained
#'   reasons suggest 2; TMDL approval suggests 4A; other pollution controls
#'   suggest 4B; not caused by a pollutant suggests 4C. Original-basis and
#'   WQS-no-longer-applicable reasons have no default: the reviewer must choose
#'   `Delisting_Category` from 2, 3A, 3B, 3C, 4A, 4B, or 4C (also 1 or 2).
#'   This column overrides any suggestion. Custom reasons are allowed with an
#'   explicit category. `Suggested_Delisting_Category` is recomputed on every
#'   run; it is never copied into the override column, so changing a reason
#'   updates the default without overwriting a reviewer decision.
#'   Delisting records export in FS, including final 3A/3B/3C categories;
#'   final 4A/4B/4C records also export in NS to set use attainment to N.
#'   Use/AU summaries retain the existing broad 2/3/5 groups, including group 5
#'   for 4A/4B/4C. Detailed categories are retained on parameter records.
#'
#'   Initial analytical runs capture `Original_R_Category` and `Original_Rationale`.
#'   `AV_Change` is regenerated from the original and current categories, appended
#'   to `Rationale`, parameter comments and AU rationale, including alongside
#'   custom text overrides. The note reads "changed during AV from X to Y";
#'   reruns replace the generated annotation instead of accumulating copies.
#'   Original counts and Exceed values remain evidence and do not reverse AV
#'   decisions. An older verified table without `Original_R_Category` leaves
#'   that value missing; recover it from the original assessment for automatic
#'   change notes. All verified rows replace analytical inputs, rather than
#'   merging with an earlier assessment.
#'
#'   CYCLE, CYCLE_LAST_ASSESSED and PARAM_TARGET_TMDL_DATE are integer years.
#'   USE_ASMT_DATE is an integer Excel date serial (origin 1899-12-30), suitable
#'   for numeric spreadsheet cells; apply a date display format when exporting
#'   if desired. These types are consistent across all upload reports, including
#'   empty reports. Fraction assignment is separate from SQUID name formatting:
#'   `ASSESSMENT_FRACTION` is retained when supplied (including AV corrections),
#'   otherwise joined from `Criteria_table` by `CHR_UID` and `DU`, then filled
#'   from `SAMPLE_FRACTION` where criteria do not match. The original sample
#'   fraction is never overwritten. To re-resolve an AV row against criteria,
#'   clear its `ASSESSMENT_FRACTION` and supply `Criteria_table` again.
#'   No new fraction lookup table is needed. Neither names, hardcoded IDs nor
#'   hardcoded use rules assign fractions when data are missing. For example,
#'   selenium's resolved fraction produces Selenium, Dissolved or Selenium,
#'   Total Recoverable in SQUID exports. Already qualified names remain intact.
#'   `SQUID_PARAM_OVERRIDE` takes precedence over export aliases.
#'   Additional export aliases follow parameter names observed in the client's
#'   2026-2028 EPA-approved integrated-list workbook: dissolved metals with
#'   supporting assessment fractions, Mercury, Total, Cyanide, Total
#'   Recoverable, Total Dissolved Solids (TDS), and Polychlorinated Biphenyls (PCBs).
#'   The historical workbook is an AU summary, not an exhaustive SQUID catalog;
#'   unrecognized parameters retain their analytical names. Historical listings
#'   do not supply current categories, delisting decisions, adjusted gross-alpha
#'   status, or fish-consumption advisories. Numeric upload fields and delisting
#'   defaults follow the client's explicit requirements rather than that workbook,
#'   which contains neither the upload fields nor a delisting-reason column.
#'
#'   ROD text uses short parameter statements with SQUID categories, counts,
#'   criterion type, fraction, available LTD years/indicators, and nutrient
#'   enrichment indicators. ASSESSMENT_RATIONALE is identical on every upload
#'   row for an AU, ordered as impairments, 3C, then delistings/full-support
#'   exceptions. PARAM_COMMENT contains only the corresponding parameter
#'   statement. Ordinary full support is omitted from the rationale; set
#'   ROD_Include to TRUE on a verified row to describe new or outside data.
#'   No previous-cycle listing history is inferred. Explicit verified 4A
#'   categories are supported; ambiguous internal `4 or 5` still exports as 5A.
#'
#'   Available analysis columns are retained in the intermediate results.
#'   ROD_Context retains supplemental evidence from other analysis sources
#'   when one characteristic has both grab and LTD results. This editable
#'   text must also be reviewed if that supporting evidence changes during
#'   verification. Missing years, duration, or nutrient constituents are never
#'   inferred from the assessment category alone.
#' @examples
#' df_Chem_combined <- NMwateR::example_chemistry_processed
#' df_Criteria <- NMwateR::example_criteria_processed
#' df_DU_processed <- NMwateR::example_DU_processed
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
#'                               , Toxics_ALU_nonHDM_table = df_Tox_ALU_nHDM
#'                               , Toxics_ALU_HDM_table = df_Toxics_ALU_HDM
#'                               , Toxics_DWS_table = df_Toxics_DWS
#'                               , Toxics_HH_table = df_Toxics_HH
#'                               , Toxics_IRR_table = df_Toxics_IRR
#'                               , Toxics_LW_table = df_Toxics_LW
#'                               , Toxics_WH_table = df_Toxics_WH
#'                               , Turbidity_ALU_table = df_Turbidity_ALU
#'                               , Criteria_table = df_Criteria)
#' df_Assess_Indiv_Res <- assessment_list$Assess_Indiv_Res
#' df_Assess_DU_Res <- assessment_list$Assess_DU_Res
#' df_Assess_AU_Res <- assessment_list$Assess_AU_Res
#' df_Assess_Upload_Report <- assessment_list$Assess_Upload_Report
#' df_Assess_Upload_Report_FS <- assessment_list$Assess_Upload_Report_FS
#' df_Assess_Upload_Report_NS <- assessment_list$Assess_Upload_Report_NS
#' df_Assess_Upload_Report_3C3B <- assessment_list$Assess_Upload_Report_3C3B
#'
#' # After assessment verification, rebuild uploads from the edited use/parameter results.
#' verified_results <- assessment_list$Assess_DU_Res
#' verified_results$AV_Notes <- NA_character_ # Replace with reviewer notes as needed.
#' # Optional export-name override; analytical characteristics are retained.
#' verified_results$SQUID_PARAM_OVERRIDE <- NA_character_
#' # For a delisting, set Individual_Category to "delist" and enter a reason.
#' # Leave Delisting_Category blank to use its default, or enter the AV decision.
#' assessment_delisting_reasons()
#' final_assessment_list <- assessment(Verified_results_table = verified_results)
#'
#' @export
#'
assessment <- function(Conventionals_ALU_table = NULL
                       , Bacteria_PCR_SCR_table = NULL
                       , Conventionals_LW_table = NULL
                       , LTD_ALU_table = NULL
                       , Nutrients_Lakes_table = NULL
                       , Nutrients_Streams_table = NULL
                       , pH_PCR_table = NULL
                       , Salinity_IRR_table = NULL
                       , Toxics_ALU_nonHDM_table = NULL
                       , Toxics_ALU_HDM_table = NULL
                       , Toxics_DWS_table = NULL
                       , Toxics_HH_table = NULL
                       , Toxics_IRR_table = NULL
                       , Toxics_LW_table = NULL
                       , Toxics_WH_table = NULL
                       , Turbidity_ALU_table = NULL
                       , Verified_results_table = NULL
                       , Cycle = 2028L
                       , Assessment_date = Sys.Date()
                       , Target_TMDL_year = Cycle + 2L
                       , Criteria_table = NULL){

  if (!is.numeric(Cycle) || length(Cycle) != 1L || is.na(Cycle) ||
      !is.finite(Cycle) || Cycle < 2000 || Cycle > 9998 ||
      Cycle != as.integer(Cycle) || Cycle %% 2 != 0) {
    stop("Cycle must be a single even-numbered assessment year.", call. = FALSE)
  }
  if (!is.numeric(Target_TMDL_year) || length(Target_TMDL_year) != 1L ||
      is.na(Target_TMDL_year) || !is.finite(Target_TMDL_year) ||
      Target_TMDL_year < 1900 || Target_TMDL_year > 9999 ||
      Target_TMDL_year != as.integer(Target_TMDL_year)) {
    stop("Target_TMDL_year must be a single year.", call. = FALSE)
  }
  Assessment_date <- tryCatch(as.Date(Assessment_date), error = function(e) NA)
  if (length(Assessment_date) != 1L || is.na(Assessment_date)) {
    stop("Assessment_date must be a single valid date.", call. = FALSE)
  }

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
    Toxics_ALU_nonHDM_table = Toxics_ALU_nonHDM_table,
    Toxics_ALU_HDM_table = Toxics_ALU_HDM_table,
    Toxics_DWS_table = Toxics_DWS_table,
    Toxics_HH_table = Toxics_HH_table,
    Toxics_IRR_table = Toxics_IRR_table,
    Toxics_LW_table = Toxics_LW_table,
    Toxics_WH_table = Toxics_WH_table,
    Turbidity_ALU_table = Turbidity_ALU_table)

  if (!is.null(Verified_results_table)) {
    if (any(!vapply(args_list, is.null, logical(1)))) {
      stop("Supply Verified_results_table alone, without analysis tables.", call. = FALSE)
    }
    if (!is.data.frame(Verified_results_table)) {
      stop("Verified_results_table must be a data frame.", call. = FALSE)
    }
    required <- c("WATER_ID", "WATER_NAME", "DU", "CHR_UID",
                  "CHARACTERISTIC_NAME", "Individual_Category")
    missing_fields <- setdiff(required, names(Verified_results_table))
    if (length(missing_fields) > 0) {
      stop("Verified_results_table is missing: ",
           paste(missing_fields, collapse = ", "),
           ". Use a verified copy of Assess_DU_Res, not the AU summary.", call. = FALSE)
    }
    df_indiv_results_v2 <- tibble::as_tibble(Verified_results_table) %>%
      dplyr::ungroup() %>%
      dplyr::select(-dplyr::any_of(c("Use_Category", "Overall_Category"))) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(setdiff(required, "CHR_UID")), as.character))
    keys <- c("WATER_ID", "DU", "CHR_UID", "CHARACTERISTIC_NAME")
    for (field in setdiff(keys, "CHR_UID")) {
      if (any(is.na(df_indiv_results_v2[[field]]) |
              !nzchar(trimws(df_indiv_results_v2[[field]])))) {
        stop("Verified_results_table requires nonblank ", field, ".", call. = FALSE)
      }
    }
    if (anyDuplicated(df_indiv_results_v2[keys])) {
      stop("Verified_results_table contains duplicate AU/use/parameter records.", call. = FALSE)
    }
    category <- tolower(trimws(df_indiv_results_v2$Individual_Category))
    category[!is.na(category) & category == "5a"] <- "5"
    df_indiv_results_v2$Individual_Category <- category
    if (any(!is.na(category) & !category %in%
            c("2", "1 or 2", "3a", "3b", "3c", "5", "5c", "4 or 5", "4a", "4b", "4c", "delist"))) {
      stop("Verified_results_table contains unsupported Individual_Category values.", call. = FALSE)
    }
    # Preserve verified decisions even when the original Exceed/Rationale differs.
    final_category <- .assessment_effective_category(df_indiv_results_v2)
    df_indiv_results_v2 <- df_indiv_results_v2 %>%
      dplyr::mutate(Determination = dplyr::case_when(
        final_category %in% c("2", "1 or 2") ~ "Fully Supporting",
        final_category %in% c("5", "5c", "4 or 5", "4a", "4b", "4c") ~ "Not Supporting",
        TRUE ~ "Not Assessed"))
    evidence_cols <- c("SAMPLE_FRACTION", "Method", "Rationale", "Exceed",
                       "n_Samples", "n_Samples_assessable", "n_Exceed",
                       "n_Exceed_Acute", "n_Exceed_Chronic", "pct_Exceed")
    for (field in setdiff(evidence_cols, names(df_indiv_results_v2))) {
      df_indiv_results_v2[[field]] <- NA
    }
  } else {
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
  data_list <- stats::setNames(lapply(names(data_list), function(nm) {
    df <- args_list[[nm]]
    df$Analysis_Source <- sub("_table$", "", nm)
    df
  }), present)

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
      WATER_ID = character(), WATER_NAME = character(), DU = character(), CHR_UID = numeric(),
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

  ## Format ####
  rownames(df_AnalysisResults) <- NULL

  # Define expected columns
  expected_cols <- c("WATER_ID", "WATER_NAME", "DU", "CHR_UID", "CHARACTERISTIC_NAME"
                     , "SAMPLE_FRACTION", "n_Samples", "n_Samples_assessable"
                     , "Method", "Rationale", "Exceed", "n_Exceed"
                     , "n_Exceed_Acute", "n_Exceed_Chronic", "pct_Exceed"
                     , "DELISTING_REASON")

  # Add missing columns with NA
  missing_cols <- setdiff(expected_cols, names(df_AnalysisResults))
  for (col in missing_cols) {
    df_AnalysisResults[[col]] <- NA
  }

  # Select and reorder columns
  df_AnalysisResults_v2 <- df_AnalysisResults %>%
    dplyr::select(dplyr::all_of(expected_cols), dplyr::everything()) %>%
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
    dplyr::mutate(CHARACTERISTIC_NAME = "Plant Nutrients"
                  , Method = "Lake Nutrients Assessment"
                  , Determination = dplyr::case_when((Category == "1 or 2") ~ "Fully Supporting"
                                                     , (Category == "3a"
                                                        | Category == "3b"
                                                        | Category == "3c") ~ "Not Assessed"
                                                     , (Category == "4 or 5") ~ "Not Supporting")) %>%
    dplyr::rename(Individual_Category = Category)

  df_Nutr_Streams_v2 <- df_Nutr_Streams %>%
    dplyr::mutate(CHARACTERISTIC_NAME = "Plant Nutrients"
                  , Assessment_Qualifier = Category
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
  missing_cols <- setdiff(names(df_indiv_results), names(df_Nutr_Streams_v2))
  df_Nutr_Streams_v2[missing_cols] <- NA

  # Align heterogeneous analysis columns without losing nutrient-only fields.
  df_indiv_results_v2 <- lapply(list(df_indiv_results, df_Nutr_Lakes_v2, df_Nutr_Streams_v2),
                               function(df) dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))) %>%
    dplyr::bind_rows() %>% readr::type_convert() %>%
    dplyr::mutate(Individual_Category = as.character(Individual_Category))

  # cleanup
  rm(df_Nutr_Lakes, df_Nutr_Lakes_v2, df_Nutr_Streams, df_Nutr_Streams_v2
     , missing_cols, df_indiv_results)
  } # END ~ initial analyses or verified results

  df_indiv_results_v2 <- .assessment_join_fraction(df_indiv_results_v2, Criteria_table)
  df_indiv_results_v2 <- .assessment_av_changes(df_indiv_results_v2,
                                               verified = !is.null(Verified_results_table))

  # DU Determinations ####
  # Water names describe the AU, not individual grab/profile records. Choose a
  # stable nonblank name from all evidence before selecting parameter results.
  au_names <- df_indiv_results_v2 %>%
    dplyr::ungroup() %>%
    dplyr::group_by(WATER_ID) %>%
    dplyr::summarise(WATER_NAME = {
      candidates <- trimws(as.character(WATER_NAME))
      candidates <- sort(unique(candidates[!is.na(candidates) & nzchar(candidates)]))
      dplyr::first(candidates, default = NA_character_)
    }, .groups = "drop")

  # Impaired > 3C > Fully Supporting > 3B > 3A. Within a category group,
  # prefer sufficient data, then more exceedances, then more samples.
  df_DU_results <- .assessment_du_context(df_indiv_results_v2) %>%
    .assessment_select_result(c("WATER_ID", "DU", "CHR_UID", "CHARACTERISTIC_NAME"),
                               "Individual_Category") %>%
    dplyr::select(-WATER_NAME) %>%
    dplyr::left_join(au_names, by = "WATER_ID") %>%
    dplyr::select(dplyr::all_of(names(df_indiv_results_v2)), dplyr::everything()) %>%
    dplyr::mutate(.final_category = .assessment_effective_category(.)) %>%
    dplyr::group_by(WATER_ID, DU) %>%
    # Nutrient category "1 or 2" is fully supporting, like "2" (issue #6).
    # Any impaired parameter still takes precedence for the use.
    dplyr::mutate(Use_Category = dplyr::case_when(any(.final_category %in% c("5", "5c", "4 or 5", "4a", "4b", "4c")) ~
                                                    "5"
                                                  , any(.final_category == "3c", na.rm = TRUE) ~ "3"
                                                  , any(.final_category %in% c("2", "1 or 2")) ~
                                                    "2"
                                                  , TRUE ~ "3")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.final_category) %>%
    dplyr::arrange(WATER_ID, DU)

  if (!"ROD_Context" %in% names(df_indiv_results_v2)) {
    df_indiv_results_v2 <- dplyr::left_join(df_indiv_results_v2,
      df_DU_results %>% dplyr::select(WATER_ID, DU, CHR_UID, CHARACTERISTIC_NAME, ROD_Context),
      by = c("WATER_ID", "DU", "CHR_UID", "CHARACTERISTIC_NAME"))
  }

  # AU Determinations ####
  # Calculate one overall conclusion per AU, across all designated uses.
  df_AU_results <- df_DU_results %>%
    dplyr::ungroup() %>%
    dplyr::group_by(WATER_ID) %>%
    dplyr::summarise(WATER_NAME = dplyr::first(stats::na.omit(WATER_NAME)
                                               , default = NA_character_)
                     , Overall_Category = dplyr::case_when(any(Use_Category == "5", na.rm = TRUE) ~
                                                             "5"
                                                           , any(Use_Category == "2", na.rm = TRUE) ~
                                                             "2"
                                                           , TRUE ~ "3")
                     , .groups = "drop") %>%
    dplyr::arrange(WATER_ID)

  # Upload Report ####
  # Preserve use/parameter-level upload records, but join the AU conclusion
  # once per WATER_ID instead of repeating or multiplying AU-level records.
  df_upload <- df_DU_results %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::any_of("Overall_Category")) %>%
    dplyr::left_join(df_AU_results %>%
                       dplyr::select(WATER_ID, Overall_Category)
                     , by = "WATER_ID") %>%
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

  uploads <- .assessment_uploads(df_upload, req_cols, Cycle, Assessment_date,
                                 Target_TMDL_year)

  # Export data ####
  return(list(
    Assess_Indiv_Res = df_indiv_results_v2,
    Assess_DU_Res = df_DU_results,
    Assess_AU_Res = df_AU_results,
    Assess_Upload_Report = uploads$all,
    Assess_Upload_Report_FS = uploads$FS,
    Assess_Upload_Report_NS = uploads$NS,
    Assess_Upload_Report_3C3B = uploads$monitoring))

} # END ~ Function
