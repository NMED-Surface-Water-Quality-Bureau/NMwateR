# Select evidence consistently for both analytical and SQUID parameter groups.
.assessment_select_result <- function(rows, keys, category_field) {
  numeric_field <- function(field) {
    if (!field %in% names(rows)) return(rep(NA_real_, nrow(rows)))
    suppressWarnings(as.numeric(as.character(rows[[field]])))
  }
  category <- tolower(trimws(as.character(rows[[category_field]])))
  rationale <- if ("Rationale" %in% names(rows)) {
    tolower(trimws(as.character(rows$Rationale)))
  } else rep(NA_character_, nrow(rows))
  rows <- dplyr::ungroup(rows)
  rows$.category_priority <- dplyr::case_when(
    category %in% c("5", "5a", "5c", "4 or 5") ~ 1L,
    category == "3c" ~ 2L,
    category %in% c("2", "1 or 2") ~ 3L,
    category == "3b" ~ 4L,
    category == "3a" ~ 5L,
    TRUE ~ 6L)
  rows$.sufficiency_priority <- dplyr::case_when(
    grepl("low[[:space:]]+n", rationale) ~ 2L,
    rationale == "data sufficient" ~ 1L,
    rationale == "data insufficient" ~ 3L,
    TRUE ~ 4L)
  # Acute and chronic excursions can overlap. Use the larger known count
  # as a fallback, rather than adding them and double-counting samples.
  rows$.exceedance_count <- dplyr::coalesce(
    numeric_field("n_Exceed"),
    pmax(numeric_field("n_Exceed_Acute"), numeric_field("n_Exceed_Chronic"), na.rm = TRUE))
  rows$.sample_count <- numeric_field("n_Samples")
  # Preserve the former category preference only after the requested evidence
  # tie-breaks. Sorting all remaining fields resolves otherwise exact ties
  # without relying on input row order (including differing rationale text).
  rows$.category_detail <- match(category, c("5c", "5", "5a", "4 or 5", "3c",
                                             "2", "1 or 2", "3b", "3a"))
  original_fields <- setdiff(names(rows), c(".category_priority", ".sufficiency_priority",
                                           ".exceedance_count", ".sample_count", ".category_detail"))
  rows %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(sort(original_fields)))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::arrange(.category_priority, .sufficiency_priority,
                   dplyr::desc(.exceedance_count), dplyr::desc(.sample_count),
                   .category_detail, .by_group = TRUE) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(original_fields))
}

# SQUID aliases apply only to exports; analytical characteristics stay intact
.assessment_squid_parameter <- function(name, fraction, chr_uid) {
  base <- tolower(trimws(sub("^(Maximum|Minimum) - ", "", name,
                            ignore.case = TRUE)))
  fraction <- tolower(trimws(as.character(fraction)))
  dplyr::case_when(
    base %in% c("temperature, water", "temperature", "4t3") ~ "Temperature",
    base %in% c("dissolved oxygen (do)", "dissolved oxygen") ~ "Dissolved oxygen",
    base == "specific conductance" ~ "Specific Conductance",
    base == "ph" ~ "pH",
    base %in% c("escherichia coli", "e. coli") ~ "E. coli",
    base %in% c("plant nutrients", "nutrients") ~ "Nutrients",
    base %in% c("total phosphorus", "phosphorus, total", "phosphorus") ~
      "Phosphorus, Total",
    base == "aluminum, dissolved" |
      (base == "aluminum" & (fraction == "dissolved" | chr_uid %in% "549001")) ~
      "Aluminum, Dissolved",
    base %in% c("aluminum, total recoverable", "aluminum, total") |
      (base == "aluminum" & (fraction %in% c("total", "total recoverable") |
                              chr_uid %in% "549002")) ~ "Aluminum, Total Recoverable",
    TRUE ~ name
  )
}

.assessment_uploads <- function(rows, columns, cycle, assessment_date, tmdl_year) {
  combine_text <- function(x) {
    x <- unique(as.character(x[!is.na(x) & nzchar(trimws(x))]))
    if (length(x)) paste(x, collapse = "\n") else NA_character_
  }
  for (field in setdiff(c("DELISTING_REASON", "PARAM_COMMENT", "ASSESSMENT_RATIONALE"),
                       names(rows))) rows[[field]] <- NA_character_

  # Decide use attainment across every station/parameter before splitting files
  uses <- rows %>%
    dplyr::group_by(ASSESSMENT_UNIT_ID, USE_NAME) %>%
    dplyr::summarise(
      AU_NAME_REF_ONLY = dplyr::first(AU_NAME_REF_ONLY),
      USE_ATTAINMENT_CODE = dplyr::case_when(
        any(PARAM_STATE_IR_CAT %in% c("5", "5c", "4 or 5")) ~ "N",
        any(PARAM_STATE_IR_CAT %in% c("2", "1 or 2", "3c")) ~ "F",
        TRUE ~ "X"), .groups = "drop")

  # Apply the same category/evidence selection used for DU results to aliases.
  parameters <- rows %>%
    dplyr::select(-dplyr::any_of("USE_ATTAINMENT_CODE")) %>%
    dplyr::mutate(
      PARAM_NAME = .assessment_squid_parameter(PARAM_NAME, SAMPLE_FRACTION, CHR_UID),
      has_delisting = !is.na(DELISTING_REASON) & nzchar(trimws(DELISTING_REASON))) %>%
    .assessment_select_result(c("ASSESSMENT_UNIT_ID", "USE_NAME", "PARAM_NAME"),
                               "PARAM_STATE_IR_CAT") %>%
    dplyr::left_join(uses %>% dplyr::select(-AU_NAME_REF_ONLY),
                     by = c("ASSESSMENT_UNIT_ID", "USE_NAME")) %>%
    dplyr::mutate(
      is_cause = PARAM_STATE_IR_CAT %in% c("5", "5c", "4 or 5"),
      is_delisting = PARAM_STATE_IR_CAT %in% c("2", "1 or 2") & has_delisting,
      is_monitoring = PARAM_STATE_IR_CAT == "3c" |
        (PARAM_STATE_IR_CAT == "3b" & USE_ATTAINMENT_CODE == "X")) %>%
    dplyr::filter(is_cause | is_delisting | is_monitoring) %>%
    dplyr::mutate(
      PARAM_STATE_IR_CAT = dplyr::case_when(
        is_delisting ~ NA_character_,
        PARAM_STATE_IR_CAT %in% c("5", "4 or 5") ~ "5A",
        TRUE ~ toupper(PARAM_STATE_IR_CAT)),
      PARAM_STATUS_NAME = dplyr::if_else(is_monitoring, "Monitored", "Cause"),
      PARAM_ATTAINMENT_CODE = dplyr::case_when(
        is_cause ~ "not meeting criteria",
        PARAM_STATE_IR_CAT == "3B" ~ "not enough information",
        TRUE ~ "meeting criteria"),
      PARAM_POLLUTANT_INDICATOR = "Y",
      PARAM_TARGET_TMDL_DATE = dplyr::if_else(is_cause, as.character(tmdl_year), NA_character_),
      PARAM_PRIORITY_RANKING = dplyr::case_when(
        PARAM_STATE_IR_CAT == "5A" ~ "High",
        PARAM_STATE_IR_CAT == "5C" ~ "Medium",
        TRUE ~ NA_character_),
      DELISTING_REASON = dplyr::if_else(is_delisting, as.character(DELISTING_REASON),
                                       NA_character_))

  # Parameter records for an AU share its rationale, retaining reviewer text and
  # evidence from all contributing aliases. Use-only records remain blank
  rationales <- parameters %>%
    dplyr::group_by(ASSESSMENT_UNIT_ID) %>%
    dplyr::summarise(ASSESSMENT_RATIONALE = combine_text(ASSESSMENT_RATIONALE),
                     .groups = "drop")
  parameters <- parameters %>%
    dplyr::select(-ASSESSMENT_RATIONALE) %>%
    dplyr::left_join(rationales, by = "ASSESSMENT_UNIT_ID")

  baseline <- uses %>%
    dplyr::mutate(
      # FS is uploaded first; NS subsequently overwrites these F values with N.
      USE_ATTAINMENT_CODE = dplyr::if_else(USE_ATTAINMENT_CODE == "X", "X", "F"),
      PARAM_ATTAINMENT_CODE = dplyr::if_else(USE_ATTAINMENT_CODE == "X",
                                            "not enough information", "meeting criteria"))
  fs <- dplyr::bind_rows(
    baseline,
    parameters %>% dplyr::filter(is_delisting) %>%
      dplyr::mutate(USE_ATTAINMENT_CODE = "F"))
  ns <- parameters %>% dplyr::filter(is_cause)
  monitoring <- parameters %>% dplyr::filter(is_monitoring)

  format_upload <- function(df) {
    df <- df %>% dplyr::mutate(
      CYCLE = as.character(cycle), CYCLE_LAST_ASSESSED = as.character(cycle),
      USE_ASMT_DATE = format(assessment_date, "%m/%d/%Y"),
      STATE_IR_CAT_CODE = NA_character_)
    for (field in setdiff(columns, names(df))) df[[field]] <- NA_character_
    df %>% dplyr::select(dplyr::all_of(columns)) %>%
      dplyr::arrange(ASSESSMENT_UNIT_ID, USE_NAME, PARAM_NAME)
  }
  reports <- lapply(list(FS = fs, NS = ns, monitoring = monitoring), format_upload)
  # Preserve upload order in the combined review report too: F/X initialization,
  # then N causes, then monitoring rows with the final use attainment
  reports$all <- dplyr::bind_rows(reports)
  reports
}
