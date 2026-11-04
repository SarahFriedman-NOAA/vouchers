# ==============================================================================
# MUSEUM DATA WRANGLING → RACE_DATA.VOUCHERS
# ==============================================================================

# This script standardizes museum-provided voucher data into a format consistent with the Oracle voucher table.


# ==============================================================================
# 1. STATIC LOOKUPS
# ==============================================================================

vessels <- tibble::tribble(
  ~vessel, ~vessel_id,
  "Ocean Explorer", 148,
  "Sea Storm", 143,
  "Alaska Provider", 176,
  "Vesteraalen", 94,
  "Miller Freeman", 21,
  "Northwest Explorer", 134,
  "Alaska Knight", 162,
  "Morning Star", 57
)


# ==============================================================================
# 3. INGEST + CLEAN SOURCE DATA
# ==============================================================================

# # ---- UW DATA ------------------------------------------------------------------
#
# uw_info <- readr::read_csv("data/updates/voucher_fishes_uw_2023.csv") %>%
#   janitor::clean_names() %>%
#   dplyr::relocate(catalog_num, .after = dplyr::last_col()) %>%
#   dplyr::mutate(
#     year = as.numeric(stringr::str_extract(cruise, "^[0-9]{4}")),
#     region = dplyr::case_when(
#       vessel %in% c(162, 94, 134) ~ "BS",
#       vessel %in% c(148, 176) & year %% 2 == 0 ~ "AI",
#       vessel %in% c(148, 176) & year %% 2 != 0 ~ "GOA",
#       TRUE ~ NA_character_
#     )
#   )


# ---- CAL ACADEMY DATA --------------------------------------------------------

# Double check all cruise/vessel/haul combinations exist
stopifnot(isTRUE(validate_haul(museum_data, all_haul)))

# `vessel` from prev_no is the numeric vessel ID; flag anything unexpected
unknown_vessels <- setdiff(unique(museum_data$vessel), vessels$vessel_id)
if (length(unknown_vessels) > 0) {
  warning(
    "Vessel IDs not in `vessels` lookup: ",
    paste(unknown_vessels, collapse = ", ")
  )
}

validated_museum_data <- museum_data %>%
  dplyr::mutate(
    collected_by = dplyr::if_else(
      grepl("aboard", determiner_last_name),
      stringr::str_remove(determiner_last_name, " aboard .+$|F\\/V .+$"),
      NA_character_
    ),
    year = as.numeric(stringr::str_extract(cruise, "[0-9]{4}")),
    catalog_num = dplyr::if_else(
      is.na(catalog_number), NA_character_, paste("CAS", catalog_number)
    ),
    voucher = ifelse(is.na(voucher), 1, voucher),
    preservative = "95% ETOH",
    notes = dplyr::case_when(
      !is.na(notes) & !is.na(remarks) ~ paste(notes, remarks, sep = "; "),
      is.na(notes) ~ remarks,
      TRUE ~ notes
    ),
    field_id = validate_species(field_id_transcribed),
    field_species_code = get_species_code(field_id)
  ) %>%
  dplyr::select(
    field_id, field_species_code,
    new_id = full_name,
    cruise, vessel, haul, voucher,
    year, preservative, collected_by,
    taxonomist = determiner_last_name,
    notes, catalog_num
  )


# ==============================================================================
# 3. HARMONIZE + BUILD VOUCHER TABLE
# ==============================================================================

message("Building museum voucher dataset...")

all_catch_species <- dplyr::left_join(
  all_catch, classification,
  by = "species_code"
) %>%
  dplyr::select(-family, -species_code)

# haul_lookup <- all_haul %>%
#   dplyr::select(hauljoin, cruise, vessel, haul, dplyr::any_of("region"))

new_vouch <- validated_museum_data %>%
  dplyr::filter(!is.na(new_id)) %>%
  dplyr::mutate(
    new_id           = validate_species(new_id),
    new_species_code = get_species_code(new_id)
  ) %>%
  dplyr::left_join(
    all_catch_species,
    by = dplyr::join_by(field_id == species_name, cruise, haul, vessel, voucher)
  ) %>%
  # dplyr::left_join(haul_lookup, by = c("cruise", "vessel", "haul")) %>%
  dplyr::rename(vessel_id = vessel, weight_kg = weight) %>%
  dplyr::mutate(
    standard_length_mm = NA_real_,
    n_specimens        = NA_integer_,
    id_date            = as.Date(NA)
  )


# ==============================================================================
# 4. QA CHECKS
# ==============================================================================

# Final IDs not in our database (must fix before proceeding)
unresolved <- new_vouch %>% dplyr::filter(is.na(new_species_code))
stopifnot("Resolve final species name issues before upload" = nrow(unresolved) == 0)

# Field IDs not in our database (review; not blocking)
unresolved_field <- new_vouch %>%
  dplyr::filter(is.na(field_species_code) & !is.na(field_id) & field_id != "NA") %>%
  dplyr::distinct(field_id)
if (nrow(unresolved_field) > 0) {
  warning(
    "Field IDs not in database: ",
    paste(unresolved_field$field_id, collapse = ", ")
  )
}

# Hauls with no hauljoin match
if (any(is.na(new_vouch$hauljoin))) {
  warning(sum(is.na(new_vouch$hauljoin)), " rows have no matching hauljoin in all_haul.")
}


# ==============================================================================
# 5. FORMAT TO ORACLE LAYOUT
# ==============================================================================

vouchers <- format_vouchers(new_vouch)


# ==============================================================================
# 6. CONNECT TO ORACLE (RACE_DATA)
# ==============================================================================

channel <- oracle_connect()


# ==============================================================================
# 7. PREVIEW, THEN UPDATE ONLY NEW / CHANGED ROWS
# ==============================================================================

# Preview: nothing is written
res <- update_oracle_vouchers(vouchers, channel, dry_run = TRUE, overwrite_with_na = FALSE)
# View(res$insert)
# View(res$changes)

# Once the preview looks right, write to Oracle:
# update_oracle_vouchers(vouchers, channel, dry_run = FALSE)
