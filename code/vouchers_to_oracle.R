# ==============================================================================
# UPLOAD COMPLETED VOUCHER DATA TO RACE_DATA.VOUCHERS
# ==============================================================================


# ------------------------------------------------------------------------------
# Define year and file paths
# ------------------------------------------------------------------------------

last_year <- as.numeric(format(Sys.time(), "%Y")) - 1

drive_name <- paste0("voucher_data_", last_year, "_fish_edited")
local_name <- paste0("voucher_data_", last_year, "_fish_completed.csv")
local_path <- here::here("data", "updates", local_name)


# ------------------------------------------------------------------------------
# Download completed voucher data from Google Drive (if not already local)
# ------------------------------------------------------------------------------

if (!file.exists(local_path)) {
  message("Downloading completed voucher file from Google Drive...")

  googledrive::drive_download(
    file      = drive_name,
    path      = local_path,
    type      = "csv",
    overwrite = FALSE
  )
}


# ------------------------------------------------------------------------------
# Read and clean new voucher data
# ------------------------------------------------------------------------------
new_fish_vouch <- readr::read_csv(local_path)

# Haul lookup (hauljoin, and region if all_haul carries it)
haul_lookup <- all_haul %>%
  dplyr::select(hauljoin, cruise, vessel_id = vessel, haul, dplyr::any_of("region"))

# Species lookups from taxonomy
tax <- taxonomy %>% dplyr::select(species_name, species_code)

new_vouch <- new_fish_vouch %>%
  dplyr::filter(!is.na(new_id)) %>% # keep only resolved IDs
  dplyr::select(-dplyr::any_of(c(
    "field_family", "species_code", "new_species_code", "field_species_code",
    "hauljoin", intersect("region", names(haul_lookup))
  ))) %>%
  dplyr::rename(dplyr::any_of(c(vessel_id = "vessel"))) %>%
  dplyr::mutate(
    id_date  = as.Date(id_date, format = "%m/%d/%Y"),
    field_id = validate_species(field_id), # fix misspellings
    field_species_code = get_species_code(field_id),
    new_id   = validate_species(new_id), # fix misspellings
    new_species_code = get_species_code(new_id),
    voucher  = tidyr::replace_na(voucher, 1),
    year     = as.numeric(stringr::str_extract(as.character(cruise), "^[0-9]{4}"))
  ) %>%
  dplyr::left_join(haul_lookup, by = c("cruise", "vessel_id", "haul"))


# ------------------------------------------------------------------------------
# Validate species resolution (must fix before proceeding)
# ------------------------------------------------------------------------------

unresolved <- new_vouch %>%
  dplyr::filter(is.na(new_species_code))

stopifnot("Resolve species name issues before upload" = nrow(unresolved) == 0)

if (any(is.na(new_vouch$hauljoin))) {
  warning(sum(is.na(new_vouch$hauljoin)), " rows have no matching hauljoin in all_haul.")
}


# ------------------------------------------------------------------------------
# Format to Oracle layout
# ------------------------------------------------------------------------------

vouchers <- format_vouchers(new_vouch)


# ------------------------------------------------------------------------------
# Connect to Oracle (RACE_DATA)
# ------------------------------------------------------------------------------

channel <- oracle_connect()


# ------------------------------------------------------------------------------
# Preview, then update only new / changed rows
# ------------------------------------------------------------------------------

# Preview: nothing is written
res <- update_oracle_vouchers(vouchers, channel, dry_run = TRUE, overwrite_with_na = FALSE)
# View(res$insert)
# View(res$changes)

# Once the preview looks right, write to Oracle:
# update_oracle_vouchers(vouchers, channel, dry_run = FALSE)
