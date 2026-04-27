# ==============================================================================
# POST-SURVEY VOUCHER SPECIMEN PROCESSING
# ==============================================================================


# ------------------------------------------------------------------------------
# Generate voucher dataset from catch data for this year
# ------------------------------------------------------------------------------


# Join catch data with species metadata and initialize voucher fields
vouch_data <- catch_year %>%
  dplyr::left_join(species0, by = "species_code") %>%
  dplyr::mutate(
    year = year,
    
    # Classify taxon group based on species_code ranges
    taxon = dplyr::case_when(
      species_code <= 31550 ~ "fish",
      species_code >= 40001 ~ "invert"
    ),
    
    # Initialize voucher-specific fields (to be filled later)
    field_id            = NA,
    new_id              = NA,
    n_specimens         = NA,
    standard_length_mm  = NA,
    preservative        = NA,
    collected_by        = NA,
    date                = NA,
    taxonomist          = NA,
    notes               = NA
  )


# ------------------------------------------------------------------------------
# Subset and format fish voucher data
# ------------------------------------------------------------------------------

vouch_data_fish <- vouch_data %>%
  dplyr::filter(taxon == "fish") %>%   # currently only processing fish
  dplyr::select(
    field_id       = species,
    field_family   = family,
    region,
    cruise,
    year,
    vessel_id      = vessel,
    haul,
    voucher,
    depth_m        = bottom_depth,
    new_id,
    n_specimens,
    standard_length_mm,
    preservative,
    collected_by,
    date,
    taxonomist,
    notes
  ) %>%
  dplyr::arrange(region, cruise, haul)


# ------------------------------------------------------------------------------
# Write raw voucher file locally
# ------------------------------------------------------------------------------

fish_file <- paste0("voucher_data_", year, "_fish_raw.csv")

readr::write_csv(
  vouch_data_fish,
  here::here("output", "raw", fish_file),
  na = ""
)



# ==============================================================================
# GOOGLE DRIVE SYNC
# ==============================================================================


# ------------------------------------------------------------------------------
# Authenticate Google Drive session
# ------------------------------------------------------------------------------

googledrive::drive_auth()


# ------------------------------------------------------------------------------
# Define file + folder structure
# ------------------------------------------------------------------------------

raw_folder_id  <- "1SBS4_B621j5MNNdJ6_0xavYgwyxoa8M9"
edit_folder_id <- "1zrUUo5lm6Xgd0ovXi9R4RYoW-1o4M73y"

# Expected edited filename
edited_file <- paste0("voucher_data_", year, "_fish_edited.csv")


# ------------------------------------------------------------------------------
# Check existing files on Drive
# ------------------------------------------------------------------------------

raw_drive_files <- googledrive::drive_ls(path = googledrive::as_id(raw_folder_id)) %>%
  purrr::pluck("name")

edit_drive_files <- googledrive::drive_ls(path = googledrive::as_id(edit_folder_id)) %>%
  purrr::pluck("name")



# ------------------------------------------------------------------------------
# Upload raw CSV if not already present
# ------------------------------------------------------------------------------

if (!fish_file %in% raw_drive_files) {
  
  message("Uploading raw voucher file to Drive...")
  
  googledrive::drive_upload(
    media    = here::here("output", "raw", fish_file),
    path     = googledrive::as_id(raw_folder_id),
    type     = "csv",
    name     = paste0("voucher_data_", year, "_fish_raw"),
    overwrite = FALSE
  )
}


# ------------------------------------------------------------------------------
# Upload editable Google Sheet version if not already present
# ------------------------------------------------------------------------------

if (!edited_file %in% edit_drive_files) {
  
  message("Uploading editable voucher file to Drive...")
  
  googledrive::drive_upload(
    media     = here::here("output", "raw", fish_file),
    path      = googledrive::as_id(edit_folder_id),
    type      = "spreadsheet",
    name      = paste0("voucher_data_", year, "_fish_edited"),
    overwrite = FALSE
  )
}