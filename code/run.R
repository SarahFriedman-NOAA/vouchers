# ==============================================================================
# VOUCHER DATA PIPELINE
# ==============================================================================

# This script orchestrates the full voucher workflow:
# 1. Setup (packages, Oracle connection, data download)
# 2. Generate voucher files and upload to Google Drive
# 3. Append vouchers to Oracle table
# 4. Process museum requests and uploads

target_year <- as.numeric(format(Sys.time(), "%Y")) # year(s) for voucher extraction
# year <- c(2025, 2026)

override <- FALSE # toggle to override cached tables and download fresh

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------
# Loads libraries, connects to Oracle, downloads required data
source("code/00_setup.R")  


# ------------------------------------------------------------------------------
# 2. POST-SURVEY: EXPORT VOUCHERS TO GOOGLE DRIVE
# ------------------------------------------------------------------------------

source("code/vouchers_to_drive.R")


# ------------------------------------------------------------------------------
# 3. AFTER RE-ID OF VOUCHERS: UPDATE ORACLE VOUCHER TABLE FROM GOOGLE DRIVE
# ------------------------------------------------------------------------------

source("code/vouchers_to_oracle.R")


# ------------------------------------------------------------------------------
# 4. MUSEUM REQUEST: HAUL METADATA
# ------------------------------------------------------------------------------

specimen_data <- get_specimen_metadata(
  input_file = "data/requests/SpecimenListSIOBIC_AlaskaVouchers_2026.xlsx",
  haul_data = all_haul,
  cruise_col = "Cruise #",
  vessel_col = "Vessel",
  haul_col = "Haul #"
  )

view(specimen_data)


# ------------------------------------------------------------------------------
# 5. MUSEUM DATA INGEST → ORACLE
# ------------------------------------------------------------------------------

# Update this path each time new museum data arrives
museum_file <- "data/updates/voucher_inverts_cas_2024.xlsx"

source("code/museum_data_to_oracle.R")
