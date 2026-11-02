# RACE_DATA Voucher Pipeline

This repository contains the R-based data pipeline for managing biological voucher specimens (fish and invertebrates) for the RACEBASE database. It handles the extraction of survey data from Oracle, synchronization of editable datasets with Google Drive, integration of external museum data (e.g., California Academy of Sciences, UW), and secure transaction-based uploads back into Oracle.

## Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Project Structure](#project-structure)
- [Usage](#usage)

## Overview {#overview}

The pipeline automates several critical tasks for the voucher specimen lifecycle:

1.  **Extraction & Caching:** Pulls required `RACEBASE` and `GAP_TAXONOMY` tables into a local cache to speed up processing and reduce database load.

2.  **Post-Survey Export:** Identifies new voucher records from catch data, formats them, and pushes them to a shared Google Drive folder for taxonomists to edit.

3.  **Data Re-Ingestion:** Pulls the human-edited sheets back from Google Drive, resolves taxonomic discrepancies, and safely updates the Oracle database.

4.  **Museum Harmonization:** Ingests external museum spreadsheets, harmonizes field identifiers with valid species codes, joins required haul metadata, and uploads final catalog numbers and identifications to Oracle.

## Prerequisites {#prerequisites}

- **R** (\>= 4.0 recommended)
- **Oracle Database Access:** You must have an active ODBC connection and credentials for the `AFSC` Oracle database, with write access to `RACE_DATA.VOUCHERS`.
- **Google Drive Access:** Requires authentication via the `googledrive` package to the specific shared folders used by the team.

**R Packages:** Install the following dependencies before running the pipeline:

``` r
install.packages(c("tidyverse", "RODBC", "here", "janitor", "googledrive", "getPass", "taxize", "readxl"))
```

*Note: The `gapindex` package is also required for Oracle connectivity.*

## Project Structure {#project-structure}

- `run.R`: The primary script.
- `code/`
  - `00_setup.R`: Loads packages, initializes functions, connects to Oracle, and triggers data downloads.
  - `00_download_data.R`: Handles local caching of Oracle tables (Species, Taxonomy, Cruise, Haul, Catch).
  - `00_functions.R`: Core utility functions for Oracle database writes, dynamic SQL generation, and interactive taxonomy validation.
  - `vouchers_to_drive.R`: Generates raw voucher templates and uploads them to Google Drive.
  - `vouchers_to_oracle.R`: Downloads edited vouchers from Drive and updates the Oracle DB.
  - `museum_data_to_oracle.R`: Cleans and formats museum spreadsheet data to conform to Oracle standards.

## Usage {#usage}

To execute the pipeline, open `run.R`, configure your `target_year` (and toggle `override` if you need to bust the local cache), and source the specific scripts you need for your current task. When running scripts that invoke taxonomy checking, ensure you are running them interactively (e.g., in RStudio) so you can respond to the species resolution prompts. Recommended workflow:

1.  After survey season, run the first part of the `run.R` script to `code/vouchers_to_drive.R`. This will pull all the voucher records from the past year and create a google spreadsheet to work off of in the Systematics Lab.

2.  After all of the voucher fishes have been processed, run `code/vouchers_to_oracle.R`, which will pull the now completed google drive sheet into the oracle vouchers table. Note that this will run an interactive script to ensure all species identifications are present in our database and have a valid species code.

3.  Museums may request haul-level locality information on the specimens we send to them. Request a spreadsheet with the field id, cruise, vessel, haul, and voucher number and use the `get_specimen_metadata()` function to create a .csv of all metadata to send to them.

4.  As museums send back information (e.g. new identifications, catalog numbers, etc.) these can be ingested into the oracle database using `code/museum_data_to_oracle.R`. Note that this will run an interactive script to ensure all species identifications are present in our database and have a valid species code.
