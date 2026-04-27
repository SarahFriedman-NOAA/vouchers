## R/setup.R ---------------------------------------------------------------


# Packages -----------------------------------------------------------------
pkgs <- c(
  "tidyverse",
  "RODBC",
  "here",
  "janitor",
  "gapindex",
  "googledrive",
  "getPass",
  "taxize"
)

invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}))

rm(pkgs)


# Functions ----------------------------------------------------------------
source("code/00_functions.R")


# Oracle connection --------------------------------------------------------
oracle_file <- "Z:/Projects/ConnectToOracle.R"

if (file.exists(oracle_file)) {
  source(oracle_file)
} else {
  gapindex::get_connected()
}


# Download Oracle data -------------------------------------------------------
source("code/00_download_data.R")


