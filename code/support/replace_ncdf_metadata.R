# Copyright 2018 Battelle Memorial Institute

# replace_ncdf_metadata.R
#
# Author: Caleb Braun
# Date:   5/25/18
#
# Replaces a netCDF metadata (global) attribute with a new value.
#
# Example use:
#
# Rscript replace_ncdf_metadata.R <path-to-files> <attribute-name> <attribute-value>
#
#   Where:
#   -	path-to-files = The path to a single .nc file, or a directory containing
#                     multiple .nc files.
#   -	attribute-name = The name of a global attribute to add or replace.
#   -	attribute-value = The text value of the global attribute
#
#   To replace the license attribute in all module-C netCDF files:
#
#     Rscript replace_ncdf_metadata.R final-output/module-C license "new license value"
#
#   Alternatively, to replace the value in just one file:
#
#     Rscript replace_ncdf_metadata.R final-output/module-C/BC-em-anthro_input4MIPs_emissions_CMIP_MESSAGE-GLOBIOM-SSP2-45-SPA2-V1_gn_201501-210012.nc license "new license value"


library(ncdf4)

# Script inputs
NCDF_DIR <- 'final-output/module-C' # Directory of files, or single .nc file
NCDF_DIR <- 'final-output/module-C/BC-em-anthro_input4MIPs_emissions_ScenarioMIP_MESSAGE-GLOBIOM-ssp2-45-1-0_gn_201501-210012.nc' # Directory of files, or single .nc file
ATT_NAME <- 'source_version'               # The name of the metadata att to replace
NEW_VAL <- 1.0      # The new value of the attribute

args <- commandArgs(TRUE)
if (length(args)) {
  NCDF_DIR <- args[1]
  ATT_NAME <- args[2]
  NEW_VAL <- args[3]
}

# Get all the file names to replace
if (substr(NCDF_DIR, nchar(NCDF_DIR) - 2, nchar(NCDF_DIR)) == ".nc") {
  fnames <- NCDF_DIR
} else {
  fnames <- list.files(NCDF_DIR, '.*\\.nc$', full.names = T)
}

for (ncdf_file_name in fnames) {
  # Open file and get attribute. In ncatt_get, varid = 0 means a global var.
  f <- nc_open(ncdf_file_name, write = TRUE)
  a <- ncatt_get(f, 0, ATT_NAME)

  if (!a$hasatt)
    message(paste("Attribute", ATT_NAME, "does not exist and will be created."))

  ncatt_put(f, 0, ATT_NAME, NEW_VAL, 'float')

  message(paste0("Replaced '", ATT_NAME, "' with new value in the file ",
                 ncdf_file_name))
  nc_close(f)
}
