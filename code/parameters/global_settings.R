# ----------------------------------------------------------------------------------
# emissions_downscaling R header file: global settings
# Authors: Modified from CEDS by Caleb Braun
# Last Updated: 5/25/18
#
# This file must be sourced by all downscaling R scripts, before any other
# sourced files. It provides global variables and necessary system settings.
# -----------------------------------------------------------------------------

# Load required libraries. If library isn't installed, outputs warning message
loadPackage <- function(pName, versions = NULL){
  minVersion <- if( !is.null(versions) ) versions[[pName]] else 0

  if( suppressMessages(!require( pName, character.only=T ) )){
    cat( "Couldn't load '", pName, "'. Please Install.\n", sep="")
    stop(paste0( "Couldn't load '", pName, "'. Please Install.\n" ))
  }

  if( packageVersion(pName) < minVersion ) {
    stop(paste0( "Package '", pName, "' version ", minVersion, " or greater is required."))
  }
}

libs <- c( plyr = "1.8.4", dplyr = "0.7.2", ggplot2 = "2.2.0",
           gridExtra = "2.2.1", magrittr = "1.5", readxl = "1.0.0",
           stringr = "1.1.0", tidyr = "0.6.3", openxlsx = "4.0.0",
           zoo = "1.7-14" )

lapply(names(libs), loadPackage, libs)


# -----------------------------------------------------------------------------
# Global settings (in CAPITALS)
# TODO: check build target. If it's "clean", or something like that, reset everything
# This first group of settings is protected--we don't want it re-set every time
# this header is read.
if( !exists( "GCAM_SOURCE_FN" ) ) { # i.e. #ifndef
  GCAM_SOURCE_FN <- c( "?" )        # name of currently executing source file (stack structure)
  GCAM_LOG_SAVE  <- c( FALSE )      # whether current log is also being saved to file (stack structure)
  GCAM_SOURCE_RD <- 0               # recursion depth, an index into above structures
  DEPENDENCIES   <- list()          # dependencies (i.e. what files scripts read)
  OUTPUTS        <- list()          # outputs (i.e. what files scripts write)
}

# Comment character for files
GCAM_DATA_COMMENT <- "#"

# List of domain (groups of files) mappings
DOMAINPATHMAP <- "mappings/domainmapping.csv"

# Specify the location of the module from the data system root directory
MODULE_PROC_ROOT <- PARAM_DIR


# -----------------------------------------------------------------------------
# Set-up protected environement for shared variables

em_gridding_env <- new.env()

# Function to retrive values from protected environement
get_constant <- function (const_name) {
  return( get( const_name, envir = em_gridding_env ) )
}


# Configuration Options ---------------------------------------------------

# Input files
em_gridding_env$reference_emissions <- 'CEDS_by_country_by_CEDS_sector_with_luc_all_em'
em_gridding_env$config_file         <- 'config_CMIP.R'

# Output netCDF metadata options
source( paste0( 'ncdf_config/', get_constant( 'config_file' ) ), em_gridding_env )

# How should NMVOC speciation be done? Value must be one of the following:
#   'all'  - Do NMVOC speciation along with all other emissions
#   'only' - Do NMVOC speciation and no other emissions
#   'none' - Do not do NMVOC speciation
em_gridding_env$voc_speciation <- 'all'

# Diagnostics
em_gridding_env$diagnostic_plots <- T
em_gridding_env$total_ems_plots  <- T    # generate plot of all ems in scenario
em_gridding_env$error_tolerance  <- 0.1  # minimum percent difference allowed

# Other settings
em_gridding_env$debug         <- T       # run in debug mode
em_gridding_env$clean_med_out <- F       # erase intermediate output when done

# The below are not used anywhere, but may be good references
em_gridding_env$supported_species       <- c( 'BC', 'CO', 'NH3', 'NOx', 'OC', 'Sulfur', 'VOC' )
em_gridding_env$supported_species_alias <- c( 'BC', 'CO', 'NH3', 'NOx', 'OC', 'SO2', 'NMVOC' )

# Standard submission header columns
em_gridding_env$submission_header_cols <- c( 'model', 'scenario', 'region', 'variable', 'unit' )

# Lock the environment and all variables within it
lockEnvironment( em_gridding_env, bindings = T )
