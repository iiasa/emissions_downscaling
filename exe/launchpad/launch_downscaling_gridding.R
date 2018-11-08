# Copyright 2018 Battelle Memorial Institute

# ------------------------------------------------------------------------------
# Program Name: launch_downscaling_gridding.R
# Author(s): Leyang Feng, Caleb Braun
# Date Last Updated: June 22, 2018
# Program Purpose: The script runs downscaling and gridding
# Input Files:
# Output Files:
# Notes:
# TODO:
# ------------------------------------------------------------------------------

# 0. Read in global settings and headers ----------------------------------

# Must be run from the emissions_downscaling/input directory
if ( !endsWith( getwd(), '/input' ) ) setwd( 'input' )
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c()
log_msg <- "Initiate downscaling routines."
script_name <- "launch_downscaling.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# 1. Set up desired IAM to be processing ----------------------------------

SCENARIO_DIAG <- get_constant( 'total_ems_plots' )
MED_OUT_CLEAN <- get_constant( 'clean_med_out' )
DEBUG <- get_constant( 'debug' )

# create unique runsuffix for intermediate files
RUNSUFFIX <- paste0( format( Sys.time(), '%m-%d-%H%M%S' ), '_',
                     substr( digest::sha1( runif(1) ), 1, 6 ) )

if ( DEBUG ) {
  message( 'Debug mode is on' )
  command_args <- c( 'IMAGE',
                     'Harmonized-DB',
                     '/Users/brau074/Documents/emissions_downscaling/input/IAM_emissions/IMAGE_SSP1-19/output_harmonized.xlsx',
                     '/Users/brau074/Documents/emissions_downscaling/final-output/module-B',
                     '/Users/brau074/Documents/emissions_downscaling/final-output/module-C',
                     'gridding', 'CO2' )

  calculationDir <- "/Users/brau074/Documents/emissions_downscaling/code/error/parameters"
  calculationYears <- 2016:2020
} else {
  command_args <- commandArgs( TRUE )  # get args from command line
}

# extract arguments from command_args
iam           <- command_args[ 1 ]
harm_status   <- command_args[ 2 ]
input_file    <- command_args[ 3 ]
modb_out      <- command_args[ 4 ]
modc_out      <- command_args[ 5 ]
gridding_flag <- command_args[ 6 ]
run_species   <- command_args[ 7 ]

# update domainmapping for current run
domainmapping <- read.csv( DOMAINPATHMAP, stringsAsFactors = F )

# create output directories (if they don't already exist)
if ( !dir.exists( modb_out ) ) dir.create( modb_out )

# modc_out only needs to be created if only the gridding flag is given
if ( gridding_flag == 'gridding' ) {
  if ( !dir.exists( modc_out ) ) dir.create( modc_out )
} else {
  modc_out <- NA
}

med_out <- paste0( '../intermediate-output/', RUNSUFFIX )
if ( dir.exists( med_out ) ) stop( "Intermediate output directory not unique" )
dir.create( med_out )

domainmapping[ domainmapping$Domain == 'MED_OUT',  "PathToDomain" ] <- med_out
domainmapping[ domainmapping$Domain == 'MODB_OUT', "PathToDomain" ] <- modb_out
domainmapping[ domainmapping$Domain == 'MODC_OUT', "PathToDomain" ] <- modc_out


# 2. Source module-B script in order --------------------------------------
modb_in <- domainmapping[ domainmapping$Domain == 'MODB', "PathToDomain" ]
source( paste0( modb_in, '/B.1.IAM_input_reformatting.R' ) )
source( paste0( modb_in, '/B.2.IAM_reference_emission_preparation.R' ) )
source( paste0( modb_in, '/B.3.regional_pop_gdp_preparation.R' ) )
source( paste0( modb_in, '/B.4.1.IAM_emissions_downscaling_linear.R' ) )
source( paste0( modb_in, '/B.4.2.IAM_emissions_downscaling_ipat.R' ) )
source( paste0( modb_in, '/B.4.3.IAM_emissions_downscaling_consistency_check.R' ) )
source( paste0( modb_in, '/B.5.IAM_emissions_downscaled_cleanup.R' ) )


# 3. Source module-C script in order --------------------------------------
if ( gridding_flag == 'gridding' ) {
  modc_in <- domainmapping[ domainmapping$Domain == 'MODC', "PathToDomain" ]
  source( paste0( modc_in, '/C.1.gridding_data_reformatting.R' ) )
  source( paste0( modc_in, '/C.2.1.gridding_nonair.R' ) )
  source( paste0( modc_in, '/C.2.2.gridding_air.R' ) )
}


# 4. clean the intermediate files -----------------------------------------
if ( MED_OUT_CLEAN ) {
  invisible( unlink( med_out, recursive = T ) )
  invisible( file.remove( paste0( '../documentation/IO_documentation_', RUNSUFFIX, '.csv' ) ) )
}
