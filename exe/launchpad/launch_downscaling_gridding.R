# ------------------------------------------------------------------------------
# Program Name: luanch_downscaling_gridding.R
# Author(s): Leyang Feng
# Date Last Updated: Nov 28, 2017
# Program Purpose: The script runs downscaling and gridding
# Input Files:
# Output Files:
# Notes:
# TODO:
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS input directory and define PARAM_DIR as the
# location of the CEDS parameters directory, relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'emissions_downscaling/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c()
log_msg <- "Initiate downscaling routines."
script_name <- "launch_downscaling.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 1. Set up desired IAM to be processing

# debug flag
debug <- F

if (debug) {
  args_from_makefile <- c( 'MESSAGE-GLOBIOM',
                           'Harmonized-DB',
                           'C:/Users/brau074/Documents/emissions_downscaling/input/IAM_emissions/MESSAGE-GLOBIOM_SSP2-45/output_harmonized.xlsx',
                           'C:/Users/brau074/Documents/emissions_downscaling/final-output/module-B',
                           'C:/Users/brau074/Documents/emissions_downscaling/final-output/module-C',
                           'NOTgridding' )

  calculationDir <- "/Users/Caleb/Documents/JGCRI/emissions_downscaling/code/error/parameters"
  calculationYears <- 2016:2020
} else {
  # get args from command line
  args_from_makefile <- commandArgs( TRUE )
}

# extract arguments from args_from_makefile
iam <-           args_from_makefile[ 1 ]
harm_status <-   args_from_makefile[ 2 ]
input_file <-    args_from_makefile[ 3 ]
modb_out <-      args_from_makefile[ 4 ]
modc_out <-      args_from_makefile[ 5 ]
gridding_flag <- args_from_makefile[ 6 ]
run_species <-   args_from_makefile[ 7 ]

# the flag for intermediate file cleaning
MED_OUT_CLEAN <- get_global_constant( 'clean_med_out' )

# update domainmapping for current run
domainmapping <- read.csv( DOMAINPATHMAP, stringsAsFactors = F )

# create output directories (if they don't already exist)
dir.create( modb_out )

# modc_out only needs to be created if only the gridding flag is given
if ( gridding_flag == 'gridding' ) {
  dir.create( modc_out )
} else {
  modc_out <- NA
}

# create unique directory for intermediate files
st_time <- format( Sys.time(), '%m-%d-%H%M%S' )
med_out <- paste0( '../intermediate-output', '/', st_time )
while ( dir.exists( med_out ) ) {
  Sys.sleep(1)
  st_time <- format( Sys.time(), '%m-%d-%H%M%S' )
  med_out <- paste0( '../intermediate-output', '/', st_time )
}
RUNSUFFIX <- st_time
dir.create( med_out )

domainmapping[ domainmapping$Domain == 'MED_OUT',  "PathToDomain" ] <- med_out
domainmapping[ domainmapping$Domain == 'MODB_OUT', "PathToDomain" ] <- modb_out
domainmapping[ domainmapping$Domain == 'MODC_OUT', "PathToDomain" ] <- modc_out


# -----------------------------------------------------------------------------
# 2. Source module-B script in order
modb_in <- domainmapping[ domainmapping$Domain == 'MODB', "PathToDomain" ]
source( paste0( modb_in, '/B.1.IAM_input_reformatting.R' ) )
source( paste0( modb_in, '/B.2.IAM_reference_emission_preparation.R' ) )
source( paste0( modb_in, '/B.3.regional_pop_gdp_preparation.R' ) )
source( paste0( modb_in, '/B.4.1.IAM_emissions_downscaling_linear.R' ) )
source( paste0( modb_in, '/B.4.2.IAM_emissions_downscaling_ipat.R' ) )
source( paste0( modb_in, '/B.4.3.IAM_emissions_downscaling_consistency_check.R' ) )
source( paste0( modb_in, '/B.5.IAM_emissions_downscaled_cleanup.R' ) )


# -----------------------------------------------------------------------------
# 3. Source module-C script in order
if ( gridding_flag == 'gridding' ) {
  modc_in <- domainmapping[ domainmapping$Domain == 'MODC', "PathToDomain" ]
  source( paste0( modc_in, '/C.1.gridding_data_reformatting.R' ) )
  source( paste0( modc_in, '/C.2.1.gridding_nonair.R' ) )
  source( paste0( modc_in, '/C.2.2.gridding_air.R' ) )
}

# -----------------------------------------------------------------------------
# 4. clean the intermediate files
if ( MED_OUT_CLEAN ) {
  invisible( unlink( med_out, recursive = T ) )
  invisible( file.remove( paste0( '../documentation/IO_documentation_', RUNSUFFIX, '.csv' ) ) )
}
