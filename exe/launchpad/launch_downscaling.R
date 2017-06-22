# ------------------------------------------------------------------------------
# Program Name: luanch_downscaling.R
# Author(s): Leyang Feng
# Date Last Updated: Mar 20, 2017 
# Program Purpose: The script runs all downscaling scripts (module-B). 
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
headers <- c( 'common_data.R', 'data_functions.R', 'module-A_functions.R', 'all_module_functions.R' ) 
log_msg <- "Initiate downscaling routines." 
script_name <- "launch_downscaling.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )
# -----------------------------------------------------------------------------
# 1. Set up desired IAM to be processing

# debug
# args_from_makefile <- c( 'MESSAGE-GLOBIOM', 
#                          'Unharmonized',
#                          'C:/Users/feng999/Documents/emissions_downscaling/input/IAM_emissions/MESSAGE-GLOBIOM_SSP2-Ref-SPA0-V25_unharmonized.xlsx',
#                          'C:/Users/feng999/Documents/emissions_downscaling/final-output/module-B' )

# debug CLI


# getting target IAM from command line arguement
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
harm_status <- args_from_makefile[ 2 ]
input_file <- args_from_makefile[ 3 ]   
modb_out <- args_from_makefile[ 4 ]    

domainmapping <- read.csv( DOMAINPATHMAP, stringsAsFactors = F )
domainmapping[ domainmapping$Domain == 'MODB_OUT', "PathToDomain" ] <- modb_out
write.csv( domainmapping, DOMAINPATHMAP, row.names = F )

# -----------------------------------------------------------------------------
# 2. Clean up relics
#fin_filelist <- list.files( '../final-output/module-B/'  )
#fin_file_list <- fin_filelist[ fin_filelist != 'README' ]
#if ( length( fin_file_list ) > 0 ) { 
#  invisible( file.remove( paste0( '../final-output/module-B/', fin_file_list ) ) )
#}

#if ( length( list.files( path = '../intermediate-output/', pattern = 'B.' ) ) > 0 ) {  
#  invisible( file.remove( paste0( '../intermediate-output/', list.files( path = '../intermediate-output/', pattern = 'B.' ) ) ) )
#}

# -----------------------------------------------------------------------------
# 3. Source module-B script in order
source( '../code/module-B/B.1.IAM_snapshot_reformatting.R' )
source( '../code/module-B/B.2.IAM_reference_emission_preparation.R' )
source( '../code/module-B/B.3.regional_pop_gdp_preparation.R' )
source( '../code/module-B/B.4.1.IAM_emissions_downscaling_linear.R' )
source( '../code/module-B/B.4.2.IAM_emissions_downscaling_ipat.R' )
source( '../code/module-B/B.5.IAM_emissions_downscaled_cleanup.R' )
if ( iam == 'REMIND-MAGPIE' ) { 
  source( '../code/module-B/B.6.IAM_emissions_regional_aggregation.R' )
}
