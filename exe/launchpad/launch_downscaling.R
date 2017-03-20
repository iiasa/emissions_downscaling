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

# -----------------------------------------------------------------------------
# 1. Set up desired IAM to be processing

# munally set up target IAM
#target_iam <- 'GCAM4'
#args_from_makefile <- target_iam

# getting target IAM from command line arguement
if ( !exists( 'args_from_makefile' ) ) target_iam <- commandArgs( TRUE )
args_from_makefile <- target_iam[ 1 ]

# -----------------------------------------------------------------------------
# 2. Clean up relics
if ( length( list.files( '../final-output/module-B/'  ) ) > 0 ) { 
  invisible( file.remove( paste0( '../final-output/module-B/', list.files( '../final-output/module-B/' ) ) ) )
}
if ( length( list.files( path = '../intermediate-output/', pattern = 'B.' ) ) > 0 ) {  
  invisible( file.remove( paste0( '../intermediate-output/', list.files( path = '../intermediate-output/', pattern = 'B.' ) ) ) )
}
# -----------------------------------------------------------------------------
# 3. Source module-B script in order
source( '../code/module-B/B.1.IAM_snapshot_reformatting.R' )
source( '../code/module-B/B.2.IAM_reference_emission_preparation.R' )
source( '../code/module-B/B.3.regional_pop_gdp_preparation.R' )
source( '../code/module-B/B.4.1.IAM_emissions_downscaling_linear.R' )
source( '../code/module-B/B.4.2.IAM_emissions_downscaling_ipat.R' )
source( '../code/module-B/B.5.IAM_emissions_downscaled_cleanup.R' )
