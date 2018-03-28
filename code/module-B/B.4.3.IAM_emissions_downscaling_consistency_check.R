# ------------------------------------------------------------------------------
# Program Name: B.4.3.IAM_emissions_downscaling_consistency_check.R
# Author(s): Xavier Gutierrez
# Date Last Updated: Mar 28, 2017
# Program Purpose: Check that downscaled countries, when aggregated to regions, match reference emissions.
# Input Files: intermediate-output/[SUFFIX]/B.[IAM]_emissions_downscaled_ipat_[SUFFIX].csv
#              intermediate-output/[SUFFIX]/B.[IAM]_emissions_ipat_[SUFFIX].csv 
#  or??        intermediate-output/[SUFFIX]/B.[ref_name]_baseyear_ipat_[SUFFIX].csv 
# 
# Output Files: 
# Notes: 
# TODO: Write the consistency check
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
log_msg <- "Downscale energy related emissions using IPAT approach" 

script_name <- "B.4.3.IAM_emissions_downscaling_consistency_check.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
if ( is.na( iam ) ) iam <- "GCAM4"

MODULE_B <- "../code/module-B/"

