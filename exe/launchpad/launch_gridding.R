# ------------------------------------------------------------------------------
# Program Name: luanch_gridding.R
# Author(s): Leyang Feng
# Date Last Updated: Mar 20, 2017 
# Program Purpose: The script runs all gridding scripts (module-C). 
# Input Files: 
# Output Files:
# Notes: 
# TODO: Find a smart way to clean final-out folder 
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
headers <- c( 'common_data.R', 'data_functions.R', 'module-A_functions.R', 'all_module_functions.R', 'gridding_functions.R', 'nc_generation_functions.R' ) 
log_msg <- "Initiate gridding routines." 
script_name <- "launch_gridding.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )
# -----------------------------------------------------------------------------
# 1. Set up desired IAM to be processing

# debug
#args_from_makefile <- c( 'MESSAGE-GLOBIOM', 
#                         'Unharmonized',
#                         'C:/Users/feng999/Documents/emissions_downscaling/final-output/module-C' )

# debug CLI


# getting target IAM from command line arguement
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
harm_status <- args_from_makefile[ 2 ]
modc_out <- args_from_makefile[ 3 ]    

domainmapping <- read.csv( DOMAINPATHMAP, stringsAsFactors = F )
domainmapping[ domainmapping$Domain == 'MODC_OUT', "PathToDomain" ] <- modc_out
write.csv( domainmapping, DOMAINPATHMAP, row.names = F )

# -----------------------------------------------------------------------------
# 2. Clean up relics
#fin_filelist <- list.files( '../final-output/module-C/'  )
#fin_file_list <- fin_filelist[ fin_filelist != 'README' ]
#if ( length( fin_file_list ) > 0 ) { 
#  invisible( file.remove( paste0( '../final-output/module-C/', fin_file_list ) ) )
#}

#if ( length( list.files( path = '../intermediate-output/', pattern = '^C.' ) ) > 0 ) {  
#  invisible( file.remove( paste0( '../intermediate-output/', list.files( path = '../intermediate-output/', pattern = 'C.' ) ) ) )
#}

# -----------------------------------------------------------------------------
# 3. Source module-B script in order
source( '../code/module-C/C.1.gridding_data_reformatting.R' )
source( '../code/module-C/C.2.1.gridding_nonair.R' )
