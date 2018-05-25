# ------------------------------------------------------------------------------
# Program Name: C.1.gridding_data_reformatting.R
# Author(s): Leyang Feng
# Date Last Updated: Apr 3, 2017
# Program Purpose: Reformat the downscaled IAM emissions for gridding
# Input Files:
# Output Files:
# Notes:
# TODO: update reference emissions so there would be no NA in X2015
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
headers <- c( 'module-A_functions.R', 'all_module_functions.R' )
log_msg <- "Reformat the downscaled IAM emissions for gridding"
script_name <- "C.1.gridding_data_reformatting.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
harm_status <- args_from_makefile[ 2 ]
if ( is.na( iam ) ) iam <- "GCAM4"

MODULE_C <- "../code/module-C/"

# ------------------------------------------------------------------------------
# 1. Read mapping files and axtract iam info
# read in master config file
master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list
iam_info_list <- iamInfoExtract( master_config, iam )

print( paste0( 'The IAM to be processed is: ', iam_name  ) )

# -----------------------------------------------------------------------------
# 2. Read in the downscaled emissions and gridding mapping file
iam_data <- readData( domain = 'MED_OUT',
                      file_name = paste0( 'B.', iam, '_', harm_status, '_emissions_downscaled_for_gridding', '_', RUNSUFFIX ) )
sector_mapping <- readData( domain = 'GRIDDING', domain_extension = 'gridding-mappings/', file_name = gridding_sector_mapping )

# -----------------------------------------------------------------------------
# 3. Make necessary change
iam_em <- merge( iam_data, sector_mapping,
                 by.x = 'sector', by.y = 'sector_name' )
iam_em$sector <- NULL
colnames( iam_em )[ which( colnames( iam_em ) == 'sector_short' ) ] <- 'sector'

# remove the version number in scenario name
iam_em$scenario<- substr(iam_em$scenario, 1, nchar(iam_em$scenario)-4)

# -----------------------------------------------------------------------------
# 4. Write out
# write the interpolated iam_data into intermediate output folder
out_filname <- paste0( 'C.', iam, '_', harm_status, '_emissions_reformatted', '_', RUNSUFFIX )
writeData( iam_em , 'MED_OUT', out_filname, meta = F )

# END
logStop()
