# ------------------------------------------------------------------------------
# Program Name: B.1.IAM_snapshot_reformatting
# Author(s): Leyang Feng
# Date Last Updated: Feb 13, 2017 
# Program Purpose: The script reads in IAM snapshots and separate the information
#                  in 'Variable' column, then write the df into intermediate_out 
#                  folder as input for next script.  
# Input Files: csv or excels in /input/IAM_snapshot folder 
# Output Files: B.[iam_name]_emissions_reformatted 
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
log_msg <- "Reformat IAM snapshot to separate information for sector, species, etc. " 
script_name <- "B.1.IAM_snapshot_reformatting"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
if ( is.na( iam ) ) iam <- "GCAM4"

MODULE_B <- "../code/module-B/"

# ------------------------------------------------------------------------------
# 1. Read mapping files and axtract iam info
# read in master config file
master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list
iam_info_list <- iamInfoExtract( master_config, iam )

print( paste0( 'The IAM to be processed is: ', iam_name  ) )

# -----------------------------------------------------------------------------
# 2. Read in the snapshot and tease out un-wanted models
# file_list <- list.files( './IAM_snapshot' )
# file_list <- file_list[ file_list!= 'README' ]
# 
# data_list <- lapply( file_list, function( file_name ) {
#   file_ext <- getFileExt( file_name )
#   file_name_no_ext <- file_path_sans_ext( file_name )
#   df <- readData( domain = 'INPUT',
#                   domain_extension = 'IAM_snapshot/',
#                   file_name = file_name_no_ext,
#                   extension = paste0( '.', file_ext ) )
#   return( df )
#   } )
# data_df <- do.call( 'rbind', data_list )
# data_df <- data_df[ data_df$model %in% iam_name, ]
snapshot_file_parts <- unlist( strsplit( snapshot_file, '.', fixed = T ) )
snapshot_file_ext <- tolower( snapshot_file_parts[ length( snapshot_file_parts ) ] )

if ( snapshot_file_ext == 'csv' ) { 
  data_df <- read.csv( snapshot_file, stringsAsFactors = F )
  data_df <- data_df[ data_df$model %in% iam_name, ]
} 
if ( snapshot_file == 'xlsx' ) { 
  data_df <- read_excel( snapshot_file ) 
  data_df <- data_df[ data_df$model %in% iam_name, ]
  }

# -----------------------------------------------------------------------------
# 3. Reformat the IAM data
iam_data <- data_df
iam_data_x_years <- grep( 'X', colnames( iam_data ), fixed = T, value = T )
iam_start_year <- as.numeric( gsub( 'X', '', iam_data_x_years[ 1 ] ) )
iam_end_year <- as.numeric( gsub( 'X', '', iam_data_x_years[ length( iam_data_x_years ) ] ) )

iam_data$em <- unlist( lapply( strsplit( iam_data$variable, split = '|', fixed = T  ), '[[', 4 ) )
iam_data$sector <- unlist( lapply( strsplit( iam_data$variable, split = '|', fixed = T  ), '[[', 5 ) )
iam_data$harm_status <- unlist( lapply( strsplit( iam_data$variable, split = '|', fixed = T  ), '[[', 6 ) )
iam_data$unit <- unlist( lapply( strsplit( iam_data$unit, split = ' ', fixed = T  ), '[[', 1 ) )

iam_data <- iam_data[ , c( 'model', 'scenario', 'region', 'em', 'sector', 'harm_status', 'unit', iam_data_x_years ) ]

# -----------------------------------------------------------------------------
# 4. Remove the un-suported emission species and keep only harmonized emissions
iam_data <- iam_data[ iam_data$em %in% supported_species, ]
iam_data <- iam_data[ iam_data$harm_status == 'Harmonized' , ]

# -----------------------------------------------------------------------------
# 5. Interpolate the iam_data into all years
all_x_years <- paste0( 'X', iam_start_year : iam_end_year )
int_x_year <- all_x_years[ which( !( all_x_years %in% iam_data_x_years ) ) ]
iam_int <- iam_data
iam_int[ , int_x_year ] <- NA
iam_int <- iam_int[ , c( 'model', 'scenario', 'region', 'em', 'sector', 'harm_status', 'unit', all_x_years ) ]
iam_int <- interpolateXyears( iam_int, int_method = 'linear' )

# -----------------------------------------------------------------------------
# 6. Write out
# write the interpolated iam_data into intermediate output folder
out_filname <- paste0( 'B.', iam_name, '_emissions_reformatted' )
writeData( iam_int, 'MED_OUT', out_filname, meta = F )

# END

