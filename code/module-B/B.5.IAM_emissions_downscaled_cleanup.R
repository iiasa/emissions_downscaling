# ------------------------------------------------------------------------------
# Program Name: B.5.IAM_emissions_downscaled_cleanup.R
# Author(s): Leyang Feng
# Date Last Updated: Feb 13, 2017
# Program Purpose: The script combines multiple part of downscaled emissions
#                  into standard IAMC layout
# Input Files: B.[iam]_emissions_downscaled_linear.csv
#              B.[iam]_emissions_downscaled_ipat.csv
#              B.[iam]_emissions_nods.csv
# Output Files: B.[iam]_emissions_downscaled.csv in final_out
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
log_msg <- "Combine multiple parts of downscaled emissions into one file"
script_name <- "B.5.IAM_emissions_downscaled_cleanup.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
harm_status <- args_from_makefile[ 2 ]
modb_out <- args_from_makefile[ 3 ]
if ( is.na( iam ) ) iam <- "GCAM4"
if ( is.na( modb_out ) ) iam <- "../final-output/module-B"

MODULE_B <- "../code/module-B/"

# ------------------------------------------------------------------------------
# 1. Read mapping files and axtract iam info
# read in master config file
master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list
iam_info_list <- iamInfoExtract( master_config, iam )

var_mapping <- readData( 'MAPPINGS', iamc_var_name_mapping )
var_mapping$IAMC <- paste0( var_mapping$IAMC, '|', harm_status )

# -----------------------------------------------------------------------------
# 2. Read different parts of IAM emissions: IAM nods, IAM_linear_downscaled, IAM_ipat_downscaled
iam_em_nods <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_nods', '_', RUNSUFFIX ) )
iam_em_linear <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_downscaled_linear', '_', RUNSUFFIX ) )
iam_em_ipat <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_downscaled_ipat', '_', RUNSUFFIX ) )

# -----------------------------------------------------------------------------
# 3. Combine different parts of downscaled IAM emissions
colnames( iam_em_nods ) [ which( colnames( iam_em_nods ) == 'region' ) ] <- 'iso'

common_header_col_names <- c( "model", "scenario", "em", "sector", "iso", "unit" )
iam_reporting_years <- c( 2015, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100 )
iam_reporting_xyears <- paste0( 'X', iam_reporting_years )
iam_gridding_years <- 2015 : 2100
iam_gridding_xyears <- paste0( 'X', iam_gridding_years )

iam_em_nods_select <- iam_em_nods[ , c( common_header_col_names, iam_reporting_xyears ) ]
iam_em_linear_select <- iam_em_linear[ , c( common_header_col_names, iam_reporting_xyears ) ]
iam_em_ipat_select <- iam_em_ipat[ , c( common_header_col_names, iam_reporting_xyears ) ]

iam_em_nods_gridding <- iam_em_nods[ , c( common_header_col_names, iam_gridding_xyears ) ]
iam_em_linear_gridding <- iam_em_linear[ , c( common_header_col_names, iam_gridding_xyears ) ]
iam_em_ipat_gridding <- iam_em_ipat[ , c( common_header_col_names, iam_gridding_xyears ) ]

iam_em_full <- rbind( iam_em_nods_select, iam_em_linear_select, iam_em_ipat_select )
iam_em_full$harm_status <- harm_status

iam_em_gridding_full <- rbind( iam_em_nods_gridding, iam_em_linear_gridding, iam_em_ipat_gridding )

# ------------------------------------------------------------------------------
# 4. reformat into standard format
iam_em_iamc <- merge( iam_em_full, var_mapping, by = 'sector', all.x = T )
colnames( iam_em_iamc )[ which( colnames( iam_em_iamc ) == 'IAMC' ) ] <- 'variable'
iam_em_iamc$variable <- unname( mapply( function( var, em ) {
  if ( em == 'SO2' ) { em <- 'Sulfur' }
  if ( em == 'NMVOC' ) { em <- 'VOC' }
  out_var <- gsub( '|XXX|', paste0( '|', em, '|' ), var, fixed = T  )
  return( out_var )
  }, iam_em_iamc$variable, iam_em_iamc$em  ) )

iam_em_iamc$unit <- paste0( 'Mt ', iam_em_iamc$em, '/yr' )
iam_em_iamc$unit <- gsub( 'Sulfur', 'SO2', iam_em_iamc$unit, fixed = T )

output_header_cols <- c( "model", "scenario", "variable", "iso", "unit", iam_reporting_xyears )

final_out <- iam_em_iamc[ , output_header_cols ]
colnames( final_out ) <- gsub( 'X', '', colnames( final_out ) )

# -----------------------------------------------------------------------------
# 5 Write out

out_filename <- paste0( 'B.', iam, '_', harm_status, '_emissions_downscaled' )
writeData( final_out, 'MODB_OUT', out_filename, meta = F )

out_filename <- paste0( 'B.', iam, '_', harm_status, '_emissions_downscaled_for_gridding', '_', RUNSUFFIX )
writeData( iam_em_gridding_full, 'MED_OUT', out_filename, meta = F )
# END
logStop()
