# ------------------------------------------------------------------------------
# Program Name: B.4.1.IAM_emissions_downscaling_linear.R
# Author(s): Leyang Feng
# Date Last Updated: Dec 07, 2016 
# Program Purpose: The script downscale emissions agriculture related emissions 
#                  using linear method 
# Input Files: CEDS_by_country_by_CEDS_sector_with_luc_all_em.csv 
#              B.[ref_name]_emissions_baseyear_linear 
# Output Files: B.[iam]_emissions_downscaled_linear.csv
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
log_msg <- "Downscale agriculture related emissions using linear method" 
script_name <- "B.4.1.IAM_emissions_downscaling_linear.R"

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

# extract target IAM and reference_em info from master mapping 
base_year <- as.numeric( base_year )
x_baseyear <- paste0( 'X', base_year )

# -----------------------------------------------------------------------------
# 2. Read IAM emissions, baseyear reference emissions, population data and GDP data 
iam_em <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_linear', '_', RUNSUFFIX ) )
ref_em <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', ref_name, '_emissions_baseyear_linear', '_', RUNSUFFIX ) )

# -----------------------------------------------------------------------------
# 3. add region information to ref_em 
ref_em_region_agg <- aggregate( ref_em[ , paste0( 'ctry_ref_em_X', base_year ) ], 
                                by = list( ref_em$em, ref_em$sector, ref_em$region ), 
                                FUN = sum ) 
colnames( ref_em_region_agg ) <- c( 'em', 'sector', 'region', paste0( 'reg_ref_em_X', base_year ) )

ref_em_reg <- merge( ref_em, ref_em_region_agg, by = c( 'em', 'sector', 'region' ) )
ref_em_reg[ , paste0( 'breakdown_X', base_year ) ] <- ref_em_reg[ , paste0( 'ctry_ref_em_X', base_year ) ] / ref_em_reg[ , paste0( 'reg_ref_em_X', base_year ) ]
ref_em_reg[ , paste0( 'breakdown_X', base_year ) ] <- ifelse( is.nan( ref_em_reg[ , paste0( 'breakdown_X', base_year ) ] ), 0, ref_em_reg[ , paste0( 'breakdown_X', base_year ) ] )

ref_em_breakdown <- ref_em_reg[ ,c( 'em', 'sector', 'region', 'iso', paste0( 'breakdown_X', base_year ) ) ]
  
# -----------------------------------------------------------------------------
# 4. downscaling
iam_em_header_cols <- grep( 'X', colnames( iam_em ), value = T, invert = T ) 

iam_em_ref <- merge( iam_em, ref_em_breakdown, by = c( 'em', 'sector', 'region', 'iso' ) )

iam_em_ref[ , paste0( 'X', ds_start_year : ds_end_year ) ] <- iam_em_ref[ , paste0( 'reg_iam_em_X', ds_start_year : ds_end_year ) ] * 
                                                                iam_em_ref[ , paste0( 'breakdown_X', base_year ) ]
ds_iam_em_agr <- iam_em_ref[ , c( iam_em_header_cols, paste0( 'X', ds_start_year : ds_end_year ) ) ]

# -----------------------------------------------------------------------------
# 5 Write out
# write baseyear reference emissions for aircraft and shipping sectors 
out_filname <- paste0( 'B.', iam, '_emissions_downscaled_linear', '_', RUNSUFFIX )
writeData( ds_iam_em_agr, 'MED_OUT', out_filname, meta = F )  

# END

