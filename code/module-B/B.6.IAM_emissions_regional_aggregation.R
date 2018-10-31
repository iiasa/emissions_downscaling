# Copyright 2018 Battelle Memorial Institute

# ------------------------------------------------------------------------------
# Program Name: B.6.IAM_emissions_regional_aggregation.R
# Author(s): Leyang Feng
# Date Last Updated: Jun 15, 2017
# Program Purpose: The script aggregates downscaled country level emissions into
#                  regional level
# Input Files:  B.[iam_name]_[harm_status]_emissions_downscaled.csv
# Output Files: B.[iam_name]_[harm_status]_emissions_downscaled_[level].csv in final_out
# Notes:
# TODO:
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Must be run from the emissions_downscaling/input directory
if ( !endsWith( getwd(), '/input' ) ) setwd( 'input' )
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( 'module-A_functions.R', 'all_module_functions.R' )
log_msg <- "Aggregate downscaled emissions into regional level"
script_name <- "B.6.IAM_emissions_regional_aggregation.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'command_args' ) ) command_args <- commandArgs( TRUE )
iam <- command_args[ 1 ]
harm_status <- command_args[ 2 ]
modb_out <- command_args[ 3 ]
if ( is.na( iam ) ) iam <- "GCAM4"
if ( is.na( modb_out ) ) iam <- "../final-output/module-B"

MODULE_B <- "../code/module-B/"

region_level <- 'R5'

# ------------------------------------------------------------------------------
# 1. Read mapping files and axtract iam info
# read in master config file
master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list
iam_info_list <- iamInfoExtract( master_config, iam )

agg_reg_mapping <- readData( 'MAPPINGS', 'REMIND_CEDS_region_mapping' )

# -----------------------------------------------------------------------------
# 2. Read downscaled IAM emissions
iam_em <- readData( domain = 'MODB_OUT', file_name = paste0( 'B.', iam_name, '_', harm_status, '_emissions_downscaled' ) )

# -----------------------------------------------------------------------------
# 3. Aggregate country level IAM emissions to regional level
iam_col_headers <- grep( '^X', colnames( iam_em ), value = T, invert = T )
iam_x_years <- grep( '^X', colnames( iam_em ), value = T )
iam_em_reg <- merge( iam_em, agg_reg_mapping[ , c( 'iso', region_level ) ],
                     by.x = 'iso', by.y = 'iso' )
iam_em_reg$X2015 <- ifelse( iam_em_reg$X2015 == 'NA', NA, iam_em_reg$X2015 )
iam_em_reg$X2015 <- as.numeric( iam_em_reg$X2015 )
iam_em_reg <- aggregate( iam_em_reg[ , c( iam_x_years ) ],
                         by  = list( iam_em_reg$model, iam_em_reg$scenario, iam_em_reg$variable, iam_em_reg[ , region_level ], iam_em_reg$unit ),
                         FUN = sum, na.rm = T )
colnames( iam_em_reg ) <- c( 'model', 'scenario', 'variable', 'region', 'unit', iam_x_years )

# ------------------------------------------------------------------------------
# 4. Write out
out_filename <- paste0( 'B.', iam_name, '_', harm_status, '_emissions_downscaled_', region_level )
writeData( iam_em_reg, 'MODB_OUT', out_filename, meta = F )

# END
logStop()
