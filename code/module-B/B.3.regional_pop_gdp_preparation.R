# Copyright 2018 Battelle Memorial Institute

# ------------------------------------------------------------------------------
# Program Name: B.3.regional_pop_gdp_preparation.R
# Author(s): Leyang Feng
# Date Last Updated: Feb 13, 2017
# Program Purpose: The script reads in GDP and POP data in the input data then
#                  compute convergence year value for downscaling
# Input Files: iiasa_gdp.csv
#              iiasa_population.csv
# Output Files: B.iiasa_gdp_iso_[iam]_region.csv
#               B.iiasa_pop_iso_[iam]_region.csv
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
log_msg <- "Prepare GDP and POP data for downscaling"
script_name <- "B.3.regional_pop_gdp_preparation.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'command_args' ) ) command_args <- commandArgs( TRUE )
iam <- command_args[ 1 ]
if ( is.na( iam ) ) iam <- "GCAM4"

MODULE_B <- "../code/module-B/"

# ------------------------------------------------------------------------------
# 1. Read mapping files and axtract iam info
# read in master mapping file
master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list
iam_info_list <- iamInfoExtract( master_config, iam )

# extract target IAM and reference_em info from master mapping
x_baseyear <- paste0( 'X', base_year )

# read in ref sector mapping file
region_mapping <- readData( domain = 'MAPPINGS', file_name = ref_region_mapping )
con_year_mapping <- readData( domain = 'MAPPINGS', file_name = ds_convergence_year_mapping )

# -----------------------------------------------------------------------------
# 2. Read IAM_emissions and reference emission data
gdp_data <- readData( domain = 'SSP_IN', file_name = 'iiasa_gdp' )
pop_data <- readData( domain = 'SSP_IN', file_name = 'iiasa_population' )

# -----------------------------------------------------------------------------
# 3. Process pop data to have region inforamtion and value

pop_header_columns <- colnames( pop_data )[ grep( '^X', colnames( pop_data ), invert = T )  ]
pop_xyears <- colnames( pop_data )[ grep( '^X', colnames( pop_data ) )  ]

colnames( pop_data ) <- c( pop_header_columns, paste0( 'ctry_pop_', pop_xyears ) )

pop_data_region_added <- merge( pop_data, region_mapping[ , c( 'iso', 'region' ) ], by = 'iso' )
pop_region <- aggregate( pop_data_region_added[ , paste0( 'ctry_pop_', pop_xyears ) ],
                         by = list( pop_data_region_added$model, pop_data_region_added$scenario,
                                    pop_data_region_added$variable, pop_data_region_added$unit, pop_data_region_added$region ),
                         FUN = sum )
colnames( pop_region ) <- c( 'model', 'scenario', 'variable', 'unit', 'region', paste0( 'reg_pop_', pop_xyears ) )
pop_iso_region <- merge( pop_data_region_added, pop_region,
                         by = c( 'model', 'scenario', 'variable', 'unit', 'region' ),
                         all.x = T )

# -----------------------------------------------------------------------------
# 4. process gdp data
# -----------------------------------------------------------------------------
# 4.1 Process gdp data to have region inforamtion and value

gdp_header_columns <- colnames( gdp_data )[ grep( '^X', colnames( gdp_data ), invert = T )  ]
gdp_xyears <- colnames( gdp_data )[ grep( '^X', colnames( gdp_data ) )  ]

colnames( gdp_data ) <- c( gdp_header_columns, paste0( 'ctry_gdp_', gdp_xyears ) )

gdp_data_region_added <- merge( gdp_data, region_mapping[ , c( 'iso', 'region' ) ], by = 'iso' )
gdp_region <- aggregate( gdp_data_region_added[ , paste0( 'ctry_gdp_', gdp_xyears ) ],
                         by = list( gdp_data_region_added$scenario, gdp_data_region_added$variable, gdp_data_region_added$unit, gdp_data_region_added$region ),
                         FUN = sum )
colnames( gdp_region ) <- c( 'scenario', 'variable', 'unit', 'region', paste0( 'reg_gdp_', gdp_xyears ) )
gdp_iso_region <- merge( gdp_data_region_added, gdp_region,
                         by = c( 'scenario', 'variable', 'unit', 'region' ),
                         all.x = T )
# ------------------------------------------------------------------------------
# 4.2 Add convergence year gdp data
calculateConYear <- function( gdp_iso_region ) {

  # subset regional GDP data to 2090 & 2100
  calc_baseyears <- c( 2090, 2100 )
  gdp_header_cols <- grep( 'X', colnames( gdp_iso_region ), value = T, invert = T  )
  temp_df <- gdp_iso_region[ , c( gdp_header_cols, paste0( 'reg_gdp_X', calc_baseyears ) ) ]

  # loop over each scenarion in data
  temp_df_list <- lapply( unique( temp_df$scenario ), function( ssp ) {

    # subset to specific scenario
    ssp_df <- temp_df[ temp_df$scenario == ssp,  ]

    # grab ssp's convergence year
    con_year <- con_year_mapping[ con_year_mapping$scenario_label == ssp, 'convergence_year' ]

    # calculate GDP in convergence year
    ssp_df <- ssp_df %>%
      mutate(reg_gdp_Xcon_year = reg_gdp_X2100 * (reg_gdp_X2100 / reg_gdp_X2090) ^ ( (con_year - 2100) / 10 ),
             reg_gdp_Xcon_year = ifelse( is.nan( reg_gdp_Xcon_year ), 0, reg_gdp_Xcon_year ),
             reg_gdp_Xcon_year = ifelse( is.infinite( reg_gdp_Xcon_year ), 0, reg_gdp_Xcon_year ) )
    ssp_df <- ssp_df[ , c( gdp_header_cols, 'reg_gdp_Xcon_year' ) ]

    } )
  temp_df <- do.call( 'rbind', temp_df_list )
  gdp_iso_region <- merge( gdp_iso_region,
                         temp_df,
                         by = gdp_header_cols )
  return( gdp_iso_region )
}

gdp_iso_region<- calculateConYear( gdp_iso_region )

# -----------------------------------------------------------------------------
# 5. Write out
# write pop_iso_region
out_filname <- paste0( 'B.', 'iiasa_pop_iso_', iam, '_region', '_', RUNSUFFIX )
writeData( pop_iso_region, 'MED_OUT', out_filname, meta = F )

# write gdp_iso_region
out_filname <- paste0( 'B.', 'iiasa_gdp_iso_', iam, '_region', '_', RUNSUFFIX )
writeData( gdp_iso_region, 'MED_OUT', out_filname, meta = F )

# END
logStop()
