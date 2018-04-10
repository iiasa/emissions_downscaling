# ------------------------------------------------------------------------------
# Program Name: B.4.2.IAM_emissions_downscaling_ipat.R
# Author(s): Leyang Feng
# Date Last Updated: Feb 13, 2017
# Program Purpose: The script downscale energy related emissions using IPAT approach 
# Input Files: B.[ref_name]_emissions_baseyear_ipat.csv
#              CEDS_by_country_by_CEDS_sector_with_luc_all_em.csv  
# Output Files: B.[iam]_emissions_downscaled_ipat.csv
# Notes: 
# TODO: find a smarter way to remove NA in section 4 
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
script_name <- "B.4.2.IAM_emissions_downscaling_ipat.R"

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

con_year_mapping <- readData( domain = 'MAPPINGS', file_name = ds_convergence_year_mapping )
con_year_mapping <- con_year_mapping[ con_year_mapping$model == iam, c( "scenario_label", "convergence_year" ) ]

# -----------------------------------------------------------------------------
# 2. Read IAM emissions, baseyear reference emissions, population data and GDP data 
iam_em <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_ipat', '_', RUNSUFFIX ) )
ref_em <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', ref_name, '_emissions_baseyear_ipat', '_', RUNSUFFIX ) )
pop_data <- readData( domain = 'MED_OUT', file_name = paste0( 'B.iiasa_pop_iso_', iam, '_region', '_', RUNSUFFIX ) )
gdp_data <- readData( domain = 'MED_OUT', file_name = paste0( 'B.iiasa_gdp_iso_', iam, '_region', '_', RUNSUFFIX ) )

# -----------------------------------------------------------------------------
# 3. create short ssp label for iam_em 
iam_em$ssp_label <- unlist( lapply( strsplit( iam_em$scenario, '-' ), '[[', 1 ) )

iam_em_data_cols <- grep( 'X', colnames( iam_em ), value = T ) 
ref_em_data_cols <- grep( 'X', colnames( ref_em ), value = T ) 
pop_data_cols <- grep( 'X', colnames( pop_data ), value = T ) 
gdp_data_cols <- grep( 'X', colnames( gdp_data ), value = T ) 

# -----------------------------------------------------------------------------
# 4. Combine all input data into a wide df 
wide_df <- merge( iam_em, ref_em[ , c( 'em', 'sector', 'region', 'iso', ref_em_data_cols ) ], by = c( 'em', 'sector', 'region', 'iso' ), all.x = T  )
wide_df[, ref_em_data_cols ] <- ifelse( is.na( wide_df[, ref_em_data_cols ] ), 0, wide_df[, ref_em_data_cols ] )
wide_df <- merge( wide_df, pop_data[ , c( 'scenario', 'region', 'iso', pop_data_cols ) ], 
                  by.x = c( 'region', 'iso', 'ssp_label' ), 
                  by.y = c( 'region', 'iso', 'scenario' ), all.x = T  ) 
wide_df <- merge( wide_df, gdp_data[ , c( 'scenario', 'region', 'iso', gdp_data_cols ) ], 
                  by.x = c( 'region', 'iso', 'ssp_label' ), 
                  by.y = c( 'region', 'iso', 'scenario' ), all.x = T  ) 

# separate emissions that has 0 or less values in convergence year 
wide_df_pos_nonCO2 <- wide_df %>% 
  filter(reg_iam_em_Xcon_year > 0 & em != "CO2") # only positive emissions. no CO2
wide_df_CO2_or_neg_nonCO2 <- wide_df %>% 
  filter(reg_iam_em_Xcon_year <= 0 | em == "CO2") # either zero/negative or CO2

# -----------------------------------------------------------------------------
# 5. Downscaling
# 5.0 ipat downscaling functions
source("../code/module-B/downscaling_ipat_functions.R")

# 5.1 downscaling for non-CO2 emissions 
ds_df_pos_nonCO2 <- downscaleIAMemissions( wide_df_pos_nonCO2, con_year_mapping, pos_nonCO2 = TRUE )

# 5.2 downscaling for CO2 emissions 
ds_df_CO2_or_neg_nonCO2 <- downscaleIAMemissions( wide_df_CO2_or_neg_nonCO2, con_year_mapping, pos_nonCO2 = FALSE )

# 5.3 combine downscaled non-CO2 emissions and CO2 emissions 
ds_df <- rbind( ds_df_pos_nonCO2, ds_df_CO2_or_neg_nonCO2 )

# -----------------------------------------------------------------------------
# 5 Write out
# write baseyear reference emissions for aircraft and shipping sectors 
out_filename <- paste0( 'B.', iam, '_emissions_downscaled_ipat', '_', RUNSUFFIX )
writeData( ds_df, 'MED_OUT', out_filename, meta = F )  

# END
logStop()
