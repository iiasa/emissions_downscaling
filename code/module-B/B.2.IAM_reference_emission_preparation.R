# ------------------------------------------------------------------------------
# Program Name: B.2.IAM_reference_emission_preparation.R
# Author(s): Leyang Feng
# Date Last Updated: Feb 13, 2017
# Program Purpose: The script Split IAM/reference emissions into separate 
#                  intermediate files based on downscaling method then compute
#                  convergence year IAM emissions 
# Input Files: B.[iam]_emissions_reformatted 
#              CEDS_by_country_by_CEDS_sector_with_luc_all_em.csv
# Output Files: B.[ref_name]_emissions_baseyear_nods
#               B.[ref_name]_emissions_baseyear_linear 
#               B.[ref_name]_emissions_baseyear_ipat 
#               B.[iam]_emissions_nods 
#               B.[iam]_emissions_linear 
#               B.[iam]_emissions_ipat 
# Notes: nods -- sectors do not need to be downscaled 
#        linear -- sectors will be downscaled using linear method 
#        ipat -- sectors will be downscaled uing ipat approach 
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
log_msg <- "Split IAM/reference emissions into separate intermediate files based on downscaling method then compute convergence year IAM emissions" 
script_name <- "B.2.IAM_reference_emission_preparation.R"

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
x_baseyear <- paste0( 'X', base_year )

# read in ref sector mapping file 
sector_mapping <- readData( domain = 'MAPPINGS', file_name = ref_sector_mapping ) 
region_mapping <- readData( domain = 'MAPPINGS', file_name = ref_region_mapping ) 
con_year_mapping <- readData( domain = 'MAPPINGS', file_name = ds_convergence_year_mapping )
con_year_mapping <- con_year_mapping[ con_year_mapping$model == iam, c( "scenario_label", "convergence_year" ) ]
method_mapping <- readData( domain = 'MAPPINGS', file_name = ds_method_mapping ) 

# -----------------------------------------------------------------------------
# 2. Read IAM_emissions and reference emission data 
iam_em <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_reformatted', '_', RUNSUFFIX ) )

ref_em <- readData( domain = 'REF_EM', file_name = 'CEDS_by_country_by_CEDS_sector_with_luc_all_em',
                    domain_extension = ref_domain_extension )

# -----------------------------------------------------------------------------
# 3. Process reference emissions
# extrac column name information from ref_em 
ref_em_header_columns <- colnames( ref_em )[ grep( '^X', colnames( ref_em ), invert = T )  ]
ref_em_xyear <- colnames( ref_em )[ grep( '^X', colnames( ref_em ) )  ]

# convert reference emissions into CEDS9 sectors if necessary 
if ( ds_sector_scheme == 'CEDS9' ) { 
  sector_level_mapping <- readData( 'MAPPINGS', 'IAMC_CEDS16_CEDS9' )
  ref_em <- merge( ref_em, sector_level_mapping, by.x = 'sector', by.y = 'CEDS16', all.x = T  )
  ref_em <- aggregate( ref_em[ , c( ref_em_xyear ) ], 
                       by = list( ref_em$iso, ref_em$CEDS9, ref_em$em, ref_em$unit ), 
                       FUN = sum )
  colnames( ref_em ) <- c( ref_em_header_columns, ref_em_xyear  )
  }

# -----------------------------------------------------------------------------
# 3.1 Pick out base year reference emissions and change column names

ref_em_baseyear <- ref_em[ , c( ref_em_header_columns, x_baseyear ) ]
colnames( ref_em_baseyear ) <- c( 'iso', 'sector', 'em', 'unit', paste0( 'ctry_ref_em_', x_baseyear ) )

# ----------------------------------------------------------------------------
# 3.2 Pick out no-downscaling sectors in reference emissions before adding region information 
sector_nodownscaling <- method_mapping[ method_mapping$downscale_method == 'none', 'sector' ]

ref_em_nods <- ref_em_baseyear[ ref_em_baseyear$sector %in% sector_nodownscaling, ]
colnames( ref_em_nods ) <- c( 'region', 'em', 'sector', 'unit', x_baseyear )
ref_em_nods$region <- 'World'

ref_em_baseyear <- ref_em_baseyear[ ref_em_baseyear$sector %!in% sector_nodownscaling, ]

# ----------------------------------------------------------------------------
# 3.3. Add region information to ref_em_baseyear
ref_em_baseyear$region <- region_mapping[ match( ref_em_baseyear$iso, region_mapping$iso ), 'region' ]
ref_em_baseyear <- ref_em_baseyear[ !is.na( ref_em_baseyear$region ), ]
ref_em_baseyear<- ref_em_baseyear[ , c( 'iso', 'em', 'sector', 'region', 'unit', paste0( 'ctry_ref_em_', x_baseyear ) ) ]

# -----------------------------------------------------------------------------
# 3.4. Pick out sectors that use linear method 
sector_linear <- method_mapping[ method_mapping$downscale_method == 'linear', 'sector' ]
ref_em_baseyear_linear <- ref_em_baseyear[ ref_em_baseyear$sector %in% sector_linear, ]

# -------------------------------------------------------------------------
# 3.5. Pick out sectors that use ipat method 
sector_ipat <- method_mapping[ method_mapping$downscale_method == 'ipat', 'sector' ]
ref_em_baseyear_ipat <- ref_em_baseyear[ ref_em_baseyear$sector %in% sector_ipat, ]

# -----------------------------------------------------------------------------
# 4. Process IAM emissions
iam_em_header_columns <- colnames( iam_em )[ grep( '^X', colnames( iam_em ), invert = T )  ]
iam_em_xyear <- colnames( iam_em )[ grep( '^X', colnames( iam_em ) )  ]

# -----------------------------------------------------------------------------
# 4.1 Pick out no-downscaling sectors in iam_em 
sector_nodownscaling <- method_mapping[ method_mapping$downscale_method == 'none', 'sector' ]
iam_em_nods <- iam_em[ iam_em$sector %in% sector_nodownscaling, ]
iam_em <- iam_em[ iam_em$sector %!in% sector_nodownscaling, ]

# -----------------------------------------------------------------------------
# 4.2 Add iso information to iam_em 
iam_em_iso_merge <- merge( iam_em, region_mapping, by.x = 'region', by.y = 'region' )
iam_em_iso_added <- iam_em_iso_merge[ c( iam_em_header_columns, 'iso', iam_em_xyear ) ]
colnames( iam_em_iso_added ) <- c( iam_em_header_columns, 'iso', paste0( 'reg_iam_em_', iam_em_xyear ) )

# -----------------------------------------------------------------------------
# 4.3 Pick out sectors that uses linear downscaling method
sector_linear <- method_mapping[ method_mapping$downscale_method == 'linear', 'sector' ]

iam_em_linear <- iam_em_iso_added[ iam_em_iso_added$sector %in% sector_linear, ]

# -----------------------------------------------------------------------------
# 4.4 Pick out sectors that uses ipat downscaling method
sector_ipat <- method_mapping[ method_mapping$downscale_method == 'ipat', 'sector' ]
iam_em_ipat <- iam_em_iso_added[ iam_em_iso_added$sector %in% sector_ipat, ]

# -----------------------------------------------------------------------------
# 4.5 Convergence year value calculation for energy relatd  
calculateCeonYear <- function( iam_em_ipat ) { 
  
  calc_baseyears <- c( 2090, 2100 )  
  iam_em_ipat_header_cols <- grep( 'X', colnames( iam_em_ipat ), value = T, invert = T  )
  temp_df <- iam_em_ipat[ , c( iam_em_ipat_header_cols, paste0( 'reg_iam_em_X', calc_baseyears ) ) ]
  temp_df$ssp_label <- unlist( lapply( strsplit( temp_df$scenario, '-' ), '[[', 1 ) ) 
  temp_df_list <- lapply( unique( temp_df$ssp_label ), function( ssp ) {
    ssp_df <- temp_df[ temp_df$ssp_label == ssp,  ]
    con_year <- con_year_mapping[ con_year_mapping$scenario_label == ssp, 'convergence_year' ]
    ssp_df$reg_iam_em_Xcon_year <- ssp_df[ , paste0( 'reg_iam_em_X', calc_baseyears[ 2 ] ) ] * ( ssp_df[ , paste0( 'reg_iam_em_X', calc_baseyears[ 2 ] ) ] / ssp_df[ , paste0( 'reg_iam_em_X', calc_baseyears[ 1 ] ) ] )^( ( con_year - calc_baseyears[ 2 ] ) / 10 )
    ssp_df$reg_iam_em_Xcon_year <- ifelse( is.nan( ssp_df$reg_iam_em_Xcon_year ), 0, ssp_df$reg_iam_em_Xcon_year )
    ssp_df$reg_iam_em_Xcon_year <- ifelse( is.infinite( ssp_df$reg_iam_em_Xcon_year ), 0, ssp_df$reg_iam_em_Xcon_year )
    ssp_df <- ssp_df[ , c( iam_em_ipat_header_cols, 'reg_iam_em_Xcon_year' ) ]
    } )
  temp_df <- do.call( 'rbind', temp_df_list )
  iam_em_ipat <- merge( iam_em_ipat, 
                        temp_df, 
                        by = iam_em_ipat_header_cols )
  return( iam_em_ipat )
}

iam_em_ipat <- calculateCeonYear( iam_em_ipat )

# -----------------------------------------------------------------------------
# 5 Write out
# write baseyear reference emissions for no-downscaling sectors 
out_filname <- paste0( 'B.', ref_name, '_emissions_baseyear_nods', '_', RUNSUFFIX )
writeData( ref_em_nods, 'MED_OUT', out_filname, meta = F )  

# write baseyear reference emissions for linear sectors 
out_filname <- paste0( 'B.', ref_name, '_emissions_baseyear_linear', '_', RUNSUFFIX )
writeData( ref_em_baseyear_linear, 'MED_OUT', out_filname, meta = F )  

# write baseyear reference emissions for ipat sectors 
out_filname <- paste0( 'B.', ref_name, '_emissions_baseyear_ipat', '_', RUNSUFFIX )
writeData( ref_em_baseyear_ipat, 'MED_OUT', out_filname, meta = F )  

# write IAM emissions for aircraft and shipping sectors 
out_filname <- paste0( 'B.', iam, '_emissions_nods', '_', RUNSUFFIX )
writeData( iam_em_nods, 'MED_OUT', out_filname, meta = F )  

# write IAM emissions for agriculture related sectors 
out_filname <- paste0( 'B.', iam, '_emissions_linear', '_', RUNSUFFIX )
writeData( iam_em_linear, 'MED_OUT', out_filname, meta = F ) 

# write IAM emissions for energy related sectors 
out_filname <- paste0( 'B.', iam, '_emissions_ipat', '_', RUNSUFFIX )
writeData( iam_em_ipat, 'MED_OUT', out_filname, meta = F )  

# END

