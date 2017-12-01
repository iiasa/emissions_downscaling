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

# seperate CO2 from wide_df 
wide_df_nonCO2 <- wide_df[ wide_df$em != 'CO2', ]
wide_df_CO2 <- wide_df[ wide_df$em == 'CO2', ]

# -----------------------------------------------------------------------------
# 5. Downscaling
# 5.1 downscaling for non-CO2 emissions 
downscaleIAMemissions_nonCO2 <- function( wide_df, con_year_mapping ) { 
    
    # set up two working df: parameter data frame and results data frame
    par_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit" ) ] 
    res_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", paste0( 'ctry_ref_em_X', base_year ) ) ] 
    
    # equation (1)
    par_df$EIRCY <- wide_df[ , 'reg_iam_em_Xcon_year' ] / wide_df[ , 'reg_gdp_Xcon_year' ]
    par_df$EICBY <- wide_df[ , paste0( 'ctry_ref_em_X', base_year ) ] / wide_df[ , paste0( 'ctry_gdp_X', base_year ) ]
    
    out_df_list <- lapply( unique( wide_df$ssp_label ), function( ssp ) { 
      par_df_ssp <- par_df[ par_df$ssp_label == ssp, ]
      res_df_ssp <- res_df[ res_df$ssp_label == ssp, ]
      wide_df_ssp <- wide_df[ wide_df$ssp_label == ssp, ]
      
      con_year <- con_year_mapping[ con_year_mapping$scenario_label == ssp, "convergence_year" ]
      
      par_df_ssp$EI_gr_C <- ( par_df_ssp$EIRCY / par_df_ssp$EICBY )^( 1 / ( con_year - base_year ) )   
      par_df_ssp$EI_gr_C <- ifelse( is.na( par_df_ssp$EI_gr_C ), 0, par_df_ssp$EI_gr_C  )
      par_df_ssp$EI_gr_C <- ifelse( is.infinite( par_df_ssp$EI_gr_C ), 0, par_df_ssp$EI_gr_C  )
      
      for ( year in ( base_year + 1 ) : ds_end_year ) { 
    
      # equation (2)
      par_df_ssp$EI_star <- res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / 
                            wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ] *
                            par_df_ssp$EI_gr_C
      par_df_ssp$E_star <- par_df_ssp$EI_star * wide_df_ssp[ , paste0( 'ctry_gdp_X', year ) ]
    
      # equation(3)
      temp_df_agg <- aggregate( par_df_ssp$E_star, 
                                by = list( wide_df_ssp$region, wide_df_ssp$ssp_label,
                                           wide_df_ssp$em, wide_df_ssp$sector, wide_df_ssp$scenario ), 
                                FUN=sum, na.rm = T )
      colnames( temp_df_agg ) <- c( 'region', 'ssp_label', 'em', 'sector', 'scenario', 'sum_E_star' )
    
      par_df_ssp$sum_E_star <- temp_df_agg[ match( paste( par_df_ssp$region, par_df_ssp$ssp_label, par_df_ssp$em, par_df_ssp$sector, par_df_ssp$scenario ), 
                                            paste( temp_df_agg$region, temp_df_agg$ssp_label, temp_df_agg$em, temp_df_agg$sector, temp_df_agg$scenario ) ), 
                                            'sum_E_star' ]
      par_df_ssp$DiffR <- wide_df_ssp[ , paste0( 'reg_iam_em_X', year ) ]  - par_df_ssp$sum_E_star
    
      # equation (5)
      par_df_ssp$E_share <- par_df_ssp$E_star / par_df_ssp$sum_E_star 
      par_df_ssp$E_share <- ifelse( is.na( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      par_df_ssp$E_share <- ifelse( is.infinite( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
    
      # equation (6)    
      par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$DiffR * par_df_ssp$E_share 
      #par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$E_star * par_df_ssp$DiffR * par_df_ssp$E_share 
      par_df_ssp$E_final <- ifelse( is.na( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      par_df_ssp$E_final <- ifelse( is.infinite( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      res_df_ssp[ , paste0( "ctry_ref_em_X", year ) ] <- par_df_ssp$E_final
      
    }
  
    out_df_ssp <- res_df_ssp[ , c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', 
                              paste0( 'ctry_ref_em_X', base_year : ds_end_year ) ) ]  
    colnames( out_df_ssp )  <- c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', paste0( 'X', base_year : ds_end_year ) )   
      
    return( out_df_ssp )
      } )
    
  out_df <- do.call( 'rbind', out_df_list )
    
  return( out_df )
}

ds_df_nonCO2 <- downscaleIAMemissions_nonCO2( wide_df_nonCO2, con_year_mapping )

# 5.2 downscaling for CO2 emissions 
downscaleIAMemissions_CO2 <- function( wide_df, con_year_mapping ) { 
  
  # set up two working df: parameter data frame and results data frame
  par_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit" ) ] 
  res_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", paste0( 'ctry_ref_em_X', base_year ) ) ] 
  
  # equation (1)
  par_df$EIRCY <- wide_df[ , 'reg_iam_em_Xcon_year' ] / wide_df[ , 'reg_gdp_Xcon_year' ]
  par_df$EICBY <- wide_df[ , paste0( 'ctry_ref_em_X', base_year ) ] / wide_df[ , paste0( 'ctry_gdp_X', base_year ) ]
  
  out_df_list <- lapply( unique( wide_df$ssp_label ), function( ssp ) { 
    par_df_ssp <- par_df[ par_df$ssp_label == ssp, ]
    res_df_ssp <- res_df[ res_df$ssp_label == ssp, ]
    wide_df_ssp <- wide_df[ wide_df$ssp_label == ssp, ]
    
    con_year <- con_year_mapping[ con_year_mapping$scenario_label == ssp, "convergence_year" ]
    
    par_df_ssp$EI_gr_C <- ( ( par_df_ssp$EIRCY - par_df_ssp$EICBY ) / par_df_ssp$EICBY ) / ( con_year - base_year ) 
    par_df_ssp$EI_gr_C <- ifelse( is.na( par_df_ssp$EI_gr_C ), 0, par_df_ssp$EI_gr_C  )
    par_df_ssp$EI_gr_C <- ifelse( is.infinite( par_df_ssp$EI_gr_C ), 0, par_df_ssp$EI_gr_C  )
    
    for ( year in ( base_year + 1 ) : ds_end_year ) { 
      
      # equation (2)
      par_df_ssp$EI_star <- ( res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / 
        wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ] ) +  
        ( res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / 
            wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ] ) * 
        par_df_ssp$EI_gr_C
      par_df_ssp$E_star <- par_df_ssp$EI_star * wide_df_ssp[ , paste0( 'ctry_gdp_X', year ) ]
      
      # equation(3)
      temp_df_agg <- aggregate( par_df_ssp$E_star, 
                                by = list( wide_df_ssp$region, wide_df_ssp$ssp_label,
                                           wide_df_ssp$em, wide_df_ssp$sector, wide_df_ssp$scenario ), 
                                FUN=sum, na.rm = T )
      colnames( temp_df_agg ) <- c( 'region', 'ssp_label', 'em', 'sector', 'scenario', 'sum_E_star' )
      
      par_df_ssp$sum_E_star <- temp_df_agg[ match( paste( par_df_ssp$region, par_df_ssp$ssp_label, par_df_ssp$em, par_df_ssp$sector, par_df_ssp$scenario ), 
                                                   paste( temp_df_agg$region, temp_df_agg$ssp_label, temp_df_agg$em, temp_df_agg$sector, temp_df_agg$scenario ) ), 
                                            'sum_E_star' ]
      par_df_ssp$DiffR <- wide_df_ssp[ , paste0( 'reg_iam_em_X', year ) ]  - par_df_ssp$sum_E_star
      
      # equation (5)
      par_df_ssp$E_share <- par_df_ssp$E_star / par_df_ssp$sum_E_star 
      par_df_ssp$E_share <- ifelse( is.na( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      par_df_ssp$E_share <- ifelse( is.infinite( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      
      # equation (6)    
      par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$DiffR * par_df_ssp$E_share 
      #par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$E_star * par_df_ssp$DiffR * par_df_ssp$E_share 
      par_df_ssp$E_final <- ifelse( is.na( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      par_df_ssp$E_final <- ifelse( is.infinite( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      res_df_ssp[ , paste0( "ctry_ref_em_X", year ) ] <- par_df_ssp$E_final
      
    }
    
    out_df_ssp <- res_df_ssp[ , c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', 
                                   paste0( 'ctry_ref_em_X', base_year : ds_end_year ) ) ]  
    colnames( out_df_ssp )  <- c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', paste0( 'X', base_year : ds_end_year ) )   
    
    return( out_df_ssp )
  } )
  
  out_df <- do.call( 'rbind', out_df_list )
  
  return( out_df )
}

ds_df_CO2 <- downscaleIAMemissions_CO2( wide_df_CO2, con_year_mapping )

# 5.3 combine downscaled non-CO2 emissions and CO2 emissions 
ds_df <- rbind( ds_df_nonCO2, ds_df_CO2 )

# -----------------------------------------------------------------------------
# 5 Write out
# write baseyear reference emissions for aircraft and shipping sectors 
out_filename <- paste0( 'B.', iam, '_emissions_downscaled_ipat', '_', RUNSUFFIX )
writeData( ds_df, 'MED_OUT', out_filename, meta = F )  

# END
logStop()
