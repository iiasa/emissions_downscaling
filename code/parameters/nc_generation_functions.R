#------------------------------------------------------------------------------
# Program Name: nc_generation_functions.R
# Author's Name: Leyang Feng
# Date Last Modified: June 30 2016
# Program Purpose: NetCDF generation related functions for gridding routine.  
# Note: 
# TODO: 
# 
# ------------------------------------------------------------------------------

# Special Packages
loadPackage( 'ncdf4' ) 
loadPackage( 'sp' )
loadPackage( 'geosphere' )

# =====================================================================
# Annual grids generation functions
# -------------------------------------------------
# generate_final_grids_nc
# Brief: generate annual nc grids for bulk emissions
# Dependencies: 
# Author: Leyang Feng
# parameters: int_grids_list - the list contains intermediate grids 
#             output_dir - the output dir
#             grid_resolution - the gridding resolution 
#             year - the current gridding year 
#             em - the gridding emission species
#             sector_name_mapping - the mapping contains final sectors short names and long names 
#             seasonality_mapping  - the seasonality mapping file   
# return: null 
# input files: null
# output: CEDS_[em]_anthro_[year]_0.5_[CEDS_version].nc
#         CEDS_[em]_anthro_[year]_0.5_[CEDS_version].csv
generate_final_grids_nc <- function( int_grids_list,
                                     output_dir, 
                                     grid_resolution, 
                                     year, 
                                     em, 
                                     sector_name_mapping,
                                     seasonality_mapping ) {
  
  # 0 set up basics
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000000 / global_grid_area / ( 365 * 24 * 60 * 60 ) # from MT to kg m-2 s-1
  days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  # 1 extract int grids in the list
  AGR_proc_grid <- int_grids_list$AGR_int_grid
  AWB_proc_grid <- int_grids_list$AWB_int_grid
  ENE_proc_grid <- int_grids_list$ENE_int_grid
  FRTB_proc_grid <- int_grids_list$FRTB_int_grid
  GRSB_proc_grid <- int_grids_list$GRSB_int_grid 
  IND_proc_grid <- int_grids_list$IND_int_grid
  PEAT_proc_grid <- int_grids_list$PEAT_int_grid
  RCO_proc_grid <- int_grids_list$RCO_int_grid
  SHP_proc_grid <- int_grids_list$SHP_int_grid
  SLV_proc_grid <- int_grids_list$SLV_int_grid
  TRA_proc_grid <- int_grids_list$TRA_int_grid
  WST_proc_grid <- int_grids_list$WST_int_grid
  
  # 2 add seasonality for each final grids
  AGR_fin_grid  <- add_seasonality( AGR_proc_grid, em, 'AGR', year, days_in_month, grid_resolution, seasonality_mapping )
  AWB_fin_grid  <- add_seasonality( AWB_proc_grid, em, 'AWB', year, days_in_month, grid_resolution, seasonality_mapping )
  ENE_fin_grid  <- add_seasonality( ENE_proc_grid, em, 'ENE', year, days_in_month, grid_resolution, seasonality_mapping )
  FRTB_fin_grid <- add_seasonality( FRTB_proc_grid, em, 'FRTB', year, days_in_month, grid_resolution, seasonality_mapping )
  GRSB_fin_grid <- add_seasonality( GRSB_proc_grid, em, 'GRSB', year, days_in_month, grid_resolution, seasonality_mapping )
  IND_fin_grid  <- add_seasonality( IND_proc_grid, em, 'IND', year, days_in_month, grid_resolution, seasonality_mapping )
  PEAT_fin_grid <- add_seasonality( PEAT_proc_grid, em, 'PEAT', year, days_in_month, grid_resolution, seasonality_mapping )
  RCO_fin_grid  <- add_seasonality( RCO_proc_grid, em, 'RCO', year, days_in_month, grid_resolution, seasonality_mapping )
  SHP_fin_grid  <- add_seasonality( SHP_proc_grid, em, 'SHP', year, days_in_month, grid_resolution, seasonality_mapping )
  SLV_fin_grid  <- add_seasonality( SLV_proc_grid, em, 'SLV', year, days_in_month, grid_resolution, seasonality_mapping )
  TRA_fin_grid  <- add_seasonality( TRA_proc_grid, em, 'TRA', year, days_in_month, grid_resolution, seasonality_mapping )
  WST_fin_grid  <- add_seasonality( WST_proc_grid, em, 'WST', year, days_in_month, grid_resolution, seasonality_mapping )
  
  # 3 calculate checksum values 
  AGR_month_em <- sum_monthly_em( AGR_fin_grid, em, 'AGR', year, days_in_month, global_grid_area, seasonality_mapping )
  AWB_month_em <- sum_monthly_em( AWB_fin_grid, em, 'AWB', year, days_in_month, global_grid_area, seasonality_mapping )
  ENE_month_em <- sum_monthly_em( ENE_fin_grid, em, 'ENE', year, days_in_month, global_grid_area, seasonality_mapping )
  FRTB_month_em <- sum_monthly_em( FRTB_fin_grid, em, 'FRTB', year, days_in_month, global_grid_area, seasonality_mapping )
  GRSB_month_em <- sum_monthly_em( GRSB_fin_grid, em, 'GRSB', year, days_in_month, global_grid_area, seasonality_mapping )
  IND_month_em <- sum_monthly_em( IND_fin_grid, em, 'IND', year, days_in_month, global_grid_area, seasonality_mapping )
  PEAT_month_em <- sum_monthly_em( PEAT_fin_grid, em, 'PEAT', year, days_in_month, global_grid_area, seasonality_mapping )
  RCO_month_em <- sum_monthly_em( RCO_fin_grid, em, 'RCO', year, days_in_month, global_grid_area, seasonality_mapping )
  SHP_month_em <- sum_monthly_em( SHP_fin_grid, em, 'SHP', year, days_in_month, global_grid_area, seasonality_mapping )
  SLV_month_em <- sum_monthly_em( SLV_fin_grid, em, 'SLV', year, days_in_month, global_grid_area, seasonality_mapping )
  TRA_month_em <- sum_monthly_em( TRA_fin_grid, em, 'TRA', year, days_in_month, global_grid_area, seasonality_mapping )
  WST_month_em <- sum_monthly_em( WST_fin_grid, em, 'WST', year, days_in_month, global_grid_area, seasonality_mapping )
  
  total_month_em <- rbind( AGR_month_em, AWB_month_em, ENE_month_em, IND_month_em, FRTB_month_em, 
                           GRSB_month_em, IND_month_em, PEAT_month_em, RCO_month_em, SHP_month_em,
                           SLV_month_em, TRA_month_em, WST_month_em )
  
  # NetCDF generation starts here
  fin_sector_list <- c( 'AGR', 'AWB', 'FRTB', 'GRSB', 'PEAT', 'IND', 'RCO', 'TRA', 'SHP', 'ENE', 'SLV', 'WST' )
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  #base_days <- as.numeric( strptime( paste0( year, '0101' ), format = '%Y%m%d', tz = 'UTC' ) - strptime( "17500101", format = "%Y%m%d", tz = 'UTC' ) )
  base_days <- ( year - 1750 ) * 365 
  time <- floor( c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 ) )
  time <- time + base_days 
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", paste0( "days since 2015-01-01 0:0:0" ), as.double( time ),  
                        calendar = '365_day', longname = 'time' )
  dim_list <- list( londim, latdim, timedim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )
  time_bnds_data <- cbind( c( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ),
                      c( 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ) )
  time_bnds_data <- time_bnds_data + base_days				  
  
  data_unit <- 'kg m-2 s-1'  
  missing_value <- 1.e20
  
  # define nc variables
  AGR <- ncvar_def( 'AGR', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'AGR', 'sector_name' ], prec = 'float', compression = 5 )
  AWB <- ncvar_def( 'AWB', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'AWB', 'sector_name' ], prec = 'float', compression = 5 )
  ENE <- ncvar_def( 'ENE', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'ENE', 'sector_name' ], prec = 'float', compression = 5 )
  FRTB <- ncvar_def( 'FRTB', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'FRTB', 'sector_name' ], prec = 'float', compression = 5 )
  GRSB <- ncvar_def( 'GRSB', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'GRSB', 'sector_name' ], prec = 'float', compression = 5 )
  IND <- ncvar_def( 'IND', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'IND', 'sector_name' ], prec = 'float', compression = 5 )
  PEAT <- ncvar_def( 'PEAT', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'PEAT', 'sector_name' ], prec = 'float', compression = 5 )
  RCO <- ncvar_def( 'RCO', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'RCO', 'sector_name' ], prec = 'float', compression = 5 )
  SHP <- ncvar_def( 'SHP', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'SHP', 'sector_name' ], prec = 'float', compression = 5 )
  SLV <- ncvar_def( 'SLV', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'SLV', 'sector_name' ], prec = 'float', compression = 5 )
  TRA <- ncvar_def( 'TRA', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'TRA', 'sector_name' ], prec = 'float', compression = 5 )
  WST <- ncvar_def( 'WST', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$sector_short == 'WST', 'sector_name' ], prec = 'float', compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  # object version stamp is from global environment
  nc_file_name <- paste0( iam, '_', em, '_', sce, '_', year, '_placeholder', '.nc' ) 
  
  # generate the var_list 
  variable_list <- list( AGR, AWB, ENE, FRTB, GRSB, IND, PEAT, RCO, SHP, SLV, TRA, WST, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create( paste0( output_dir, '/',nc_file_name ), variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
    ncvar_put( nc_new, AGR, t( flip_a_matrix( AGR_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, AWB, t( flip_a_matrix( AWB_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, ENE, t( flip_a_matrix( ENE_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, FRTB, t( flip_a_matrix( FRTB_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, GRSB, t( flip_a_matrix( GRSB_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, IND, t( flip_a_matrix( IND_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, PEAT, t( flip_a_matrix( PEAT_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, RCO, t( flip_a_matrix( RCO_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, SHP, t( flip_a_matrix( SHP_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, SLV, t( flip_a_matrix( SLV_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, TRA, t( flip_a_matrix( TRA_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, WST, t( flip_a_matrix( WST_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  }
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )
  
  # nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "time", "standard_name", "time" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )
  # attributes for variables
  for ( each_var in fin_sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0( 'Annual Emissions of ', em, ' ', iam, ' ', sce  ) )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'xx' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', 'xxx' ) 
  #ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', 
  #                                        as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ),
  #                                        ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', 'xxx' )
  #ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  #ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  #ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  #ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes of all oxidized fossil carbon. CO2 from solid and liquid biofuel combustion is not included.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  #ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', 'xxx' )
  #ncatt_put( nc_new, 0, 'dataset_version_number', '1.0.0' )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  #ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'mip_era', 'xxx' )
  #ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
  ncatt_put( nc_new, 0, 'target_mip', 'xxx' )
  
  global_total_emission <- sum( total_month_em$value ) * 0.000001 
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Mt/year' ) )
  
  # species information 
  #reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  #info_line <- reporting_info[ reporting_info$em == em, 'info' ] 
  #ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
  # close nc_new
  nc_close( nc_new)
  
  # additional section: write a summary and check text
  summary_name <- paste0( output_dir, '/', iam, '_', em, '_', sce, '_', year, '_placeholder', '.csv' ) 
  write.csv( total_month_em, file = summary_name, row.names = F )
}

# -------------------------------------------------
# generate_final_grids_nc_aircraft
# Brief: generate annual nc grids for aircraft emissions
# Dependencies: 
# Author: Leyang Feng
# parameters: AIR_em_global - aircraft annual grid   
#             output_dir - the output dir
#             grid_resolution - the gridding resolution 
#             year - the current gridding year 
#             em - the gridding emission species
#             sector_name_mapping - the mapping contains final sectors short names 
# return:  
# input files: 
# output: 
generate_final_grids_nc_aircraft <- function( AIR_em_global,
                                     output_dir, 
                                     grid_resolution, 
                                     year, 
                                     em, 
                                     seasonality_mapping ) {
  
  # 0 
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  # 1
  AIR_proc_grid <- AIR_em_global
  
  # 2
  AIR_fin_grid <- add_seasonality( AIR_proc_grid, em, 'AIR', year, days_in_month, grid_resolution, seasonality_mapping ) 
  
  # 3
  AIR_month_em <- sum_monthly_em( AIR_fin_grid, em, 'AIR', year, days_in_month, global_grid_area, seasonality_mapping )

  total_month_em <- AIR_month_em
  
 # NetCDF generation starts here  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  levs <- seq( 0.305, 14.945, 0.61 )
  base_days <- ( year - 1750 ) * 365 
  time <- floor( c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 ) )
  time <- time + base_days 
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  levdim <- ncdim_def( "level", "km", as.double ( levs ), longname = 'altitude' ) 
  timedim <- ncdim_def( "time", paste0( "days since 1750-01-01 0:0:0" ), as.double( time ), 
                        calendar = '365_day', longname = 'time' )
  dim_list <- list( londim, latdim, levdim, timedim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )
  time_bnds_data <- cbind( c( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ),
                      c( 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ) )
  time_bnds_data <- time_bnds_data + base_days
  
  data_unit <- 'kg m-2 s-1'  
  missing_value <- 1.e20
  
  # define nc variables
  
  AIR <- ncvar_def( 'AIR', data_unit, dim_list, missval = missing_value, longname = 'Aircraft', prec = 'float', compression = 5  )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  nc_file_name <- paste0( 'CEDS_', em, '_AIR_anthro_', year, '_', grid_resolution, '_', version_stamp, '.nc' ) 
  
  # generate the var_list 
  variable_list <- list( AIR, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create(  paste0( output_dir, nc_file_name ), variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  # transpose and flip the data first 
  air_dim <- dim( AIR_fin_grid )
  AIR_array <- array( dim = c( air_dim[ 2 ], air_dim[ 1 ], air_dim[ 3 ], air_dim[ 4 ] ) )
  for ( i in 1 : air_dim[ 4 ] ) {
    for ( j in 1 : air_dim[ 3 ] ) {
	  AIR_array[ , , j, i ] <- t( flip_a_matrix( AIR_fin_grid[ , , j, i ] ) ) 
	}
  }
  # then put the data into nc 
  ncvar_put( nc_new, AIR, AIR_array )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )
  
  # nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "time", "standard_name", "time" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )
  ncatt_put( nc_new, "level", "axis", "Z" )
  # attributes for variables
  ncatt_put( nc_new, 'AIR', 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, 'AIR', 'missing_value', 1e+20, prec = 'float' )
  # nc global attributes
  ncatt_put( nc_new, 0, 'title', paste0('Annual Aircraft Emissions of ', em ) )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', 
                                          as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ),
                                          ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes of all oxidized fossil carbon. CO2 from solid and liquid biofuel combustion is not included.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', '1.0.0' )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
  
  global_total_emission <- sum( total_month_em$value ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )
  
  # species information 
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ] 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
  # close nc_new
  nc_close( nc_new)
  
  # additional section: write a summary and check text
  summary_name <- paste0( output_dir, 'CEDS_', em, '_AIR_anthro_', year, '_', grid_resolution, '_', version_stamp, '.csv' )
  write.csv( total_month_em, file = summary_name, row.names = F )
}

# =====================================================================
# Diagnostic grids generation functions
# -------------------------------------------------
# generate_annual_total_emissions_grids_nc
# Brief: generate total emissions grids without seasonality 
# Dependencies: 
# Author: Leyang Feng
# parameters: output_dir
#             int_grids_list - the list contains intermediate grids  
#             grid_resolution 
#             year 
#             em 
# return: null
# input files: null
# output: CEDS_[em]_anthro_[year]_TOTAL_0.5_[CEDS_version].nc
#         CEDS_[em]_anthro_[year]_TOTAL_0.5_[CEDS_version].csv

generate_annual_total_emissions_grids_nc <- function( output_dir, int_grids_list, grid_resolution, year, em ) {
  
  # 0 set up some basics for later use
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
    
  # 1 adding all sector's grid together so generate total emission grid 
  
  # convert flux back to mass 
  total_grid_mass <- lapply( int_grids_list, '/', flux_factor )
  total_grid_mass <- Reduce( '+', total_grid_mass )
  
  total_sum_in_kt <- sum( total_grid_mass )
  
  total_grid_flux <- total_grid_mass * flux_factor 
  
  # generate the nc file 
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  dim_list <- list( londim, latdim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )

  data_unit <- 'kg m-2 s-1'  
  
  # define nc variables
  missing_value <- 1.e20
  long_name <- 'Global total emissions '
  total_emission <- ncvar_def( 'total_emission', data_unit, dim_list, missval = missing_value, longname = long_name , prec = 'float', compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  
  # generate nc file name
  nc_file_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', 'TOTAL', '_', grid_resolution, '_', version_stamp, '.nc' ) 
  
  # generate the var_list 
  variable_list <- list( total_emission, lat_bnds, lon_bnds ) 

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  ncvar_put( nc_new, total_emission, t( flip_a_matrix( total_grid_flux ) ) )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )

  
  # nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  # attributes for variables
  ncatt_put( nc_new, total_emission, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, total_emission, 'missing_value', 1e+20, prec = 'float' )
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em ) )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', 
                                          as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ),
                                          ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  #ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  #ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes of all oxidized fossil carbon. CO2 from solid and liquid biofuel combustion is not included.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', '1.0.0' )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )

  global_total_emission <- total_sum_in_kt * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )
  # species information 
  species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- species_info[ species_info$species == em, ]$info 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
    
  # close nc_new
  nc_close( nc_new)
 
  # additional section: write a summary and check text

  summary_table <- data.frame( year = year, em = em,  
                               global_total = total_sum_in_kt,
                               unit = 'kt', stringsAsFactors = F )

  summary_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', 'TOTAL', '_', grid_resolution, '_', version_stamp, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}

# -------------------------------------------------
# generate_monthly_total_emissions_grids_nc
# Brief: generate total emissions grids with seasonality 
# Dependencies: 
# Author: Leyang Feng
# parameters: output_dir
#             int_grids_list - the list contains intermediate grids  
#             grid_resolution 
#             year 
#             em 
#             seasoanlity_mapping - seasonality mapping file 
# return: null 
# input files: null
# output: CEDS_[em]_anthro_[year]_TOTAL_monthly_[CEDS_version].nc
#         CEDS_[em]_anthro_[year]_TOTAL_monthly_[CEDS_version].csv
generate_monthly_total_emissions_grids_nc <- function( output_dir, 
                                                      int_grids_list, 
                                                      grid_resolution, 
                                                      year, 
                                                      em,
                                                      seasonality_mapping ) {
  
  # 0 
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  # 1
  AGR_proc_grid <- int_grids_list$AGR_int_grid
  ENE_proc_grid <- int_grids_list$ELEC_int_grid + int_grids_list$ETRN_int_grid + int_grids_list$FFFI_int_grid + int_grids_list$FLR_int_grid
  IND_proc_grid <- int_grids_list$INDC_int_grid + int_grids_list$INPU_int_grid
  TRA_proc_grid <- int_grids_list$NRTR_int_grid + int_grids_list$ROAD_int_grid
  RCORC_proc_grid <- int_grids_list$RCORC_int_grid
  RCOO_proc_grid <- int_grids_list$RCOO_int_grid
  SLV_proc_grid <- int_grids_list$SLV_int_grid
  WST_proc_grid <- int_grids_list$WST_int_grid
  SHP_proc_grid <- int_grids_list$SHP_int_grid + int_grids_list$TANK_int_grid
  
  # 2
  AGR_fin_grid <- add_seasonality( AGR_proc_grid, em, 'AGR', year, days_in_month, grid_resolution, seasonality_mapping ) 
  ENE_fin_grid <- add_seasonality( ENE_proc_grid, em, 'ENE', year, days_in_month, grid_resolution, seasonality_mapping ) 
  IND_fin_grid <- add_seasonality( IND_proc_grid, em, 'IND', year, days_in_month, grid_resolution, seasonality_mapping ) 
  TRA_fin_grid <- add_seasonality( TRA_proc_grid, em, 'TRA', year, days_in_month, grid_resolution, seasonality_mapping ) 
  SLV_fin_grid <- add_seasonality( SLV_proc_grid, em, 'SLV', year, days_in_month, grid_resolution, seasonality_mapping ) 
  WST_fin_grid <- add_seasonality( WST_proc_grid, em, 'WST', year, days_in_month, grid_resolution, seasonality_mapping ) 
  SHP_fin_grid <- add_seasonality( SHP_proc_grid, em, 'SHP', year, days_in_month, grid_resolution, seasonality_mapping ) 
  RCORC_fin_grid <- add_seasonality( RCORC_proc_grid, em, 'RCORC', year, days_in_month, grid_resolution, seasonality_mapping ) 
  RCOO_fin_grid <- add_seasonality( RCOO_proc_grid, em, 'RCOO', year, days_in_month, grid_resolution, seasonality_mapping ) 
  
  RCO_fin_grid <- RCORC_fin_grid + RCOO_fin_grid
  
  # 3
  AGR_month_em <- sum_monthly_em( AGR_fin_grid, em, 'AGR', year, days_in_month, global_grid_area, seasonality_mapping )
  ENE_month_em <- sum_monthly_em( ENE_fin_grid, em, 'ENE', year, days_in_month, global_grid_area, seasonality_mapping )
  IND_month_em <- sum_monthly_em( IND_fin_grid, em, 'IND', year, days_in_month, global_grid_area, seasonality_mapping )
  TRA_month_em <- sum_monthly_em( TRA_fin_grid, em, 'TRA', year, days_in_month, global_grid_area, seasonality_mapping )
  SLV_month_em <- sum_monthly_em( SLV_fin_grid, em, 'SLV', year, days_in_month, global_grid_area, seasonality_mapping )
  WST_month_em <- sum_monthly_em( WST_fin_grid, em, 'WST', year, days_in_month, global_grid_area, seasonality_mapping )
  SHP_month_em <- sum_monthly_em( SHP_fin_grid, em, 'SHP', year, days_in_month, global_grid_area, seasonality_mapping )
  RCORC_month_em <- sum_monthly_em( RCORC_fin_grid, em, 'RCORC', year, days_in_month, global_grid_area, seasonality_mapping )
  RCOO_month_em <- sum_monthly_em( RCOO_fin_grid, em, 'RCOO', year, days_in_month, global_grid_area, seasonality_mapping )
  
  RCO_month_em <- data.frame( em = em, sector = 'RCO', year = year, month = 1 : 12, units = 'kt', 
                              value = RCORC_month_em$value + RCOO_month_em$value, stringsAsFactors = F )
  total_month_em <- rbind( AGR_month_em, ENE_month_em, IND_month_em, TRA_month_em, 
                           SLV_month_em, WST_month_em, SHP_month_em, RCO_month_em )
  total_month_em <- aggregate( total_month_em$value, 
                               by = list( total_month_em$em, total_month_em$year, 
                                          total_month_em$month, total_month_em$units ), 
                               FUN = sum ) 
  
  total_grid_month <- AGR_fin_grid + ENE_fin_grid + IND_fin_grid + TRA_fin_grid + 
    SLV_fin_grid + WST_fin_grid + SHP_fin_grid + RCO_fin_grid 
  
  # generate the nc file 
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  base_days <- ( year - 1750 ) * 365 
  time <- floor( c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 ) )
  time <- time + base_days 
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", paste0( "days since 1750-01-01 0:0:0" ), as.double( time ),  
                        calendar = '365_day', longname = 'time' )
  dim_list <- list( londim, latdim, timedim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )
  time_bnds_data <- cbind( c( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ),
                      c( 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ) )
  time_bnds_data <- time_bnds_data + base_days				  
  
  data_unit <- 'kg m-2 s-1'  
  missing_value <- 1.e20
  
  # define nc variables
  long_name <- 'global monthly total emissions '
  total_emission <- ncvar_def( 'total_emission', data_unit, dim_list, missval = missing_value, longname = long_name , prec = 'float', compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  nc_file_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', 'TOTAL_monthly', '_', grid_resolution, '_', version_stamp, '.nc' ) 
  
  # generate the var_list 
  variable_list <- list( total_emission, lat_bnds, lon_bnds, time_bnds ) 

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
  ncvar_put( nc_new, total_emission, t( flip_a_matrix( total_grid_month[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  }
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )
  
  # nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "time", "standard_name", "time" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )
  # attributes for variables
  ncatt_put( nc_new, total_emission, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, total_emission, 'missing_value', 1e+20, prec = 'float' )
  # nc global attributes
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em ) )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', 
                                          as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ),
                                          ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  #ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  #ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes of all oxidized fossil carbon. CO2 from solid and liquid biofuel combustion is not included.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', '1.0.0' )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )

  global_total_emission <- sum( total_month_em$value ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )
  # species information 
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ] 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
    
  # close nc_new
  nc_close( nc_new)
 
  # additional section: write a summary and check text
  summary_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', 'TOTAL_monthly', '_', grid_resolution, '_', version_stamp, '.csv' )
  write.csv( total_month_em, file = summary_name, row.names = F )
}
