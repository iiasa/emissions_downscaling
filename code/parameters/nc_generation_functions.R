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
# -------------------------------------------------
# generate_bulk_grids_nc
# Brief: 
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return: null 
# input files: null
# output: 
generate_bulk_grids_nc <- function( allyear_grids_list, 
                                    output_dir, 
                                    grid_resolution, 
                                    year_list, 
                                    em, 
                                    sector_name_mapping,
                                    seasonality_mapping ) { 
  
  # ---
  # 0. Define some needed variables
  bulk_sectors <- c( "AGR", "ENE", "IND", "RCO", "SHP", "SLV", "TRA", "WST" )
  
  # ---
  # 1. Prepare data from writing
  # (1) emission array 
  year_data_list <- lapply( year_list, function( year ) { 
    
    current_year_grids_list <- allyear_grids_list[[ paste0( 'X', year ) ]] 
    current_year_sector_array <- array( dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                                 180 / grid_resolution, 
                                                 length( bulk_sectors ), length( year ) * 12 ) ) 
    for ( i in seq_along( bulk_sectors ) ) { 

      temp_array <- current_year_grids_list[[ bulk_sectors[ i ] ]]
      # flip each time slice of tmep_array 
      temp_array <- array( unlist( lapply( 1 : 12, function( i ) { t( flip_a_matrix( temp_array[ , , i ] ) ) } ) ), dim = c( 360 / grid_resolution, 180 / grid_resolution, 12 ) )
      
      current_year_sector_array[ , , i, ] <- temp_array
    }
    return( current_year_sector_array )
    } )
  
  em_array <- array( unlist( year_data_list ),  dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                                         180 / grid_resolution, 
                                                         length( bulk_sectors ), length( year_list ) * 12 ) ) 
  
  # (2) lons data and lon bound data  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  
  # (3) lats data and lat bound data 
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  
  # (4) time dimension data 
  month_middle_days <- floor( c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 ) )
  time_data <- c( )
  for ( year  in year_list ) { 
    base_days <- ( year - 2015 ) * 365 
    time_data <- c( time_data, month_middle_days + base_days )
    }
  
  # (5) time dimension bounds data 
  month_bnds_days <- cbind( c( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ),
                           c( 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ) )
  time_bnds_data <- matrix( ncol = 2 )
  for ( year  in year_list ) { 
    base_days <- ( year - 2015 ) * 365 
    time_bnds_data <- rbind( time_bnds_data, month_bnds_days + base_days )
  }
  time_bnds_data <- time_bnds_data[ 2 : nrow( time_bnds_data ), ]
  
  # (6) sector dimension data and bounds data 
  sectors <- 0 : ( length( bulk_sectors ) - 1 )
  sector_bnds_data <- cbind( seq( -0.5, 6.5, 1 ),
                             seq( 0.5, 7.5, 1 ) )

  # (7) upper lower bnds data 
  bnds <- 1 : 2
  
  # ---
  # 2. define nc dimensions 
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", paste0( "days since 2015-01-01 0:0:0" ), as.double( time_data ),  
                        calendar = '365_day', longname = 'time', unlim = T )
  sectordim <- ncdim_def( "sector", "", sectors, longname = 'sector' )
  
  dim_list <- list( londim, latdim, sectordim, timedim )
  
  bndsdim <- ncdim_def( 'bound', '', as.integer( bnds ), longname = 'bound', create_dimvar = F )
		  
  # ---
  # 3. generate nc file name and some variables
  FN_version_tag <- paste0( 'IAMC', '-', dataset_version_number )  
  MD_dataset_version_number_value <- dataset_version_number 
  MD_source_value <- 'IAMC Scenario Database hosted at IIASA'
  MD_source_id_value <- FN_version_tag
  FN_source_id_value <- MD_source_id_value
  FN_variable_id_value <- paste0( iam, '-', scenario, '-', harm_status, '-', em, '-em-anthro' )
  nc_file_name <- paste0( FN_variable_id_value, '_input4MIPs_emissions_CMIP_', FN_version_tag, '_gn_201501-210012.nc' )
  nc_file_name_w_path <- paste0( output_dir, '/', nc_file_name ) 
  
  # generate flat_var variable name 
  MD_variable_id_value <- FN_variable_id_value
  flat_var_name <- MD_variable_id_value 
  flat_var_longname <- flat_var_name
  
  # define unit and missing value 
  data_unit <- 'Mt m-2 s-1'  
  missing_value <- 1.e20
  
  # ---
  # 4. define nc variables  
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, longname = flat_var_longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  sector_bnds <- ncvar_def( 'sector_bnds', '', list( bndsdim, sectordim ), prec = 'double' )
  
  # ---
  # 5. generate the var_list
  variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds, sector_bnds ) 
  
  # ---
  # 6. create new nc file
  nc_new <- nc_create( nc_file_name_w_path, variable_list, force_v4 = T )
  
  # ---
  # 7. put nc variables into the nc file
  ncvar_put( nc_new, flat_var, em_array )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )
  ncvar_put( nc_new, sector_bnds, t( sector_bnds_data ) )
  
  # ---
  # 8. nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lon", "modulo", 360.0, prec = 'double' )
  ncatt_put( nc_new, "lon", "realtopology", "circular" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "topology", "circular" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "lat", "realtopology", "linear" ) 
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )
  ncatt_put( nc_new, "time", "realtopology", "linear" )
  ncatt_put( nc_new, "time", "standard_name", "time" )
  ncatt_put( nc_new, "sector", "bounds", "sector_bnds" )
  ncatt_put( nc_new, "sector", "ids", "0: AGR; 1: ENE; 2: IND; 3: RCO; 4: SHP; 5:SLV; 6: TRA; 7: WST" )
  # attributes for variables
  ncatt_put( nc_new, flat_var_name, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, flat_var_name, 'long_name', flat_var_longname )
  ncatt_put( nc_new, flat_var_name, 'missing_value', 1e+20, prec = 'float' )
  # nc global attributes
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )  
  ncatt_put( nc_new, 0, 'comment', 'Test and evaluation data release for SSP harmonized, gridded emissions. Data harmonized to historical emissions CEDS-v2017-05-18 (anthropogenic) and v1.2 (land-use change)' )
  ncatt_put( nc_new, 0, 'contact', 'Steven J. Smith (ssmith@pnnl.gov)' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )  
  ncatt_put( nc_new, 0, 'dataset_version_number', MD_dataset_version_number_value )
  ncatt_put( nc_new, 0, 'external_variables', 'gridcell_area' )  
  ncatt_put( nc_new, 0, 'frequency', 'mon' )  
  ncatt_put( nc_new, 0, 'further_info_url', 'https://secure.iiasa.ac.at/web-apps/ene/SspDb/' )  
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )  
  ncatt_put( nc_new, 0, 'grid_label', 'gn' )
  ncatt_put( nc_new, 0, 'nominal_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'history', paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ), '; College Park, MD, USA') )
  ncatt_put( nc_new, 0, 'institution', 'Gridded data generated at IIASA using codes developed at JGCRI' )
  ncatt_put( nc_new, 0, 'institution_id', 'IIASA' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'product', 'primary-emissions-data' )  
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'references', 'See: https://secure.iiasa.ac.at/web-apps/ene/SspDb/ for references' )
  ncatt_put( nc_new, 0, 'source', 'IAMC Scenario Database hosted at IIASA' )
  ncatt_put( nc_new, 0, 'source_id', MD_source_id_value )
  ncatt_put( nc_new, 0, 'table_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
  ncatt_put( nc_new, 0, 'title', paste0( 'Future Anthropogenic Emissions of ', em, ' prepared for input4MIPs' ) )
  ncatt_put( nc_new, 0, 'variable_id', MD_variable_id_value )
  
  # some other metadata
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes. Note that emissions are provided in uneven year intervals (2015, 2020, then at 10 year intervals) as these are the years for which projection data is available.' )
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ] 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
  # ---
  # 9. close nc_new
  nc_close( nc_new )
} 

# -------------------------------------------------
# generate_openburning_grids_nc
# Brief: 
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return: null 
# input files: null
# output: 
generate_openburning_grids_nc <- function( allyear_grids_list, 
                                           output_dir, 
                                           grid_resolution, 
                                           year_list, 
                                           em, 
                                           sector_name_mapping,
                                           seasonality_mapping ) { 
  
  # ---
  # 0. Define some needed variables
  openburning_sectors <- c( "AWB", "FRTB", "GRSB", "PEAT" )
  
  # ---
  # 1. Prepare data from writing
  # (1) emission array 
  year_data_list <- lapply( year_list, function( year ) { 
    
    current_year_grids_list <- allyear_grids_list[[ paste0( 'X', year ) ]] 
    current_year_sector_array <- array( dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                                 180 / grid_resolution, 
                                                 length( openburning_sectors ), length( year ) * 12 ) ) 
    for ( i in seq_along( openburning_sectors ) ) { 
      
      temp_array <- current_year_grids_list[[ openburning_sectors[ i ] ]]
      # flip each time slice of tmep_array 
      temp_array <- array( unlist( lapply( 1 : 12, function( i ) { t( flip_a_matrix( temp_array[ , , i ] ) ) } ) ), dim = c( 360 / grid_resolution, 180 / grid_resolution, 12 ) )
      
      current_year_sector_array[ , , i, ] <- temp_array
    }
    return( current_year_sector_array )
  } )
  
  em_array <- array( unlist( year_data_list ),  dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                                         180 / grid_resolution, 
                                                         length( openburning_sectors ), length( year_list ) * 12 ) ) 
  
  # (2) lons data and lon bound data  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  
  # (3) lats data and lat bound data 
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  
  # (4) time dimension data 
  month_middle_days <- floor( c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 ) )
  time_data <- c( )
  for ( year  in year_list ) { 
    base_days <- ( year - 2015 ) * 365 
    time_data <- c( time_data, month_middle_days + base_days )
  }
  
  # (5) time dimension bounds data 
  month_bnds_days <- cbind( c( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ),
                            c( 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ) )
  time_bnds_data <- matrix( ncol = 2 )
  for ( year  in year_list ) { 
    base_days <- ( year - 2015 ) * 365 
    time_bnds_data <- rbind( time_bnds_data, month_bnds_days + base_days )
  }
  time_bnds_data <- time_bnds_data[ 2 : nrow( time_bnds_data ), ]
  
  # (6) sector dimension data and bounds data 
  sectors <- 0 : ( length( openburning_sectors ) - 1 ) 
  sector_bnds_data <- cbind( seq( -0.5, 2.5, 1 ),
                             seq( 0.5, 3.5, 1 ) )
  
  # (7) upper lower bnds data 
  bnds <- 1 : 2
  
  # ---
  # 2. define nc dimensions 
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", paste0( "days since 2015-01-01 0:0:0" ), as.double( time_data ),  
                        calendar = '365_day', longname = 'time', unlim = T )
  sectordim <- ncdim_def( "sector", "", sectors, longname = 'sector' )
  
  dim_list <- list( londim, latdim, sectordim, timedim )
  
  bndsdim <- ncdim_def( 'bound', '', as.integer( bnds ), longname = 'bound', create_dimvar = F )
  
  # ---
  # 3. generate nc file name and some variables
  FN_version_tag <- paste0( 'IAMC', '-', dataset_version_number )  
  MD_dataset_version_number_value <- dataset_version_number 
  MD_source_value <- 'IAMC Scenario Database hosted at IIASA'
  MD_source_id_value <- FN_version_tag
  FN_source_id_value <- MD_source_id_value
  FN_variable_id_value <- paste0( iam, '-', scenario, '-', harm_status, '-', em, '-em-openburning' )
  nc_file_name <- paste0( FN_variable_id_value, '_input4MIPs_emissions_CMIP_', FN_version_tag, '_gn_201501-210012.nc' )
  nc_file_name_w_path <- paste0( output_dir, '/', nc_file_name ) 
  
  # generate flat_var variable name 
  MD_variable_id_value <- FN_variable_id_value
  flat_var_name <- MD_variable_id_value 
  flat_var_longname <- flat_var_name
  
  # define unit and missing value 
  data_unit <- 'Mt m-2 s-1'  
  missing_value <- 1.e20
  
  # ---
  # 4. define nc variables  
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, longname = flat_var_longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  sector_bnds <- ncvar_def( 'sector_bnds', '', list( bndsdim, sectordim ), prec = 'double' )
  
  # ---
  # 5. generate the var_list
  variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds, sector_bnds ) 
  
  # ---
  # 6. create new nc file
  nc_new <- nc_create( nc_file_name_w_path, variable_list, force_v4 = T )
  
  # ---
  # 7. put nc variables into the nc file
  ncvar_put( nc_new, flat_var, em_array )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )
  ncvar_put( nc_new, sector_bnds, t( sector_bnds_data ) )
  
  # ---
  # 8. nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lon", "modulo", 360.0, prec = 'double' )
  ncatt_put( nc_new, "lon", "realtopology", "circular" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "topology", "circular" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "lat", "realtopology", "linear" ) 
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )
  ncatt_put( nc_new, "time", "realtopology", "linear" )
  ncatt_put( nc_new, "time", "standard_name", "time" )
  ncatt_put( nc_new, "sector", "bounds", "sector_bnds" )
  ncatt_put( nc_new, "sector", "ids", "0: AWB; 1: FRTB; 2: GRSB; 3: PEAT" )
  # attributes for variables
  ncatt_put( nc_new, flat_var_name, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, flat_var_name, 'long_name', flat_var_longname )
  ncatt_put( nc_new, flat_var_name, 'missing_value', 1e+20, prec = 'float' )
  # nc global attributes
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )  
  ncatt_put( nc_new, 0, 'comment', 'Test and evaluation data release for SSP harmonized, gridded emissions. Data harmonized to historical emissions CEDS-v2017-05-18 (anthropogenic) and v1.2 (land-use change)' )
  ncatt_put( nc_new, 0, 'contact', 'Steven J. Smith (ssmith@pnnl.gov)' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )  
  ncatt_put( nc_new, 0, 'dataset_version_number', MD_dataset_version_number_value )
  ncatt_put( nc_new, 0, 'external_variables', 'gridcell_area' )  
  ncatt_put( nc_new, 0, 'frequency', 'mon' )  
  ncatt_put( nc_new, 0, 'further_info_url', 'https://secure.iiasa.ac.at/web-apps/ene/SspDb/' )  
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )  
  ncatt_put( nc_new, 0, 'grid_label', 'gn' )
  ncatt_put( nc_new, 0, 'nominal_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'history', paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ), '; College Park, MD, USA') )
  ncatt_put( nc_new, 0, 'institution', 'Gridded data generated at IIASA using codes developed at JGCRI' )
  ncatt_put( nc_new, 0, 'institution_id', 'IIASA' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'product', 'primary-emissions-data' )  
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'references', 'See: https://secure.iiasa.ac.at/web-apps/ene/SspDb/ for references' )
  ncatt_put( nc_new, 0, 'source', 'IAMC Scenario Database hosted at IIASA' )
  ncatt_put( nc_new, 0, 'source_id', MD_source_id_value )
  ncatt_put( nc_new, 0, 'table_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
  ncatt_put( nc_new, 0, 'title', paste0( 'Future Anthropogenic Emissions of ', em, ' prepared for input4MIPs' ) )
  ncatt_put( nc_new, 0, 'variable_id', MD_variable_id_value )
  
  # some other metadata
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes. Note that emissions are provided in uneven year intervals (2015, 2020, then at 10 year intervals) as these are the years for which projection data is available.' )
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ] 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
  # ---
  # 9. close nc_new
  nc_close( nc_new )
} 

