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
library( 'ncdf4' )
library( 'sp' )
library( 'geosphere' )

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
                                    em ) {

  # ---
  # 0. Define variables specific to anthro
  #    Order matters, so make sure bulk_sectors and sector_ids are in correct positions
  bulk_sectors <- c( "AGR", "ENE", "IND", "TRA", "RCO", "SLV", "WST", "SHP" )
  sector_ids <- "0: Agriculture; 1: Energy; 2: Industrial; 3: Transportation; 4: Residential, Commercial, Other; 5: Solvents production and application; 6: Waste; 7: International Shipping"
  sector_type <- "anthro"

  if ( !any( names( allyear_grids_list[[1]] ) %in% bulk_sectors ) ) {
    warning( paste( "No bulk sectors found for", em, "in", scenario ) )
    return( invisible( NULL ) )
  }

  # 1. Build and write out netCDF file
  #    Returns: diag_cells - diagnostic cells list
  #             out_name - output file name
  diagnostics <- build_ncdf( allyear_grids_list, output_dir, grid_resolution,
                             year_list, em, bulk_sectors, sector_type,
                             sector_ids )

  # 2. Create diagnostic cells plots
  diagnostic_cells <- do.call( 'rbind', diagnostics$diag_cells )
  out_name <- gsub( '.nc', '_cells', diagnostics$out_name, fixed = T )
  writeData( diagnostic_cells, 'DIAG_OUT', out_name, meta = F )
  diagnostic_cells <- aggregate( diagnostic_cells$value, by = list( diagnostic_cells$loc, diagnostic_cells$em,
                                                                    diagnostic_cells$year, diagnostic_cells$month,
                                                                    diagnostic_cells$unit ),
                                 FUN = sum )
  names( diagnostic_cells ) <- c( 'loc', 'em', 'year', 'month', 'unit', 'value' )
  diagnostic_cells <- arrange( diagnostic_cells, loc, em, year, month )
  for ( em in unique( diagnostic_cells$em ) ) {
    for ( loc in unique( diagnostic_cells$loc ) ) {
      plot_df <- diagnostic_cells[ diagnostic_cells$loc == loc & diagnostic_cells$em == em, ]
      plot_df$date <-paste0(plot_df$year, "-", sprintf( '%02d', plot_df$month ))
      plot_df$date <-  lubridate::ymd(plot_df$date, truncated=2)
      plot <- ggplot( plot_df ) +
        geom_line( aes( x = date, y = value ) ) +
        ylab( paste0( em, ' Mt' ) ) +
        xlab( '' ) +
        scale_x_date(date_breaks = "10 years", labels=scales::date_format("%Y-%m"), date_minor_breaks = "6 months") +
        ggtitle( paste0( gsub( '_cells', '', out_name ), '\n', loc ) )
      ggsave( filePath( "DIAG_OUT", paste0( out_name, '_', em, '_', loc ), extension = ".jpeg" ), units = 'in', width = 13, height = 5 )
    }
  }
  invisible( gc( ) )
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
                                           em ) {

  # ---
  # 0. Define variables specific to openburning
  openburning_sectors <- c( "AWB", "FRTB", "GRSB", "PEAT" )
  sector_type <- "openburning"
  sector_ids <- "0: Agricultural Waste Burning On Fields; 1: Forest Burning; 2: Grassland Burning; 3: Peat Burning"

  if ( !any( names( allyear_grids_list[[1]] ) %in% openburning_sectors ) ) {
    warning( paste( "No open burning sectors found for", em, "in", scenario ) )
    return( invisible( NULL ) )
  }

  # 1. Build and write out netCDF file
  #    Returns: diag_cells - diagnostic cells list
  #             out_name - output file name
  diagnostics <- build_ncdf( allyear_grids_list, output_dir, grid_resolution,
                             year_list, em, openburning_sectors, sector_type,
                             sector_ids, aggregate_sectors = TRUE )
  diagnostics <- build_ncdf( allyear_grids_list, output_dir, grid_resolution,
                             year_list, em, openburning_sectors, sector_type,
                             sector_ids, sector_shares = TRUE)

  # 2. Create diagnostic cells plots
  diagnostic_cells <- do.call( 'rbind', diagnostics$diag_cells )
  out_name <- gsub( '.nc', '_cells', diagnostics$out_name, fixed = T )
  writeData( diagnostic_cells, 'DIAG_OUT', out_name, meta = F )
  diagnostic_cells <- aggregate( diagnostic_cells$value, by = list( diagnostic_cells$loc, diagnostic_cells$em,
                                                                    diagnostic_cells$year, diagnostic_cells$month,
                                                                    diagnostic_cells$unit ),
                                 FUN = sum )
  names( diagnostic_cells ) <- c( 'loc', 'em', 'year', 'month', 'unit', 'value' )
  diagnostic_cells <- arrange( diagnostic_cells, loc, em, year, month )
  for ( em in unique( diagnostic_cells$em ) ) {
    for ( loc in unique( diagnostic_cells$loc ) ) {
      plot_df <- diagnostic_cells[ diagnostic_cells$loc == loc & diagnostic_cells$em == em, ]
      plot_df$time_line <- 1 : nrow( plot_df )
      plot_df$time_label <- paste0( plot_df$year, sprintf( '%02d', plot_df$month ) )
      plot <- ggplot( plot_df ) +
        geom_line( aes( x = time_line, y = value ) ) +
        ylab( paste0( em, ' Mt' ) ) +
        xlab( '' ) +
        scale_x_continuous( breaks = seq( 1, nrow( plot_df ), 10 ), labels = plot_df$time_label[ seq( 1, nrow( plot_df ), 10 ) ] ) +
        ggtitle( paste0( gsub( '_cells', '', out_name ), '\n', loc ) )
      ggsave( filePath( "DIAG_OUT", paste0( out_name, '_', em, '_', loc ), extension = ".jpeg" ), units = 'in', width = 13, height = 5 )
    }
  }

  invisible( gc( ) )
}


# -------------------------------------------------
# generate_air_grids_nc
# Brief:
# Dependencies:
# Author: Leyang Feng
# parameters:
# return: null
# input files: null
# output:

generate_air_grids_nc <- function( allyear_grids_list,
                                   output_dir,
                                   grid_resolution,
                                   year,
                                   em ) {


  # ---
  # 0. Define some needed variables
  global_grid_area <- grid_area( grid_resolution, all_lon = T )

  # ---
  # 1. Prepare data from writing
  # (1) emission array
  year_data_list <- lapply( year_list, function( year ) {

    current_year_grids <- allyear_grids_list[[ paste0( 'X', year ) ]]
    current_year_array <- array( dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                          180 / grid_resolution,
                                          25, length( year ) * 12 ) )

    # flip the dimension
    for ( i in 1 : 25 ) {  # go through each height layer
      for ( j in 1 : 12 ) {  # go through each month
        current_year_array[ , , i, j ] <- t( flip_a_matrix( current_year_grids[ , , i, j ] ) )
      }
    }

    # checksum calculation
    current_year_grids_no_height <- apply( current_year_grids, c( 1, 2, 4 ), sum )
    temp_array_checksum <- unlist( lapply( 1  : 12, function( i ) {
      sum( current_year_grids_no_height[ , , i ] *
             global_grid_area *
             ( days_in_month[ i ] * 24 * 60 * 60 ) /
             1000000000 ) # convert from kg m-2 s-1 to Mt for sum
    } ) )

    temp_checksum_df <- data.frame( em = em,
                                    sector = 'AIR',
                                    year = year,
                                    month = 1:12,
                                    unit = 'Mt',
                                    value = temp_array_checksum,
                                    stringsAsFactors = F )

    return( list( current_year_array, temp_checksum_df ) )
  } )

  checksum_df_list <- lapply( year_data_list, '[[', 2 )
  year_data_list <- lapply( year_data_list, '[[', 1 )
  em_array <- array( unlist( year_data_list ),  dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                                         180 / grid_resolution,
                                                         25, length( year_list ) * 12 ) )

  # (2) lons data and lon bound data
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lon_bnds_data <- cbind( lons - grid_resolution / 2, lons + grid_resolution / 2 )

  # (3) lats data and lat bound data
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  lat_bnds_data <- cbind( lats - grid_resolution / 2, lats + grid_resolution / 2 )

  # (4) levs data and levs bound data
  levs <- seq( 0.305, 14.945, 0.61 )

  # (5) time dimension data
  month_middle_days <- floor(cumsum(days_in_month) - days_in_month / 2)
  time_data <- rep( ( year_list - 2015 ) * 365, each = 12 ) + month_middle_days

  # (6) time dimension bounds data
  month_bnds_days <- cbind( c( 0, cumsum(days_in_month)[1:11] ), cumsum(days_in_month) )
  time_bnds_data <- do.call( rbind, lapply( year_list, function( yr ) {
    month_bnds_days + ( ( yr - 2015 ) * 365 )
  } ) )

  # (7) upper lower bnds data
  bnds <- 1 : 2

  # ---
  # 2. define nc dimensions
  # Define nc dimensions
  londim <- ncdim_def( "lon", "degrees_east",  lons, longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", lats, longname = 'latitude' )
  levdim <- ncdim_def( "level", "km", as.double ( levs ), longname = 'altitude' )
  timedim <- ncdim_def( "time", paste0( "days since 2015-01-01 0:0:0" ), time_data,
                        calendar = '365_day', longname = 'time', unlim = T )

  dim_list <- list( londim, latdim, levdim, timedim )

  bndsdim <- ncdim_def( 'bound', '', as.integer( bnds ), longname = 'bound', create_dimvar = F )

  # ---
  # 3. generate nc file name and some variables
  if (em == 'Sulfur') {FN_em <- 'SO2'} else {FN_em <- em}

  dataset_version_number <- get_global_constant( "dataset_version_number" )
  institution_id <- get_global_constant( "institution_id" )
  institution <- get_global_constant( "institution" )
  target_mip <- get_global_constant( "target_mip" )
  license <- get_global_constant( "license" )

  # Generate comment here to preserve SPA information from original scenario
  MD_comment <- paste0( 'SSP harmonized, gridded emissions for IAMC-', iam, '_',
                        scenario, '. Data harmonized to historical emissions ',
                        'CEDS-v2017-05-18 (anthropogenic) and v1.2 (land-use change)' )
  scenario <- clean_scenario_name( scenario )

  # In UoM-AIM-ssp370-lowNTCF all instances of ScenarioMIP are to be changed to
  # AerChemMIP, including filename.
  if ( iam == 'AIM' && scenario == 'ssp370-lowNTCF' ) {
    new_target <- 'AerChemMIP'
    license <- gsub( target_mip, new_target, license )
    target_mip <- new_target
  }

  MD_source_id_value <- paste0( institution_id, '-', iam, '-', scenario, '-', gsub("[.]", "-", dataset_version_number) )
  FN_variable_id_value <- paste0( FN_em, '-em-AIR-anthro' )
  nc_file_name <- paste( FN_variable_id_value, 'input4MIPs_emissions', target_mip, MD_source_id_value, 'gn_201501-210012.nc', sep = '_' )
  nc_file_name_w_path <- paste0( output_dir, '/', nc_file_name )

  # generate flat_var variable name
  MD_variable_id_value <- gsub( "-", "_", FN_variable_id_value ) # Change to underscore for metadata
  flat_var_name <- MD_variable_id_value
  flat_var_longname <- flat_var_name

  # define unit and missing value
  data_unit <- 'kg m-2 s-1'
  missing_value <- 1.e20

  sector_long_name <- 'anthropogenic aircraft emissions'
  product <- 'primary-emissions-data'

  # ---
  # 4. define nc variables
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, longname = flat_var_longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )

  # ---
  # 5. generate the var_list
  variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds )

  # ---
  # 6. create new nc file
  nc_new <- nc_create( nc_file_name_w_path, variable_list, force_v4 = T )

  # ---
  # 7. put nc variables into the nc file
  ncvar_put( nc_new, flat_var, em_array )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )

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
  # attributes for variables
  ncatt_put( nc_new, flat_var_name, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, flat_var_name, 'long_name', flat_var_longname )
  ncatt_put( nc_new, flat_var_name, 'missing_value', 1e+20, prec = 'float' )
  # nc global attributes
  add_global_atts( nc_new, MD_comment, institution, institution_id, product,
                   dataset_version_number, target_mip, MD_source_id_value,
                   sector_long_name, FN_em, MD_variable_id_value )

  # some other metadata
  ncatt_put( nc_new, 0, 'license', license )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes. Note that emissions are provided in uneven year intervals (2015, 2020, then at 10 year intervals) as these are the years for which projection data is available.' )
  reporting_info <- c ( Sulfur = 'Mass flux of SOx, reported as SO2',
                        NOx =    'Mass flux of NOx, reported as NO2',
                        CO =     'Mass flux of CO',
                        VOC =    'Mass flux of NMVOC (total mass emitted)',
                        NH3 =    'Mass flux of NH3',
                        BC =     'Mass flux of BC, reported as carbon mass',
                        OC =     'Mass flux of OC, reported as carbon mass',
                        CO2 =    'Mass flux of CO2',
                        CH4 =    'Mass flux of CH4' )
  if ( grepl( 'VOC\\d\\d', em ) ) {
    info_line <- paste( 'Mass flux of', em, '(total mass emitted)' )
  } else {
    info_line <- reporting_info[ em ]
  }
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # ---
  # 9. close nc_new
  nc_close( nc_new )

  # ---
  # 10. add checksum file
  out_name <- gsub( '.nc', '.csv', nc_file_name_w_path, fixed = T )
  out_df <- do.call( 'rbind', checksum_df_list )
  write.csv( out_df, out_name, row.names = F )

  invisible( gc( ) )
}


# Construct and output a NetCDF file
#
# Parameters:
#   ncdf_sectors      - the sectors to include
#   sector_type       - one of "anthro" or "openburning"
#   sector_ids        - string for the NetCDF metadata of the sector ids
#   aggregate_sectors - sum along the sector dimension?
#   sector_shares     - output sector's total value or share of all sectors
build_ncdf <- function( allyear_grids_list, output_dir, grid_resolution,
                        year_list, em, ncdf_sectors, sector_type, sector_ids,
                        aggregate_sectors = F, sector_shares = F ) {

  stopifnot( !( aggregate_sectors && sector_shares ) ) # both can't be TRUE

  # Prepare data from writing
  # (1) emission array
  year_data_list <- lapply( year_list, function( year ) {

    current_year_grids_list <- allyear_grids_list[[ paste0( 'X', year ) ]]
    current_year_sector_array <- array( dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                                 180 / grid_resolution,
                                                 length( ncdf_sectors ), 12 ) )

    temp_checksum_storage <- c()
    temp_cell_value_storage <- data.frame()
    for ( i in seq_along( ncdf_sectors ) ) {
      current_sector <- ncdf_sectors[ i ]
      temp_array <- current_year_grids_list[[ current_sector ]]
      temp_array_checksum <- temp_array
      # flip each time slice of temp_array
      temp_array <- array( unlist( lapply( 1 : 12, function( i ) { t( flip_a_matrix( temp_array[ , , i ] ) ) } ) ), dim = c( 360 / grid_resolution, 180 / grid_resolution, 12 ) )

      current_year_sector_array[ , , i, ] <- temp_array

      # checksum and diagnostic cells computation
      checksum_diag_list <- lapply( 1 : 12, function( i ) {
        current_month <- i
        # convert the matrix from from kg m-2 s-1 to Mt
        conv_mat <- temp_array_checksum[ , , i ] *
          grid_area( grid_resolution, all_lon = T ) *
          ( days_in_month[ i ] * 24 * 60 * 60 ) /
          1000000000

        # computation for checksum
        conv_mat_sum <- sum( conv_mat )

        # computation for diagnostic cells
        cell_value_list <- lapply( 1 : nrow( diagnostic_cells ) , function( i ) {
          out_df <- data.frame( em = em,
                                sector = current_sector,
                                year = year,
                                month = current_month,
                                unit = 'Mt',
                                value = conv_mat[ diagnostic_cells$row[ i ], diagnostic_cells$col[ i ] ],
                                stringsAsFactors = F )
        } )
        cell_value_df <- do.call( 'rbind', cell_value_list )
        cell_value_df <- cbind( diagnostic_cells, cell_value_df )

        return( list( conv_mat_sum, cell_value_df ) )
      } )

      temp_array_checksum <- unlist( lapply( checksum_diag_list, '[[', 1 ) )
      temp_checksum_storage <- c( temp_checksum_storage, temp_array_checksum )

      temp_cell_value_df <- do.call( 'rbind', lapply( checksum_diag_list, '[[', 2 ) )
      temp_cell_value_storage <- rbind( temp_cell_value_storage, temp_cell_value_df )
    }

    temp_checksum_df <- data.frame( em = em,
                                    sector = unlist( lapply( ncdf_sectors, rep, 12 ) ),
                                    year = year,
                                    month = rep( 1:12, length( ncdf_sectors ) ),
                                    unit = 'Mt',
                                    value = temp_checksum_storage,
                                    stringsAsFactors = F )

    return( list( current_year_sector_array, temp_checksum_df, temp_cell_value_storage ) )
  } )

  checksum_df_list <- lapply( year_data_list, '[[', 2 )
  diagnostic_cells_list <- lapply( year_data_list, '[[', 3 )
  year_data_list <- lapply( year_data_list, '[[', 1 )

  em_array <- array( unlist( year_data_list ),  dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                                         180 / grid_resolution,
                                                         length( ncdf_sectors ),
                                                         length( year_list ) * 12 ) )

  if ( aggregate_sectors ) {
    em_array <- apply(em_array, c( 1, 2, 4 ), sum)
  }
  if ( sector_shares ) {
    em_array <- prop.table( em_array, c( 1, 2, 4 ) )
    em_array[is.nan(em_array)] <- 0
  }

  # (2) lons data and lon bound data
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lon_bnds_data <- cbind( lons - grid_resolution / 2, lons + grid_resolution / 2 )

  # (3) lats data and lat bound data
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  lat_bnds_data <- cbind( lats - grid_resolution / 2, lats + grid_resolution / 2 )

  # (4) time dimension data
  month_middle_days <- floor(cumsum(days_in_month) - days_in_month / 2)
  time_data <- rep( ( year_list - 2015 ) * 365, each = 12 ) + month_middle_days

  # (5) time dimension bounds data
  month_bnds_days <- cbind( c( 0, cumsum(days_in_month)[1:11] ), cumsum(days_in_month) )
  time_bnds_data <- do.call( rbind, lapply( year_list, function( yr ) {
    month_bnds_days + ( ( yr - 2015 ) * 365 )
  } ) )

  # Define nc dimensions
  londim <- ncdim_def( "lon", "degrees_east",  lons, longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", lats, longname = 'latitude' )
  timedim <- ncdim_def( "time", paste0( "days since 2015-01-01 0:0:0" ), time_data,
                        calendar = '365_day', longname = 'time', unlim = T )

  # sector dimension data and bounds data
  if ( !aggregate_sectors ) {
    sectors <- 0 : ( length( ncdf_sectors ) - 1 )
    sector_bnds_data <- cbind( sectors - 0.5, sectors + 0.5 )
    sectordim <- ncdim_def( "sector", "", sectors, longname = 'sector' )
    dim_list <- list( londim, latdim, sectordim, timedim )
  } else {
    dim_list <- list( londim, latdim, timedim )
  }

  # upper lower bnds data
  bnds <- 1 : 2
  bndsdim <- ncdim_def( 'bound', '', bnds, longname = 'bound', create_dimvar = F )

  # Generate nc file name and some variables
  dataset_version_number <- get_global_constant( "dataset_version_number" )
  institution_id <- get_global_constant( "institution_id" )
  institution <- get_global_constant( "institution" )
  target_mip <- get_global_constant( "target_mip" )
  license <- get_global_constant( "license" )

  # Sulfur and sub-VOCs get renamed for the file output
  if ( em == 'Sulfur' ) {
    FN_em <- 'SO2'
  } else if ( grepl( 'VOC\\d\\d', em ) ) {
    FN_em <- paste0( em, '-', substr( sub( '_', '-', get_VOC_name( em ) ), 1, 10 ) )
    sector_type <- paste0( 'speciated-VOC-', sector_type )
  } else {
    FN_em <- em
  }

  # The default file is emissions, so don't append any additional identifier for
  # aggregated emissions; Do add this information later in descriptive metadata
  if ( sector_shares ) {
    sector_type_for_filename <- paste0( sector_type, '-share')
    FN_variable_id_value <- paste( FN_em, sector_type_for_filename, sep = '-' )
    data_unit <- 'percent'
  } else {
    sector_type_for_filename <- sector_type
    FN_variable_id_value <- paste( FN_em, 'em', sector_type_for_filename, sep = '-' )
    data_unit <- 'kg m-2 s-1'
  }

  product <- 'primary-emissions-data'
  if ( grepl( 'VOC\\d\\d', em ) ) {
    FN_em <- paste( em, get_VOC_name( em ) )
    product <- 'supplementary-emissions-data'
  }

  missing_value <- 1.e20

  # Generate comment here to preserve SPA information from original scenario
  # (iam and scenario are variables in the global namespace)
  # Add description of aggregate open burning
  MD_comment <- paste0( 'SSP harmonized, gridded emissions for IAMC-', iam, '_',
                        scenario, '. Data harmonized to historical emissions ',
                        'CEDS-v2017-05-18 (anthropogenic) and v1.2 (land-use change).' )
	if ( aggregate_sectors && ( sector_type == "openburning" ) ) {
  	MD_comment <- paste( MD_comment, sector_type, 'emissions are provided here',
                        'as one aggregate total. Future emissions shares by',
                        'land-type are provided in a separate file.' )
	}

  scenario <- clean_scenario_name( scenario )

  # In UoM-AIM-ssp370-lowNTCF all instances of ScenarioMIP are to be changed to
  # AerChemMIP, including filename.
  if ( iam == 'AIM' && scenario == 'ssp370-lowNTCF' ) {
    new_target <- 'AerChemMIP'
    license <- gsub( target_mip, new_target, license )
    target_mip <- new_target
  }

  MD_source_id_value <- paste0( institution_id, '-', iam, '-', scenario, '-', gsub("[.]", "-", dataset_version_number) )
  nc_file_name <- paste( FN_variable_id_value, 'input4MIPs_emissions', target_mip, MD_source_id_value, 'gn_201501-210012.nc', sep = '_' )
  nc_file_name_w_path <- paste0( output_dir, '/', nc_file_name )

  # Now that file name has been generated, re-format variable_id to have underscores

  # generate flat_var variable name
  MD_variable_id_value <- gsub( "-", "_", FN_variable_id_value ) # Change to underscore for metadata
  flat_var_name <- MD_variable_id_value

  sector_long_name <- 'anthropogenic emissions'
  if ( sector_type == 'openburning' ) {
    sector_long_name <- 'open burning'
    if ( sector_shares )
      sector_long_name <- paste( sector_long_name, 'sector shares' )
    if ( aggregate_sectors )
      sector_long_name <- paste( 'total', sector_long_name, 'emissions' )
  }
  longname <- paste( FN_em, toTitleCase( sector_long_name ) )

  # Data usage tips and reporting unit change if they are shares or not
  em_key <- c( 'Sulfur', 'NOx', 'CO', 'VOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' )
  em_actual <- c( 'SOx', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' )
  em_val <- em_actual[ em == em_key ]
  if (length(em_val)==0) {em_val <- 'VOC'}
  if ( sector_shares ) {
    data_usage_tips <- 'These are monthly averages.'
    info_line <- paste( 'Fraction of', em_val, 'from each land category listed in the sector variable' )
  } else {
    data_usage_tips <- 'Note that these are monthly average fluxes.'
    info_line <- paste( 'Mass flux of', em_val )
    if ( em_val == 'NMVOC' ) info_line <- paste( info_line, '(total mass emitted)' )
    if ( em_val == 'BC' || em_val == 'OC' ) info_line <- paste0( info_line, ', reported as carbon mass' )
  }
  if ( em_val == 'SOx' ) info_line <- paste0( info_line, ', reported as SO2' )
  if ( em_val == 'NOx' ) {
    reporting_unit <- if ( sector_type == 'openburning' ) 'NO' else 'NO2'
    info_line <- paste0( info_line, ', reported as ', reporting_unit )
  }
  if ( grepl( 'VOC\\d\\d', em ) ) { info_line <- paste( 'Mass flux of', FN_em, '(total mass emitted)' ) }

  data_usage_tips <- paste( data_usage_tips, 'Note that emissions are provided in uneven year intervals (2015, 2020, then at 10 year intervals) as these are the years for which projection data is available.' )

  # ---
  # 4. define nc variables
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, longname = longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )

  # ---
  # 5. generate the var_list
  if ( !aggregate_sectors ) {
    sector_bnds <- ncvar_def( 'sector_bnds', '', list( bndsdim, sectordim ), prec = 'double' )
    variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds, sector_bnds )
  } else {
    variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds )
  }

  # ---
  # 6. create new nc file
  nc_new <- nc_create( nc_file_name_w_path, variable_list, force_v4 = T )

  # ---
  # 7. put nc variables into the nc file
  ncvar_put( nc_new, flat_var, em_array )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )
  if ( !aggregate_sectors ) {
    ncvar_put( nc_new, sector_bnds, t( sector_bnds_data ) )
  }

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
  if ( !aggregate_sectors ) {
    ncatt_put( nc_new, "sector", "bounds", "sector_bnds" )
    ncatt_put( nc_new, "sector", "ids", sector_ids )
  }
  # attributes for variables
  ncatt_put( nc_new, flat_var_name, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, flat_var_name, 'long_name', longname )
  ncatt_put( nc_new, flat_var_name, 'missing_value', 1e+20, prec = 'float' )
  # nc global attributes
  add_global_atts( nc_new, MD_comment, institution, institution_id, product,
                   dataset_version_number, target_mip, MD_source_id_value,
                   sector_long_name, FN_em, MD_variable_id_value )
  # some other metadata
  ncatt_put( nc_new, 0, 'license', license )
  ncatt_put( nc_new, 0, 'data_usage_tips', data_usage_tips )
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  ncatt_put( nc_new, 0, 'tracking_id', paste0( "hdl:21.14100/", uuid() ) )

  # ---
  # 9. close nc_new
  nc_close( nc_new )

  # ---
  # 10. add checksum file
  out_name <- gsub( '.nc', '.csv', nc_file_name_w_path, fixed = T )
  out_df <- do.call( 'rbind', checksum_df_list )

  # diag
  sector_mapping <- readData( domain = 'GRIDDING', domain_extension = 'gridding-mappings/', file_name = gridding_sector_mapping )
  in_df <- readData('MED_OUT', paste0( 'B.', iam, '_emissions_reformatted', '_', RUNSUFFIX )) %>%
    dplyr::filter(em == !!em) %>%
    dplyr::select(sector_name = sector, one_of(make.names(year_list))) %>%
    dplyr::group_by(sector_name) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::gather(year, value, make.names(year_list)) %>%
    dplyr::mutate(year = as.integer(sub('X', '', year)))
  diff_df <- out_df %>%
    dplyr::mutate(sector_short = as.character(sector)) %>%
    dplyr::select(-sector, -em, -month) %>%
    dplyr::group_by(sector_short, year, unit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(sector_mapping, by = 'sector_short') %>%
    dplyr::left_join(in_df, by = c('year', 'sector_name')) %>%
    dplyr::mutate(pct_diff = (value.x - value.y) / value.y) %>%
    dplyr::mutate(pct_diff = if_else(is.nan(pct_diff), 0, pct_diff * 100)) %>%
    dplyr::rename(grid_sum = value.x, orig_sum = value.y) %>%
    dplyr::select(sector_name, year, unit, grid_sum, orig_sum, pct_diff)

  largest_diff <- round(max(abs(diff_df$pct_diff), na.rm = T), 4)
  if (largest_diff > 1) {
    warning(paste('Values for', em, 'were modified by up to', largest_diff, 'percent'))
  }

  writeData( diff_df, 'DIAG_OUT', sub( '.nc', '_DIFF', nc_file_name, fixed = T), meta = F )
  write.csv( out_df, out_name, row.names = F )

  return( list( out_name = nc_file_name, diag_cells = diagnostic_cells_list ) )
}


# Add global attributes to a netCDF file
add_global_atts <- function( nc_new, MD_comment, institution, institution_id,
                             product, dataset_version_number, target_mip,
                             MD_source_id_value, sector_long_name, FN_em,
                             MD_variable_id_value ) {

  creation_date <- as.POSIXlt( Sys.time(), "UTC" )
  location <- get_global_constant( "location" )
  history <- paste0( format( creation_date, format = '%d-%m-%Y %H:%M:%S %p %Z' ), '; ', location )

  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'comment', MD_comment )
  ncatt_put( nc_new, 0, 'contact', 'Steven J. Smith (ssmith@pnnl.gov)' )
  ncatt_put( nc_new, 0, 'creation_date', format( creation_date, format = '%Y-%m-%dT%H:%M:%SZ' ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'external_variables', 'gridcell_area' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'further_info_url', 'https://secure.iiasa.ac.at/web-apps/ene/SspDb/' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'grid_label', 'gn' )
  ncatt_put( nc_new, 0, 'nominal_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'history', history )
  ncatt_put( nc_new, 0, 'institution', institution )
  ncatt_put( nc_new, 0, 'institution_id', institution_id )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'product', product )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'references', 'See: https://secure.iiasa.ac.at/web-apps/ene/SspDb/ for references' )
  ncatt_put( nc_new, 0, 'source', 'IAMC Scenario Database hosted at IIASA' )
  ncatt_put( nc_new, 0, 'source_id', MD_source_id_value )
  ncatt_put( nc_new, 0, 'source_version', dataset_version_number )
  ncatt_put( nc_new, 0, 'table_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'target_mip', target_mip )
  ncatt_put( nc_new, 0, 'title', paste( 'Future', sector_long_name, 'of', FN_em, 'prepared for input4MIPs' ) )
  ncatt_put( nc_new, 0, 'variable_id', MD_variable_id_value )
}


# Version 4 UUIDs have the form xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
# where x is any hexadecimal digit and y is one of 8, 9, A, or B
# e.g., f47ac10b-58cc-4372-a567-0e02b2c3d479
#
# Modified from https://gist.github.com/cbare/5979354
uuid <- function() {
  hex_digits <- as.hexmode( 0:15 )
  y_digits <- hex_digits[9:12]

  paste(
    paste0( sample( hex_digits, 8, replace=TRUE ), collapse='' ),
    paste0( sample( hex_digits, 4, replace=TRUE ), collapse='' ),
    paste0( '4', paste0( sample( hex_digits, 3, replace=TRUE ), collapse='' ), collapse='' ),
    paste0( sample( y_digits,1 ), paste0( sample( hex_digits, 3, replace=TRUE ), collapse='' ), collapse='' ),
    paste0( sample( hex_digits, 12, replace=TRUE ), collapse='' ),
    sep = '-'
  )
}


# Historically, only the first 10 letters of the voc name are included
get_VOC_name <- function( voc ) {
  voc_map <- read.csv( 'gridding/gridding-mappings/VOC_id_name_mapping.csv',
                       row.names = 1, stringsAsFactors = F )
  voc_map[ voc, 'VOC_name' ]
}

clean_scenario_name <- function( scenario ) {
  scenario <- tolower( scenario )
  scenario <- gsub("lowntcf", "lowNTCF", scenario) # Special capitalization case
  scenario <- gsub("-os", "-over", scenario) # Special case for REMIND-MAGPIE-ssp534-os
  scenario <- gsub("-spa[0123456789]", "", scenario) # Remove SPA designation
  scenario <- gsub("ssp3-ref", "ssp3-70", scenario) # CMIP-specific change to RCP nomenclature
  scenario <- gsub("ssp3-lowNTCF", "ssp3-70-lowNTCF", scenario) # CMIP-specific change to RCP nomenclature
  scenario <- gsub("ssp5-ref", "ssp5-85", scenario) # CMIP-specific change to RCP nomenclature
  scenario <- gsub("(ssp\\d)-(\\d)\\.?(\\d)", "\\1\\2\\3", scenario) # Remove ssp hyphen
}
