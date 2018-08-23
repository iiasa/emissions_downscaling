#------------------------------------------------------------------------------
# Program Name: nc_generation_functions.R
# Authors: Leyang Feng, Caleb Braun
# Date Last Modified: August, 2018
# Program Purpose: Define functions for generating NetCDF files for the gridding
#   routine. The three main functions that should be called outside of this file
#   are:
#     1. generate_air_grids_nc()
#     2. generate_bulk_grids_nc()
#     3. generate_openburning_grids_nc()
# ------------------------------------------------------------------------------

# Special Packages
library( 'ncdf4' )
library( 'sp' )
library( 'geosphere' )

# Output a NetCDF file for the bulk sectors of an emissions species
#
# Convert gridded data from a nested list of sectors by years to a NetCDF file
# containing total emissions by anthropogenic sector. Additionally write out
# checksum and diagnostic csv files.
#
# Args:
#   allyear_grids_list: A list of lists. The outer list must be named Xyears,
#     the inner list must contain open burning sectors, and the contents must
#     3d arrays (lat / lon / month)
#   output_dir: Path to write the ouput files
#   grid_resolution: Resolution in degrees of the spatial data
#   year_list: List of years from the data to write out
#   em: Name of the emission species
#   sub_nmvoc: Is the emission a NMVOC subspecies?
#
# Return:
#   NULL
generate_bulk_grids_nc <- function( allyear_grids_list, output_dir,
                                    grid_resolution, year_list, em,
                                    sub_nmvoc ) {

  # Define variables specific to anthro. Note that order matters, so make sure
  # bulk_sectors and sector_ids are in correct positions.
  bulk_sectors <- c( "AGR", "ENE", "IND", "TRA", "RCO", "SLV", "WST", "SHP" )
  sector_ids <- "0: Agriculture; 1: Energy; 2: Industrial; 3: Transportation; 4: Residential, Commercial, Other; 5: Solvents production and application; 6: Waste; 7: International Shipping"
  sector_type <- "anthro"

  if ( !any( names( allyear_grids_list[[1]] ) %in% bulk_sectors ) ) {
    warning( paste( "No bulk sectors found for", em, "in", scenario ) )
    return( invisible( NULL ) )
  }

  # Build and write out netCDF file
  # Returns: diag_cells - diagnostic cells list
  #          out_name - output file name
  diagnostics <- build_ncdf( allyear_grids_list, output_dir, grid_resolution,
                             year_list, em, sub_nmvoc, bulk_sectors,
                             sector_type, sector_ids )

  # Create diagnostic cells plots
  diagnostic_cells <- do.call( 'rbind', diagnostics$diag_cells )
  out_name <- gsub( '.nc', '_cells', diagnostics$out_name, fixed = T )
  writeData( diagnostic_cells, 'DIAG_OUT', out_name, meta = F )

  GENERATE_PLOTS <- get_global_constant( 'diagnostic_plots' )
  if ( GENERATE_PLOTS ) {
    printLog( 'Generating diagnostic plots' )

    # Aggregate to regional (selected cell) totals by month, then plot the
    # timeseries for each unique emission and region
    diagnostic_cells %>%
      dplyr::mutate( full_date = as.Date( paste( year, sprintf( '%02d', month ), '01', sep = '-' ) ) ) %>%
      dplyr::arrange( loc, em, full_date ) %>%
      dplyr::group_by( loc, em, unit, full_date ) %>%
      dplyr::summarise( value = sum( value ) ) %>%
      dplyr::group_by( loc, em, unit ) %>%
      dplyr::do({
        em <- unique( .$em )
        loc <- unique( .$loc )
        plt <- ggplot( data = . ) +
          geom_line( aes( x = full_date, y = value ) ) +
          ylab( paste0( em, ' Mt' ) ) +
          xlab( '' ) +
          scale_x_date( date_breaks = "10 years",
                        labels=scales::date_format( "%Y-%m" ),
                        date_minor_breaks = "6 months" ) +
          ggtitle( paste0( gsub( '_cells', '', out_name ), '\n', loc ) )

        ggsave( filename = filePath( "DIAG_OUT", paste0( out_name, '_', em, '_', loc ), extension = ".jpeg" ),
                plot = plt, units = 'in', width = 13, height = 5 )
        invisible( . )
      })
  }

  invisible( gc( ) )
}


# Output NetCDF files for the open burning sectors of an emissions species
#
# Convert gridded data from a nested list of sectors by years to a NetCDF file
# containing total emissions by sector and a NetCDF file containing sector
# shares of total emissions. Additionally write out checksum and diagnostic
# csv files.
#
# Args:
#   allyear_grids_list: A list of lists. The outer list must be named Xyears,
#     the inner list must contain open burning sectors, and the contents must
#     3d arrays (lat / lon / month)
#   output_dir: Path to write the ouput files
#   grid_resolution: Resolution in degrees of the spatial data
#   year_list: List of years from the data to write out
#   em: Name of the emission species
#   sub_nmvoc: Is the emission a NMVOC subspecies?
#
# Return:
#   NULL
generate_openburning_grids_nc <- function( allyear_grids_list, output_dir,
                                           grid_resolution, year_list, em,
                                           sub_nmvoc ) {
  # Define variables specific to openburning
  openburning_sectors <- c( "AWB", "FRTB", "GRSB", "PEAT" )
  sector_type <- "openburning"
  sector_ids <- "0: Agricultural Waste Burning On Fields; 1: Forest Burning; 2: Grassland Burning; 3: Peat Burning"

  if ( !any( names( allyear_grids_list[[1]] ) %in% openburning_sectors ) ) {
    warning( paste( "No open burning sectors found for", em, "in", scenario ) )
    return( invisible( NULL ) )
  }

  # Build and write out netCDF file
  # Returns: diag_cells - diagnostic cells list
  #          out_name - output file name
  diag_agg <- build_ncdf( allyear_grids_list, output_dir, grid_resolution,
                          year_list, em, sub_nmvoc, openburning_sectors,
                          sector_type, sector_ids, aggregate_sectors = TRUE )
  diag_share <- build_ncdf( allyear_grids_list, output_dir, grid_resolution,
                            year_list, em, sub_nmvoc, openburning_sectors,
                            sector_type, sector_ids, sector_shares = TRUE )

  # Create diagnostic cells plots
  for ( diagnostics in list( diag_agg, diag_share ) ) {

    diagnostic_cells <- do.call( 'rbind', diagnostics$diag_cells )
    out_name <- gsub( '.nc', '_cells', diagnostics$out_name, fixed = T )
    writeData( diagnostic_cells, 'DIAG_OUT', out_name, meta = F )

    GENERATE_PLOTS <- get_global_constant( 'diagnostic_plots' )
    if ( GENERATE_PLOTS ) {
      printLog( 'Generating diagnostic plots' )

      # Aggregate to regional (selected cell) totals by month
      diagnostic_cells <- diagnostic_cells %>%
        dplyr::group_by( loc, em, year, month, unit ) %>%
        dplyr::summarise( value = sum( value ) ) %>%
        dplyr::arrange( loc, em, year, month )

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
    }
  }

  invisible( gc( ) )
}


# Output a NetCDF file for aircraft emissions
#
# Convert gridded data from a nested list of sectors by years to a NetCDF file
# containing total aircraft emissions. Additionally write out checksum and
# diagnostic csv files.
#
# Args:
#   allyear_grids_list: A list of lists. The outer list must be named Xyears,
#     the inner list must contain open burning sectors, and the contents must
#     3d arrays (lat / lon / month)
#   output_dir: Path to write the ouput files
#   grid_resolution: Resolution in degrees of the spatial data
#   year: TODO: Why is this year and not year_list??
#   em: Name of the emission species
#
# Return:
#   NULL
generate_air_grids_nc <- function( allyear_grids_list,
                                   output_dir,
                                   grid_resolution,
                                   year,
                                   em ) {
  global_grid_area <- grid_area( grid_resolution, all_lon = T )

  # Prepare data for writing
  year_data_list <- lapply( year_list, function( year ) {

    current_year_grids <- allyear_grids_list[[ paste0( 'X', year ) ]]
    current_year_array <- array( dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                          180 / grid_resolution,
                                          25, length( year ) * 12 ) )

    # flip the dimension
    for ( i in 1 : 25 ) {  # go through each height layer
      for ( j in 1 : 12 ) {  # go through each month
        current_year_array[ , , i, j ] <- rotate_a_matrix( current_year_grids[ , , i, j ] )
      }
    }

    # checksum calculation
    current_year_grids_no_height <- apply( current_year_grids, c( 1, 2, 4 ), sum )
    temp_array_checksum <- unlist( lapply( 1  : 12, function( i ) {
      sum( current_year_grids_no_height[ , , i ] *
             global_grid_area *
             ( days_in_month[ i ] * 24 * 60 * 60 ) /
             1000000 ) # convert from kg m-2 s-1 to kt for sum
    } ) )

    temp_checksum_df <- data.frame( year = year,
                                    em = em,
                                    sector = 'AIR',
                                    month = 1:12,
                                    global_total = temp_array_checksum,
                                    units = 'kt',
                                    stringsAsFactors = F )

    return( list( current_year_array, temp_checksum_df ) )
  } )

  checksum_df_list <- lapply( year_data_list, '[[', 2 )
  year_data_list <- lapply( year_data_list, '[[', 1 )
  em_array <- array( unlist( year_data_list ),  dim = c( 360 / grid_resolution, # lat lon flipped to accommodate nc write-in
                                                         180 / grid_resolution,
                                                         25, length( year_list ) * 12 ) )

  bnds <- prepBounds( grid_resolution, days_in_month, year_list, "AIR" )

  # levs data and levs bound data
  levs <- seq( 0.305, 14.945, 0.61 )

  # ---
  # 2. define nc dimensions
  # Define nc dimensions
  londim <- ncdim_def( "lon", "degrees_east",  bnds$lons, longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", bnds$lats, longname = 'latitude' )
  levdim <- ncdim_def( "level", "km", levs, longname = 'altitude' )
  timedim <- ncdim_def( "time", paste0( "days since 2015-01-01 0:0:0" ),
                        bnds$time_data, calendar = '365_day', longname = 'time',
                        unlim = T )
  bndsdim <- ncdim_def( 'bound', '', 1:2, longname = 'bound', create_dimvar = F )

  dim_list <- list( londim, latdim, levdim, timedim )

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

  # define unit and missing value
  data_unit <- 'kg m-2 s-1'
  missing_value <- 1.e20

  sector_long_name <- 'anthropogenic aircraft emissions'
  longname <- paste( FN_em, toTitleCase( sector_long_name ) )
  product <- 'primary-emissions-data'


  # ---
  # 4. define nc variables
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, longname = longname, compression = 5 )
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
  ncvar_put( nc_new, lon_bnds, t( bnds$lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( bnds$lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( bnds$time_bnds_data ) )

  # ---
  # 8. nc variable attributes
  # attributes for dimensions
  add_variable_atts( nc_new, FALSE, sector_ids, jflat_var_name, longname, missing_value )

  # nc global attributes
  add_global_atts( nc_new, MD_comment, institution, institution_id, product,
                   dataset_version_number, target_mip, MD_source_id_value,
                   sector_long_name, FN_em, MD_variable_id_value )

  # some other metadata
  ncatt_put( nc_new, 0, 'license', license )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes. Note that emissions are provided in uneven year intervals (2015, 2020, then at 10 year intervals) as these are the years for which projection data is available.' )
  reporting_info <- c( Sulfur = 'Mass flux of SOx, reported as SO2',
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
  ncatt_put( nc_new, 0, 'tracking_id', paste0( "hdl:21.14100/", uuid() ) )

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
# Args:
#   ncdf_sectors: The sectors to include
#   sector_type: One of "anthro" or "openburning"
#   sector_ids: String for the NetCDF metadata of the sector ids
#   aggregate_sectors: Sum along the sector dimension?
#   sector_shares: Output sector's total value or share of all sectors
build_ncdf <- function( allyear_grids_list, output_dir, grid_resolution,
                        year_list, em, sub_nmvoc, ncdf_sectors, sector_type,
                        sector_ids, aggregate_sectors = F, sector_shares = F ) {

  SEC_IN_MONTH <- days_in_month * 24 * 60 * 60
  KT_PER_KG <- 1e-06
  NMONTHS <- 12L

  # Filter data to only the gridding years specified in gridding_initialize()
  Xyears <- intersect( paste0( 'X', year_list ), names( allyear_grids_list ) )
  year_grids_list <- allyear_grids_list[ Xyears ]

  # Parameter checks
  stopifnot( dir.exists( output_dir ) )
  stopifnot( length( year_grids_list ) > 0 )
  stopifnot( sector_type %in% c( "anthro", "openburning" ) )
  stopifnot( !( aggregate_sectors && sector_shares ) ) # both can't be TRUE


  ### Prepare data for writing to NetCDF

  # Define array dimensions:
  #    each year array:    (lon x lat x sectors x months in year)
  #    array of all years: (lon x lat x sectors x all months)
  lon_res <- as.integer( 360 / grid_resolution )
  lat_res <- as.integer( 180 / grid_resolution )
  year_grid_dims <- c( lon_res, lat_res, length( ncdf_sectors ), NMONTHS )
  all_years_dims <- year_grid_dims * c( 1, 1, 1, length( year_grids_list ) )

  # Flip lat and lon to accommodate nc write-in
  year_grids_rtd <- lapply( year_grids_list, rotate_lat_lon, year_grid_dims )

  # Flatten list of all year grids into one large array
  em_array <- array( unlist( year_grids_rtd, use.names = F ), all_years_dims )

  # Apply transformations on the sector dimension (the 3rd one), if requested
  if ( aggregate_sectors ) {
    # em_array <- apply( em_array, c( 1, 2, 4 ), sum )
    em_array <- em_array
  } else if ( sector_shares ) {
    em_array <- prop.table( em_array, c( 1, 2, 4 ) )
    em_array[ is.nan( em_array ) ] <- 0
  }


  ### Define NetCDF dimensions
  bnds <- prepBounds( grid_resolution, days_in_month, year_list, ncdf_sectors )

  bndsdim <- ncdim_def( 'bound', '', 1:2, longname = 'bound', create_dimvar = F )
  londim <- ncdim_def( "lon", "degrees_east",  bnds$lons, longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", bnds$lats, longname = 'latitude' )
  sectordim <- ncdim_def( "sector", "", bnds$sectors, longname = 'sector' )
  timedim <- ncdim_def( "time", paste0( "days since 2015-01-01 0:0:0" ),
                        bnds$time_data, calendar = '365_day', longname = 'time',
                        unlim = T )

  if ( !aggregate_sectors ) {
    dim_list <- list( londim, latdim, sectordim, timedim )
  } else {
    dim_list <- list( londim, latdim, timedim )
  }


  ### Generate nc file name and some variables
  #
  # fn = file name
  # md = metadata

  DATASET_VERSION <- get_global_constant( "dataset_version_number" )
  INSTITUTION_ID <- get_global_constant( "institution_id" )
  INSTITUTION <- get_global_constant( "institution" )
  target_mip <- get_global_constant( "target_mip" )
  license <- get_global_constant( "license" )

  NC_COMPRESSION <- 5
  MISSING_VALUE <- 1.e20
  DATA_UNIT <- if_else( sector_shares, 'percent', 'kg m-2 s-1' )
  DATA_TYPE <- if_else( sub_nmvoc, 'supplemental-data', '' )
  PRODUCT   <- if_else( sub_nmvoc, 'supplementary-emissions-data',
                        'primary-emissions-data' )

  DATASET_GROUP <- 'input4MIPs_emissions'
  GRID_LABEL <- 'gn'
  TIME_RANGE <- '201501-210012'

  fn_em <- clean_em_name( em, sub_nmvoc, sector_type )
  fn_sector <- clean_sector_name( sector_type, sector_shares, sub_nmvoc )
  fn_scenario <- clean_scenario_name( scenario )

  # In UoM-AIM-ssp370-lowNTCF all instances of ScenarioMIP are to be changed to
  # AerChemMIP, including filename.
  if ( iam == 'AIM' && fn_scenario == 'ssp370-lowNTCF' ) {
    new_target <- 'AerChemMIP'
    md_license <- gsub( target_mip, new_target, license )
    target_mip <- new_target
  }

  fn_variable_id <- paste( fn_em, fn_sector, sep = '-' )
  md_source_id <- paste( INSTITUTION_ID, iam, fn_scenario, gsub( "\\.", "-", DATASET_VERSION ), DATA_TYPE, sep = '-' )
  nc_file_name <- paste( fn_variable_id, DATASET_GROUP, target_mip, md_source_id, GRID_LABEL, TIME_RANGE, sep = '_' )
  nc_file_name_w_path <- paste0( output_dir, '/', nc_file_name, '.nc' )

  # Change hyphens in variable name to underscores for the metadata
  flat_var_name <- gsub( "-", "_", fn_variable_id )

  # Generate comment with SPA information from original scenario (iam and
  # scenario are variables in the global namespace). Add description of
  # aggregate for open burning
  md_comment <- paste0( 'SSP harmonized, gridded emissions for IAMC-', iam, '_',
                        scenario, '. Data harmonized to historical emissions ',
                        'CEDS-v2017-05-18 (anthropogenic) and v1.2 (land-use ',
                        'change).' )
	if ( aggregate_sectors && ( sector_type == "openburning" ) ) {
  	md_comment <- paste( md_comment, sector_type, 'emissions are provided here',
                        'as one aggregate total. Future emissions shares by',
                        'land-type are provided in a separate file.' )
	}

  if ( sector_type == 'openburning' && aggregate_sectors ) {
    sector_long_name <- 'open burning sector shares'
  } else if ( sector_type == 'openburning' && sector_shares ) {
    sector_long_name <- 'total open burning emissions'
  } else if ( sector_type == 'openburning') {
    sector_long_name <- 'open burning'
  } else {
    sector_long_name <- 'anthropogenic emissions'
  }
  longname <- paste( fn_em, toTitleCase( sector_long_name ) )

  data_details <- build_data_details( em, fn_em, sector_shares, sector_type, sub_nmvoc )


  ### Define NetCDF variables
  flat_var  <- ncvar_def( flat_var_name, DATA_UNIT, dim_list,missval = MISSING_VALUE,
                          longname = longname, compression = NC_COMPRESSION )
  lon_bnds    <- ncvar_def( 'lon_bnds',    '', list( bndsdim, londim ),    prec = 'double' )
  lat_bnds    <- ncvar_def( 'lat_bnds',    '', list( bndsdim, latdim ),    prec = 'double' )
  time_bnds   <- ncvar_def( 'time_bnds',   '', list( bndsdim, timedim ),   prec = 'double' )
  sector_bnds <- ncvar_def( 'sector_bnds', '', list( bndsdim, sectordim ), prec = 'double' )

  if ( !aggregate_sectors ) {
    variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds, sector_bnds )
  } else {
    variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds )
  }


  ### Create and write the NetCDF file
  nc_new <- nc_create( nc_file_name_w_path, variable_list, force_v4 = T )

  ncvar_put( nc_new, flat_var,  em_array )
  ncvar_put( nc_new, lon_bnds,  t( bnds$lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds,  t( bnds$lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( bnds$time_bnds_data ) )
  if ( !aggregate_sectors ) {
    ncvar_put( nc_new, sector_bnds, t( bnds$sector_bnds_data ) )
  }


  ### Add variable and global attributes
  add_variable_atts( nc_new, !aggregate_sectors, sector_ids, flat_var_name,
                     longname, MISSING_VALUE )

  add_global_atts( nc_new, md_comment, INSTITUTION, INSTITUTION_ID, PRODUCT,
                   DATASET_VERSION, target_mip, md_source_id, sector_long_name,
                   fn_em, flat_var_name )

  # Sub-NMVOC specific metadata
  if ( sub_nmvoc ) add_sub_voc_atts( nc_new, em )

  # some other metadata
  ncatt_put( nc_new, 0, 'license', license )
  ncatt_put( nc_new, 0, 'data_usage_tips', data_details$data_usage_tips )
  ncatt_put( nc_new, 0, 'reporting_unit', data_details$info_line )
  ncatt_put( nc_new, 0, 'tracking_id', paste0( "hdl:21.14100/", uuid() ) )

  # Close and write the NetCDF file
  nc_close( nc_new )


  ### Generate diagnostics

  # Build conversion array to convert monthly values to kt. The dimensions are
  # (lon x lat x 12), where each value represents the conversion factor for
  # going from kg m-2 s-1 to kt per month for each grid cell for each month.
  grid_cell_column <- grid_area( grid_resolution, all_lon = T )
  grid_cell_conv <- rep( t( grid_cell_column ), NMONTHS ) %>%
    array( dim = c( lon_res, lat_res, NMONTHS ) ) %>%
    sweep( 3, SEC_IN_MONTH * KT_PER_KG, `*` )

  # Convert to kt
  year_grids_kt <- lapply( year_grids_rtd, sweep, c( 1, 2, 4 ), grid_cell_conv, `*` )

  out_name <- gsub( '.nc', '.csv', nc_file_name_w_path, fixed = T )
  out_df <- build_checksum( year_grids_kt, em, ncdf_sectors, year_list )

  sector_mapping <- readData( domain = 'GRIDDING', domain_extension = 'gridding-mappings/', file_name = gridding_sector_mapping )
  in_df <- readData('MED_OUT', paste0( 'B.', iam, '_emissions_reformatted', '_', RUNSUFFIX )) %>%
    dplyr::filter(em == !!em) %>%
    dplyr::select(sector_name = sector, one_of(make.names(year_list))) %>%
    dplyr::group_by(sector_name) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::gather(year, value, make.names(year_list)) %>%
    dplyr::mutate(year = as.integer(sub('X', '', year)), global_total = value * 1000) # Convert to kt
  diff_df <- out_df %>%
    dplyr::mutate(sector_short = as.character(sector)) %>%
    dplyr::select(-sector, -em, -month) %>%
    dplyr::group_by(sector_short, year, units) %>%
    dplyr::summarise(global_total = sum(global_total)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(sector_mapping, by = 'sector_short') %>%
    dplyr::left_join(in_df, by = c('year', 'sector_name')) %>%
    dplyr::mutate(pct_diff = (global_total.x - global_total.y) / global_total.y) %>%
    dplyr::mutate(pct_diff = if_else(is.nan(pct_diff), 0, pct_diff * 100)) %>%
    dplyr::rename(grid_sum = global_total.x, orig_sum = global_total.y) %>%
    dplyr::select(sector_name, year, units, grid_sum, orig_sum, pct_diff)

  ERR_TOL <- get_global_constant( 'error_tolerance' )
  largest_diff <- round(max(abs(diff_df$pct_diff), na.rm = T), 4)

  if ( largest_diff > ERR_TOL ) {
    warning( paste('Values for', em, 'were modified by up to', largest_diff, 'percent') )

    err_rows <- diff_df %>%
      dplyr::filter( is.nan( pct_diff ) | is.na( pct_diff ) | abs( pct_diff ) > ERR_TOL ) %>%
      dplyr::mutate( em = !!em, scenario = !!scenario ) %>%
      dplyr::select( scenario, em, everything() )

    err_fname <- paste0( '../diagnostic-output/ERROR_', RUNSUFFIX, '.csv' )
    add_to_file <- file.exists( err_fname )
    write.table( err_rows, file = err_fname, append = add_to_file, sep = ',',
                 row.names = F, col.names = !add_to_file )
  }

  writeData( diff_df, 'DIAG_OUT', sub( '.nc', '_DIFF', nc_file_name, fixed = T ), meta = F )
  write.csv( out_df, out_name, row.names = F )

  diagnostic_cells_list <- extract_diag_cells( year_grids_list, diagnostic_cells,
                                               ncdf_sectors, lat_res, em)
  return( list( out_name = nc_file_name, diag_cells = diagnostic_cells_list ) )
}


# Add global attributes to a netCDF file
add_global_atts <- function( nc_new, md_comment, institution, institution_id,
                             product, dataset_version_number, target_mip,
                             md_source_id, sector_long_name, fn_em,
                             md_variable_id ) {

  creation_date <- as.POSIXlt( Sys.time(), "UTC" )
  location <- get_global_constant( "location" )
  history <- paste0( format( creation_date, format = '%d-%m-%Y %H:%M:%S %p %Z' ), '; ', location )

  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'comment', md_comment )
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
  ncatt_put( nc_new, 0, 'source_id', md_source_id )
  ncatt_put( nc_new, 0, 'source_version', dataset_version_number )
  ncatt_put( nc_new, 0, 'table_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'target_mip', target_mip )
  ncatt_put( nc_new, 0, 'title', paste( 'Future', sector_long_name, 'of', fn_em, 'prepared for input4MIPs' ) )
  ncatt_put( nc_new, 0, 'variable_id', md_variable_id )
}

add_variable_atts <- function( nc_new, sector_var, sector_ids, flat_var_name,
                               longname, missing_value ) {
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
  if ( sector_var ) {
    ncatt_put( nc_new, "sector", "bounds", "sector_bnds" )
    ncatt_put( nc_new, "sector", "ids", sector_ids )
  }
  ncatt_put( nc_new, flat_var_name, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, flat_var_name, 'long_name', longname )
  ncatt_put( nc_new, flat_var_name, 'missing_value', missing_value, prec = 'float' )
}

add_sub_voc_atts <- function( nc_new, voc_id ) {
  VOC_name <- get_VOC_info( voc_id, 'name' )
  molecular_weight <- get_VOC_info( voc_id, 'weight' )
  molecular_weight_unit <- "g mole-1"

  ncatt_put( nc_new, 0, 'VOC_name', VOC_name )
  ncatt_put( nc_new, 0, 'molecular_weight', molecular_weight, prec = 'float' )
  ncatt_put( nc_new, 0, 'molecular_weight_unit', molecular_weight_unit )
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
get_VOC_info <- function( voc, type ) {
  voc_map <- read.csv( 'gridding/gridding-mappings/VOC_id_name_mapping.csv',
                       row.names = 1, stringsAsFactors = F )
  if ( type == 'name' ) {
    voc_map[ voc, 'VOC_name' ]
  }
  else if ( type == 'weight' ) {
    voc_map[ voc, 'molecular.weight' ]
  }
  else {
    stop( 'invalid argument type to get_VOC_info' )
  }
}


# Data usage tips and reporting unit change if they are shares or not
build_data_details <- function( em, fn_em, sector_shares, sector_type, sub_nmvoc ) {
  em_key <- c( 'Sulfur', 'NOx', 'CO', 'NMVOC', 'VOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' )
  em_actual <- c( 'SOx', 'NOx', 'CO', 'NMVOC', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' )
  em_val <- em_actual[ em == em_key ]
  if ( length( em_val ) == 0 ) { em_val <- 'NMVOC' }
  if ( sector_shares ) {
    data_usage_tips <- 'These are monthly averages.'
    info_line <- paste( 'Fraction of', em_val, 'from each land category listed',
                        'in the sector variable' )
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
  if ( sub_nmvoc ) { info_line <- paste( 'Mass flux of', fn_em, '(total mass emitted)' ) }

  data_usage_tips <- paste( data_usage_tips, 'Note that emissions are provided',
                            'in uneven year intervals (2015, 2020, then at 10',
                            'year intervals) as these are the years for which',
                            'projection data is available.' )

  list( data_usage_tips = data_usage_tips, info_line = info_line)
}


# Clean the emission name
#
# Replace 'Sulfur' with 'SO2' and lookup standard name for sub-VOC emissions
# then prefix with 'NMVOC'.
#
# Args:
#  em: Original emission name
#  sub_nmvoc: Is the emission type a sub-VOC?
#  sector_type: One of 'anthropogenic' or 'openburning'
#
# Returns:
#   The cleaned emission name
clean_em_name <- function( em, sub_nmvoc, sector_type ) {
  if ( em == 'Sulfur' ) {
    'SO2'
  } else if ( sub_nmvoc && sector_type == 'openburning' ) {
    paste0( 'NMVOC-', sub( '\\.', '-', em ) )
  } else if ( sub_nmvoc ) {
    paste0( em, '-', substr( sub( '_', '-', get_VOC_info( em, 'name' ) ), 1, 10 ) )
  } else {
    em
  }
}


# Clean the scenario name
#
# Args:
#  scenario: Original scenario name
#
# Returns:
#   The cleaned scenario name
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


clean_sector_name <- function( sector_type, sector_shares, sub_nmvoc ) {
  speciated <- if ( sub_nmvoc ) 'speciated-VOC-' else ''
  if ( sector_shares ) {
    paste0( speciated, sector_type, '-share')
  } else {
    paste0( 'em-', speciated, sector_type )
  }
}


# Build the bounds for the NetCDF dimensions
#
# Args:
#   grid_resolution: Resolution in degrees
#   days_in_month: Vector of days in each month (non leap year) in order
#   year_list: Integer vector of years
#   ncdf_sectors: Sector names
#
# Returns:
#   Named list of bounds and bounds data
prepBounds <- function( grid_resolution, days_in_month, year_list, ncdf_sectors ) {
  # Prepare lons data and lon bound data
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lon_bnds_data <- cbind( lons - grid_resolution / 2, lons + grid_resolution / 2 )

  # Prepare lats data and lat bound data
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  lat_bnds_data <- cbind( lats - grid_resolution / 2, lats + grid_resolution / 2 )

  # Prepare time dimension data
  month_middle_days <- floor(cumsum(days_in_month) - days_in_month / 2)
  time_data <- rep( ( year_list - 2015 ) * 365, each = 12 ) + month_middle_days

  # Prepare time dimension bounds data
  month_bnds_days <- cbind( c( 0, cumsum(days_in_month)[1:11] ), cumsum(days_in_month) )
  time_bnds_data <- do.call( rbind, lapply( year_list, function( yr ) {
    month_bnds_days + ( ( yr - 2015 ) * 365 )
  } ) )

  # Prepare sector dimension and bounds data
  sectors <- seq_along( ncdf_sectors ) - 1
  sector_bnds_data <- cbind( sectors - 0.5, sectors + 0.5 )

  return( list( lons      = lons,      lon_bnds_data    = lon_bnds_data,
                lats      = lats,      lat_bnds_data    = lat_bnds_data,
                time_data = time_data, time_bnds_data   = time_bnds_data,
                sectors   = sectors,   sector_bnds_data = sector_bnds_data ) )
}


# Build global checksum csv file
#
# Build a data.frame of global totals (in kt) for each year, sector, and month
# for the given emission species.
#
# Args:
#   year_data_list: List of arrays, each containing emission totals for each
#     year in kt for each grid cell for each sector for each month
#   em: Emission species name
#   ncdf_sectors: Sector names
#   year_list: Integer vector of output years
#
# Returns:
#   The checksum data.frame
build_checksum <- function( year_data_list, em, ncdf_sectors, year_list ) {
  checksum_df <- do.call( rbind, lapply( year_data_list, colSums, dims = 2 ) )

  data.frame( em, ncdf_sectors, checksum_df ) %>%
    cbind( rep( year_list, each = length( ncdf_sectors ) ), . ) %>%
    setNames( c( 'year', 'em', 'sector', 1:12 ) ) %>%
    tidyr::gather( 'month', 'global_total', '1':'12' ) %>%
    dplyr::mutate( units = 'kt' ) %>%
    dplyr::arrange( year, sector )
}


# Extract values at key grid cells
#
# Use matrix indexing to extract just the cells of interest speficied by
# diagnostic_cells for each year, month, and sector.
#
# Args:
#   year_grids_list: List of arrays, each containing emission totals for each
#     year in kt for each grid cell for each sector for each month
#   diagnostic_cells: A data.frame specifying cells to pull out
#   ncdf_sectors: Names of sectors in the order given in year_grids_list
#   lat_res:
#   em: Emission species name
#
# Returns:
#   List of data.frames of diagnostic cell values for each year
extract_diag_cells <- function( year_grids_list, diagnostic_cells, ncdf_sectors, lat_res, em ) {
  diagnostic_cells_indices <- diagnostic_cells[ c('col', 'row') ] %>%
    tidyr::crossing( sector = 1:length( ncdf_sectors ), month = 1:12 ) %>%
    dplyr::arrange( sector, month ) %>%
    dplyr::mutate( row = lat_res - row + 1L ) %>% # Account for rotation
    as.matrix()

  lapply( names( year_grids_list ), function( Xyear ) {
    year_grid <- year_grids_list[[ Xyear ]]
    cell_values <- year_grid[ diagnostic_cells_indices ]

    cbind( diagnostic_cells,
           data.frame( em = em,
                       sector = ncdf_sectors[ diagnostic_cells_indices[ , 3 ] ],
                       year = as.integer( substr( Xyear, 2, 5 ) ),
                       month = diagnostic_cells_indices[ , 4 ],
                       unit = 'kt',
                       value = cell_values / 1000,
                       stringsAsFactors = F) )
  })
}
