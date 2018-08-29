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

  allyear_grids_list <- lapply( allyear_grids_list, `[`, bulk_sectors )
  if ( !any( names( allyear_grids_list[[1]] ) %in% bulk_sectors ) ) {
    warning( paste( "No bulk sectors found for", em, "in", scenario ) )
    return( invisible( NULL ) )
  }

  # Build and write out netCDF file
  build_ncdf( allyear_grids_list, output_dir, grid_resolution, year_list, em,
              sub_nmvoc, bulk_sectors, sector_type, sector_ids )
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
  # Define variables specific to open burning. Note that order matters, so make
  # sure bulk_sectors and sector_ids are in correct positions.
  openburning_sectors <- c( "AWB", "FRTB", "GRSB", "PEAT" )
  sector_type <- "openburning"
  sector_ids <- "0: Agricultural Waste Burning On Fields; 1: Forest Burning; 2: Grassland Burning; 3: Peat Burning"

  allyear_grids_list <- lapply( allyear_grids_list, `[`, openburning_sectors )
  if ( !any( names( allyear_grids_list[[1]] ) %in% openburning_sectors ) ) {
    warning( paste( "No open burning sectors found for", em, "in", scenario ) )
    return( invisible( NULL ) )
  }

  # Build and write out netCDF file where all sectors are summed together
  build_ncdf( allyear_grids_list, output_dir, grid_resolution, year_list, em,
              sub_nmvoc, openburning_sectors, sector_type, sector_ids,
              aggregate_sectors = TRUE )

  # Build and write out netCDF file where values are sector shares
  build_ncdf( allyear_grids_list, output_dir, grid_resolution, year_list, em,
              sub_nmvoc, openburning_sectors, sector_type, sector_ids,
              sector_shares = TRUE )
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
generate_air_grids_nc <- function( allyear_grids_list, output_dir,
                                   grid_resolution, year_list, em ) {

  # Define variables specific to aircraft emissions. Note that the 'sector'
  # variable is just the altitude layer.
  air_sectors <- 1:25
  sector_type <- "AIR-anthro"
  sector_ids <- ""

  # Build and write out netCDF file
  build_ncdf( allyear_grids_list, output_dir, grid_resolution,
                          year_list, em, FALSE, air_sectors, sector_type,
                          sector_ids )
}


# Construct and output a NetCDF file
#
# Args:
#   year_grids_list:
#   output_dir: Where should the NetCDF be written?
#   grid_resolution:
#   year_list:
#   em:
#   sub_nmvoc:
#   ncdf_sectors: The sectors to include
#   sector_type: One of "AIR", "anthro", or "openburning"
#   sector_ids: String for the NetCDF metadata of the sector ids
#   aggregate_sectors: Sum along the sector dimension?
#   sector_shares: Output sector's total value or share of all sectors
#
# Returns:
#   NULL
build_ncdf <- function( year_grids_list, output_dir, grid_resolution,
                        year_list, em, sub_nmvoc, ncdf_sectors, sector_type,
                        sector_ids, aggregate_sectors = F, sector_shares = F ) {

  # Filter data to only the gridding years specified in gridding_initialize()
  Xyears <- intersect( paste0( 'X', year_list ), names( year_grids_list ) )
  year_grids_list <- year_grids_list[ Xyears ]

  # Parameter checks
  stopifnot( dir.exists( output_dir ) )
  stopifnot( length( year_grids_list ) > 0 )
  stopifnot( sector_type %in% c( "AIR-anthro", "anthro", "openburning" ) )
  stopifnot( !( aggregate_sectors && sector_shares ) ) # both can't be TRUE

  fn_em <- clean_em_name( em, sub_nmvoc, sector_type )
  fn_sector <- clean_sector_name( sector_type, sector_shares, sub_nmvoc )
  fn_scenario <- clean_scenario_name( scenario )

  var_atts <- build_nc_var_atts( fn_em, fn_sector, aggregate_sectors,
                                 sector_shares, sector_type )
  global_atts <- build_global_atts( em, fn_em, fn_scenario, sub_nmvoc,
                                    sector_shares, sector_type, var_atts )

  nc_file_name <- build_nc_filename( var_atts$em_var_name_fn, global_atts )
  nc_file_path <- paste0( output_dir, '/', nc_file_name )

  ### Prepare data for writing to NetCDF
  em_array <- unlist_for_ncdf( year_grids_list, ncdf_sectors, grid_resolution,
                               aggregate_sectors, sector_shares, nc_file_path,
                               sector_type )

  ### Define NetCDF dimensions
  bnds <- prep_bounds( grid_resolution, days_in_month, year_list, ncdf_sectors )
  ncdims <- define_ncdims( bnds, aggregate_sectors, sector_type )

  ### Define NetCDF variables
  ncvars <- define_ncvars( ncdims, var_atts, sector_type )

  ### Create and write the NetCDF file
  nc_new <- nc_create( nc_file_path, ncvars, force_v4 = T )

  ncvar_put( nc_new, ncvars$em_var,    em_array )
  ncvar_put( nc_new, ncvars$lon_bnds,  t( bnds$lon_bnds_data ) )
  ncvar_put( nc_new, ncvars$lat_bnds,  t( bnds$lat_bnds_data ) )
  ncvar_put( nc_new, ncvars$time_bnds, t( bnds$time_bnds_data ) )

  if ( !is.null( ncvars$sector_bnds ) ) {
    ncvar_put( nc_new, ncvars$sector_bnds, t( bnds$sector_bnds_data ) )
  }

  ### Add variable and global attributes
  add_variable_atts( nc_new, var_atts, !is.null( ncdims$sectordim ), sector_ids )

  invisible( mapply( ncatt_put, names( global_atts ), global_atts,
                     MoreArgs = list( nc = nc_new, varid = 0 ) ) )

  # Close and write the NetCDF file
  nc_close( nc_new )
}


# Add variable attributes to a NetCDF file
#
# Add the standard values for representing the grid and time variables. If there
# is sectoral data, add that as well.
#
# Args:
#    nc_new: A ncdf4 object
#    atts: List of the variable attributes
#    sector_var: Does the data have a dimension for sectors?
#    sector_ids: The sector ids, used only if there is a sector dimension
#
# Returns:
#    NULL
add_variable_atts <- function( nc_new, atts, sector_var, sector_ids ) {
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
  ncatt_put( nc_new, atts$em_var_name, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, atts$em_var_name, 'long_name', atts$longname )
  ncatt_put( nc_new, atts$em_var_name, 'missing_value', atts$missing_value, prec = 'float' )
}


build_global_atts <- function( em, fn_em, fn_scenario, sub_nmvoc, sector_shares,
                               sector_type, var_atts ) {
  creation_date   <- as.POSIXlt( Sys.time(), "UTC" )
  reporting_unit  <- build_reporting_unit( em, fn_em, sector_shares, sector_type, sub_nmvoc )
  product         <- paste0( if_else( sub_nmvoc, 'supplementary', 'primary' ), '-emissions-data' )
  title           <- paste( 'Future', var_atts$sector_long_name, 'of', fn_em )
  data_usage_tips <- paste(
    'These are monthly average', if_else( sector_shares, 's.', 'fluxes.' ),
    'Note that emissions are provided in uneven year intervals (2015, 2020,',
    'then at 10 year intervals) as these are the years for which projection',
    'data is available.'
  )

  default_atts <- list(
    Conventions        = 'CF-1.6',
    comment            = NULL,
    creation_date      = format( creation_date, format = '%Y-%m-%dT%H:%M:%SZ' ),
    data_structure     = 'grid',
    dataset_category   = 'emissions',
    data_usage_tips    = data_usage_tips,
    external_variables = 'gridcell_area',
    frequency          = 'mon',
    grid               = '0.5x0.5 degree latitude x longitude',
    grid_label         = 'gn',
    history            = format( creation_date, format = '%d-%m-%Y %H:%M:%S %p %Z' ),
    institution        = NULL,
    nominal_resolution = '50 km',
    product            = product,
    realm              = 'atmos',
    references         = NULL,
    reporting_unit     = reporting_unit,
    source             = NULL,
    source_version     = '1.0',
    table_id           = 'input4MIPs',
    target_mip         = 'ScenarioMIP',
    tracking_id        = paste0( "hdl:21.14100/", uuid() ),
    title              = title,
    variable_id        = var_atts$em_var_name
  )

  user_atts <- get_constant( 'NC_ATTS' )
  user_att_additions <- get_constant( 'NC_ATT_ADDITIONS' )

  defaults_to_add <- names( default_atts ) %!in% names( user_atts )
  global_atts <- c( user_atts, default_atts[ defaults_to_add ] )

  suffixes_to_add <- names( global_atts ) %in% names( user_att_additions )
  global_atts[ suffixes_to_add ] <- paste0( global_atts[ suffixes_to_add ],
                                            user_att_additions )

  # The global attribute source_id is a composition of other attributes
  source_id <- c( global_atts$institution_id, iam, fn_scenario,
                  gsub( "\\.", "-", global_atts$source_version ),
                  if ( sub_nmvoc ) 'supplemental-data' else NULL )
  global_atts$source_id <- paste( source_id, collapse = '-' )

  global_atts <- compile_template( global_atts )

  # Return sorted list of global attributes
  return( global_atts[ sort( names( global_atts ) ) ] )
}


build_nc_var_atts <- function( fn_em, fn_sector, aggregate_sectors, sector_shares, sector_type ) {
  em_var_name_fn <- paste( fn_em, fn_sector, sep = '-' )
  em_var_name <- gsub( "-", "_", em_var_name_fn )

  data_unit <- if_else( sector_shares, 'percent', 'kg m-2 s-1' )

  if ( sector_type == 'openburning' && aggregate_sectors ) {
    sector_long_name <- 'total open burning emissions'
  } else if ( sector_type == 'openburning' && sector_shares ) {
    sector_long_name <- 'open burning sector shares'
  } else if ( sector_type == 'openburning') {
    sector_long_name <- 'open burning'
  } else if ( sector_type == 'AIR-anthro') {
    sector_long_name <- 'aircraft anthropogenic emissions'
  } else {
    sector_long_name <- 'anthropogenic emissions'
  }

  longname <- paste( fn_em, toTitleCase( sector_long_name ) )

  list(
    em_var_name_fn   = em_var_name_fn,
    em_var_name      = em_var_name,
    data_unit        = data_unit,
    sector_long_name = sector_long_name,
    longname         = longname,
    missing_value    = 1e+20,
    nc_compression   = 5
  )
}


# Build the output filename
#
# Piece together the output filename from key attributes. Follows the convention
# specified for CMIP6 (http://goo.gl/r8up31) if all parts are provided.
#
# Args:
#   em_var_name_fn: The emission variable name without underscores
#   global_atts: A list of the NetCDF's global attributes
#
# Returns:
#   The output filename
build_nc_filename <- function( em_var_name_fn, global_atts ) {
  # These are global variables defined in all_module_functions.R
  time_range <- paste0( ds_start_year, '01-', ds_end_year, '12' )

  # Put together the pieces of the file name and separate with underscores
  # (Not all parts are required, but using c() removes the NULL values)
  filename <- c( em_var_name_fn,               global_atts$activity_id,
                 global_atts$dataset_category, global_atts$target_mip,
                 global_atts$source_id,        global_atts$grid_label,
                 time_range )

  filename <- paste( filename, collapse = '_' )

  return( paste0( filename, '.nc' ) )
}


# Build the reporting_unit global attribute
#
# The reporting_unit global attribute has several nuances, addressed here.
# - The emission 'VOC' is not allowed, so we report it as NMVOC
# - Sulfur is reported as SOx
# - If outputing sector shares, we need to report that data is a fraction
# - NOx emissions are reported as NO for open burning and NO2 for anthro
#
# Args:
#   em: The emission species
#   fn_em: The full emission species name
#   sector_shares: Are we outputting sector shares?
#   sector_type: Either 'openburning' or 'anthro'
#   sub_nmvoc: Is em a NMVOC sub species?
#
# Returns:
#   The value of the reporting_unit global attribute
build_reporting_unit <- function( em, fn_em, sector_shares, sector_type, sub_nmvoc ) {

  reporting_em <- switch ( em, Sulfur = 'SOx', VOC = 'NMVOC', em )

  if ( sub_nmvoc ) {
    reporting_unit <- paste( 'Mass flux of', fn_em, '(total mass emitted)' )
  } else if ( sector_shares ) {
    reporting_unit <- paste( 'Fraction of', reporting_em, 'from each land',
                             'category listed in the sector variable' )
  } else if ( reporting_em == 'NMVOC' ) {
    reporting_unit <- 'Mass flux of NMVOC (total mass emitted)'
  } else if ( reporting_em == 'BC' || reporting_em == 'OC' ) {
    reporting_unit <- paste0( 'Mass flux of ', reporting_em, ', reported as ',
                              'carbon mass' )
  } else {
    reporting_unit <- paste( 'Mass flux of', reporting_em )
  }

  if ( reporting_em == 'SOx' ) {
    reporting_unit <- paste0( reporting_unit, ', reported as SO2' )
  } else if ( reporting_em == 'NOx' && sector_type == 'openburning' ) {
    reporting_unit <- paste0( reporting_unit, ', reported as NO' )
  } else if ( reporting_em == 'NOx' && sector_type == 'anthro' ) {
    reporting_unit <- paste0( reporting_unit, ', reported as NO2' )
  }

  return( reporting_unit )
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


# Evaluate variables defined in config text
#
# Replace variables in a list of character vectors designated by double
# brackets. For example:
#   > info <- list( "My R version is [[R.version.string]]", "pi: [[pi]]" )
#   > compile_template(info)
#   [[1]]
#   [1] "My R version is R version 3.5.1 (2018-07-02)"
#
#   [[2]]
#   [1] "pi: 3.14159265358979"
#
# Does not work for list elements of length greater than one.
#
# Args:
#   l: List of character vectors
#
# Returns:
#   Compiled list of character vectors
compile_template <- function( l ) {
  lapply( l, function( x ) {
    splits <- strsplit( x, '(\\[\\[|\\]\\])' )[[1]]
    if ( length( splits ) == 1 ) return( x )
    splits[ c( F, T ) ] <- sapply( splits[ c( F, T ) ], get, parent.frame() )
    paste( splits, collapse = '' )
  })
}


define_ncdims <- function( bnds, aggregate_sectors, sector_type ) {
  londim <- ncdim_def( 'lon', 'degrees_east',  bnds$lons, longname = 'longitude' )
  latdim <- ncdim_def( 'lat', 'degrees_north', bnds$lats, longname = 'latitude' )
  timedim <- ncdim_def( 'time', paste0( 'days since 2015-01-01 0:0:0' ),
                        bnds$time_data, calendar = '365_day', longname = 'time',
                        unlim = T )

  dim_list <- list( londim = londim, latdim = latdim )

  # 3rd dimension is 'altitude' for aircraft and 'sector' for everything else,
  # unless the sectors are being aggregated.
  if ( !aggregate_sectors ) {
    if ( sector_type == 'AIR-anthro' )
      dim_list$levdim <- ncdim_def( 'level', 'km', bnds$levs, longname = 'altitude' )
    else
      dim_list$sectordim <- ncdim_def( 'sector', '', bnds$sectors, longname = 'sector' )
  }

  # Last dimension is time
  dim_list$timedim <- timedim

  return( dim_list )
}


define_ncvars <- function( dim_list, atts, sector_type ) {
  bndsdim <- ncdim_def( 'bound', '', 1:2, longname = 'bound', create_dimvar = F )

  lon_bnds  <- ncvar_def( 'lon_bnds',  '', list( bndsdim, dim_list$londim ),  prec = 'double' )
  lat_bnds  <- ncvar_def( 'lat_bnds',  '', list( bndsdim, dim_list$latdim ),  prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, dim_list$timedim ), prec = 'double' )

  em_var  <- ncvar_def( name        = atts$em_var_name,
                        units       = atts$data_unit,
                        dim         = dim_list,
                        missval     = atts$missing_value,
                        longname    = atts$longname,
                        compression = atts$nc_compression )


  var_list <- list( em_var = em_var, lon_bnds = lon_bnds, lat_bnds = lat_bnds,
                    time_bnds = time_bnds )

  if ( !is.null( dim_list$sectordim ) ) {
    var_list$sector_bnds <-
      ncvar_def( 'sector_bnds', '', list( bndsdim, dim_list$sectordim ), prec = 'double' )
  }

  return( var_list )
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
prep_bounds <- function( grid_resolution, days_in_month, year_list, ncdf_sectors ) {
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

  # For aircraft emissions, prepare level bounds data
  levs <- seq( 0.305, 14.945, 0.61 )

  return( list( lons      = lons,      lon_bnds_data    = lon_bnds_data,
                lats      = lats,      lat_bnds_data    = lat_bnds_data,
                time_data = time_data, time_bnds_data   = time_bnds_data,
                sectors   = sectors,   sector_bnds_data = sector_bnds_data,
                levs      = levs ) )
}


# Write global checksum csv file
#
# Build a data.frame of global totals (in kt) for each year, sector, and month
# for the given emission species.
#
# Args:
#   out_name: Name of the output file
#   year_grids_rtd: List of arrays, each containing emission totals for each
#     year in kt for each grid cell for each sector for each month
#   em: Emission species name
#   ncdf_sectors: Sector names
#   res: The grid resolution
#
# Returns:
#   The checksum data.frame
write_checksum <- function( out_name, year_grids_rtd, em, ncdf_sectors, res ) {
  KT_PER_KG <- 1e-06
  SEC_IN_MONTH <- days_in_month * 24 * 60 * 60
  NMONTHS <- 12L

  grid_dims <- dim( year_grids_rtd[[1]] )

  # Build conversion array to convert monthly values to kt. The dimensions are
  # (lon x lat x 12), where each value represents the conversion factor for
  # going from kg m-2 s-1 to kt per month for each grid cell for each month.
  grid_cell_column <- grid_area( res, all_lon = T )
  grid_cell_conv <- rep( t( grid_cell_column ), NMONTHS ) %>%
    array( dim = c( grid_dims[ 1:2 ], NMONTHS ) ) %>%
    sweep( 3, SEC_IN_MONTH * KT_PER_KG, `*` )

  # Convert to kt
  year_grids_kt <- lapply( year_grids_rtd, sweep, 1:3, grid_cell_conv, `*` )

  # Sum over all grid cells (converts 3d to 2d)
  checksum_df <- do.call( rbind, lapply( year_grids_kt, colSums, dims = 2 ) )

  year_list <- as.integer( substr( names( year_grids_rtd ), 2, 5 ) )

  out_df <- data.frame( em, seq( NMONTHS ), checksum_df ) %>%
    cbind( rep( year_list, each = NMONTHS ), . ) %>%
    setNames( c( 'year', 'em', 'month', ncdf_sectors ) ) %>%
    tidyr::gather( 'sector', 'global_total', ncdf_sectors ) %>%
    dplyr::mutate( units = 'kt' ) %>%
    dplyr::arrange( year, sector )

  # Write out file
  write.csv( out_df, out_name, row.names = F )

  out_df
}


# Write global difference diagnostics csv file
#
# Args:
#   global_sums: Data frame of emissions global totals per month and sector
#   out_name: Name of the output NetCDF file (used for naming the csv)
#   em: Emission species name
#
# Returns:
#   NULL
write_diffs <- function( global_sums, out_name, em ) {
  ERR_TOL <- get_constant( 'error_tolerance' )

  # gridding_sector_mapping is a global variable
  sector_mapping <- readData( 'GRIDDING', gridding_sector_mapping,
                              domain_extension = 'gridding-mappings/'  )

  # Original data
  in_df <- readData( 'MED_OUT', paste0( 'B.', iam, '_emissions_reformatted', '_', RUNSUFFIX ) ) %>%
    dplyr::filter( em == !!em ) %>%
    dplyr::select( sector_name = sector, one_of( make.names( year_list ) ) ) %>%
    dplyr::group_by( sector_name ) %>%
    dplyr::summarise_if( is.numeric, sum ) %>%
    tidyr::gather( year, value, make.names( year_list ) ) %>%
    dplyr::mutate( year = as.integer( sub( 'X', '', year ) ), global_total = value * 1000 ) # Convert to kt

  # Compare with the new global sums. First aggregate to the sector level, then
  # join on the longer sector name and join on the historical data.
  diff_df <- global_sums %>%
    dplyr::mutate( sector_short = as.character( sector ) ) %>%
    dplyr::select( -sector, -em, -month ) %>%
    dplyr::group_by( sector_short, year, units ) %>%
    dplyr::summarise( global_total = sum( global_total ) ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join( sector_mapping, by = 'sector_short' ) %>%
    dplyr::left_join( in_df, by = c('year', 'sector_name') ) %>%
    dplyr::mutate( pct_diff = ( global_total.x - global_total.y ) / global_total.y ) %>%
    dplyr::mutate( pct_diff = if_else( is.nan( pct_diff ), 0, pct_diff * 100 ) ) %>%
    dplyr::rename( grid_sum = global_total.x, orig_sum = global_total.y ) %>%
    dplyr::select( sector_name, year, units, grid_sum, orig_sum, pct_diff )

  largest_diff <- round( max( abs( diff_df$pct_diff ), na.rm = T ), 4 )

  # Warn if there are any differences greater than the error tolerance global
  # setting.
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

  writeData( diff_df, 'DIAG_OUT', paste( out_name, '_DIFF' ), meta = F )
}


# Convert list of yearly gridded data into single array for NetCDF
#
# Transform the data into the format required to write to the NetCDF object.
# This means rotating the lat/lon dimensions of each array and flattening the
# list into a single array.
#
# This function also writes out diagnostics about the gridded data.
#
# Args:
#   year_grids_list: List of arrays or list of lists of arrays
#   ncdf_sectors: Sector names
#   grid_resolution: Resolution of the data
#   aggregate_sectors: Should the sectors be summed?
#   sector_shares: Represent values as their share compared to other sectors?
#   diag_fname: NetCDF file path to use for creating diagnostic file names
#   sector_type: The sector type
#
# Returns:
#   The processed multidimensional array
unlist_for_ncdf <- function( year_grids_list, ncdf_sectors, grid_resolution,
                             aggregate_sectors, sector_shares, diag_fname,
                             sector_type ) {
  NMONTHS <- 12L
  NSECTORS <- length( ncdf_sectors )
  GENERATE_PLOTS <- get_constant( 'diagnostic_plots' )

  # Define array dimensions:
  #    each year array:    (lon x lat x sectors x months in year)
  #    array of all years: (lon x lat x sectors x all months)
  lon_res <- as.integer( 360 / grid_resolution )
  lat_res <- as.integer( 180 / grid_resolution )
  all_months <- length( year_grids_list ) * NMONTHS
  all_years_dims <- as.integer( c( lon_res, lat_res, NSECTORS, all_months ) )

  # Flip lat and lon to accommodate nc write-in
  year_grids_rtd <- rotate_lat_lon( year_grids_list )

  # Flatten list of all year grids into one large array
  em_array <- array( unlist( year_grids_rtd, use.names = F ), all_years_dims )

  # Apply transformations on the sector dimension (the 3rd one), if requested
  if ( aggregate_sectors ) {
    em_array <- apply( em_array, c( 1, 2, 4 ), sum )
  } else if ( sector_shares ) {
    em_array <- prop.table( em_array, c( 1, 2, 4 ) )
    em_array[ is.nan( em_array ) ] <- 0
  }

  ### Diagnostics:

  # Checksum and diff functions need a singly nested list
  if ( is.list( year_grids_rtd[[1]] ) ) {
    grid_dims <- dim( year_grids_rtd[[1]][[1]] )
    year_grids_rtd <- lapply( year_grids_rtd, unlist, use.names = F )
    year_grids_rtd <- lapply( year_grids_rtd, `dim<-`, c( grid_dims, NSECTORS ) )
  }

  # Always write out checksum and diff files
  out_path <- gsub( '.nc', '.csv', diag_fname, fixed = T )

  # Remove everything to the last slash '.*/', capture everything up to the
  # extension '([^/]+)', then the extension '\\.csv'
  out_name <- sub( '.*/([^/]+)\\.csv', '\\1', out_path )

  global_sums <- write_checksum( out_path, year_grids_rtd, em, ncdf_sectors, grid_resolution )
  write_diffs( global_sums, out_name, em )

  # Generate diagnostics
  if ( GENERATE_PLOTS ) {
    source( filePath( 'DIAG', 'generate_plots', '.R' ) )
    diag_cells <- extract_diag_cells( year_grids_rtd, ncdf_sectors, lat_res, em )
    generate_plots( global_sums, diag_cells, out_name, em, sector_type )
  }

  return( em_array )
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

