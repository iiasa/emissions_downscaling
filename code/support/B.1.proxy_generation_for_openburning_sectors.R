# Copyright 2018 Battelle Memorial Institute

# this script creates  proxies for IAMC sectors Forest Burning, Grassland Burning, and Peat Burning for all soppourted species except CO2
# using biomassburning grids version 1.2
# this script should be updated to include CO2 if newer version of biomassburning grids contains CO2.

# basic logics:
# (1) read in emissions from year 2006 to 2015 (10 years)
# (2) seperate into 6 sectors
# (3) compute 10 years average for each sector
# (4) generate proxy for 3 sectors as year 2015
# (5) generate seasonality profile based on proxies.
# byproducts: seasonality profile for sector FRTB, GRSB, and PEAT.

# Run from the input directory

# ----
# 0. set up dir and libs
library( 'ncdf4' )
library( 'raster' )

# ----
# 1. set up some basics
proxy_year <- 2015
em_list <-c( 'BC', 'CH4', 'CO', 'NOx', 'NH3', 'NMVOC-bulk', 'OC', 'SO2' )
sector_list <- c( 'AGRI', 'BORF', 'DEFO', 'PEAT', 'SAVA', 'TEMF' )
meta_var_name <- c( 'lat_bnds', 'latitude', 'lon_bnds', 'longitude', 'time', 'time_bnds', "gridcellarea" )
em_nc_path <- './biomass_burning_emissions_1_2/emissions'
sec_nc_path <- './biomass_burning_emissions_1_2/sectoral_percentage'


# ----
# 2. start the processing in a huge function
thingsToDoPerSpecies <- function( em ) {

  # ---
  # 2.1. read in emissions
  nc_list <- list.files( em_nc_path, pattern = paste0( em, '-em' ) )
  nc_file <- grep( '185001-201512', nc_list, value = T )

  temp_nc <- nc_open( paste0( em_nc_path, '/', nc_file ) )
  var_list <- unlist( lapply( seq_along( temp_nc$var ), function( i ) { temp_nc$var[[ i ]]$name } ) )
  var_name <- var_list[ which( !( var_list %in% meta_var_name ) ) ]
  temp_var <- ncvar_get( temp_nc, var_name, start = c( 1, 1, 1873 ) )
  nc_close( temp_nc )
  em_block <- temp_var
  rm( temp_var )
  em_block[ is.na( em_block ) ] <- 0

  # ---
  # 2.2. generate proxy for each sector
  processAsector <- function( sector ) {
    # --
    # read in sector ncs
    sec_filename <- list.files( sec_nc_path, pattern = paste0( em, '-percentage-', sector, '-em' ) )
    temp_nc <- nc_open( paste0( sec_nc_path, '/', sec_filename ) )
    var_list <- unlist( lapply( seq_along( temp_nc$var ), function( i ) { temp_nc$var[[ i ]]$name } ) )
    var_name <- var_list[ which( !( var_list %in% meta_var_name ) ) ]
    temp_var <- ncvar_get( temp_nc, var_name, start = c( 1, 1, 3073 ) )
    nc_close( temp_nc )
    sector_block <- temp_var
    rm( temp_var )
    sector_block[ is.na( sector_block ) ] <- 0
    sector_block <- sector_block * 0.01 # percentage conversion

    # --
    # apply sector block onto emission block
    em_sec_block <- em_block * sector_block

    # --
    # adjust dimension for each layer of time dimension
    em_sec_block_flipped <-  array( unlist( lapply( 1 : dim( em_sec_block )[3], function( i ) {
      t( em_sec_block[ , , i ] ) } ) ), dim = c( 720, 1440, 120 ) )

    return( em_sec_block_flipped )
  }

  em_block_AWB <- processAsector( 'AGRI' )
  em_block_BORF <- processAsector( 'BORF' )
  em_block_DEFO <- processAsector( 'DEFO' )
  em_block_PEAT <- processAsector( 'PEAT' )
  em_block_SAVA <- processAsector( 'SAVA' )
  em_block_TEMF <- processAsector( 'TEMF' )

  em_block_AWB <- em_block_AWB
  em_block_FRTB <- em_block_BORF + em_block_DEFO + em_block_TEMF
  em_block_GRSB <- em_block_SAVA
  em_block_PEAT <- em_block_PEAT

  # --
  # compute 10 years mean for each sector
  meanGrid_10years <- function( em_block_sector ) {
    temp_month_list <- 1 : 12
    temp_year_list <- 1 : 10

    storage <- c( )
    for ( month in temp_month_list ) {
      block_index <- ( temp_year_list - 1 ) * 12 + month
      em_block_sector_month <- em_block_sector[ , , block_index ]
      em_block_sector_month_mean <- apply( em_block_sector_month, c( 1, 2 ), sum ) / 10
      temp_ras <- raster( em_block_sector_month_mean )
      temp_ras <- aggregate( temp_ras, fact = 2, fun = sum )
      em_block_sector_month_mean  <- as.matrix( temp_ras )
      storage <- c( storage, as.vector( em_block_sector_month_mean ) )
      }
    mean_block <- array( storage, dim = c( 360, 720, 12 ) )
    return( mean_block )
  }
  em_block_AWB_mean <- meanGrid_10years( em_block_AWB )
  em_block_FRTB_mean <- meanGrid_10years( em_block_FRTB )
  em_block_GRSB_mean <- meanGrid_10years( em_block_GRSB )
  em_block_PEAT_mean <- meanGrid_10years( em_block_PEAT )

  # --
  # create proxy
  proxy_AWB <- apply( em_block_AWB_mean, c( 1, 2 ), mean )
  proxy_FRTB <- apply( em_block_FRTB_mean, c( 1, 2 ), mean )
  proxy_GRSB <- apply( em_block_GRSB_mean, c( 1, 2 ), mean )
  proxy_PEAT <- apply( em_block_PEAT_mean, c( 1, 2 ), mean )

  # --
  # create seasonality profile
  sea_AWB <- em_block_AWB_mean / array( apply( em_block_AWB_mean, c( 1, 2 ), sum ), dim = c( 360, 720, 12 ) )
  sea_FRTB <- em_block_FRTB_mean / array( apply( em_block_FRTB_mean, c( 1, 2 ), sum ), dim = c( 360, 720, 12 ) )
  sea_GRSB <- em_block_GRSB_mean / array( apply( em_block_GRSB_mean, c( 1, 2 ), sum ), dim = c( 360, 720, 12 ) )
  sea_PEAT <- em_block_PEAT_mean / array( apply( em_block_PEAT_mean, c( 1, 2 ), sum ), dim = c( 360, 720, 12 ) )

  # fill the rest NaN/NA values into 1/12 as no seasonality variation
  # Those cells ( probably also unuseful ) will be masked
  # out by annual emission  grids anyways if there's no emission
  # but the advantage of having non-zero value in seasonality grids
  # is that it doesn't mask out if emission is not within any country.
  sea_AWB[ is.na( sea_AWB ) ] <- 1/12
  sea_FRTB[ is.na( sea_FRTB ) ] <- 1/12
  sea_GRSB[ is.na( sea_GRSB ) ] <- 1/12
  sea_PEAT[ is.na( sea_PEAT ) ] <- 1/12

  # --
  # write out
  if ( em == 'NMVOC-bulk' ) { em <- 'NMVOC' }
  if ( em == 'SO2' ) { em <- 'Sulfur' }

  out_name <- paste0( em, '_', proxy_year, '_AWB' )
  assign( out_name, proxy_AWB )
  save( list = out_name, file = paste0( './gridding/proxy-CEDS9/', out_name ) )

  out_name <- paste0( em, '_', proxy_year, '_FRTB' )
  assign( out_name, proxy_FRTB )
  save( list = out_name, file = paste0( './gridding/proxy-CEDS9/', out_name ) )

  out_name <- paste0( em, '_', proxy_year, '_GRSB' )
  assign( out_name, proxy_GRSB )
  save( list = out_name, file = paste0( './gridding/proxy-CEDS9/', out_name ) )

  out_name <- paste0( em, '_', proxy_year, '_PEAT' )
  assign( out_name, proxy_PEAT )
  save( list = out_name, file = paste0( './gridding/proxy-CEDS9/', out_name ) )

  out_name <- paste0( 'AWB_', em, '_seasonality' )
  assign( out_name, sea_AWB )
  save( list = out_name, file = paste0( './gridding/seasonality-CEDS9/', out_name ) )

  out_name <- paste0( 'FRTB_', em, '_seasonality' )
  assign( out_name, sea_FRTB )
  save( list = out_name, file = paste0( './gridding/seasonality-CEDS9/', out_name ) )

  out_name <- paste0( 'GRSB_', em, '_seasonality' )
  assign( out_name, sea_GRSB )
  save( list = out_name, file = paste0( './gridding/seasonality-CEDS9/', out_name ) )

  out_name <- paste0( 'PEAT_', em, '_seasonality' )
  assign( out_name, sea_PEAT )
  save( list = out_name, file = paste0( './gridding/seasonality-CEDS9/', out_name ) )

}

invisible( lapply( em_list, thingsToDoPerSpecies ) )
