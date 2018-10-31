# Support functions for plotting diagnostic data.
#
# Caleb Braun
# 8/22/18
#
# This file uses csv files of historical gridded data found at
# http://www.globalchange.umd.edu/data/ceds/checksum_1750_2014.zip. It compares
# the latest years in that timeseries to the output csv files that are generated
# with each netCDF file from emissions_downscaling.

# Extract values at key grid cells
#
# Use matrix indexing to extract just the cells of interest speficied by
# diagnostic_cells for each year, month, and sector.
#
# Args:
#   year_grids_list: List of arrays, each containing emission totals for each
#     year in kt for each grid cell for each sector for each month
#   ncdf_sectors: Names of sectors in the order given in year_grids_list
#   lat_res: Number of latitude cells
#   em: Emission species name
#
# Returns:
#   List of data.frames of diagnostic cell values for each year
extract_diag_cells <- function( year_grids_list, ncdf_sectors, lat_res, em ) {
  # A data.frame specifying cells to pull out
  diagnostic_cells <- readData( 'GRIDDING', 'diagnostic_cells',
                                domain_extension = 'gridding-mappings/' )

  diagnostic_cells_indices <- diagnostic_cells[ c('col', 'row') ] %>%
    tidyr::crossing( month = 1:12, sector = 1:length( ncdf_sectors ) ) %>%
    dplyr::arrange( sector, month ) %>%
    dplyr::mutate( row = lat_res - row + 1L ) %>% # Account for rotation
    as.matrix()

  # Use matrix indexing to pull all month/sector/lat/lon values from grids
  lapply( names( year_grids_list ), function( Xyear ) {
    year_grid <- year_grids_list[[ Xyear ]]
    cell_values <- year_grid[ diagnostic_cells_indices ]

    cbind( diagnostic_cells,
           data.frame( em = em,
                       sector = ncdf_sectors[ diagnostic_cells_indices[ , 4 ] ],
                       year = as.integer( substr( Xyear, 2, 5 ) ),
                       month = diagnostic_cells_indices[ , 3 ],
                       unit = 'kt',
                       value = cell_values / 1000,
                       stringsAsFactors = F) )
  })
}


generate_plots <- function( global_sums, diag_cells, diag_fname, em, sector_type ) {
  printLog( 'Generating diagnostic plots' )

  # Write diagnostic cells csv file
  diagnostic_cells <- do.call( 'rbind', diag_cells )
  out_name <- paste( diag_fname, '_cells' )
  writeData( diagnostic_cells, 'DIAG_OUT', out_name, meta = F )

  if ( sub_nmvoc ) {
    printLog( 'NMVOC diagnostics not currently supported' )
    # return( sub_nmvoc_plots( global_sums, diag_fname, em, sector_type ) )
  } else if ( sector_type == 'anthro' ) {
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
  } else if ( sector_type == 'openburning' ) {
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

  invisible( NULL )
}

# Given a scenario name (e.g. GCAM4-ssp434), output a facetted plot for each
# VOC containing the emission's global totals by sector
#
# Args:
#   scen: Scenario name
#   historical: Historical data correctly formatted for comparison
#   sector_type: One of 'openburning' or 'anthro'
plot_scenario_ems <- function( scen, historical, sector_type ) {
  print( paste("Plotting global emissions for scenario", scen) )

  # Get filenames of anthro, AIR, and OPENBURNING (not share) emissions
  if ( sector_type == 'openburning' ) {
    scen_files <- list.files( pattern = paste0('NMVOC.*-em-speciated.*', scen, '-1-1.*.csv') )
    stopifnot( length(scen_files) == 25 ) # The number of sub-VOCs for burning
  } else {
    scen_files <- list.files(pattern = paste0('VOC[0-2].*', scen, '-1-1.*.csv'))
    stopifnot(length(scen_files) == 23) # The number of sub-VOCs
  }

  lapply(scen_files, function(sub_voc) {
    sub_voc_ems <- read.csv(sub_voc, stringsAsFactors = F)
    stopifnot('units' %in% names(sub_voc_ems))
    if (OPENBURNING) {
      sub_voc_num <- sub('NMVOC-(.+)-em.*', '\\1', sub_voc)
      sub_voc_num <- sub('-', '_', sub_voc_num)
    } else {
      sub_voc_num <- substr(sub_voc, 1, 5)
    }

    hist <- dplyr::filter(historical, em == sub_voc_num) %>%
      dplyr::mutate(sector = 'All Sectors')

    sub_voc_ems <- sub_voc_ems %>%
      dplyr::mutate(global_total = if_else(units == 'Mt', global_total * 1000, global_total)) %>%
      dplyr::mutate(units = if_else(units == 'Mt', 'kt', units)) %>%
      dplyr::group_by(year, em, sector, units) %>%
      dplyr::summarise(global_total = sum(global_total)) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(hist)

    totals <- sub_voc_ems %>%
      dplyr::group_by(year, em, units) %>%
      dplyr::summarise(global_total = sum(global_total)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(sector = 'All Sectors')

    sub_voc_ems_total <- sub_voc_ems %>%
      dplyr::bind_rows(totals) %>%
      dplyr::mutate(sector = factor(sector, levels = unique(c(sub_voc_ems$sector, 'All Sectors'))))

    # Add dummy data to set ylimits to 0 or minimum value (to account for negative
    # CO2 emissions). See https://stackoverflow.com/q/18046051/8715278
    dummy <- sub_voc_ems %>%
      dplyr::group_by(em) %>%
      dplyr::summarise(year = sub_voc_ems$year[1], global_total = min(min(0, global_total)))

    ggplot(data = sub_voc_ems_total, aes(x=year, y=global_total)) +
      geom_line() +
      geom_vline(xintercept = 2015, alpha = 0.5, linetype = 'dotted') +
      geom_blank(data = dummy) +
      labs(y = "kt") +
      scale_x_continuous(breaks=seq(2000, 2100, 15)) +
      facet_wrap(. ~ sector, scales = "free_y") +
      ggtitle(paste(scen, "global", sub_voc_num, "emissions"))

    outname <- paste0(DIAG_OUTPUT_DIR, '/', scen, '_', sub_voc_num, '_global_ems.png')
    ggsave(outname, width = 9, height = 5)
  })
}


# Set up and call charting function ---------------------------------------
#
# Start by then reading in the historical files. Note that these historical
# files must be downloaded from ESFG and processed with make_checksums.R,
# however some may be found on the CEDS page linked above.
sub_nmvoc_plots <- function( global_sums, diag_fname, em, sector_type ) {
  BURN_NMVOC_HIST_FILE_PTRN <- 'NMVOC-.*-em-biomassburning.*201512.*\\.csv'
  ANTH_NMVOC_HIST_FILE_PTRN <- 'VOC[0-2].*201412.csv'
  NMVOC_REF_DIR <- filePath( 'REF_EM', 'nmvoc', '/' )

  if ( sector_type == 'openburning' ) {
    file_pattern <- BURN_NMVOC_HIST_FILE_PTRN
  } else if ( sector_type == 'anthro' )  {
    file_pattern <- ANTH_NMVOC_HIST_FILE_PTRN
  } else {
    return( invisible( NULL ) )
  }

  hist_file_names <- list.files( NMVOC_REF_DIR, file_pattern, full.names = T )

  # Return if we can't find any historical files
  if ( length( hist_file_names ) == 0 ) return( invisibile( NULL ) )

  # Combine historical files for all VOCs and aggregate to year level. Because
  # there is a discrepancy in the historical CEDS checksum files, both 'value'
  # and 'global_total' are allowed for the total emission value column. The year
  # 2015 is filtered out of the historical data, because that is the start year
  # for the 'future' predictions.
  historical <- do.call( rbind, lapply( hist_file_names, read.csv, stringsAsFactors = F ) )
  if ( 'value' %in% names( historical ) ) {
    historical <- dplyr::rename( historical, global_total = value )
  }

  historical <- historical %>%
    dplyr::filter( year != 2015 ) %>%
    dplyr::group_by( year, em, sector, units ) %>%
    dplyr::summarise( global_total = sum( global_total ) ) %>%
    dplyr::ungroup()

  plot_scenario_ems( fn_scenario, historical, sector_type )
}
