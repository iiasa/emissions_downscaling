# Copyright 2018 Battelle Memorial Institute

# voc_speciation.R
#
# Generate charts analyzing VOC speciation
#
# Caleb Braun
# 6/22/18
#
# This file uses the output csv files that are generated with each netCDF file.
#
# Must be run with emissions_downscaling/input as working directory

library(dplyr)
library(tidyr)
library(ggplot2)

# Run script when sourced even if on own
if (!exists('PARAM_DIR')) {
  PARAM_DIR <- "../code/parameters/"
  source( paste0( PARAM_DIR, "header.R" ) )
  script_name <- "global_total_ems.R"
  initialize( script_name, NULL, NULL )

  modc_out <- "../final-output/module-C"
  domainmapping <- read.csv( DOMAINPATHMAP, stringsAsFactors = F )
}

modc_out <- 'C:/Users/brau074/tmp/speciated-vocs-6-27'

generate_global_total_ems <- function() {

  # Get filenames of anthro sub-VOCs
  all_files <- list.files(modc_out, '.*supplemental.*csv')
  scenarios <- all_files %>%
    data.frame(fn = ., stringsAsFactors = F) %>%
    tidyr::separate(fn, c('em', 'scenario'), '_', extra = 'merge') %>%
    dplyr::pull(scenario) %>%
    unique()

  historical <- read_in_reference_ems()
  lapply(scenarios, plot_scenario_ems, historical, all_files)
}


# Reads in and prepares historical emissions
#
# - Removes non-anthro sectors
# - Maps to CEDS9
# - Aggregates to global total emissions by sector and year
# - Changes the global sector 'Industrial Process and Product Use' to map to SHP
#   because it represents tanker loading emissions
read_in_reference_ems <- function() {
  HISTORICAL_EMS <- get_constant( 'reference_emissions' )
  historical_ems_fname <- paste0( 'CEDS_CMIP6_to_2015/', HISTORICAL_EMS )
  sector_mapping <- readData('MAPPINGS', 'CEDS_sector_mapping' ) %>%
    dplyr::select(sector = CEDS16, CEDS9 = CEdS9_abr) %>%
    dplyr::distinct()

  readData( 'REF_EM', file_name = historical_ems_fname ) %>%
    dplyr::filter(em == 'VOC' | em == 'NMVOC',
                  !grepl('Burning', sector), sector != "Aircraft") %>%
    dplyr::left_join(sector_mapping, by = 'sector') %>%
    dplyr::mutate(CEDS9 = if_else(sector == 'Industrial Process and Product Use' &
                                    iso == 'global', 'SHP', CEDS9)) %>%
    dplyr::mutate(em = "NMVOC", sector = CEDS9) %>%
    dplyr::group_by(em, sector) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::gather('year', 'global_total', starts_with('X')) %>%
    dplyr::mutate(global_total = global_total * 1000) # Convert kt to Mt
}

# Given a scenario name (e.g. GCAM4-ssp434), output a facetted plot containing
# each emission's global totals, including historical data.
plot_scenario_ems <- function(scen, historical, all_files) {
  scen_short_name <- sub('input4MIPs_emissions_(.*)-supplemental.*', '\\1', scen)
  printLog(paste("Plotting global emissions for", scen_short_name))

  sectors <- paste0(modc_out, '/', grep(scen, all_files, value = T))
  sectors <- grep('openburning-share', sectors, value = T, invert = T)
  all_sectors <- do.call(rbind, lapply(sectors, read.csv, stringsAsFactors = F))
  agg_sectors <- all_sectors %>%
    dplyr::group_by(em, sector, year) %>%
    dplyr::summarise(global_total = sum(global_total)) %>%
    dplyr::mutate(source = 'NMVOC subspecies')

  agg <- agg_sectors %>%
    dplyr::group_by(em, year) %>%
    dplyr::summarise(global_total = sum(global_total))

  voc_regex <- paste0('NMVOC-em-anthro.*', scen_short_name, '.*csv')
  voc_fname <- list.files(modc_out, voc_regex, full.names = T)
  stopifnot(length(voc_fname) == 1)
  total_voc <- read.csv(voc_fname, stringsAsFactors = F) %>%
    dplyr::group_by(em, sector, year) %>%
    dplyr::summarise(global_total = sum(global_total)) %>%
    dplyr::mutate(source = 'NMVOC bulk total')

  total_all <- historical %>%
    dplyr::mutate(year = as.integer(substr(year, 2, 5)),
                  source = 'NMVOC historical total') %>%
    dplyr::bind_rows(total_voc)

  total_agg <- total_all %>%
    dplyr::group_by(em, year, source) %>%
    dplyr::summarise(global_total = sum(global_total))

  ggplot() +
    geom_area(data = agg_sectors, aes(x=year, y=global_total, fill=em), color = '#7f7f7f') +
    geom_line(data = total_all, aes(x=year, y=global_total, color=source), size=1) +
    geom_vline(xintercept = 2015, alpha = 0.5, linetype = 'dotted') +
    labs(x = "Year", y = "kt") +
    scale_fill_viridis_d(direction=-1, option="A") +
    scale_color_manual(name = "Source", values = c('black', 'grey')) +
    facet_wrap(. ~ sector, scales = 'free_y') +
    ggtitle(paste(scen_short_name, "Global Anthro NMVOC Emissions"))

  ggsave(filePath("DIAG_OUT", paste0(scen_short_name, '_global_vocs_sector'), '.png'),
         width = 8, height = 6)

  ggplot() +
    geom_area(data = agg, aes(x=year, y=global_total, fill=em), color = '#7f7f7f') +
    geom_line(data = total_agg, aes(x=year, y=global_total, color=source), size=1) +
    geom_vline(xintercept = 2015, alpha = 0.5, linetype = 'dotted') +
    labs(x = "Year", y = "Mt") +
    scale_fill_viridis_d(direction=-1, option="A") +
    scale_color_manual(name = "Source", values = c('black', 'grey')) +
    ggtitle(paste(scen_short_name, "Global Anthro NMVOC Emissions"))

  ggsave(filePath("DIAG_OUT", paste0(scen_short_name, '_global_vocs'), '.png'),
         width = 8, height = 6)
}


generate_global_total_ems()
