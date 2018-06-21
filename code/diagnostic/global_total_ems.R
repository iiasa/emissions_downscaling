# global_total_ems.R
#
# Generate line graphs of total emissions for each emission for each scenario.
#
# Caleb Braun
# 6/21/18
#
# This file uses the output csv files that are generated with each netCDF file.
# Note that CO2 openburning emissions are removed from the historical dataset
# before comparison, as emissions_downscaling does not currently generate CO2
# emissions for burning sectors.

library(dplyr)
library(tidyr)
library(ggplot2)


generate_global_total_ems <- function() {

  # Get filenames of anthro, AIR, and openburning (not share) emissions
  all_files <- list.files(modc_out, '.*csv')
  scenarios <- all_files %>%
    data.frame(fn = ., stringsAsFactors = F) %>%
    tidyr::separate(fn, c('em', 'scenario'), '_', extra = 'merge') %>%
    dplyr::filter(!grepl('openburning-share', em)) %>%
    dplyr::pull(scenario) %>%
    unique()

  historical <- read_in_reference_ems()
  sapply(scenarios, plot_scenario_ems, historical, all_files)
}

read_in_reference_ems <- function() {
  HISTORICAL_EMS <- get_global_constant( 'reference_emissions' )
  historical_ems_fname <- paste0( 'CEDS_CMIP6_to_2015/', HISTORICAL_EMS )

  readData( 'REF_EM', file_name = historical_ems_fname ) %>%
    dplyr::filter(!(em == 'CO2' & grepl('Burning', sector))) %>%
    dplyr::group_by(em) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::gather('year', 'value', starts_with('X'))
}

# Given a scenario name (e.g. GCAM4-ssp434), output a facetted plot containing
# each emission's global totals, including historical data.
plot_scenario_ems <- function(scen, historical, all_files) {
  print(paste("Plotting global emissions for", sub('(.*)_gn_.*', '\\1', scen)))

  all_sector <- lapply(scen, function(sect) {
    sector <- paste0(modc_out, '/', grep(sect, all_files, value = T))
    sector_comb <- do.call(rbind, lapply(sector, read.csv, stringsAsFactors = F))
    sector_comb %>%
      dplyr::group_by(em, sector, year) %>%
      dplyr::summarise(value = sum(value))
  })

  agg_sector <- do.call(rbind, all_sector) %>%
    dplyr::group_by(em, sector, year) %>%
    dplyr::summarise(value = sum(value))

  agg <- dplyr::group_by(agg_sector, em, year) %>%
    dplyr::summarise(value = sum(value))

  all_ems <- historical %>%
    dplyr::mutate(year = as.integer(substr(year, 2, 5))) %>%
    dplyr::bind_rows(agg)

  # Add dummy data to set ylimits to 0 or minimum value (to account for negative
  # CO2 emissions). See https://stackoverflow.com/q/18046051/8715278
  dummy <- all_ems %>%
    dplyr::group_by(em) %>%
    dplyr::summarise(year = all_ems$year[1], value = min(min(0, value)))

  ggplot(all_ems, aes(x=year, y=value, group = em)) +
    geom_line() +
    geom_vline(xintercept = 2015, alpha = 0.5, linetype = 'dotted') +
    geom_blank(data = dummy) +
    labs(y = "Mt") +
    facet_wrap(. ~ em, scales = "free_y") +
    ggtitle(paste(scen, "global emissions"))

  ggsave(paste0(scen, '_global_ems.png'), width = 6, height = 4)
}


# Run script when sourced
generate_global_total_ems()
