# Copyright 2018 Battelle Memorial Institute

# globalcharts.R
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


# Replace with the path to your final output directory
FINAL_OUTPUT_DIR <- ''
FINAL_OUTPUT_DIR <- "C:/Users/brau074/tmp/downscale-analysis"

# Replace with the path to your historical emissions file
HISTORICAL_EMS <- 'CEDS_by_country_by_CEDS_sector_with_luc_all_em.csv'


# Given a scenario name (e.g. GCAM4-ssp434), output a facetted plot containing
# each emission's global totals, including historical data.
plot_scenario_ems <- function(scen, historical) {
  print(paste("Plotting global emissions for scenario", scen))

  # Get filenames of anthro, AIR, and openburning (not share) emissions
  scen_files <- list.files(pattern = paste0(scen, '-1-1.*.csv'))
  sect_files <- unique(sub('[^-]*', '', scen_files))
  sect_files <- grep('openburning-share', sect_files, value = T, invert = T)

  all_sector <- lapply(sect_files, function(sect) {
    sector <- grep(sect, scen_files, value = T)
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



wd <- getwd()
setwd(FINAL_OUTPUT_DIR)

historical <- read.csv(HISTORICAL_EMS, stringsAsFactors = F) %>%
  dplyr::filter(!(em == 'CO2' & grepl('Burning', sector))) %>%
  dplyr::group_by(em) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  tidyr::gather('year', 'value', X1970:X2015)

# Get the base name of each scenario (e.g. REMIND-MAGPIE-ssp534-over)
all_files <- list.files(pattern = ".*input4MIPs.*csv")
scenarios <- unique(sub('.*IAMC-(.*)-1-1.*', '\\1', all_files))

sapply(scenarios, plot_scenario_ems, historical)

setwd(wd)
