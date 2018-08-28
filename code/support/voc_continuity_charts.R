# voc_continuity_charts.R
#
# Generate line graphs combining historical and future emissions for sub-VOCs
#
# Caleb Braun
# 6/26/18
#
# This file uses csv files of historical gridded data found at
# http://www.globalchange.umd.edu/data/ceds/checksum_1750_2014.zip. It compares
# the latest years in that timeseries to the output csv files that are generated
# with each netCDF file from emissions_downscaling.

library(dplyr)
library(tidyr)
library(ggplot2)


# Replace with the path to your final output directory
FINAL_OUTPUT_DIR <- "C:/Users/brau074/Documents/emissions_downscaling/final-output/module-C"

# Replace with the path to your historical emissions files
HISTORICAL_EMS <- 'C:/Users/brau074/Downloads/historicalVOC/VUA_SMALL'

# Replace with the path to where you want the plots saved
DIAG_OUTPUT_DIR <- "C:/Users/brau074/Documents/emissions_downscaling/diagnostic-output/nmvoc_meta"

OPENBURNING <- TRUE
BURN_NMVOC_HIST_FILE_PTRN <- 'NMVOC-.*-em-biomassburning.*201512.*\\.csv'
ANTH_NMVOC_HIST_FILE_PTRN <- 'VOC[0-2].*201412.csv'

# Given a scenario name (e.g. GCAM4-ssp434), output a facetted plot for each
# VOC containing the emission's global totals by sector
plot_scenario_ems <- function(scen, historical, OPENBURNING = F) {
  print(paste("Plotting global emissions for scenario", scen))

  # Get filenames of anthro, AIR, and OPENBURNING (not share) emissions
  if (OPENBURNING) {
    scen_files <- list.files(pattern = paste0('NMVOC.*-em-speciated.*', scen, '-1-1.*.csv'))
    stopifnot(length(scen_files) == 25) # The number of sub-VOCs for burning
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
# Start by remembering which directory we started in for minimum side effects,
# then read in the historical files. Note that these historical files must be
# downloaded from ESFG and processed with make_checksums.R, however some may be
# found on the CEDS page linked above.
wd <- getwd()
setwd(HISTORICAL_EMS)

file_pattern <- if (OPENBURNING) BURN_NMVOC_HIST_FILE_PTRN else ANTH_NMVOC_HIST_FILE_PTRN
hist_file_names <- list.files(pattern = file_pattern)

# Combine historical files for all VOCs and aggregate to year level. Because
# there is a discrepancy in the historical CEDS checksum files, both 'value' and
# 'global_total' are allowed for the total emission value column. The year 2015
# is filtered out of the historical data, because that is the start year for
# the 'future' predictions.
historical <- do.call(rbind, lapply(hist_file_names, read.csv, stringsAsFactors = F))
if ('value' %in% names(historical)) {
  historical <- dplyr::rename(historical, global_total = value)
}
historical <- historical %>%
  dplyr::filter(year != 2015) %>%
  dplyr::group_by(year, em, sector, units) %>%
  dplyr::summarise(global_total = sum(global_total)) %>%
  dplyr::ungroup()

# Get the base name of each scenario (e.g. REMIND-MAGPIE-ssp534-over)
setwd(FINAL_OUTPUT_DIR)
all_files <- list.files(pattern = "NMVOC-.*em-speciated.*.csv")
scenarios <- unique(sub('.*IAMC-(.*)-1-1.*', '\\1', all_files))

sapply(scenarios, plot_scenario_ems, historical, OPENBURNING = T)

setwd(wd)

stop()



# bulkVOCS ----------------------------------------------------------------

bulk_NMVOC <- read.csv('C:/Users/brau074/tmp/nmvoc-6-25/VOC-em-anthro_input4MIPs_emissions_ScenarioMIP_IAMC-REMIND-MAGPIE-ssp585-1-1_gn_201501-210012.csv', stringsAsFactors = F)
hist_NMVOC <- read.csv(paste0(HISTORICAL_EMS, '/NMVOC-em-anthro_input4MIPs_emissions_CMIP_CEDS-2017-05-18_gn_200001-201412.csv'), stringsAsFactors = F)
bulk_NMVOC <- bulk_NMVOC %>%
  dplyr::rename(units = unit, global_total = value) %>%
  dplyr::mutate(global_total = global_total * 1000) %>%
  dplyr::bind_rows(hist_NMVOC) %>%
  dplyr::group_by(year, em, sector, units) %>%
  dplyr::summarise(global_total = sum(global_total)) %>%
  dplyr::ungroup()

totals <- bulk_NMVOC %>%
  dplyr::group_by(year, em, units) %>%
  dplyr::summarise(global_total = sum(global_total)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(sector = 'All Sectors')

bulk_voc_ems_total <- bulk_NMVOC %>%
  dplyr::bind_rows(totals) %>%
  dplyr::mutate(sector = factor(sector, levels = c(unique(bulk_NMVOC$sector), 'All Sectors')))

# Add dummy data to set ylimits to 0 or minimum value (to account for negative
# CO2 emissions). See https://stackoverflow.com/q/18046051/8715278
dummy <- bulk_voc_ems_total %>%
  dplyr::group_by(em) %>%
  dplyr::summarise(year = bulk_NMVOC$year[1], global_total = min(min(0, global_total)))


ggplot(data = bulk_voc_ems_total, aes(x=year, y=global_total)) +
  geom_line() +
  geom_vline(xintercept = 2015, alpha = 0.5, linetype = 'dotted') +
  geom_blank(data = dummy) +
  labs(y = "kt") +
  scale_x_continuous(breaks=seq(2000, 2100, 15)) +
  facet_wrap(. ~ sector, scales = "free_y") +
  ggtitle(paste('REMIND-MAGPIE-ssp585', "global", 'bulk VOC', "emissions"))
