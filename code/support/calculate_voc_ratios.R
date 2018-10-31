# Copyright 2018 Battelle Memorial Institute

# calculate_voc_ratios.R
#
# Calculate the sub-VOC ratios for a given sector from the historical sub-VOC
# files. Only returns ratios at the global level.
#
# Caleb Braun
# 6/27/18
#
# This script uses csv files of historical gridded data found at
# http://www.globalchange.umd.edu/data/ceds/checksum_1750_2014.zip. It takes
# the NMVOC breakdowns for all years in those files.

library(dplyr)
library(tidyr)

# Replace with the path to your historical emissions file
HISTORICAL_EMS <- 'C:/Users/brau074/Downloads/historicalVOC/checksum_1750_2014_CEDS-2017-05-18'

hist_file_names <- list.files(HISTORICAL_EMS, 'VOC[0-2].*201412.csv', full.names = T)

# Combine historical files for all VOCs and aggregate to year level
global_voc_ratios <- do.call(rbind, lapply(hist_file_names, read.csv, stringsAsFactors = F)) %>%
  dplyr::group_by(year, em, sector, units) %>%
  dplyr::summarise(global_total = sum(global_total)) %>%
  dplyr::group_by(year, sector) %>%
  dplyr::mutate(share = global_total / sum(global_total)) %>%
  dplyr::group_by(em, sector) %>%
  dplyr::summarise(share_avg = mean(share)) %>%
  tidyr::spread(em, share_avg)

global_voc_ratios
