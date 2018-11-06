# Copyright 2018 Battelle Memorial Institute

# config_default.R
#
# This file contains the default values for the output NetCDF files.
#
# There are also output configuration files available for IAMC and CMIP.

NC_ATTS <- list(
  Conventions        = 'CF-1.6',
  frequency          = 'mon',
  grid               = '0.5x0.5 degree latitude x longitude',
  location           = 'Laxenburg, Austria',
  nominal_resolution = '50 km'
)
