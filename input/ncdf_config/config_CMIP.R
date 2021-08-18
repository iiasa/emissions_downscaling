# Copyright 2018 Battelle Memorial Institute

# config_default.R
#
# This file contains the default values for the output NetCDF files for CMIP. To
# use this configuration, make set the global setting 'config_file' to the name
# of this file (config_CMIP.R).
#
# All elements of NC_ATTS will be written out as metadata in the final NetCDF,
# replacing any defaults.
#
# Special Rules:
#   1. Any word inside of double brackets will be replaced with that variable
#      at run time. This is so that variables like 'scenario' or 'iam' are
#      available for use.
#   2. To add on to a default metadata value, put the suffix in a list titled
#      NC_ATT_ADDITIONS.

NC_ATTS <- list(
  Conventions        = 'CF-1.6',
  activity_id        = 'input4MIPs',
  comment            = 'gridded emissions for [[iam]]_[[scenario]]. Data harmonized to historical emissions CEDS-v2021-04-21 (anthropogenic) and GFED-CMIP6 v1.2 (land-use change).',
  contact            = 'YOUR NAME HERE (YOUR-EMAIL@XXX.XXX)',
  data_structure     = 'grid',
  dataset_category   = 'emissions',
  external_variables = 'gridcell_area',
  frequency          = 'mon',
  further_info_url   = 'ADD URL',
  grid               = '0.5x0.5 degree latitude x longitude',
  grid_label         = 'gn',
  license            = 'EDIT AS NEEDED Gridded emissions are licensed under a Creative Commons Attribution-ShareAlike 4.0 International License (https://creativecommons.org/licenses). Consult https://pcmdi.llnl.gov/CMIP6/TermsOfUse for terms of use governing input4MIPs output, including citation requirements and proper acknowledgment. Further information about this data, including some limitations, can be found via the further_info_url (recorded as a global attribute in this file). The data producers and data providers make no warranty, either express or implied, including, but not limited to, warranties of merchantability and fitness for a particular purpose. All liabilities arising from the supply of the information (including any liability arising in negligence) are excluded to the fullest extent permitted by law.',
  institution        = 'YOUR INSTITUTION',
  institution_id     = 'YOUR INSTITUTION ACRONYMN (e.g. IAMC)',
  mip_era            = 'CMIP6Plus',
  nominal_resolution = '50 km',
  realm              = 'atmos',
  references         = 'See: ADD REFERENCE OR WEB SITE',
  source             = 'ADD SOURCE',
  source_version     = '1.0',
  table_id           = 'input4MIPs',
  target_mip         = 'ScenarioMIP'
)

NC_ATT_ADDITIONS <- list(
  history = '; ADD CITY, COUNTRY',
  title   = ' ADD TITLE'
)
