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
  comment            = 'SSP harmonized, gridded emissions for IAMC-[[iam]]_[[scenario]]. Data harmonized to historical emissions CEDS-v2017-05-18 (anthropogenic) and v1.2 (land-use change).',
  contact            = 'Steven J. Smith (ssmith@pnnl.gov)',
  data_structure     = 'grid',
  dataset_category   = 'emissions',
  external_variables = 'gridcell_area',
  frequency          = 'mon',
  further_info_url   = 'https://secure.iiasa.ac.at/web-apps/ene/SspDb/',
  grid               = '0.5x0.5 degree latitude x longitude',
  grid_label         = 'gn',
  license            = 'ScenarioMIP gridded emissions data produced by the IAMC are licensed under a Creative Commons Attribution-ShareAlike 4.0 International License (https://creativecommons.org/licenses). Consult https://pcmdi.llnl.gov/CMIP6/TermsOfUse for terms of use governing input4MIPs output, including citation requirements and proper acknowledgment. Further information about this data, including some limitations, can be found via the further_info_url (recorded as a global attribute in this file). The data producers and data providers make no warranty, either express or implied, including, but not limited to, warranties of merchantability and fitness for a particular purpose. All liabilities arising from the supply of the information (including any liability arising in negligence) are excluded to the fullest extent permitted by law.',
  institution        = 'Integrated Assessment Modeling Consortium',
  institution_id     = 'IAMC',
  mip_era            = 'CMIP6',
  nominal_resolution = '50 km',
  realm              = 'atmos',
  references         = 'See: https://secure.iiasa.ac.at/web-apps/ene/SspDb/ for references',
  source             = 'IAMC Scenario Database hosted at IIASA',
  source_version     = '1.1',
  table_id           = 'input4MIPs',
  target_mip         = 'ScenarioMIP'
)

NC_ATT_ADDITIONS <- list(
  history = '; College Park, MD',
  title   = ' prepared for input4MIPs'
)
