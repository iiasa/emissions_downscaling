# ------------------------------------------------------------------------------
# Program Name: B.4.3.IAM_emissions_downscaling_consistency_check.R
# Author(s): Xavier Gutierrez
# Date Last Updated: Mar 28, 2017
# Program Purpose: Check that downscaled countries, when aggregated to regions, match reference emissions.
# Input Files: 
#              intermediate-output/[SUFFIX]/B.[IAM]_emissions_linear_[SUFFIX].csv      
#              intermediate-output/[SUFFIX]/B.[IAM]_emissions_downscaled_linear_[SUFFIX].csv
#              intermediate-output/[SUFFIX]/B.[IAM]_emissions_ipat_[SUFFIX].csv 
#              intermediate-output/[SUFFIX]/B.[IAM]_emissions_ipat_[SUFFIX].csv 
#              intermediate-output/[SUFFIX]/B.[IAM]_emissions_downscaled_ipat_[SUFFIX].csv
# 
# Output Files: 
# Notes: 
# TODO: Write the consistency check
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS input directory and define PARAM_DIR as the
# location of the CEDS parameters directory, relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'emissions_downscaling/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provides logging, file support, and system functions - and start the script log.
headers <- c( 'common_data.R', 'data_functions.R', 'module-A_functions.R', 'all_module_functions.R' ) 
log_msg <- "Downscale energy related emissions using IPAT approach" 

script_name <- "B.4.3.IAM_emissions_downscaling_consistency_check.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
if ( is.na( iam ) ) iam <- "GCAM4"

MODULE_B <- "../code/module-B/"

# ------------------------------------------------------------------------------
# 1. Read IAM emissions and downscaled emissions 

ds_lin_in <-  readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_linear', '_', RUNSUFFIX ))
ds_lin_out <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_downscaled_linear', '_', RUNSUFFIX ))

ds_ipat_in <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_ipat', '_', RUNSUFFIX ))
ds_ipat_out <- readData( domain = 'MED_OUT', file_name = paste0( 'B.', iam, '_emissions_downscaled_ipat', '_', RUNSUFFIX ) )

# -----------------------------------------------------------------------------
# 2. Confirm region-consistency for linear downscaling
# 
# in order to perform the consistency check, we are collapsing the 'iso' and 
# 'sector' columns. 
#
# Before downscaling, 'iso' isn't used as a uniquely-identifying key, meaning
# values are the same for all rows of the same region, regardless of iso. 
# To collapse the 'iso' column, we select all columns != iso and grab all 
# distinct rows. 
# 
# To collapse the 'sector' column, we aggregate (sum) by all columns except sector
# and value. 

ds_lin_in.agg <- ds_lin_in %>% 
  rename_all(funs(gsub("reg_iam_em_", "", .))) %>% 
  gather(x_year, value, -model, -scenario, -region, -em, -sector, -harm_status, -unit, -iso) %>% 
  select(model, scenario, region, em, sector, harm_status, unit, x_year, value) %>% 
  distinct() %>% 
  group_by(model, scenario, region, em, harm_status, unit, x_year) %>% 
  summarise(value=sum(value)) %>% # aggregate over sectors
  ungroup() %>% 
  mutate(value = round(value, 5)) # in order to compare two df's, must round to same precision

ds_lin_out.agg <- ds_lin_out %>% 
  gather(x_year, value, -model, -scenario, -region, -em, -sector, -harm_status,-unit, -iso) %>% 
  group_by(model, scenario, region, em, harm_status, unit, x_year) %>% 
  summarise(value=sum(value)) %>% # aggregate over sectors
  ungroup() %>% 
  mutate(value = round(value, 5)) # in order to compare two df's, must round to same precision


# don't provide 'by' argument so that join(x,y) compares on all columns 
df <- anti_join(ds_lin_in.agg, ds_lin_out.agg) # rows from input that don't match output
df2 <- anti_join(ds_lin_out.agg, ds_lin_in.agg) # rows from output that don't match input

# -----------------------------------------------------------------------------
# 3. Confirm region-consistency for ipat downscaling
# 
# in order to perform the consistency check, we are collapsing the 'iso' and 
# 'sector' columns. 
#
# Before downscaling, 'iso' isn't used as a uniquely-identifying key, meaning
# values are the same for all rows of the same region, regardless of iso. 
# To collapse the 'iso' column, we select all columns != iso and grab all 
# distinct rows. 
# 
# To collapse the 'sector' column, we aggregate (sum) by all columns except sector
# and value. 

ds_ipat_in.agg <- ds_ipat_in %>% 
  rename_all(funs(gsub("reg_iam_em_", "", .))) %>% 
  select(-harm_status) %>% # not used in ds_ipat_out so dropping from input
  select(-Xcon_year) %>% # not a year included in output 
  gather(x_year, value, -model, -scenario, -region, -em, -sector, -unit, -iso) %>% 
  select(model, scenario, region, em, sector, unit, x_year, value) %>% 
  distinct() %>% 
  group_by(model, scenario, region, em, unit, x_year) %>% 
  summarise(value=sum(value)) %>% # aggregate over sectors, 
  ungroup() %>% 
  mutate(value = round(value, 5)) # in order to compare two df's, must round to same precision

ds_ipat_out.agg <- ds_ipat_out %>% 
  gather(x_year, value, -model, -scenario, -region, -em, -sector,-unit, -iso) %>% 
  group_by(model, scenario, region, em, unit, x_year) %>% 
  summarise(value=sum(value)) %>% # aggregate over sectors, 
  ungroup() %>% 
  mutate(value = round(value, 5)) # in order to compare two df's, must round to same precision

# don't provide 'by' argument so that join(x,y) compares on all columns 
df <- anti_join(ds_ipat_in.agg, ds_ipat_out.agg) # rows from input that don't match output
df2 <- anti_join(ds_ipat_out.agg, ds_ipat_in.agg) # rows from output that don't match input

