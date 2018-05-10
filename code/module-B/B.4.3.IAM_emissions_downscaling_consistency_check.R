# ------------------------------------------------------------------------------
# Program Name: B.4.3.IAM_emissions_downscaling_consistency_check.R
# Author(s): Xavier Gutierrez
# Date Last Updated: Mar 28, 2017
# Program Purpose: Check that downscaled countries, when aggregated to regions, match reference emissions.
# Input Files: 
#              intermediate-output/[SUFFIX]/B.[IAM]_emissions_linear_[SUFFIX].csv      
#              intermediate-output/[SUFFIX]/B.[IAM]_emissions_downscaled_linear_[SUFFIX].csv
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
log_msg <- "Run checksum test on downscaling output" 

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

# digits of precision in sum comparison
d <- 5

# tolerance for error
t <- 0.05/100 # 0.05%

# -----------------------------------------------------------------------------
# 3. Aggregate IAM Emissions and Linear-Downscaling Emissions
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
  rename_all(funs(gsub("reg_iam_em_X", "", .))) %>% 
  select(-harm_status) %>% 
  gather(year, value, -model, -scenario, -region, -em, -sector, -unit, -iso) %>% 
  select(model, scenario, region, em, sector, unit, year, value) %>% 
  distinct() %>% 
  group_by(model, scenario, region, em, sector, unit, year) %>% 
  summarise(value=sum(value)) %>%
  ungroup() %>% 
  mutate(value = round(value, digits = d)) # in order to compare two df's, must round to same precision


ds_lin_out.agg <- ds_lin_out %>%   
  rename_all(funs(gsub("X", "", .))) %>% 
  select(-harm_status) %>% 
  gather(year, value, -model, -scenario, -region, -em, -sector,-unit, -iso) %>% 
  group_by(model, scenario, region, em, sector, unit, year) %>% 
  summarise(value=sum(value)) %>%
  ungroup() %>% 
  mutate(value = round(value, digits = d)) # in order to compare two df's, must round to same precision

# -----------------------------------------------------------------------------
# 3. Aggregate IAM Emissions and IPAT-Downscaling Emissions
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
  rename_all(funs(gsub("reg_iam_em_X", "", .))) %>% 
  select(-harm_status) %>% # not used in ds_ipat_out so dropping from input
  select(-con_year) %>% # not a year included in output 
  gather(year, value, -model, -scenario, -region, -em, -sector, -unit, -iso) %>% 
  select(model, scenario, region, em, sector, unit, year, value) %>% 
  distinct() %>% 
  group_by(model, scenario, region, em, sector, unit, year) %>% 
  summarise(value=sum(value)) %>% 
  ungroup() %>% 
  mutate(value = round(value, digits = d)) # in order to compare two df's, must round to same precision

ds_ipat_out.agg <- ds_ipat_out %>% 
  rename_all(funs(gsub("X", "", .))) %>% 
  gather(year, value, -model, -scenario, -region, -em, -sector,-unit, -iso) %>% 
  group_by(model, scenario, region, em, sector, unit, year) %>% 
  summarise(value=sum(value)) %>% # aggregate over sectors, 
  ungroup() %>% 
  mutate(value = round(value, digits = d)) # in order to compare two df's, must round to same precision

#  ------------------------------------------------------------------------
# 4. Error logging
# Check for mismatched rows between the two aggregated emissions dataframes
# If there are any mismatched rows, print to error log the identifying keys for those rows
# normal log contains reference to error log
# Write in error/ directory a diagnostic file that contains the two sets of mismatched rows side-by-side


errorLogging <- function(in.agg, out.agg, method, t) {
  
  # same number of rows/cols, different values
  in.mis <- anti_join(in.agg, out.agg, by = c("model", "scenario", "region", "em", "sector", "unit", "year", "value"))
  out.mis <- anti_join(out.agg, in.agg, by = c("model", "scenario", "region", "em", "sector", "unit", "year", "value"))
  
  # check if any rows in mismatched values df
  if(nrow(in.mis) != 0) {
    printLog(paste0(method, " downscaling: there are output rows that don't match input. Check error log"))
    
    # distinct error log for each (m, s) with mismatched values
    for (model.name in unique(in.mis$model)) {
      for (scen in unique(in.mis$scenario)) {
        
        # m can't contain any /
        model.name.mod <- gsub("/", "-", model.name)
        
        # open error log, name according to (m, s)
        fn <- paste0("../code/error/ERROR-", method, " ", model.name.mod, ", ", scen, ".txt")
        zz <- file(fn, open="wt")
        sink(zz) # divert session output to error log
        print(paste0(method, " downscaling error"))
        print("Downscaled emissions aggregated to native IAM regions don't match IAM emissions")
        
        df2 <- in.mis %>% 
          select(-value, -unit) %>% # drop columns that don't need to be reported
          filter(model == model.name & scenario == scen) %>% # filter to distinct model & scenario
          group_by(model, scenario, region, em, sector, ) %>% # for each region-em-sector in (m,s), 
          summarise(years = paste0(year, collapse=", ")) %>% # print years that have mismatched values
          ungroup()
        
        # for each mismatched row, print the following keys:
        # model, scenario, region, em, harm_status, year 
        print("Rows with mismatched values:")
        for (i in 1:nrow(df2)) {
          paste0(df2[i,], collapse=", ") %>% 
            print()
        }
        
        # close sink diversion then file connection
        sink()
        close(zz)
      }
    } # end of error logging
    
    # write diagnostic file with both sets of mismatched values ( for all models & scenarios )
    
    # contains the rows from input that don't match output
    in.mis <- in.mis %>% 
      dplyr::rename(input=value)
    
    # contains the rows from output that don't match input
    out.mis <- out.mis %>% 
      dplyr::rename(output=value)
    
    # place input/output data columns next to each other for each year of mismatched values
    mis <- full_join(in.mis, out.mis,
                     by=c("model", "scenario", "region", "em", "sector", "unit", "year")) %>% 
      mutate(difference = output - input,
             percentDifference = difference / input) %>% 
      filter(percentDifference > t) %>% 
      filter( year %in% seq(2015, 2100, by=5))  # only keep model years (every 5)
    # mismatch due to downscaling
    mis.ds <- mis %>% 
      filter(year != 2015) # drop base year values
    
    # mismatch due to harmonization (only base year values)
    mis.harm <- mis %>% 
      filter(year == 2015)
    
    
    # construct diagnostic file name
    iam <- unique(mis$model)
    out_filename.ds <- paste0( iam, '_emissions_downscaled_', method, '_inconsistent' )
    out_filename.harm <- paste0( iam, '_emissions_downscaled_', method, '_inconsistent_harmonization' )
    
    # save diagnostic file
    writeData( mis.ds, 'ERR', out_filename.ds, meta = F ) 
    writeData( mis.harm, 'ERR', out_filename.harm, meta = F ) 
    
    
  } else {
    printLog(paste0(method, " downscaling: all output rows match input"))
  }
}

# Linear Downscaling
errorLogging(ds_lin_in.agg, ds_lin_out.agg, "Linear", t)

#IPAT Downscaling
errorLogging(ds_ipat_in.agg, ds_ipat_out.agg, "IPAT", t)

# END
logStop()
