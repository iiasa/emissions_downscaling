# Copyright 2018 Battelle Memorial Institute

# ------------------------------------------------------------------------------
# Program Name: B.1.IAM_input_reformatting
# Author(s): Leyang Feng
# Date Last Updated: Feb 13, 2017
# Program Purpose: The script reads in IAM snapshots and separate the information
#                  in 'Variable' column, then write the df into intermediate_out
#                  folder as input for next script.
# Input Files:
# Output Files: B.[iam]_emissions_reformatted
# Notes:
# TODO: Find a more smart way to deal with emissions only for world region
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Must be run from the emissions_downscaling/input directory
if ( !endsWith( getwd(), '/input' ) ) setwd( 'input' )
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( 'data_functions.R', 'module-A_functions.R', 'all_module_functions.R' )
log_msg <- "Reformat IAM input to separate information for sector, species, etc. "
script_name <- "B.1.IAM_input_reformatting"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'command_args' ) ) command_args <- commandArgs( TRUE )
iam         <- command_args[ 1 ]
harm_status <- command_args[ 2 ]
input_file  <- command_args[ 3 ]
modb_out    <- command_args[ 4 ]
run_species <- command_args[ 7 ]
if ( is.na( iam ) ) iam <- "GCAM4"
if ( is.na( input_file ) ) stop( 'No snapshot file provided!' )

# ------------------------------------------------------------------------------
# 1. Read mapping files and axtract iam info
# read in master config file
master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list
iam_info_list <- iamInfoExtract( master_config, iam )

ds_sector_mapping <- readData( 'MAPPINGS', ds_sector_mapping )
region_mapping <- readData( 'MAPPINGS', ref_region_mapping )

printLog( paste0( 'The IAM to be processed is: ', iam_name  ) )

# -----------------------------------------------------------------------------
# 2. Read in the input data

data_df <- read_excel( input_file )
names( data_df ) <- tolower( names( data_df ) )

# -----------------------------------------------------------------------------
# 3. Reformat the IAM data
year_list <- c( 2015, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100 )
x_year_list <- paste0( 'X', year_list )
native_reg_list <- sort( unique( region_mapping$region ) )

iam_data <- data_df[ , c( 'model', 'scenario', 'region', 'variable', year_list ) ]

# AIM Team requested model name be "AIM"
if ( iam == 'AIM' && 'AIM/CGE' %in% iam_data$model ) {
  message( "Input model is 'AIM/CGE', but output will show 'AIM' only.")
  iam_data$model <- sub( '/CGE', '', iam_data$model, fixed = TRUE )
}

ds_sector_map <- ds_sector_mapping %>%
  dplyr::filter( harm_status == !!harm_status ) %>%
  dplyr::select( em, variable, sector, harm_status )
iam_data <- iam_data %>%
  dplyr::filter( model %in% iam_name ) %>%
  dplyr::inner_join( ds_sector_map, by = 'variable' ) %>%
  dplyr::filter( !is.na( sector ) ) %>%
  dplyr::mutate( unit = 'Mt' ) %>%
  dplyr::mutate( em = gsub( '^VOC$', 'NMVOC', em ) ) %>%
  dplyr::rename_all( make.names ) %>%
  dplyr::select( model, scenario, region, em, sector, harm_status, unit, one_of( x_year_list ) )

if (object.size(iam_data) < 4000 )  stop("No input data found after filtering. Check formatting and model name.")

# Filter for if only one emission species is desired.
VOC_SPEC <- get_constant( 'voc_speciation' )
sub_spec <- !is.na( run_species )

if ( VOC_SPEC == 'only' && sub_spec && run_species %!in% c( "VOC", "NMVOC" ) ) {
  stop( paste0( "Cannot speciate NMVOCs for emission ", run_species, ". ",
                "Check voc_speciation option in global_settings.R.") )
} else if ( VOC_SPEC == 'only' ) {
  iam_data <- iam_data[ iam_data$em == "NMVOC", ]
} else if ( sub_spec && run_species %in% iam_data$em ) {
  iam_data <- iam_data[ iam_data$em == run_species, ]
} else if ( sub_spec ) {
  stop( paste0( "Cannot downscale for missing emission ", run_species, ". ",
                "Supported emission species include: ",
                paste( get_constant( 'supported_species' ), collapse = ', ' ) ) )
}


# -----------------------------------------------------------------------------
# 4. Complete the layout and fill the missing sector/region with value 0
native_reg_list <- native_reg_list
sector_list <- sort( unique( ds_sector_mapping$sector ) )
scenario_list <- sort( unique( iam_data$scenario ) )
em_list <- sort( unique( iam_data$em ) )

genCompleteLayout <- function( ) {
  sector_list <- sector_list[ sector_list != "International Shipping" ]
  sector_list <- sector_list[ sector_list != "Aircraft" ]
  sce_res_list <- lapply( scenario_list, function( sce ) {
    em_res_list <- lapply( em_list, function( em ) {
      sec_res_list <- lapply( sector_list, function( sec ) {
        reg_res_list <- lapply( native_reg_list, function( reg ) {

          out_df <- data.frame( scenario = sce,
                                em = em,
                                region = reg,
                                sector = sec,
                                stringsAsFactors = F )

        } )
        reg_res <- do.call( 'rbind', reg_res_list )
      } )
      sec_res <- do.call( 'rbind', sec_res_list )

      model_world <- 'World'

      air_res <- data.frame( scenario = sce,
                             em = em,
                             region = model_world,
                             sector = "Aircraft",
                             stringsAsFactors = F )
      shp_res <- data.frame( scenario = sce,
                             em = em,
                             region = model_world,
                             sector = "International Shipping",
                             stringsAsFactors = F )
      sec_res <- rbind( sec_res, air_res, shp_res )
    } )
    em_res <- do.call( 'rbind', em_res_list )
  } )
  sce_res <- do.call( 'rbind', sce_res_list )
  sce_res$model <- unique( iam_data$model )
  sce_res$harm_status <- unique( iam_data$harm_status )
  sce_res$unit <- unique( iam_data$unit )

  return( sce_res )
}
complete_layout <- genCompleteLayout( )

iam_data <- merge( iam_data,
                   complete_layout,
                   by = c( 'model', 'scenario', 'region', 'em', 'sector', 'harm_status', 'unit' ),
                   all.y = T )
iam_data[ is.na( iam_data ) ] <- 0

# -----------------------------------------------------------------------------
# 4.5 checknon-CO2 negative emissions in 2100
if ( any( iam_data[ iam_data$em != 'CO2', 'X2100' ] < 0 ) ) {
  printLog( 'There are negative non-CO2 emissions in given input file!' )
} else {
  printLog( 'All non-CO2 emissions are positive' )
}

# -----------------------------------------------------------------------------
# 5. Interpolate the iam_data into all years
all_x_years <- paste0( 'X', year_list[ 1 ] : year_list[ length( year_list ) ] )
int_x_year <- all_x_years[ which( !( all_x_years %in% x_year_list ) ) ]
iam_int <- iam_data
iam_int[ , int_x_year ] <- NA
iam_int <- iam_int[ , c( 'model', 'scenario', 'region', 'em', 'sector', 'harm_status', 'unit', all_x_years ) ]
iam_int <- interpolateXyears( iam_int, int_method = 'linear' )

# -----------------------------------------------------------------------------
# 6. Write out
# write the interpolated iam_data into intermediate output folder
out_filname <- paste0( 'B.', iam, '_emissions_reformatted', '_', RUNSUFFIX )
writeData( iam_int, 'MED_OUT', out_filname, meta = F )

# END
logStop()
