# ------------------------------------------------------------------------------
# Program Name: C.2.2.gridding_air.R
# Author(s): Leyang Feng
# Date Last Updated: Nov 3, 2017
# Program Purpose: Gridding aircraft emissions in given IAM emissions file
# Input Files:
# Output Files:
# Notes:
# TODO:
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Must be run from the emissions_downscaling/input directory
if ( !endsWith( getwd(), '/input' ) ) setwd( 'input' )
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( 'gridding_functions.R', 'module-A_functions.R',
              'all_module_functions.R', 'nc_generation_functions.R' )
log_msg <- "Gridding aircraft emissions in given IAM emissions file"
script_name <- "C.2.2.gridding_air.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
harm_status <- args_from_makefile[ 2 ]
modc_out <- args_from_makefile[ 5 ]
if ( is.na( iam ) ) iam <- "GCAM4"
if ( is.na( modc_out ) ) iam <- "../final-output/module-C"


# ------------------------------------------------------------------------------
# 1. Read mapping files and axtract iam info
# read in master config file
master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list
iam_info_list <- iamInfoExtract( master_config, iam )

printLog( paste0( 'The IAM to be processed is: ', iam_name  ) )


# -----------------------------------------------------------------------------
# 2. Initialize gridding settings
output_dir <- modc_out
proxy_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = gridding_proxy_path )
proxy_backup_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "proxy-backup/")
mask_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "mask/")
seasonality_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "seasonality-CEDS9/" )

gridding_initialize( grid_resolution = grid_resolution,
                     start_year = ds_start_year,
                     end_year = ds_end_year,
                     load_masks = T,
                     load_seasonality_profile = T )


# -----------------------------------------------------------------------------
# 3. Read in emissions file and mappings
iam_data_fname <- paste0( 'C.', iam, '_', harm_status, '_emissions_reformatted_', RUNSUFFIX )
iam_data <- readData( domain = 'MED_OUT', file_name = iam_data_fname )

grid_maps_ext <- 'gridding-mappings/'
proxy_mapping <- readData( 'GRIDDING', domain_extension = grid_maps_ext,
                           file_name = gridding_proxy_mapping )
seasonality_mapping <- readData( 'GRIDDING', domain_extension = grid_maps_ext,
                                 file_name = gridding_seasonality_mapping )


# -----------------------------------------------------------------------------
# 4. Pre-processing of iam emissions
# remove all non-AIR sectors which have been gridded in a previous routine
iam_em <- iam_data[ iam_data$sector == 'AIR', ]
if ( nrow( iam_em ) ) iam_em$iso <- 'global'

emissions <- sort( unique( iam_em$em ) )
scenarios <- sort( unique( iam_em$scenario ) )


# -----------------------------------------------------------------------------
# 5. Gridding and writing output data

for ( scenario in scenarios ) {
  for ( em in emissions ) {
    gridding_em <- iam_em[ iam_em$scenario == scenario & iam_em$em == em, ]

    allyear_grids_list <- grid_all_years_air( year_list,
                                              em,
                                              grid_resolution,
                                              gridding_em,
                                              proxy_mapping,
                                              seasonality_mapping )

    # Build and write out netCDF file
    write_ncdf( year_grids_list = allyear_grids_list,
                output_dir      = output_dir,
                grid_resolution = grid_resolution,
                year_list       = year_list,
                em              = em,
                scenario        = scenario,
                sub_nmvoc       = FALSE,         # We don't do aircraft NMVOCs
                sector_type     = "AIR-anthro",
                ncdf_sectors    = 1:25,          # This is the altitude layer
                sector_ids      = "" )
  }
}

logStop()
