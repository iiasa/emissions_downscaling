# ------------------------------------------------------------------------------
# Program Name: C.2.1.gridding_nonair.R
# Author(s): Leyang Feng
# Date Last Updated: Apr 3, 2017
# Program Purpose: Gridding none-aircraft emissions in given IAM emissions file
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
headers <- c( 'data_functions.R', 'gridding_functions.R', 'module-A_functions.R',
              'all_module_functions.R', 'nc_generation_functions.R' )
log_msg <- "Gridding non-aircraft emissions in given IAM emissions file"
script_name <- "C.2.1.gridding_nonair.R"

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

printLog( paste( 'The IAM to be processed is:', iam_name ) )


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
seasonality_mapping <- readData( 'GRIDDING', domain_extension = grid_maps_ext,
                                 file_name = gridding_seasonality_mapping )
sector_name_mapping <- readData( 'GRIDDING', domain_extension = grid_maps_ext,
                                 file_name = gridding_sector_mapping )
location_index <-      readData( 'GRIDDING', domain_extension = grid_maps_ext,
                                 file_name = gridding_location_index )
proxy_mapping <-       readData( 'GRIDDING', domain_extension = grid_maps_ext,
                                 file_name = gridding_proxy_mapping )
proxy_sub_mapping <-   readData( 'GRIDDING', domain_extension = grid_maps_ext,
                                 file_name = gridding_proxy_substitution_mapping )

voc_map <- read.csv( 'gridding/gridding-mappings/VOC_id_name_mapping.csv',
                     row.names = 1, stringsAsFactors = F )


# -----------------------------------------------------------------------------
# 4. Pre-processing of iam emissions
# remove AIR sector which will be gridded in a separate routine
iam_em <- iam_data[ iam_data$sector != 'AIR', ]

emissions <- sort( unique( iam_em$em ) )
scenarios <- sort( unique( iam_em$scenario ) )

nmvoc_ems <- row.names( voc_map )

# -----------------------------------------------------------------------------
# 5. Gridding and writing output data

# Define variables specific to each sector type. Note that order matters, so
# make sure the position of the variable matches its id.
BULK_SECTORS <- c( "AGR", "ENE", "IND", "TRA", "RCO", "SLV", "WST", "SHP" )
BULK_SECTOR_IDS <- paste( "0: Agriculture; 1: Energy; 2: Industrial;",
                          "3: Transportation; 4: Residential, Commercial, Other;",
                          "5: Solvents production and application; 6: Waste;",
                          "7: International Shipping" )
OPENBURNING_SECTORS <- c( "AWB", "FRTB", "GRSB", "PEAT" )
OPENBURNING_SECTOR_IDS <- paste( "0: Agricultural Waste Burning On Fields;",
                                 "1: Forest Burning; 2: Grassland Burning;",
                                 "3: Peat Burning" )

for ( scenario in scenarios ) {
  for ( em in emissions ) {
    gridding_em <- iam_em[ iam_em$scenario == scenario & iam_em$em == em, ]

    # for gridding all years, the em is used only to look up proxy / seasonality
    sub_nmvoc <- em %in% nmvoc_ems
    proxy_em <- if_else( sub_nmvoc, 'NMVOC', em )

    allyear_grids_list <- grid_all_years( year_list,
                                          proxy_em,
                                          grid_resolution,
                                          gridding_em,
                                          location_index,
                                          proxy_mapping,
                                          proxy_sub_mapping )

    anthro_grids <- lapply( allyear_grids_list, `[`, BULK_SECTORS )
    openburning_grids <- lapply( allyear_grids_list, `[`, OPENBURNING_SECTORS )

    # Build and write out netCDF file of anthropogenic emissions
    if ( all( is.na( unlist( lapply( anthro_grids, names ) ) ) ) ) {
      warning( paste( "No anthro sectors found for", em, "in", scenario ) )
    } else {
      write_ncdf( year_grids_list = anthro_grids,
                  output_dir      = output_dir,
                  grid_resolution = grid_resolution,
                  year_list       = year_list,
                  em              = em,
                  scenario        = scenario,
                  sub_nmvoc       = sub_nmvoc,
                  sector_type     = "anthro",
                  ncdf_sectors    = BULK_SECTORS,
                  sector_ids      = BULK_SECTOR_IDS )
    }

    if ( all( is.na( unlist( lapply( openburning_grids, names ) ) ) ) ) {
      warning( paste( "No open burning sectors found for", em, "in", scenario ) )
    } else {
      # Build and write out netCDF file of aggergated openburning sectors
      write_ncdf( year_grids_list   = openburning_grids,
                  output_dir        = output_dir,
                  grid_resolution   = grid_resolution,
                  year_list         = year_list,
                  em                = em,
                  scenario          = scenario,
                  sub_nmvoc         = sub_nmvoc,
                  sector_type       = "openburning",
                  ncdf_sectors      = OPENBURNING_SECTORS,
                  sector_ids        = OPENBURNING_SECTOR_IDS,
                  aggregate_sectors = TRUE,
                  sector_shares     = FALSE )

      # Build and write out netCDF file of openburning sector shares
      write_ncdf( year_grids_list   = openburning_grids,
                  output_dir        = output_dir,
                  grid_resolution   = grid_resolution,
                  year_list         = year_list,
                  em                = em,
                  scenario          = scenario,
                  sub_nmvoc         = sub_nmvoc,
                  sector_type       = "openburning",
                  ncdf_sectors      = OPENBURNING_SECTORS,
                  sector_ids        = OPENBURNING_SECTOR_IDS,
                  aggregate_sectors = FALSE,
                  sector_shares     = TRUE )
    }
  }
}

logStop()
