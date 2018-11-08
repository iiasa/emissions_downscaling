# Copyright 2018 Battelle Memorial Institute

# ------------------------------------------------------------------------------
# Program Name: C.2.1.gridding_nonair.R
# Author(s): Leyang Feng, Caleb Braun
# Date Last Updated: November, 2018
# Program Purpose: Gridding non-aircraft emissions in given IAM emissions file
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
if ( !exists( 'command_args' ) ) command_args <- commandArgs( TRUE )
iam <- command_args[ 1 ]
harm_status <- command_args[ 2 ]
modc_out <- command_args[ 5 ]
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

voc_map   <- read.csv( 'gridding/gridding-mappings/VOC_id_name_mapping.csv',
                       row.names = 1, stringsAsFactors = F )
ratio_map <- read.csv( 'gridding/gridding-mappings/ratio_mapping.csv',
                       stringsAsFactors = F )


# -----------------------------------------------------------------------------
# 4. Pre-processing of iam emissions
# remove AIR sector which will be gridded in a separate routine
iam_em <- iam_data[ iam_data$sector != 'AIR', ]

emissions <- sort( unique( iam_em$em ) )
scenarios <- sort( unique( iam_em$scenario ) )

nmvoc_ems <- row.names( voc_map )

# -----------------------------------------------------------------------------
# 5. Gridding and writing output data

# Define variables specific to each sector type. Note that order matters.
BULK_SECTOR_ID_MAP <- c(
  AGR = "Agriculture",
  ENE = "Energy",
  IND = "Industrial",
  TRA = "Transportation",
  RCO = "Residential, Commercial, Other",
  SLV = "Solvents production and application",
  WST = "Waste",
  SHP = "International Shipping",
  NEGCO2 = "Negative CO2 Emissions"
)

OPENBURNING_SECTOR_ID_MAP <- c(
  AWB  = "Agricultural Waste Burning On Fields",
  FRTB = "Forest Burning",
  GRSB = "Grassland Burning",
  PEAT = "Peat Burning"
)

grid_list <- function( grids_list, sector_id_map, sect_type, em, scen,
                       sub_nmvoc, aggregate_sectors = F, sector_shares = F ) {
  grids_list <- lapply( grids_list, `[`, names( sector_id_map ) )

  if ( all( is.na( unlist( lapply( grids_list, names ) ) ) ) ) {
    message( paste( "No", sect_type, "sectors found for", em, "in", scenario ) )
    return()
  }

  # Recurse once to build and write out netCDF file of openburning sector
  # shares, then write out netCDF file of aggregated openburning sectors
  if ( sect_type == 'openburning' && !sector_shares ) {
    grid_list( grids_list, sector_id_map, sect_type, em, scen, sub_nmvoc, F, T )
    aggregate_sectors = TRUE
  }

  write_ncdf( year_grids_list   = grids_list,
              output_dir        = output_dir,
              grid_resolution   = grid_resolution,
              year_list         = year_list,
              em                = em,
              scenario          = scen,
              sub_nmvoc         = sub_nmvoc,
              sector_type       = sect_type,
              sector_id_map     = sector_id_map,
              aggregate_sectors = aggregate_sectors,
              sector_shares     = sector_shares )
}


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

    # NOTE: Applying emission ratios is not fully supported yet
    ratio_ems <- ratio_map[ ratio_map$em == em, ]

    sapply( unique( em, c( ratio_ems$ratio_em ) ), function( ratio_em ) {
      if ( ratio_em != em ) {
        allyear_grids_list <- calculate_ratio_em( allyear_grids_list, ratio_ems,
                                                  ratio_em )
      }
      grid_list( allyear_grids_list, BULK_SECTOR_ID_MAP, 'anthro', em, scenario, sub_nmvoc )
      grid_list( allyear_grids_list, OPENBURNING_SECTOR_ID_MAP, 'openburning', em, scenario, sub_nmvoc )
    })
  }
}

logStop()
