# ------------------------------------------------------------------------------
# Program Name: C.1.gridding_data_reformatting.R
# Author(s): Leyang Feng, Caleb Braun
# Date Last Updated: May 29, 2018
# Program Purpose: Reformat the downscaled IAM emissions for gridding. Speciate
#                  VOCS if requested.
# Input Files:     B.IAM_HARM-STATUS_emissions_downscaled_for_gridding_RUNSUFFIX
# Output Files:    C.IAM_HARM-STATUS_emissions_reformatted_RUNSUFFIX
# Notes:
# TODO: update reference emissions so there would be no NA in X2015
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
headers <- c( 'module-A_functions.R', 'all_module_functions.R' )
log_msg <- "Reformat the downscaled IAM emissions for gridding"
script_name <- "C.1.gridding_data_reformatting.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
harm_status <- args_from_makefile[ 2 ]
run_species <- args_from_makefile[ 7 ]
if ( is.na( iam ) ) iam <- "GCAM4"


# ------------------------------------------------------------------------------
# 1. Read mapping files and extract iam info
# read in master config file
master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list
iam_info_list <- iamInfoExtract( master_config, iam )

printLog( paste0( 'The IAM to be processed is: ', iam_name  ) )


# -----------------------------------------------------------------------------
# 2. Read in the downscaled emissions and gridding mapping file
iam_data_fname <- paste0( 'B.', iam, '_', harm_status, '_emissions_downscaled_for_gridding_', RUNSUFFIX )
iam_data <- readData( domain = 'MED_OUT', file_name = iam_data_fname )
sector_mapping <- readData( domain = 'GRIDDING',
                            domain_extension = 'gridding-mappings/',
                            file_name = gridding_sector_mapping )


# -----------------------------------------------------------------------------
# 3. Disaggregate VOCs
# Take the anthropogenic VOC emissions and speciate them into the CEDS species
VOC_SPEC <- get_global_constant('voc_speciation')

if ( VOC_SPEC != 'none' ) {
  VOC_ratios <- readData( 'GRIDDING', 'VOC_ratio_AllSectors', domain_extension = "gridding-mappings/" )
  CEDS_maps <-  readData( 'MAPPINGS', 'CEDS_sector_mapping' )

  VOC_ratios$iso <- gsub( 'global', 'World', VOC_ratios$iso )
  VOC_ratios$sector <- gsub( 'TANK', 'SHP', VOC_ratios$sector )

  # create map from CEDS16_abr format (ex. WST) to CEDS9 format (ex. Waste)
  CEDS16_abr_to_CEDS9 <- CEDS_maps %>%
    dplyr::select( CEDS16_abr, CEDS9 ) %>%
    dplyr::filter( !grepl( 'Burning', CEDS9 ) ) %>%
    dplyr::distinct()

  # map the sub-VOC shares of each sector to CEDS9 format
  VOC_ratios_CEDS9 <- VOC_ratios %>%
    # dplyr::filter( iso != 'global' ) %>%
    dplyr::left_join( CEDS16_abr_to_CEDS9, by = c( 'sector' = 'CEDS16_abr' ) ) %>%
    dplyr::mutate( em = 'VOC' ) %>%
    tidyr::gather( sub_VOC, ratio, VOC01:VOC25 ) %>%
    dplyr::group_by( iso, CEDS9, em, sub_VOC ) %>%
    dplyr::summarise( ratio = mean( ratio ) )

  # assert that after aggregating, ratios still sum to one for each sector
  ratio_sums <- VOC_ratios_CEDS9 %>%
    dplyr::group_by( iso, CEDS9, em ) %>%
    dplyr::summarise( ratio = sum( ratio ) )
  stopifnot( all( round( ratio_sums$ratio, 8 ) == 1 ) )

  # Check if user requested a specific sub-VOC
  if ( !is.na( run_species ) && run_species %in% names( VOC_ratios ) )
    em_filter <- run_species
  else
    em_filter <- names( VOC_ratios )

  # disaggregate VOCs into sub-VOCs, then multiply each sub-VOC by its
  # corresponding ratio
  iam_data_sub_vocs <- iam_data %>%
    dplyr::left_join( VOC_ratios_CEDS9, by = c( 'iso', 'em', 'sector' = 'CEDS9' ) ) %>%
    dplyr::mutate( ratio = if_else( is.na( ratio ), 1, ratio ),
                   em    = if_else( is.na( sub_VOC ), em, sub_VOC ) ) %>%
    dplyr::filter( em %in% em_filter ) %>%
    dplyr::mutate_at( vars( num_range( 'X', ds_start_year:ds_end_year ) ), funs( . * ratio ) ) %>%
    dplyr::select( -sub_VOC, -ratio )

  # Remove non-sub-VOC emissions if specified, otherwise keep 'VOC' original
  if ( VOC_SPEC == 'all' ) {
    iam_data <- dplyr::bind_rows( iam_data, iam_data_sub_vocs )
  } else {
    iam_data <- iam_data_sub_vocs
  }
}


# -----------------------------------------------------------------------------
# 4. Map to sector short name and remove version number in scenario name.
iam_em <- iam_data %>%
  dplyr::inner_join( sector_mapping, by = c( 'sector' = 'sector_name' ) ) %>%
  dplyr::mutate( sector = sector_short,
                 scenario = substr( scenario, 1, nchar( scenario ) - 4) ) %>%
  dplyr::select( -sector_short )


# -----------------------------------------------------------------------------
# 5. Write out
# write the interpolated iam_data into intermediate output folder
out_fname <- paste0( 'C.', iam, '_', harm_status, '_emissions_reformatted_', RUNSUFFIX )
writeData( iam_em, 'MED_OUT', out_fname, meta = F )

logStop()
