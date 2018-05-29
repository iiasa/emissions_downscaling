#-----------------------------------------------------------------------------------------------------------------
# Program Name: all_module_functions.R
# Author's Name: Leyang Feng
# Date Last Modified:
# Program Purpose:
# Note:
# TODO:
# ----------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# iamInfoExtract
# Brief: Extract IAM configuration parameters into global environment
# Dependencies:
# Author: Leyang Feng
# parameters: iam_info -- the raw df read from master_config.csv
# return:
# input files:
# output: iam_info_list
iamInfoExtract <- function( master_config, iam ) {

  # first check whether the given iam is valid or not
  iam <- gsub( '-', '.', iam, fixed = T )
  model_names <- colnames( master_config )[ 2 : ncol( master_config ) ]
  if ( ( iam %in% model_names ) == F ) { stop( 'The provided IAM does not have configuration information' ) }

  # extract the IAM configuration information
  tango_iam_list <- as.list( master_config[ , iam ] )
  parameter_names <- master_config[ , 'model' ]
  names( tango_iam_list ) <- parameter_names

  # go through each element in the list
  # and write the parameter into global environment
  # then modifiy the list if necessary
  iam_name <<- gsub( '.', '-', iam, fixed = T )
  iam_path <<- tango_iam_list$model_datapath
  iam_domain_extension <<- tango_iam_list$iam_data_domain_extension
  iam_sheet_name <<- tango_iam_list$sheet_name

  # - harmonization related parameters
  iam_interpolation_method <<- tango_iam_list$interpolation_method
  iam_variable_list <<- tango_iam_list$variable_list
  ref_name <<- tango_iam_list$reference_em
  ref_version <<- tango_iam_list$reference_em_version
  ref_path <<- tango_iam_list$reference_em_path
  ref_domain_extension <<- tango_iam_list$reference_em_domain_extension
  iam_sector_mapping <<- tango_iam_list$iam_sector_mapping
  ref_sector_mapping <<- tango_iam_list$reference_sector_mapping
  ref_region_mapping <<- tango_iam_list$reference_region_mapping
  base_year <<- tango_iam_list$base_year
  harm_start_year <<- tango_iam_list$harmonization_start_year
  harm_end_year <<- tango_iam_list$harmonization_end_year
  harmonization_method_specific_flag <<- tango_iam_list$harmonization_method_specific
  if ( is.na( harmonization_method_specific_flag ) ) harmonization_method_specific_flag <- 'N'
  if ( harmonization_method_specific_flag == 'Y' ) {
    harm_method_specific_type <<- tango_iam_list$harmonization_method_specific_type
    harm_method_specific_mapping <<- tango_iam_list$harmonization_method_specific_mapping
    printLog( paste0( harm_method_specific_type, '-specified harmonization method is provided. Check ',
                      harm_method_specific_mapping, '.csv for detailed harmonization method seetings' ) )
    harmonization_method <<- NA
    harm_type <<- NA
    offset_reduce_value <<- NA
    offset_reduce_year <<- NA
    ratio_reduce_value <<- NA
    ratio_reduce_year <<- NA
  } else {
    harmonization_method_specific <- NA
    harmonization_method_specific_type <- NA
    harmonization_method_specific_mapping <- NA
    harm_method_list <- c( 'offset_constant', 'offset_reduced',
                           'ratio_constant', 'ratio_reduced', 'no_harmonization' )
    harm_method <<- tango_iam_list$harmonization_method
    harm_type <<- unlist( strsplit( harm_method, '_' ) )[ 1 ]
    if ( ( harm_method %in% harm_method_list ) == F ) {
      stop( 'harmonization provided in the configuration file is invalid' ) }
    if ( harm_method %in% c( 'offset_constant', 'ratio_constant' ) ) {
      offset_reduce_value <- NA
      offset_reduce_year <- NA
      ratio_reduce_value <- NA
      ratio_reduce_year <- NA
    } else if ( harm_method == 'offset_reduced' ) {
      offset_reduce_value <<- tango_iam_list$offset_reduce_value
      offset_reduce_year <<- tango_iam_list$offset_reduce_year
      ratio_reduce_value <- NA
      ratio_reduce_year <- NA
    } else if( harm_method == 'ratio_reduced' ) {
      offset_reduce_value <- NA
      offset_reduce_year <- NA
      ratio_reduce_value <<- tango_iam_list$ratio_reduce_value
      ratio_reduce_year <<- tango_iam_list$ratio_reduce_year
      }
  }

  # - downscaling related parameters
  ds_sector_scheme <<- tango_iam_list$downscaling_sector_scheme
  ds_convergence_year_mapping <<- tango_iam_list$downscaling_convergence_year_mapping
  ds_start_year <<- as.integer( tango_iam_list$downscaling_start_year )
  ds_end_year <<- as.integer( tango_iam_list$downscaling_end_year)
  if ( ds_sector_scheme == 'CEDS16' ) {
    ds_sector_mapping <<- tango_iam_list$downscaling_sector_mapping_CEDS16
    ds_method_mapping <<- tango_iam_list$downscaling_method_mapping_CEDS16
    iamc_var_name_mapping <<- tango_iam_list$IAMC_sector_name_mapping_CEDS16
  } else {
      ds_sector_mapping <<- tango_iam_list$downscaling_sector_mapping_CEDS9
      ds_method_mapping <<- tango_iam_list$downscaling_method_mapping_CEDS9
      iamc_var_name_mapping <<- tango_iam_list$IAMC_sector_name_mapping_CEDS9
      }
  # - gridding related parameters
  gridding_sector_scheme <<- tango_iam_list$gridding_sector_scheme
  gridding_location_index <<- tango_iam_list$gridding_location_index
  grid_resolution <<- as.numeric( tango_iam_list$gridding_resolution )
    if ( ds_sector_scheme == 'CEDS16' ) {
    gridding_sector_mapping <<- tango_iam_list$gridding_sector_mapping_CEDS16
    gridding_proxy_path <<- tango_iam_list$gridding_proxy_path_CEDS16
    gridding_proxy_substitution_mapping <<- tango_iam_list$gridding_proxy_substitution_mapping_CEDS16
    gridding_seasonality_mapping <<- tango_iam_list$gridding_seasonality_mapping_CEDS16
    gridding_proxy_mapping <<- tango_iam_list$gridding_proxy_mapping_CEDS16
  } else {
    gridding_sector_mapping <<- tango_iam_list$gridding_sector_mapping_CEDS9
    gridding_proxy_path <<- tango_iam_list$gridding_proxy_path_CEDS9
    gridding_proxy_substitution_mapping <<- tango_iam_list$gridding_proxy_substitution_mapping_CEDS9
    gridding_seasonality_mapping <<- tango_iam_list$gridding_seasonality_mapping_CEDS9
    gridding_proxy_mapping <<- tango_iam_list$gridding_proxy_mapping_CEDS9
  }

  # return the list into global environment
  return( tango_iam_list )
  }

# ------------------------------------------------------------------------------
# getFileExt
# Brief: get file extensions for a list of file names
# Dependencies:
# Author: Leyang Feng
# parameters: file_list
# return:
# input files:
# output:
getFileExt <- function( file_list ) {
  split_res <- strsplit( file_list, split = '.', fixed = T )
  ext_list <- sort( unique( unlist( lapply( split_res, function( each_res ) {
    split_len <- length( each_res )
    tango_ext <- each_res[ split_len ]
    } ) ) ) )

  if ( length( ext_list ) > 1 ) { stop( 'More than one file type provided' ) }
  if ( !( ext_list %in% c( 'xlsx', 'csv', 'xls') )  ) { stop( 'Unknown file type provided' ) }

  return( ext_list )
  }
