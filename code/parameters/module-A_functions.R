#-----------------------------------------------------------------------------------------------------------------
# Program Name: module-A_functions.R
# Author's Name: Leyang Feng
# Date Last Modified: 
# Program Purpose: Core functions for module-A
# Note: 
# TODO: 
# ----------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# interpolateXyears_linear
# Brief:  linearly interpolate griven df ( based on na.approx {zoo} ) 
# Dependencies: interpolate_NAs{data_functions.R} 
# Author: Leyang Feng
# parameters: df - the df needs to be interpolated. MUST contains no header columns. 
# return: linearly interpolated df
# input files: null
# output: null
interpolateXyears_linear <- function( df ) { 
  int_df_linear <- interpolate_NAs( df )
  return( int_df_linear )
  }

# ------------------------------------------------------------------------------
# interpolateXyears
# Brief: interpolate NA values under X_year columns for a given data frame.   
# Dependencies: interpolateXyears_linear
# Author: Leyang Feng
# parameters: df - the data frame for interpolation
#             iam_info -- the iam_info list 
# return: the interpolated data frame 
# input files: null
# output: null
interpolateXyears <- function( df, int_method = 'linear' ) {
  
  # extract the 'header' columns and put them aside for later use
  header_col_names <- grep( 'X', colnames( df ), value = T, invert = T )
  header_cols <- df[ , header_col_names ]
  
  # extract the X_years columns and use them in interpolation 
  X_year_cols <- df[ , grep( 'X', colnames( df ), value = T ) ]
  
  # do the interpolation regarding to different interpolation methods 
  if ( int_method == 'linear' ) { 
    int_X_years <- interpolateXyears_linear( X_year_cols )
  } else {
    stop( 'interpolation method not recognized! ' )
    }
  
  # add the header columns back to the interpolated X_years df 
  int_df <- cbind( header_cols, int_X_years )
  
  return( int_df )
  }


# -----------------------------------------------------------------------------
# convertUnit 
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null
convertUnit <- function( df ) {
  
  # define some conversion factor 
  kt2mt <- 0.001
  
  # check the unit column first 
  df_colnames <- tolower( colnames( df ) )
  unit_col_index <- which(  df_colnames %in% c( 'units', 'unit' ) )
  
  if ( length( unit_col_index ) > 1 ) {
    stop( printLog( 'More than one unit column in the given data frame' ) )
    }
  
  # do the unit conversion 
  df_unit_list <- tolower( unique( df[ , unit_col_index ] ) ) 
  
  df_unit_others <- df_unit_list[ which( df_unit_list %!in% 'mt' ) ]
  
  if ( length( df_unit_others ) > 0 ) { 
    
    df_converted <- df 
    
    cols_for_conversion <- grep( 'X', colnames( df_converted ), value = T )
    
    for ( df_unit_other in df_unit_others ) { 
       if ( df_unit_other == 'kt' ) { 
         rows_for_conversion <- which( df_converted[ , unit_col_index ] %in% df_unit_other ) 
         df_converted[ rows_for_conversion, cols_for_conversion ] <- df_converted[ rows_for_conversion, cols_for_conversion ] * kt2mt
         df_converted[ , unit_col_index ] <- 'mt'  
         }
      }
    } else { df_converted <- df }
  
  return( df_converted )
}

# -----------------------------------------------------------------------------
# dropUncoveredEmissions  
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null
dropUncoveredIAMEmissions <- function( em_coverage_mapping, em_data, iam_name, ref_name ) {
  
  iam_em_list <- em_coverage_mapping[ , iam_name ] 
  ref_em_list <- em_coverage_mapping[ , ref_name ]
  
  common_em <- intersect( iam_em_list, ref_em_list ) 

  em_not_in_ref <- iam_em_list[ which( iam_em_list %!in% common_em ) ]
  printLog( paste0( 'Reference emission data does not have following emission species: ', 
                    paste( em_not_in_ref, collapse = ', ' ) ) )
  
  
  
  rows_dropping <- unlist( lapply( em_not_in_ref, grep, em_data[ , 'em' ] ) )
  
  if ( length( rows_dropping ) > 0  ) {
  
    em_data_kept <- em_data[ -rows_dropping, ]
    em_data_dropped <- em_data[ rows_dropping, ]
  
    return_list <- list( em_data_kept, em_data_dropped )
    names( return_list ) <- c( 'iam_em', 'iam_em_dropped' )
    
  } else { 
    em_data_kept <- em_data
    em_data_dropped <- data.frame() 
    
    return_list <- list( em_data_kept, em_data_dropped )
    names( return_list ) <- c( 'iam_em', 'iam_em_dropped' )
    
    }
  
  return( return_list )
  }

# -----------------------------------------------------------------------------
# createIAMLayout
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null
createIAMLayout <- function( num_regions, num_sectors, num_scenarios, num_em, num_model, num_unit ) {
  layout<- data.frame( em = NULL, region = NULL, CEDS16 = NULL, scenario = NULL )
  for ( em in num_em ) {
    for ( scenario in num_scenarios ) {
      for ( sector in num_sectors ) {
        
        df_block <- data.frame( em = em, region = num_regions, CEDS16 = sector, scenario = scenario, stringsAsFactors = F )
        
        layout <- rbind( layout, df_block ) 
        }
      }
  }
  
  layout$unit <- num_unit 
  layout$model <- num_model
  return( layout )
  
  }

# --------------------------------------------------------------------------
# completeLayoutNA
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null
completeLayoutNA <- function( df, num_regions, num_sectors, num_scenarios, num_em, num_model, num_unit, iam_x_years ) { 
  
  standard_layout <- createIAMLayout( num_regions, num_sectors, num_scenarios, num_em, num_model, num_unit ) 
  
  df_merge <- merge( standard_layout, df, by = intersect( names( standard_layout ), names( df ) ), 
                     all.x = T )
  df_return <- df_merge[ , c( c(  'model', 'scenario', 'region', 'em', 'CEDS16', 'unit', iam_x_years ) ) ]

  return( df_return )
  }
# -----------------------------------------------
# extractVarInfo
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null
extractVarInfo <- function( x, var_col = 'VARIABLE' ) { 
  
  out_df <- x
  
  temp_var <- x[ , var_col ] 
  temp_str_parts <- strsplit( temp_var, split = '|', fixed = T )
  temp_parts_count <- unlist( lapply( temp_str_parts, length ) )
  var_list <- unlist( lapply( seq_along( temp_str_parts ), function( i ) { 
    paste( temp_str_parts[[ i ]] [ 1 : ( temp_parts_count[i] - 1 ) ], collapse = '|' ) } ) )
  em_list <- unlist( lapply( temp_str_parts, '[[', 4 ) )
  
  out_df[ , var_col ] <- var_list 
  out_df$em <- em_list
  
  return( out_df )
}

# -----------------------------------------------
# CalculateOffset
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null

calculateOffset <- function( iam_em, 
                             ref_em, 
                             baseyear = '2015', 
                             harmonization_type  = NA, 
                             harm_method_specific_flag = 'N',
                             harm_method_specific_mapping = NA ) {
  # basic logics: The function goes into two mode depending on the value of harm_method_specific_flag. 
  #               The value of harmo_method_specific_flag is decided in the master_confic.csv
  #               If the harm_method_specific_flag does not equal to 'Y', the function uses default 
  #               harmonization method defined in the master_config.csv which applys to all emissions for 
  #               all emission species, sector, and region. 
  #               If the harm_method_specific_flag equals to 'Y', detailed harmonization should be provided 
  #               in the mappings folder defining different harmonization methods for emission species, sector, region 
  #               or their combination. 
  
  # ----
  # 1. merge iam_em and ref_em together using 'region', 'em', 'CEDS16'
  x_baseyear <- paste0( 'X', baseyear )
  iam_baseyear <- iam_em[ , c( "model", "scenario", "em", "CEDS16", "region", 'unit', x_baseyear ) ]
  ref_baseyear <- ref_em[ , c( "ref_em", "em", "CEDS16", "region", "unit", x_baseyear ) ]
  
  df_merge <- merge( iam_baseyear, ref_baseyear, by = c( 'em', 'region', 'CEDS16' ), all.x = T )
  
  # ---
  # 2. layout check -- there should be no NAs in the merged data frame 
  NA_check <- any( is.na( c( df_merge[ , paste0( x_baseyear, '.x' ) ], df_merge[ , paste0( x_baseyear, '.y' ) ] ) ) )
  
  if ( NA_check ) { 
    message( 'There are umatched emissions in either IAM emissions or reference emissions...converting unmatched items into 0 ')
    df_merge[ , paste0( x_baseyear, '.x' ) ] <- ifelse( is.na( df_merge[ , paste0( x_baseyear, '.x' ) ] ) , 0, 
                                                      df_merge[ , paste0( x_baseyear, '.x' ) ] )
    df_merge[ , paste0( x_baseyear, '.y' ) ] <- ifelse( is.na( df_merge[ , paste0( x_baseyear, '.y' ) ] ), 0, 
                                                      df_merge[ , paste0( x_baseyear, '.y' ) ] )
    }
  
  # ---
  # 3. chcek flag value and decide harmonization stradegy 
  if ( harm_method_specific_flag == 'Y' ) { 
    # under this condition, the harm_method_specific_type should exist in the global environment
    # harm_method_specific_type could have several valid values, for now only construct method for 'sector'
    if ( harm_method_specific_type == 'sector' ) { 
      harm_sectors <- harm_method_specific_mapping$CEDS16 
      offset_df_list <- lapply( harm_sectors, calculateOffsetBySector, df_merge, x_baseyear, harm_method_specific_mapping )
      offset_df <- do.call( 'rbind.fill', offset_df_list )
      }
    if ( harm_method_specific_type == 'sector-region' ) { 
      stop( 'the method is not supported yet.' )
      }
    
  }
  if ( harm_method_specific_flag != 'Y' ) { 
    # ---
    # 3.2. generate offset_df using default harmonization method ( which has two possibilities )
  if ( harmonization_type == 'offset' ) { 
    header_col_name <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit.y" ) 
    offset_df <- df_merge[ , header_col_name ]
    colnames( offset_df ) <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit" ) 
    offset_df$harmonization_type <- harmonization_type
    offset_df[ , x_baseyear ] <- df_merge[ , paste0( x_baseyear, '.y' ) ] - df_merge[ , paste0( x_baseyear, '.x' ) ]
    }
  
  if ( harmonization_type == 'ratio' ) { 
    header_col_name <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit.y" ) 
    offset_df <- df_merge[ , header_col_name ]
    colnames( offset_df ) <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit" ) 
    offset_df$harmonization_type <- harmonization_type
    offset_df[ , x_baseyear ] <- df_merge[ , paste0( x_baseyear, '.y' ) ] / df_merge[ , paste0( x_baseyear, '.x' ) ]
    offset_df[ , x_baseyear ] <- ifelse( is.nan( offset_df[ , x_baseyear ] ), 1, offset_df[ , x_baseyear ] )
    offset_df[ , x_baseyear ] <- ifelse( is.infinite( offset_df[ , x_baseyear ] ), 1, offset_df[ , x_baseyear ] )
  }
  
  if ( is.na( harmonization_type ) ) { 
    stop( 'Something wrong. The harm_specific_flag shoud be Y. ' )
    }    
    
  }  
  
  # ---
  # 4. additional routine of write out both offset and ratio as a diagnostic output
  header_col_name <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit.y" ) 
  diag_df <- df_merge[ , header_col_name ]
  colnames( diag_df ) <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit" ) 
  diag_df[ , paste0( 'offset_', x_baseyear ) ] <- df_merge[ , paste0( x_baseyear, '.y' ) ] - df_merge[ , paste0( x_baseyear, '.x' ) ]
  diag_df[ , paste0( 'offset_', x_baseyear ) ] <- abs( diag_df[ , paste0( 'offset_', x_baseyear ) ] )
  diag_df[ , paste0( 'ratio_', x_baseyear ) ] <- df_merge[ , paste0( x_baseyear, '.y' ) ] / df_merge[ , paste0( x_baseyear, '.x' ) ]
  diag_df[ , paste0( 'iam_', x_baseyear ) ] <- df_merge[ , paste0( x_baseyear, '.x' ) ]
  diag_df[ , paste0( 'ref_', x_baseyear ) ] <- df_merge[ , paste0( x_baseyear, '.y' ) ]
  out_filname <- paste0( 'A.', ref_name, '_', iam_name, '_baseyear_offset_ratio' )
  writeData( diag_df, 'DIAG_OUT', out_filname )  
  
  return( offset_df )
}

# -----------------------------------------------
# calculateOffsetBySector
# Brief:  Called by CalculateOffset
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null

calculateOffsetBySector <- function( sector, full_df, x_baseyear, harm_method_mapping ) { 
  # ---
  # 1. extract harm method adn parameters from harm_mthod_mapping
  harm_method <- harm_method_mapping[ harm_method_mapping$CEDS16 == sector, 'harm_method' ]
  harm_type <- unlist( strsplit( harm_method, '_' ) )[ 1 ]
  
  # ---
  # 2. extract the sector data from full_df  
  harm_temp_df <- full_df[ full_df$CEDS16 == sector, ]
  
  # ---
  # 3. calculate offset
  if ( harm_type == 'offset' ) { 
    header_col_name <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit.y" ) 
    offset_df <- harm_temp_df[ , header_col_name ]
    colnames( offset_df ) <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit" ) 
    offset_df$harmonization_type <- harm_type
    offset_df[ , x_baseyear ] <- harm_temp_df[ , paste0( x_baseyear, '.y' ) ] - harm_temp_df[ , paste0( x_baseyear, '.x' ) ]
    }
  if ( harm_type == 'ratio' ) { 
    header_col_name <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit.y" ) 
    offset_df <- harm_temp_df[ , header_col_name ]
    colnames( offset_df ) <- c( "em", "region", "CEDS16", "model", "scenario", "ref_em", "unit" ) 
    offset_df$harmonization_type <- harm_type
    offset_df[ , x_baseyear ] <- harm_temp_df[ , paste0( x_baseyear, '.y' ) ] / harm_temp_df[ , paste0( x_baseyear, '.x' ) ]
    offset_df[ , x_baseyear ] <- ifelse( is.nan( offset_df[ , x_baseyear ] ), 1, offset_df[ , x_baseyear ] )
    offset_df[ , x_baseyear ] <- ifelse( is.infinite( offset_df[ , x_baseyear ] ), 1, offset_df[ , x_baseyear ] )
  }
  
  return( offset_df )	
}

# -----------------------------------------------
# extendOffset
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null

extendOffset <- function( offset_df, 
                          baseyear = '2015',
						              harmonization_method = NA,
						              harm_method_specific_flag = 'N',
						              harm_method_specific_mapping = NA,
                          harm_start_year = NA,
						              harm_end_year = NA,
						              offset_reduce_year = NA, 
					                offset_reduce_value = NA,
						              ratio_reduce_year = NA,
					                ratio_reduce_value = NA ) { 
  # basic logics: The function goes into two mode depending 
  #               on the value of harm_method_specific_flag.
  #               The value of harmo_method_specific_flag is
  #               decided in the master_confic.csv
  #               If the harm_method_specific_flag does not equal to 'Y', 
  #               the function uses default harmonization method defined 
  #               in the master_config.csv which applys to all emissions for 
  #               all emission species, sector, and region. 
  #               If the harm_method_specific_flag equals to 'Y', 
  #               detailed harmonization should be provided in 
  #               the mappings folder defining different harmonization methods 
  #               for emission species, sector, region or their combination. 
  
  # ---
  # 1. setup some basics 
  header_col_names <- c( "model", "scenario", "em", "region", "CEDS16", "unit" ) 
  x_baseyear <- paste0( 'X', baseyear )
  
  # ---
  # 2. chcek flag value and decide harmonization stradegy 
  if ( harm_method_specific_flag == 'Y' ) { 
    # ---
	  # 2.1 condition when specific harmonization methods are available
	
    # under this condition, the harm_method_specific_type should exist in the global environment
    # harm_method_specific_type could have several valid values, for now only construct method for 'sector'
    if ( harm_method_specific_type == 'sector' ) { 
      #harm_sectors <- harm_method_specific_mapping$CEDS16 
      harm_sectors <- unique( offset_df$CEDS16 )
      offset_extended_list <- lapply( harm_sectors, 
                                      harmMethod_selectNapply_bySector, 
                                      offset_df, 
                                      harm_method_mapping, 
                                      harm_start_year, 
                                      harm_end_year, 
                                      x_baseyear, 
                                      header_col_names )
      offset_extended <- do.call( 'rbind.fill', offset_extended_list )
      }
    if ( harm_method_specific_type == 'sector-region' ) { 
      stop( 'the method is not supported yet.' )
      }
  }
  if ( harm_method_specific_flag != 'Y' ) { 
    # ---
  	# 2.2 condition when default harmonization method is avaiable
	
  	# under this condition, there are four methods avaiable
    # ------
    offset_extended <- harmMethod_selectNapply( harm_method = harmonization_method,
                                                offset_df = offset_df,
                                                harm_start_year = harm_start_year,
                                                harm_end_year = harm_end_year,
									                              offset_reduce_year = offset_reduce_year, 
								                                offset_reduce_value = offset_reduce_value,
							                                  ratio_reduce_year = ratio_reduce_year,
							                                  ratio_reduce_value = ratio_reduce_value ,
							                                  x_baseyear = x_baseyear,
							                                  header_col_names = header_col_names )    
  }
  
  return( offset_extended )
}

# -----------------------------------------------
# harm_offsetConstant
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null

harm_offsetConstant <- function( offset_df, 
                                 harm_start_year, 
								                 harm_end_year,
                                 x_baseyear,
                                 header_col_names ) { 
  offset_extended <- offset_df 
  harm_x_years <- paste0( 'X', harm_start_year : harm_end_year )
  harm_x_years_to_fill <- harm_x_years[ which( !( harm_x_years %in% x_baseyear ) ) ]
  offset_extended[ , harm_x_years_to_fill ] <- offset_extended[ , x_baseyear ]
  offset_extended <- offset_extended[ , c( header_col_names, harm_x_years ) ]
  
  return( offset_extended ) 
}

# -----------------------------------------------
# harm_offsetReduced
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null

harm_offsetReduced <- function( offset_df,
                                harm_start_year,
								                harm_end_year, 
								                offset_reduce_year, 
								                offset_reduce_value,
								                x_baseyear,
								                header_col_names ) {
  offset_extended <- offset_df 
  harm_x_years <- paste0( 'X', harm_start_year : harm_end_year )
  x_reduce_year <- paste0( 'X', offset_reduce_year ) 
  reduce_x_period <- paste0( 'X', harm_start_year : offset_reduce_year )
  reduce_x_period_to_fill <- reduce_x_period[ which( !( reduce_x_period %in% x_baseyear ) ) ]
  offset_extended[ , reduce_x_period_to_fill ] <- NA
  offset_extended[ , x_reduce_year ] <- offset_reduce_value 
  offset_extended <- interpolateXyears( offset_extended, int_method = 'linear' )
  rest_years_to_fill <- harm_x_years[ which( !( harm_x_years %in% reduce_x_period ) ) ]
  offset_extended[ , rest_years_to_fill ] <- 0
  offset_extended <- offset_extended[ , c( header_col_names, harm_x_years ) ]
	  
  return( offset_extended )
}

# -----------------------------------------------
# harm_RatioConstant
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null

harm_RatioConstant <- function( offset_df,
                                harm_start_year,
							                	harm_end_year,
							                 	x_baseyear, 
							                	header_col_names ) { 
  offset_extended <- offset_df 
  harm_x_years <- paste0( 'X', harm_start_year : harm_end_year )
  harm_x_years_to_fill <- harm_x_years[ which( !( harm_x_years %in% x_baseyear ) ) ]
  offset_extended[ , harm_x_years_to_fill ] <- offset_extended[ , x_baseyear ]
  offset_extended <- offset_extended[ , c( header_col_names, harm_x_years ) ] 
 
  return( offset_extended ) 
}

# -----------------------------------------------
# harm_RatioReduced
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null

harm_RatioReduced <- function( offset_df,
                               harm_start_year,
							                 harm_end_year,
							                 ratio_reduce_year,
							                 ratio_reduce_value,
							                 x_baseyear,
							                 header_col_names ) { 
  offset_extended <- offset_df 
  harm_x_years <- paste0( 'X', harm_start_year : harm_end_year )
  x_reduce_year <- paste0( 'X', ratio_reduce_year ) 
  reduce_x_period <- paste0( 'X', harm_start_year : ratio_reduce_year )
  reduce_x_period_to_fill <- reduce_x_period[ which( !( reduce_x_period %in% x_baseyear ) ) ]
  offset_extended[ , reduce_x_period_to_fill ] <- NA
  offset_extended[ , x_reduce_year ] <- ratio_reduce_value 
  offset_extended <- interpolateXyears( offset_extended, int_method = 'linear' )
  rest_years_to_fill <- harm_x_years[ which( !( harm_x_years %in% reduce_x_period ) ) ]
  offset_extended[ , rest_years_to_fill ] <- 1
  offset_extended <- offset_extended[ , c( header_col_names, harm_x_years ) ]

  return( offset_extended )
  }

# -----------------------------------------------
# harmMethod_selectNapply
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null
  
harmMethod_selectNapply <- function( harm_method = NA,
                                     offset_df = NA,
									                   harm_start_year = NA,
							                       harm_end_year = NA,
									                   offset_reduce_year = NA, 
								                     offset_reduce_value = NA,
							                       ratio_reduce_year = NA,
							                       ratio_reduce_value = NA ,
							                       x_baseyear = NA,
							                       header_col_names = NA ) { 
  if ( harm_method == 'offset_constant' ) {  
    offset_extended <- harm_offsetConstant( offset_df, 
                                            harm_start_year, 
						                                harm_end_year,
                                            x_baseyear,
                                            header_col_names )
  }
  if ( harm_method == 'offset_reduced' ) { 
    offset_extended <- harm_offsetReduced( offset_df,
	                                         harm_start_year,
								                           harm_end_year, 
							                             offset_reduce_year, 
								                           offset_reduce_value,
								                           x_baseyear,
								                           header_col_names )
  }
  if ( harm_method == 'ratio_constant' ) { 
    offset_extended <- harm_RatioConstant( offset_df,
                                           harm_start_year,
								                           harm_end_year,
								                           x_baseyear, 
								                           header_col_names ) }
  if ( harm_method == 'ratio_reduced' ) { 
    offset_extended <- harm_RatioReduced( offset_df,
                                          harm_start_year,
							                            harm_end_year,
							                            ratio_reduce_year,
							                            ratio_reduce_value,
							                            x_baseyear,
							                            header_col_names )
	}
  
  return( offset_extended )
}

# -----------------------------------------------
# harmMethod_selectNapply_bySector
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null

harmMethod_selectNapply_bySector <- function( sector = NA,  
                                              offset_df = NA,
											                        harm_method_mapping = NA,
									                            harm_start_year = NA,
							                                harm_end_year = NA,
							                                x_baseyear = NA,
							                                header_col_names = NA ) { 
										 
  harm_method <- harm_method_mapping[ harm_method_mapping$CEDS16 == sector, "harm_method" ] 
  offset_reduce_year <- harm_method_mapping[ harm_method_mapping$CEDS16 == sector, "offset_reduced_year" ]
  offset_reduce_value <- harm_method_mapping[ harm_method_mapping$CEDS16 == sector, "offset_reduced_value" ]
  ratio_reduce_year <- harm_method_mapping[ harm_method_mapping$CEDS16 == sector, "ratio_reduced_year" ]
  ratio_reduce_value <- harm_method_mapping[ harm_method_mapping$CEDS16 == sector, "ratio_reduced_value" ]
  
  offset_df <- offset_df[ offset_df$CEDS16 == sector, ]
  
  offset_extended <- harmMethod_selectNapply( harm_method = harm_method,
                                              offset_df = offset_df,
									                            harm_start_year = harm_start_year,
							                                harm_end_year = harm_end_year,
									                            offset_reduce_year = offset_reduce_year, 
								                              offset_reduce_value = offset_reduce_value,
							                                ratio_reduce_year = ratio_reduce_year,
							                                ratio_reduce_value = ratio_reduce_value ,
							                                x_baseyear = x_baseyear,
							                                header_col_names = header_col_names )
  
  return( offset_extended )
}

# -----------------------------------------------
# applyOffset
# Brief:  
# Dependencies: 
# Author: Leyang Feng
# parameters: 
# return:  
# input files: null
# output: null

applyOffset <- function( iam_em, 
                         offset_df, 
                         harm_start_year = NA,
                         harm_end_year = NA, 
                         harmonization_type = NA,
                         harmonization_method_specific_flag = NA,
                         harmonization_method_specific_mapping = NA ) { 
  
  # basic logic: The function has two modes depending on the value of harmonization_method_specific_flag. 
  
  
  # ---
  # 1. set up some basics
  iam_em_x_years <- grep( 'X', colnames( iam_em ), value = T )  
  harm_x_years <- paste0( 'X', harm_start_year : harm_end_year )
  none_harm_x_years <- iam_em_x_years[ which( !( iam_em_x_years %in% harm_x_years ) ) ]
  header_col_names <- c( "model", "scenario", "em", "CEDS16", "region", "unit" )
  
  # ---
  # 2. merge the iam_em and offset_df together
  merge_df <- merge( iam_em, offset_df, by = c( "em", "CEDS16", "region", 'scenario' ) )
  
  # ---
  # 3. apply offset depending on the harmonization stratedgy 
  if ( harmonization_method_specific_flag == 'Y' ) { 
    
    # sector check 
    iam_sector_list <- sort( unique( iam_em$CEDS16 ) )
    offset_sector_list <- sort( unique( offset_df$CEDS16 ) )
    if ( !identical( iam_sector_list, offset_sector_list ) ) { stop( 'something wrong, the sectors does not match ' ) }
    
    # apply the offset -- three conditions (1) all offset
    #                                      (2) all ratio
    #                                      (3) some offst some ratio
    
    # extract offset and ratio sectors from the mapping
    mapping_offset_sector <- harmonization_method_specific_mapping[ harmonization_method_specific_mapping$harm_method %in% c( 'offset_constant', 'offset_reduced' ), 'CEDS16' ]
    mapping_ratio_sector <- harmonization_method_specific_mapping[ harmonization_method_specific_mapping$harm_method %in% c( 'ratio_constant', 'ratio_reduced' ), 'CEDS16' ]
    # find offset and ratio sectors for iam_em sectors
    offset_sector_list <- iam_sector_list[ which( iam_sector_list %in% mapping_offset_sector ) ] 
    ratio_sector_list <- iam_sector_list[ which( iam_sector_list %in% mapping_ratio_sector ) ]
    # condition (1) -- apply offset method on merge_df 
    if ( length( ratio_sector_list ) == 0 ) { 
      harmed_df <- merge_df
      harmed_df[ , paste0( harm_x_years, '.x' ) ] <- harmed_df[ , paste0( harm_x_years, '.x' ) ] + 
                                                     harmed_df[ , paste0( harm_x_years, '.y' ) ]
      harmed_df <- harmed_df[ , c( "model.x", "scenario", "em", "CEDS16", "region", "unit.x", 
                                   none_harm_x_years, paste0( harm_x_years, '.x' ) ) ]
      colnames( harmed_df ) <- gsub( '.x', '', colnames( harmed_df ) )
      }
    # condition (2) -- apply ratio method on merge_df 
    if ( length( offset_sector_list ) == 0 ) { 
      harmed_df <- merge_df
      harmed_df[ , paste0( harm_x_years, '.x' ) ] <- harmed_df[ , paste0( harm_x_years, '.x' ) ] * 
                                                     harmed_df[ , paste0( harm_x_years, '.y' ) ]
    
      harmed_df <- harmed_df[ , c( "model.x", "scenario", "em", "CEDS16", "region", "unit.x", 
                                 none_harm_x_years, paste0( harm_x_years, '.x' ) ) ]
      colnames( harmed_df ) <- gsub( '.x', '', colnames( harmed_df ) )
    }
    # condition (3) 
    if ( length( offset_sector_list )!= 0 & length( ratio_sector_list ) != 0 ) { 
      harmed_df_offset <- merge_df[ merge_df$CEDS16 %in% offset_sector_list,  ]
      harmed_df_offset[ , paste0( harm_x_years, '.x' ) ] <- harmed_df_offset[ , paste0( harm_x_years, '.x' ) ] + 
                                                            harmed_df_offset[ , paste0( harm_x_years, '.y' ) ]
      harmed_df_offset <- harmed_df_offset[ , c( "model.x", "scenario", "em", "CEDS16", "region", "unit.x", 
                                                 none_harm_x_years, paste0( harm_x_years, '.x' ) ) ]
      colnames( harmed_df_offset ) <- gsub( '.x', '', colnames( harmed_df_offset ) )
    
      harmed_df_ratio <- merge_df[ merge_df$CEDS16 %in% ratio_sector_list,  ]
      harmed_df_ratio[ , paste0( harm_x_years, '.x' ) ] <- harmed_df_ratio[ , paste0( harm_x_years, '.x' ) ] * 
                                                           harmed_df_ratio[ , paste0( harm_x_years, '.y' ) ]
      harmed_df_ratio <- harmed_df_ratio[ , c( "model.x", "scenario", "em", "CEDS16", "region", "unit.x", 
                                   none_harm_x_years, paste0( harm_x_years, '.x' ) ) ]
      colnames( harmed_df_ratio ) <- gsub( '.x', '', colnames( harmed_df_ratio ) )
      
      harmed_df <- rbind( harmed_df_offset, harmed_df_ratio )

      }
    }
  if ( harmonization_method_specific_flag != 'Y' ) { 
    if ( harmonization_type == 'offset' ) { 
      harmed_df <- merge_df
      harmed_df[ , paste0( harm_x_years, '.x' ) ] <- harmed_df[ , paste0( harm_x_years, '.x' ) ] + 
                                                     harmed_df[ , paste0( harm_x_years, '.y' ) ]
      harmed_df <- harmed_df[ , c( "model.x", "scenario", "em", "CEDS16", "region", "unit.x", 
                                   none_harm_x_years, paste0( harm_x_years, '.x' ) ) ]
      colnames( harmed_df ) <- gsub( '.x', '', colnames( harmed_df ) )
    }
    if ( harmonization_type == 'ratio' ) { 
      harmed_df <- merge_df
      harmed_df[ , paste0( harm_x_years, '.x' ) ] <- harmed_df[ , paste0( harm_x_years, '.x' ) ] * 
                                                     harmed_df[ , paste0( harm_x_years, '.y' ) ]
    
      harmed_df <- harmed_df[ , c( "model.x", "scenario", "em", "CEDS16", "region", "unit.x", 
                                 none_harm_x_years, paste0( harm_x_years, '.x' ) ) ]
      colnames( harmed_df ) <- gsub( '.x', '', colnames( harmed_df ) )
    }
  }
  
  return( harmed_df )
  }