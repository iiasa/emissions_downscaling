# ------------------------------------------------------------------------------
# Program Name: B.1.IAM_snapshot_reformatting
# Author(s): Leyang Feng
# Date Last Updated: Feb 13, 2017 
# Program Purpose: The script reads in IAM snapshots and separate the information
#                  in 'Variable' column, then write the df into intermediate_out 
#                  folder as input for next script.  
# Input Files: csv or excels in /input/IAM_snapshot folder 
# Output Files: B.[iam_name]_emissions_reformatted 
# Notes: 
# TODO: Check with Matt -- duplicated rows in Snapshot data file? 
#       Find a more smart way to deal with emissions only for world region 
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
  log_msg <- "Reformat IAM snapshot to separate information for sector, species, etc. " 
  script_name <- "B.1.IAM_snapshot_reformatting"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
  if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
  iam <- args_from_makefile[ 1 ]
  harm_status <- args_from_makefile[ 2 ]
  snapshot_file <- args_from_makefile[ 3 ]   
  modb_out <- args_from_makefile[ 4 ]  
  if ( is.na( iam ) ) iam <- "GCAM4"
  if ( is.na( snapshot_file ) ) stop( 'No snapshot file provided!' )

  MODULE_B <- "../code/module-B/"

# ------------------------------------------------------------------------------
# 1. Read mapping files and axtract iam info
# read in master config file
  master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list
  iam_info_list <- iamInfoExtract( master_config, iam )

  ds_sector_mapping <- readData( 'MAPPINGS', ds_sector_mapping )
  region_mapping <- readData( 'MAPPINGS', ref_region_mapping )

  print( paste0( 'The IAM to be processed is: ', iam_name  ) )

# -----------------------------------------------------------------------------
# 2. Read in the snapshot and tease out un-wanted models
# file_list <- list.files( './IAM_snapshot' )
# file_list <- file_list[ file_list!= 'README' ]
# 
# data_list <- lapply( file_list, function( file_name ) {
#   file_ext <- getFileExt( file_name )
#   file_name_no_ext <- file_path_sans_ext( file_name )
#   df <- readData( domain = 'INPUT',
#                   domain_extension = 'IAM_snapshot/',
#                   file_name = file_name_no_ext,
#                   extension = paste0( '.', file_ext ) )
#   return( df )
#   } )
# data_df <- do.call( 'rbind', data_list )
# data_df <- data_df[ data_df$model %in% iam_name, ]
  snapshot_file_parts <- unlist( strsplit( snapshot_file, '.', fixed = T ) )
  snapshot_file_ext <- tolower( snapshot_file_parts[ length( snapshot_file_parts ) ] )

  if ( snapshot_file_ext == 'csv' ) { 
    data_df <- read.csv( snapshot_file, stringsAsFactors = F )
  }  
  if ( snapshot_file == 'xlsx' ) { 
    data_df <- read_excel( snapshot_file ) 
  }

# remove duplicated rows in snapshot data 
  data_df <- unique( data_df )

# -----------------------------------------------------------------------------
# 3. Reformat the IAM data
  year_list <- c( 2015, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100 )
  x_year_list <- paste0( 'X', year_list )
  native_reg_list <- sort( unique( region_mapping$region ) )  
  R5_regions <- c( "R5ASIA", "R5LAM", "R5MAF", "R5OECD", "R5REF" )

  iam_data <- data_df
  iam_data <- iam_data[ iam_data$MODEL %in% iam_name, ]
  iam_data <- iam_data[ iam_data$TSYEAR %in% year_list, ]
  iam_data <- merge( iam_data, 
                     ds_sector_mapping[ ds_sector_mapping$harm_status == harm_status, ], 
                     by.x = 'WHAT', by.y = 'variable' )
  iam_data <- iam_data[ !is.na( iam_data$CEDS9 ), ]  
  iam_data$unit <- 'Mt'

  iam_data <- iam_data[ , c( 'model', 'SCENARIO', 'REGION', 'em',  'CEDS9', 'harm_status', 'unit', 'TSYEAR', 'TSVALUE' ) ]
  colnames( iam_data ) <- c( 'model', 'scenario', 'region', 'em', 'sector', 'harm_status', 'unit', 'year' , 'value' )

  iam_data <- iam_data[ iam_data$region %!in% R5_regions, ]

# -----------------------------------------------------------------------------
# 4. Complete the layout and fill the missing sector/region with value 0 
  native_reg_list <- native_reg_list 
  sector_list <- sort( unique( ds_sector_mapping$CEDS9 ) )
  scenario_list <- sort( unique( iam_data$scenario ) ) 
  em_list <- sort( unique( iam_data$em ) )
  year_list <- year_list 
  
  genCompleteLayout <- function( ) { 
    sector_list <- sector_list[ sector_list != "International Shipping" ]
    sector_list <- sector_list[ sector_list != "Aircraft" ]
    sce_res_list <- lapply( scenario_list, function( sce ) { 
      em_res_list <- lapply( em_list, function( em ) { 
        sec_res_list <- lapply( sector_list, function( sec ) { 
          reg_res_list <- lapply( native_reg_list, function( reg ) {
            year_res_list <- lapply( year_list, function( year ) { 
              out_df <- data.frame( scenario = sce, 
                                    em = em, 
                                    region = reg,
                                    sector = sec, 
                                    year = year, 
                                    stringsAsFactors = F )
              } )
            year_res <- do.call( 'rbind', year_res_list )
            } ) 
          reg_res <- do.call( 'rbind', reg_res_list )
          } )
        sec_res <- do.call( 'rbind', sec_res_list )
        if ( iam == 'GCAM4' ) model_world <- 'World'
        if ( iam == 'REMIND-MAGPIE' ) model_world <- 'World'
        air_res <- data.frame( scenario = sce, 
                               em = em, 
                               region = model_world,
                               sector = "Aircraft", 
                               year = year_list, 
                               stringsAsFactors = F )
        shp_res <- data.frame( scenario = sce, 
                               em = em, 
                               region = model_world,
                               sector = "International Shipping", 
                               year = year_list, 
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
                     by = c( 'model', 'scenario', 'region', 'em', 'sector', 'harm_status', 'unit', 'year' ), 
                     all.y = T )
  iam_data$value <- ifelse( is.na( iam_data$value ), 0, iam_data$value ) 
  
  iam_data <- spread( iam_data, year, value ) 
  colnames( iam_data ) <- c( 'model', 'scenario', 'region', 'em', 'sector', 'harm_status', 'unit', paste0( 'X', year_list ) ) 

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
  out_filname <- paste0( 'B.', iam_name, '_emissions_reformatted' )
  writeData( iam_int, 'MED_OUT', out_filname, meta = F )

# END

