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
headers <- c( 'common_data.R', 'data_functions.R', 'module-A_functions.R', 'all_module_functions.R', 'gridding_functions.R' ) 
log_msg <- "Gridding none-aircraft emissions in given IAM emissions file " 
script_name <- "C.2.1.gridding_nonair.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define IAM variable
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
harm_status <- args_from_makefile[ 2 ]
modc_out <- args_from_makefile[ 3 ]   
if ( is.na( iam ) ) iam <- "GCAM4"

MODULE_C <- "../code/module-C/"

# ------------------------------------------------------------------------------
# 1. Read mapping files and axtract iam info
# read in master config file 
master_config <- readData( 'MAPPINGS', 'master_config', column_names = F )
# select iam configuration line from the mapping and read the iam information as a list 
iam_info_list <- iamInfoExtract( master_config, iam )

print( paste0( 'The IAM to be processed is: ', iam_name  ) )  

# -----------------------------------------------------------------------------
# 2. Initialize gridding settings 
output_dir <- modc_out
proxy_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = gridding_proxy_path )
proxy_backup_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "proxy-backup/")
mask_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "mask/")
seasonality_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "seasonality-CEDS9/" )

gridding_initialize( grid_resolution = grid_resolution,
                     start_year = 2015,
                     end_year = 2100,
                     load_masks = T, 
                     load_seasonality_profile = T )

# -----------------------------------------------------------------------------
# 3. Read in emissions file and mappings 
iam_data <- readData( domain = 'MED_OUT', 
                      file_name = paste0( 'C.', iam_name, '_',  harm_status, '_emissions_reformatted' ) )

iam_data$X2015 <- ifelse( iam_data$X2015 == 'NA', 0, iam_data$X2015 )
iam_data$X2015 <- as.numeric( iam_data$X2015 )

proxy_mapping  <- readData( domain = 'GRIDDING', 
                            domain_extension = 'gridding-mappings/',
                            file_name = gridding_proxy_mapping )
location_index <- readData( domain = 'GRIDDING', 
                            domain_extension = 'gridding-mappings/', 
                            file_name = gridding_location_index ) 
seasonality_mapping <- readData( domain = 'GRIDDING', 
                                 domain_extension = 'gridding-mappings/', 
                                 file_name = gridding_seasonality_mapping ) 
proxy_substitution_mapping <- readData( domain = 'GRIDDING', 
                                        domain_extension = 'gridding-mappings/', 
                                        file_name = gridding_proxy_substitution_mapping ) 
sector_name_mapping <- readData( domain = 'GRIDDING', 
                                 domain_extension = 'gridding-mappings/', 
                                 file_name = gridding_sector_mapping ) 

# -----------------------------------------------------------------------------
# 4. Pre-processing of iam emissions
# remove AIR sector which will be gridded in a separate routine 
iam_em <- iam_data[ iam_data$sector != 'AIR', ]

em_list <- sort( unique( iam_em$em ) )

sce_list <- sort( unique( iam_em$scenario ) )

# -----------------------------------------------------------------------------
# 3. Gridding and writing output data 

for ( sce in sce_list ) { 
  for ( em in em_list ) { 
    gridding_em <- iam_em[ iam_em$scenario == sce & iam_em$em == em, ]
    
    for ( year in year_list ) { 
      
      int_grids_list <- grid_one_year( em, 
                                       year, 
                                       grid_resolution,
                                       gridding_em, 
                                       location_index, 
                                       proxy_mapping, 
                                       proxy_substitution_mapping )
      generate_final_grids_nc( int_grids_list,
                               output_dir,
                               grid_resolution,
                               year,
                               em,
                               sector_name_mapping,
                               seasonality_mapping )
      }
    } 
  }

# -----------------------------------------------------------------------------
# 4. Write out
 

# END
