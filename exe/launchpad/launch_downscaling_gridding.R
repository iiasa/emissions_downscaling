# ------------------------------------------------------------------------------
# Program Name: luanch_downscaling_gridding.R
# Author(s): Leyang Feng
# Date Last Updated: Nov 28, 2017 
# Program Purpose: The script runs downscaling and gridding
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
headers <- c( 'common_data.R', 'data_functions.R', 'module-A_functions.R', 'all_module_functions.R' ) 
log_msg <- "Initiate downscaling routines." 
script_name <- "launch_downscaling.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 1. Set up desired IAM to be processing

# debug
args_from_makefile <- c( 'REMIND-MAGPIE',
                        'Harmonized-DB',
                        'C:/Users/guti220/Desktop/emissions_downscaling/input/IAM_emissions/output_harmonized.xlsx',
                        'C:/Users/guti220/Desktop/emissions_downscaling/final-output/module-B',
                        'C:/Users/guti220/Desktop/emissions_downscaling/final-output/module-C',
                        'NOTgridding' )

# the flag for intermediate file cleaning
MED_OUT_CLEAN <- T

# getting target IAM from command line arguement
if ( !exists( 'args_from_makefile' ) ) args_from_makefile <- commandArgs( TRUE )
iam <- args_from_makefile[ 1 ]
harm_status <- args_from_makefile[ 2 ]
input_file <- args_from_makefile[ 3 ]   
modb_out <- args_from_makefile[ 4 ]    
modc_out <- args_from_makefile[ 5 ]
gridding_flag <- args_from_makefile[ 6 ]
RUNSUFFIX <- substr( sha1( runif(1) ), 1, 6 ) 

# update domainmapping for current run 
domainmapping <- read.csv( DOMAINPATHMAP, stringsAsFactors = F )

# create modb_out 
dir.create( modb_out )
# create modc_out if only the gridding flag is given  
if ( gridding_flag == 'gridding' ) { 
dir.create( modc_out )
} else { 
  modc_out <- NA 
  }

# create unique directory for intermediate files 
med_out <- paste0( '../intermediate-output', '/', RUNSUFFIX )
dir.create( med_out )

domainmapping[ domainmapping$Domain == 'MED_OUT', "PathToDomain" ] <- med_out
domainmapping[ domainmapping$Domain == 'MODB_OUT', "PathToDomain" ] <- modb_out
domainmapping[ domainmapping$Domain == 'MODC_OUT', "PathToDomain" ] <- modc_out
#write.csv( domainmapping, DOMAINPATHMAP, row.names = F )

# -----------------------------------------------------------------------------
# 2. Source module-B script in order
source( '../code/module-B/B.1.IAM_input_reformatting.R' )
source( '../code/module-B/B.2.IAM_reference_emission_preparation.R' )
source( '../code/module-B/B.3.regional_pop_gdp_preparation.R' )
source( '../code/module-B/B.4.1.IAM_emissions_downscaling_linear.R' )
source( '../code/module-B/B.4.2.IAM_emissions_downscaling_ipat.R' )
source( '../code/module-B/B.5.IAM_emissions_downscaled_cleanup.R' )

# -----------------------------------------------------------------------------
# 3. Source module-C script in order
if ( gridding_flag == 'gridding' ) { 
  source( '../code/module-C/C.1.gridding_data_reformatting.R' )
  source( '../code/module-C/C.2.1.gridding_nonair.R' )
  source( '../code/module-C/C.2.2.gridding_air.R' ) 
}

# -----------------------------------------------------------------------------
# 4. clean the intermediate files
if ( MED_OUT_CLEAN ) { 
  invisible( unlink( med_out, recursive = T ) )
  invisible( file.remove( paste0( '../documentation/IO_documentation_', RUNSUFFIX, '.csv' ) ) )
}
