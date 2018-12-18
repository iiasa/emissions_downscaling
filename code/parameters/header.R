# Copyright 2018 Battelle Memorial Institute

# ------------------------------------------------------------------------------
# IAMH R header file: Script initialization (adapted from CEDS)
# Authors: Jon Seibert, Caleb Braun
# Last Updated: 8 November 2018
#
# This file must be sourced by all IAMH R scripts to perform log initialization,
#   read in other required functions, and note initial dependencies.
# Functions contained:
#   sourceFunctions, addDep, initialize
#
# Notes: Requires functions in IO_functions.R (automatically loaded)
#
# ------------------------------------------------------------------------------

initialize <- function( script_name, log_msg, headers ) {
  # Ensure the critical headers are read in first, in the correct order
  required_headers <- c( "IO_functions.R", "global_settings.R" )
  headers <- union( required_headers, headers )

  # PARAM_DIR is a global var defined by each script
  header_paths <- paste0( PARAM_DIR, headers )

  invisible( lapply( header_paths, source ) )
	logStart( script_name )
  clearMeta()
  invisible( lapply( header_paths, addDependency ) )
  printLog( log_msg )
}
