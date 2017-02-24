#Define some common data 

supported_species <- c( 'BC', 'CO', 'NH3', 'NOx', 'OC', 'Sulfur', 'VOC' )
supported_species_alias <- c( 'BC', 'CO', 'NH3', 'NOx', 'OC', 'SO2', 'NMVOC' )

# Standard submission header columns 
submission_header_cols <- c( 'model', 'scenario', 'region', 'variable', 'unit' ) 