

# downscaleIAMemissions ----------------------------------------
downscaleIAMemissions <- function( wide_df, con_year_mapping, pos_nonCO2) { 
  
  # construct column-names that are dependet on indicated base_year
  # strings (use for select())
  ref_em_BY <- paste0('ctry_ref_em_X', base_year) 
  gdp_BY <- paste0("ctry_gdp_X", base_year)
  # symbols (used for mutate())
  ref_em_BY.quo <- rlang::sym(ref_em_BY) # symbol
  gdp_BY.quo <- rlang::sym(gdp_BY)
  
  # set up two working df: parameter data frame and results data frame
  par_df <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, ref_em_BY)
  res_df <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, ref_em_BY)
  
  # equation (1)
  par_df <- wide_df %>% 
    mutate(EIRCY = reg_iam_em_Xcon_year / reg_gdp_Xcon_year,
           EICBY = !!ref_em_BY.quo / !!gdp_BY.quo)
  
  # need to replace countries' sectors that have zero emissions intensity growth 
  # in BY with a non-zero value
  
  # first, grab all sectors with non-zero baseyear emissions intensity growth
  nonzero_in_BY <- par_df %>%
    filter(EICBY != 0 )
  # then, calculate min(that region's set of sectoral emissions intensity growth) / 3
  replacement_values <- nonzero_in_BY %>%
    group_by(region, em, unit) %>%
    summarise(replacement_value = min(ctry_ref_em_X2015)/3) %>%
    ungroup()
  # then, replace zero-valued baseyears with the value calculated above
  # the zero-valued iso sectors are replaced with the above calculated values
  zero_in_BY <- par_df %>%
    filter(EICBY == 0 ) %>%
    select(-EICBY) %>%
    left_join(replacement_values) %>% 
    dplyr::rename(EICBY = replacement_value)
  # bind zero and non-zero baseyear emissions intensity growth rows together
  par_df <- rbind(nonzero_in_BY, zero_in_BY)
  
  # return truncated zero_in_BY for diagnostic reporting
  zero_in_BY <- zero_in_BY %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, harm_status, unit)

  # loop over all ssp's in data
  out_df_list <- lapply( unique( wide_df$ssp_label ), function( ssp ) { 
    
    # filter df's to correct ssp
    par_df_ssp <- par_df %>% filter(ssp_label == ssp)
    res_df_ssp <- res_df %>% filter(ssp_label == ssp)
    wide_df_ssp <- wide_df %>% filter(ssp_label == ssp)
    con_year <- con_year_mapping %>% filter(scenario_label == ssp) %>% select(convergence_year) %>% as.numeric()
    
    # equation (2)
    if(pos_nonCO2) { # only positive & non-co2 emissions
      par_df_ssp <- par_df_ssp %>% 
        mutate(EI_gr_C = ( EIRCY / EICBY ) ^ ( 1 / ( con_year - base_year ) ),
               EI_gr_C = ifelse( is.na( EI_gr_C ), 0, EI_gr_C ),
               EI_gr_C = ifelse( is.infinite( EI_gr_C ), 0, EI_gr_C ) )
    
     
    } else { # either CO2 or zero/negative
      par_df_ssp <- par_df_ssp %>% 
        mutate(EI_gr_C = ( ( EIRCY - EICBY ) / EICBY ) / ( con_year - base_year ) ,
               EI_gr_C = ifelse( is.na( EI_gr_C ), 0, EI_gr_C ),
               EI_gr_C = ifelse( is.infinite( EI_gr_C ), 0, EI_gr_C ) )
    }
    
    # apply rest of calculation to each timestep 
    for ( year in ( base_year + 1 ) : ds_end_year ) { 
      
      # equation (3)
      if(pos_nonCO2) { # only positive & non-co2 emissions
      
        par_df_ssp$EI_star <- ( res_df_ssp[ , paste0( 'ctry_ref_em_X', (year - 1) )] / 
                                  wide_df_ssp[ , paste0( 'ctry_gdp_X', (year - 1) )] ) * 
                    par_df_ssp$EI_gr_C
        
      } else { # either CO2 or zero/negative 
        
        par_df_ssp$EI_star <- ( res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / 
                                  wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ] ) + 
          ( res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / 
              wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ] ) * 
          par_df_ssp$EI_gr_C
        
      }
      
      # equation (4)
      par_df_ssp$E_star <- par_df_ssp$EI_star * wide_df_ssp[ , paste0( 'ctry_gdp_X', year ) ]
      
      # calculate 2nd term of equation (5)
      temp_df_agg <- aggregate( par_df_ssp$E_star, 
                                by = list( wide_df_ssp$region, wide_df_ssp$ssp_label,
                                           wide_df_ssp$em, wide_df_ssp$sector, wide_df_ssp$scenario ), 
                                FUN=sum, na.rm = T )
      colnames( temp_df_agg ) <- c( 'region', 'ssp_label', 'em', 'sector', 'scenario', 'sum_E_star' )
      
      par_df_ssp$sum_E_star <- temp_df_agg[ match( paste( par_df_ssp$region, par_df_ssp$ssp_label, par_df_ssp$em, par_df_ssp$sector, par_df_ssp$scenario ), 
                                                   paste( temp_df_agg$region, temp_df_agg$ssp_label, temp_df_agg$em, temp_df_agg$sector, temp_df_agg$scenario ) ), 
                                            'sum_E_star' ]
      
      # equation (5)
      par_df_ssp$DiffR <- wide_df_ssp[ , paste0( 'reg_iam_em_X', year ) ]  - par_df_ssp$sum_E_star
      
      # equation (6)
      par_df_ssp <- par_df_ssp %>% 
        mutate(E_share = E_star / sum_E_star,
               E_share = ifelse( is.na( E_share ), 0, E_share ),
               E_share = ifelse( is.infinite( E_share ), 0, E_share ) )
      
      # equation (7)    
      par_df_ssp <- par_df_ssp %>% 
        mutate(E_final = E_star + DiffR * E_share, 
               E_final = ifelse( is.na( E_final ), 0, E_final ),
               E_final = ifelse( is.infinite( E_final ), 0, E_final ) )
      
      # drop final emissions result into results df
      res_df_ssp[ , paste0( "ctry_ref_em_X", year ) ] <- par_df_ssp$E_final
      
    }
    
    out_df_ssp <- res_df_ssp[ , c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', 
                                   paste0( 'ctry_ref_em_X', base_year : ds_end_year ) ) ]  
    colnames( out_df_ssp )  <- c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', paste0( 'X', base_year : ds_end_year ) )   
    
    return( out_df_ssp )
  } )
  
  out_df <- do.call( 'rbind', out_df_list )
  
  return( list(out_df, zero_in_BY ) )
}
# downscaleIAMemissions_pos_nonCO2 ----------------------------------------
downscaleIAMemissions_pos_nonCO2 <- function( wide_df, con_year_mapping ) { 
  
  # set up two working df: parameter data frame and results data frame
  par_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit" ) ] 
  res_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", paste0( 'ctry_ref_em_X', base_year ) ) ] 
  
  # equation (1)
  par_df$EIRCY <- wide_df[ , 'reg_iam_em_Xcon_year' ] / wide_df[ , 'reg_gdp_Xcon_year' ]
  par_df$EICBY <- wide_df[ , paste0( 'ctry_ref_em_X', base_year ) ] / wide_df[ , paste0( 'ctry_gdp_X', base_year ) ]
  
  out_df_list <- lapply( unique( wide_df$ssp_label ), function( ssp ) { 
    par_df_ssp <- par_df[ par_df$ssp_label == ssp, ]
    res_df_ssp <- res_df[ res_df$ssp_label == ssp, ]
    wide_df_ssp <- wide_df[ wide_df$ssp_label == ssp, ]
    
    con_year <- con_year_mapping[ con_year_mapping$scenario_label == ssp, "convergence_year" ]
    
    # equation (2)
    par_df_ssp$EI_gr_C <- ( par_df_ssp$EIRCY / par_df_ssp$EICBY )^( 1 / ( con_year - base_year ) )   
    par_df_ssp$EI_gr_C <- ifelse( is.na( par_df_ssp$EI_gr_C ), 0, par_df_ssp$EI_gr_C  )
    par_df_ssp$EI_gr_C <- ifelse( is.infinite( par_df_ssp$EI_gr_C ), 0, par_df_ssp$EI_gr_C  )
    
    for ( year in ( base_year + 1 ) : ds_end_year ) { 
      
      # equation (2)
      par_df_ssp$EI_star <- res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / 
        wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ] *
        par_df_ssp$EI_gr_C
      # equation (4)
      par_df_ssp$E_star <- par_df_ssp$EI_star * wide_df_ssp[ , paste0( 'ctry_gdp_X', year ) ]
      
      # equation(3)
      temp_df_agg <- aggregate( par_df_ssp$E_star, 
                                by = list( wide_df_ssp$region, wide_df_ssp$ssp_label,
                                           wide_df_ssp$em, wide_df_ssp$sector, wide_df_ssp$scenario ), 
                                FUN=sum, na.rm = T )
      colnames( temp_df_agg ) <- c( 'region', 'ssp_label', 'em', 'sector', 'scenario', 'sum_E_star' )
      
      par_df_ssp$sum_E_star <- temp_df_agg[ match( paste( par_df_ssp$region, par_df_ssp$ssp_label, par_df_ssp$em, par_df_ssp$sector, par_df_ssp$scenario ), 
                                                   paste( temp_df_agg$region, temp_df_agg$ssp_label, temp_df_agg$em, temp_df_agg$sector, temp_df_agg$scenario ) ), 
                                            'sum_E_star' ]
      par_df_ssp$DiffR <- wide_df_ssp[ , paste0( 'reg_iam_em_X', year ) ]  - par_df_ssp$sum_E_star
      
      # equation (5)
      par_df_ssp$E_share <- par_df_ssp$E_star / par_df_ssp$sum_E_star 
      par_df_ssp$E_share <- ifelse( is.na( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      par_df_ssp$E_share <- ifelse( is.infinite( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      
      # equation (6)    
      par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$DiffR * par_df_ssp$E_share 
      #par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$E_star * par_df_ssp$DiffR * par_df_ssp$E_share 
      par_df_ssp$E_final <- ifelse( is.na( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      par_df_ssp$E_final <- ifelse( is.infinite( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      res_df_ssp[ , paste0( "ctry_ref_em_X", year ) ] <- par_df_ssp$E_final
      
    }
    
    out_df_ssp <- res_df_ssp[ , c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', 
                                   paste0( 'ctry_ref_em_X', base_year : ds_end_year ) ) ]  
    colnames( out_df_ssp )  <- c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', paste0( 'X', base_year : ds_end_year ) )   
    
    return( out_df_ssp )
  } )
  
  out_df <- do.call( 'rbind', out_df_list )
  
  return( out_df )
}

# downscaleIAMemissions_CO2_or_neg_nonCO2 ---------------------------------
downscaleIAMemissions_CO2_or_neg_nonCO2 <- function( wide_df, con_year_mapping ) { 
  
  # set up two working df: parameter data frame and results data frame
  par_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit" ) ] 
  res_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", paste0( 'ctry_ref_em_X', base_year ) ) ] 
  
  # equation (1)
  par_df$EIRCY <- wide_df[ , 'reg_iam_em_Xcon_year' ] / wide_df[ , 'reg_gdp_Xcon_year' ]
  par_df$EICBY <- wide_df[ , paste0( 'ctry_ref_em_X', base_year ) ] / wide_df[ , paste0( 'ctry_gdp_X', base_year ) ]
  
  out_df_list <- lapply( unique( wide_df$ssp_label ), function( ssp ) { 
    par_df_ssp <- par_df[ par_df$ssp_label == ssp, ]
    res_df_ssp <- res_df[ res_df$ssp_label == ssp, ]
    wide_df_ssp <- wide_df[ wide_df$ssp_label == ssp, ]
    
    con_year <- con_year_mapping[ con_year_mapping$scenario_label == ssp, "convergence_year" ]
    
    par_df_ssp$EI_gr_C <- ( ( par_df_ssp$EIRCY - par_df_ssp$EICBY ) / par_df_ssp$EICBY ) / ( con_year - base_year ) 
    par_df_ssp$EI_gr_C <- ifelse( is.na( par_df_ssp$EI_gr_C ), 0, par_df_ssp$EI_gr_C  )
    par_df_ssp$EI_gr_C <- ifelse( is.infinite( par_df_ssp$EI_gr_C ), 0, par_df_ssp$EI_gr_C  )
    
    for ( year in ( base_year + 1 ) : ds_end_year ) { 
      
      # equation (2)
      par_df_ssp$EI_star <- ( res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / 
                                wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ] ) +  
        ( res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / 
            wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ] ) * 
        par_df_ssp$EI_gr_C
      par_df_ssp$E_star <- par_df_ssp$EI_star * wide_df_ssp[ , paste0( 'ctry_gdp_X', year ) ]
      
      # equation(3)
      temp_df_agg <- aggregate( par_df_ssp$E_star, 
                                by = list( wide_df_ssp$region, wide_df_ssp$ssp_label,
                                           wide_df_ssp$em, wide_df_ssp$sector, wide_df_ssp$scenario ), 
                                FUN=sum, na.rm = T )
      colnames( temp_df_agg ) <- c( 'region', 'ssp_label', 'em', 'sector', 'scenario', 'sum_E_star' )
      
      par_df_ssp$sum_E_star <- temp_df_agg[ match( paste( par_df_ssp$region, par_df_ssp$ssp_label, par_df_ssp$em, par_df_ssp$sector, par_df_ssp$scenario ), 
                                                   paste( temp_df_agg$region, temp_df_agg$ssp_label, temp_df_agg$em, temp_df_agg$sector, temp_df_agg$scenario ) ), 
                                            'sum_E_star' ]
      par_df_ssp$DiffR <- wide_df_ssp[ , paste0( 'reg_iam_em_X', year ) ]  - par_df_ssp$sum_E_star
      
      # equation (5)
      par_df_ssp$E_share <- par_df_ssp$E_star / par_df_ssp$sum_E_star 
      par_df_ssp$E_share <- ifelse( is.na( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      par_df_ssp$E_share <- ifelse( is.infinite( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      
      # equation (6)    
      par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$DiffR * par_df_ssp$E_share 
      #par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$E_star * par_df_ssp$DiffR * par_df_ssp$E_share 
      par_df_ssp$E_final <- ifelse( is.na( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      par_df_ssp$E_final <- ifelse( is.infinite( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      res_df_ssp[ , paste0( "ctry_ref_em_X", year ) ] <- par_df_ssp$E_final
      
    }
    
    out_df_ssp <- res_df_ssp[ , c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', 
                                   paste0( 'ctry_ref_em_X', base_year : ds_end_year ) ) ]  
    colnames( out_df_ssp )  <- c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', paste0( 'X', base_year : ds_end_year ) )   
    
    return( out_df_ssp )
  } )
  
  out_df <- do.call( 'rbind', out_df_list )
  
  return( out_df )
}