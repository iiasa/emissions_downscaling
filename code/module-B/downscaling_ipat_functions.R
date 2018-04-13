

# downscaleIAMemissions ----------------------------------------
downscaleIAMemissions <- function( wide_df, con_year_mapping, pos_nonCO2) { 
  
  # construct column-names that are dependet on indicated base_year
  # strings (use for select())
  ctry_ref_em_Xbase_year <-  paste0('ctry_ref_em_X', base_year)
  ctry_gdp_Xbase_year <- paste0("ctry_gdp_X", base_year)
  
  # set up two working df: parameter data frame and results data frame
  par_df <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit)
  res_df <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, ref_em_BY)
  
  # equation (1)
  par_df <- par_df %>% 
    mutate(reg_iam_em_Xcon_year = wide_df[["reg_iam_em_Xcon_year"]],
           reg_gdp_Xcon_year = wide_df[["reg_gdp_Xcon_year"]],
           EIRCY = reg_iam_em_Xcon_year / reg_gdp_Xcon_year,
           !!ctry_ref_em_Xbase_year := wide_df[[ctry_ref_em_Xbase_year]],
           !!ctry_gdp_Xbase_year := wide_df[[ctry_gdp_Xbase_year]],
           EICBY =  ctry_ref_em_Xbase_year / ctry_gdp_Xbase_year )
  
  # need to replace countries' sectors that have zero emissions intensity growth in BY with a non-zero value
  par_df <- adjustEICBY(par_df)
  
  
  # loop over all ssp's in data
  out_df_list <- lapply( unique( wide_df$ssp_label ), function( ssp ) { 
    
    # filter df's to correct ssp
    par_df_ssp <- par_df %>% filter(ssp_label == ssp)
    res_df_ssp <- res_df %>% filter(ssp_label == ssp)
    wide_df_ssp <- wide_df %>% filter(ssp_label == ssp)
    con_year <- con_year_mapping %>% filter(scenario_label == ssp) %>% select(convergence_year) %>% as.numeric()
    
    # equation (2) (emissions intensity growth rate, country-level)
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
      
      # strings used to call columns of data
      
      # used to dynamically refer to last year's calculated downscaled emissions (res_df)
      ctry_ref_em_X_year_less1 <- paste0( 'ctry_ref_em_X', ( year - 1 ) ) 
      
      # used to convert between emissions and emissions intensity (wide_df)
      ctry_gdp_em_X_year_less1 <- paste0( 'ctry_gdp_X', ( year - 1 ) )
      ctry_gdp_em_X_year <- paste0( 'ctry_gdp_X', year )
      
      # equation (3) (this year's EI-value)
      # pos_nonCO2:: this year's EI = last year's EI * growth rate ( where growth rate > 1)
      # ! pos_nonCO2:: this year's EI = last year's EI + last year's EI * growth rate (where growth rate < 1)
      
      if(pos_nonCO2) { # only positive & non-co2 emissions
      
        if (year == base_year + 1 ) { # EI* = EIBY (modified base_year values)
          
          par_df_ssp <- par_df_ssp %>% 
            mutate(EI_star = EICBY * EI_gr_C) 
          
        } else { # all other timesteps calculate EI* dynamically from last year's E-value (downscaled emissions) & GDP-value
          
          par_df_ssp <- par_df_ssp %>% 
            mutate(EI_star = ( res_df_ssp[[ctry_ref_em_X_year_less1]] / wide_df_ssp[[ctry_gdp_em_X_year_less1]] ) * 
                     EI_gr_C )
        }

        
      } else { # either CO2 or zero/negative 
        
        if (year == base_year + 1) { # EI* = EIBY (modified base_year values)
          
          par_df_ssp <- par_df_ssp %>% 
            mutate(EI_star = EICBY + EICBY* EI_gr_C)
          
        } else { # all other timesteps calculate EI* dynamically from last year's E-value (downscaled emissions) & GDP-value
        
          par_df_ssp <- par_df_ssp %>% 
            mutate( EI_star = ( res_df_ssp[[ctry_ref_em_X_year_less1]] / wide_df_ssp[[ctry_gdp_em_X_year_less1]] ) + 
            ( res_df_ssp[[ctry_ref_em_X_year_less1]] / wide_df_ssp[[ctry_gdp_em_X_year_less1]] ) * EI_gr_C )
        }
        
      }
      
      # equation (4)
      # this year's emissions = this year's emissions intensity * this year's GDP
      par_df_ssp <- par_df_ssp %>% 
        mutate(E_star = EI_star * wide_df_ssp[[ctry_gdp_em_X_year]] )
      
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
  
  return( list(out_df, zero_in_BY.trunc ) )
}

adjustEICBY <- function(par_df) {
  # this methodology doesn't affect industrial sector
  industrial <- par_df %>% 
    filter(sector == "Industrial Sector")
  
  # replacement values can't be taken from countries in a region with zero emissions as well 
  zero_IAMreg_ref_em_BY <- par_df %>% 
    anti_join(., industrial) %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>% 
    summarise(IAMreg_ref_em = sum(ctry_ref_em_X2015)) %>% 
    ungroup() %>% 
    filter(IAMreg_ref_em == 0 ) %>% 
    select(-IAMreg_ref_em) %>% 
    inner_join(., par_df)  
  
  # the rows we're modifying are...
  zero_in_BY <- par_df %>%
    filter(EICBY == 0) %>% # zero-valued EICBY 
    anti_join(., industrial) %>% # not industrial sector
    anti_join(., zero_IAMreg_ref_em_BY) # not zero-sum @ IAM-region-level
  
  # replacement values are calculated from all remaining rows...
  nonzero_in_BY <- par_df %>% 
    anti_join(., industrial) %>% 
    anti_join(., zero_IAMreg_ref_em_BY) %>% 
    anti_join(., zero_in_BY)
  
  # by choosing min(that region's set of emissions intensity growth values for the same sector) / 3
  replacement_values <- nonzero_in_BY %>%
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>%
    summarise(replacement_value = min(EICBY)/3) %>%
    ungroup()
  
  # replace zero-valued baseyears with the value calculated above
  zero_in_BY.mod <- zero_in_BY %>% 
    select(-EICBY) %>%
    left_join(replacement_values) %>% 
    dplyr::rename(EICBY = replacement_value)
  
  # diagnostic output report truncated set of columns 
  zero_in_BY.trunc <- zero_in_BY %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit)
  
  # bind unmodified (industrial, zero_IAMreg_ref_em_BY, nonzero_in_BY) and modified (zero_in_BY.mod) rows together
  par_df.mod <- rbind(industrial, zero_IAMreg_ref_em_BY, nonzero_in_BY, zero_in_BY.mod)
  
  return(par_df.mod)
  
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