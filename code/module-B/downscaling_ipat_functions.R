

# downscaleIAMemissions ----------------------------------------
downscaleIAMemissions <- function( wide_df, con_year_mapping, pos_nonCO2) { 
  
  # set up two working df: parameter data frame and results data frame
  
  # when a parameter is needed for a step in the calculation, a separate df is 
  # subset from wide_df to isolate a specific year's GDP, or from res_df for 
  # the previous year's downscaled emissions. 
  # this new df is then left_joined to par_df so as to match the new parameter 
  # column to the existing data in par_df using the original par_df columns (region, iso, ssp_label, em, ...)
  # in this way, each time-step has every step of the calculation stored in par_df, but reports only
  # E_final in res_df.
  
  par_df <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit)
  res_df <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, paste0('ctry_ref_em_X', base_year))
  
  # equation (1) :
  # regional EI in convergence year (using IAM emissions)
  # country EI in base year (using REF emissions)
  EI_params <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, 
           reg_iam_em_Xcon_year, reg_gdp_Xcon_year, 
           paste0('ctry_ref_em_X', base_year), paste0("ctry_gdp_X", base_year))
  names(EI_params) <- c(names(EI_params)[1:10], "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year")
  
  par_df <- par_df %>% 
    left_join(EI_params) %>% 
    mutate(EIRCY = reg_iam_em_Xcon_year / reg_gdp_Xcon_year ,
           EICBY = ctry_ref_em_Xbase_year / ctry_gdp_Xbase_year )

  # need to replace countries' sectors that have zero emissions intensity growth in BY with a non-zero value
  out <- adjustEICBY(par_df)
  par_df <- out[[1]]
  zero_in_BY.trunc <- out[[2]]
  
  # loop over all ssp's in data
  out_df_list <- lapply( unique( wide_df$ssp_label ), function( ssp ) { 
    
    # filter df's to correct ssp
    par_df_ssp <- par_df %>% filter(ssp_label == ssp)
    res_df_ssp <- res_df %>% filter(ssp_label == ssp)
    wide_df_ssp <- wide_df %>% filter(ssp_label == ssp) 
    con_year <- con_year_mapping %>% filter(scenario_label == ssp) %>% select(convergence_year) %>% as.numeric()
    
    # equation (2) : 
    # country-level emissions intensity growth rate 
    # calculation depends if there are negative emissions in CY 
    
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
    
    # calculation of each year's downscaled country emissions
    for ( year in ( base_year + 1 ) : ds_end_year ) {
      
      # strings used to call columns of data:
      
      # used to dynamically refer to last year's calculated downscaled emissions (res_df)
      ctry_ref_em_X_year_less1 <- paste0( 'ctry_ref_em_X', ( year - 1 ) ) 
      
      # used to convert between emissions and emissions intensity (wide_df)
      ctry_gdp_X_year_less1 <- paste0( 'ctry_gdp_X', ( year - 1 ) )
      ctry_gdp_X_year <- paste0( 'ctry_gdp_X', year )
      
      # equation (3):
      # this year's EI ~ {last year's EI, EI growth rate}
      
      # identify/calculate last year's EI and drop into par_df_ssp
      if (year == base_year + 1) {
        # either use adjusted EICBY values when calculating first time step...
        par_df_ssp <- par_df_ssp %>% 
          mutate(EI_prev = EICBY)
      } else { 
        # or dynamically calculate last years EI-value from last year's GDP & downscaled emissions

        # last year's downscaled emissions
        ctry_ref_em_prev <- res_df_ssp %>% 
          select(region, iso, ssp_label, em, sector, model, scenario, unit,
                 ctry_ref_em_X_year_less1) 
        names(ctry_ref_em_prev) <- c(names(ctry_ref_em_prev)[1:8], "ctry_ref_em_prev")

        # last year's gdp
        ctry_gdp_prev <- wide_df_ssp %>% 
          select(region, iso, ssp_label, em, sector, model, scenario, unit,
                 ctry_gdp_X_year_less1)
        names(ctry_gdp_prev) <- c(names(ctry_gdp_prev)[1:8], "ctry_gdp_prev")

        # EI = E / GDP
        # drop *new* previous year's emissions & GDP into par_df_ssp
        if ( any( c("ctry_ref_em_prev", "ctry_gdp_prev") %in% names(par_df_ssp) ) ) {
          par_df_ssp <- par_df_ssp %>% 
            select(-matches("prev"))
        }      
        par_df_ssp <- par_df_ssp %>%
          left_join(ctry_ref_em_prev) %>% 
          left_join(ctry_gdp_prev) %>% 
          mutate( EI_prev = ctry_ref_em_prev / ctry_gdp_prev )
      }
      
      # just like EI growth rate, calculating this year's EI depends on 
      # regional (IAM) emissions in covnergence year
      if(pos_nonCO2) { # only positive & non-co2 emissions
      
        par_df_ssp <- par_df_ssp %>% 
          mutate(EI_star = EI_prev * EI_gr_C)
        
      } else { # either CO2 or zero/negative 
        
        par_df_ssp <- par_df_ssp %>% 
          mutate(EI_star = EI_prev + abs(EI_prev) * EI_gr_C)
        
      }
      
      # EI_star pathways for zero_in_BY rows is bounded by minimum EI_star in nonzero_in_BY rows
      par_df_ssp <- adjustEI_star(par_df_ssp, zero_in_BY.trunc)
      
      # equation (4) : 
      # this year's preliminary emissions = this year's emissions intensity * this year's GDP
      
      # drop this year's GDP into par_df_ssp
      gdp <- wide_df_ssp %>% 
        select(region, iso, ssp_label, em, sector, model, scenario, unit, 
               ctry_gdp_X_year) 
      names(gdp) <- c(names(gdp)[1:8], "ctry_gdp_X_year")

      # E = EI * GDP
      # drop *this* year's GDP into par_df_ssp
      if ("ctry_gdp_X_year" %in% names(par_df_ssp) ) {
        par_df_ssp <- par_df_ssp %>% 
          select(-ctry_gdp_X_year)
      }   
      par_df_ssp <- par_df_ssp %>% 
        left_join(gdp) %>% 
        mutate(E_star = EI_star * ctry_gdp_X_year)

      # 2nd term of equation (5):
      # calculate regional emissions from this time step's preliminary country emissions
      temp_df_agg <- par_df_ssp %>%
        group_by(region, ssp_label, em, sector, scenario) %>%
        summarise(sum_E_star = sum(E_star, na.rm=T)) %>%
        ungroup()
      
      # drop *this year's* calculated regional emissions into par_df_ssp
      if ("sum_E_star" %in% names(par_df_ssp) ) {
        par_df_ssp <- par_df_ssp %>% 
          select(-sum_E_star)
      }      
      par_df_ssp <- par_df_ssp %>%
        left_join(temp_df_agg, by=c("region", "ssp_label", "em", "sector", "scenario"))
      
      
      # equation (5):
      # difference between IAM regional emissions and calculated regional emissions
      reg_IAM_em <- wide_df_ssp %>% 
        select( region, iso, ssp_label, em, sector, model, scenario, paste0('reg_iam_em_X', year) )
      names(reg_IAM_em) <- c(names(reg_IAM_em)[1:7], "regional_IAM_Emissions")
      
      # drop *this year's* regional IAM emissions into par_df_ssp
      if ("regional_IAM_Emissions" %in% names(par_df_ssp) ) {
        par_df_ssp <- par_df_ssp %>% 
          select(-regional_IAM_Emissions)
      }
      
      # DiffR = IAM_Regional_Emissions - REF_Regional_Emissions
      par_df_ssp <- par_df_ssp %>% 
        left_join(reg_IAM_em, by=names(reg_IAM_em)[1:7] ) %>% 
        mutate(DiffR = regional_IAM_Emissions  - sum_E_star)
      
      # equation (6):
      # each country's share of calculated regional emissions
      # (used to portion out DiffR)
      par_df_ssp <- par_df_ssp %>% 
        mutate(E_share = E_star / sum_E_star,
               E_share = ifelse( is.na( E_share ), 0, E_share ),
               E_share = ifelse( is.infinite( E_share ), 0, E_share ) )
      
      # equation (7):
      # final adjustment to preliminary country emissions 
      # using portion of DiffR = IAM_Regional_Emissinos  REF_Regional_Emissions
      par_df_ssp <- par_df_ssp %>% 
        mutate(E_adj = DiffR * E_share,
               E_final = E_star + E_adj, 
               E_final = ifelse( is.na( E_final ), 0, E_final ),
               E_final = ifelse( is.infinite( E_final ), 0, E_final ) )
      
      # save calculation dataframe
      if (year %in% c(2016:2020)) {
        saveCalculation(par_df_ssp, "branch", year, pos_nonCO2)
      }
      
      # drop final emissions result into results df
      df <- par_df_ssp %>% 
        select(region, iso, ssp_label, em, sector, model, scenario, unit, E_final)
      res_df_ssp <- res_df_ssp %>% 
        left_join(df, by=c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit"))
      
      # format name of new final emissions data column
      names(res_df_ssp) <- c(names(res_df_ssp)[1:ncol(res_df_ssp)-1], 
                             paste0( "ctry_ref_em_X", year ))
    }
    
    out_df_ssp <- res_df_ssp[ , c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', 
                                   paste0( 'ctry_ref_em_X', base_year : ds_end_year ) ) ]  
    colnames( out_df_ssp )  <- c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', paste0( 'X', base_year : ds_end_year ) )   
    
    return( out_df_ssp )
  } )
  
  out_df <- do.call( 'rbind', out_df_list )
  
  return( list(out_df, zero_in_BY.trunc ) )
}

saveCalculation <- function(par_df_ssp, file, year, pos_nonCO2) {
  fn <- paste0("C:/users/guti220/desktop/random files/ds_calculation/", file)
  
  # append to saved calculation file on 2nd ipat downscaling routine
  if (pos_nonCO2) {
    app <- F
  } else {
    app <- T
  }
  col <- ! app
  
  
  par_df_ssp %>% 
    select(region, iso, em, sector, model, scenario, 
           matches("em_Xcon"), matches("gdp_Xcon"), matches("EIRCY"), 
           matches("em_Xbase"), matches("gdp_Xbase"), matches("EICBY"), matches("EI_gr_C"),
           matches("ref_em_prev"), matches("gdp_prev"), matches("EI_prev"), matches("EI_star"), 
           matches("gdp_X_year"), matches("^E_star"),
           matches("sum_E_star"), matches("regional_IAM_Emissions"), matches("DiffR"), 
           matches("E_share"), matches("E_adj"), matches("E_final")) %>% 
    arrange(ctry_ref_em_Xbase_year) %>% 
    write.table(paste(fn, year, "csv", sep="."), append=app, col.names=col, row.names=F, sep=",")
  
  # check top CO2 emitters from zero_in_BY diagnostic file
  par_df_ssp %>% 
    select(region, iso, em, sector, model, scenario, 
           matches("em_Xcon"), matches("gdp_Xcon"), matches("EIRCY"), 
           matches("em_Xbase"), matches("gdp_Xbase"), matches("EICBY"), matches("EI_gr_C"),
           matches("ref_em_prev"), matches("gdp_prev"), matches("EI_prev"), matches("EI_star"), 
           matches("gdp_X_year"), matches("^E_star"),
           matches("sum_E_star"), matches("regional_IAM_Emissions"), matches("DiffR"), 
           matches("E_share"), matches("E_adj"), matches("E_final")) %>% 
    filter(region == "AFR" & em == "CO2" & sector == "Energy Sector") %>% 
    arrange(ctry_ref_em_Xbase_year) %>% 
    write.table(paste(fn, 3, year, "csv", sep="."), row.names=F, sep=",")
}

adjustEICBY <- function(par_df) {
  # this methodology doesn't affect industrial sector
  industrial <- par_df %>% 
    filter(sector == "Industrial Sector")
  
  # replacement values can't be taken from countries in a region with zero emissions as well 
  zero_IAMreg_ref_em_BY <- par_df %>% 
    anti_join(., industrial) %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>% 
    summarise(reg_ref_em_Xbase_year = sum(ctry_ref_em_Xbase_year)) %>% 
    ungroup() %>% 
    filter(reg_ref_em_Xbase_year == 0 ) %>% 
    select(-reg_ref_em_Xbase_year) %>% 
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
  
  return(list(par_df.mod, zero_in_BY.trunc))
  
}

adjustEI_star <- function(par_df, zero_in_BY.trunc) {
  nonzero_in_BY <- par_df %>% 
    anti_join(zero_in_BY.trunc)
  
  replacement_values <- nonzero_in_BY %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>%
    summarise(replacement_value = min(abs(EI_star))) %>% 
    ungroup()
  
  zero_in_BY.mod <- par_df %>% 
    inner_join(zero_in_BY.trunc) %>% 
    left_join(replacement_values) %>% 
    mutate(EI_star = ifelse(abs(EI_star) > abs(replacement_value), replacement_value, EI_star)) %>% 
    select(-replacement_value)
  
  return(rbind(nonzero_in_BY, zero_in_BY.mod))
  
}

# downscaleIAMemissions_pos_nonCO2 ----------------------------------------
downscaleIAMemissions_pos_nonCO2 <- function( wide_df, con_year_mapping ) { 
  
  # set up two working df: parameter data frame and results data frame
  par_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", paste0( 'ctry_ref_em_X', base_year ) ) ] 
  res_df <- wide_df[ , c( "region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", paste0( 'ctry_ref_em_X', base_year ) ) ] 
  
  # equation (1)
  par_df$reg_iam_em_Xcon_year <- wide_df[ , 'reg_iam_em_Xcon_year' ]
  par_df$reg_gdp_Xcon_year <- wide_df[ , 'reg_gdp_Xcon_year' ]
  par_df$EIRCY <- par_df$reg_iam_em_Xcon_year / par_df$reg_gdp_Xcon_year
  
  par_df$ctry_ref_em_Xbase_year <- wide_df[ , paste0( 'ctry_ref_em_X', base_year ) ]
  par_df$ctry_gdp_Xbase_year <- wide_df[ , paste0( 'ctry_gdp_X', base_year ) ]
  par_df$EICBY <- par_df$ctry_ref_em_Xbase_year / par_df$ctry_gdp_Xbase_year
  
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
      par_df_ssp$EI_prev <- res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ]
      par_df_ssp$EI_star <-  par_df_ssp$EI_prev * par_df_ssp$EI_gr_C
      
      # equation (4)
      par_df_ssp$ctry_gdp_X_year <- wide_df_ssp[ , paste0( 'ctry_gdp_X', year ) ]
      par_df_ssp$E_star <- par_df_ssp$EI_star * par_df_ssp$ctry_gdp_X_year
      
      # equation(3)
      temp_df_agg <- aggregate( par_df_ssp$E_star, 
                                by = list( wide_df_ssp$region, wide_df_ssp$ssp_label,
                                           wide_df_ssp$em, wide_df_ssp$sector, wide_df_ssp$scenario ), 
                                FUN=sum, na.rm = T )
      colnames( temp_df_agg ) <- c( 'region', 'ssp_label', 'em', 'sector', 'scenario', 'sum_E_star' )
      
      par_df_ssp$sum_E_star <- temp_df_agg[ match( paste( par_df_ssp$region, par_df_ssp$ssp_label, par_df_ssp$em, par_df_ssp$sector, par_df_ssp$scenario ), 
                                                   paste( temp_df_agg$region, temp_df_agg$ssp_label, temp_df_agg$em, temp_df_agg$sector, temp_df_agg$scenario ) ), 
                                            'sum_E_star' ]
      par_df_ssp$regional_IAM_Emissions <- wide_df_ssp[ , paste0( 'reg_iam_em_X', year ) ]
      par_df_ssp$DiffR <- par_df_ssp$regional_IAM_Emissions - par_df_ssp$sum_E_star
      
      # equation (5)
      par_df_ssp$E_share <- par_df_ssp$E_star / par_df_ssp$sum_E_star 
      par_df_ssp$E_share <- ifelse( is.na( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      par_df_ssp$E_share <- ifelse( is.infinite( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      
      # equation (6)    
      par_df_ssp$E_adj <- par_df_ssp$DiffR * par_df_ssp$E_share 
      par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$E_adj
      #par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$E_star * par_df_ssp$DiffR * par_df_ssp$E_share 
      par_df_ssp$E_final <- ifelse( is.na( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      par_df_ssp$E_final <- ifelse( is.infinite( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      res_df_ssp[ , paste0( "ctry_ref_em_X", year ) ] <- par_df_ssp$E_final
      
      # save calculation dataframe
      if (year %in% c(2016:2020)) {
        saveCalculation(par_df_ssp, "master", 2, year, pos_nonCO2=TRUE)
      }
      
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
  par_df$reg_iam_em_Xcon_year <- wide_df[ , 'reg_iam_em_Xcon_year' ]
  par_df$reg_gdp_Xcon_year <- wide_df[ , 'reg_gdp_Xcon_year' ]
  par_df$EIRCY <- par_df$reg_iam_em_Xcon_year / par_df$reg_gdp_Xcon_year
  
  par_df$ctry_ref_em_Xbase_year <- wide_df[ , paste0( 'ctry_ref_em_X', base_year ) ]
  par_df$ctry_gdp_Xbase_year <- wide_df[ , paste0( 'ctry_gdp_X', base_year ) ]
  par_df$EICBY <- par_df$ctry_ref_em_Xbase_year / par_df$ctry_gdp_Xbase_year
  
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
      par_df_ssp$EI_prev <- res_df_ssp[ , paste0( 'ctry_ref_em_X', ( year - 1 ) ) ] / wide_df_ssp[ , paste0( 'ctry_gdp_X', ( year - 1 ) ) ]
      par_df_ssp$EI_star <- par_df_ssp$EI_prev + par_df_ssp$EI_prev * par_df_ssp$EI_gr_C
      
      par_df_ssp$ctry_gdp_X_year <- wide_df_ssp[ , paste0( 'ctry_gdp_X', year ) ]
      par_df_ssp$E_star <- par_df_ssp$EI_star * par_df_ssp$ctry_gdp_X_year

      # equation(3)
      temp_df_agg <- aggregate( par_df_ssp$E_star, 
                                by = list( wide_df_ssp$region, wide_df_ssp$ssp_label,
                                           wide_df_ssp$em, wide_df_ssp$sector, wide_df_ssp$scenario ), 
                                FUN=sum, na.rm = T )
      colnames( temp_df_agg ) <- c( 'region', 'ssp_label', 'em', 'sector', 'scenario', 'sum_E_star' )
      
      par_df_ssp$sum_E_star <- temp_df_agg[ match( paste( par_df_ssp$region, par_df_ssp$ssp_label, par_df_ssp$em, par_df_ssp$sector, par_df_ssp$scenario ), 
                                                   paste( temp_df_agg$region, temp_df_agg$ssp_label, temp_df_agg$em, temp_df_agg$sector, temp_df_agg$scenario ) ), 
                                            'sum_E_star' ]
      par_df_ssp$regional_IAM_Emissions <- wide_df_ssp[ , paste0( 'reg_iam_em_X', year ) ]
      par_df_ssp$DiffR <- par_df_ssp$regional_IAM_Emissions - par_df_ssp$sum_E_star
      
      # equation (5)
      par_df_ssp$E_share <- par_df_ssp$E_star / par_df_ssp$sum_E_star 
      par_df_ssp$E_share <- ifelse( is.na( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      par_df_ssp$E_share <- ifelse( is.infinite( par_df_ssp$E_share ), 0, par_df_ssp$E_share  )
      
      # equation (6)    
      par_df_ssp$E_adj <- par_df_ssp$DiffR * par_df_ssp$E_share 
      par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$E_adj
      #par_df_ssp$E_final <- par_df_ssp$E_star + par_df_ssp$E_star * par_df_ssp$DiffR * par_df_ssp$E_share 
      par_df_ssp$E_final <- ifelse( is.na( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      par_df_ssp$E_final <- ifelse( is.infinite( par_df_ssp$E_final ), 0, par_df_ssp$E_final )
      res_df_ssp[ , paste0( "ctry_ref_em_X", year ) ] <- par_df_ssp$E_final
      
      # save calculation dataframe
      if (year %in% c(2016:2020)) {
        saveCalculation(par_df_ssp, "master", 2, year, pos_nonCO2=FALSE)
      }
      
    }
    
    out_df_ssp <- res_df_ssp[ , c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', 
                                   paste0( 'ctry_ref_em_X', base_year : ds_end_year ) ) ]  
    colnames( out_df_ssp )  <- c( 'region', 'iso', 'em', 'sector', 'model', 'scenario', 'unit', paste0( 'X', base_year : ds_end_year ) )   
    
    return( out_df_ssp )
  } )
  
  out_df <- do.call( 'rbind', out_df_list )
  
  return( out_df )
}