

# downscaleIAMemissions ----------------------------------------
downscaleIAMemissions <- function( wide_df, con_year_mapping, CO2_or_negCY, twoGrowthRates = FALSE) { 
  
  # set up two working df: parameter data frame and results data frame
  
  # when a parameter is needed for a step in the calculation, a separate df is 
  # subset from wide_df to isolate a specific year's GDP, or from res_df for 
  # the previous year's downscaled emissions. 
  # this new df is then left_joined to par_df so as to match the new parameter 
  # column to the existing data in par_df using the original par_df columns (region, iso, ssp_label, em, ...)
  # in this way, each time-step has every step of the calculation stored in par_df, but reports only
  # E_final in res_df.
  
  par_df <- wide_df %>% 
    select( region, iso, ssp_label, em, sector, model, scenario, unit ) %>% 
    mutate( base_year = base_year %>% as.numeric() ) %>% 
    left_join( con_year_mapping, by=c("ssp_label" = "scenario_label") ) %>% 
    dplyr::rename( con_year = convergence_year ) 
  
  res_df <- wide_df %>% 
    select( region, iso, ssp_label, em, sector, model, scenario, unit, paste0('ctry_ref_em_X', base_year) )
  
  if (CO2_or_negCY & twoGrowthRates) {
    # adds 'numGrowthRates' column which identifies whether emissions 
    # (1) peak in BY or CY -> 1 growth rate
    # (2) never peak -> "0"(-valued) growth rate
    # (3) peak in some other year -> 2 growth rates
    # also adds 'peak_year' column to par_df (NA if numGrowthRates==0)
    par_df <- identifyNumGrowthRates( wide_df, con_year_mapping, par_df )
    
    # Calculate EI for CY, BY, and PY (if numGrowthRates==2)
    par_df <- equation1a( wide_df, con_year_mapping, par_df )
  } else {
    # Calculate EI for CY, BY
    par_df <- equation1( wide_df, con_year_mapping, par_df )
    
  }
  
  # replace zero-valued emissions intensity in BY with 1/3 regional minimum
  out <- adjustEICBY( par_df, CO2_or_negCY , twoGrowthRates)
  par_df <- out[[1]]
  zero_in_BY.trunc <- out[[2]] # record which rows required adjustment
  
  # loop over all ssp's in data
  out_df_list <- lapply( unique( wide_df$ssp_label ), function( ssp ) { 
    
    # filter df's to correct ssp
    par_df_ssp <- par_df %>% filter( ssp_label == ssp )
    res_df_ssp <- res_df %>% filter( ssp_label == ssp )
    wide_df_ssp <- wide_df %>% filter( ssp_label == ssp ) 

    if (CO2_or_negCY & twoGrowthRates) {
      # calculate EI_gr_C_am
      par_df_ssp <- equation2a_EI_gr_C_am( par_df_ssp )
    } else {
      # calculate EI_gr_C
      par_df_ssp <- equation2( par_df_ssp )
    }
    
    base_year <- as.numeric( base_year )
    ds_end_year <- as.numeric( ds_end_year )
    # calculation of each year's downscaled country emissions
    for ( year in seq( base_year + 1, ds_end_year ) ) {
      
      if (CO2_or_negCY & twoGrowthRates) {
        
        # before applying growth rate, we must identify whether we are using EI_gr_C_am or EI_gr_C_pm
        par_df_ssp <- identify_EI_gr_C( par_df_ssp, year )
        
        # calculate this year's preliminary emissions intensity from last year's emissions & GDP
        par_df_ssp <- equation3a( wide_df_ssp, res_df_ssp, par_df_ssp, year ) 
      } else {
          
        # calculate this year's preliminary emissions intensity from last year's emissions & GDP
        par_df_ssp <- equation3( wide_df_ssp, res_df_ssp, par_df_ssp, year )
      }
            
      # EI_star pathways for zero_in_BY rows is bounded by minimum EI_star in nonzero_in_BY rows
      par_df_ssp <- adjustEI_star( par_df_ssp, zero_in_BY.trunc )
      
      # this year's preliminary emissions = this year's emissions intensity * this year's GDP
      par_df_ssp <- equation4( wide_df_ssp, par_df_ssp, year ) 
      
      # scale preliminary emissions to match this year's regional IAM emissions
      par_df_ssp <- equation5( wide_df_ssp, par_df_ssp, year, res_df_ssp )
      
      # calculate each country's share of preliminary, calculated regional emissions
      par_df_ssp <- equation6( par_df_ssp )
      
      # rescale preliminary downscaled emissions so that they sum to IAM regional emissions
      par_df_ssp <- equation7( par_df_ssp )
      
      if ( debug ) {
        
        # # output calculation parameters data.frame
        # if ( year %in% calculationYears ) {
        #   saveCalculation( par_df_ssp, year, calculationYears, calculationDir )
        # }
        # 
        # # save_parameter( par_df_ssp, year, DiffR, calculationDir )
        # save_parameter( par_df_ssp, year, EI_star, calculationDir )
        
      }
      
      if (CO2_or_negCY & twoGrowthRates) {
        # calculate EI_gr_C_pm (updates column if numGrowthRates = 2 & year == peak_year)
        par_df_ssp <- equation2a_EI_gr_C_pm( par_df_ssp, year )
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

# adds 'numGrowthRates' column which identifies whether emissions 
# (1) peak in BY or CY -> 1 growth rate
# (2) never peak -> "0"(-valued) growth rate
# (3) peak in some other year -> 2 growth rates
# also adds 'peak_year' column to par_df (NA if numGrowthRates %in% c(0,1))
identifyNumGrowthRates <- function(wide_df, con_year_mapping, par_df) {
  
  # grab regional emissions (excluding CY)
  reg_iam_em.wide <- wide_df %>% 
    select(region, ssp_label, em, sector, model, scenario, unit, matches("reg_iam_em")) %>% 
    distinct()
  names(reg_iam_em.wide) <- gsub("reg_iam_em_X", "", names(reg_iam_em.wide))
  
  # melt to long format
  reg_iam_em <- reg_iam_em.wide %>% 
    gather(key=year, value=value, -region, -ssp_label, -em, -sector, -model, -scenario, -unit) %>% 
    dplyr::rename(E=value) %>% 
    left_join(con_year_mapping, by=c("ssp_label" = "scenario_label")) %>% 
    dplyr::rename(con_year = convergence_year) %>% 
    mutate(year = ifelse(year == "con_year", con_year, year)) %>%
    select(-con_year) %>% 
    mutate(year = as.numeric(year))
  
  # identify peaking behavior in each series
  
  # peaks in BY or CY -> numGrowthRates = 1
  monotonic <- reg_iam_em %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>% 
    filter(length(unique(E)) != 1) %>% # non-zero slope
    filter(E==max(E)) %>%
    ungroup() %>% 
    filter(year == base_year | year >= 2100) %>% 
    mutate(numGrowthRates=1)
  
  # flat emissions (can be zero, or non-zero as a result of harmonization procedure) -> numGrowthRates = 0
  flat <- reg_iam_em %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>% 
    filter(length(unique(E)) == 1) %>% # zero slope
    ungroup() %>% 
    mutate(numGrowthRates=0)
  
  # peaks in a single year that isn't BY or CY > numGrowthRates = 2
  peaks <- reg_iam_em %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>% 
    filter(E==max(E)) %>%
    ungroup() %>% 
    anti_join(monotonic, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
    anti_join(flat, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
    mutate(numGrowthRates=2) 
  
  # data.frame that holds "numGrowthRates" and "peak_year" parameters
  # flat contains entire emissions time-series, so must collapse by storing 'peak_year' as N/A and grabbing distinct columns
  # After doing this, the data.frame is uniquely identified by base set of columns (see initialization of par_df) 
  PY <- monotonic %>% 
    full_join(flat, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit", "year", "E", "numGrowthRates")) %>% 
    full_join(peaks, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit", "year", "E", "numGrowthRates")) %>% 
    mutate(peak_year = ifelse(numGrowthRates %in% c(1,2), year, NA)) %>% 
    select(-year) %>% 
    distinct() %>% 
    select(-E)
    
  df <- par_df %>% 
    left_join(PY, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit"))
}

# Calculate EICBY & EIRCY (all cases), & EIRPY if numGrowthRates = 2
equation1a <- function(wide_df, con_year_mapping, par_df) {
  
  # grab E & GDP for both BY & CY
  EI_CYBY_params <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, 
           reg_iam_em_Xcon_year, reg_gdp_Xcon_year, 
           paste0('ctry_ref_em_X', base_year), paste0("ctry_gdp_X", base_year))
  names(EI_CYBY_params) <- c(names(EI_CYBY_params)[1:10], "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year")
  
  # attach E & GDP to par_df for both BY & CY
  par_df <- par_df %>% 
    left_join(EI_CYBY_params,  by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit"))
  
  # in order to attach E & GDP for PY, we must match on par_df's peak_year
  # This requires E & GDP for all years in long format
  
  # grab E for all years (long format)
  E.wide <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, matches("reg_iam_em_X"))
  names(E.wide) <- gsub("reg_iam_em_X", "", names(E.wide))
  E <- E.wide %>% 
    gather(key=year, value=E, -region, -iso, -ssp_label, -em, -sector, -model, -scenario, -unit) %>% 
    mutate(numGrowthRates=2) # used along with peak_year to match PY to correct rows
  
  # grab GDP for all years (long format)
  GDP.wide <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, matches("reg_gdp_X"))
  names(GDP.wide) <- gsub("reg_gdp_X", "", names(GDP.wide))
  GDP <- GDP.wide %>% 
    gather(key=year, value=GDP, -region, -iso, -ssp_label, -em, -sector, -model, -scenario, -unit) %>% 
    mutate(numGrowthRates=2) # used along with peak_year to match PY to correct rows
  
  # combine E & GDP for all years (long format)
  EI_PY_params <- inner_join(E, GDP, 
                             by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", "year", "numGrowthRates")) %>% 
    left_join(con_year_mapping, by=c("ssp_label" = "scenario_label")) %>% 
    dplyr::rename(con_year = convergence_year) %>% 
    mutate(year = ifelse(year == "con_year", con_year, year)) %>%
    select(-con_year) %>% 
    mutate(year = as.numeric(year))
    
  
  # attach E & GDP for PY (matches only if numGrowthRates = 2)
  par_df <- par_df %>% 
    left_join(EI_PY_params, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
                                   "peak_year"="year", "numGrowthRates")) %>% 
    dplyr::rename(reg_iam_em_Xpeak_year=E,
                  reg_gdp_Xpeak_year=GDP)
  
  
  # EI = E / GDP
  par_df <- par_df %>% 
    mutate( 
      # EICBY: country (CEDS) EI in base year (used for all numGrowthRates)
      EICBY = ctry_ref_em_Xbase_year / ctry_gdp_Xbase_year, 
      
      # EIRCY: regional (IAM) EI in convergence year (used for all numGrowthRates)
      # not used until after peak if numGrowthRates = 2
      EIRCY = reg_iam_em_Xcon_year / reg_gdp_Xcon_year,
      
      # EIRPY: regional (IAM) EI in peak year (used only if numGrowthRates = 2)
      EIRPY = ifelse(numGrowthRates == 2, 
                     reg_iam_em_Xpeak_year / reg_gdp_Xpeak_year, 
                     NA),
      
      # EICPY: country (downscaled) EI in peak_year (used only if numGrowthRates = 2)
      # this is calculated using the downscaled emissions intensity when year == peak_year
      # update_EI_gr_C() checks if this is true and calculates EICPY and the post-peak growth rate
      # when it is. 
      EICPY = NA)
}

# Calculate EICBY & EIRCY
equation1 <- function(wide_df, con_year_mapping, par_df) {
  # grab E & GDP for both BY & CY
  EI_CYBY_params <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, 
           reg_iam_em_Xcon_year, reg_gdp_Xcon_year, 
           paste0('ctry_ref_em_X', base_year), paste0("ctry_gdp_X", base_year))
  names(EI_CYBY_params) <- c(names(EI_CYBY_params)[1:10], "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year")
  
  # attach E & GDP to par_df for both BY & CY
  par_df <- par_df %>% 
    left_join(EI_CYBY_params,  by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit"))
  
  # EI = E / GDP
  par_df <- par_df %>% 
    mutate( 
      # EICBY: country (CEDS) EI in base year (used for all numGrowthRates)
      EICBY = ctry_ref_em_Xbase_year / ctry_gdp_Xbase_year, 
      
      # EIRCY: regional (IAM) EI in convergence year (used for all numGrowthRates)
      # not used until after peak if numGrowthRates = 2
      EIRCY = reg_iam_em_Xcon_year / reg_gdp_Xcon_year
      
      )
}

# replace zero-valued emissions intensity in BY with 1/3 regional minimum
adjustEICBY <- function(par_df, CO2_or_negCY, twoGrowthRates) {
  
  if (CO2_or_negCY & twoGrowthRates) {
    joinCols <- c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
      "base_year", "con_year", "numGrowthRates", "peak_year", 
      "reg_iam_em_Xcon_year", "reg_gdp_Xcon_year", "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year", "reg_iam_em_Xpeak_year", "reg_gdp_Xpeak_year", 
      "EICBY", "EIRCY", "EIRPY", "EICPY")
  } else {
    joinCols <- c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
                  "base_year", "con_year", 
                  "reg_iam_em_Xcon_year", "reg_gdp_Xcon_year", "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year", 
                  "EICBY", "EIRCY")
  }
  
  # this methodology doesn't affect industrial sector
  industrial <- par_df %>% 
    filter(sector == "Industrial Sector")
  
  # replacement values can't be taken from countries in a region with zero emissions as well 
  zero_IAMreg_ref_em_BY <- par_df %>% 
    anti_join(., industrial, by=joinCols) %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>% 
    summarise(reg_ref_em_Xbase_year = sum(ctry_ref_em_Xbase_year)) %>% 
    ungroup() %>% 
    filter(reg_ref_em_Xbase_year == 0 ) %>% 
    select(-reg_ref_em_Xbase_year) %>% 
    inner_join(., par_df, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit"))  
  
  # the rows we're modifying are...
  zero_in_BY <- par_df %>%
    filter(EICBY == 0) %>% # zero-valued EICBY 
    # not industrial sector
    anti_join(., industrial, by = joinCols) %>%  
    # not zero-sum @ IAM-region-level
    anti_join(., zero_IAMreg_ref_em_BY, by=joinCols)
  
  # replacement values are calculated from all remaining rows...
  nonzero_in_BY <- par_df %>% 
    anti_join(., industrial, by = joinCols) %>% 
    anti_join(., zero_IAMreg_ref_em_BY, by = joinCols) %>% 
    anti_join(., zero_in_BY, by = joinCols)
  
  # by choosing min(that region's set of emissions intensity growth values for the same sector) / 3
  replacement_values <- nonzero_in_BY %>%
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>%
    summarise(replacement_value = min(EICBY)/3) %>%
    ungroup()
  
  # replace zero-valued baseyears with the value calculated above
  zero_in_BY.mod <- zero_in_BY %>% 
    select(-EICBY) %>%
    left_join(replacement_values, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
    dplyr::rename(EICBY = replacement_value)
  
  # diagnostic output report truncated set of columns 
  zero_in_BY.trunc <- zero_in_BY %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit)
  
  # bind unmodified (industrial, zero_IAMreg_ref_em_BY, nonzero_in_BY) and modified (zero_in_BY.mod) rows together
  par_df.mod <- rbind(industrial, zero_IAMreg_ref_em_BY, nonzero_in_BY, zero_in_BY.mod)
  
  return(list(par_df.mod, zero_in_BY.trunc))
  
}

# calculate EI_gr_C (nonCO2 & posCY)
equation2 <- function(par_df_ssp) {
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      # equation 2: nonCO2 & posCY 
      EI_gr_C = ( EIRCY / EICBY ) ^ ( 1/ (con_year - base_year) ),
      
      # clean NA or Inf values
      EI_gr_C = ifelse( is.na( EI_gr_C ), 0, EI_gr_C ),
      EI_gr_C = ifelse( is.infinite( EI_gr_C ), 0, EI_gr_C )
    )
  
}

# calculate EI_gr_C_am (CO2_or_negCY)
equation2a_EI_gr_C_am <- function(par_df_ssp) {
  
  par_df_ssp <- par_df_ssp %>%  
    mutate( dist = con_year - 2100 ) 
  
  # if numGrowthRates = 0, EI_gr_C_am = 0
  # if numGrowthRates = 1, calculate EI_gr_C_am from EICBY & EIRCY
  # if numGrowthRates = 2, calculate EI_gr_C_pm from EICBY & EIRPY
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      
      ## initialize column
      EI_gr_C_am = NA,
      
      ## numGrowthRates = 0 -> set to zero
      EI_gr_C_am = ifelse(
        
        # logical test
        numGrowthRates==0,
        
        # TRUE : flat emissions
        0,
        
        # FALSE : 1 or 2 growth rates. Must calculate (below)
        EI_gr_C_am ),
      
      ## numGrowthRates = 1 -> calculate EI_gr_C_am using EICBY  & EIRCY
      EI_gr_C_am = ifelse(
        
        # logical test
        numGrowthRates==1,
        
        # TRUE
        # equation 2a: CO2 or zero/negative CY emissions
        ( EIRCY - EICBY ) / ( con_year - base_year ),
                 
        # FALSE : leave as is
        # either 0 growth rates (above) or 2 growth rates (below) 
        EI_gr_C_am 
        
      ), # end EI_gr_C_am, numGrowthRates = 1
      
      
      # numGrowthRates = 2 -> calculate EI_gr_C_am using EICBY & EIRPY
      EI_gr_C_am = ifelse(
        
        # logical test
        numGrowthRates==2,
        
        # TRUE
        # equation 2a: CO2 or zero/negative CY emissions
        ( EIRPY - EICBY )  / ( peak_year + dist - base_year ),
        
        # FALSE: numGrowthRates != 2, leave as is
        EI_gr_C_am
        
      ), # end EI_gr_C_am, numGrowthRates = 2
      
     # clean NA or Inf values
     EI_gr_C_am = ifelse( is.na( EI_gr_C_am ), 0, EI_gr_C_am ),
     EI_gr_C_am = ifelse( is.infinite( EI_gr_C_am ), 0, EI_gr_C_am ),
     
     # initialize column
     EI_gr_C_pm = NA
     
     ) # end mutate
}

# calculate EI_gr_C_pm (CO2_or_negCY)
equation2a_EI_gr_C_pm <- function(par_df_ssp, year) {
  
  # if numGrowthRates = 0, EI_gr_C_pm = 0
  # if numGrowthRates = 1, EI_gr_C_pm = EI_gr_C_am
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      
      ## post-peak EI growth rate initialized w/ NA by equation2_EI_gr_C_am()
      
      ## if numGrowthRates %in% c(0,1) -> set to EI_gr_C_am
      EI_gr_C_pm = ifelse(
        
        # logical test
        numGrowthRates %in% c(0,1),
          
        # TRUE : flat emissions
        EI_gr_C_am,
          
        # FALSE : leave as is. Must calculate post-peak growth rate (below)
        EI_gr_C_pm
        
        ) ## end EI_gr_C_pm, numGrowthRates %in% c(0,1)
          
    ) ### end mutate, numGrowthRates %in% c(0,1)
  
  ### numGrowthRates = 2
  ### If peak_year, calculate EI_gr_C_pm from EICPY & EIRCY
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      
      ## EICPY
      EICPY = ifelse(
        
        # logical test 
        numGrowthRates == 2 & year == peak_year,
        
        # TRUE: emissions peak & peaked this year
        E_final / ctry_gdp_X_year,
        
        # FALSE : emissions either don't peak or don't peak this year
        # leave as is
        EICPY
        
      ), ## end EICPY, numGrowthRates=2
      
      ## EI_gr_C_pm 
      EI_gr_C_pm = ifelse(
        
        # logical test
        numGrowthRates==2 & year == peak_year,
        
        # TRUE
        # equation 2a: CO2 or zero/negative CY emissions
        ( EIRCY - EICPY ) / ( peak_year + dist - base_year ),
            
        # FALSE : leave as is
        EI_gr_C_pm
        
        ) ## end EI_gr_C_pm, numGrowthRates=2
      
      ) ### end mutate, numGrowthRates=2
        
  
  ### clean all EI_gr_C_pm NA or Inf values
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      EI_gr_C_pm = ifelse( is.na( EI_gr_C_pm ), 0, EI_gr_C_pm ),
      EI_gr_C_pm = ifelse( is.infinite( EI_gr_C_pm ), 0, EI_gr_C_pm )
    ) ### end mutate
      
}

# identify EI_gr_C at beginning of each time-step (no calculation in this function!) (CO2_or_negCY)
identify_EI_gr_C <- function(par_df_ssp, year) {
  
  # if numGrowthRates = 0, EI_gr_C = 0
  # if numGrowthRates = 1, EI_gr_C = EI_gr_C_am
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      
      ## initialize column
      EI_gr_C = NA,
      
      ## numGrowthRates = 0, EI_gr_C = 0
      EI_gr_C = ifelse(
        
        # logical test
        numGrowthRates == 0,
        
        # TRUE 
        0,
        
        # FALSE
        EI_gr_C
        
      ), ## end EI_gr_C, numGrowthRates = 0
      
      ## numGrowthRates = 1, EI_gr_C = EI_gr_C_am
      EI_gr_C = ifelse(
        
        # logical test
        numGrowthRates == 1,
        
        # TRUE
        EI_gr_C_am,
        
        # FALSE
        EI_gr_C
      )
      )## end EI_gr_C, numGrowthRates = 1

  
  # if numGrowthRates = 2 & year <= peak_year, EI_gr_C = EI_gr_C_am
  # if numGrowthRates = 2 & year > peak_year, EI_gr_C = EI_gr_C_pm
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      
      ## numGrowthRates = 2...
      EI_gr_C = ifelse(
        
        # logical test
        numGrowthRates == 2,
        
        # TRUE : peak_year?
        ifelse(
          
          # logical test
          year <= peak_year,
          
          # TRUE : EI_gr_C_pm hasn't been calculatd yet
          EI_gr_C_am,
          
          # FALSE : have passed peak_year
          EI_gr_C_pm
        ),
        
        # FALSE
        EI_gr_C
        
      ) ## end EI_gr_C, numGrowthRates = 2
      
      ) ### end mutate
}

# calculate this year's preliminary emissions intensity from last year's emissions & GDP (nonCO2 & posCY)
equation3 <- function(wide_df_ssp, res_df_ssp, par_df_ssp, year) {

  # identify/calculate last year's EI and drop into par_df_ssp
  
  if (year == as.numeric(base_year) + 1) {
    # either use adjusted EICBY values when calculating first time step...
    par_df_ssp <- par_df_ssp %>% 
      mutate(EI_prev = EICBY,
             ctry_ref_em_prev = "",
             ctry_gdp_prev = "")
    
  } else { 
    # or dynamically calculate last years EI-value from last year's GDP & downscaled emissions
    
    # last year's downscaled emissions (res_df)
    ctry_ref_em_X_year_less1 <- paste0( 'ctry_ref_em_X', ( year - 1 ) ) 
    ctry_ref_em_prev <- res_df_ssp %>% 
      select(region, iso, ssp_label, em, sector, model, scenario, unit,
             ctry_ref_em_X_year_less1) 
    names(ctry_ref_em_prev) <- c(names(ctry_ref_em_prev)[1:8], "ctry_ref_em_prev")
    
    # last year's gdp (wide_df)
    ctry_gdp_X_year_less1 <- paste0( 'ctry_gdp_X', ( year - 1 ) )
    ctry_gdp_prev <- wide_df_ssp %>% 
      select(region, iso, ssp_label, em, sector, model, scenario, unit,
             ctry_gdp_X_year_less1)
    names(ctry_gdp_prev) <- c(names(ctry_gdp_prev)[1:8], "ctry_gdp_prev")
    
    # in order to drop *new* previous year's E & GDP into par_df, we have to drop
    # the *old* previous year's E  & GDP already inside par_df
    if ( any( c("ctry_ref_em_prev", "ctry_gdp_prev") %in% names(par_df_ssp) ) ) {
      par_df_ssp <- par_df_ssp %>% 
        select(-matches("prev"))
    }      
    
    # EI_prev = E_prev / GDP_prev
    par_df_ssp <- par_df_ssp %>%
      left_join(ctry_ref_em_prev, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
      left_join(ctry_gdp_prev, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
      mutate( EI_prev = ctry_ref_em_prev / ctry_gdp_prev )
  }
  

  # calculate next year's preliminary emissions intensity
  # equation depends on entry in 'equation3' 
  # see identifyEquations2and3()
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      
      # equation 3: nonCO2 & posCY 
      EI_star = EI_prev * EI_gr_C
      
    )
}

# calculate this year's preliminary emissions intensity from last year's emissions & GDP (CO2_or_negCY)
equation3a <- function(wide_df_ssp, res_df_ssp, par_df_ssp, year) {
  
  # identify/calculate last year's EI and drop into par_df_ssp
  
  if (year == as.numeric(base_year) + 1) {
    # either use adjusted EICBY values when calculating first time step...
    par_df_ssp <- par_df_ssp %>% 
      mutate(EI_prev = EICBY,
             ctry_ref_em_prev = "",
             ctry_gdp_prev = "")
    
  } else { 
    # or dynamically calculate last years EI-value from last year's GDP & downscaled emissions
    
    # last year's downscaled emissions (res_df)
    ctry_ref_em_X_year_less1 <- paste0( 'ctry_ref_em_X', ( year - 1 ) ) 
    ctry_ref_em_prev <- res_df_ssp %>% 
      select(region, iso, ssp_label, em, sector, model, scenario, unit,
             ctry_ref_em_X_year_less1) 
    names(ctry_ref_em_prev) <- c(names(ctry_ref_em_prev)[1:8], "ctry_ref_em_prev")
    
    # last year's gdp (wide_df)
    ctry_gdp_X_year_less1 <- paste0( 'ctry_gdp_X', ( year - 1 ) )
    ctry_gdp_prev <- wide_df_ssp %>% 
      select(region, iso, ssp_label, em, sector, model, scenario, unit,
             ctry_gdp_X_year_less1)
    names(ctry_gdp_prev) <- c(names(ctry_gdp_prev)[1:8], "ctry_gdp_prev")
    
    # in order to drop *new* previous year's E & GDP into par_df, we have to drop
    # the *old* previous year's E  & GDP already inside par_df
    if ( any( c("ctry_ref_em_prev", "ctry_gdp_prev") %in% names(par_df_ssp) ) ) {
      par_df_ssp <- par_df_ssp %>% 
        select(-matches("prev"))
    }      
    
    # EI_prev = E_prev / GDP_prev
    par_df_ssp <- par_df_ssp %>%
      left_join(ctry_ref_em_prev, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
      left_join(ctry_gdp_prev, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
      mutate( EI_prev = ctry_ref_em_prev / ctry_gdp_prev )
  }
  
  
  # calculate next year's preliminary emissions intensity
  # equation depends on entry in 'equation3' 
  # see identifyEquations2and3()
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      
      # equation 3a: CO2 or zero/negative CY emissions
      EI_star = EI_prev + abs(EI_prev) * EI_gr_C
      )
}

# zero in BY rows Emissions Intensity bounded by min(abs(EI_star)) in region
adjustEI_star <- function(par_df, zero_in_BY.trunc) {
  nonzero_in_BY <- par_df %>% 
    anti_join(zero_in_BY.trunc, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit"))
  
  replacement_values <- nonzero_in_BY %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>%
    summarise(replacement_value = min(abs(EI_star))) %>% 
    ungroup()
  
  zero_in_BY.mod <- par_df %>% 
    inner_join(zero_in_BY.trunc, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
    left_join(replacement_values, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
    mutate(EI_star = ifelse(abs(EI_star) > abs(replacement_value), replacement_value, EI_star)) %>% 
    select(-replacement_value)
  
  return(rbind(nonzero_in_BY, zero_in_BY.mod))
  
}

# this year's preliminary emissions = this year's emissions intensity * this year's GDP
equation4 <- function(wide_df_ssp, par_df_ssp, year) {
  # grab this year's GDP 
  ctry_gdp_X_year <- paste0( 'ctry_gdp_X', year )
  gdp <- wide_df_ssp %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, 
           ctry_gdp_X_year) 
  names(gdp) <- c(names(gdp)[1:8], "ctry_gdp_X_year")
  
  # drop last year's GDP
  if ("ctry_gdp_X_year" %in% names(par_df_ssp) ) {
    par_df_ssp <- par_df_ssp %>% 
      select(-ctry_gdp_X_year)
  }   
  
  # drop *this* year's GDP into par_df_ssp
  # E = EI * GDP
  par_df_ssp <- par_df_ssp %>% 
    left_join(gdp, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
    mutate(E_star = EI_star * ctry_gdp_X_year)
}

# scale preliminary emissions to match this year's regional IAM emissions
equation5 <- function(wide_df_ssp, par_df_ssp, year, res_df_ssp) {
  # 2nd term of equation (5):
  # calculate regional emissions from this time step's preliminary country emissions
  temp_df_agg <- par_df_ssp %>%
    group_by(region, ssp_label, em, sector, scenario) %>%
    summarise(sum_E_star = sum(E_star, na.rm=T)) %>%
    ungroup()
  
  # drop last year's calculated regional emissions from par_df_ssp
  if ("sum_E_star" %in% names(par_df_ssp) ) {
    par_df_ssp <- par_df_ssp %>% 
      select(-sum_E_star)
  }
  
  # drop this year's calculated regional emissions into par_df_ssp
  par_df_ssp <- par_df_ssp %>%
    left_join(temp_df_agg, by=c("region", "ssp_label", "em", "sector", "scenario"))
  
  # grab this year's IAM regional emissions 
  reg_IAM_em <- wide_df_ssp %>% 
    select( region, iso, ssp_label, em, sector, model, scenario, paste0('reg_iam_em_X', year) )
  names(reg_IAM_em) <- c(names(reg_IAM_em)[1:7], "regional_IAM_Emissions")
  
  # drop last year's IAM regional emissions from par_df_ssp
  if ("regional_IAM_Emissions" %in% names(par_df_ssp) ) {
    par_df_ssp <- par_df_ssp %>% 
      select(-regional_IAM_Emissions)
  }
  
  # drop *this* year's IAM regional emissions into par_df_ssp
  par_df_ssp <- par_df_ssp %>% 
    left_join(reg_IAM_em , by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario"))
  
  # check if calculated regional emissions are zero but IAM regional emissions are non-zero
  # need to modify E_star & sum_E_star if sum_E_star == 0 & regional_IAM_Emissions != 0
  replace <- par_df_ssp %>%
    filter(sum_E_star==0 & regional_IAM_Emissions != 0)
  if (nrow(replace) > 0 ) {
    # we have to make modifications to E_star & sum_E_star in order to distribute the difference between
    # sum_E_star & regional_IAM_Emissions (DiffR, calculated below)
    # see Eqn's 6 & 7 for details as to why we have to make this modification

    # for these cases, we replace E_star & sum_E_star with country and regional emissions from
    # the last year there were non-zero regional emissions

    # get downscaled country emissions
    ctry_ref_em <- res_df_ssp %>%
      select(region, iso, ssp_label, em, sector, model, scenario, unit, matches("ctry_ref_em"))
    names(ctry_ref_em) <- gsub("ctry_ref_em_X", "", names(ctry_ref_em))
    ctry_ref_em.melt <- ctry_ref_em %>%
      gather(key=year, value=E.country, -region, -iso, -ssp_label, -em, -sector, -model, -scenario, -unit) %>%
      mutate(year = as.numeric(year))

    # aggregate regional emissions, but subset to the last non-zero year
    reg_ref_em.melt <- ctry_ref_em.melt %>%
      group_by(region, ssp_label, em, sector, model, scenario, unit, year) %>%
      # get regional totals
      summarise(E.region=sum(E.country)) %>%
      ungroup() %>%
      group_by(region, ssp_label, em, sector, model, scenario, unit) %>%
      # grab the last year there was a non-zero regional total
      filter(E.region != 0) %>%
      filter(year == max(year)) %>%
      ungroup()

    # combine country and regional emissions
    ctry_and_reg_em.melt <- ctry_ref_em.melt %>%
      inner_join(reg_ref_em.melt, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit", "year")) %>%
      select(-year)

    # replace E_star & sum_E_star w/ E.country & E.region, respectively
    replace <- replace %>%
      left_join(ctry_and_reg_em.melt, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>%
      select(-E_star, -sum_E_star) %>%
      dplyr::rename(E_star = E.country,
                    sum_E_star = E.region)

    # drop modified countries into par_df_ssp while leaving all other countries unaffected
    par_df_ssp <- par_df_ssp %>%
      anti_join(replace, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>%
      rbind(replace)
  }

  # leftover regional emissions (distributed across downscaled countries) = IAM_emissions - sum_E_star
 par_df_ssp <- par_df_ssp %>% 
    mutate(DiffR = regional_IAM_Emissions  - sum_E_star)
  
  
}

# calculate each country's share of preliminary, calculated regional emissions
equation6 <- function(par_df_ssp) {
  # each country's share of calculated regional emissions
  # (used to portion out DiffR)
  # E_share always positive so that E_adj is same sign as DiffR
  par_df_ssp <- par_df_ssp %>% 
    mutate(E_share = E_star / sum_E_star,
           E_share = ifelse( is.na( E_share ), 0, E_share ),
           E_share = ifelse( is.infinite( E_share ), 0, E_share ) )
}

# rescale preliminary downscaled emissions so that they sum to IAM regional emissions
equation7 <- function(par_df_ssp) {
  # final adjustment to preliminary country emissions 
  # using portion of DiffR = IAM_Regional_Emissinos  REF_Regional_Emissions
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      E_adj = DiffR * E_share,
      E_final = E_star + E_adj,
      E_final = ifelse( is.na( E_final ), 0, E_final ),
      E_final = ifelse( is.infinite( E_final ), 0, E_final ) 
           )
  
}

# output calculation parameters data.frame
saveCalculation <- function(par_df_ssp, year, calculationYears, calculationDir) {
  fn <- paste0(calculationDir, "/par_df", ".csv") 
  
  if (year == calculationYears[1]) {
    print(paste0("Initializing ", fn, " with ", year, " data"))
    app <- FALSE
  } else {
    print(paste0("Appending ", year, " data to ", fn))
    app <- TRUE
  }
  col <- !app
  
  df <- par_df_ssp %>% 
    mutate(year = year) %>% 
    group_by(year, region, em, sector, model, scenario) %>% 
    mutate(
      EICBY.percentDiff = (EICBY - mean(EICBY)) / mean(EICBY),
      EI_gr_C_am.percentDiff = (EI_gr_C_am - mean(EI_gr_C_am)) / mean(EI_gr_C_am),
      EI_prev.percentDiff = (EI_prev - mean(EI_prev)) / mean(EI_prev),
      EI_star.percentDiff = (EI_star - mean(EI_star)) / mean(EI_star),
      ctry_gdp_X_year.percentDiff = (ctry_gdp_X_year - mean(ctry_gdp_X_year)) / mean(ctry_gdp_X_year),
      E_star.percentDiff = (E_star - mean(E_star)) / mean(E_star),
      regionalDeviation = DiffR / regional_IAM_Emissions
    ) %>% 
    select(year, region, iso, em, sector, model, scenario, 
           base_year, con_year, dist, numGrowthRates, peak_year,
           equation2,
           equation3,
           matches("em_Xbase"), matches("gdp_Xbase"), 
           matches("em_Xpeak"), matches("gdp_Xpeak"),
           matches("em_Xcon"), matches("gdp_Xcon"),
           matches("EICBY"), matches("EIRPY"), matches("EICPY"), matches("EIRCY"), 
           matches("EI_gr_C_am"), matches("EI_gr_C_pm"), matches("EI_gr_C$"),
           matches("ref_em_prev"), matches("gdp_prev"), matches("EI_prev"),
           matches("EI_star"), 
           matches("gdp_X_year"), matches("^E_star"),
           matches("sum_E_star"), matches("regional_IAM_Emissions"), matches("DiffR"), matches("regionalDeviation"),
           matches("E_share"), matches("E_adj"), matches("E_final")) %>% 
    arrange(ctry_ref_em_Xbase_year) %>% 
    filter(em == "CO2" & sector == "Energy Sector")
  
  # df.melt <- df %>% 
  #   gather(key=parameter, value=value, -year, -region, -iso, -em, -sector, -model, -scenario) 
  write.table(df, fn, append=app, sep=",", col.names=col, row.names=F)
    
}

# append timeseries of column parameter
save_parameter <- function(par_df_ssp, year, column, calculationDir) {
  column.quo <- rlang::enquo(column)
  column.string <- rlang::quo_name(column.quo)
  df <- par_df_ssp %>% 
    select(region, iso, em, sector, model, scenario, !!column.quo) %>% 
    mutate(year = year)
  
  
  if (year == 2016) {
    app <- FALSE
  } else {
    app <- TRUE
  }
  col <- !app
  fn <- paste0(calculationDir, "/", column.string, ".csv")
  write.table(df, fn, append=app, sep=",", col.names=col, row.names=F)
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
      if (year %in% c(2097:2100)) {
        saveCalculation(par_df_ssp, "master", 2, year)
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
        saveCalculation(par_df_ssp, "master", 2, year)
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