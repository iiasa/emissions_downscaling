

# downscaleIAMemissions ----------------------------------------
downscaleIAMemissions <- function( wide_df, con_year_mapping) { 
  
  # set up two working df: parameter data frame and results data frame
  
  # when a parameter is needed for a step in the calculation, a separate df is 
  # subset from wide_df to isolate a specific year's GDP, or from res_df for 
  # the previous year's downscaled emissions. 
  # this new df is then left_joined to par_df so as to match the new parameter 
  # column to the existing data in par_df using the original par_df columns (region, iso, ssp_label, em, ...)
  # in this way, each time-step has every step of the calculation stored in par_df, but reports only
  # E_final in res_df.
  
  par_df <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit) %>% 
    mutate(base_year = base_year) %>% 
    left_join(con_year_mapping, by=c("ssp_label" = "scenario_label")) %>% 
    dplyr::rename(con_year = convergence_year) %>% 
    mutate(dist = con_year - 2100) # used for calculating pre-peak growth rate
  
  res_df <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, paste0('ctry_ref_em_X', base_year))
  
  # identify EI_Gr_C methodology to be applied 
  # whether to apply 1 or 2 growth rates (or growthrate = 0 ) depends on if/when emissions peak
  # this function attaches to par_df the following columns
  # case: 1 for monotonic growth (peaks in BY or CY), 2 for flat growth (no peak), 3 for peak not in CY or BY
  # peak_year: year of peak for cases 1 or 3, N/A for case 2
  par_df <- identifyGrowthMethods(wide_df, con_year_mapping, par_df)
  
  # Calculate EI for CY, BY, and PY (in case 3)
  par_df <- equation1(wide_df, con_year_mapping, par_df)
  
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

    # calculate EI growth rate depending on (a) case (peak or not) and (b) CY emissions (pos or neg)
    par_df_ssp <- equation2(par_df_ssp)
    
    # calculation of each year's downscaled country emissions
    for ( year in ( base_year + 1 ) : ds_end_year ) {
      
      # calculate next year's prelminary emissions intensity 
      par_df_ssp <- equation3(wide_df_ssp, res_df_ssp, par_df_ssp, year) 
      
      # EI_star pathways for zero_in_BY rows is bounded by minimum EI_star in nonzero_in_BY rows
      par_df_ssp <- adjustEI_star(par_df_ssp, zero_in_BY.trunc)
      
      # equation (4) : 
      # this year's preliminary emissions = this year's emissions intensity * this year's GDP
      
      # drop this year's GDP into par_df_ssp
      ctry_gdp_X_year <- paste0( 'ctry_gdp_X', year )
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
        left_join(gdp, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
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
        left_join(reg_IAM_em , by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario")) %>% 
        mutate(DiffR = regional_IAM_Emissions  - sum_E_star)
      
      # equation (6):
      # each country's share of calculated regional emissions
      # (used to portion out DiffR)
      # E_share always positive so that E_adj is same sign as DiffR
      par_df_ssp <- par_df_ssp %>% 
        mutate(E_share = abs(E_star) / abs(sum_E_star),
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
      if (year %in% c(2070:2073)) {
        saveCalculation(par_df_ssp, "branch", year)
      }
      
      save_EI_prelim(par_df_ssp, year)
      
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

# adds 'case' column which identifies whether emissions (1) peak in BY or CY, (2) never peak, or (3) peak in some other year
# also adds 'peak_year' column to par_df
identifyGrowthMethods <- function(wide_df, con_year_mapping, par_df) {
  # grab regional emissions
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
  
  # case 1: peaks in BY or CY
  monotonic <- reg_iam_em %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>% 
    filter(length(unique(E)) != 1) %>% # non-zero slope
    filter(E==max(E)) %>%
    ungroup() %>% 
    filter(year == base_year | year >= 2100) %>% 
    mutate(case=1)
  
  # case 2: flat emissions (can be zero, or non-zero as a result of harmonization procedure)
  flat <- reg_iam_em %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>% 
    filter(length(unique(E)) == 1) %>% # zero slope
    ungroup() %>% 
    mutate(case=2)
  
  # case 3: peaks in a single year that isn't BY or CY
  peaks <- reg_iam_em %>% 
    group_by(region, ssp_label, em, sector, model, scenario, unit) %>% 
    filter(E==max(E)) %>%
    ungroup() %>% 
    anti_join(monotonic, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
    anti_join(flat, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit")) %>% 
    mutate(case=3) 
  
  # data.frame that holds "case" and "peak_year" parameters
  # flat contains entire emissions time-series, so must collapse by storing 'peak_year' as N/A and grabbing distinct columns
  # After doing this, the data.frame is uniquely identified by base set of columns (see initialization of par_df) 
  PY <- monotonic %>% 
    full_join(flat, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit", "year", "E", "case")) %>% 
    full_join(peaks, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit", "year", "E", "case")) %>% 
    mutate(peak_year = ifelse(case %in% c(1,3), year, NA)) %>% 
    select(-year) %>% 
    distinct() %>% 
    select(-E)
  
  par_df <- par_df %>% 
    left_join(PY, by = c("region", "ssp_label", "em", "sector", "model", "scenario", "unit"))
}

# Calculate EI for CY, BY, and PY (in case 3)
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
  
  # in order to attach E & GDP for PY, we must match on par_df's peak_year
  # This requires E & GDP for all years in long format
  
  # grab E for all years (long format)
  E.wide <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, matches("reg_iam_em_X"))
  names(E.wide) <- gsub("reg_iam_em_X", "", names(E.wide))
  E <- E.wide %>% 
    gather(key=year, value=E, -region, -iso, -ssp_label, -em, -sector, -model, -scenario, -unit) %>% 
    mutate(case=3) # used along with peak_year to match PY to correct rows
  
  # grab GDP for all years (long format)
  GDP.wide <- wide_df %>% 
    select(region, iso, ssp_label, em, sector, model, scenario, unit, matches("reg_gdp_X"))
  names(GDP.wide) <- gsub("reg_gdp_X", "", names(GDP.wide))
  GDP <- GDP.wide %>% 
    gather(key=year, value=GDP, -region, -iso, -ssp_label, -em, -sector, -model, -scenario, -unit) %>% 
    mutate(case=3) # used along with peak_year to match PY to correct rows
  
  # combine E & GDP for all years (long format)
  EI_PY_params <- inner_join(E, GDP, 
                             by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", "year", "case")) %>% 
    left_join(con_year_mapping, by=c("ssp_label" = "scenario_label")) %>% 
    dplyr::rename(con_year = convergence_year) %>% 
    mutate(year = ifelse(year == "con_year", con_year, year)) %>%
    select(-con_year) %>% 
    mutate(year = as.numeric(year))
    
  
  # attach E & GDP only for case 3 peak_years
  par_df <- par_df %>% 
    left_join(EI_PY_params, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
                                   "peak_year"="year", "case")) %>% 
    dplyr::rename(reg_iam_em_Xpeak_year=E,
                  reg_gdp_Xpeak_year=GDP)
  
  
  # EI = E / GDP
  par_df <- par_df %>% 
    mutate( 
      # EICBY: country (CEDS) EI in base year (used in all cases)
      EICBY = ctry_ref_em_Xbase_year / ctry_gdp_Xbase_year, 
      
      # EIRCY: regional (IAM) EI in convergence year (used in all cases)
      # not used until after peak for case 3
      EIRCY = reg_iam_em_Xcon_year / reg_gdp_Xcon_year,
      
      # EIRPY: regional (IAM) EI in peak year (used only in case 3)
      EIRPY = ifelse(case==3, 
                     reg_iam_em_Xpeak_year / reg_gdp_Xpeak_year, 
                     NA),
      
      # EICPY: country (downscaled) EI in peak_year (used only in case 3)
      # identified for each row when calculation reaches peak_year
      EICPY = NA )
  
}

adjustEICBY <- function(par_df) {
  # this methodology doesn't affect industrial sector
  industrial <- par_df %>% 
    filter(sector == "Industrial Sector")
  
  # replacement values can't be taken from countries in a region with zero emissions as well 
  zero_IAMreg_ref_em_BY <- par_df %>% 
    anti_join(., industrial, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
                                    "base_year", "con_year", "dist", "case", "peak_year", 
                                    "reg_iam_em_Xcon_year", "reg_gdp_Xcon_year", "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year", "reg_iam_em_Xpeak_year", "reg_gdp_Xpeak_year", 
                                    "EICBY", "EIRCY", "EIRPY")) %>% 
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
    anti_join(., industrial, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
                                    "base_year", "con_year", "dist", "case", "peak_year", 
                                    "reg_iam_em_Xcon_year", "reg_gdp_Xcon_year", "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year", "reg_iam_em_Xpeak_year", "reg_gdp_Xpeak_year", 
                                    "EICBY", "EIRCY", "EIRPY")) %>%  
    # not zero-sum @ IAM-region-level
    anti_join(., zero_IAMreg_ref_em_BY, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
                                               "base_year", "con_year", "dist", "case", "peak_year", 
                                               "reg_iam_em_Xcon_year", "reg_gdp_Xcon_year", "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year", "reg_iam_em_Xpeak_year", "reg_gdp_Xpeak_year", 
                                               "EICBY", "EIRCY", "EIRPY"))
  
  # replacement values are calculated from all remaining rows...
  nonzero_in_BY <- par_df %>% 
    anti_join(., industrial, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
                                    "base_year", "con_year", "dist", "case", "peak_year", 
                                    "reg_iam_em_Xcon_year", "reg_gdp_Xcon_year", "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year", "reg_iam_em_Xpeak_year", "reg_gdp_Xpeak_year", 
                                    "EICBY", "EIRCY", "EIRPY")) %>% 
    anti_join(., zero_IAMreg_ref_em_BY, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
                                               "base_year", "con_year", "dist", "case", "peak_year", 
                                               "reg_iam_em_Xcon_year", "reg_gdp_Xcon_year", "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year", "reg_iam_em_Xpeak_year", "reg_gdp_Xpeak_year", 
                                               "EICBY", "EIRCY", "EIRPY")) %>% 
    anti_join(., zero_in_BY, by = c("region", "iso", "ssp_label", "em", "sector", "model", "scenario", "unit", 
                                    "base_year", "con_year", "dist", "case", "peak_year", 
                                    "reg_iam_em_Xcon_year", "reg_gdp_Xcon_year", "ctry_ref_em_Xbase_year", "ctry_gdp_Xbase_year", "reg_iam_em_Xpeak_year", "reg_gdp_Xpeak_year", 
                                    "EICBY", "EIRCY", "EIRPY"))
  
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

# calculate EI growth rate depending on (a) case and (b) CY emissions
equation2 <- function(par_df_ssp) {
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      
      EI_gr_C = ifelse(
        
        # logical test
        case %in% c(1,2),
        
        # TRUE
        # use EICBY & EIRCY for cases 1 & 2
        ifelse(reg_iam_em_Xcon_year >= 0,
               
               # eq 2 for positive CY emissions
               ( EIRCY / EICBY ) ^ ( 1 / ( con_year - base_year ) ),
               # eq 2a for negative CY emissions
               ( ( EIRCY - EICBY ) / EICBY ) / ( con_year - base_year ) 
        ),
        
        # FALSE
        # use EICBY & EIRPY for case 3 
        ifelse(reg_iam_em_Xcon_year >= 0,
               
               # eq 2 for positive CY emissions
               ( EIRPY / EICBY ) ^ ( 1 / ( peak_year + dist - base_year ) ),
               # eq 2a for negative CY emissions
               ( ( EIRPY - EICBY ) / EICBY ) / ( peak_year + dist - base_year ) 
        )
      ) ,
     
                
     # clean NA or Inf values
     EI_gr_C = ifelse( is.na( EI_gr_C ), 0, EI_gr_C ),
     EI_gr_C = ifelse( is.infinite( EI_gr_C ), 0, EI_gr_C ) )
}

# calculate next year's preliminary emissions intensity
equation3 <- function(wide_df_ssp, res_df_ssp, par_df_ssp, year) {
  
  
  # identify/calculate last year's EI and drop into par_df_ssp
  
  if (year == base_year + 1) {
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
  
  # countries that peak in emission use two growth rates
  # if it is after the peak_year, we must use the post-peak growth rate.
  # post-peak growth rate depends on EICPY & EIRCY
  # when year == peak_year + 1, EICPY = EI_prev
  # this means for year == peak_year, we have to (1) identify EICPY and (2) update EI_gr_C
  
  par_df_ssp <- par_df_ssp %>% 
    mutate(
      
      # ID EICPY-value if case 3 & year == peak_year + 1
      EICPY = ifelse (
        
        # logical test 
        case==3,
        
        # TRUE: emissions peak therefore we may have to store EICPY this timestep
        ifelse (
          
          # logical test
          year == peak_year + 1,
          
          # TRUE: it's currently 1 year after peak_year, so EICPY = EI_prev
          EI_prev,
          
          # FALSE: EI_prev != EICPY, so EICPY remains as is 
          # note: EICPY was initialied as NA in eq. 1, but EICPY may have been ID'd in prev timestep (or will be in later timestep)
          EICPY
        ),
        
        # FALSE: emissions do not peak therefore we leave EICPY as NA (initialized in eq. 1)
        EICPY
      ),
      
      # update EI_gr_C if case 3 & year == peak_year + 1
      EI_gr_C = ifelse(
        
        # logical test
        case==3,
        
        # TRUE: emissions peak therefore we may have to update EI_gr_C this timestep
        ifelse(
          
          # logical test
          year == peak_year + 1,
          
          #TRUE: it is 1 year after peak_year, so we update EI_gr_C using recently calculated EICPY
          ifelse(reg_iam_em_Xcon_year >= 0,
                 
                 # eq 2 for positive CY emissions
                 ( EIRCY / EICPY ) ^ ( 1 / ( con_year - peak_year ) ),
                 # eq 2a for negative CY emissions
                 ( ( EIRCY - EICPY ) / EICPY ) / ( con_year - peak_year ) 
          ),
          
          # FALSE: leave EI_gr_C alone
          # either (a) emissions haven't peaked yet 
          # or (b) post-peak growth rate has already been calculated
          EI_gr_C
        ),
        
        # FALSE: emissions don't peak therefore we maintain EI_gr_C
        EI_gr_C
      )
    )
  
  # calculate next year's prelminary emissions intensity
  # formula depends on regional IAM emissions in CY 
  par_df_ssp <- par_df_ssp %>% 
    mutate(EI_star = ifelse(reg_iam_em_Xcon_year >= 0,
                            EI_prev * EI_gr_C, 
                            EI_prev + abs(EI_prev) * EI_gr_C))
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

# output calculation parameters data.frame
saveCalculation <- function(par_df_ssp, file, year) {
  fn <- paste0("C:/users/guti220/desktop/random files/ds_calculation/", file) %>% 
    paste(., year, "csv", sep=".")
  print(paste0("saving ", fn))
  
  par_df_ssp %>% 
    select(region, iso, em, sector, model, scenario, 
           base_year, con_year, dist, case, peak_year,
           matches("em_Xbase"), matches("gdp_Xbase"), matches("EICBY"),
           matches("em_Xpeak"), matches("gdp_Xpeak"), matches("EIRPY"), matches("EICPY"),
           matches("em_Xcon"), matches("gdp_Xcon"), matches("EIRCY"), 
           matches("EI_gr_C"),
           matches("ref_em_prev"), matches("gdp_prev"), matches("EI_prev"), matches("EI_star"), 
           matches("gdp_X_year"), matches("^E_star"),
           matches("sum_E_star"), matches("regional_IAM_Emissions"), matches("DiffR"), 
           matches("E_share"), matches("E_adj"), matches("E_final")) %>% 
    arrange(ctry_ref_em_Xbase_year) %>% 
    #filter(region == "AFR" & em == "CO2" & sector == "Energy Sector") %>% 
    write.csv(fn, row.names=F)
}

# append timeseries of Emissions Intensities before scaling
save_EI_prelim <- function(par_df_ssp, year) {
  EI_prelim <- par_df_ssp %>% 
    select(region, iso, em, sector, model, scenario, EI_star) %>% 
    mutate(year = year)
  
  
  if (year == 2016) {
    app <- FALSE
  } else {
    app <- TRUE
  }
  col <- !app
  fn <- paste0("C:/users/xavie/desktop/work/ds_calculation/EI_prelim.csv")
  write.table(EI_prelim, fn, append=app, sep=",", col.names=col, row.names=F)
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