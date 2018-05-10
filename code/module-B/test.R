
errorLogging <- function(in.agg, out.agg, method, t) {
  
  # same number of rows/cols, different values
  in.mis <- anti_join(in.agg, out.agg, by = c("model", "scenario", "region", "em", "sector", "unit", "year", "value"))
  out.mis <- anti_join(out.agg, in.agg, by = c("model", "scenario", "region", "em", "sector", "unit", "year", "value"))
  
  # check if any rows in mismatched values df
  if(nrow(in.mis) != 0) {
    printLog(paste0(method, " downscaling: there are output rows that don't match input. Check error log"))
    
    # distinct error log for each (m, s) with mismatched values
    for (model.name in unique(in.mis$model)) {
      for (scen in unique(in.mis$scenario)) {
        
        # m can't contain any /
        model.name.mod <- gsub("/", "-", model.name)
        
        # open error log, name according to (m, s)
        fn <- paste0("../code/error/ERROR-", method, " ", model.name.mod, ", ", scen, ".txt")
        zz <- file(fn, open="wt")
        sink(zz) # divert session output to error log
        print(paste0(method, " downscaling error"))
        print("Downscaled emissions aggregated to native IAM regions don't match IAM emissions")
        
        df2 <- in.mis %>% 
          select(-value, -unit) %>% # drop columns that don't need to be reported
          filter(model == model.name & scenario == scen) %>% # filter to distinct model & scenario
          group_by(model, scenario, region, em, sector, ) %>% # for each region-em-sector in (m,s), 
          summarise(years = paste0(year, collapse=", ")) %>% # print years that have mismatched values
          ungroup()
        
        # for each mismatched row, print the following keys:
        # model, scenario, region, em, harm_status, year 
        print("Rows with mismatched values:")
        for (i in 1:nrow(df2)) {
          paste0(df2[i,], collapse=", ") %>% 
            print()
        }
        
        # close sink diversion then file connection
        sink()
        close(zz)
      }
    } # end of error logging
    
    # write diagnostic file with both sets of mismatched values ( for all models & scenarios )
    
    # contains the rows from input that don't match output
    in.mis <- in.mis %>% 
      dplyr::rename(input=value)
    
    # contains the rows from output that don't match input
    out.mis <- out.mis %>% 
      dplyr::rename(output=value)
    
    # place input/output data columns next to each other for each year of mismatched values
    mis <- full_join(in.mis, out.mis,
                     by=c("model", "scenario", "region", "em", "sector", "unit", "year")) %>% 
      mutate(difference = output - input,
             percentDifference = difference / input) %>% 
      filter(percentDifference > t) %>% 
      filter( year %in% seq(2015, 2100, by=5))  # only keep model years (every 5)
    mis.ds <- mis %>% 
      filter(year != 2015) # drop base year values
    mis.harm <- mis %>% 
      filter(year == 2015)
      
    
    # construct diagnostic file name
    iam <- unique(mis$model)
    out_filename.ds <- paste0( iam, '_emissions_downscaled_', method, '_inconsistent' )
    out_filename.harm <- paste0( iam, '_emissions_downscaled_', method, '_inconsistent_harmonization' )
    
    # save diagnostic file
    writeData( mis.ds, 'ERR', out_filename.ds, meta = F ) 
    writeData( mis.harm, 'ERR', out_filename.harm, meta = F ) 
    
    
  } else {
    printLog(paste0(method, " downscaling: all output rows match input"))
  }
}
