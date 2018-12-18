# Copyright 2018 Battelle Memorial Institute

# ------------------------------------------------------------------------------
# Program Name: C.2.1.gridding_nonair.R
# Author: Caleb Braun
# Date: November, 2018
# Program Purpose: Split out negative emissions and distibute in new sector
# ------------------------------------------------------------------------------

addNegativesAsSector <- function(allyear_grids_list, neg_totals) {
  neg_grid <- readNegativeCO2Grid()
  multiplier <- prop.table(neg_grid)
  neg_em_grids <- lapply(neg_totals, `*`, multiplier)
  neg_em_grids <- lapply(neg_em_grids, convertToFlux, unit = 'Mt')
  neg_em_grids <- lapply(neg_em_grids, extendToMonth)

  Map(`[[<-`, allyear_grids_list, 'NEGCO2', neg_em_grids)
}

readNegativeCO2Grid <- function() {
  fn <- filePath('GRIDDING', 'negCO2/LIGNOPotentialT_2010', '.nc')
  nc <- ncdf4::nc_open(fn)
  potential_biofuel_grid <- ncdf4::ncvar_get(nc)
  ncdf4::nc_close(nc)

  potential_biofuel_grid[is.na(potential_biofuel_grid)] <- 0

  potential_biofuel_grid
}

extractNegativeEms <- function(gridding_em, year_list) {
  gridding_em %>%
    dplyr::select(num_range('X', year_list)) %>%
    dplyr::mutate_all(funs(if_else(. > 0, 0, .))) %>%
    colSums()
}

zeroNegativeEms <- function(gridding_em) {
    dplyr::mutate_if(gridding_em, is.numeric, funs(if_else(. < 0, 0, .)))
}

convertToFlux <- function(grid, unit) {
  SEC_IN_YEAR <- 365 * 24 * 60 * 60

  kg_per_unit <- switch (tolower(unit),
    mt = 1e9,
    kt = 1e6,
    t  = 1e3,
    kg = 1,
    stop("Unit not recognized.")
  )

  global_grid_area <- grid_area(grid_resolution, all_lon = T)

  stopifnot(all(dim(grid) == dim(global_grid_area)))

  grid * (kg_per_unit / global_grid_area / SEC_IN_YEAR) # from Mt to kg m-2 s-1
}

extendToMonth <- function(grid) {
  old.dims <- dim(grid)
  new.dims <- c(old.dims, 12)
  array(rep(grid, 12), dim = new.dims)
}
