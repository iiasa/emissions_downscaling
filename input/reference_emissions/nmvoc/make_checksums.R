# make_checksums.R --------------------------------------------------------
#
# Take historical speciated NMVOCs from VUA and output checksum files in the
# format created by emissions_downscaling.
#
# Caleb Braun
# 7/10/18
#
# To use, you must have a directory containing the historical files, which have
# the standard format:
#
#   NMVOC-EM-em-biomassburning_input4MIPs_emissions_CMIP_VUA-CMIP-BB4CMIP6-1-2_gn_START-END.nc
#
# where EM is the speciated NMVOC (e.g. C10H16), and START and END specify the
# date range covered by the file in YYYYMM format. These files can be downloaded
# from ESGF.

library('ncdf4')
library('dplyr')


# Declare globals, find files ---------------------------------------------

wd <- getwd()
setwd('C:/Users/brau074/Downloads/historicalVOC/VUA_SMALL')

GRIDCELL_AREAS_FILE <- 'CEDS_gridcell_area_05.nc'
NC_FILE_PATTERN <- 'NMVOC-[CHMT].*-SMALL.nc'
DIAG_CELLS_FILE <- 'diagnostic_cells.csv'

DAYS_PER_MONTH <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
SECONDS_IN_DAY <- 24 * 60 * 60

GEN_DIAG_CELLS <- TRUE


gridcell_area <- nc_open(GRIDCELL_AREAS_FILE)
gc_areas <- ncvar_get(gridcell_area, varid = 'gridcell area')
nc_close(gridcell_area)

vua_files <- list.files(pattern = NC_FILE_PATTERN)
stopifnot(length(vua_files) > 0)

rotate_a_matrix <- function( x ) {
  t( x[ nrow(x):1, ] )
}

# Write diagnostic cells --------------------------------------------------

# Parameter:
#   cell_totals - A 3d array (lon, lat, time) of values in kg/s
write_diagnostic_cells <- function(cell_totals, varid, fname) {
  START_YEAR <- 2006
  diag_cells <- read.csv(DIAG_CELLS_FILE, stringsAsFactors = F)

  # convert the matrix from from kg/s to Mt/month
  cell_totals <- sweep(cell_totals, 3, (DAYS_PER_MONTH * SECONDS_IN_DAY / 1e6), `*`)

  # computation for diagnostic cells by month
  nmonths <- dim(cell_totals)[3]
  cells_by_month <- lapply(1:nmonths, function(month) {
    cells <- rotate_a_matrix(cell_totals[ , , month])
    cells <- cells[360:1, 720:1]
    cells <- cells[ as.matrix( diag_cells[ c( 'row', 'col' ) ] ) ]
    cells[is.na(cells)] <- 0
    cell_value_df <- data.frame( em = varid,
                                 sector = 'all',
                                 year = START_YEAR + floor(month / 12),
                                 month = ((month - 1) %% 12) + 1,
                                 unit = 'Mt',
                                 value = cells,
                                 stringsAsFactors = F )
    cell_value_df <- cbind( diag_cells, cell_value_df )
  })

  cells_by_month <- do.call(rbind, cells_by_month)
  write.csv(cells_by_month, sub('\\.nc', '_cells.csv', fname), row.names = F)
}


# Loop through files, calculate global totals, write to csv ---------------

invisible(lapply(vua_files, function(fname) {
  i <- which(fname == vua_files)
  cat(paste0("\rProcessing file ", i, "/", length(vua_files), '... '))

  # Open the NetCDF file and read the variable that matches the filename.
  f <- nc_open(fname)
  fvars <- names(f$var)
  varid <- fvars[sapply(fvars, grepl, gsub('-', '_', fname))]
  stopifnot(length(varid) == 1)       # Assert that one and only one var matches
  vals <- ncvar_get(f, varid)
  stopifnot(length(dim(vals)) == 3)   # Assert that values are 3d (lon/lat/time)
  stopifnot(dim(vals)[3] %% 12 == 0)  # Assert that time dimension can be months
  nc_close(f)

  # 1. Multiply each grid cell by the cell's area
  # 2. Sum all cells by month to get global total
  # 3. Convert values from kg m-2 s-1 to kt/month
  total_by_mon <- sweep(vals, c(1,2), gc_areas, `*`)
  if (GEN_DIAG_CELLS) write_diagnostic_cells(total_by_mon, varid, fname)
  total_by_mon <- colSums(total_by_mon, na.rm = T, dims = 2)
  total_by_mon <- total_by_mon * (DAYS_PER_MONTH * SECONDS_IN_DAY / 1e6)

  df <- data.frame(em = varid, sector = 'all', year = rep(2006:2015, each = 12),
                   month = rep(1:12, times = 10), units = 'kt',
                   global_total = total_by_mon, stringsAsFactors = F)

  write.csv(df, sub('\\.nc', '.csv', fname), row.names = F)
}))

cat('\nDone.\n')
setwd(wd)

