# IAM Emissions Downscaling

These routines take emissions data from Integrated Assessment Models and do two things:

1. Downscale from native IAM regions to country level
2. Map those country level emissions to a global, spatial grid, and export those into netCDF files in CMIP6 standard format.

User input can specify to run just downscaling, or downscaling & gridding. See the wiki page [Running the system](https://github.com/iiasa/emissions_downscaling/wiki/Running-the-system) for a quick guide to getting started.

### Example output:

![BC emissions example](/documentation/img/BC-anthro_emissions.png)
