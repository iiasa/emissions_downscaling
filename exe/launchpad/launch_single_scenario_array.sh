#!/bin/bash
#SBATCH -A IHESD
#SBATCH -t 90
#SBATCH -n 1
#SBATCH -p shared

# -----------------------------------------------------------------------------
# Copyright 2018 Battelle Memorial Institute
#
# Use this script to run an emission species for all scenarios in parallel
# using the Slurm scheduler.
#
# Run as an array with the command:
#     sbatch --array=0-8 launch_single_em_array.sh BC
# to launch (9 scenarios * 1 species) runs
# -----------------------------------------------------------------------------

module purge
module load R/3.4.3
module load gcc/4.9.2 # needed by lubridate package

now=$(date)
echo "Current time: $now"

# Set working directory to package root
while [ ! -d "input" ]; do
	cd ../
done

# Set up our scenarios and emissions
declare -a ems=('BC' 'CH4' 'CO' 'CO2' 'NH3' 'NOx' 'OC' 'Sulfur' 'NMVOC') # 9 emissions

iam=$1
iam_dir=$2

# -----------------------------------------------------------------------------
# RUN
#
# All path parameters to exe/launchpad/launch_downscaling_gridding.R must be
# specified relative to the input directory.
#
# Parameters are, in order:
#     1. Model name
#     2. Harmonization type
#     3. Input .xlsx file
#     4. Module B output
#     5. Module C output
#     6. Gridding flag
#     7. Run species (optional)
echo "Run command:"
echo "Rscript exe/launchpad/launch_downscaling_gridding.R $iam Harmonized-DB IAM_emissions/$iam_dir/output_harmonized.xlsx ../final-output/modB/ ../final-output/modC/ gridding $em --nosave --no-restore"

Rscript exe/launchpad/launch_downscaling_gridding.R $iam Harmonized-DB IAM_emissions/$iam_dir/output_harmonized.xlsx ../final-output/module-B/ ../final-output/module-C/ gridding $em --nosave --no-restore

cd exe/launchpad/

now=$(date)
echo "Current time: $now"
