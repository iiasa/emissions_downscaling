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
declare -a iam_dirs=('IMAGE_SSP1-19' 'IMAGE_SSP1-26' 'MESSAGE-GLOBIOM_SSP2-45' 'AIM_SSP3-Ref' 'AIM_SSP3-LowNTCF' 'GCAM4_SSP4-34' 'GCAM4_SSP4-60' 'REMIND-MAGPIE_SSP5-Ref' 'REMIND-MAGPIE_SSP5-34-OS')
declare -a iams=('IMAGE' 'IMAGE' 'MESSAGE-GLOBIOM' 'AIM' 'AIM' 'GCAM4' 'GCAM4' 'REMIND-MAGPIE' 'REMIND-MAGPIE') # 9 scenarios

em=$1

# When running with --array, the environment variable $SLURM_ARRAY_TASK_ID is
# set uniquely for each run
tid=$SLURM_ARRAY_TASK_ID

# Get the index of the iam for this particular run.
iam_id=$tid
iam_dir=${iam_dirs[iam_id]}
iam=${iams[iam_id]}


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
