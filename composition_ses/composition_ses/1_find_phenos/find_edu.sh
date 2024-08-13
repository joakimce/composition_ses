#!/bin/bash
#SBATCH --job-name=find_edu
#SBATCH --account=p805
#SBATCH --partition=bigmem
#SBATCH --time=10:00:00
#SBATCH --nodes=2
#SBATCH --mem-per-cpu=32GB
#SBATCH --cpus-per-task=4

# SLURM settings
source /cluster/bin/jobsetup
module purge
set -o errexit

module load R/4.0.0-foss-2020a 

Rscript --no-save --no-restore /ess/p805/data/durable/projects/joakim/SES_composition/scripts/1_find_phenos/find_edu.R 