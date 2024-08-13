#!/bin/bash
#SBATCH --job-name=grm_025
#SBATCH --account=p805
#SBATCH --time=100:00:00
#SBATCH --partition=bigmem
#SBATCH --mem-per-cpu=50GB
#SBATCH --cpus-per-task=5
cd /ess/p805/data/durable/projects/joakim/SES_composition/data/gcta/
/cluster/projects/p805/joakim/software/gcta_1.91.7beta/gcta64 \
--grm ses_GRM \
--make-grm \
--grm-cutoff 0.025 \
--threads 10 \
--out ses_GRM_025
