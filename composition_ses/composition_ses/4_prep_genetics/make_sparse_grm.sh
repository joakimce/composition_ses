#!/bin/bash
#SBATCH --job-name=grm_sparse
#SBATCH --account=p805
#SBATCH --time=1:00:00
#SBATCH --partition=bigmem
#SBATCH --mem-per-cpu=25GB
#SBATCH --cpus-per-task=3
cd /ess/p805/data/durable/projects/joakim/SES_composition/data/gcta/
/cluster/projects/p805/rosac/software/gcta_1.91.7beta/gcta64 \
--grm ses_GRM \
--make-bK-sparse 0.05 \
--out ses_GRM_sparse
