#!/bin/bash
#SBATCH --job-name=grm_ses
#SBATCH --account=p805
#SBATCH --partition=bigmem
#SBATCH --time=100:00:00
#SBATCH --nodes=2
#SBATCH --cpus-per-task=18
#SBATCH --mem-per-cpu=200GB

cd /ess/p805/data/durable/projects/joakim/SES_composition/data/gcta/
/cluster/projects/p805/joakim/software/gcta_1.91.7beta/gcta64 \
--bfile /cluster/projects/p805/snpdata/MoBaPsychGen_v1_1m \
--make-grm \
--keep ses.keep \
--maf 0.01 \
--threads 10 \
--out ses_GRM


