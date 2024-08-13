#!/bin/bash
#SBATCH --job-name=greml_occ_inc
#SBATCH --account=p805
#SBATCH --time=100:00:00
#SBATCH --partition=bigmem
#SBATCH --time=100:00:00
#SBATCH --mem-per-cpu=40GB
#SBATCH --cpus-per-task=10
inp="/ess/p805/data/durable/projects/joakim/SES_composition/data/gcta/"
out="/ess/p805/data/durable/projects/joakim/SES_composition/results/"
cd ${inp}
/cluster/projects/p805/rosac/software/gcta_1.91.7beta/gcta64 \
--grm ses_GRM_025 \
--reml-bivar \
--pheno occ_inc.txt \
--qcovar q_covars.txt \
--covar n_covars.txt \
--out ${out}bivariate_greml_occ_inc
