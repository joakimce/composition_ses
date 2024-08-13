#!/bin/bash
#SBATCH --job-name=greml_wea
#SBATCH --account=p805
#SBATCH --time=100:00:00
#SBATCH --mem-per-cpu=20G
#SBATCH --cpus-per-task=5
inp="/tsd/ess/p805/data/durable/projects/joakim/SES_composition/data/gcta/"
out="/tsd/ess/p805/data/durable/projects/joakim/SES_composition/results/"
cd ${inp}
/cluster/projects/p805/rosac/software/gcta_1.91.7beta/gcta64 \
--grm ea_025 \
--reml \
--pheno phenos_gen.txt \
--mpheno 4 \
--qcovar qcovars_dist.txt \
--covar covar_sex.txt \
--out ${out}4
