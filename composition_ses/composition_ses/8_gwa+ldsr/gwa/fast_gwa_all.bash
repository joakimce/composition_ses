cd /ess/p805/data/durable/projects/joakim/SES_composition/scripts/8_ldsr/

names=("" "edu" "occ" "inc" "wea")

for i in {1..4}
do
echo '#!/bin/bash
#
# Job name:
#SBATCH --job-name=gwas_'${names[$i]}'
#
#Project:
#SBATCH --account=p805
#SBATCH --partition=bigmem
#SBATCH --time=72:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=20GB
#SBATCH --cpus-per-task=16
inp="/ess/p805/data/durable/projects/joakim/SES_composition/data/gcta/"
out="/ess/p805/data/durable/projects/joakim/SES_composition/data/gwas/"
cd ${inp}
/cluster/projects/p805/software/gcta/gcta-1.94.1 \
--fastGWA-mlm \
--bfile /cluster/projects/p805/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--grm-sparse ses_GRM_sparse \
--pheno phenos_gen.txt \
--mpheno '$i' \
--qcovar q_covars.txt \
--covar n_covars.txt \
--est-vg HE \
--threads 10 \
--out ${out}'${names[$i]}'' > gwas_${names[$i]}.sh
done

names=("" "edu" "occ" "inc" "wea")
for i in {1..4}
do
sbatch gwas_${names[$i]}.sh
done 
