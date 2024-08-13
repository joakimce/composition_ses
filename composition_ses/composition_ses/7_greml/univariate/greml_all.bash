cd /ess/p805/data/durable/projects/joakim/SES_composition/scripts/7_greml/


names=("" "edu" "occ" "inc" "wea")
for i in {1..4}
do
echo '#!/bin/bash
#SBATCH --job-name=greml_'${names[$i]}'
#SBATCH --account=p805
#SBATCH --time=100:00:00
#SBATCH --mem-per-cpu=20G
#SBATCH --cpus-per-task=5
inp="/ess/p805/data/durable/projects/joakim/SES_composition/data/gcta/"
out="/ess/p805/data/durable/projects/joakim/SES_composition/results/"
cd ${inp}
/cluster/projects/p805/rosac/software/gcta_1.91.7beta/gcta64 \
--grm ses_GRM_025 \
--reml \
--pheno phenos_gen.txt \
--mpheno '$i' \
--qcovar q_covars_dist.txt \
--covar n_covars.txt \
--out ${out}'$i'' > greml_${names[$i]}.sh
done



names=("" "edu" "occ" "inc" "wea")
for i in {1..4}
do
sbatch greml_${names[$i]}.sh
done 
