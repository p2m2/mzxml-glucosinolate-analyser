#!/bin/bash
#SBATCH --job-name=mzxmlGlucosinolateJob
#SBATCH --chdir=/home/genouest/inra_umr1349/ofilangi/workspace/mzxml-glucosinolate-analyser
#SBATCH --output=out.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8

FILES="/groups/arch_igepp/metabolomics/MassSpectrometerOutputFile/brassimet/202111/Racines/MzXML*Neg/*.mzXML"

. /local/env/envconda.sh
export PATH=$HOME/bin:$PATH
conda activate /home/genouest/inra_umr1349/ofilangi/sbt_env
java -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainDetection $FILES
