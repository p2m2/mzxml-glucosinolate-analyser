## Genouest

[doc](https://help.genouest.org/usage/slurm/)

```bash
srun --mem=20G --pty bash
. /local/env/envconda.sh
export PATH=$HOME/bin:$PATH
conda activate /home/genouest/inra_umr1349/ofilangi/sbt_env
sbt assembly
```

```bash
#!/bin/bash
#SBATCH --job-name=mzxml_gluco
#SBATCH --chdir=/home/genouest/inra_umr1349/ofilangi/workspace/mzxml-glucosinolate-analyser
#SBATCH --output=out.txt
#SBATCH --ntasks=1

. /local/env/envconda.sh
export PATH=$HOME/bin:$PATH
conda activate /home/genouest/inra_umr1349/ofilangi/sbt_env
java -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainClustering /groups/arch_igepp/metabolomics/MassSpectrometerOutputFile/brassimet/202111/Feuilles/MzXML\ Neg/Brassinet\ F\ 125\ A\ 1500\ uL\ 0.14\ ug\ Api\ Neg_01_7879.mzXML
```

```bash
sbatch --mem=50G script.sh
squeue -u ofilangi
scontrol show job 11038276
```
