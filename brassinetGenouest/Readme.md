# Genouest jobs

## Batch job

check [slurm job](./slurm_genouest.sh)

```shell
sbatch --mem=20G slurm_genouest.sh
```
### Check job

```shell
squeue -u ofilangi
```

## Interactive job


```shell
srun --mem=20G --pty bash
srun --mem=20G --cpus-per-task=8 --pty bash
```

### Set environment

```shell
. /local/env/envconda.sh
export PATH=$HOME/bin:$PATH
conda activate /home/genouest/inra_umr1349/ofilangi/sbt_env
```

### Example with blanc dataset

```shell
FILES="/groups/arch_igepp/metabolomics/MassSpectrometerOutputFile/brassimet/202111/Racines/MzXML*Neg/Brassinet_Racine_blanc-2_Neg_01_8540.mzXML"
java -cp ../assembly/pack.jar fr.inrae.metabolomics.p2m2.MainDetection $FILES
```

