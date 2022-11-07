### Genouest

srun --mem=20G --pty bash
srun --mem=20G --cpus-per-task=8 --pty bash

sbatch --mem=20G script.sh

. /local/env/envconda.sh
export PATH=$HOME/bin:$PATH
conda activate /home/genouest/inra_umr1349/ofilangi/sbt_env


squeue -u ofilangi