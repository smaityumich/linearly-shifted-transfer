#!/bin/bash
#SBATCH --output=logs/op-sim_%A_%a.out
#SBATCH --array=0-99
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=20gb
#SBATCH --time=1:00:00
#SBATCH --account=<account>
#SBATCH --mail-type=NONE
#SBATCH --mail-user=<your-mail>
#SBATCH --partition=standard
echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
Rscript ~/projects/posterior-drift/sim/transfer.R $SLURM_ARRAY_TASK_ID
