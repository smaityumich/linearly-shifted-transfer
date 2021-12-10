#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=25
#SBATCH --mem=120gb
#SBATCH --time=30:00:00
#SBATCH --account=<account>
#SBATCH --mail-type=NONE
#SBATCH --mail-user=<mail>
#SBATCH --partition=standard
Rscript ~/projects/posterior-drift/UKBB/codes/R/RF_gini.R
