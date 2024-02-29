#!/bin/bash
#SBATCH --partition=production # partition to submit to
#SBATCH --job-name="htseq" # Job name
#SBATCH --array=1-350
#SBATCH --nodes=1 # single node, anything more than 1 will not run
#SBATCH --ntasks=1 # equivalent to cpus, stick to around 20 max on gc64, or gc128 nodes
#SBATCH --mem-per-cpu=4000 # in MB, memory pool all cores, default is 2GB per cpu
#SBATCH --time=0-08:00:00  # expected time of completion in hours, minutes, seconds, default 1-day
#SBATCH --output=htseq_%A_%a.out # STDOUT
#SBATCH --error=htseq_%A_%a.err # STDERR
#SBATCH --mail-user=jnmaloof@ucdavis.edu # does not work yet
#SBATCH --mail-type=ALL # does not work yet
# This will be run once for a single process

/bin/hostname

start=`date +%s`

## modules

module load anaconda3/23.1.0

## Conda set up

aklog

source ~/.bashrc

conda activate /share/malooflab/Packages/Conda_Envs/HTseq2.0.5

## Set up Environment and variables

cd /share/malooflab/Julin/git/S_tort-vernalization/intermediate/bams

source ~/.bashrc
conda activate /share/malooflab/Packages/Conda_Envs/HTseq2.0.5

echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

bamfile=`sed "${SLURM_ARRAY_TASK_ID}q;d" ../bams_to_count.txt`

echo "bamfile $bamfile"

sample=$(basename $bamfile | grep -o -E "[a-z]{2}[a-z0-9]-[0-9]{2}[-_][iv][0-9]")

echo "sample $sample"

## get counts

htseq-count \
  --format=bam \
  --order=pos \
  --stranded=no \
  --max-reads-in-buffer=10000000 \
  --idattr=Parent \
  $bamfile \
  /share/malooflab/ref_genomes/S_diversifolious/Sdiv.2023HiFiasm/HiFiasm_S.div.small.gff3 > ../counts/$sample_counts.tsv



## Run Stats

end=`date +%s`
runtime=$((end-start))
echo $runtime seconds to completion

