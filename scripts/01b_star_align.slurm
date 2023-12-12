#!/bin/bash
#SBATCH --partition=production # partition to submit to
#SBATCH --job-name="mapping_array" # Job name
#SBATCH --array=1-2388
#SBATCH --nodes=1 # single node, anything more than 1 will not run
#SBATCH --ntasks=2 # equivalent to cpus, stick to around 20 max on gc64, or gc128 nodes
#SBATCH --mem-per-cpu=4000 # in MB, memory pool all cores, default is 2GB per cpu
#SBATCH --time=0-01:00:00  # expected time of completion in hours, minutes, seconds, default 1-day
#SBATCH --output=mapping_array_%A_%a.out # STDOUT
#SBATCH --error=mapping_array_%A_%a.err # STDERR
#SBATCH --mail-user=jnmaloof@ucdavis.edu # does not work yet
#SBATCH --mail-type=ALL # does not work yet
# This will be run once for a single process

/bin/hostname

start=`date +%s`

## Load required modules

module load star/2.7.10b
module load samtools/1.17

## Set up Environment and variables

cd /share/malooflab/Julin/git/S_tort-vernalization/intermediate/bams

echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

read1=`sed "${SLURM_ARRAY_TASK_ID}q;d" ../R1_fastqs.txt`

read2=`echo $read1 | sed s/_R1_/_R2_/`

echo "read1 $read1"
echo "read2 $read2"

sample=$(basename $read1 | grep -o -E "[a-z]{2}[a-z0-9]-[0-9]{2}[-_][iv][0-9]")

echo "sample $sample"

## mapping

STAR --runMode alignReads \
  --runThreadN 2 \
  --genomeDir /share/malooflab/ref_genomes/S_diversifolious/Sdiv.2023HiFiasm/STAR_index \
  --readFilesIn ${read1} ${read2}   --readFilesCommand gunzip -c \
  --outFileNamePrefix "${sample}_" --outSAMtype BAM SortedByCoordinate \
  --outReadsUnmapped Fastx \

time samtools index ${sample}.bam


## Run Stats

end=`date +%s`
runtime=$((end-start))
echo $runtime seconds to completion

