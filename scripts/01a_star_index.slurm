#!/bin/bash
#SBATCH --partition=dev # partition to submit to
#SBATCH --job-name="STAR index" # Job name
#SBATCH --nodes=1 # single node, anything more than 1 will not run
#SBATCH --ntasks=1 # equivalent to cpus, stick to around 20 max on gc64, or gc128 nodes
#SBATCH --exclusive
#SBATCH --mem-per-cpu=4000 # in MB, memory pool all cores, default is 2GB per cpu
#SBATCH --time=0-01:00:00  # expected time of completion in hours, minutes, seconds, default 1-day
#SBATCH --output=sb_%A_Star-index.out # File to which STDOUT will be written
#SBATCH --error=sb_%A_Star-index.err # File to which STDERR will be written
#SBATCH --mail-user=jnmaloof@ucdavis.edu
#SBATCH --mail-type=ALL

# This will be run once for a single process
# Script to generate genome index files for STAR aligner

/bin/hostname

start=`date +%s`

## Load required modules

module load star/2.7.10b

## set up environment

cd /share/malooflab/ref_genomes/S_diversifolious/Sdiv.2023HiFiasm

## Run STAR

STAR --runMode genomeGenerate \
     --runThreadN 1 \
     --genomeDir STAR_index \
     --genomeFastaFiles HiFiasm_S.div.fasta \
     --sjdbGTFfile HiFiasm_S.div.small.gff3 \
     --sjdbGTFtagExonParentTranscript Parent \
     --sjdbOverhang 149 \
     --genomeSAindexNbases 13
     

  
## Run stats

end=`date +%s`
runtime=$((end-start))
echo $runtime seconds to completion

