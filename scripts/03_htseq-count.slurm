#!/bin/bash
#SBATCH --partition=dev # partition to submit to
#SBATCH --job-name="htseq" # Job name
# #SBATCH --array=1-74
#SBATCH --nodes=1 # single node, anything more than 1 will not run
#SBATCH --ntasks=2 # equivalent to cpus, stick to around 20 max on gc64, or gc128 nodes
#SBATCH --mem-per-cpu=4000 # in MB, memory pool all cores, default is 2GB per cpu
#SBATCH --time=0-03:00:00  # expected time of completion in hours, minutes, seconds, default 1-day
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

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/software/anaconda3/23.1.0/lssc0-linux/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/software/anaconda3/23.1.0/lssc0-linux/etc/profile.d/conda.sh" ]; then
        . "/software/anaconda3/23.1.0/lssc0-linux/etc/profile.d/conda.sh"
    else
        export PATH="/software/anaconda3/23.1.0/lssc0-linux/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

conda activate /share/malooflab/Packages/Conda_Envs/HTseq2.0.5

## Set up Environment and variables

cd /share/malooflab/Julin/git/S_tort-vernalization/intermediate/bams

source ~/.bashrc
conda activate 

# echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

## get counts

htseq-counts \
  --format=bam \
  --order=pos \
  --max-reads-in-buffer=10000000 \
  --idattr=Parent \
  yo1-50_v2_Aligned.sortedByCoord.out.bam wl2-21-i2_Aligned.sortedByCoord.out.bam \
  /share/malooflab/ref_genomes/S_diversifolious/Sdiv.2023HiFiasm/HiFiasm_S.div.small.gff3 > counts.tsv



## Run Stats

end=`date +%s`
runtime=$((end-start))
echo $runtime seconds to completion

