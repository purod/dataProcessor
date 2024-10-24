#!/bin/bash -l

#$ -cwd
#$ -pe orte 1 
#$ -o job-$JOB_ID.stdout
#$ -e job-$JOB_ID.stderr
#$ -N vcf2genotype
#$ -l mem_requested=200g

perl vcf2genotype.pl RE-HEADERED_AnswerALS_866.anno_and_geno.20210923.vcf "annovar_output" "deleterious" 0.05
