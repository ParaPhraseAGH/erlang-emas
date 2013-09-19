#/bin/bash
#PBS -A plgkrzywic2013b 
#PBS -N emas-xtrct
#PBS -k n
#PBS -l mem=1024mb
#PBS -l nodes=1:ppn=1

java -jar $EmasRoot/stat-extractor.jar $outputPath fitnessMax:avg populationSum:avg
