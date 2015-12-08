## Written by: Catrina M. Loucks
## September 1, 2015

## This is the driver script that will call modulars scripts to attack each chunk
## of the problem: this software will allow you to assess the swimming-induced paralysis 
## (SWIP) phenotype using data from the Multi-worm tracker (Swierczek et al., 2011). 
##
##
## Set working directory to project's root directory
##
## Requires the following input from the user:
##		$1:	directory that data is in 
##		$2: gigabytes of memory to be used to run Choreography (dependent upon
##			the machine you are using
##		$3: absolute path to chore.jar (offline analysis program Choreography)
##	
	
##zip files if necessary
##cd $1	
##for foldername in *; do cd $foldername; zip ../$foldername *; cd ..; done

## Call choreography to analyze the MWT data. This must be done for each plate (i.e. each 
## .zip folder). Choreography output options here ask for reversals occurring within 0.5 s 
## of a stimulus and speed over the duration of the entire experiment (averaged over all 
## the worms on the plate).

for zipfolder in *.zip; do java -Xmx$2g -jar $3 --shadowless -p 0.027 -N all -o Dlk --plugin Reoutline::despike --plugin Respine $zipfolder; done

## need to create a large file containing all data files with 
## data, plate name and strain name in each row
##grep -r '[0-9]' $(find ./data -name '*.dat') > merged.file
for filename in $(find . -name '*.dat'); do grep -H '[0-9]' $filename >> swip.dat; done
cd ../..

## create figure (Reversal probability versus stimulus number, plotting 95% confidence 
## interval) and do stats (Logistic regression comparing initial reversal probability and
## final reversal probability between groups).
rscript bin/SWIP_CL.R $1

