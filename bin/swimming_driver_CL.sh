## Written by: Catrina M. Loucks
## September 1, 2015

## This is the driver script that will call modulars scripts to attack each chunk
## of the problem: this software will allow you to assess the swimming-induced paralysis 
## (SWIP) phenotype using data from the Multi-worm tracker (Swierczek et al., 2011). 
## 
## Before running this script, set the working directory to project's root directory.
##
## Requires the following input from the user:
##		$1: directory that data is in (must be a folder with only the .zip files from the tracker)
##		$2: gigabytes of memory to be used to run Choreography (dependent upon
##			the machine you are using
##		$3: absolute path to chore.jar (offline analysis program Choreography)
##	
## Example usage of this script from the Bash Shell:
## bash bin/swimming_driver_CL.sh data/test/ 4 /Users/catrinaloucks/Documents/PhD/EFHC1/dopamine_swimming/bin/Chore.jar
	
## change to the directory that data is in
cd $1	

## Call choreography to analyze the MWT data. This must be done for each plate (i.e. each 
## .zip folder)
for zipfolder in *.zip; do java -Xmx$2g -jar $3 --shadowless -p 0.027 -N all -o Dlk --plugin Reoutline::despike --plugin Respine $zipfolder; done

## need to create a large file containing all data files with 
## data, plate name and strain name in each row
for filename in $(find . -name '*.dat'); do grep -H '[0-9]' $filename >> swip.dat; done

## create figure (Reversal probability versus stimulus number, plotting 95% confidence 
## interval) and do stats (Logistic regression comparing initial reversal probability and
## final reversal probability between groups).
rscript ../../bin/SWIP_CL.R swip.dat


