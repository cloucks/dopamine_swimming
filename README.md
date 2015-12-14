## Analysis of swimming-induced paralysis (SWIP) in *Caenorhabditis elegans* 

###Summary

Wild-type *C. elegans* are able to swim in a liquid (M9 or water) for an extended period of time, while mutants that have excessive dopamine signalling, such as *dat-1* animals with defects in the dopamine re-uptake transporter, show high levels of paralysis after 10 minutes. In addition, these animals show decreased thrashing frequency compared to wild-type animals across the entire 10 minutes. This analysis was made to look at novel mutants to assess the possibility of increased dopamine signalling.

###Experimental set-up

I use synchronized L4 animals for this experiment. 10 gravid adults are set on individual plates and killed after 3 hours. Plates are then left at 20 degrees for 48 hours to get L4 animals. On the day of the experiment, 10 worms are picked into 30 ul of tap water in the centre well of a 9-well pyrex plate. Worms are then tracked for 660s on the Multi-worm tracker. Before each run, the pyrex plate is washed with 70% ethanol and dried with a kim-wipe to keep to surface fibre-free (small fibres can look a lot like worms). When setting up the Multi-worm tracker, the region of interest is changed each time to make sure that it is only recognizing the pool of water. This limits the amount of debris that may be detected. The camera is mounted ~23 cm from where the pyrex plate sits. 

The settings for the Multi-worm tracker are as follows:
Object Contrast (%) = 5
Fill Hysteresis (%) = 50
Maximum Object Size (pixels) = 400
Minimum Object Size (pixels) = 20
Object Size Hysteresis (%) = 10

###Analysis set-up

The working directory is a folder with all the files needed for a SWIP project. This folder contains a bin folder containing all of the required programs and a data folder containing individual folders with unique names corresponding to individual experiments. Each of the unique experimental folders in the data folder will contain all of the Multi-worm tracker files as .zip files that you want to use for a particular graph and can be over a few days. Files are taken from the Multi-worm tracker, zipped and put in a new folder.

An example folder set-up is as follows:



