## Analysis of swimming-induced paralysis (SWIP) in *Caenorhabditis elegans* 

###Summary

Wild-type *C. elegans* are able to swim in a liquid (M9 or water) for an extended period of time, while mutants that have excessive dopamine signalling, such as *dat-1* animals with defects in the dopamine re-uptake transporter, show high levels of paralysis after 10 minutes. In addition, these animals show decreased thrashing frequency compared to wild-type animals across the entire 10 minutes. This analysis was made to look at novel mutants to assess the possibility of increased dopamine signalling.

###Experimental set-up

I use synchronized L4 animals for this experiment. 10 gravid adults are set on individual plates and killed after 3 hours. Plates are then left at 20 degrees for 48 hours to get L4 animals. Before each run, a 9-well pyrex plate is washed with 70% ethanol and dried with a kim-wipe to keep to surface fibre-free (small fibres can look a lot like worms). On the day of the experiment, 10 worms are picked into 30 ul of tap water in the centre well of the pyrex plate. Worms are then tracked for 660s on the Multi-worm tracker.  When setting up the Multi-worm tracker, the region of interest is changed each time to make sure that it is only recognizing the pool of water. This limits the amount of debris that may be detected. The camera is mounted ~23 cm from where the pyrex plate sits. 

The settings for the Multi-worm tracker are as follows:
Object Contrast (%) = 5
Fill Hysteresis (%) = 50
Maximum Object Size (pixels) = 400
Minimum Object Size (pixels) = 20
Object Size Hysteresis (%) = 10

###Analysis set-up

The working directory is a folder with all the files needed for a SWIP project. This folder contains a bin folder containing all of the required programs and a data folder containing individual folders with unique names corresponding to individual experiments. Each of the unique experimental folders in the data folder will contain all of the Multi-worm tracker files as .zip files that you want to use for a particular graph (.zip files can be from a few days). Files are taken from the Multi-worm tracker, zipped and put in a new folder.

An example folder set-up is as follows:

SWIP = working directory
SWIP/bin = folder where all programs are kept and contains:
  -Chore.jar (jar file to run choreography to extract length and kink of animals)
  -swimming_driver_CL.sh (shell script to run from the command line to get graphical output for the SWIP experiments)
  -SWIP_CL.R (R script to run from within the shell script that analyses the data to get the graphs)
SWIP/data = folder where all data are kept in .zip files (in individual folders for individual experiments, e.g., test)

The output is as follows:

The graph that I find the most useful is Thrashing_frequency_throughout. Worms swim with C-shaped bends in liquid that can be extracted as maxima and minima in the kink values extracted by choreography. This graph shows the average number of thrashes per second (Hz) for individual strains. Analysis was binned in 5 second intervals and only worms persisting for the entire 5 seconds were used. Wild-type worms have a high, and relatively constant thrashing frequency, while dat-1 mutants have a lower and steadily decreasing thrashing frequency due to progressive paralysis. Other mutants may show similarities to either strain. I prefer this graph because there are jumps and falls in the thrashing frequency over time.

The other graphs I created are preiliminary, and if you would like to use them, I can help to make them more polished.
  -Thrashing_frequency_10min (boxplot of thrashing frequency at 10 minutes)
  -Fraction_swimming_throughout (fraction of worms showing at least one thrash/5 second bin throughout the 660 seconds)
  -Fraction_swimming_10min (fraction of worms showing at least one thrash/5 second bin at 10 minutes)


