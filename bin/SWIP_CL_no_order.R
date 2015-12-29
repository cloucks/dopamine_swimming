#Tidied up by Catrina Loucks, 2015-11-09
#Written by Catrina Loucks, 2015

#download and load required libraries

if(!require("stringr")) {
  install.packages("stringr")
  library(stringr)
}
if(!require("splus2R")) {
  install.packages("splus2R")
  library(splus2R)
}
if(!require("plyr")) {
  install.packages("plyr")
  library(plyr)
}
if(!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require("binom")) {
  install.packages("binom")
  library(binom)
}

##main function

main <- function() {
  
  args <- commandArgs(trailingOnly = TRUE)
  swip.data <- args[1]
  
  ##using function to extract column names
  parsed.swip  <- extract.col(read.table(swip.data))
  
  ##counting thrashes over time
  thrashes.swip <- count.thrashes(parsed.swip)
  
  ##plot thrashing frequency over time 
  plot.thrashing.frequency(thrashes.swip)
  
  ##plot number of worms swimming at 10min
  plot.fraction.swimming(thrashes.swip)
}



# function for choreography output with column names -----------------------------------

extract.col <- function(data){
  ## split up column V1 into date, plate, time and strain 
  date <- str_extract(data$V1, "[0-9]{8}")
  plate <- str_extract(data$V1, "[0-9]{8}_[0-9]{6}")
  time <- str_extract(data$V1, ":[0-9]+[.][0-9]+")
  time <- str_extract(time, "[0-9]+[.][0-9]+")
  strain <- str_extract(data$V1,"[A-Za-z]+[-]?[0-9]*[A-Za-z]*[0-9]*")
  
  ## combine new columns with merged file
  new.data <- cbind(date, plate, time, strain, data[,2:dim(data)[2]])
  
  #re-order strain factor so that plots are in appropriate order
  #new.data$strain <- factor(new.data$strain, levels=c("N2","dat-1","gk","fkh-8","tm","ow47","r1","er1","res1","res2","N2res1","N2res2","N2frag1","N2frag2"), order=TRUE)
  
  ##rename columns  
  colnames(new.data) <- c("date", "plate", "time", "strain", "ID", "length", "kink")
  
  return(new.data)
  
}

# function for counting thrashes over time --------------------------------


count.thrashes <- function(data){

##new code from Evan's analysis

upper_thresh=80 ##set the local kink maxima threshold (degrees)
lower_thresh=50 ##set the local kink minima threshold (degrees)
switch_time=0.5 ##set the maximum time between maxima and minima (seconds)

  
  ##remove lines with NA values
  ##parsed.swip <- parsed.swip[complete.cases(parsed.swip),]
  parsed.swip <- data[complete.cases(data),]
  
  
  ##replace time column (factor) with time as numeric
  parsed.swip$time  <- as.numeric(levels(parsed.swip$time))[parsed.swip$time]
  
  
  ##get rid of data from 0-20s of the experiment (sometimes the tracker doesn't start tracking 
  ##until 15s into the experiment)
  parsed.swip  <- parsed.swip[which(parsed.swip$time>20),]
  
  ##find peaks (maxima)
  
  ##extract peaks (maxima for kink)
  parsed.swip$kink.peaks <- peaks(parsed.swip$kink, span=7)
  
  ##only keep maxima for kink if above user set upper_thresh (e.g. 80 degrees)
  parsed.swip$kink.above.thresh <- parsed.swip$kink > upper_thresh
  

  parsed.swip$good.peak <- parsed.swip$kink.peaks + parsed.swip$kink.above.thresh 
  parsed.swip$good.peak[parsed.swip$good.peak == 1] <- 0
  parsed.swip$good.peak[parsed.swip$good.peak == 2] <- 1
  
  ##calculate minima by using maxima calculations of negative values of kink
  parsed.swip$kink.minima <- -(parsed.swip$kink)
  parsed.swip$kink.minima <- peaks(parsed.swip$kink.minima, span=7)
  
  #only keep minima for kink if below user set upper_thresh (e.g. 50 degrees)
  
  parsed.swip$kink.below.thresh <- parsed.swip$kink < lower_thresh
  
  parsed.swip$good.minima <- parsed.swip$kink.minima + parsed.swip$kink.below.thresh 
  parsed.swip$good.minima[parsed.swip$good.minima == 1] <- 0
  parsed.swip$good.minima[parsed.swip$good.minima == 2] <- 1
  
  ##summarise over time periods
  
  ##break up by time periods by adding a new column (every 5 seconds)
  parsed.swip$time.interval.5s <- cut(parsed.swip$time, breaks=seq(0, max(parsed.swip$time), by = 5))
  
  ##change time interval to lower number of time interval (20-25=20)
  parsed.swip$time.interval.5s <- as.numeric(str_extract(parsed.swip$time.interval.5s, "[1-9]{1}[0-9]*"))
  
  ##remove lines with NA values (rows at 660+ seconds can't figure out the time interval and are thus NA)
  parsed.swip <- parsed.swip[complete.cases(parsed.swip),]
  
  ##remove lines with length values under 0.2000 (trying to exclude anything that isn't a worm, when worms bend for swimming their length becomes ~0.4000-0.5000, while straight worms are ~ 0.7000-0.8000), used a more conservative/smaller length because some strains are smaller than others
  
  swip.animal <- ddply(parsed.swip,.(plate,strain,ID),
                   transform,
                   mean.length=mean(length))
  
  swip.animal <- swip.animal[swip.animal$mean.length > 0.2000,]
  
  ##make two new columns that calculate the minimum and maximum time worms were tracked in a given time period (did worms persist for the entire 5s?)
  swip.5s <- ddply(swip.animal,.(plate,strain,ID,time.interval.5s),
                   transform,
                   min.time=min(time), 
                   max.time=max(time))
  
  ##only count a maximum if it's followed by a minimum in 0.5s
  
  swip.max.min <- swip.5s[swip.5s$good.peak == 1 | swip.5s$good.min == 1,]
  #swip.no.threshold <- parsed.swip[parsed.swip$kink.peaks == TRUE | parsed.swip$kink.minima == TRUE,]
  
  swip.max.min.time.diff <- swip.max.min %>%
  mutate(min.max = time - lag(time, default = 0)) %>%
  mutate(real.max = good.peak == 1 & lead(good.minima == 1) & lead(min.max) < switch_time) 
  
  ##make a new dataframe that summarises the average number of peaks for each ID according to strain and plate and output the min and max times
  swip.5s <- ddply(swip.max.min.time.diff,.(plate,strain,ID,time.interval.5s,min.time,max.time),summarise,sum.good.peak=sum(real.max))
  
  
    ##summarise to show kinks/s per worm per time period so that only worms that persist the entire 5s time period are used
    swip.5s.persisting <- ddply(swip.5s,.(plate,strain,ID,time.interval.5s),summarise, kinks=ifelse(min.time<(time.interval.5s+0.05) && max.time>(time.interval.5s+4.95), sum.good.peak/5,NA)) 
    
    swip.5s.persisting <- swip.5s.persisting[complete.cases(swip.5s.persisting),] 
}
  

# function to plot thrashing frequency in 5s blocks -----------------------
  
  plot.thrashing.frequency <- function(data){
    
    ##summarise to show mean of kinks (thrashing frequency) and standard deviation for a time period per strain (don't include lines with na)
    swip.5s.persisting.summ <- ddply(data,.(strain,time.interval.5s),summarise,mean.kinks=mean(kinks,na.rm=TRUE),stdev=stdev(kinks,na.rm=TRUE), N=length(which(!is.na(kinks))), median.kinks=median(kinks,na.rm=TRUE))
    
    ##calculate sem
    swip.5s.persisting.summ$sem <- swip.5s.persisting.summ$stdev/sqrt(swip.5s.persisting.summ$N)
    
    swip.5s.persisting.summ$upper <- swip.5s.persisting.summ$mean.kinks + swip.5s.persisting.summ$sem
    swip.5s.persisting.summ$lower <- swip.5s.persisting.summ$mean.kinks - swip.5s.persisting.summ$sem
    
  ##plotting kink for each 5 sec block for all worms existing for the entire block
  plot1  <- ggplot(swip.5s.persisting.summ, aes(x = time.interval.5s, y = mean.kinks, colour = strain)) +
    geom_line(size=1) +
    geom_errorbar(alpha=0.4, aes(ymin=lower, ymax=upper)) + ##error bars lighter
    scale_y_continuous(limits=c(0.0, 3.5), breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5), expand = c(0, 0)) + ##Set the y-axis limits to a range from 0 to 1, y-values every 0.5, and remove extra space above and below
    scale_x_discrete(breaks=c(0,100,200,300,400,500,600)) + ## x-axis values every 100s
    scale_color_discrete(name="") + ## remove legend title
    theme(legend.key = element_blank()) + ## remove boxes around legend values
    theme_classic() + ## remove background to make it white
    labs(x="Time", y="Thrashing frequency (Hz)")
    
  plot1
  
  ggsave(plot1, file="Thrashing_frequency_throughout.pdf", useDingbats=FALSE, height=4, width=6, units="in", dpi=300)
  

#make a new dataframe with only the time period at 600s (10 min)
  #kinks.600 <- swip.5s.persisting[swip.5s.persisting$time.interval.5s == 600,]
kinks.600 <- data[data$time.interval.5s == 600,]

##make a new data frame showing thrashes at 10 min

plot2 <- ggplot(kinks.600, aes(x=strain, y=kinks)) +
  geom_point(position=position_jitter(width=0.1)) +
  geom_boxplot(alpha=0.2, outlier.shape = NA) +
  scale_y_continuous(limits=c(0.0, 3.5), breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5), expand = c(0, 0)) + ##Set the y-axis limits to a range from 0 to 1, y-values every 0.5, and remove extra space above and below
  scale_color_discrete(name="") + ## remove legend title
  theme(legend.key = element_blank()) + ## remove boxes around legend values
  theme_classic() + ## remove background to make it white
  labs(x="Strain", y="Thrashing frequency at 10 min (Hz)")
plot2

ggsave(plot2, file="Thrashing_frequency_10min.pdf", useDingbats=FALSE, height=4, width=6, units="in", dpi=300)

}


# function to plot thrashing frequency in 5s blocks -----------------------

plot.fraction.swimming <- function(data){

##graph fraction swimming throughout entire experiment
  
  ##fraction.swimming.throughout <- ddply(swip.5s.persisting,.(strain,time.interval.5s),summarise,fraction.swimming=sum(kinks!=0.0)/length(kinks), N=length(kinks))

fraction.swimming.throughout <- ddply(data,.(strain,time.interval.5s),summarise,fraction.swimming=sum(kinks!=0.0)/length(kinks), sum.thrashes= sum(kinks!=0.0), N=length(kinks))

conf_int <- binom.confint(fraction.swimming.throughout$sum.thrashes, fraction.swimming.throughout$N, 
                          methods = "exact")

## Add these confidence intervals to the data frame
fraction.swimming.throughout$conf_int_lower <- conf_int$lower
fraction.swimming.throughout$conf_int_upper <- conf_int$upper

plot3 <- ggplot(fraction.swimming.throughout, aes(x=as.numeric(time.interval.5s), y=fraction.swimming, colour=strain)) +
  geom_line(size=1) +
  ##geom_errorbar(alpha=0.4, aes(ymin=conf_int_lower, ymax=conf_int_upper), width=0.5) +
  scale_y_continuous(limits=c(0.0, 1.0), breaks=c(0.0,0.2,0.4,0.6,0.8,1.0), expand = c(0, 0)) + ##Set the y-axis limits to a range from 0 to 1, y-values every 0.5, and remove extra space above and below
  scale_color_discrete(name="") + ## remove legend title
  theme(legend.key = element_blank()) + ## remove boxes around legend values
  theme_classic() + ## remove background to make it white
  labs(x="Strain", y="Proportion swimming at 10 minutes")
plot3

ggsave(plot3, file="Fraction_swimming_throughout.pdf", useDingbats=FALSE, height=4, width=6, units="in", dpi=300)

#fraction swimming at 600s
fraction.swimming.600 <- fraction.swimming.throughout[fraction.swimming.throughout$time.interval.5s == 600,]

plot4 <- ggplot(fraction.swimming.600, aes(x=strain, y=fraction.swimming)) +
  geom_bar(stat="identity", fill="grey") +
  geom_errorbar(aes(ymin=conf_int_lower, ymax=conf_int_upper), width=0.5) +
  scale_y_continuous(limits=c(0.0, 1.0), breaks=c(0.0,0.2,0.4,0.6,0.8,1.0), expand = c(0, 0)) + ##Set the y-axis limits to a range from 0 to 1, y-values every 0.5, and remove extra space above and below
  scale_color_discrete(name="") + ## remove legend title
  theme(legend.key = element_blank()) + ## remove boxes around legend values
  theme_classic() + ## remove background to make it white
  labs(x="Strain", y="Proportion swimming at 10 minutes")

plot4


ggsave(plot4, file="Fraction_swimming_10min.pdf", useDingbats=FALSE, height=4, width=6, units="in", dpi=300)
}

main()