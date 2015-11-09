#Tidied up by Catrina Loucks, 2015-11-09
#Written by Catrina Loucks, 2015

#load required libraries
library(stringr)
library(splus2R)
library(plyr)
library(ggplot2)

##main function

main <- function() {
  
  ##using function to extract column names
  parsed.swip  <- extract.col(read.table("swip.dat"))
  
  ##test
  ##original.parsed.data <- parsed.data
  ##test.data <- parsed.data[1:5000,]
  ##parsed.data <- test.data
  
  ##call script to look at kink 
  plot.kink(parsed.data)
}


##function for creating choreography output file with column names

extract.col <- function(data){
  ## split up column V1 into date, plate, time and strain 
  date <- str_extract(data$V1, "[0-9]{8}")
  plate <- str_extract(data$V1, "[0-9]{8}_[0-9]{6}")
  time <- str_extract(data$V1, ":[0-9]+[.][0-9]+")
  time <- str_extract(time, "[0-9]+[.][0-9]+")
  strain <- str_extract(data$V1,"[A-Za-z]+[-]?[0-9]*[A-Za-z]*[0-9]*")
  
  ## combine new columns with merged file
  new.data <- cbind(date, plate, time, strain, data[,2:dim(data)[2]])
  
  ##rename columns  
  colnames(new.data) <- c("date", "plate", "time", "strain", "ID", "length", "kink")
  
  return(new.data)
  
}

##new code from Evan's analysis

upper_thresh=115 ##set the local kink maxima threshold (degrees)
lower_thresh=65 ##set the local kink minima threshold (degress)
switch_time=0.5 ##set the maximum time between maxima and minima (seconds)



plot.kink <- function(parsed.data) {
  
  ##remove lines with NA values
  parsed.swip <- parsed.swip[complete.cases(parsed.swip),]
  
  ##remove lines with length values under 0.2000 (trying to exclude anything that isn't a worm, when worms bend for swimming their length becomes ~0.4000-0.5000, while straight worms are ~ 0.7000-0.8000), used a more conservative/smaller length because some strains are smaller than others
  ##doesn't work - want to remove IDs that are always below 0.2000
  ##parsed.data <- parsed.data[parsed.data$length>0.2000, ]
  
  ##replace time column (factor) with time as numeric
  parsed.swip$time  <- as.numeric(levels(parsed.swip$time))[parsed.swip$time]
  
  
  ##get rid of data from 0-20s of the experiment (sometimes the tracker doesn't start tracking 
  ##until 15s into the experiment)
  parsed.swip  <- parsed.swip[which(parsed.swip$time>20),]
  
  ##find peaks (maxima)
  
  ##extract peaks (maxima for kink)
  parsed.swip$kink.peaks <- peaks(parsed.swip$kink, span=5)
  
  ##only keep maxima for kink if above user set upper_thresh (e.g. 115 degrees)
  parsed.swip$kink.above.thresh <- parsed.swip$kink > upper_thresh
  

  parsed.swip$good.peak <- parsed.swip$kink.peaks + parsed.swip$kink.above.thresh 
  parsed.swip$good.peak[parsed.swip$good.peak == 1] <- 0
  parsed.swip$good.peak[parsed.swip$good.peak == 2] <- 1
  
  
  
  ##summarise over time periods
  
  ##break up by time periods by adding a new column (every 5 seconds)
  parsed.swip$time.interval.5s <- cut(parsed.swip$time, breaks=seq(0, max(parsed.swip$time), by = 5))
  
  ##change time interval to lower number of time interval (20-25=20)
  parsed.swip$time.interval.5s <- as.numeric(str_extract(parsed.swip$time.interval, "[1-9]{1}[0-9]*"))
  
  ##remove lines with NA values (rows at 660+ seconds can't figure out the time interval and are thus NA)
  parsed.swip <- parsed.swip[complete.cases(parsed.swip),]
  
  ##make a new dataframe that adds a new column with the sum of good peaks in every time interval
  swip.5s <- ddply(parsed.swip,.(plate,strain,ID,time.interval.5s),transform,sum.good.peak = sum(good.peak))
  
  ##make two new columns that calculate the minimum and maximum time worms were tracked in a given time period (did worms persist for the entire 5s?)
  swip.5s <- ddply(parsed.swip,.(plate,strain,ID,time.interval.5s),
                               transform,
                               min.time=min(time), 
                               max.time=max(time))
  
  ##make a new dataframe that summarises the average number of peaks for each ID according to strain and plate and output the min and max times
  swip.5s <- ddply(swip.5s,.(plate,strain,ID,time.interval.5s,min.time,max.time),summarise,sum.good.peak=sum(good.peak))
  
  
    ##summarise to show kinks/s per worm per time period so that only worms that persist the entire 5s time period are used
    swip.5s.persisting <- ddply(swip.5s,.(plate,strain,ID,time.interval.5s),summarise, kinks=ifelse(min.time<(time.interval.5s+0.05) && max.time>(time.interval.5s+4.95), sum.good.peak/5,NA)) 
    
    swip.5s.persisting <- swip.5s.persisting[complete.cases(swip.5s.persisting),]
  
  ##summarise to show mean of kinks (thrashing frequency) and standard deviation for a time period per strain (don't include lines with na)
    swip.5s.persisting.summ <- ddply(swip.5s.persisting,.(strain,time.interval.5s),summarise,mean.kinks=mean(kinks,na.rm=TRUE),stdev=stdev(kinks,na.rm=TRUE), N=length(which(!is.na(kinks))))
  
  ##calculate sem
    swip.5s.persisting.summ$sem <- swip.5s.persisting.summ$stdev/sqrt(swip.5s.persisting.summ$N)
  
    swip.5s.persisting.summ$upper <- swip.5s.persisting.summ$mean.kinks + swip.5s.persisting.summ$sem
    swip.5s.persisting.summ$lower <- swip.5s.persisting.summ$mean.kinks - swip.5s.persisting.summ$sem
  
  
  ##plotting kink for each 5 sec block for all worms existing for the entire block
  plot  <- ggplot(swip.5s.persisting.summ, aes(x = time.interval.5s, y = mean.kinks, colour = strain)) +
    geom_line() + geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    labs(x="Time", y="Kink")
  
  plot
  
  ggsave(plot, file="Figure.pdf", useDingbats=FALSE, height=4, width=6, units="in", dpi=300)
  
}

#extract number of worms paralysed at 10 min

#make a new dataframe with only the time period at 600s (10 min)
kinks.600 <- swip.5s.persisting[swip.5s.persisting$time.interval.5s == 600,]

##make a new data frame summarising worms swimming in the 5 seconds at 10 min/worms that were tracked in the last 5s

swimming.600 <- ddply(kinks.600,.(strain),summarise,fraction.swimming=sum(kinks!=0.0)/length(kinks), N=length(kinks))

plot <- ggplot(swimming.600, aes(x=strain, y=fraction.swimming)) +
  geom_point()
plot

