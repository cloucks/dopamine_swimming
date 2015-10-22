##main function

main <- function() {
  
  ##using function to extract column names
  parsed.data  <- extract.col(read.table("swip.dat"))
  
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
  library(stringr)
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
  parsed.data <- parsed.data[complete.cases(parsed.data),]
  
  ##remove lines with length values under 0.2000 (trying to exclude anything that isn't a worm, when worms bend for swimming their length becomes ~0.4000-0.5000, while straight worms are ~ 0.7000-0.8000), used a more conservative/smaller length because some strains are smaller than others
  parsed.data <- parsed.data[parsed.data$length>0.2000, ]
  
  ##replace time column (factor) with time as numeric
  parsed.data$time  <- as.numeric(levels(parsed.data$time))[parsed.data$time]
  
  
  ##get rid of data from 0-20s of the experiment (sometimes the tracker doesn't start tracking 
  ##until 15s into the experiment)
  parsed.data.over20  <- parsed.data[which(parsed.data$time>20),]
  
  ##find peaks (maxima)
  
  require("splus2R")
  
  ##making a variable for a particular worm
  ##dat3 <- parsed.data.tint[parsed.data.tint$ID == 3 & parsed.data.tint$strain=="dat1",]
  
  ##making variables for individual strains
  ##dat1 <- parsed.data.tint[parsed.data.tint$strain == "dat1",]
  ##N2 <- parsed.data.tint[parsed.data.tint$strain == "N2",]
  ##x3 <- parsed.data.tint[parsed.data.tint$strain == "x3",]
  
  ##extract peaks (maxima for kink)
  parsed.data.over20$kink.peaks <- peaks(parsed.data.over20$kink, span=9)
  
  ##only keep maxima for kink if above user set upper_thresh (e.g. 115 degrees)
  parsed.data.over20$kink.above.thresh <- parsed.data.over20$kink > upper_thresh
  

  parsed.data.over20$good.peak <- parsed.data.over20$kink.peaks + parsed.data.over20$kink.above.thresh 
  parsed.data.over20$good.peak[parsed.data.over20$good.peak == 1] <- 0
  parsed.data.over20$good.peak[parsed.data.over20$good.peak == 2] <- 1
  
  
  
  ##summarise over time periods
  
  
  ##using daply to get multiple data frames based on a splitting factor
  ##x <- daply(df, .(splitting_variable), function(x)return(x))
  
  
  ##break up by time periods by adding a new column (every 5 seconds)
  parsed.data.over20$time.interval <- cut(parsed.data.over20$time, breaks=seq(0, max(parsed.data.over20$time), by = 5))
  
  ##change time interval to lower number of time interval (20-25=20)
  parsed.data.over20$time.interval <- as.numeric(str_extract(parsed.data.over20$time.interval, "[1-9]{1}[0-9]*"))
  
  library(plyr)
  
  ##make a new dataframe that adds a new column with the sum of good peaks in every time interval
  parsed.data.over20.tint <- ddply(parsed.data.over20,.(plate,strain,ID,time.interval),transform,sum.good.peak = sum(good.peak))
  
  ##remove lines with NA values
  parsed.data.over20.tint <- parsed.data.over20.tint[complete.cases(parsed.data.over20.tint),]
  
  ##test
  parsed.data.over20.summ <- ddply(parsed.data.over20.tint,.(plate,strain,ID,time.interval),transform,min.time=min(time), 
                                   max.time=max(time))
  
  parsed.data.over20.summ2 <- ddply(parsed.data.over20.summ,.(plate,strain,ID,time.interval,min.time,max.time),summarise,sum.good.peak=mean(sum.good.peak))
  
  
    ##summarise to show rate of kinks per worm per time period
    worm.time.span.kinks <- ddply(parsed.data.over20.summ2,.(plate,strain,ID,time.interval),summarise,
                                  kinks=ifelse(min.time<(time.interval+0.05) && max.time>(time.interval+4.95), sum.good.peak/5,NA)) 
  
  
  ##summarise to show rate of kinks per worm per time period
  ##worm.time.span.kinks <- ddply(parsed.data.over20.tint,.(plate,strain,ID),summarise,
                                ##min.time=min(time), 
                                ##max.time=max(time),
                                ##kinks30=ifelse(min(time)<30 && max(time)>35, sum.good.peak/5,NA))
  
  
  ##summarise to show mean of kinks (thrashing frequency) and standard deviation for a time period
  worm.time.span.kinks2 <- ddply(worm.time.span.kinks,.(strain,time.interval),summarise,mean.kinks=mean(kinks,na.rm=TRUE),stdev=stdev(kinks,na.rm=TRUE), N=length(which(!is.na(kinks))))
  
  worm.time.span.kinks2$sem <- worm.time.span.kinks2$stdev/sqrt(worm.time.span.kinks2$N)
  
  worm.time.span.kinks2$upper <- worm.time.span.kinks2$mean.kinks + worm.time.span.kinks2$sem
  worm.time.span.kinks2$lower <- worm.time.span.kinks2$mean.kinks - worm.time.span.kinks2$sem
  
  
  ##plotting kink for each 5 sec block for all worms existing for the entire block
  g  <- ggplot(worm.time.span.kinks2, aes(x = time.interval, y = mean.kinks, colour = strain)) +
    geom_line() + geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    labs(x="Time", y="Kink")
  
  g
  
}

