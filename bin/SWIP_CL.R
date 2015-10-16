##main function

main <- function() {
  
  ##using function to extract column names
  parsed.data  <- extract.col(read.table("swip.dat"))
  
  
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
  
  ##replace time column (factor) with time as numeric
  parsed.data$time  <- as.numeric(levels(parsed.data$time))[parsed.data$time]
  
  ##bin into time intervals to make it quicker to plot (average speed over every 20s for 11 min)
  
  
  ##get rid of data from 0-40s of the experiment (sometimes the tracker doesn't start tracking 
  ##until 15s into the experiment)
  parsed.data.over20  <- parsed.data[which(parsed.data$time>20),]
  
  ##divide time into intervals (e.g. 40-41) 
  cut1 <- cut(parsed.data.over20$time, breaks=seq(0, max(parsed.data$time), by = 1))
  
  
  ##extract intervals as the min of the interval (e.g. 2 from 2-3)
  time.interval <- as.numeric(str_extract(cut1, "[1-9]{1}[0-9]*"))
  
  parsed.data.tint <- parsed.data.over20
  
  
  ##replace time column with the time interval (lower limit of time period +1 = 
  ##upper limit of time interval)
  parsed.data.tint$time <- time.interval
  
  ##make time a factor again
  parsed.data.tint$time <- as.factor(parsed.data.tint$time)
  
  ##remove rows with NA values caused by binning
  parsed.data.tint <- parsed.data.tint[complete.cases(parsed.data.tint),]
  
  ##find peaks (maxima)
  
  require("splus2R")
  
  ##making a variable for a particular worm
  ##dat3 <- parsed.data.tint[parsed.data.tint$ID == 3 & parsed.data.tint$strain=="dat1",]
  
  ##making variables for individual strains
  ##dat1 <- parsed.data.tint[parsed.data.tint$strain == "dat1",]
  ##N2 <- parsed.data.tint[parsed.data.tint$strain == "N2",]
  ##x3 <- parsed.data.tint[parsed.data.tint$strain == "x3",]
  
  ##extract peaks (maxima for kink)
  parsed.data.tint$kink.peaks <- peaks(parsed.data.tint$kink, span=9)
  
  ##only keep maxima for kink if above user set upper_thresh (e.g. 115 degrees)
  parsed.data.tint$kink.above.thresh <- parsed.data.tint$kink > upper_thresh
  
  parsed.data.tint$good.peak <- parsed.data.tint$kink.peaks + parsed.data.tint$kink.above.thresh 
  parsed.data.tint$good.peak[parsed.data.tint$good.peak == 1] <- 0
  parsed.data.tint$good.peak[parsed.data.tint$good.peak == 2] <- 1
  
  
   


  
  ##summarise over time periods
  
  
  ##using daply to get multiple data frames based on a splitting factor
  ##x <- daply(df, .(splitting_variable), function(x)return(x))
  
  
  library(plyr)
  ##sum of kink peaks over each strain for each ID for each time period
  kink.peaks.tint.sum <- ddply(parsed.data.tint,.(strain,time,ID),summarise,kink=sum(good.peak, na.rm=TRUE))
  
  ##average over each strain for each time period
  kink.peaks.tint.mean <- ddply(kink.peaks.tint.sum,.(strain,time),summarise,kink.mean=(mean(kink)))
  
  ##making bigger blocks of 20s for plotting
  kink.peaks.tint.mean$time <- as.numeric(kink.peaks.tint.mean$time)
  
  cut2 <- cut(kink.peaks.tint.mean$time, breaks=seq(min(kink.peaks.tint.mean$time), max(kink.peaks.tint.mean$time), by = 20))
  
  time.interval <- as.numeric(str_extract(cut2, "[1-9]{1}[0-9]*"))
  kink.peaks.tint.mean$time <- time.interval
  kink.peaks.tint.mean <- kink.peaks.tint.mean[complete.cases(kink.peaks.tint.mean),]
  kink.peaks.tint.mean$time <- as.numeric(kink.peaks.tint.mean$time)
  kink.peaks.tint.mean20 <- ddply(kink.peaks.tint.mean,.(strain,time),summarise,kink.mean20=(mean(kink.mean)))
  
  
  
  
  
  ##plotting kink at 20s intervals
  g20  <- ggplot(kink.peaks.tint.mean20, aes(x = time, y = kink.mean20, colour = strain)) +
    geom_line() + geom_point() +
    labs(x="Time", y="Kink")
  
  g20
  
}

