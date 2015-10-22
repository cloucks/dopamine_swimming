merged.file <- read.table("merged.file")

##main function

main <- function() {
  
  ##using function to extract column names
  data <- extract.col(read.table("swip.dat"))
  
  
  ##call script to look at kink and/or crab
  plot.kink.crab(data)
}

extract.col <- function(data){
  ## split up column V1 into date, plate, time and strain 
  library(stringr)
  date <- str_extract(data$V1, "[0-9]{8}")
  plate <- str_extract(data$V1, "[0-9]{8}_[0-9]{6}")
  time <- str_extract(data$V1, "[0-9]+[.][0-9]+")
  strain <- str_extract(data$V1,"[A-Za-z]+[-]?[0-9]?")
  
  
  
  ## combine new columns with merged file
  data <- cbind(date, plate, time, strain, data[,2:dim(data)[2]])
  
  
  ##rename columns  
  colnames(data) <- c("date", "plate", "time", "strain", "ID", "kink", "crab")
  
  return(data)
  
}


plot.kink.crab <- function(data) {
  
  ##remove lines with NA values
  data <- data[complete.cases(data),]
  
  ##replace time column (factor) with time as numeric
  data$time  <- as.numeric(levels(data$time))[data$time]
  
  ##get rid of data from 0-40s of the experiment (sometimes the tracker doesn't    ##start tracking until 15s into the experiment)
  data  <- data[which(data$time>40),]
  
  ##make test tables for single worms of different strains
  ##N2 <- parsed.data.over40[parsed.data.over40$ID == 1 & parsed.data.over40$strain=="N2",]
  ##N2$kink.peaks <- peaks(N2$kink, span=9)
  ##N2$crab.peaks <- peaks(N2$crab, span=9)
  
  ##divide time into intervals (e.g. 40-41) 
  ##cut1 <- cut(data$time, breaks=seq(0, max(data$time), by = 1))
  cut2 <- cut(data$time, breaks=seq(0, max(data$time), by = 20))
  
  
  ##extract intervals as the min of the interval (e.g. 2 from 2-3)
  time.interval <- as.numeric(str_extract(cut2, "[1-9]{1}[0-9]*"))
  
  
  ##replace time column with the time interval (lower limit of time period +1 = 
  ##upper limit of time interval)
  data$time <- time.interval
  
  ##make time a factor again
 data$time <- as.factor(data$time)
  
  ##find peaks (maxima)
  
  require("splus2R")
  
  ##making a variable for a particular worm
  ##dat3 <- parsed.data.tint[parsed.data.tint$ID == 3 & parsed.data.tint$strain=="dat1",]
  
  ##making variables for individual strains
  ##dat1 <- parsed.data.tint[parsed.data.tint$strain == "dat1",]
  ##N2 <- parsed.data.tint[parsed.data.tint$strain == "N2",]
  ##x3 <- parsed.data.tint[parsed.data.tint$strain == "x3",]
  
  ##extract peaks (maxima for kink and crab)
  data$kink.peaks <- peaks(data$kink, span=9)
  data$crab.peaks <- peaks(data$crab, span=9)
  
  ##summarise over time periods
  ##using daply to get multiple data frames based on a splitting factor
  ##x <- daply(df, .(splitting_variable), function(x)return(x))
  
  
  library(plyr)
  ##sum of kink peaks over each strain for each ID for each time period
  kink.peaks.sum <- ddply(data,.(strain,time,ID),summarise,kink=sum(kink.peaks, na.rm=TRUE))
  
  ##average over each strain for each time period
  kink.peaks.mean <- ddply(kink.peaks.sum,.(strain,time),summarise,kink.mean=(mean(kink))/20)
 
 kink.peaks.mean.total <- ddply(kink.peaks.mean,.(strain),summarise,kink.mean.total=mean(kink.mean))
  
  ##repeat for crab
  ##sum of kink peaks over each strain for each ID for each time period
  crab.peaks.sum <- ddply(data,.(strain,time,ID),summarise,crab=sum(crab.peaks, na.rm=TRUE))
  
  ##average over each strain for each time period
  crab.peaks.mean <- ddply(crab.peaks.sum,.(strain,time),summarise,crab.mean=(mean(crab))/20)
  
 crab.peaks.mean.total <- ddply(crab.peaks.mean,.(strain),summarise,crab.mean.total=mean(crab.mean))
  
  
  ##plotting kink
  
  require(ggplot2)
  g  <- ggplot(kink.peaks.mean, aes(x = time, y = kink.mean, colour = strain)) +
    geom_point() +
    labs(x="Time", y="Kink")
  
  g
  
  ##repeat for crab
  
  g  <- ggplot(crab.peaks.mean, aes(x = time, y = crab.mean, colour = strain)) +
    geom_point() +
    labs(x="Time", y="Crab")
  
  g
  
}
