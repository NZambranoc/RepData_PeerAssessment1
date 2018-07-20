library(tidyverse)

#### Download and unzip Data from web ####
data_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
down.file <- "./Data/AMD.zip"
data <- "./data/activity.csv"

if (!file.exists(down.file)) { # If data has been already downloadead will use existing file
    
    download.file(data_url, destfile = down.file)
    unzip(zipfile = down.file,exdir = "Data/")
} else { 
    
    cdate <- file.info(data)$ctime
    message(paste0("The file '", basename(data) ,"' has already been downloaded on ",cdate,", loading from disk..."))
    rm(cdate)
    
}

### Load Raw Data to Memory ###

data <- read_csv(data)
data <- na.exclude(x) # remove na.values


#Produce Histogram with the freq. dist. of total Steps by Day
stepsbydate <- summarise(total.steps=sum(steps),.data = group_by(data,date))
with(stepsbydate, qplot(total.steps,binwidth=3000, xlab = "Total Steps taken by date", ylab = "Freq",main = "total number of steps taken each day"))
summarise(total.steps=mean(steps),.data = group_by(data,date)) %>% plot(type="l")


x <- do.call(data.frame,aggregate(steps , data, function(x) c(mn=mean(x),md=median(x))))
sapply(x[c(2,3)], mean)

do.call(data.frame,x[2]) %>% sapply(mean)
x$steps
          



fill



avgstepsinterval <- group_by(data,interval) %>% summarise(total.steps=mean(steps))

ggplot(avgstepsinterval,aes(x=interval,y=total.steps)) +
    geom_line() +
    labs(title = "Average steps taken per Interval",
         subtitle= "Activity Monitoring Data — Oct - Nov 2012 \n(Breaks in Military Time)",
         x="Interval\n(Breaks in Military Time)",
         y="Avg. Steps") +
    scale_x_continuous(breaks=seq(0,2355,100),
                      x labels = seq(0,2355,100) %>% sprintf(fmt = "%04d") %>% strptime(format = "%H%M") %>% format(format="%H:%M"))
print(avgstepsinterval)

rawdata2 <- rawdata


rawdata2$steps <-  if_else(is.na(rawdata$steps),
       avgstepsinterval$total.steps[match(rawdata$interval,avgstepsinterval$interval)],
       1)

rawdata2 %>% filter(interval==0)

