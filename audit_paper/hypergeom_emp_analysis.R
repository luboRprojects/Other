#====== Data =======
library(ggplot2)
library(lubridate)
library(scales)

data.in <- read.csv("data_audit.txt", sep="\t")
data.in$date <- myd(paste0(data.in$month,"/1") )

#----- Analyse -----
prop.test(n=240, x=6)

prop.data <- data.in
for(i in 1:nrow(data.in) ){
 temp <- prop.test(n=data.in[i, 2], x=data.in[i, 3])
  prop.data[i,"proportion"] <- temp$estimate
  prop.data[i,"low"] <- temp$conf.int[1]
  prop.data[i,"upp"] <- temp$conf.int[2]
}

p1 <- ggplot(prop.data, aes(x=date, y=proportion, label = required)) + 
 geom_bar(stat="identity", fill="grey90", colour="darkblue") + 
 geom_point(size=2) + 
 geom_errorbar(aes(ymin=low, ymax=upp), size=0.75) + 
 scale_y_continuous("Proportion of erroneous documents") +
 scale_x_datetime("Date", labels = date_format("%m/%y"),
  limits=c(min(prop.data$date), max(prop.data$date)), breaks = date_breaks("1 month")) +
 geom_text(aes(x=date, y=0.003), size=3) + 
 theme_bw()

p2 <- p1 + 
 geom_rect(aes(xmin=as.POSIXct("2014-05-20 UTC"),
  xmax=as.POSIXct("2014-06-15 UTC"), ymin=0, ymax=0.05),
  fill="grey80", colour="darkblue") +
 geom_rect(aes(xmin=as.POSIXct("2014-06-21 UTC"),
  xmax=as.POSIXct("2014-08-13 UTC UTC"), ymin=0, ymax=0.05),
  fill="grey80", colour="darkblue")
p2

dates <- c(as.POSIXct("2014-06-15 UTC"), 
 as.POSIXct("2014-07-31 UTC") )
reason <- c("REASON","Holiday")

data.caption <- data.frame(date=dates, reason, position = 0.025)

p2 + geom_text(data=data.caption, aes(x=date, y=position, label = reason), angle = 90 )


