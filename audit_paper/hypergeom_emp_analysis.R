#====== Data =======
library(ggplot2)
library(lubridate)

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

ggplot(prop.data, aes(x=date, y=proportion, label = required)) + 
 geom_bar(stat="identity", fill="grey", colour="darkblue") + 
 geom_errorbar(aes(ymin=low, ymax=upp)) + 
 scale_y_continuous("Proportion of Erroneous documents") +
 scale_x_datetime("Date") +
 geom_text(aes(x=date, y=0.003), size=3) + 
 theme_bw()