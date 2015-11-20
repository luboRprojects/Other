library(xts)
library(dplyr)
library(lubridate)

# Define time-period
t <- seq(from=-5*pi, to=5*pi, by = 0.05)

#--- Generate Cycle
y.c <- sin(t) * 20

#--- Generate Trend
y.t <- 50 + 1.5*t

#--- Generate Seasonality
y.s <- rep( rep(1.5*c(-8, 4, 9, -5), each=4),  times=ceiling(length(t)/16))[seq(length(t))]

#--- Generate Residuals
y.r <- 5*rnorm(length(t))

#--- Artificial Data 
data.ts <- data.frame(y.c, y.s, y.t, y.r)
data.ts$y <- apply(data.ts, 1, sum)

ts.index <- dmy(
 paste0(
  rep(1, times=length(t)), "/", 
  rep(1:12, times=ceiling(length(t)/12))[seq(length(t))], "/", 
  rep( (2014-ceiling(length(t)/12))  : 2015, each=12)[seq(length(t))] ) 
)

rownames(data.ts) <- ts.index
data.ts <- as.xts(data.ts)

plot(data.ts)
plot(data.ts$y)

# Constant seasonality:
data.ts$quarter <- ifelse(
 month(index(data.ts)) <= 3, 1, 
  ifelse( month(index(data.ts)) <= 6, 2, 
   ifelse( month(index(data.ts)) <= 9, 3, 4)  
  )
 )

grand.mean <- mean(data.ts$y)

season.effect <- data.ts %>% as.data.frame() %>%
 group_by(quarter) %>%
 summarise(
  value  = mean(y), 
  effect = mean(y) - grand.mean)

head(data.ts)













#------	DEV	----------
y = ts(x$Used, start=c(2011, yday("2011-11-01")), frequency=365)


