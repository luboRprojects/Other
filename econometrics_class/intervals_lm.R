library(dplyr)
library(ggplot2)
library(ggthemes)

set.seed(123)
x <- rnorm(30, mean=10, sd=3)
y <- 2 + 1.5*x + rnorm(30, sd=5)

fit.lm <- lm(y~x)

plot(y~x)
abline(coef(fit.lm))

names(fit.lm)
summary(fit.lm)

sd(fit.lm$residuals) # Incorrect!

# Standard Deviation of Residuals
summary(fit.lm)$sigma 
df.fit <- length(x)-2
sd.res <- sqrt(sum((y - predict(fit.lm))^2)/(df.fit))

# Compute Confidence Intervals for Conditional Means:
y.hat <- predict(fit.lm)

pvar.x <- sum( (x-mean(x))^2 )

pred.mean.l <- y.hat - t.quant * sd.res * (1/length(x) + (x-mean(x))^2 /  pvar.x)
pred.mean.u <- y.hat + t.quant * sd.res * (1/length(x) + (x-mean(x))^2 /  pvar.x)

pred.point.l <- y.hat - t.quant * sd.res * (1 + 1/length(x) + (x-mean(x))^2 /  pvar.x)
pred.point.u <- y.hat + t.quant * sd.res * (1 + 1/length(x) + (x-mean(x))^2 /  pvar.x)


data <- data.frame(x, y, y.hat, pred.mean.l, pred.mean.u, pred.point.l, pred.point.u) %>%
 arrange(x)

data %>% ggplot(aes(x=x, y=y) ) + 
 geom_point() + 
 geom_line(aes(y=y.hat)) + 
 geom_ribbon(aes(ymin=pred.mean.l, ymax=pred.mean.u, fill = "red"), alpha=0.2) +
 geom_ribbon(aes(ymin=pred.point.l, ymax=pred.point.u, fill = "blue"), alpha=0.1)  + 
 scale_fill_identity(name = "Type of Interval", guide = "legend", 
  labels = c("Prediction", "Confidence")) + 
 theme_bw()


