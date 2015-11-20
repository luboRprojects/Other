library(ggplot2)
library(gridExtra)
n <- 70

t <- 1:n
y <- 1 + 0.2*t + 0.3*t^2 + rnorm(10, mean = 0, sd=15)

plot(y~t, type="p")
fit.q <- lm(y ~ t + I(t^2) )
coef(fit.q)

quad.fun <- function(t){
as.numeric(coef(fit.q)[1] + coef(fit.q)[2]*t + coef(fit.q)[3]*t^2)
}

quad.fun(5)

p1 <- ggplot(data.frame(t=1:n,y), aes(x=t, y=y) ) + 
scale_x_continuous(limits=c(0,n)) + 
stat_function(fun=quad.fun, size=2, colour="darkblue")+
geom_point(size=2, col="red") + theme_bw()
p1 # ggsave("quad_func.pdf")


d1 <- c(mean(diff(y) ), diff(y))
d2 <- c(rep(mean(diff(diff(y))), 2),diff(diff(y)))

p2 <- ggplot(data.frame(t=1:n,d1,d2), aes(x=t, y=d1) ) + 
scale_x_continuous(limits=c(0,n), "Time") + 
scale_y_continuous("Differences") + 
geom_smooth(method='lm', alpha=0, size=2)+
annotate("text", x=10, y=70, label="První diference") +
geom_line(size=1.5, col="red") + theme_bw()

p3 <- ggplot(data.frame(t=1:n,d1,d2), aes(x=t, y=d2) ) + 
scale_x_continuous(limits=c(0,n), "Time") + 
geom_smooth(method='lm', alpha=0, size=2) +
annotate("text", x=5, y=70, label="Druhá diference") +
scale_y_continuous("Differences") + geom_line(size=1.5, col="red") + theme_bw()

cairo_pdf("differences.pdf", width=10, height=7)
 grid.arrange(p2, p3, ncol=1)
dev.off()

summary(lm(d1~t))
summary(lm(d2~t))


#----------
fit.l <- lm(y ~ t)
fit.q <- lm(y ~ t + I(t^2) )

anova(fit.l, fit.q)

summary(fit.l)
summary(fit.q)



