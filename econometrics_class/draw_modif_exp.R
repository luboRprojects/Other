library(ggplot2)
x <- 0:12

# Modif Exponential Trend

mexp <- function(x){
 12000-10000*0.7^x
}

mexp2 <- function(x){
 12000-8000*0.5^x
}

ggplot(data.frame(x=1:12), aes(x=x) ) + 
scale_x_continuous(limits=c(0,12)) + 
scale_y_continuous(limits=c(0, 15000), breaks = seq(from=0, to=12000, by=2000) ) +
stat_function(fun=mexp, size=2, colour="red") + 
stat_function(fun=mexp2, size=2, colour="blue") + 
annotate("text", x=9, y=9000, label="y=12000-10000*0.7^x", col="red") + 
annotate("text", x=2.5, y=13000, label="y=12000-8000*0.5^x", col="blue") + 
annotate("text", x=6, y=15000, label="Modified Exponential Function", col="black") + 
#annotate("text", x=6, y=15000, label="Modifikovaná exponenciála", col="black") + 
theme_bw()

cairo_pdf("modif_expon_en.pdf")

dev.off()