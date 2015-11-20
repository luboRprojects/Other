library(ggplot2)

#===== Modified Exponential Curve ======

mexp.data <- read.table("mexp_data.txt", sep="\t", header=TRUE)
mexp.fit <- nls(y ~ k+a0*a1^t, start = list(k=11600, a0=-10600, a1=0.5), data = mexp.data)

mexp.fun <- function(t){
 coef(mexp.fit)[1] + coef(mexp.fit)[2] * coef(mexp.fit)[3]^t
}

mexpsum.fun <- function(t){
 11620.0683 + -10659.17949 * 0.776917183^t
}

ggplot(data = mexp.data, aes(x=t, y=y)) + 
 geom_point(colour="red", size=5) + 
 #stat_function(fun=mexp.fun, size=1.0, colour="darkblue") + 
 annotate("text", x=7.6, y=4900, label="Modified Exponential Trend:", col="black") + 
 annotate("text", x=8, y=4400, label="y=11620 + -10659.18 * 0.777^t", col="blue") + 
 stat_function(fun=mexpsum.fun, size=1.0, colour="blue") + theme_bw()

cairo_pdf("modif_exp.pdf")


#===== Logistic Trend ======
log.data <- read.table("log_data.txt", sep="\t", header=TRUE)

log.fit <- nls(y ~ 1/(k+a0*a1^t), start = list(k=0.00005, a0=0.001, a1=0.5), data = log.data )

log.fun <- function(t){
 1/(coef(log.fit)[1] + coef(log.fit)[2] * coef(log.fit)[3]^t)
}

log.sums <- function(t){
 1/(0.0000520143 + 0.083985511 * 0.4953460852640^t)
}

ggplot(data=log.data, aes(x=t, y=y)) + 
 geom_point(colour="red", size=5) + 
 #stat_function(fun=log.fun, size=1.0, colour="darkblue") + 
 stat_function(fun=log.sums, size=1.5, colour="blue") + 
 annotate("text", x=3, y=17000, label="Logistic Trend:", col="black") + 
 annotate("text", x=5, y=16000, label="y=1/(c + 0.083 * 0.495^t)", col="blue") + 
 annotate("text", x=4.1, y=14500, label="c=0.0000520143", col="black") + 
 annotate("text", x=3.7, y=13500, label="1/c=19225.48", col="black") + 
theme_bw()

#cairo_pdf()

#===== Gompertz Trend ======
gomp.data <- read.table("gomp_data.txt", sep="\t", header=TRUE)
gomp.fit <- nls(y ~ k*a0^a1^t, start = list(k=7.6, a0=0.0249, a1=0.7137), data = gomp.data)
log.fit2 <- nls(y ~ 1/(k+a0*a1^t), start = list(k=0.0005, a0=0.001, a1=0.5), data = gomp.data )

gomp.fun <- function(t){
 coef(gomp.fit)[1] * coef(gomp.fit)[2] ^ coef(gomp.fit)[3]^t
}

gomp.sum <- function(t){
 7.675502442 * 0.024932533 ^ 0.713701154 ^ t
}

log.fun2 <- function(t){
 1/(coef(log.fit2)[1] + coef(log.fit2)[2] * coef(log.fit2)[3]^t)
}

ggplot(data = gomp.data, aes(x=t, y=y)) + 
 geom_point(colour="red", size=5) + 
 #stat_function(fun=gomp.fun, size=1.0, colour="darkblue") + 
 stat_function(fun=gomp.sum, size=1.5, colour="blue") + 
 annotate("text", x=10, y=3, label="Gomperzt Trend:", col="blue") + 
 annotate("text", x=12, y=2.5, label="y=7.675 * 0.0249 ^ 0.71 ^ t", col="blue") +  
 stat_function(fun=log.fun2, size=1.5, colour="green") + 
 annotate("text", x=10, y=1.75, label="Logistic Trend:", col="green") + 
 annotate("text", x=13, y=1.25, label="y=1/(0.1305 + 1.9979 * 0.6192 ^ t)", col="green") +
theme_bw()

cairo_pdf("gomp_logis.pdf")

