library(ggplot2)

x <- rnorm(50)
y <- 1.5 + 1.2*x + rnorm(50)

lm(y~x)
data <- data.frame(x,y)

expr1 <- expression("y==alpha_{0}")
_0+alpha_1x)
# expr1 <- expression("Spearman's"~rho == 0.34)

cairo_pdf("regression.pdf")

ggplot(data, aes(x=x,y=y)) + geom_point(size=2) + 
 geom_smooth(method="lm", size=1.5)+
 geom_abline(intercept = 1.5, slope = 1.3, col="red", size=1.75) +
  annotate("text", x = 0.5, y =-2, 
           parse = T, label = "y==alpha[0]~+~alpha[1]*x", size=9, colour="red") +
  annotate("text", x = -0.25, y =4.5, 
           parse = T, label = "y==a[0]~+~a[1]*x", size=9, colour="blue") +
 theme_bw()


dev.off()