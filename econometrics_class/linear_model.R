t <- 1:7
y <- c(12, 14, 15, 16, 18, 20, 21)

plot(y~t, type="l",lwd=2, main="Linear Growth Model")
points(y~t, type="p", lwd=4)
abline(lm(y~t), col="red", lwd=2)

pdf("linear_model.pdf")
dev.off()