x <- rnorm(n=50,mean=2,sd=5)
alpha0 <- 5
alpha1 <- 2
rezid <- rnorm(50,0,3)
y <- alpha0 + alpha1*x + rezid
plot(y~x, col="blue", pch=19, xlim=c(-1, 5))

fit1<-lm(y~x)$coeff
abline(fit1, lwd=2)

coef<-matrix(nrow=100,ncol=2)

for (i in 1:100){
 x <- rnorm(n=50,mean=2,sd=5)
 alpha0 <- 5
 alpha1 <- 2
 rezid <- rnorm(50,0,5)
 y <- alpha0 + alpha1*x + rezid
 fit1 <- lm(y~x)$coeff
 coef[i,1] <- as.numeric(fit1[1])
 coef[i,2] <- as.numeric(fit1[2])
}

plot(y~x, col="blue", pch=19, ylim = c(min(y), max(y+1)), xlim=c(min(x)-1, max(x)+1) ) #min(coef[,1])

for(i in 1:100){
 abline(coef[i, ],col = "grey70")
}


names(coef)<-c("a0","a1")
colMeans(coef)
hist(coef[,1],main="histogram a0",xlab="a0")
hist(coef[,1],main="histogram a0",xlab="a0",prob=TRUE)
curve(dnorm(x, mean=mean(coef[,1]), sd=sd(coef[,1])), add=TRUE)

hist(coef[,2],main="histogram a1",xlab="a1")
hist(coef[,2],main="histogram a1",xlab="a1",prob=TRUE)
curve(dnorm(x, mean=mean(coef[,2]), sd=sd(coef[,2])), add=TRUE)

sd(coef[,1])
sd(coef[,2])

fit<-lm(y~x)
summary(fit)

quantile(coef[,1],probs=c(0.025,0.975))
quantile(coef[,2],probs=c(0.025,0.975))

fit<-lm(y~x)
summary(fit)

plot(coef)
