# Hyper 
population <- 200
x <- 2
m <- 10
n <- population - m
k <- 5

dhyper(x, m, n, k)


#-------------
population <- 150
x <- 3 # err.doc in sample
k <- 10 # test sample size
m <- 10 # number of err. doc total
n <- population - m # number of OK doc total

pop <- 1:population 

probs <- data.frame(matrix(0, ncol=2, nrow=population ))
for(i in 1:population) {
 m <- pop[i] # number of err. doc total
 n <- population - m # number of OK doc total
 probs[i,1] <- m
 probs[i,2] <- dhyper(x, m, n, k)
}

plot(probs)
probs[which.max(probs[,2]), ]







