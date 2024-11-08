# https://freakonometrics.hypotheses.org/77996

install.packages('NeuralNetTools')
install.packages('scatterplot3d')
library(nnet)
library(NeuralNetTools)
library(scatterplot3d)

set.seed(12345)
n <- 100
x <- c(runif(n),1+runif(n),2+runif(n))
y <- rep(c(0,1,0),each=n)

minmax <- function(z) (z-min(z))/(max(z)-min(z))
xm <- minmax(x)
df <- data.frame(x=xm,y=y)

plot(df$x,rep(0,3*n),col=1+df$y)

sigmoid <- function(x) 1 / (1 + exp(-x))

set.seed(1234)
model_nnet <- nnet(y~x,size=2,data=df)

w <- neuralweights(model_nnet)
x1 <- cbind(1,df$x)%*%w$wts$"hidden 1 1"
x2 <- cbind(1,df$x)%*%w$wts$"hidden 1 2"
b <- w$wts$`out 1`
plot(sigmoid(x1),sigmoid(x2),col=1+df$y)

abline(a=-b[1]/b[3],b=-b[2]/b[3])

set.seed(12345)
n <- 100
x <- c(runif(n),1+runif(n),2+runif(n),3+runif(n))
y <- rep(c(0,1,0,1),each=n)
xm <- minmax(x)
df <- data.frame(x=xm,y=y)
plot(df$x,rep(0,4*n),col=1+df$y)

set.seed(321)
model_nnet <- nnet(y~x,size=3,data=df)
w <- neuralweights(model_nnet)
x1 <- cbind(1,df$x)%*%w$wts$"hidden 1 1"
x2 <- cbind(1,df$x)%*%w$wts$"hidden 1 2"
x3 <- cbind(1,df$x)%*%w$wts$"hidden 1 3"
b <- w$wts$`out 1`

s3d <- scatterplot3d(x=sigmoid(x1),
                    y=sigmoid(x2), z=sigmoid(x3),color=1+df$y)

set.seed(123)
n <- 500
x1 <- runif(n)*3-1.5
x2 <- runif(n)*3-1.5
y <- (x1^2+x2^2)<=1
x1m <- minmax(x1)
x2m <- minmax(x2)
df <- data.frame(x1=x1m,x2=x2m,y=y)
plot(df$x1,df$x2,col=1+df$y)

set.seed(1234)
model_nnet <- nnet(y~x1+x2,size=3,data=df)
w <- neuralweights(model_nnet)
x1 <- cbind(1,df$x1,df$x2)%*%w$wts$"hidden 1 1"
x2 <- cbind(1,df$x1,df$x2)%*%w$wts$"hidden 1 2"
x3 <- cbind(1,df$x1,df$x2)%*%w$wts$"hidden 1 3"
b <- w$wts$`out 1`

s3d <- scatterplot3d(x=sigmoid(x1), y=sigmoid(x2), z=sigmoid(x3),
                    color=1+df$y)
