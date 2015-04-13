# Date: Oct 9th 2014
# By Ziang Jia
# W4240
# HomeWork 03 <Due: Oct 14th 2014>
# Problem 4

### preworks
#==============#
set.seed(1)

# a)
#=============#
x <- rnorm(100)

# b)
#=============#
eps <- rnorm(100,0,0.25^(1/2))

# c)
#=============#
y <- -1+0.5*x+eps

# d)
#=============#
plot(x,y,main="Scatter plot of X versus Y")

# e)
#=============#
lm.y <- lm(y~x)
summary(lm.y)

# f)
#=============#
plot(x,y, main="Figure. Regression line versus Population line")
abline(lm.y,col="red")
abline(-1,0.5,col="blue")
legend(1,-1.5,c("Regression line","Popupation line"), title="Lengend",col=c("red","blue"),lty=c(1,1))

# g)
#=============#
z<-x^2
lm.yp <- lm(y~x+z)
summary(lm.yp)
plot(x,y, main="Figure. Regression line versus Quadratic regression line")
abline(lm.y,col="red")
xp = sort(x)
yp <--0.97164+0.50858*xp-0.05946*xp^2
lines(xp,yp,type="l")

# h)
#=============#
par(mfrow=c(2,2))
for(i in 1:4){
  x <- rnorm(100)
  eps <- rnorm(100,0,0.5/i)
  y <- -1+0.5*x+eps
  plot(x,y,main="Regression line with reduced variance")
  lm.y <- lm(y~x)
  abline(lm.y,col="red")
  abline(-1,0.5,col="blue")
  legend(0,-1.5,c("Regression line","Popupation line"), title="Lengend",col=c("red","blue"),lty=c(1,1))
}

#i)
#=============#
par(mfrow=c(2,2))
for(i in 1:4){
  x <- rnorm(100)
  eps <- rnorm(100,0,0.25*i)
  y <- -1+0.5*x+eps
  plot(x,y,main="Regression line with reduced variance")
  lm.y <- lm(y~x)
  abline(lm.y,col="red")
  abline(-1,0.5,col="blue")
  legend(0,-1.5,c("Regression line","Popupation line"), title="Lengend",col=c("red","blue"),lty=c(1,1))
}

# j)
#=============#
for(i in 1:3){
  x <- rnorm(100)
  eps <- rnorm(100,0,0.25*i)
  y <- -1+0.5*x+eps
  lm.y <- lm(y~x)
  cf<-confint(lm.y,level=0.95)
  print(cf)
}





