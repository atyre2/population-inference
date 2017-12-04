
library(rasterVis)
library(gridExtra)
library(fields)
library(raster)


rihompp <- function(Beta0, Beta1, x) {
                    lambda.max <- max(exp(Beta0 + Beta1*x[]))
                    n <- rpois(n = 1, lambda = lambda.max)
                    s <- cbind(runif(n,extent(x)[1],extent(x)[2]),runif(n,extent(x)[3],extent(x)[4]))
                    lambda <- exp(Beta0 + Beta1*extract(x,s))
                    thin <- rbinom(n = n, size = 1, p = lambda/lambda.max)
                    s <- s[which(thin == 1),]
                    return(s)
                                      }

# Define global plotting characteristics
ramp.gray <- colorRampPalette(c("white","gray30"))(2000)  # raster color palette

# Simulate data
set.seed(3220)
x1 <- raster(, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA,nrows=200,ncol=200)
knots <- aggregate(x1,4)
D <- rdist(rasterToPoints(x1),rasterToPoints(knots))
x1[] <-  scale(exp(-D/0.015)%*%rnorm(dim(D)[2]))
loc1 <- rihompp(4,1,x1)

#Plot data
image(x1,asp=TRUE,xaxt="n",yaxt="n",xlab="",ylab="",col=ramp.gray)
points(loc1,pch=4,col="gold")

# MLE estimation using IPP likelihood 
ll.ipp <- function(beta,X.1,X.0){sum(X.1%*%beta) - mean(exp(X.0%*%beta)) - log(factorial(dim(X.1)[1]))}
nll.ipp <- function(par){beta <- par;-ll.ipp(beta,X.1,X.0)} 

# Using all possible background points
X.1 <- model.matrix(~extract(x1,loc1))
X.0 <- model.matrix(~x1[])
est1 <- optim(c(0,0),fn=nll.ipp,hessian=TRUE)
est1$par # Truth 4,1
est1$value # Exact likelihood value

# Using a much smaller sample of backgroung points
X.0 <- model.matrix(~sample(x1,size=1000,replace=TRUE))
est2 <- optim(c(0,0),fn=nll.ipp,hessian=TRUE)
est2$par # Truth 4,1
est2$value # Approximate likelihood value. As the size of the sample goes to ininity it will approach the extact value



# Using all possible background points
df1 <- rbind(data.frame(y=1,x=extract(x1,loc1)),
            data.frame(y=0,x=x1[]))
m1 <- glm(y~x,data=df1,family=binomial(link=logit))
m2 <- glm(y~x,data=df1,family=poisson(link=log)) 
coef(m1) # Should be close to est1$par for coefficient for x
coef(m2) # Should be close to est1$par for coefficient for x
logLik(m1)
logLik(m2)

# Using smaller random sample of background points
sample <- sample(which(df1$y==0),size=1000,replace=TRUE)
df2 <- df1[c(which(df1$y==1),sample),]
m3 <- glm(y~x,data=df2,family=binomial(link=logit))
m4 <- glm(y~x,data=df2,family=poisson(link=log)) 
coef(m3) # Should be close to est1$par for coefficient for x
coef(m4) # Should be close to est1$par for coefficient for x
logLik(m3)
logLik(m4)


