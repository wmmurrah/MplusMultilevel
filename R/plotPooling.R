## Read & clean the data
# get radon data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon
library ("arm")

# load ecslk
df <- pop2

popular <- df$popular
class <- df$class


n <- length(popular)
y <- popular

# get county index variable
class.num<- as.vector(class)
uniq <- unique(class)
J <- length(uniq)
class <- rep (NA, J)
for (i in 1:J){
  class[class.num==uniq[i]] <- i
}

# no predictors
ybarbar = mean(y)

sample.size <- as.vector (table (class))
sample.size.jittered <- sample.size*exp(runif(J, -.1, .1))
cl.mns = tapply(y,class,mean)
cl.vars = tapply(y,class,var)
cl.sds = mean(sqrt(cl.vars[!is.na(cl.vars)]))/sqrt(sample.size)
cl.sds.sep = sqrt(tapply(y,class,var)/sample.size)

# varying-intercept model, no predictors

pop.data <- list ("n", "J", "y", "class")
pop.inits <- function (){
  list (a=rnorm(J), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
pop.parameters <- c ("a", "mu.a", "sigma.y", "sigma.a")

mlm.pop.nopred <- bugs (pop.data, pop.inits, pop.parameters,  n.chains=3, 
                          "pop.multilevel.nopred.bug", bugs.directory="C:/Program Files/WinBUGS14",
                          working.directory=NULL, clearWD=TRUE, n.iter=10, debug=TRUE)

## Figure 12.1 (a)

par(mfrow=c(1,2))
plot (sample.size.jittered, cl.mns, cex.lab=.9, cex.axis=1,
      xlab="sample size in class j",
      ylab="avg. popularity in class j",
      pch=20, mgp=c(1.5,.5,0),
      ylim=c(0,10), yaxt="n", xaxt="n")
axis (1, 1:30, cex.axis=.9, mgp=c(1.5,.5,0))
axis (2, seq(0,10), cex.axis=.9, mgp=c(1.5,.5,0))
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
         cl.mns[j] + c(-1,1)*cl.sds[j], lwd=.5)
  #         cl.mns[j] + c(-1,1)*mean(cl.sds[!is.na(cl.sds)]), lwd=.5)
}
abline(h=5.08)
title("No pooling",cex.main=.9, line=1)
#abline(h=ybarbar)
points(sample.size.jittered[36],cty.mns[36],cex=4)

## Figure 12.1 (b)
library(lme4)
mlm.pop.nopred <- lmer(popular ~ 1 +( 1 | class), pop2)
mlm.coef <- as.vector(coef(mlm.pop.nopred)[1])
mlm.se <- as.vector(se.coef(mlm.pop.nopred))
coefs <- mlm.coef$class
se <- as.data.frame(mlm.se$class)
names(coefs) <- "coef"
names(se) <- "se"
plot (sample.size.jittered, coefs$coef, cex.lab=.9, cex.axis=1,
      xlab="sample size in class j",
      ylab="avg. popularity in class j",
      pch=20, mgp=c(1.5,.5,0),
      ylim=c(0,10), yaxt="n", xaxt="n")
axis (1, 1:30, cex.axis=.9, mgp=c(1.5,.5,0))
axis (2, seq(0,10), cex.axis=.9, mgp=c(1.5,.5,0))
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
         coefs$coef[j] + c(-1,1)*se$se[j],
         lwd=.5)
}
abline(h=5.08)
points(sample.size.jittered[36],mlm.radon.nopred$median$a[36],cex=4)#,col="red")
title("Multilevel model",cex.main=.9, line=1)







