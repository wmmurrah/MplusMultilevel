# load popular2.sav into R.
library(foreign)

pop2 <- read.spss(file="popular2.sav",use.value.labels=FALSE,to.data.frame=TRUE,
                  use.missings=TRUE)
str(pop2)
names(pop2)
summary(pop2)
save(pop2,file="pop2.Rdata")


# basic ML models ---------------------------------------------------------

library(lme4)
library(psychometric)
library(texreg)
mod0 <- lmer(popular ~  (1 |class), data=pop2)
summary(mod0)
screenreg(mod0)
ICC1.lme(popular,class,pop2)
data(popular)
mod1 <- lmer(popular ~ factor(sex) + extrav + texp + (1 + factor(sex) + extrav |class), 
             data=pop2,REML=F)
summary(mod1)
screenreg(mod1)
mod2 <- lmer(popular ~ factor(sex) + extrav + texp + (1 + extrav | class), pop2)
summary(mod2)
vcov(mod2)
mod3 <- lmer(popular ~ factor(sex) + extrav*texp + (1 + extrav | class), pop2)
summary(mod3)

b.mod2 <- lmer(scale(popular) ~ factor(sex) + scale(extrav) + scale(texp) 
               + (1 + scale(extrav) | class), pop2)
summary(b.mod2)


# Residual Diagnostics ----------------------------------------------------

linresid <- resid(b.mod2)
std.resid <- (linresid - mean(linresid))/sd(linresid)
qqnorm(std.resid, pch=20, xlab="Normal score" ,ylab ="std(residuals)", main=" ")
abline(h=0, v=0,lty="dotted")

plot(fitted(b.mod2),std.resid,pch=20,xlab="Predicted Values",ylab="std(residuals)",
     main="Figure 2.2: level 1 standardized residuals plotted against predicted popularity")
abline(h=0,lty="dotted")

plot((pop2$popular - resid(b.mod2)),std.resid,pch=20,xlab="Predicted Values",ylab="std(residuals)",
     main="Figure 2.2: level 1 standardized residuals plotted against predicted popularity")
abline(h=0,lty="dotted")


# Standardization ------------------------------------------------------------

library(lme4)
mod1 <- lmer(popular ~ 1 + (1 | class),pop2)
summary(mod1)
mod2 <- lmer(popular ~ extrav + (1 + extrav| class),pop2)
summary(mod2)
mod3 <- lmer(popular ~ scale(extrav,scale=F) + (scale(extrav,scale=F)|class),pop2, REML=F)
summary(mod3)
mod4 <- lmer(popular ~ scale(extrav) + (scale(extrav)|class),pop2, REML=F)
summary(mod4)



# Interaction -------------------------------------------------------------

mod1 <- lmer(popular ~ sex + extrav + texp + (extrav |class),pop2)
summary(mod1)
mod2 <- lmer(popular ~ sex + extrav + texp + extrav*texp + (extrav | class), pop2)
summary(mod2)

mod3 <- lmer(popular ~ sex + scale(extrav,scale=F)*scale(texp,scale=F) +
  (1 + scale(extrav,scale=F) | class), pop2)
summary(mod3)


# Plot Interaction --------------------------------------------------------


wjap.pttx <- lm(wjap2~tx1+wjap1+tx1:wjap1,data=scmimc)
colors <- ifelse(scmimc$tx1==1,"black","gray")
wjap.x <- plot(scmimc$wjap1,scmimc$wjap2, xlab="Applied Problems Pre-test",ylab="Applied Problems Post-test",
               col=colors,pch=20)
wjap.x <- curve(cbind(1,1,x,1*x)%*%coef(wjap.pttx),add=TRUE,col="black")
curve(cbind(1,0,x,0*x)%*%coef(wjap.pttx),add=TRUE,col="gray")


