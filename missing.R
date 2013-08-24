np.mod <- lm(popular ~ sex + extrav,pop2)
formula <- popular ~ sex + extrav
n.class <- max(pop2$class)
tp.mod <- function(formula){
  tp.list <- list(NA)
  for (i in 1:n.class){
    mod.name <- paste("tpmod",i,sep="")
    mod <- summary(lm(formula,pop2,subset=pop2$class==i))
    tp.list[[i]] <- (mod.name = mod)
  }
  return(tp.list)
}

library(lme4)
library(texreg)
popular.m <- popmis$popular
popmis$extrav <- popdata$extrav

pop2$popular.m <- popular.m
mod <- lmer(popular ~ factor(sex) + texp + (1 | school),popdata)
lwmod <- lmer(popular.m ~ factor(sex) + texp + (1 | school),popmis)
screenreg(list(mod,lwmod))

ini <- mice(popmis, maxit = 0)
pred <- ini$pred
pred["popular", ] <- c(0, -2, 0, 2, 1, 2, 0)
imp <- mice(popmis, meth = c("", "", "2l.norm", "", "",
                             "", ""), pred = pred, maxit = 1, seed = 71152)
mod.mi <- with(imp,lm(popular ~ sex + texp + (1 + texp)))

mi <- (pool(mod.mi)
round(summary(mi),2)
       
       
comp <- pop2[,c("popular","popular.m")]
       
       
densityplot(imp)
       
popdata$sex <- recode(popdata$sex, "'girl'=1;'boy'=0")