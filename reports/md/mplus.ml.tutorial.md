# MPlus Brownbag Multilevel Modeling Tutorial

## Data:


```r
load("C:/RStudioProjects/MplusTutorials/MplusMultilevel/data/pop2.Rdata")
library(psych)
library(xtable)
library(arm)
library(car)
library(texreg)
library(GGally)
str(pop2, give.attr = FALSE)
```

```
'data.frame':	2000 obs. of  15 variables:
 $ pupil    : num  1 2 3 4 5 6 7 8 9 10 ...
 $ class    : num  1 1 1 1 1 1 1 1 1 1 ...
 $ extrav   : atomic  5 7 4 3 5 4 5 4 5 5 ...
 $ sex      : atomic  1 0 1 1 1 0 0 0 0 0 ...
 $ texp     : atomic  24 24 24 24 24 24 24 24 24 24 ...
 $ popular  : atomic  6.3 4.9 5.3 4.7 6 4.7 5.9 4.2 5.2 3.9 ...
 $ popteach : atomic  6 5 6 5 6 5 5 5 5 3 ...
 $ Zextrav  : num  -0.17 1.414 -0.962 -1.755 -0.17 ...
 $ Zsex     : num  0.989 -1.011 0.989 0.989 0.989 ...
 $ Ztexp    : num  1.49 1.49 1.49 1.49 1.49 ...
 $ Zpopular : num  0.885 -0.128 0.162 -0.272 0.668 ...
 $ Zpopteach: num  0.6691 -0.0431 0.6691 -0.0431 0.6691 ...
 $ Cextrav  : num  -0.215 1.785 -1.215 -2.215 -0.215 ...
 $ Ctexp    : num  9.74 9.74 9.74 9.74 9.74 ...
 $ Csex     : num  0.5 -0.5 0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5 -0.5 ...
```




```r

print(xtable(describe(pop2[, 1:6], interp = F, skew = F)), type = "html")
```

<!-- html table generated in R 2.15.3 by xtable 1.7-1 package -->
<!-- Thu Apr 04 10:02:43 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> var </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> pupil </TD> <TD align="right">   1 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 10.65 </TD> <TD align="right"> 5.97 </TD> <TD align="right"> 11.00 </TD> <TD align="right"> 10.56 </TD> <TD align="right"> 7.41 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 26.00 </TD> <TD align="right"> 25.00 </TD> <TD align="right"> 0.13 </TD> </TR>
  <TR> <TD align="right"> class </TD> <TD align="right">   2 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 50.37 </TD> <TD align="right"> 29.08 </TD> <TD align="right"> 51.00 </TD> <TD align="right"> 50.33 </TD> <TD align="right"> 37.81 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 100.00 </TD> <TD align="right"> 99.00 </TD> <TD align="right"> 0.65 </TD> </TR>
  <TR> <TD align="right"> extrav </TD> <TD align="right">   3 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 5.21 </TD> <TD align="right"> 1.26 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 5.17 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 10.00 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 0.03 </TD> </TR>
  <TR> <TD align="right"> sex </TD> <TD align="right">   4 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> texp </TD> <TD align="right">   5 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 14.26 </TD> <TD align="right"> 6.55 </TD> <TD align="right"> 15.00 </TD> <TD align="right"> 14.40 </TD> <TD align="right"> 8.15 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 25.00 </TD> <TD align="right"> 23.00 </TD> <TD align="right"> 0.15 </TD> </TR>
  <TR> <TD align="right"> popular </TD> <TD align="right">   6 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 5.08 </TD> <TD align="right"> 1.38 </TD> <TD align="right"> 5.10 </TD> <TD align="right"> 5.08 </TD> <TD align="right"> 1.33 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 9.50 </TD> <TD align="right"> 9.50 </TD> <TD align="right"> 0.03 </TD> </TR>
   </TABLE>


## Change 'sex' to 'female' and add Unique ID variable 


```r
names(pop2)[4] <- "female"
pop2$id <- (pop2$class * 100) + pop2$pupil
u.id <- unique(pop2$id)
print(paste("# unique id variables =", length(u.id)))
```

```
[1] "# unique id variables = 2000"
```




```r

print(xtable(describe(pop2[, c(1:6, 16)], interp = F, skew = F)), type = "html")
```

<!-- html table generated in R 2.15.3 by xtable 1.7-1 package -->
<!-- Thu Apr 04 10:02:43 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> var </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> pupil </TD> <TD align="right">   1 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 10.65 </TD> <TD align="right"> 5.97 </TD> <TD align="right"> 11.00 </TD> <TD align="right"> 10.56 </TD> <TD align="right"> 7.41 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 26.00 </TD> <TD align="right"> 25.00 </TD> <TD align="right"> 0.13 </TD> </TR>
  <TR> <TD align="right"> class </TD> <TD align="right">   2 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 50.37 </TD> <TD align="right"> 29.08 </TD> <TD align="right"> 51.00 </TD> <TD align="right"> 50.33 </TD> <TD align="right"> 37.81 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 100.00 </TD> <TD align="right"> 99.00 </TD> <TD align="right"> 0.65 </TD> </TR>
  <TR> <TD align="right"> extrav </TD> <TD align="right">   3 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 5.21 </TD> <TD align="right"> 1.26 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 5.17 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 10.00 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 0.03 </TD> </TR>
  <TR> <TD align="right"> female </TD> <TD align="right">   4 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> texp </TD> <TD align="right">   5 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 14.26 </TD> <TD align="right"> 6.55 </TD> <TD align="right"> 15.00 </TD> <TD align="right"> 14.40 </TD> <TD align="right"> 8.15 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 25.00 </TD> <TD align="right"> 23.00 </TD> <TD align="right"> 0.15 </TD> </TR>
  <TR> <TD align="right"> popular </TD> <TD align="right">   6 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 5.08 </TD> <TD align="right"> 1.38 </TD> <TD align="right"> 5.10 </TD> <TD align="right"> 5.08 </TD> <TD align="right"> 1.33 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 9.50 </TD> <TD align="right"> 9.50 </TD> <TD align="right"> 0.03 </TD> </TR>
  <TR> <TD align="right"> id </TD> <TD align="right">   7 </TD> <TD align="right"> 2000.00 </TD> <TD align="right"> 5047.60 </TD> <TD align="right"> 2907.72 </TD> <TD align="right"> 5102.50 </TD> <TD align="right"> 5043.29 </TD> <TD align="right"> 3779.15 </TD> <TD align="right"> 101.00 </TD> <TD align="right"> 10020.00 </TD> <TD align="right"> 9919.00 </TD> <TD align="right"> 65.02 </TD> </TR>
   </TABLE>



```r
some(pop2[, c(2, 1, 16)], n = 20)
```

```
     class pupil   id
57       3    17  317
58       3    18  318
242     12    13 1213
454     22    20 2220
694     35     1 3501
731     36    18 3618
842     42    19 4219
1035    52    14 5214
1315    66    12 6612
1323    66    20 6620
1345    68     1 6801
1643    83     9 8309
1667    84    15 8415
1692    85    19 8519
1704    86     9 8609
1796    90    17 9017
1804    91     2 9102
1837    92    17 9217
1893    95    20 9520
1980    99    23 9923
```


## historgrams


```r
par(mfcol = c(2, 2))
hist(pop2$popular, col = "red", breaks = 10)
hist(pop2$female, col = "red", breaks = 2)
hist(pop2$extrav, col = "red", breaks = 10)
hist(pop2$texp, col = "red", breaks = 10)
```

![plot of chunk hists](figure/hists.png) 


## correlations


```r
# corrplot is found in the 'arm' package
corrplot(pop2[, 3:6], color = TRUE)
```

![plot of chunk corrplot](figure/corrplot.png) 



```r
ggpairs(pop2[, 3:6])
```

![plot of chunk pairplot](figure/pairplot.png) 



## Research Question:


### Are extraverted students more popular than intraverted students?
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />

One OLS regression, ignoring nesting within classes (n=2000):
$$
Y_{i}=\beta_{0} + \beta_{1} X_{1i} + \beta_{2} X_{2i} + e_{i}
$$
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
$$
popularity_{i}=\beta_{0} + \beta_{1} female_{1i} + \beta_{2} extraversion_{2i} + e_{i}
$$

<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
separate OLS regression equation for each class:
$$
Y_{ij}=\beta_{0j} + \beta_{1j}X_{1ij} + \beta_{2j}X_{2ij} + e_{ij}
$$
<br />
<br />
<br />
<br />
<br />

$$
popularity_{ij}=\beta_{0j} + \beta_{1j}female_{1ij} + \beta_{2j}extraversion_{2ij} + e_{ij}
$$
<br />
<br />
<br />
<br />
<br />

 -or-
 $$
Y_{ij}=\beta_{0j} + \beta_{1j}X_{1ij} + \beta_{2j}X_{2ij} + \beta_{3j}X_{3j} + ... + e_{ij}
$$
<br />
<br />
<br />
<br />
<br />
 $$
popularity_{ij}=\beta_{0j} + \beta_{1j}female_{1ij} + \beta_{2j}extraversion_{2ij}  + \beta_{3j}class_{3j} + ... + e_{ij}
$$
<br />
<br />
<br />
<br />
<br />


```r


# Models
ols <- lm(popular ~ female + extrav, pop2)
ols.classIV.mod <- lm(popular ~ female + extrav + factor(class), pop2)
ml.mod <- lmer(popular ~ female + extrav + (1 | class), pop2)
mod.names <- c("OLS", "OLS w/class as IV", "MLM")

# make table
htmlreg(list(ols, ols.classIV.mod, ml.mod), omit.coef = "class", model.names = mod.names)
```


<!DOCTYPE html>
<html>
  <head>
    <title></title>
    <style type="text/css">
      table {
        border: none;
      }
      th {
        text-align: left;
        border-top: 2px solid black;
        border-bottom: 1px solid black;
        padding-right: 12px;
      }
      .midRule {
        border-top: 1px solid black;
      }
      .bottomRule {
        border-bottom: 2px solid black;
      }
      td {
        padding-right: 12px;
        border: none;
      }
      caption span {
        float: left;
      }
      sup {
        vertical-align: 4px;
      }
    </style>
  </head>

  <body>
    <table cellspacing="0">
      <tr>
        <th class="modelnames"></th>
        <th><b>OLS</b></th>
        <th><b>OLS w/class as IV</b></th>
        <th><b>MLM</b></th>
      </tr>
      <tr>
        <td>(Intercept)</td>
        <td>2.79<sup>***</sup></td>
        <td>2.27<sup>***</sup></td>
        <td>2.14</td>
      </tr>
      <tr>
        <td></td>
        <td>(0.10)</td>
        <td>(0.19)</td>
        <td>(0.12)</td>
      </tr>
      <tr>
        <td>female</td>
        <td>1.51<sup>***</sup></td>
        <td>1.24<sup>***</sup></td>
        <td>1.25</td>
      </tr>
      <tr>
        <td></td>
        <td>(0.05)</td>
        <td>(0.04)</td>
        <td>(0.04)</td>
      </tr>
      <tr>
        <td>extrav</td>
        <td>0.29<sup>***</sup></td>
        <td>0.45<sup>***</sup></td>
        <td>0.44</td>
      </tr>
      <tr>
        <td></td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.02)</td>
      </tr>
      <tr>
        <td class="midRule">R<sup>2</sup></td>
        <td class="midRule">0.39</td>
        <td class="midRule">0.71</td>
        <td class="midRule"></td>
      </tr>
      <tr>
        <td>Adj. R<sup>2</sup></td>
        <td>0.39</td>
        <td>0.69</td>
        <td></td>
      </tr>
      <tr>
        <td>Num. obs.</td>
        <td>2000</td>
        <td>2000</td>
        <td>2000</td>
      </tr>
      <tr>
        <td>AIC</td>
        <td></td>
        <td></td>
        <td>4958.30</td>
      </tr>
      <tr>
        <td>BIC</td>
        <td></td>
        <td></td>
        <td>4986.30</td>
      </tr>
      <tr>
        <td>Log Likelihood</td>
        <td></td>
        <td></td>
        <td>-2474.15</td>
      </tr>
      <tr>
        <td>Deviance</td>
        <td></td>
        <td></td>
        <td>4948.30</td>
      </tr>
      <tr>
        <td>Num. groups: class</td>
        <td></td>
        <td></td>
        <td>100</td>
      </tr>
      <tr>
        <td>Variance: class.(Intercept)</td>
        <td></td>
        <td></td>
        <td>0.63</td>
      </tr>
      <tr>
        <td class="bottomRule">Variance: Residual</td>
        <td class="bottomRule"></td>
        <td class="bottomRule"></td>
        <td class="bottomRule">0.59</td>
      </tr>
      <tr>
        <td colspan="4"><span style="font-size:0.8em"><sup>***</sup>p &lt; 0.001, <sup>**</sup>p &lt; 0.01, <sup>*</sup>p &lt; 0.05, <sup>&middot;</sup>p &lt; 0.1</span></td>
      </tr>
     </table>
  </body>
</html>

```r

# show class coefficients

ols.classIV.noInt.mod <- lm(popular ~ factor(class) + female + extrav - 1, pop2)
class.coefs <- as.data.frame(cbind(coef(ols.classIV.noInt.mod)[c(1:20)], se.coef(ols.classIV.noInt.mod)[c(1:20)]))
names(class.coefs) <- c("coefficient", "s.e.")
print(xtable(class.coefs, caption = "First 20 class coefficients"), type = "html")
```

<!-- html table generated in R 2.15.3 by xtable 1.7-1 package -->
<!-- Thu Apr 04 10:02:45 2013 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> First 20 class coefficients </CAPTION>
<TR> <TH>  </TH> <TH> coefficient </TH> <TH> s.e. </TH>  </TR>
  <TR> <TD align="right"> factor(class)1 </TD> <TD align="right"> 2.27 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)2 </TD> <TD align="right"> 1.30 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)3 </TD> <TD align="right"> 1.51 </TD> <TD align="right"> 0.20 </TD> </TR>
  <TR> <TD align="right"> factor(class)4 </TD> <TD align="right"> 2.79 </TD> <TD align="right"> 0.18 </TD> </TR>
  <TR> <TD align="right"> factor(class)5 </TD> <TD align="right"> 2.05 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)6 </TD> <TD align="right"> 1.46 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)7 </TD> <TD align="right"> 1.39 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)8 </TD> <TD align="right"> 0.78 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)9 </TD> <TD align="right"> 1.86 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)10 </TD> <TD align="right"> 2.40 </TD> <TD align="right"> 0.18 </TD> </TR>
  <TR> <TD align="right"> factor(class)11 </TD> <TD align="right"> 1.54 </TD> <TD align="right"> 0.18 </TD> </TR>
  <TR> <TD align="right"> factor(class)12 </TD> <TD align="right"> 1.74 </TD> <TD align="right"> 0.20 </TD> </TR>
  <TR> <TD align="right"> factor(class)13 </TD> <TD align="right"> 3.09 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)14 </TD> <TD align="right"> 0.55 </TD> <TD align="right"> 0.22 </TD> </TR>
  <TR> <TD align="right"> factor(class)15 </TD> <TD align="right"> 1.82 </TD> <TD align="right"> 0.18 </TD> </TR>
  <TR> <TD align="right"> factor(class)16 </TD> <TD align="right"> 1.21 </TD> <TD align="right"> 0.21 </TD> </TR>
  <TR> <TD align="right"> factor(class)17 </TD> <TD align="right"> 2.34 </TD> <TD align="right"> 0.17 </TD> </TR>
  <TR> <TD align="right"> factor(class)18 </TD> <TD align="right"> 3.51 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)19 </TD> <TD align="right"> 2.79 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> factor(class)20 </TD> <TD align="right"> 2.09 </TD> <TD align="right"> 0.19 </TD> </TR>
   </TABLE>

```r
paste("Range of class coefficients = ", round(range(coef(ols.classIV.noInt.mod)[1:100])[1], 
    2), " to ", round(range(coef(ols.classIV.noInt.mod)[1:100])[2], 2))
```

[1] "Range of class coefficients =  0.06  to  3.87"

```r
paste(" mean s.e. of class coefficients = ", round(mean(se.coef(ols.classIV.noInt.mod)[1:100]), 
    2))
```

[1] " mean s.e. of class coefficients =  0.19"



Intercept only model:

level 1: 
$$
Y_{ij}=\beta_{0j}+e_{ij}
$$
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
level 2:
$$
\beta_{0j}=\gamma_{00}+u_{0j}
$$
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />

combined:
$$
Y_{ij}=\gamma_{00}+u_{0j} + e_{ij}
$$
<br />
<br />
<br />
<br />
<br />
Intraclass correlation (ICC):
$$ 
\rho =\frac{\sigma_{u_0}^2}{\sigma_{u_0}^2 + \sigma_{e}^2}
$$



```r
mod0 <- lmer(popular ~ 1 + (1 | class), pop2, REML = FALSE)
mod1 <- lmer(popular ~ female + extrav + (1 | class), pop2, REML = FALSE)
mod2 <- lmer(popular ~ female + extrav + texp + (1 + extrav | class), pop2, 
    REML = FALSE)
mod3 <- lmer(popular ~ female + extrav * texp + (1 + extrav | class), pop2, 
    REML = FALSE)
mod4 <- lmer(popular ~ female + Cextrav * Ctexp + (1 + Cextrav | class), pop2, 
    REML = F)
names(mod4@fixef)[3:5] <- c("extrav", "texp", "extrav:texp")
mod.names <- c("unconditional", "level 1", "level 2", "interaction", "int. vars. centered")
htmlreg(list(mod0, mod1, mod2, mod3, mod4), model.names = mod.names, symbol = "+", 
    caption = "Multilevel Models", caption.above = TRUE)
```


<!DOCTYPE html>
<html>
  <head>
    <title>Multilevel Models</title>
    <style type="text/css">
      table {
        border: none;
      }
      th {
        text-align: left;
        border-top: 2px solid black;
        border-bottom: 1px solid black;
        padding-right: 12px;
      }
      .midRule {
        border-top: 1px solid black;
      }
      .bottomRule {
        border-bottom: 2px solid black;
      }
      td {
        padding-right: 12px;
        border: none;
      }
      caption span {
        float: left;
      }
      sup {
        vertical-align: 4px;
      }
    </style>
  </head>

  <body>
    <table cellspacing="0">
      <caption align="top" style="margin-bottom:0.3em">Multilevel Models</caption>
      <tr>
        <th class="modelnames"></th>
        <th><b>unconditional</b></th>
        <th><b>level 1</b></th>
        <th><b>level 2</b></th>
        <th><b>interaction</b></th>
        <th><b>int. vars. centered</b></th>
      </tr>
      <tr>
        <td>(Intercept)</td>
        <td>5.08</td>
        <td>2.14</td>
        <td>0.74</td>
        <td>-1.21</td>
        <td>4.37</td>
      </tr>
      <tr>
        <td></td>
        <td>(0.09)</td>
        <td>(0.12)</td>
        <td>(0.20)</td>
        <td>(0.27)</td>
        <td>(0.06)</td>
      </tr>
      <tr>
        <td>female</td>
        <td></td>
        <td>1.25</td>
        <td>1.25</td>
        <td>1.24</td>
        <td>1.24</td>
      </tr>
      <tr>
        <td></td>
        <td></td>
        <td>(0.04)</td>
        <td>(0.04)</td>
        <td>(0.04)</td>
        <td>(0.04)</td>
      </tr>
      <tr>
        <td>extrav</td>
        <td></td>
        <td>0.44</td>
        <td>0.45</td>
        <td>0.80</td>
        <td>0.45</td>
      </tr>
      <tr>
        <td></td>
        <td></td>
        <td>(0.02)</td>
        <td>(0.02)</td>
        <td>(0.04)</td>
        <td>(0.02)</td>
      </tr>
      <tr>
        <td>texp</td>
        <td></td>
        <td></td>
        <td>0.09</td>
        <td>0.23</td>
        <td>0.10</td>
      </tr>
      <tr>
        <td></td>
        <td></td>
        <td></td>
        <td>(0.01)</td>
        <td>(0.02)</td>
        <td>(0.01)</td>
      </tr>
      <tr>
        <td>extrav:texp</td>
        <td></td>
        <td></td>
        <td></td>
        <td>-0.02</td>
        <td>-0.02</td>
      </tr>
      <tr>
        <td></td>
        <td></td>
        <td></td>
        <td></td>
        <td>(0.00)</td>
        <td>(0.00)</td>
      </tr>
      <tr>
        <td class="midRule">AIC</td>
        <td class="midRule">6333.47</td>
        <td class="midRule">4943.95</td>
        <td class="midRule">4828.81</td>
        <td class="midRule">4765.62</td>
        <td class="midRule">4765.62</td>
      </tr>
      <tr>
        <td>BIC</td>
        <td>6350.27</td>
        <td>4971.96</td>
        <td>4873.61</td>
        <td>4816.03</td>
        <td>4816.03</td>
      </tr>
      <tr>
        <td>Log Likelihood</td>
        <td>-3163.73</td>
        <td>-2466.98</td>
        <td>-2406.40</td>
        <td>-2373.81</td>
        <td>-2373.81</td>
      </tr>
      <tr>
        <td>Deviance</td>
        <td>6327.47</td>
        <td>4933.95</td>
        <td>4812.81</td>
        <td>4747.62</td>
        <td>4747.62</td>
      </tr>
      <tr>
        <td>Num. obs.</td>
        <td>2000</td>
        <td>2000</td>
        <td>2000</td>
        <td>2000</td>
        <td>2000</td>
      </tr>
      <tr>
        <td>Num. groups: class</td>
        <td>100</td>
        <td>100</td>
        <td>100</td>
        <td>100</td>
        <td>100</td>
      </tr>
      <tr>
        <td>Variance: class.(Intercept)</td>
        <td>0.69</td>
        <td>0.62</td>
        <td>1.28</td>
        <td>0.45</td>
        <td>0.28</td>
      </tr>
      <tr>
        <td>Variance: Residual</td>
        <td>1.22</td>
        <td>0.59</td>
        <td>0.55</td>
        <td>0.55</td>
        <td>0.55</td>
      </tr>
      <tr>
        <td>Variance: class.extrav</td>
        <td></td>
        <td></td>
        <td>0.03</td>
        <td>0.00</td>
        <td></td>
      </tr>
      <tr>
        <td class="bottomRule">Variance: class.Cextrav</td>
        <td class="bottomRule"></td>
        <td class="bottomRule"></td>
        <td class="bottomRule"></td>
        <td class="bottomRule"></td>
        <td class="bottomRule">0.00</td>
      </tr>
      <tr>
        <td colspan="6"><span style="font-size:0.8em"><sup>***</sup>p &lt; 0.001, <sup>**</sup>p &lt; 0.01, <sup>*</sup>p &lt; 0.05, <sup>+</sup>p &lt; 0.1</span></td>
      </tr>
     </table>
  </body>
</html>



## Preparing pop2 data for Mplus in R

```r
# The foreign package contains many functions to import various types of
# data from other stats programs.
library(foreign)
# use this command to read in an SPSS file:
popular2 <- read.spss(file = "data/popular2.sav", use.value.labels = FALSE, 
    to.data.frame = TRUE, use.missings = TRUE)
# The following command creates a .dat file and and .inp file with the
# basic Mplus code.
library(MplusAutomation)
prepareMplusData(df = popular2, filename = "data/pop2.dat", inpfile = "mplus/bare.inp")
```


