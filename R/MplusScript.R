# Basic Analyses in Mplus

library(MplusAutomation)
prepareMplusData(pop2, filename="pop2.dat")

# Mplus syntax generated from above function.

TITLE: pop2
DATA: FILE = "~/Dropbox/R/LearningAnalyses/2_StatisticalMethods/
Multilevel/MATA_Hox/2BasicModel/MplusAnalyses/pop2.dat";
VARIABLE: NAMES = pupil class extrav sex texp popular poptch Zextrav Zsex Ztexp
Zpopular Zpoptch Cextrav Ctexp Csex;

MISSING=.;


# basic.uc ----------------------------------------------------------------
setwd("~/Dropbox/R/LearningAnalyses/2_StatisticalMethods/Multilevel/MATA_Hox/2BasicModel/MplusAnalyses/basic.uc/")
file.edit('basic.uc.inp')
runModels()
file.edit('basic.uc.out')

# basic.1 -----------------------------------------------------------------
setwd("~/Dropbox/R/LearningAnalyses/2_StatisticalMethods/Multilevel/MATA_Hox/2BasicModel/MplusAnalyses/basic.1/")
file.edit('basic.1.inp')
runModels()
file.edit('basic.1.out')
