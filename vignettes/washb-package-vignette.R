## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 300)
options(scipen=20)

## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval = FALSE, tidy=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  devtools::install_github("ben-arnold/washb")

## ---- eval = FALSE, tidy=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#      install.packages("sandwich")
#      install.packages("lmtest")
#      install.packages("coin")
#      install.packages("plyr")
#      install.packages("metafor")

## ---- eval = FALSE, tidy=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#      install.packages("tmle")
#      install.packages("SuperLearner")

## ---- results = "hide",tidy=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(washb)

## ---- results="hide", eval=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load and merge the final analysis files
# enrollment charactersitics, diarrhea measurements, and treatment assignments
# note: these are unblinded treatment assignments in the encrypted volume 0-treatment-assignments
# if you do not have access to these data through Dropbox, but need it, please contact UCB
washb_bd_tr    <- read.csv('/Volumes/0-Treatment-assignments/washb-bangladesh-tr.csv')
washb_bd_enrol <- read.csv('~/dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-enrol.csv')
washb_bd_diar  <- read.csv('~/dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-diar.csv')

# drop svydate and month because they are superceded in the child level diarrhea data
washb_bd_enrol$svydate <- NULL
washb_bd_enrol$month <- NULL

# merge the treatment assignments to the baseline dataset
dd <- merge(washb_bd_enrol,washb_bd_tr,by=c("clusterid","block"),all.x=T,all.y=T)

# merge the baseline data with diarrhea measurements (keep only compounds with follow-up data)
dd <- merge(dd,washb_bd_diar,by=c("dataid","clusterid","block"),all.x=F,all.y=T)

# subset to post-intervention measurements: Year 1 or Year 2
dd <- subset(dd,svy==1|svy==2)

# exlude new births that are not index children
dd <- subset(dd,sibnewbirth==0)

# exclude children with missing data
dd <- subset(dd,!is.na(dd$diar7d))

# re-order the tr factor for convenience
dd$tr <- factor(dd$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

# ensure that month is coded as a factor
dd$month <- factor(dd$month)

# sort the data for perfect replication when using random splits for V-fold cross-validation (washb_tmle and adj permutation tests)
dd <- dd[order(dd$block,dd$clusterid,dd$dataid,dd$childid),]


## ---- eval=FALSE, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # defunct code that andrew used to load data -- deprecated with the generalized work flow, above
#  # but retained in this script in case we need it for anything
#  #Load unblinded data into the "dd" object from sysdata
#  #dd<-loadUnblindedData("washb_bd_diarCleanUnblinded")
#  # setwd("C:/Users/andre/Documents/washb package data/data")
#  #
#  # load("washb_bd_diarCleanUnblinded.RData")
#  # dd<-washb_bd_diarCleanUnblinded
#  # load("washb_bd_anthroCleanUnblinded.Rdata")
#  # lazd<-washb_bd_anthroCleanUnblinded

## ---- eval=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# load the anthropometry dataset
washb_bd_anthro  <- read.csv('~/dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-anthro.csv')

#  merge to final analysis files, loaded above (keep only compounds with follow-up data)
lazd <- merge(washb_bd_enrol,washb_bd_tr,by=c("clusterid","block"),all.x=T,all.y=T)
lazd <- merge(lazd,washb_bd_anthro,by=c("dataid","clusterid","block"),all.x=F,all.y=T)

# subset to the index (target) children measured in Year 2 (primary outcome)
lazd <- subset(lazd,svy==2)
lazd <- subset(lazd,tchild=="Target child")

# drop children with extreme LAZ values
lazd <- subset(lazd,laz_x!=1)

# re-order the tr factor for convenience
lazd$tr <- factor(lazd$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

# ensure that month is coded as a factor
lazd$month <- factor(lazd$month)

# rename aged to agedays (for consistency with the anthro file)
lazd$agedays <- lazd$aged

# sort the data for perfect replication when using random splits for V-fold cross-validation (washb_tmle and adj permutation tests)
lazd <- lazd[order(lazd$block,lazd$clusterid,lazd$dataid,lazd$childid),]


## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_mean(Y,id,print=TRUE)

## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_ttest(Y,tr,strat,contrast)

## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_mh(Y,tr,strat,contrast,measure="RR")

## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_glm(Y, tr, pair, W=NULL, forcedW=NULL, V=NULL, id, contrast, family='gaussian', pval=0.2, print=TRUE)

## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_tmle(Y,tr,W=NULL,id,pair=NULL, Delta = rep(1,length(Y)), family="gaussian", contrast, Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet") ,g.SL.library=Q.SL.library, pval=0.2, seed=NULL, print=TRUE)

## ---- eval=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
unlist(adj.LAZ.tmle.C.N$estimates$ATE)

## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_permute(Y,tr,pair,contrast,nreps=100000,seed=NULL)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_prescreen(Y=dd$diar7d,Ws=W,family="binomial", pval=0.2, print=TRUE)

## ----eval=F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_lincom(lc=NULL,varlist=NULL,fit,vcv, measure="RR", flag=NULL)

## ---- include=FALSE, eval=F, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #Table of Primary Outcome Results
#  
#  ##7-day diarrheal disease recall outcome
#  
#  Contrast v. control | Estimator | PR | 95% CI | SE logPR | P-value
#  --------------------|-----------|----|--------|----------|--------
#  Water | Unadjusted MH  | `r round(diff.h1[1,1],3)` | `r round((diff.h1[1,2:3]),3)` | `r round(diff.h1[1,5],3)` | `r round(diff.h1[1,7],3)`
#  Water | Unadjusted GLM |  `r round(unadj.glm.h1[[1]][1],3)` |(`r round(unadj.glm.h1[[1]][2:3],3)`) | `r round(unadj.glm.h1[[1]][5],3)` | `r round(unadj.glm.h1[[1]][7],3)`
#  Water | Adjusted GLM | `r round(adj.glm.h1[[1]][1],3)` |(`r round(adj.glm.h1[[1]][2:3],3)`) | `r round(adj.glm.h1[[1]][5],3)` | `r round(adj.glm.h1[[1]][7],3)`
#  Water | Adjusted GLM + TMLE |  | | |
#  Water | Adjusted SL + TMLE |  | | |
#  Water | Wilcoxon  permutation test |  | | |`r #permute.diff.h1[1]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  Sanitation | Unadjusted MH  | `r round(diff.h1[2,1],3)` | `r round((diff.h1[2,2:3]),3)` | `r round(diff.h1[2,5],3)` | `r diff.h1[2,7]`
#  Sanitation | Unadjusted GLM |  `r round(unadj.glm.h1[[2]][1],3)` |(`r round(unadj.glm.h1[[2]][2:3],3)`) | `r round(unadj.glm.h1[[2]][5],3)` | `r unadj.glm.h1[[2]][7]`
#  Sanitation | Adjusted GLM | `r round(adj.glm.h1[[2]][1],3)` |(`r round(adj.glm.h1[[2]][2:3],3)`) | `r round(adj.glm.h1[[2]][5],3)` | `r (adj.glm.h1[[2]][7])`
#  Sanitation | Adjusted GLM + TMLE |  | | |-
#  Sanitation | Adjusted SL + TMLE |  | | |-
#  Sanitation | Wilcoxon  permutation test |  | | |`r #permute.diff.h1[2]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  Handwashing | Unadjusted MH  | `r round(diff.h1[3,1],3)` | `r round((diff.h1[3,2:3]),3)` | `r round(diff.h1[3,5],3)` | `r round(diff.h1[3,7],3)`
#  Handwashing | Unadjusted GLM |  `r round(unadj.glm.h1[[3]][1],3)` |(`r round(unadj.glm.h1[[3]][2:3],3)`) | `r round(unadj.glm.h1[[3]][5],3)` | `r round(unadj.glm.h1[[3]][7],3)`
#  Handwashing | Adjusted GLM | `r round(adj.glm.h1[[3]][1],3)` |(`r round(adj.glm.h1[[3]][2:3],3)`) | `r round(adj.glm.h1[[3]][5],3)` | `r round(adj.glm.h1[[3]][7],3)`
#  Handwashing | Adjusted GLM + TMLE |  | | |
#  Handwashing | Adjusted SL + TMLE |  | | |
#  Handwashing | Wilcoxon  permutation test |  | | |`r #permute.diff.h1[3]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  WSH | Unadjusted MH  | `r round(diff.h1[4,1],3)` | `r round((diff.h1[4,2:3]),3)` | `r round(diff.h1[4,5],3)` | `r round(diff.h1[4,7],3)`
#  WSH | Unadjusted GLM |  `r round(unadj.glm.h1[[4]][1],3)` |(`r round(unadj.glm.h1[[4]][2:3],3)`) | `r round(unadj.glm.h1[[4]][5],3)` | `r round(unadj.glm.h1[[4]][7],3)`
#  WSH | Adjusted GLM | `r round(adj.glm.h1[[4]][1],3)` |(`r round(adj.glm.h1[[4]][2:3],3)`) | `r round(adj.glm.h1[[4]][5],3)` | `r round(adj.glm.h1[[4]][7],3)`
#  WSH | Adjusted GLM + TMLE |  | | |
#  WSH | Adjusted SL + TMLE |  | | |
#  WSH | Wilcoxon  permutation test |  | | |`r #permute.diff.h1[4]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  Nutrition | Unadjusted MH  | `r round(diff.h1[5,1],3)` | `r round((diff.h1[5,2:3]),3)` | `r round(diff.h1[5,5],3)` | `r round(diff.h1[5,7],3)`
#  Nutrition | Unadjusted GLM |  `r round(unadj.glm.h1[[5]][1],3)` |(`r round(unadj.glm.h1[[5]][2:3],3)`) | `r round(unadj.glm.h1[[5]][5],3)` | `r round(unadj.glm.h1[[5]][7],3)`
#  Nutrition | Adjusted GLM | `r round(adj.glm.h1[[5]][1],3)` |(`r round(adj.glm.h1[[5]][2:3],3)`) | `r round(adj.glm.h1[[5]][5],3)` | `r round(adj.glm.h1[[5]][7],3)`
#  Nutrition | Adjusted GLM + TMLE |  | | |
#  Nutrition | Adjusted SL + TMLE |  | | |
#  Nutrition | Wilcoxon  permutation test |  | | |`r #permute.diff.h1[5]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  WSH+Nutrition | Unadjusted MH  | `r round(diff.h1[6,1],3)` | `r round((diff.h1[6,2:3]),3)` | `r round(diff.h1[6,5],3)` | `r round(diff.h1[6,7],3)`
#  WSH+Nutrition | Unadjusted GLM |  `r round(unadj.glm.h1[[6]][1],3)` |(`r round(unadj.glm.h1[[6]][2:3],3)`) | `r round(unadj.glm.h1[[6]][5],3)` | `r round(unadj.glm.h1[[6]][7],3)`
#  WSH+Nutrition | Adjusted GLM | `r round(adj.glm.h1[[6]][1],3)` |(`r round(adj.glm.h1[[6]][2:3],3)`) | `r round(adj.glm.h1[[6]][5],3)` | `r round(adj.glm.h1[[6]][7],3)`
#  WSH+Nutrition | Adjusted GLM + TMLE |  | | |
#  WSH+Nutrition | Adjusted SL + TMLE |  | | |
#  WSH+Nutrition | Wilcoxon  permutation test |  | | |`r #permute.diff.h1[6]`
#  
#  <br>
#  <br>
#  <br>
#  
#  ##Length-for-Age Z-score outcome
#  
#  Contrast v. control | Estimator | Coef | 95% CI | SE  | P-value
#  --------------------|-----------|------|--------|-----|--------
#  Water | Unadjusted ttest | `r round(diff.h1LAZ[1,1],3)` |(`r round((diff.h1LAZ[1,2:3]),3)`) | t-stat:  `r round(diff.h1LAZ[1,4],3)`| `r round(diff.h1LAZ[1,5],3)`
#  Water | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[1]]$TR[1],3)` |(`r round(unadj.glm.h1LAZ[[1]]$TR[2:3],3)`) |SE:  `r round(unadj.glm.h1LAZ[[1]]$TR[4],3)` | `r round(unadj.glm.h1LAZ[[1]]$TR[6],3)`
#  Water | Adjusted GLM | `r round(adj.glm.h1LAZ[[1]]$TR[1],3)` |(`r round(adj.glm.h1LAZ[[1]]$TR[2:3],3)`) |SE:  `r round(adj.glm.h1LAZ[[1]]$TR[4],3)` | `r round(adj.glm.h1LAZ[[1]]$TR[6],3)`
#  Water | Adjusted GLM + TMLE |  | | |
#  Water | Adjusted SL + TMLE |  | | |
#  Water | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  Sanitation | Unadjusted ttest | `r round(diff.h1LAZ[2,1],3)` |(`r round((diff.h1LAZ[2,2:3]),3)`) | t-stat:  `r round(diff.h1LAZ[2,4],3)`| `r round(diff.h1LAZ[2,5],3)`
#  Sanitation | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[2]]$TR[1],3)` |(`r round(unadj.glm.h1LAZ[[2]]$TR[2:3],3)`) |SE:  `r round(unadj.glm.h1LAZ[[2]]$TR[4],3)` | `r round(unadj.glm.h1LAZ[[2]]$TR[6],3)`
#  Sanitation | Adjusted GLM | `r round(adj.glm.h1LAZ[[2]]$TR[1],3)` |(`r round(adj.glm.h1LAZ[[2]]$TR[2:3],3)`) |SE:  `r round(adj.glm.h1LAZ[[2]]$TR[4],3)` | `r round(adj.glm.h1LAZ[[2]]$TR[6],3)`
#  Sanitation | Adjusted GLM + TMLE |  | | |
#  Sanitation | Adjusted SL + TMLE |  | | |
#  Sanitation | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  Handwashing | Unadjusted ttest | `r round(diff.h1LAZ[3,1],3)` |(`r round((diff.h1LAZ[3,2:3]),3)`) | t-stat:  `r round(diff.h1LAZ[3,4],3)`| `r round(diff.h1LAZ[3,5],3)`
#  Handwashing | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[3]]$TR[1],3)` |(`r round(unadj.glm.h1LAZ[[3]]$TR[2:3],3)`) |SE:  `r round(unadj.glm.h1LAZ[[3]]$TR[4],3)` | `r round(unadj.glm.h1LAZ[[3]]$TR[6],3)`
#  Handwashing | Adjusted GLM | `r round(adj.glm.h1LAZ[[3]]$TR[1],3)` |(`r round(adj.glm.h1LAZ[[3]]$TR[2:3],3)`) |SE:  `r round(adj.glm.h1LAZ[[3]]$TR[4],3)` | `r round(adj.glm.h1LAZ[[3]]$TR[6],3)`
#  Handwashing | Adjusted GLM + TMLE |  | | |
#  Handwashing | Adjusted SL + TMLE |  | | |
#  Handwashing | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  WSH | Unadjusted ttest | `r round(diff.h1LAZ[4,1],3)` |(`r round((diff.h1LAZ[4,2:3]),3)`) | t-stat:  `r round(diff.h1LAZ[4,4],3)`| `r round(diff.h1LAZ[4,5],3)`
#  WSH | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[4]]$TR[1],3)` |(`r round(unadj.glm.h1LAZ[[4]]$TR[2:3],3)`) |SE:  `r round(unadj.glm.h1LAZ[[4]]$TR[4],3)` | `r round(unadj.glm.h1LAZ[[4]]$TR[6],3)`
#  WSH | Adjusted GLM | `r round(adj.glm.h1LAZ[[5]]$TR[1],3)` |(`r round(adj.glm.h1LAZ[[5]]$TR[2:3],3)`) |SE:  `r round(adj.glm.h1LAZ[[5]]$TR[4],3)` | `r round(adj.glm.h1LAZ[[5]]$TR[6],3)`
#  WSH | Adjusted GLM + TMLE |  | | |
#  WSH | Adjusted SL + TMLE |  | | |
#  WSH | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  Nutrition | Unadjusted ttest | `r round(diff.h1LAZ[5,1],3)` |(`r round((diff.h1LAZ[5,2:3]),3)`) | t-stat:  `r round(diff.h1LAZ[5,4],3)`| `r round(diff.h1LAZ[5,5],3)`
#  Nutrition | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[5]]$TR[1],3)` |(`r round(unadj.glm.h1LAZ[[5]]$TR[2:3],3)`) |SE:  `r round(unadj.glm.h1LAZ[[5]]$TR[4],3)` | `r round(unadj.glm.h1LAZ[[5]]$TR[6],3)`
#  Nutrition | Adjusted GLM | `r round(adj.glm.h1LAZ[[5]]$TR[1],3)` |(`r round(adj.glm.h1LAZ[[5]]$TR[2:3],3)`) |SE:  `r round(adj.glm.h1LAZ[[5]]$TR[4],3)` | `r round(adj.glm.h1LAZ[[5]]$TR[6],3)`
#  Nutrition | Adjusted GLM + TMLE |  | | |
#  Nutrition | Adjusted SL + TMLE |  | | |
#  Nutrition | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  WSH+Nutrition | Unadjusted ttest | `r round(diff.h1LAZ[6,1],3)` |(`r round((diff.h1LAZ[6,2:3]),3)`) | t-stat:  `r round(diff.h1LAZ[6,4],3)`| `r round(diff.h1LAZ[6,5],3)`
#  WSH+Nutrition | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[6]]$TR[1],3)` |(`r round(unadj.glm.h1LAZ[[6]]$TR[2:3],3)`) |SE:  `r round(unadj.glm.h1LAZ[[6]]$TR[4],3)` | `r round(unadj.glm.h1LAZ[[6]]$TR[6],3)`
#  WSH+Nutrition | Adjusted GLM | `r round(adj.glm.h1LAZ[[6]]$TR[1],3)` |(`r round(adj.glm.h1LAZ[[6]]$TR[2:3],3)`) |SE:  `r round(adj.glm.h1LAZ[[6]]$TR[4],3)` | `r round(adj.glm.h1LAZ[[6]]$TR[6],3)`
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  
#  
#  <br>
#  <br>
#  <br>
#  
#  
#  ##Diarrheal disease comparisons between treatments and control, stratified into target children and siblings.
#  
#  Contrast v. control | Child Type | Coefficient |  95% CI  |  P-value
#  --------------------|------------|-------------|----------|---------
#  Water |`r glm.byChildType[[1]]$lincom[1,1]` | `r glm.byChildType[[1]]$lincom[1,2]` | `r round(glm.byChildType[[1]]$lincom[1,4:5],3)` | `r glm.byChildType[[1]]$lincom[1,7]`
#  - |`r glm.byChildType[[1]]$lincom[2,1]`  | `r glm.byChildType[[1]]$lincom[2,2]` | `r round(glm.byChildType[[1]]$lincom[2,4:5],3)` | `r glm.byChildType[[1]]$lincom[2,7]`
#  Sanitation |`r glm.byChildType[[2]]$lincom[1,1]` | `r glm.byChildType[[2]]$lincom[1,2]` | `r round(glm.byChildType[[2]]$lincom[1,4:5],3)` | `r glm.byChildType[[2]]$lincom[1,7]`
#  - |`r glm.byChildType[[2]]$lincom[2,1]`  | `r glm.byChildType[[2]]$lincom[2,2]` | `r round(glm.byChildType[[2]]$lincom[2,4:5],3)` | `r glm.byChildType[[2]]$lincom[2,7]`
#  Handwashing |`r glm.byChildType[[3]]$lincom[1,1]` | `r glm.byChildType[[3]]$lincom[1,2]` | `r round(glm.byChildType[[3]]$lincom[1,4:5],3)` | `r glm.byChildType[[3]]$lincom[1,7]`
#  - |`r glm.byChildType[[3]]$lincom[2,1]`  | `r glm.byChildType[[3]]$lincom[2,2]` | `r round(glm.byChildType[[3]]$lincom[2,4:5],3)` | `r glm.byChildType[[3]]$lincom[2,7]`
#  WSH |`r glm.byChildType[[4]]$lincom[1,1]` | `r glm.byChildType[[4]]$lincom[1,2]` | `r round(glm.byChildType[[4]]$lincom[1,4:5],3)` | `r glm.byChildType[[4]]$lincom[1,7]`
#  - |`r glm.byChildType[[4]]$lincom[2,1]`  | `r glm.byChildType[[4]]$lincom[2,2]` | `r round(glm.byChildType[[4]]$lincom[2,4:5],3)` | `r glm.byChildType[[4]]$lincom[2,7]`
#  Nutrition |`r glm.byChildType[[5]]$lincom[1,1]` | `r glm.byChildType[[5]]$lincom[1,2]` | `r round(glm.byChildType[[5]]$lincom[1,4:5],3)` | `r glm.byChildType[[5]]$lincom[1,7]`
#  - |`r glm.byChildType[[5]]$lincom[2,1]`  | `r glm.byChildType[[5]]$lincom[2,2]` | `r round(glm.byChildType[[5]]$lincom[2,4:5],3)` | `r glm.byChildType[[5]]$lincom[2,7]`
#  WSH+Nutrition |`r glm.byChildType[[6]]$lincom[1,1]` | `r glm.byChildType[[6]]$lincom[1,2]` | `r round(glm.byChildType[[6]]$lincom[1,4:5],3)` | `r glm.byChildType[[6]]$lincom[1,7]`
#  - |`r glm.byChildType[[6]]$lincom[2,1]`  | `r glm.byChildType[[6]]$lincom[2,2]` | `r round(glm.byChildType[[6]]$lincom[2,4:5],3)` | `r glm.byChildType[[6]]$lincom[2,7]`
#  
#  
#  
#  <br>
#  <br>
#  <br>
#  
#  ##Risk difference between treatments and control, stratified into target children and siblings.
#  Contrast v. control | Child Type | Coefficient |  95% CI  |  P-value
#  --------------------|------------|-------------|----------|-----------
#  Water |`r glm.byChildType[[1]]$lincomRD[1,1]` | `r glm.byChildType[[1]]$lincomRD[1,2]` | `r round(glm.byChildType[[1]]$lincomRD[1,4:5],3)` | `r glm.byChildType[[1]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[1]]$lincomRD[2,1]`  | `r glm.byChildType[[1]]$lincomRD[2,2]` | `r round(glm.byChildType[[1]]$lincomRD[2,4:5],3)` | `r glm.byChildType[[1]]$lincomRD[2,7]`
#  Sanitation |`r glm.byChildType[[2]]$lincomRD[1,1]` | `r glm.byChildType[[2]]$lincomRD[1,2]` | `r round(glm.byChildType[[2]]$lincomRD[1,4:5],3)` | `r glm.byChildType[[2]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[2]]$lincomRD[2,1]`  | `r glm.byChildType[[2]]$lincomRD[2,2]` | `r round(glm.byChildType[[2]]$lincomRD[2,4:5],3)` | `r glm.byChildType[[2]]$lincomRD[2,7]`
#  Handwashing |`r glm.byChildType[[3]]$lincomRD[1,1]` | `r glm.byChildType[[3]]$lincomRD[1,2]` | `r round(glm.byChildType[[3]]$lincomRD[1,4:5],3)` | `r glm.byChildType[[3]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[3]]$lincomRD[2,1]`  | `r glm.byChildType[[3]]$lincomRD[2,2]` | `r round(glm.byChildType[[3]]$lincomRD[2,4:5],3)` | `r glm.byChildType[[3]]$lincomRD[2,7]`
#  WSH |`r glm.byChildType[[4]]$lincomRD[1,1]` | `r glm.byChildType[[4]]$lincomRD[1,2]` | `r round(glm.byChildType[[4]]$lincomRD[1,4:5],3)` | `r glm.byChildType[[4]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[4]]$lincomRD[2,1]`  | `r glm.byChildType[[4]]$lincomRD[2,2]` | `r round(glm.byChildType[[4]]$lincomRD[2,4:5],3)` | `r glm.byChildType[[4]]$lincomRD[2,7]`
#  Nutrition |`r glm.byChildType[[5]]$lincomRD[1,1]` | `r glm.byChildType[[5]]$lincomRD[1,2]` | `r round(glm.byChildType[[5]]$lincomRD[1,4:5],3)` | `r glm.byChildType[[5]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[5]]$lincomRD[2,1]`  | `r glm.byChildType[[5]]$lincomRD[2,2]` | `r round(glm.byChildType[[5]]$lincomRD[2,4:5],3)` | `r glm.byChildType[[5]]$lincomRD[2,7]`
#  WSH + Nutrition |`r glm.byChildType[[6]]$lincomRD[1,1]` | `r glm.byChildType[[6]]$lincomRD[1,2]` | `r round(glm.byChildType[[6]]$lincomRD[1,4:5],3)` | `r glm.byChildType[[6]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[6]]$lincomRD[2,1]`  | `r glm.byChildType[[6]]$lincomRD[2,2]` | `r round(glm.byChildType[[6]]$lincomRD[2,4:5],3)` | `r glm.byChildType[[6]]$lincomRD[2,7]`
#  
#  
#  
#  <br>
#  <br>
#  <br>
#  
#  ##Food Security Subgroup
#  ###LAZ comparisons between treatment and control arms, stratified by food security level.
#  Results from the washb_glm function run above with `V="hfiacat"`.
#  
#  Contrast v. control | Food Security Subgroup | Coefficient |  95% CI  |  P-value
#  --------------------|------------------------|-------------|----------|---------
#  Nutrition |`r unadj.glm.byFoodSecurity[[5]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[5]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[5]]$lincom[1,4:5],3)` | `r unadj.glm.byFoodSecurity[[5]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[5]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[5]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[5]]$lincom[2,4:5],3)` | `r unadj.glm.byFoodSecurity[[5]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[5]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[5]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[5]]$lincom[3,4:5],3)` | `r unadj.glm.byFoodSecurity[[5]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[5]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[5]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[5]]$lincom[4,4:5],3)` | `r unadj.glm.byFoodSecurity[[5]]$lincom[4,7]`
#  
#  
#  
#  
#  
#  Water |`r unadj.glm.byFoodSecurity[[1]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[1]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[1]]$lincom[1,4:5],3)` | `r unadj.glm.byFoodSecurity[[1]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[1]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[1]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[1]]$lincom[2,4:5],3)` | `r unadj.glm.byFoodSecurity[[1]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[1]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[1]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[1]]$lincom[3,4:5],3)` | `r unadj.glm.byFoodSecurity[[1]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[1]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[1]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[1]]$lincom[4,4:5],3)` | `r unadj.glm.byFoodSecurity[[1]]$lincom[4,7]`
#  Sanitation |`r unadj.glm.byFoodSecurity[[2]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[2]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[2]]$lincom[1,4:5],3)` | `r unadj.glm.byFoodSecurity[[2]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[2]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[2]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[2]]$lincom[2,4:5],3)` | `r unadj.glm.byFoodSecurity[[2]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[2]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[2]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[2]]$lincom[3,4:5],3)` | `r unadj.glm.byFoodSecurity[[2]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[2]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[2]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[2]]$lincom[4,4:5],3)` | `r unadj.glm.byFoodSecurity[[2]]$lincom[4,7]`
#  Handwashing |`r unadj.glm.byFoodSecurity[[3]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[3]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[3]]$lincom[1,4:5],3)` | `r unadj.glm.byFoodSecurity[[3]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[3]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[3]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[3]]$lincom[2,4:5],3)` | `r unadj.glm.byFoodSecurity[[3]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[3]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[3]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[3]]$lincom[3,4:5],3)` | `r unadj.glm.byFoodSecurity[[3]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[3]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[3]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[3]]$lincom[4,4:5],3)` | `r unadj.glm.byFoodSecurity[[3]]$lincom[4,7]`
#  WSH |`r unadj.glm.byFoodSecurity[[4]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[4]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[4]]$lincom[1,4:5],3)` | `r unadj.glm.byFoodSecurity[[4]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[4]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[4]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[4]]$lincom[2,4:5],3)` | `r unadj.glm.byFoodSecurity[[4]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[4]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[4]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[4]]$lincom[3,4:5],3)` | `r unadj.glm.byFoodSecurity[[4]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[4]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[4]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[4]]$lincom[4,4:5],3)` | `r unadj.glm.byFoodSecurity[[4]]$lincom[4,7]`
#  WSH + Nutrition |`r unadj.glm.byFoodSecurity[[6]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[6]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[6]]$lincom[1,4:5],3)` | `r unadj.glm.byFoodSecurity[[6]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[6]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[6]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[6]]$lincom[2,4:5],3)` | `r unadj.glm.byFoodSecurity[[6]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[6]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[6]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[6]]$lincom[3,4:5],3)` | `r unadj.glm.byFoodSecurity[[6]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[6]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[6]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[6]]$lincom[4,4:5],3)` | `r unadj.glm.byFoodSecurity[[6]]$lincom[4,7]`
#  
#  
#  <br>
#  <br>
#  <br>

