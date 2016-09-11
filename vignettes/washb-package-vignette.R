## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 300)
options(scipen=20)

## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval = FALSE, tidy=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#      install.packages("sandwich")
#      install.packages("lmtest")
#      install.packages("coin")
#      install.packages("plyr")
#      install.packages("metafor")

## ---- results = "hide"------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(washb)

## ---- results="hide", eval=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # drop svydate and month because they are superceded in the child level diarrhea data
#    washb_bd_enrol$svydate <- NULL
#    washb_bd_enrol$month <- NULL
#  
#  # merge the baseline dataset to the follow-up dataset
#  ad <- merge(washb_bd_enrol,washb_bd_diar,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
#  
#  # subset to the relevant measurement
#  # Year 1 or Year 2
#  ad <- subset(ad,svy==1|svy==2)
#  
#  #subset the diarrhea to children <36 mos at enrollment
#  ### (exlude new births that are not target children)
#  ad <- subset(ad,sibnewbirth==0)
#  ad <- subset(ad,gt36mos==0)
#  
#  # Exclude children with missing data
#  ad <- subset(ad,!is.na(ad$diar7d))
#  
#  #Re-order the tr factor for convenience
#  ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
#  
#  #Ensure that month is coded as a factor
#  ad$month <- factor(ad$month)
#  
#  #Sort the data for perfect replication when using V-fold cross-validation
#  ad <- ad[order(ad$block,ad$clusterid,ad$dataid,ad$childid),]

## ---- eval=TRUE, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Load unblinded data into the "ad" object from sysdata

#ad<-loadUnblindedData("washb_bd_diarCleanUnblinded")
setwd("C:/Users/andre/Documents/washb/data")

load("washb_bd_diarCleanUnblinded.RData")
ad<-washb_bd_diarCleanUnblinded
#data(washb_bd_diarCleanUnblinded)


## ---- eval=F, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  data(washb_bd_anthro)
#  data(washb_bd_enrol)
#    washb_bd_enrol$svydate <- NULL
#    washb_bd_enrol$month <- NULL
#  laz <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
#  
#  # subset to the endline target children
#  laz <- subset(laz,svy==2)
#  laz <- subset(laz,tchild=="Target child")
#  
#  # Drop children with extreme LAZ values
#  laz <- subset(laz,laz_x!=1)
#  
#  laz$tr <- factor(laz$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
#  laz$month <- factor(laz$month)
#  laz <- laz[order(laz$block,laz$clusterid,laz$dataid,laz$childid),]
#  
#  

## ---- eval=T, include=FALSE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Load unblinded data into the "laz" object from sysdata
#laz<-loadUnblindedData("washb_bd_diarCleanUnblinded")
#data(washb_bd_anthroCleanUnblinded)
#laz<-washb_bd_anthroCleanUnblinded
setwd("C:/Users/andre/Documents/washb/data")
load("washb_bd_anthroCleanUnblinded.Rdata")
laz<-washb_bd_anthroCleanUnblinded


## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_mean(Y,id,print=TRUE)

## ---- cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MomAge<-washb_mean(Y=washb_bd_enrol$momage,id=washb_bd_enrol$clusterid,print=TRUE)
MomEduY<-washb_mean(Y=washb_bd_enrol$momeduy,id=washb_bd_enrol$clusterid,print=TRUE)


## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_glm(Y,tr,pair,W=NULL, forcedW=NULL, V=NULL, id,contrast,family="binonial(link='log')", pval=0.2, print=TRUE)

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Diar.glm.C.S <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'))

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Diar.glm.C.S$TR

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RD.Diar.glm.C.S <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, id=ad$clusterid, contrast=c("Control","Sanitation"), family="gaussian", verbose=FALSE)

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
LAZ.glm.C.S <- washb_glm(Y=laz$laz,tr=laz$tr,pair=laz$block, id=laz$clusterid, contrast=c("Control","Sanitation"), family="gaussian", verbose=FALSE)

## ---- results = "hide" , cache=TRUE, comment=NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Subset dataset to covariates to be screened 
Ws_diar <- subset(ad,select=c("month","agedays","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))

#Subset the LAZ dataset to covariates to be screened for inclusion in adjusted glm models
Ws_laz <- subset(laz,select=c("fracode","month","aged","sex","birthord","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
adj.Diar.glm.C.S <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws_diar, id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'), verbose=FALSE)

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glm.C.S <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws_diar, forcedW=c("agedays","sex"), id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'), verbose=FALSE)

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a W variable containing "tchild" and other potential covariates:
W_tchild <- subset(ad,select=c("tchild"))

#Estimate subgroup analysis glm with washb_glm
glm.C.N.byChildType <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=W_tchild, V="tchild", id=ad$clusterid, contrast=c("Control","Nutrition"), family=binomial(link='log'), verbose=FALSE)

#Examine the treatment effect across subgroups with `objectname'$lincom
glm.C.N.byChildType$lincom

## ---- include=FALSE, warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###Subgroup analysis with multi-level factor
#The `V` argument can be used with multilevel factors. Here is code for a subgroup analysis of the effect of sanitation #treatment on child endline LAZ, by household food security levels. 

#Create a W variable containing only "hfiacat" for the unadjusted subgroup analysis:
W_hfiacat <- subset(laz,select=c("hfiacat"))

#Estimate subgroup analysis glm with washb_glm
glm.C.N.byFoodSecurity <- washb_glm(Y=laz$laz,tr=laz$tr,pair=laz$block, W=W_hfiacat, forcedW=NULL, V="hfiacat", id=laz$clusterid, contrast=c("Control","Nutrition"), family="gaussian", verbose=FALSE)

## ---- eval=F, include=T, comment=NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #Use the washb_prescreen function to select adjustment covariates associated with the outcome
#  adj.W<-washb_prescreen(Y=laz$laz,Ws_laz,family="gaussian", pval=0.2)
#  
#  #Subset the laz dataset to control and nutrition arms:
#        laz.subset=laz[which(laz$tr=="Control"|laz$tr=="Nutrition"),]
#  
#  #Subset the LAZ dataset to the selected adjustment covariates and LAZ, as well as tr and block, which will be needed in the permutation test
#  perm.adj.data <- subset(laz.subset,select=c(adj.W, "laz", "tr", "block"))
#  
#  #Subset to complete cases
#  perm.adj.data<-perm.adj.data[complete.cases(perm.adj.data),]
#  
#  Wselect <- subset(perm.adj.data,select=c(adj.W, "laz"))
#  #fit the glm model
#  fit <- glm(laz~., family="gaussian", data=Wselect)
#  
#  #Use the predict to return predicted LAZ from the adjusted glm model, and subtract it from the observed LAZ outcome
#  residuals<-Wselect$laz-predict(fit)
#  
#  #run the permutation test function
#  permute.adj.C.S <- washb_permute(Y=residuals,tr=perm.adj.data$tr,pair=perm.adj.data$block,contrast=c("Control","Sanitation"), nreps=100000,seed=1241353)

## ---- eval=F, include=F, comment=NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  Superlearner could alo be used to fit a more flexible adjustment model. Code is provided below but not run to save computing time.
#  # pre-screen the covariates for those associated with the outcome (LR test P<0.2)
#  # see Wprescreen() and design.matrix() in the base functions
#  Wscreen <- Wprescreen(Y=LAZ$Y,Ws=LAZ[,5:ncol(LAZ)],family="gaussian")
#  Wselect <- subset(LAZ,select=Wscreen)
#  Wselect <- design.matrix(Wselect)
#  
#  # algorithmic fit of the outcome as a function of selected covariates
#  #set.seed(209448)
#  set.seed(589320)
#  SLfit1 <- SuperLearner(Y=LAZ$Y,X=Wselect,id=LAZ$id,
#                         family="gaussian",
#                         SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
#                         )
#  SLfit1
#  LAZ$pY <- as.vector(predict(SLfit1)$pred)
#  LAZ$r <- LAZ$Y-LAZ$pY
#  
#  
#  # Hypothesis 1 permutation tests
#  permute.super.C.S <- washb_permute(Y=SLd$r,tr=SLd$tr,block=SLd$block,contrast=c("Control","Sanitation"),seed=1241353)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_prescreen(Y=ad$diar7d,Ws=W,family="binomial", pval=0.2, print=TRUE)

## ---- eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glm.C.N.byChildType$lincom

## ---- eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Create lc vector of 0's equal in length to the number of coefficients from the glm model.
lc=rep(0,nrow(glm.C.N.byChildType$fit))
#Examine model coefficients (minus the pair-matched block estimates) to determine the position of coefficients to combine.
glm.C.N.byChildType$fit[1:3,]
  #Replace the second position in the vector with 1 (the position of the treatment coefficient in the model)
lc[2]<-1
  #Run the lincom function and compare output to the treatment effect from the GLM model.
washb_lincom(lc=lc,fit=glm.C.N.byChildType$fit,vcv=glm.C.N.byChildType$vcv, measure="RR") 

## ---- eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Add a 1 at the 4th position in the lc vector to include the 4th coefficient, the interaction term, in the linear combination.
lc[4]<-1
washb_lincom(lc=lc,fit=glm.C.N.byChildType$fit,vcv=glm.C.N.byChildType$vcv, measure="RR") 

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

