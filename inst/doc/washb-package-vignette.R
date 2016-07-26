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

## ---- eval=T----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(washb_bd_enrol)
data(washb_bd_diar)
data(washb_bd_anthro)


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


## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_prescreen(Y=ad$diar7d,Ws=,family="binomial", pval=0.2, print=TRUE)

## ---- results = "hide" , cache=TRUE, comment=NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Subset dataset to covariates to be screened 
Ws_diar <- subset(ad,select=c("month","agedays","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))

#Subset the LAZ dataset to covariates to be screened for inclusion in adjusted glm models
Ws_laz <- subset(laz,select=c("month","aged","sex","birthord","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prescreened_varnames<-washb_prescreen(Y=ad$diar7d,Ws_diar,family="binomial", pval=0.2)

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prescreened_varnames
prescreened_vars <- subset(Ws_diar,select=prescreened_varnames)
#Examine the first five observations of the second selected variable, child age in days:
prescreened_vars[1:5,2]


## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_glm(Y,tr,pair,W=NULL, forcedW=NULL, V=NULL, id,contrast,family="binonial(link='log')", pval=0.2, print=TRUE)

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Diar.glm.C.S <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'))

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Diar.glm.C.S$RDfit[1:2,]

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
LAZ.glm.C.S <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'))

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
adj.Diar.glm.C.S <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws_diar, id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'))

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glm.C.S <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws_diar, forcedW=c("agedays","sex"), id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'))

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a W variable containing "tchild" and other potential covariates:
W_tchild <- subset(ad,select=c("tchild"))

#Estimate subgroup analysis glm with washb_glm
glm.C.S.byChildType <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=W_tchild, V="tchild", id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'), print=FALSE)

#Examine the treatment effect across subgroups with `objectname'$lincom
glm.C.S.byChildType$lincom


## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a W variable containing only "hfiacat" for the unadjusted subgroup analysis:
W_hfiacat <- subset(laz,select=c("hfiacat"))

#Estimate subgroup analysis glm with washb_glm
glm.C.S.byFoodSecurity <- washb_glm(Y=laz$laz,tr=laz$tr,pair=laz$block, W=W_hfiacat, forcedW=NULL, V="hfiacat", id=laz$clusterid, contrast=c("Control","Sanitation"), family="gaussian", print=FALSE)
glm.C.S.byFoodSecurity$lincom


## ---- include=FALSE, eval=F, cache=T----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #Below, run the diar and LAZ primary outcome and subgroup analysis for all contrasts. Not included in the document, but output in the table at the bottom of the document.
#  
#  #Diar
#  unadj.glm.h1 <- t(sapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$bloc, W=NULL, forcedW=NULL, V=NULL, id=ad$clusterid, family=binomial(link='log'), print=FALSE))
#  adj.glm.h1 <- t(sapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws_diar, forcedW=NULL, V=NULL, id=ad$clusterid, family=binomial(link='log'), print=FALSE))
#  glm.byChildType <- lapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=W_tchild, forcedW=NULL, V="tchild", id=ad$clusterid, family=binomial(link='log'), print=FALSE)
#  
#  
#  #LAZ Unadjusted GLM
#  unadj.glm.h1LAZ <- lapply(h1.contrasts,washb_glm,Y=laz$laz,tr=laz$tr,pair=laz$block, W=NULL,forcedW=NULL, V=NULL, id=laz$clusterid, family="gaussian",print=FALSE)
#  #Adjusted GLM
#  adj.glm.h1LAZ <- lapply(h1.contrasts,washb_glm,Y=laz$laz,tr=laz$tr,pair=laz$block, W=Ws_laz,forcedW=NULL, V=NULL, id=laz$clusterid, family="gaussian",print=FALSE)
#  #Unadjusted subgroup:
#  unadj.glm.byFoodSecurity <- lapply(h1.contrasts,washb_glm,Y=laz$laz,tr=laz$tr,pair=laz$block, W=W_hfiacat, forcedW=NULL, V="hfiacat", id=laz$clusterid, family="gaussian", print=FALSE)
#  
#  

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Sanitation"), strat=ad$block,measure="RR")

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Sanitation"), strat=ad$block,measure="RD")

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create vector of contrasts  for the pre-specified hypothesis 1 (each intervention arm vs. control) to facilitate comparisons between arms.
h1.contrasts <- list(c("Control","Water"), c("Control","Sanitation"), c("Control","Handwashing"), c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))

#Apply sapply to run the function on each comparison in the h1.contrasts list
diff.h1 <- t(sapply(h1.contrasts,washb_mh,Y=ad$diar7d,tr=ad$tr,strat=ad$block,measure="RR"))
rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
print(diff.h1)

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Diar.glm.C.S$TR

## ---- cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Run washb_ttest on Sanitation vs. control arm comparison
washb_ttest(Y=laz$laz,tr=laz$tr,strat=laz$block, contrast=c("Control","Sanitation"))

#Use sapply to apply across all contrasts
diff.h1LAZ <- t(sapply(h1.contrasts,washb_ttest,Y=laz$laz,tr=laz$tr,strat=laz$block))
rownames(diff.h1LAZ) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
print(diff.h1LAZ)

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
LAZ.glm.C.S$TR

## ---- eval=TRUE, warning=FALSE, message=FALSE, cache=TRUE, tidy=TRUE, comment=NA--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
permute.C.S <- washb_permute(Y=ad$diar7d, tr=ad$tr, pair=ad$block, contrast=c("Control","Sanitation"), nreps=100000, seed=242524)

## ---- eval=F, include=F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #Use sapply to apply the function across all arm comparisons, and assign results to matrices for hypothesis 1 and hypothesis 2 comparisons:
#  #permute.diff.h1<-t(sapply(h1.contrasts,washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=10000, seed=12345))
#  #rownames(permute.diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
#  
#  
#  #Examing the output across treatment arms:
#  #permute.diff.h1

## ---- eval=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glm.C.S.byChildType$lincom

## ---- eval=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Create lc vector of 0's equal in length to the number of coefficients from the glm model.
lc=rep(0,nrow(glm.C.S.byChildType$fit))
#Examine model coefficients (minus the pair-matched block estimates) to determine the position of coefficients to combine.
glm.C.S.byChildType$fit[1:6,]
  #Replace the second position in the vector with 1 (the position of the treatment coefficient in the model)
lc[2]<-1
  #Run the lincom function and compare output to the treatment effect from the GLM model.
washb_lincom(lc=lc,fit=glm.C.S.byChildType$fit,vcv=glm.C.S.byChildType$vcv, measure="RR") 

## ---- eval=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Add a 1 at the 4th position in the lc vector to include the 4th coefficient, the interaction term, in the linear combination.
lc[4]<-1
washb_lincom(lc=lc,fit=glm.C.S.byChildType$fit,vcv=glm.C.S.byChildType$vcv, measure="RR") 

#Alternatively, use a character vector of coefficient names rather than an index vector of variable positions.
varlist<-c("VTarget child","trSanitation:VTarget child")
washb_lincom(varlist=varlist,fit=glm.C.S.byChildType$RDfit,vcv=glm.C.S.byChildType$vcvRD, measure="RR") 

## ---- eval=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Calculate risk difference using the risk difference output from the washb_glm function:
washb_lincom(lc=lc,fit=glm.C.S.byChildType$RDfit,vcv=glm.C.S.byChildType$vcvRD, measure="RD") 
#Compare to RD by subgroup from the washb_glm function:
glm.C.S.byChildType$lincomRD

## ---- include=FALSE, eval=F, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  
#  
#  #Table of Primary Outcome Results
#  
#  ##7-day diarrheal disease recall outcome
#  
#  Contrast v. control | Estimator | PR | 95% CI | SE logPR | P-value
#  --------------------|-----------|----|--------|----------|--------
#  Water | Unadjusted MH  | `r round(diff.h1[1,1],2)` | `r round((diff.h1[1,2:3]),2)` | `r round(diff.h1[1,5],2)` | `r round(diff.h1[1,7],4)`
#  Water | Unadjusted GLM |  `r round(unadj.glm.h1[[1]][1],2)` |(`r round(unadj.glm.h1[[1]][2:3],2)`) | `r round(unadj.glm.h1[[1]][5],4)` | `r round(unadj.glm.h1[[1]][7],4)`
#  Water | Adjusted GLM | `r round(adj.glm.h1[[1]][1],2)` |(`r round(adj.glm.h1[[1]][2:3],2)`) | `r round(adj.glm.h1[[1]][5],4)` | `r round(adj.glm.h1[[1]][7],4)`
#  Water | Adjusted GLM + TMLE |  | | |
#  Water | Adjusted SL + TMLE |  | | |
#  Water | Wilcoxon  permutation test |  | | |`r permute.diff.h1[1]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  Sanitation | Unadjusted MH  | `r round(diff.h1[2,1],2)` | `r round((diff.h1[2,2:3]),2)` | `r round(diff.h1[2,5],2)` | `r diff.h1[2,7]`
#  Sanitation | Unadjusted GLM |  `r round(unadj.glm.h1[[2]][1],2)` |(`r round(unadj.glm.h1[[2]][2:3],2)`) | `r round(unadj.glm.h1[[2]][5],4)` | `r unadj.glm.h1[[2]][7]`
#  Sanitation | Adjusted GLM | `r round(adj.glm.h1[[2]][1],2)` |(`r round(adj.glm.h1[[2]][2:3],2)`) | `r round(adj.glm.h1[[2]][5],4)` | `r (adj.glm.h1[[2]][7])`
#  Sanitation | Adjusted GLM + TMLE |  | | |-
#  Sanitation | Adjusted SL + TMLE |  | | |-
#  Sanitation | Wilcoxon  permutation test |  | | |`r permute.diff.h1[2]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  Handwashing | Unadjusted MH  | `r round(diff.h1[3,1],2)` | `r round((diff.h1[3,2:3]),2)` | `r round(diff.h1[3,5],2)` | `r round(diff.h1[3,7],4)`
#  Handwashing | Unadjusted GLM |  `r round(unadj.glm.h1[[3]][1],2)` |(`r round(unadj.glm.h1[[3]][2:3],2)`) | `r round(unadj.glm.h1[[3]][5],4)` | `r round(unadj.glm.h1[[3]][7],4)`
#  Handwashing | Adjusted GLM | `r round(adj.glm.h1[[3]][1],2)` |(`r round(adj.glm.h1[[3]][2:3],2)`) | `r round(adj.glm.h1[[3]][5],4)` | `r round(adj.glm.h1[[3]][7],4)`
#  Handwashing | Adjusted GLM + TMLE |  | | |
#  Handwashing | Adjusted SL + TMLE |  | | |
#  Handwashing | Wilcoxon  permutation test |  | | |`r permute.diff.h1[3]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  Nutrition | Unadjusted MH  | `r round(diff.h1[4,1],2)` | `r round((diff.h1[4,2:3]),2)` | `r round(diff.h1[4,5],2)` | `r round(diff.h1[4,7],4)`
#  Nutrition | Unadjusted GLM |  `r round(unadj.glm.h1[[4]][1],2)` |(`r round(unadj.glm.h1[[4]][2:3],2)`) | `r round(unadj.glm.h1[[4]][5],4)` | `r round(unadj.glm.h1[[4]][7],4)`
#  Nutrition | Adjusted GLM | `r round(adj.glm.h1[[4]][1],2)` |(`r round(adj.glm.h1[[4]][2:3],2)`) | `r round(adj.glm.h1[[4]][5],4)` | `r round(adj.glm.h1[[4]][7],4)`
#  Nutrition | Adjusted GLM + TMLE |  | | |
#  Nutrition | Adjusted SL + TMLE |  | | |
#  Nutrition | Wilcoxon  permutation test |  | | |`r permute.diff.h1[4]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  WSH | Unadjusted MH  | `r round(diff.h1[5,1],2)` | `r round((diff.h1[5,2:3]),2)` | `r round(diff.h1[5,5],2)` | `r round(diff.h1[5,7],4)`
#  WSH | Unadjusted GLM |  `r round(unadj.glm.h1[[5]][1],2)` |(`r round(unadj.glm.h1[[5]][2:3],2)`) | `r round(unadj.glm.h1[[5]][5],4)` | `r round(unadj.glm.h1[[5]][7],4)`
#  WSH | Adjusted GLM | `r round(adj.glm.h1[[5]][1],2)` |(`r round(adj.glm.h1[[5]][2:3],2)`) | `r round(adj.glm.h1[[5]][5],4)` | `r round(adj.glm.h1[[5]][7],4)`
#  WSH | Adjusted GLM + TMLE |  | | |
#  WSH | Adjusted SL + TMLE |  | | |
#  WSH | Wilcoxon  permutation test |  | | |`r permute.diff.h1[5]`
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  WSH+Nutrition | Unadjusted MH  | `r round(diff.h1[6,1],2)` | `r round((diff.h1[6,2:3]),2)` | `r round(diff.h1[6,5],2)` | `r round(diff.h1[6,7],4)`
#  WSH+Nutrition | Unadjusted GLM |  `r round(unadj.glm.h1[[6]][1],2)` |(`r round(unadj.glm.h1[[6]][2:3],2)`) | `r round(unadj.glm.h1[[6]][5],4)` | `r round(unadj.glm.h1[[6]][7],4)`
#  WSH+Nutrition | Adjusted GLM | `r round(adj.glm.h1[[6]][1],2)` |(`r round(adj.glm.h1[[6]][2:3],2)`) | `r round(adj.glm.h1[[6]][5],4)` | `r round(adj.glm.h1[[6]][7],4)`
#  WSH+Nutrition | Adjusted GLM + TMLE |  | | |
#  WSH+Nutrition | Adjusted SL + TMLE |  | | |
#  WSH+Nutrition | Wilcoxon  permutation test |  | | |`r permute.diff.h1[6]`
#  
#  <br>
#  <br>
#  <br>
#  
#  ##Length-for-Age Z-score outcome
#  
#  Contrast v. control | Estimator | Difference | 95% CI | t-stat/SE | P-value
#  --------------------|-----------|------------|--------|-----------|--------
#  Water | Unadjusted ttest | `r round(diff.h1LAZ[1,1],2)` |(`r round((diff.h1LAZ[1,2:3]),2)`) | t-stat:  `r round(diff.h1LAZ[1,4],2)`| `r round(diff.h1LAZ[1,5],3)`
#  Water | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[1]]$TR[1],2)` |(`r round(unadj.glm.h1LAZ[[1]]$TR[2:3],2)`) |SE:  `r round(unadj.glm.h1LAZ[[1]]$TR[4],2)` | `r round(unadj.glm.h1LAZ[[1]]$TR[6],3)`
#  Water | Adjusted GLM | `r round(adj.glm.h1LAZ[[1]]$TR[1],2)` |(`r round(adj.glm.h1LAZ[[1]]$TR[2:3],2)`) |SE:  `r round(adj.glm.h1LAZ[[1]]$TR[4],2)` | `r round(adj.glm.h1LAZ[[1]]$TR[6],3)`
#  Water | Adjusted GLM + TMLE |  | | |
#  Water | Adjusted SL + TMLE |  | | |
#  Water | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  Sanitation | Unadjusted ttest | `r round(diff.h1LAZ[2,1],2)` |(`r round((diff.h1LAZ[2,2:3]),2)`) | t-stat:  `r round(diff.h1LAZ[2,4],2)`| `r round(diff.h1LAZ[2,5],3)`
#  Sanitation | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[2]]$TR[1],2)` |(`r round(unadj.glm.h1LAZ[[2]]$TR[2:3],2)`) |SE:  `r round(unadj.glm.h1LAZ[[2]]$TR[4],2)` | `r round(unadj.glm.h1LAZ[[2]]$TR[6],3)`
#  Sanitation | Adjusted GLM | `r round(adj.glm.h1LAZ[[2]]$TR[1],2)` |(`r round(adj.glm.h1LAZ[[2]]$TR[2:3],2)`) |SE:  `r round(adj.glm.h1LAZ[[2]]$TR[4],2)` | `r round(adj.glm.h1LAZ[[2]]$TR[6],3)`
#  Sanitation | Adjusted GLM + TMLE |  | | |
#  Sanitation | Adjusted SL + TMLE |  | | |
#  Sanitation | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  Handwashing | Unadjusted ttest | `r round(diff.h1LAZ[3,1],2)` |(`r round((diff.h1LAZ[3,2:3]),2)`) | t-stat:  `r round(diff.h1LAZ[3,4],2)`| `r round(diff.h1LAZ[3,5],3)`
#  Handwashing | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[3]]$TR[1],2)` |(`r round(unadj.glm.h1LAZ[[3]]$TR[2:3],2)`) |SE:  `r round(unadj.glm.h1LAZ[[3]]$TR[4],2)` | `r round(unadj.glm.h1LAZ[[3]]$TR[6],3)`
#  Handwashing | Adjusted GLM | `r round(adj.glm.h1LAZ[[3]]$TR[1],2)` |(`r round(adj.glm.h1LAZ[[3]]$TR[2:3],2)`) |SE:  `r round(adj.glm.h1LAZ[[3]]$TR[4],2)` | `r round(adj.glm.h1LAZ[[3]]$TR[6],3)`
#  Handwashing | Adjusted GLM + TMLE |  | | |
#  Handwashing | Adjusted SL + TMLE |  | | |
#  Handwashing | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  Nutrition | Unadjusted ttest | `r round(diff.h1LAZ[4,1],2)` |(`r round((diff.h1LAZ[4,2:3]),2)`) | t-stat:  `r round(diff.h1LAZ[4,4],2)`| `r round(diff.h1LAZ[4,5],3)`
#  Nutrition | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[4]]$TR[1],2)` |(`r round(unadj.glm.h1LAZ[[4]]$TR[2:3],2)`) |SE:  `r round(unadj.glm.h1LAZ[[4]]$TR[4],2)` | `r round(unadj.glm.h1LAZ[[4]]$TR[6],3)`
#  Nutrition | Adjusted GLM | `r round(adj.glm.h1LAZ[[5]]$TR[1],2)` |(`r round(adj.glm.h1LAZ[[5]]$TR[2:3],2)`) |SE:  `r round(adj.glm.h1LAZ[[5]]$TR[4],2)` | `r round(adj.glm.h1LAZ[[5]]$TR[6],3)`
#  Nutrition | Adjusted GLM + TMLE |  | | |
#  Nutrition | Adjusted SL + TMLE |  | | |
#  Nutrition | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  WSH | Unadjusted ttest | `r round(diff.h1LAZ[5,1],2)` |(`r round((diff.h1LAZ[5,2:3]),2)`) | t-stat:  `r round(diff.h1LAZ[5,4],2)`| `r round(diff.h1LAZ[5,5],3)`
#  WSH | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[5]]$TR[1],2)` |(`r round(unadj.glm.h1LAZ[[5]]$TR[2:3],2)`) |SE:  `r round(unadj.glm.h1LAZ[[5]]$TR[4],2)` | `r round(unadj.glm.h1LAZ[[5]]$TR[6],3)`
#  WSH | Adjusted GLM | `r round(adj.glm.h1LAZ[[5]]$TR[1],2)` |(`r round(adj.glm.h1LAZ[[5]]$TR[2:3],2)`) |SE:  `r round(adj.glm.h1LAZ[[5]]$TR[4],2)` | `r round(adj.glm.h1LAZ[[5]]$TR[6],3)`
#  WSH | Adjusted GLM + TMLE |  | | |
#  WSH | Adjusted SL + TMLE |  | | |
#  WSH | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  WSH+Nutrition | Unadjusted ttest | `r round(diff.h1LAZ[6,1],2)` |(`r round((diff.h1LAZ[6,2:3]),2)`) | t-stat:  `r round(diff.h1LAZ[6,4],2)`| `r round(diff.h1LAZ[6,5],3)`
#  WSH+Nutrition | Unadjusted GLM | `r round(unadj.glm.h1LAZ[[6]]$TR[1],2)` |(`r round(unadj.glm.h1LAZ[[6]]$TR[2:3],2)`) |SE:  `r round(unadj.glm.h1LAZ[[6]]$TR[4],2)` | `r round(unadj.glm.h1LAZ[[6]]$TR[6],3)`
#  WSH+Nutrition | Adjusted GLM | `r round(adj.glm.h1LAZ[[6]]$TR[1],2)` |(`r round(adj.glm.h1LAZ[[6]]$TR[2:3],2)`) |SE:  `r round(adj.glm.h1LAZ[[6]]$TR[4],2)` | `r round(adj.glm.h1LAZ[[6]]$TR[6],3)`
#  WSH+Nutrition | Adjusted GLM + TMLE |  | | |
#  WSH+Nutrition | Adjusted SL + TMLE |  | | |
#  WSH+Nutrition | Wilcoxon  permutation test |  | | |
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
#  Water |`r glm.byChildType[[1]]$lincom[1,1]` | `r glm.byChildType[[1]]$lincom[1,2]` | `r round(glm.byChildType[[1]]$lincom[1,4:5],2)` | `r glm.byChildType[[1]]$lincom[1,7]`
#  - |`r glm.byChildType[[1]]$lincom[2,1]`  | `r glm.byChildType[[1]]$lincom[2,2]` | `r round(glm.byChildType[[1]]$lincom[2,4:5],2)` | `r glm.byChildType[[1]]$lincom[2,7]`
#  Sanitation |`r glm.byChildType[[2]]$lincom[1,1]` | `r glm.byChildType[[2]]$lincom[1,2]` | `r round(glm.byChildType[[2]]$lincom[1,4:5],2)` | `r glm.byChildType[[2]]$lincom[1,7]`
#  - |`r glm.byChildType[[2]]$lincom[2,1]`  | `r glm.byChildType[[2]]$lincom[2,2]` | `r round(glm.byChildType[[2]]$lincom[2,4:5],2)` | `r glm.byChildType[[2]]$lincom[2,7]`
#  Handwashing |`r glm.byChildType[[3]]$lincom[1,1]` | `r glm.byChildType[[3]]$lincom[1,2]` | `r round(glm.byChildType[[3]]$lincom[1,4:5],2)` | `r glm.byChildType[[3]]$lincom[1,7]`
#  - |`r glm.byChildType[[3]]$lincom[2,1]`  | `r glm.byChildType[[3]]$lincom[2,2]` | `r round(glm.byChildType[[3]]$lincom[2,4:5],2)` | `r glm.byChildType[[3]]$lincom[2,7]`
#  Nutrition |`r glm.byChildType[[4]]$lincom[1,1]` | `r glm.byChildType[[4]]$lincom[1,2]` | `r round(glm.byChildType[[4]]$lincom[1,4:5],2)` | `r glm.byChildType[[4]]$lincom[1,7]`
#  - |`r glm.byChildType[[4]]$lincom[2,1]`  | `r glm.byChildType[[4]]$lincom[2,2]` | `r round(glm.byChildType[[4]]$lincom[2,4:5],2)` | `r glm.byChildType[[4]]$lincom[2,7]`
#  WSH |`r glm.byChildType[[5]]$lincom[1,1]` | `r glm.byChildType[[5]]$lincom[1,2]` | `r round(glm.byChildType[[5]]$lincom[1,4:5],2)` | `r glm.byChildType[[5]]$lincom[1,7]`
#  - |`r glm.byChildType[[5]]$lincom[2,1]`  | `r glm.byChildType[[5]]$lincom[2,2]` | `r round(glm.byChildType[[5]]$lincom[2,4:5],2)` | `r glm.byChildType[[5]]$lincom[2,7]`
#  WSH+Nutrition |`r glm.byChildType[[6]]$lincom[1,1]` | `r glm.byChildType[[6]]$lincom[1,2]` | `r round(glm.byChildType[[6]]$lincom[1,4:5],2)` | `r glm.byChildType[[6]]$lincom[1,7]`
#  - |`r glm.byChildType[[6]]$lincom[2,1]`  | `r glm.byChildType[[6]]$lincom[2,2]` | `r round(glm.byChildType[[6]]$lincom[2,4:5],2)` | `r glm.byChildType[[6]]$lincom[2,7]`
#  
#  
#  
#  <br>
#  <br>
#  <br>
#  
#  ##Risk difference between treatments and control, stratified into target children and siblings.
#  Contrast v. control | Child Type | Coefficient |  95% CI  |  P-value
#  --------------------|------------------------|-------------|----------|---------
#  Water |`r glm.byChildType[[1]]$lincomRD[1,1]` | `r glm.byChildType[[1]]$lincomRD[1,2]` | `r round(glm.byChildType[[1]]$lincomRD[1,4:5],2)` | `r glm.byChildType[[1]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[1]]$lincomRD[2,1]`  | `r glm.byChildType[[1]]$lincomRD[2,2]` | `r round(glm.byChildType[[1]]$lincomRD[2,4:5],2)` | `r glm.byChildType[[1]]$lincomRD[2,7]`
#  Sanitation |`r glm.byChildType[[2]]$lincomRD[1,1]` | `r glm.byChildType[[2]]$lincomRD[1,2]` | `r round(glm.byChildType[[2]]$lincomRD[1,4:5],2)` | `r glm.byChildType[[2]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[2]]$lincomRD[2,1]`  | `r glm.byChildType[[2]]$lincomRD[2,2]` | `r round(glm.byChildType[[2]]$lincomRD[2,4:5],2)` | `r glm.byChildType[[2]]$lincomRD[2,7]`
#  Handwashing |`r glm.byChildType[[3]]$lincomRD[1,1]` | `r glm.byChildType[[3]]$lincomRD[1,2]` | `r round(glm.byChildType[[3]]$lincomRD[1,4:5],2)` | `r glm.byChildType[[3]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[3]]$lincomRD[2,1]`  | `r glm.byChildType[[3]]$lincomRD[2,2]` | `r round(glm.byChildType[[3]]$lincomRD[2,4:5],2)` | `r glm.byChildType[[3]]$lincomRD[2,7]`
#  Nutrition |`r glm.byChildType[[4]]$lincomRD[1,1]` | `r glm.byChildType[[4]]$lincomRD[1,2]` | `r round(glm.byChildType[[4]]$lincomRD[1,4:5],2)` | `r glm.byChildType[[4]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[4]]$lincomRD[2,1]`  | `r glm.byChildType[[4]]$lincomRD[2,2]` | `r round(glm.byChildType[[4]]$lincomRD[2,4:5],2)` | `r glm.byChildType[[4]]$lincomRD[2,7]`
#  WSH |`r glm.byChildType[[5]]$lincomRD[1,1]` | `r glm.byChildType[[5]]$lincomRD[1,2]` | `r round(glm.byChildType[[5]]$lincomRD[1,4:5],2)` | `r glm.byChildType[[5]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[5]]$lincomRD[2,1]`  | `r glm.byChildType[[5]]$lincomRD[2,2]` | `r round(glm.byChildType[[5]]$lincomRD[2,4:5],2)` | `r glm.byChildType[[5]]$lincomRD[2,7]`
#  WSH + Nutrition |`r glm.byChildType[[6]]$lincomRD[1,1]` | `r glm.byChildType[[6]]$lincomRD[1,2]` | `r round(glm.byChildType[[6]]$lincomRD[1,4:5],2)` | `r glm.byChildType[[6]]$lincomRD[1,7]`
#  - |`r glm.byChildType[[6]]$lincomRD[2,1]`  | `r glm.byChildType[[6]]$lincomRD[2,2]` | `r round(glm.byChildType[[6]]$lincomRD[2,4:5],2)` | `r glm.byChildType[[6]]$lincomRD[2,7]`
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
#  Contrast v. control | Food Security Subgroup | Coefficient |  95% CI  |  P-value
#  --------------------|------------------------|-------------|----------|---------
#  Water |`r unadj.glm.byFoodSecurity[[1]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[1]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[1]]$lincom[1,4:5],2)` | `r unadj.glm.byFoodSecurity[[1]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[1]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[1]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[1]]$lincom[2,4:5],2)` | `r unadj.glm.byFoodSecurity[[1]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[1]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[1]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[1]]$lincom[3,4:5],2)` | `r unadj.glm.byFoodSecurity[[1]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[1]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[1]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[1]]$lincom[4,4:5],2)` | `r unadj.glm.byFoodSecurity[[1]]$lincom[4,7]`
#  Sanitation |`r unadj.glm.byFoodSecurity[[2]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[2]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[2]]$lincom[1,4:5],2)` | `r unadj.glm.byFoodSecurity[[2]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[2]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[2]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[2]]$lincom[2,4:5],2)` | `r unadj.glm.byFoodSecurity[[2]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[2]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[2]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[2]]$lincom[3,4:5],2)` | `r unadj.glm.byFoodSecurity[[2]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[2]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[2]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[2]]$lincom[4,4:5],2)` | `r unadj.glm.byFoodSecurity[[2]]$lincom[4,7]`
#  Handwashing |`r unadj.glm.byFoodSecurity[[3]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[3]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[3]]$lincom[1,4:5],2)` | `r unadj.glm.byFoodSecurity[[3]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[3]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[3]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[3]]$lincom[2,4:5],2)` | `r unadj.glm.byFoodSecurity[[3]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[3]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[3]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[3]]$lincom[3,4:5],2)` | `r unadj.glm.byFoodSecurity[[3]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[3]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[3]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[3]]$lincom[4,4:5],2)` | `r unadj.glm.byFoodSecurity[[3]]$lincom[4,7]`
#  Nutrition |`r unadj.glm.byFoodSecurity[[4]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[4]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[4]]$lincom[1,4:5],2)` | `r unadj.glm.byFoodSecurity[[4]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[4]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[4]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[4]]$lincom[2,4:5],2)` | `r unadj.glm.byFoodSecurity[[4]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[4]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[4]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[4]]$lincom[3,4:5],2)` | `r unadj.glm.byFoodSecurity[[4]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[4]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[4]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[4]]$lincom[4,4:5],2)` | `r unadj.glm.byFoodSecurity[[4]]$lincom[4,7]`
#  WSH |`r unadj.glm.byFoodSecurity[[5]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[5]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[5]]$lincom[1,4:5],2)` | `r unadj.glm.byFoodSecurity[[5]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[5]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[5]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[5]]$lincom[2,4:5],2)` | `r unadj.glm.byFoodSecurity[[5]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[5]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[5]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[5]]$lincom[3,4:5],2)` | `r unadj.glm.byFoodSecurity[[5]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[5]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[5]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[5]]$lincom[4,4:5],2)` | `r unadj.glm.byFoodSecurity[[5]]$lincom[4,7]`
#  WSH + Nutrition |`r unadj.glm.byFoodSecurity[[6]]$lincom[1,1]` | `r unadj.glm.byFoodSecurity[[6]]$lincom[1,2]` | `r round(unadj.glm.byFoodSecurity[[6]]$lincom[1,4:5],2)` | `r unadj.glm.byFoodSecurity[[6]]$lincom[1,7]`
#  - |`r unadj.glm.byFoodSecurity[[6]]$lincom[2,1]`  | `r unadj.glm.byFoodSecurity[[6]]$lincom[2,2]` | `r round(unadj.glm.byFoodSecurity[[6]]$lincom[2,4:5],2)` | `r unadj.glm.byFoodSecurity[[6]]$lincom[2,7]`
#  - |`r unadj.glm.byFoodSecurity[[6]]$lincom[3,1]`  | `r unadj.glm.byFoodSecurity[[6]]$lincom[3,2]` | `r round(unadj.glm.byFoodSecurity[[6]]$lincom[3,4:5],2)` | `r unadj.glm.byFoodSecurity[[6]]$lincom[3,7]`
#  - |`r unadj.glm.byFoodSecurity[[6]]$lincom[4,1]`  | `r unadj.glm.byFoodSecurity[[6]]$lincom[4,2]` | `r round(unadj.glm.byFoodSecurity[[6]]$lincom[4,4:5],2)` | `r unadj.glm.byFoodSecurity[[6]]$lincom[4,7]`
#  
#  
#  <br>
#  <br>
#  <br>
#  

