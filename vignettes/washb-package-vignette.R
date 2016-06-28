## ----set-options, echo=FALSE, cache=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)

## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval = FALSE, tidy=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#      install.packages("sandwich")
#      install.packages("lmtest")
#      install.packages("coin")
#      install.packages("plyr")
#      install.packages("metafor")

## ---- results = "hide"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(washb)

## ---- include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 rm(list=ls())
 baseLoc<- system.file(package="washb")  
 extPath<- file.path(baseLoc, "data")

 load(file.path(extPath, "washb_bd_enrol.Rdata"))  
 load(file.path(extPath, "washb_bd_diar.Rdata"))  

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  data(washb_bd_enrol)
#  data(washb_bd_diar)
#  

## ---- results="hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop svydate and month because they are superceded in the child level diarrhea data
  washb_bd_enrol$svydate <- NULL
  washb_bd_enrol$month <- NULL

# merge the baseline dataset to the follow-up dataset
ad <- merge(washb_bd_enrol,washb_bd_diar,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)

# subset to the relevant measurement
# Year 1 or Year 2
ad <- subset(ad,svy==1|svy==2)

#subset the diarrhea to children <36 mos at enrollment
### (exlude new births that are not target children)
ad <- subset(ad,sibnewbirth==0)
ad <- subset(ad,gt36mos==0)

# Exclude children with missing data
ad <- subset(ad,!is.na(ad$diar7d))

#Re-order the tr factor for convenience
ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

#Ensure that month is coded as a factor
ad$month <- factor(ad$month)

#Sort the data for perfect replication when using V-fold cross-validation
ad <- ad[order(ad$block,ad$clusterid,ad$dataid,ad$childid),]

## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Ws <- subset(ad,select=c("fracode","month","agedays","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))


## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
washb_prescreen(Y=ad$diar7d,Ws,family="binomial")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h1.contrasts <- list(
  c("Control","Water"),
  c("Control","Sanitation"),
  c("Control","Handwashing"),
  c("Control","WSH"),
  c("Control","Nutrition"),
  c("Control","Nutrition + WSH")
)
h2.contrasts <- list(
  c("Water","WSH"),
  c("Sanitation","WSH"),
  c("Handwashing","WSH")
)

## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
CvW<-washb_MH.pooled(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RR")
#Exponentiate coefficients to calculate the prevalence ratio:
print(exp(CvW))

## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Hypothesis 1
diff.h1 <- t(sapply(h1.contrasts,washb_MH.pooled,Y=ad$diar7d,tr=ad$tr,strat=ad$block,measure="RR"))
rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
print(exp(diff.h1))

#Hypothesis 2
diff.h2 <- t(sapply(h2.contrasts,washb_MH.pooled,Y=ad$diar7d,tr=ad$tr,strat=ad$block,measure="RR"))
rownames(diff.h2) <- c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
print(exp(diff.h2))

## ---- eval=FALSE, warning=FALSE, message=FALSE, cache=TRUE, tidy=TRUE---------------------------------------------------------------------------------------------------------------------------------
#  permute.diff.h1<-t(sapply(h1.contrasts,washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=10000, seed=12345))
#  rownames(permute.diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
#  permute.diff.h1
#  
#  permute.diff.h2<-t(sapply(h2.contrasts,washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=10000, seed=12345))
#  rownames(permute.diff.h2) <- c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
#  permute.diff.h2

## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------

unadj.glm.h1 <- t(sapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, family="binomial"))

unadj.glm.h2 <- t(sapply(h2.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, family="binomial"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

adj.glm.h1 <- t(sapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, family="binomial"))

adj.glm.h2 <- t(sapply(h2.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, family="binomial"))


## ---- eval=FALSE,  results="hide"---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  load(file.path(extPath, "washb_bd_enrol.Rdata"))
#  load(file.path(extPath, "washb_bd_anthro.Rdata"))
#  
#  # drop svydate and month because they are superceded in the child level diarrhea data
#    washb_bd_enrol$svydate <- NULL
#    washb_bd_enrol$month <- NULL
#  
#  # merge the baseline dataset to the follow-up dataset
#  ad <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
#  dim(washb_bd_anthro)
#  dim(ad)
#  
#  #---------------------------------------
#  # subset to the relevant measurement
#  # Year 1 or Year 2
#  #---------------------------------------
#  table(ad$svy)
#  ad <- subset(ad,svy==2)
#  dim(ad)
#  
#  # subset the anthropometry to target children (excluding siblings)
#  dim(ad)
#  ad <- subset(ad,tchild=="Target child")
#  dim(ad)
#  
#  # Drop children with extreme LAZ values
#  table(ad$laz_x)
#  ad <- subset(ad,laz_x!=1)
#  
#  
#  # Exclude children with missing data (none)
#  table(is.na(ad$laz))
#  
#  # re-order the tr factor for convenience
#  ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
#  
#  # ensure that month is coded as a factor
#  ad$month <- factor(ad$month)
#  
#  # sort the data for perfect replication with jade on the V-fold cross-validation
#  ad <- ad[order(ad$block,ad$clusterid,ad$dataid,ad$childid),]
#  
#  #---------------------------------------
#  # Select covariates with univariate
#  # associations with the outcome of
#  # P<0.2 based on a liklihood ratio test
#  #---------------------------------------
#  
#  # drop due to so many missing values?
#  # asset_clock
#  
#  Ws <- subset(ad,select=c("fracode","month","aged","sex","birthord","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))
#  
#  
#  #---------------------------------------
#  # Estimate adjusted mean differences
#  #---------------------------------------
#  #---------------------------------------
#  # H1: Each intervention arm vs. Control
#  #---------------------------------------
#  
#  #Function:paired.ttest
#  #### unadjusted estimates (paired t-test)

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  diff.h1 <- t(sapply(h1.contrasts,washb_ITT.unadj,Y=ad$laz,tr=ad$tr,strat=ad$block,measure="RD"))
#  rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
#  print(diff.h1)

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Water"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Sanitation"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Handwashing"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","WSH"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition + WSH"), family="gaussian")))

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #print
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Water"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Sanitation"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Handwashing"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","WSH"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition"), family="gaussian")))
#  
#  ((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition + WSH"), family="gaussian")))

