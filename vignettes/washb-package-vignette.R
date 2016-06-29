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

## ---- results="hide", cache=TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

## ---- results = "hide" , cache=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------

Ws <- subset(ad,select=c("fracode","month","agedays","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))


## ---- warning=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------
prescreened_varnames<-washb_prescreen(Y=ad$diar7d,Ws,family="binomial")

## ---- warning=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------
prescreened_varnames
prescreened_vars <- subset(Ws,select=prescreened_varnames)
#Examine the first five observations of the first selected variable:
prescreened_vars[1:5,1]


## ---- echo=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
washb_mean(Y=ad$momage,id=ad$clusterid,print=TRUE)


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

## ---- warning=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------
CvW<-washb_MH.pooled(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RR")
#Exponentiate coefficients to calculate the prevalence ratio:
print(exp(CvW))

## ---- warning=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------
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

## ---- warning=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------

unadj.glm.h1 <- t(sapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, family=binomial(link='log')))

#unadj.glm.h2 <- t(sapply(h2.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, family=binomial(link='log')))


## ---- warning=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------

adj.glm.h1 <- t(sapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, family=binomial(link='log')))

#adj.glm.h2 <- t(sapply(h2.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, family=binomial(link='log')))


## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  data(washb_bd_anthro)
#  data(washb_bd_enrol)
#  
#  ad <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)

## ---- include=FALSE, eval=TRUE, cache=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
load(file.path(extPath, "washb_bd_enrol.Rdata"))  
load(file.path(extPath, "washb_bd_anthro.Rdata"))  
  washb_bd_enrol$svydate <- NULL
  washb_bd_enrol$month <- NULL
ad <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
ad <- subset(ad,svy==2)
ad <- subset(ad,tchild=="Target child")

# Drop children with extreme LAZ values
ad <- subset(ad,laz_x!=1)

ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
ad$month <- factor(ad$month)
ad <- ad[order(ad$block,ad$clusterid,ad$dataid,ad$childid),]

Ws <- subset(ad,select=c("fracode","month","aged","sex","birthord","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))

## ---- cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Run washb_paired.ttest on water vs. control arm comparison
washb_paired.ttest(Y=ad$laz,tr=ad$tr,strat=ad$block, contrast=c("Control","Water"))

#Use sapply to apply across all contrasts
diff.h1LAZ <- t(sapply(h1.contrasts,washb_paired.ttest,Y=ad$laz,tr=ad$tr,strat=ad$block))
rownames(diff.h1LAZ) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
round(print(diff.h1LAZ),3)

## ---- warning=FALSE, message=FALSE, eval=TRUE, cache=TRUE---------------------------------------------------------------------------------------------------------------------------------------------
####Unadjusted GLM

unadj.glm.h1LAZ <- t(sapply(h1.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, family="gaussian"))

unadj.glm.h2LAZ <- t(sapply(h2.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, family="gaussian"))


## ---- warning=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------

adj.glm.h1LAZ <- t(sapply(h1.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, family="gaussian"))

adj.glm.h2LAZ <- t(sapply(h2.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, family="gaussian"))


