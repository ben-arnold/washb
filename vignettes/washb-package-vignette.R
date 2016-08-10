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

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_prescreen(Y=ad$diar7d,Ws=,family="binomial", pval=0.2, print=TRUE)

## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  washb_glm(Y,tr,pair,W=NULL, forcedW=NULL, V=NULL, id,contrast,family="binonial(link='log')", pval=0.2, print=TRUE)

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a W variable containing only "hfiacat" for the unadjusted subgroup analysis:
W_hfiacat <- subset(laz,select=c("hfiacat"))

#Estimate subgroup analysis glm with washb_glm
glm.C.N.byFoodSecurity <- washb_glm(Y=laz$laz,tr=laz$tr,pair=laz$block, W=W_hfiacat, forcedW=NULL, V="hfiacat", id=laz$clusterid, contrast=c("Control","Nutrition"), family="gaussian", print=FALSE)
glm.C.N.byFoodSecurity$lincom


## ---- eval=T, include=F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Use sapply to apply the function across all arm comparisons, and assign results to matrices for hypothesis 1 and hypothesis 2 comparisons:
permute.diff.h1<-t(sapply(h1.contrasts,washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=10000, seed=12345))
rownames(permute.diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")


#Examing the output across treatment arms:
#permute.diff.h1

## ---- eval=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Add a 1 at the 4th position in the lc vector to include the 4th coefficient, the interaction term, in the linear combination.
lc[4]<-1
washb_lincom(lc=lc,fit=glm.C.N.byChildType$fit,vcv=glm.C.N.byChildType$vcv, measure="RR") 

