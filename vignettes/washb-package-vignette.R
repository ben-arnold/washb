## ----set-options, echo=FALSE, cache=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)

## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE, results = "hide"---------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(washb)
rm(list=ls())

## ----echo=FALSE, results = "hide"---------------------------------------------------------------------------------------------------------------------------------------------------------------------
 baseLoc<- system.file(package="washb")  
 extPath<- file.path(baseLoc, "data")

 load(file.path(extPath, "washb_bd_enrol.Rdata"))  
 load(file.path(extPath, "washb_bd_diar.Rdata"))  

## ---- results = "hide", tidy=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop svydate and month because they are superceded in the child level diarrhea data
  washb_bd_enrol$svydate <- NULL
  washb_bd_enrol$month <- NULL

# merge the baseline dataset to the follow-up dataset
ad <- merge(washb_bd_enrol,washb_bd_diar,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
dim(washb_bd_diar)
dim(ad)

# subset to the relevant measurement
# Year 1 or Year 2
table(ad$svy)
ad <- subset(ad,svy==1|svy==2)
dim(ad)

#subset the diarrhea to children <36 mos at enrollment
### (exlude new births that are not target children)
dim(ad)
table(ad$sibnewbirth)
ad <- subset(ad,sibnewbirth==0)
dim(ad)
table(ad$gt36mos)
ad <- subset(ad,gt36mos==0)

# Exclude children with missing data
table(ad$tchild,is.na(ad$diar7d),ad$svy)
ad <- subset(ad,!is.na(ad$diar7d))

#Re-order the tr factor for convenience
ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

#Ensure that month is coded as a factor
ad$month <- factor(ad$month)

#Sort the data for perfect replication when using V-fold cross-validation
ad <- ad[order(ad$block,ad$clusterid,ad$dataid,ad$childid),]

## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Ws <- subset(ad,select=c("fracode","month","agedays","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
washb_prescreen(Y=ad$diar7d,Ws,family="poisson")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h1.contrasts <- list(
  c("Control","Water"),
  c("Control","Sanitation"),
  c("Control","Handwashing"),
  c("Control","WSH"),
  c("Control","Nutrition"),
  c("Control","Nutrition + WSH")
)

# unadjusted estimates (mantel-haenszel)
diff.h1 <- t(sapply(h1.contrasts,washb_ITT.unadj,Y=ad$diar7d,tr=ad$tr,strat=ad$block,binomial=TRUE,measure="RD"))
rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")

