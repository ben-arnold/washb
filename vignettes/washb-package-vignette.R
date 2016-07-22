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

## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  data(washb_bd_enrol)
#  data(washb_bd_diar)
#  

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

data(washb_bd_diarCleanUnblinded)
ad<-washb_bd_diarCleanUnblinded


## ---- comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h1.contrasts <- list(
  c("Control","Water"),
  c("Control","Sanitation"),
  c("Control","Handwashing"),
  c("Control","WSH"),
  c("Control","Nutrition"),
  c("Control","Nutrition + WSH")
)

## ---- eval=F, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  data(washb_bd_anthro)
#  data(washb_bd_enrol)
#    washb_bd_enrol$svydate <- NULL
#    washb_bd_enrol$month <- NULL
#  ad <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
#  ad <- subset(ad,svy==2)
#  ad <- subset(ad,tchild=="Target child")
#  
#  # Drop children with extreme LAZ values
#  ad <- subset(ad,laz_x!=1)
#  
#  ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
#  ad$month <- factor(ad$month)
#  ad <- ad[order(ad$block,ad$clusterid,ad$dataid,ad$childid),]
#  
#  Ws <- subset(ad,select=c("fracode","month","aged","sex","birthord","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))
#  

## ---- eval=T, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Load unblinded data into the "ad" object from sysdata
#ad<-loadUnblindedData("washb_bd_diarCleanUnblinded")
data(washb_bd_anthroCleanUnblinded)
ad<-washb_bd_anthroCleanUnblinded
  
Ws <- subset(ad,select=c("fracode","month","aged","sex","birthord","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))


