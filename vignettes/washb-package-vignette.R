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

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  data(washb_bd_enrol)
#  data(washb_bd_diar)
#  

## ---- results="hide", eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

## ---- eval=FALSE, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #To do: merge in file to de-randomized treatment assignment
#  
#  data(washb_bd_diarScrambled)
#  
#  data(treatmentAssignment)
#  #merge datasets
#  merged <- merge(washb_bd_diarScrambled,treatmentAssignment,by=c("index"),all.x=F,all.y=T)
#  #drop randomized treatment assignment
#  
#  ad<-merged

## ---- include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(washb_bd_diarClean)
ad<-washb_bd_diarClean

## ---- comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h1.contrasts <- list(
  c("Control","Water"),
  c("Control","Sanitation"),
  c("Control","Handwashing"),
  c("Control","WSH"),
  c("Control","Nutrition"),
  c("Control","Nutrition + WSH")
)

## ---- eval=FALSE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

## ---- eval=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Extend code to all treatment/control comparisons with lapply
glm.byFoodSecurity <- lapply(h1.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=Wsubgroup, forcedW=NULL, V="hfiacat", id=ad$clusterid, family="gaussian", print=FALSE)

#hfiacat <- subset(ad,select=c("hfiacat","aged"))
#uadj.glm.C.W.byFoodSecurity <- washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=hfiacat, forcedW=NULL, V="hfiacat", id=ad$clusterid, contrast=c("Control","Water"), family="gaussian", print=TRUE)

#unadj.glm.byFoodSecurity <- lapply(h1.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=hfiacat, forcedW=NULL, V="hfiacat", id=ad$clusterid, family="gaussian", print=FALSE)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

