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

## ---- results = "hide" , cache=TRUE, comment=NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Ws <- subset(ad,select=c("fracode","month","agedays","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))


## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prescreened_varnames<-washb_prescreen(Y=ad$diar7d,Ws,family="binomial", pval=0.2)

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prescreened_varnames
prescreened_vars <- subset(Ws,select=prescreened_varnames)
#Examine the first five observations of the first selected variable:
prescreened_vars[1:5,1]


## ---- echo=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MeanMomage<-washb_mean(Y=ad$momage,id=ad$clusterid,print=TRUE)

## ---- comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h1.contrasts <- list(
  c("Control","Water"),
  c("Control","Sanitation"),
  c("Control","Handwashing"),
  c("Control","WSH"),
  c("Control","Nutrition"),
  c("Control","Nutrition + WSH")
)

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RR")

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RD")

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Hypothesis 1
diff.h1 <- t(sapply(h1.contrasts,washb_mh,Y=ad$diar7d,tr=ad$tr,strat=ad$block,measure="RR"))
rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
print(diff.h1)

## ---- eval=TRUE, warning=FALSE, message=FALSE, cache=TRUE, tidy=TRUE, comment=NA--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
permute.C.W <- washb_permute(Y=ad$diar7d, tr=ad$tr, pair=ad$block, contrast=c("Control","Water"), nreps=100000, seed=242524)

## ---- eval=TRUE, results = "hide", warning=FALSE, message=FALSE, cache=TRUE, tidy=TRUE, comment=NA--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

permute.diff.h1<-t(sapply(h1.contrasts,washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=10000, seed=12345))
rownames(permute.diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")

## ---- eval=TRUE, warning=FALSE, message=FALSE, cache=TRUE, tidy=TRUE, comment=NA--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
permute.diff.h1

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glm.C.W <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, id=ad$clusterid, contrast=c("Control","Water"), family=binomial(link='log'))


## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glm.C.W$RDfit

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

unadj.glm.h1 <- t(sapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$bloc, W=NULL, forcedW=NULL, V=NULL, id=ad$clusterid, family=binomial(link='log'), print=FALSE))

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

adj.glm.h1 <- t(sapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws, forcedW=NULL, V=NULL, id=ad$clusterid, family=binomial(link='log'), print=FALSE))

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glm.C.W <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws, forcedW=c("agedays","sex"), id=ad$clusterid, contrast=c("Control","Water"), family=binomial(link='log'))

## ---- warning=FALSE, message=FALSE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Create a W variable containing "tchild" and other potential covariates:
W_tchild <- subset(ad,select=c("tchild","month","agedays","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))


#Estimate subgroup analysis glm with washb_glm
glm.C.W.byChildType <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=W_tchild, V="tchild", id=ad$clusterid, contrast=c("Control","Water"), family=binomial(link='log'), print=FALSE)

#Examine the treatment effect across subgroups with `objectname'$lincom
glm.C.W.byChildType$lincom

glm.byChildType <- lapply(h1.contrasts,washb_glm,Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=W_tchild, forcedW=NULL, V="tchild", id=ad$clusterid, family=binomial(link='log'), print=FALSE)

## ---- eval=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Fit the model
glm.C.N.byChildType <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=W_tchild, V="tchild", id=ad$clusterid, contrast=c("Control","Nutrition"), family=binomial(link='log'), print=FALSE)

## ---- eval=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Create lc vector of 0's equal in length to the number of coefficients from the glm model.
lc=rep(0,nrow(glm.C.N.byChildType$fit))
  #Replace the second position in the vector with 1 (the position of the treatment coefficient in the model)
lc[2]<-1
  #Run the lincom function and compare output to the treatment effect from the GLM model.
washb_lincom(lc=lc,fit=glm.C.N.byChildType$fit,vcv=glm.C.N.byChildType$vcv, measure="RR") 
  #GLM model:
glm.C.N.byChildType$TR

#Now, the function will be used to compare target children in the nutrition arm with household assets to siblings in the control arm without household assets.
#Examine model coefficients (minus the pair-matched block estimates).
glm.C.N.byChildType$fit[1:29,]
#Create an index vector of treatment(2), target child(3), interaction term(4), watmin(22), elec(23), asset_table(26), asset_khat(27).
lc=rep(0,nrow(glm.C.N.byChildType$fit))
lc[c(2:4,22,23,26,27)]<-1
#Calculate combined prevalence ratio through the linear combination of model coefficients:
washb_lincom(lc=lc,fit=glm.C.N.byChildType$fit,vcv=glm.C.N.byChildType$vcv, measure="RR") 

#Calculate risk difference using the risk difference output from the washb_glm function:
washb_lincom(lc=lc,fit=glm.C.N.byChildType$RDfit,vcv=glm.C.N.byChildType$vcvRD, measure="RD") 

#Alternatively, use a character vector of coefficient names rather than an index vector of variable positions.
varlist<-c("trNutrition","month6","hfiacatSeverely Food Insecure")
washb_lincom(varlist=varlist,fit=glm.C.N.byChildType$RDfit,vcv=glm.C.N.byChildType$vcvRD, measure="RR") 


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

## ---- include=FALSE, eval=TRUE, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(washb_bd_anthroClean)
ad<-washb_bd_anthroClean

Ws <- subset(ad,select=c("fracode","month","aged","sex","birthord","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))

## ---- cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Run washb_ttest on water vs. control arm comparison
washb_ttest(Y=ad$laz,tr=ad$tr,strat=ad$block, contrast=c("Control","Water"))

#Use sapply to apply across all contrasts
diff.h1LAZ <- t(sapply(h1.contrasts,washb_ttest,Y=ad$laz,tr=ad$tr,strat=ad$block))
rownames(diff.h1LAZ) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Unadjusted GLM
unadj.glm.h1LAZ <- lapply(h1.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, V=NULL, id=ad$clusterid, family="gaussian",print=FALSE)

#Adjusted GLM
adj.glm.h1LAZ <- lapply(h1.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, V=NULL, id=ad$clusterid, family="gaussian",print=FALSE)

## ---- warning=FALSE, message=FALSE, eval=T, cache=TRUE, comment=NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a W variable containing only "hfiacat" for the unadjusted subgroup analysis:
W_hfiacat <- subset(ad,select=c("hfiacat"))

#Create a W variable containing "hfiacat" and other potential covariates:= for the adjusted subgroup analysis
Wsubgroup <- subset(ad,select=c("month","aged","sex","birthord","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))


#Estimate subgroup analysis glm with washb_glm
glm.C.W.byFoodSecurity <- washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Wsubgroup, forcedW=NULL, V="hfiacat", id=ad$clusterid, contrast=c("Control","Water"), family="gaussian", print=FALSE)
glm.C.W.byFoodSecurity$lincom


## ---- eval=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Extend code to all treatment/control comparisons with lapply
#Unadjusted:
unadj.glm.byFoodSecurity <- lapply(h1.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=W_hfiacat, forcedW=NULL, V="hfiacat", id=ad$clusterid, family="gaussian", print=FALSE)
#Adjusted
glm.byFoodSecurity <- lapply(h1.contrasts,washb_glm,Y=ad$laz,tr=ad$tr,pair=ad$block, W=Wsubgroup, forcedW=NULL, V="hfiacat", id=ad$clusterid, family="gaussian", print=FALSE)

