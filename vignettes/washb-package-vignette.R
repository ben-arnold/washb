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

## ---- tidy=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diff.h1 <- t(sapply(h1.contrasts,washb_ITT.unadj,Y=ad$diar7d,tr=ad$tr,strat=ad$block,binomial=TRUE,measure="RR"))
rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
print(exp(diff.h1))

## ---- eval = FALSE, tidy=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  ##Need to fix so code runs
#  permute.diff.h1<-t(sapply(h1.contrasts,washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=10000, seed=12345))
#  rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")

## ---- eval = FALSE, tidy=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # adjusted estimates (tmle)
#  cwfit    <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.67,0.33),contrast=c("Control","Water"),seed=3902288)
#  csfit    <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.67,0.33),contrast=c("Control","Sanitation"),seed=6125274)
#  chfit    <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.67,0.33),contrast=c("Control","Handwashing"),seed=4499261)
#  cwshfit  <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.67,0.33),contrast=c("Control","WSH"),seed=4545085)
#  cnfit    <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.67,0.33),contrast=c("Control","Nutrition"),seed=1964134)
#  cwshnfit <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.67,0.33),contrast=c("Control","Nutrition + WSH"),seed=4397157)
#  
#  # pull out the estimates from the tmle objects and summarize them in a matrix
#  tmle.summary.rd <- function(x) {
#    res <- c(x$estimates$ATE$psi,x$estimates$ATE$CI[1],x$estimates$ATE$CI[2],x$estimates$ATE$pvalue)
#    names(res) <- c("rd","ci.lb","ci.ub","p")
#    return(res)
#  }
#  tmle.summary.pr <- function(x) {
#    res <- c(x$estimates$RR$psi,x$estimates$RR$CI[1],x$estimates$RR$CI[2],x$estimates$RR$pvalue)
#    names(res) <- c("pr","ci.lb","ci.ub","p")
#    return(res)
#  }
#  
#  tmle.h1 <- list(cwfit,csfit,chfit,cwshfit,cnfit,cwshnfit)
#  rd.tmle.h1 <- t(sapply(tmle.h1,tmle.summary.rd))
#  pr.tmle.h1 <- t(sapply(tmle.h1,tmle.summary.pr))
#  rownames(rd.tmle.h1) <- rownames(pr.tmle.h1) <- rownames(diff.h1)
#  
#  # print results
#  round(diff.h1,4)
#  round(rd.tmle.h1,4)
#  round(pr.tmle.h1,4)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

h2.contrasts <- list(
  c("Water","WSH"),
  c("Sanitation","WSH"),
  c("Handwashing","WSH")
)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# unadjusted estimates (paired t-test)
diff.h2 <- t(sapply(h2.contrasts,washb_ITT.unadj,Y=ad$diar7d,tr=ad$tr,strat=ad$block,binomial=TRUE,measure="RR"))
rownames(diff.h2) <- c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
print(exp(diff.h2))

## ---- eval = FALSE, tidy=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  
#  # adjusted estimates (tmle)
#  wshwfit    <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.5,0.5),contrast=c("Water","WSH"),seed=8440041)
#  wshhfit    <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.5,0.5),contrast=c("Handwashing","WSH"),seed=1948434)
#  wshsfit    <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.5,0.5),contrast=c("Sanitation","WSH"),seed=1289364)
#  
#  tmle.h2 <- list(wshwfit,wshsfit,wshhfit)
#  rd.tmle.h2 <- t(sapply(tmle.h2,tmle.summary.rd))
#  pr.tmle.h2 <- t(sapply(tmle.h2,tmle.summary.pr))
#  rownames(rd.tmle.h2) <- rownames(pr.tmle.h2) <- rownames(diff.h2)
#  
#  # print results
#  round(diff.h2,4)
#  round(rd.tmle.h2,4)
#  round(pr.tmle.h2,4)

## ---- eval = FALSE, tidy=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  
#   h3.contrasts <- list(
#     c("WSH","Nutrition + WSH"),
#     c("Nutrition","Nutrition + WSH")
#   )
#  
#   # unadjusted estimates (paired t-test)
#   diff.h3 <- t(sapply(h3.contrasts,ITT.unadj,Y=ad$diar7d,tr=ad$tr,strat=ad$block))
#   rownames(diff.h3) <- c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")
#  
#   # adjusted estimates (tmle)
#   set.seed(792348)
#   wshwshnfit <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.5,0.5),contrast=c("WSH","Nutrition + WSH"))
#   nwshnfit   <- ITT.tmle(Y=ad$diar7d,tr=ad$tr,strat=ad$block,W=Ws,id=ad$clusterid,family="binomial",prtr=c(0.5,0.5),contrast=c("Nutrition","Nutrition + WSH"))
#  
#   tmle.h3 <- list(nwshnfit,wshwshnfit)
#   rd.tmle.h3 <- t(sapply(tmle.h3,tmle.summary.rd))
#   rownames(rd.tmle.h3) <- rownames(diff.h3)

## ---- eval = FALSE, tidy=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  
#  diar_h1_rd_adj <- rd.tmle.h1
#  diar_h2_rd_adj <- rd.tmle.h2
#  # diar_h3_rd_adj <- rd.tmle.h3
#  
#  diar_h1_pr_adj <- pr.tmle.h1
#  diar_h2_pr_adj <- pr.tmle.h2
#  # diar_h3_pr_adj <- pr.tmle.h3

## ----  eval = FALSE, tidy=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  
#  diar_h1_rd_adj
#  
#  diar_h2_rd_adj
#  
#  diar_h1_pr_adj
#  
#  diar_h2_pr_adj

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#print((diff.h1))
print(exp(diff.h1))

#print((diff.h2))
print(exp(diff.h2))



print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Water"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Sanitation"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Handwashing"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","WSH"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition + WSH"), family="binomial"))

#diff.h1 <- t(sapply(h1.contrasts,washb_ITT.unadj,Y=ad$diar7d,tr=ad$tr,strat=ad$block,binomial=TRUE,measure="RD"))
#rownames(glm.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
#print(diff.h1)


#Need to fix/debug
#glm.h1<-washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=h1.contrasts, family="binomial")
#rownames(glm.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
#print(glm.h1)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Water"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Sanitation"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Handwashing"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","WSH"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition"), family="binomial"))

print(washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition + WSH"), family="binomial"))

## ----  results="hide"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load(file.path(extPath, "washb_bd_enrol.Rdata"))  
load(file.path(extPath, "washb_bd_anthro.Rdata"))  

# drop svydate and month because they are superceded in the child level diarrhea data
  washb_bd_enrol$svydate <- NULL
  washb_bd_enrol$month <- NULL

# merge the baseline dataset to the follow-up dataset
ad <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
dim(washb_bd_anthro)
dim(ad)

#---------------------------------------
# subset to the relevant measurement
# Year 1 or Year 2
#---------------------------------------
table(ad$svy)
ad <- subset(ad,svy==2)
dim(ad)

# subset the anthropometry to target children (excluding siblings)
dim(ad)
ad <- subset(ad,tchild=="Target child")
dim(ad)

# Drop children with extreme LAZ values
table(ad$laz_x)
ad <- subset(ad,laz_x!=1)


# Exclude children with missing data (none)
table(is.na(ad$laz))

# re-order the tr factor for convenience
ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

# ensure that month is coded as a factor
ad$month <- factor(ad$month)

# sort the data for perfect replication with jade on the V-fold cross-validation
ad <- ad[order(ad$block,ad$clusterid,ad$dataid,ad$childid),]

#---------------------------------------
# Select covariates with univariate
# associations with the outcome of
# P<0.2 based on a liklihood ratio test
#---------------------------------------

# drop due to so many missing values?
# asset_clock

Ws <- subset(ad,select=c("fracode","month","aged","sex","birthord","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))


#---------------------------------------
# Estimate adjusted mean differences
#---------------------------------------
#---------------------------------------
# H1: Each intervention arm vs. Control
#---------------------------------------
h1.contrasts <- list(
  c("Control","Water"),
  c("Control","Sanitation"),
  c("Control","Handwashing"),
  c("Control","WSH"),
  c("Control","Nutrition"),
  c("Control","Nutrition + WSH")
)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diff.h1 <- t(sapply(h1.contrasts,washb_ITT.unadj,Y=ad$laz,tr=ad$tr,strat=ad$block,measure="RD"))
rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
print(diff.h1)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Water"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Sanitation"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Handwashing"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","WSH"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=NULL,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition + WSH"), family="gaussian")))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Water"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Sanitation"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Handwashing"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","WSH"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition"), family="gaussian")))

print((washb_glm(Y=ad$laz,tr=ad$tr,pair=ad$block, W=Ws,forcedW=NULL, id=ad$clusterid, contrast=c("Control","Nutrition + WSH"), family="gaussian")))

