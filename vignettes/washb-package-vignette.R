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
washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RR")

## ---- warning=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------
washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RD")

## ---- warning=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------
#Hypothesis 1
diff.h1 <- t(sapply(h1.contrasts,washb_mh,Y=ad$diar7d,tr=ad$tr,strat=ad$block,measure="RR"))
rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
print(diff.h1)

#Hypothesis 2
diff.h2 <- t(sapply(h2.contrasts,washb_mh,Y=ad$diar7d,tr=ad$tr,strat=ad$block,measure="RR"))
rownames(diff.h2) <- c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
print(diff.h2)

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  data(washb_bd_anthro)
#  data(washb_bd_enrol)
#  
#  ad <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)

## ---- cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Run washb_ttest on water vs. control arm comparison
washb_ttest(Y=ad$laz,tr=ad$tr,strat=ad$block, contrast=c("Control","Water"))

#Use sapply to apply across all contrasts
diff.h1LAZ <- t(sapply(h1.contrasts,washb_ttest,Y=ad$laz,tr=ad$tr,strat=ad$block))
rownames(diff.h1LAZ) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
round(print(diff.h1LAZ),3)

## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #Table of Primary Outcome Results
#  
#  ###7-day diarrheal disease recall outcome
#  
#  Contrast v. control | Estimator | PR | 95% CI | SE | P-value
#  --------------------|-----------|----|--------|----|--------
#  Water | Unadjusted MH  | `r round(diff.h1[1,1],2)` | `r round((diff.h1[1,2:3]),2)` | `r round(diff.h1[1,5],2)` | `r round(diff.h1[1,7],4)`
#  Water | Unadjusted GLM |  `r round(unadj.glm.h1[[1]][2,1],2)` |(`r round(unadj.glm.h1[[1]][2,2:3],2)`) | `r round(unadj.glm.h1[[7]][2,2],4)` | `r round(unadj.glm.h1[[7]][2,4],4)`
#  Water | Adjusted GLM | `r round(adj.glm.h1[[1]][2,1],2)` |(`r round(adj.glm.h1[[1]][2,2:3],2)`) | `r round(adj.glm.h1[[7]][2,2],4)` | `r round(adj.glm.h1[[7]][2,4],4)`
#  Water | Adjusted GLM + TMLE |  | | |
#  Water | Adjusted SL + TMLE |  | | |
#  Water | Wilcoxon  permutation test |  | |`r permute.diff.h1[1]` |
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  Sanitation | Unadjusted MH  | `r round(diff.h1[2,1],2)` | `r round((diff.h1[2,2:3]),2)` | `r round(diff.h1[2,5],2)` | `r round(diff.h1[2,7],4)`
#  Sanitation | Unadjusted GLM |  `r round(unadj.glm.h1[[2]][2,1],2)` |(`r round(unadj.glm.h1[[2]][2,2:3],2)`) | `r round(unadj.glm.h1[[8]][2,2],4)` | `r round(unadj.glm.h1[[8]][2,4],4)`
#  Sanitation | Adjusted GLM | `r round(adj.glm.h1[[2]][2,1],2)` |(`r round(adj.glm.h1[[2]][2,2:3],2)`) | `r round(adj.glm.h1[[8]][2,2],4)` | `r round(adj.glm.h1[[8]][2,4],4)`
#  Sanitation | Adjusted GLM + TMLE | | | |
#  Sanitation | Adjusted SL + TMLE | | | |
#  Sanitation | Wilcoxon  permutation test |  | |`r permute.diff.h1[2]` |
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  Handwashing | Unadjusted MH  | `r round(diff.h1[3,1],2)` | `r round((diff.h1[3,2:3]),2)` | `r round(diff.h1[3,5],2)` | `r round(diff.h1[3,7],4)`
#  Handwashing | Unadjusted GLM |  `r round(unadj.glm.h1[[3]][2,1],2)` |(`r round(unadj.glm.h1[[3]][2,2:3],2)`) | `r round(unadj.glm.h1[[9]][2,2],4)` | `r round(unadj.glm.h1[[9]][2,4],4)`
#  Handwashing | Adjusted GLM | `r round(adj.glm.h1[[3]][2,1],2)` |(`r round(adj.glm.h1[[3]][2,2:3],2)`) | `r round(adj.glm.h1[[9]][2,2],4)` | `r round(adj.glm.h1[[9]][2,4],4)`
#  Handwashing | Adjusted GLM + TMLE | | | |
#  Handwashing | Adjusted SL + TMLE | | | |
#  Handwashing | Wilcoxon  permutation test |  | |`r permute.diff.h1[3]` |
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  WSH | Unadjusted MH  | `r round(diff.h1[4,1],2)` | `r round((diff.h1[4,2:3]),2)` | `r round(diff.h1[4,5],2)` | `r round(diff.h1[4,7],4)`
#  WSH | Unadjusted GLM |  `r round(unadj.glm.h1[[4]][2,1],2)` |(`r round(unadj.glm.h1[[4]][2,2:3],2)`) | `r round(unadj.glm.h1[[10]][2,2],4)` | `r round(unadj.glm.h1[[10]][2,4],4)`
#  WSH | Adjusted GLM | `r round(adj.glm.h1[[4]][2,1],2)` |(`r round(adj.glm.h1[[4]][2,2:3],2)`) | `r round(adj.glm.h1[[10]][2,2],4)` | `r round(adj.glm.h1[[10]][2,4],4)`
#  WSH | Adjusted GLM + TMLE | | | |
#  WSH | Adjusted SL + TMLE | | | |
#  WSH | Wilcoxon  permutation test |  | |`r permute.diff.h1[4]` |
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  Nutrition | Unadjusted MH  | `r round(diff.h1[5,1],2)` | `r round((diff.h1[5,2:3]),2)` | `r round(diff.h1[5,5],2)` | `r round(diff.h1[5,7],4)`
#  Nutrition | Unadjusted GLM |  `r round(unadj.glm.h1[[5]][2,1],2)` |(`r round(unadj.glm.h1[[5]][2,2:3],2)`) | `r round(unadj.glm.h1[[11]][2,2],4)` | `r round(unadj.glm.h1[[11]][2,4],4)`
#  Nutrition | Adjusted GLM | `r round(adj.glm.h1[[5]][2,1],2)` |(`r round(adj.glm.h1[[5]][2,2:3],2)`) | `r round(adj.glm.h1[[11]][2,2],4)` | `r round(adj.glm.h1[[11]][2,4],4)`
#  Nutrition | Adjusted GLM + TMLE | | | |
#  Nutrition | Adjusted SL + TMLE | | | |
#  Nutrition | Wilcoxon  permutation test |  | |`r permute.diff.h1[5]` |
#  -----------------------------------------|--------------------------------------|---------------|-----------------------|------------|-----------
#  WSH+N | Unadjusted MH  | `r round(diff.h1[6,1],2)` | `r round((diff.h1[6,2:3]),2)` | `r round(diff.h1[6,5],2)` | `r round(diff.h1[6,7],4)`
#  WSH+N | Unadjusted GLM |  `r round(unadj.glm.h1[[6]][2,1],2)` |(`r round(unadj.glm.h1[[6]][2,2:3],2)`) | `r round(unadj.glm.h1[[12]][2,2],4)` | `r round(unadj.glm.h1[[12]][2,4],4)`
#  WSH+N | Adjusted GLM | `r round(adj.glm.h1[[6]][2,1],2)` |(`r round(adj.glm.h1[[6]][2,2:3],2)`) | `r round(adj.glm.h1[[12]][2,2],4)` | `r round(adj.glm.h1[[12]][2,4],4)`
#  WSH+N | Adjusted GLM + TMLE | | | |
#  WSH+N | Adjusted SL + TMLE | | | |
#  WSH+N | Wilcoxon  permutation test |  | |`r permute.diff.h1[6]` |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  
#  
#  
#  
#  
#  
#  
#  ###Length-for-Age Z-score outcome
#  
#  Contrast v. control | Estimator | Difference | 95% CI | t-stat/SE | P-value
#  --------------------|-----------|------------|--------|----|--------
#  Water | Unadjusted ttest | `r round(diff.h1LAZ[1,1],2)` |(`r round((diff.h1LAZ[1,2:3]),2)`) | t-stat:  `r round(diff.h1LAZ[1,4],2)`| `r round(diff.h1LAZ[1,5],3)`
#  Water | Unadjusted GLM |  `r round(unadj.glm.h1LAZ[[1]][2,1],2)` |(`r round(unadj.glm.h1LAZ[[1]][2,2:3],2)`) |SE:  `r round(unadj.glm.h1LAZ[[7]][2,2],2)` | `r round(unadj.glm.h1LAZ[[7]][2,4],3)`
#  Water | Adjusted GLM | `r round(adj.glm.h1LAZ[[1]][2,1],2)` |(`r round(adj.glm.h1LAZ[[1]][2,2:3],2)`) |SE:  `r round(adj.glm.h1LAZ[[7]][2,2],2)` | `r round(adj.glm.h1LAZ[[7]][2,4],3)`
#  Water | Adjusted GLM + TMLE |  | | |
#  Water | Adjusted SL + TMLE |  | | |
#  Water | Wilcoxon  permutation test |  | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  Sanitation | Unadjusted ttest | `r round((diff.h1LAZ[2,1]),2)` |(`r round((diff.h1LAZ[2,2:3]),2)`) | `r round(diff.h1LAZ[2,4],2)`| `r round(diff.h1LAZ[2,5],3)`
#  Sanitation | Unadjusted GLM |  `r round(unadj.glm.h1LAZ[[2]][2,1],2)` |(`r round(unadj.glm.h1LAZ[[2]][2,2:3],2)`) | `r round(unadj.glm.h1LAZ[[8]][2,2],2)` | `r round(unadj.glm.h1LAZ[[8]][2,4],3)`
#  Sanitation | Adjusted GLM | `r round(adj.glm.h1LAZ[[2]][2,1],2)` |(`r round(adj.glm.h1LAZ[[2]][2,2:3],2)`) | `r round(adj.glm.h1LAZ[[8]][2,2],2)` | `r round(adj.glm.h1LAZ[[8]][2,4],3)`
#  Sanitation | Adjusted GLM + TMLE | | | |
#  Sanitation | Adjusted SL + TMLE | | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  Handwashing | Unadjusted ttest | `r round((diff.h1LAZ[3,1]),2)` |(`r round((diff.h1LAZ[3,2:3]),2)`) | `r round(diff.h1LAZ[3,4],2)`| `r round(diff.h1LAZ[3,5],3)`
#  Handwashing | Unadjusted GLM |  `r round(unadj.glm.h1LAZ[[3]][2,1],2)` |(`r round(unadj.glm.h1LAZ[[3]][2,2:3],2)`) | `r round(unadj.glm.h1LAZ[[9]][2,2],2)` | `r round(unadj.glm.h1LAZ[[9]][2,4],3)`
#  Handwashing | Adjusted GLM | `r round(adj.glm.h1LAZ[[3]][2,1],2)` |(`r round(adj.glm.h1LAZ[[3]][2,2:3],2)`) | `r round(adj.glm.h1LAZ[[9]][2,2],2)` | `r round(adj.glm.h1LAZ[[9]][2,4],3)`
#  Handwashing | Adjusted GLM + TMLE | | | |
#  Handwashing | Adjusted SL + TMLE | | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  WSH | Unadjusted ttest | `r round((diff.h1LAZ[4,1]),2)` |(`r round((diff.h1LAZ[4,2:3]),2)`) | `r round(diff.h1LAZ[4,4],2)`| `r round(diff.h1LAZ[4,5],3)`
#  WSH | Unadjusted GLM |  `r round(unadj.glm.h1LAZ[[4]][2,1],2)` |(`r round(unadj.glm.h1LAZ[[4]][2,2:3],2)`) | `r round(unadj.glm.h1LAZ[[10]][2,2],2)` | `r round(unadj.glm.h1LAZ[[10]][2,4],3)`
#  WSH | Adjusted GLM | `r round(adj.glm.h1LAZ[[4]][2,1],2)` |(`r round(adj.glm.h1LAZ[[4]][2,2:3],2)`) | `r round(adj.glm.h1LAZ[[10]][2,2],2)` | `r round(adj.glm.h1LAZ[[10]][2,4],3)`
#  WSH | Adjusted GLM + TMLE | | | |
#  WSH | Adjusted SL + TMLE | | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  Nutrition | Unadjusted ttest | `r round((diff.h1LAZ[5,1]),2)` |(`r round((diff.h1LAZ[5,2:3]),2)`) | `r round(diff.h1LAZ[5,4],4)`| `r round(diff.h1LAZ[5,5],3)`
#  Nutrition | Unadjusted GLM |  `r round(unadj.glm.h1LAZ[[5]][2,1],2)` |(`r round(unadj.glm.h1LAZ[[5]][2,2:3],2)`) | `r round(unadj.glm.h1LAZ[[11]][2,2],4)` | `r round(unadj.glm.h1LAZ[[11]][2,4],4)`
#  Nutrition | Adjusted GLM | `r round(adj.glm.h1LAZ[[5]][2,1],2)` |(`r round(adj.glm.h1LAZ[[5]][2,2:3],2)`) | `r round(adj.glm.h1LAZ[[11]][2,2],4)` | `r round(adj.glm.h1LAZ[[11]][2,4],4)`
#  Nutrition | Adjusted GLM + TMLE | | | |
#  Nutrition | Adjusted SL + TMLE | | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  WSH+N | Unadjusted ttest | `r round((diff.h1LAZ[6,1]),2)` |(`r round((diff.h1LAZ[6,2:3]),2)`) | `r round(diff.h1LAZ[6,4],4)`| `r round(diff.h1LAZ[6,5],3)`
#  WSH+N | Unadjusted GLM |  `r round(unadj.glm.h1LAZ[[6]][2,1],2)` |(`r round(unadj.glm.h1LAZ[[6]][2,2:3],2)`) | `r round(unadj.glm.h1LAZ[[12]][2,2],2)` | `r round(unadj.glm.h1LAZ[[12]][2,4],3)`
#  WSH+N | Adjusted GLM | `r round(adj.glm.h1LAZ[[6]][2,1],2)` |(`r round(adj.glm.h1LAZ[[6]][2,2:3],2)`) | `r round(adj.glm.h1LAZ[[12]][2,2],2)` | `r round(adj.glm.h1LAZ[[12]][2,4],3)`
#  WSH+N | Adjusted GLM + TMLE | | | |
#  WSH+N | Adjusted SL + TMLE | | | |
#  -----------------------------------------|--------------------------------------|---------------|--------------|------------|-------------
#  

