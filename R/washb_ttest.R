#' washb_ttest
#'
#'
#' Function to call the paired t-test for two different arms of the study.
#' Estimates the paired t-test for differences in means paired within randomization blocks.
#' The arguments Y,tr,strat, below need to be from the same dataset:
#'
#' @param Y quantitative outcome variable (e.g. LAZ)
#' @param tr binary treatment group variable, comparison group first
#' @param strat stratification variable (here: block)
#' @param contrast vector of length 2 that includes the tr groups to contrast
#'
#' @return data frame with mean difference ("diff"), 95% confidence intervals ("ci.lb" and "ci.ub"), t-statistic ("t-stat"), and p-value("p")
#' @export
#'
#' @examples
#'  #The washb_ttest function
#'
#'  #Load in Bandladesh anthropometry data and enrollment data
#'  data(washb_bd_anthro)
#'  data(washb_bd_enrol)
#'
#'  # drop svydate and month because they are superceded in the child level diarrhea data
#'  washb_bd_enrol$svydate <- NULL
#'  washb_bd_enrol$month <- NULL
#'  ad <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
#'  ad <- subset(ad,svy==2)
#'  ad <- subset(ad,tchild=="Target child")
#'
#   # Drop children with extreme LAZ values
#'  ad <- subset(ad,laz_x!=1)
#'
#'  #Make sure the treatment group variables are set as factors:
#'  ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
#'
#'  #Run paired ttest function:
#'  washb_ttest(Y=ad$laz,tr=ad$tr,strat=ad$block, contrast=c("Control","Water"))




washb_ttest <- function(Y,tr,strat,contrast) {
  ttdat <- data.frame(Y=Y[tr==contrast[1]|tr==contrast[2]],
                      tr=tr[tr==contrast[1]|tr==contrast[2]],
                      strat=strat[tr==contrast[1]|tr==contrast[2]])
  ttdat$tr <- factor(ttdat$tr,levels=contrast[1:2])
  blockmeans <- tapply(ttdat$Y,list(ttdat$strat,ttdat$tr),function(x) mean(x))
  t.est <- t.test(x=blockmeans[,2],y=blockmeans[,1],alternative="two.sided",paired=TRUE,var.equal=FALSE,conf.level=0.95)

  # format output
  res <- c(t.est$estimate,t.est$conf.int[1],t.est$conf.int[2],t.est$statistic,t.est$p.value)
  names(res) <- c("diff","ci.lb","ci.ub","t-stat","p")
  return(res)
}
