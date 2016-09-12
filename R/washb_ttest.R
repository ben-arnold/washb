#' washb_ttest
#'
#' Function to call the paired t-test for two different arms of the study.
#' Estimates the paired t-test for differences in means paired within randomization blocks.
#' The arguments Y,tr, and strat below need to be from the same dataset:
#'
#'
#' @usage
#' washb_ttest(Y,tr,strat,contrast)
#'
#' @param Y quantitative outcome variable (e.g. LAZ)
#' @param tr binary treatment group variable, comparison group first
#' @param strat stratification variable (here: block)
#' @param contrast vector of length 2 that includes the tr groups to contrast
#'
#' @return
#' data frame with mean difference (diff), 95% confidence intervals (ci.lb and ci.ub), t-statistic (t-stat), and p-value(p)
#'
#' @export
#'
#' @examples
#' #The washb_ttest function
#'
#'  #Load in Bangladesh anthropometry data.
#'  data(washb_bd_anthro)
#'  data(washb_bd_enrol)
#'
#'  washb_bd_enrol$svydate <- NULL
#'  washb_bd_enrol$month <- NULL
#'  laz <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
#'
#'  # subset to the endline target children
#'  laz <- subset(laz,svy==2)
#'  laz <- subset(laz,tchild=="Target child")
#'
#'  # Drop children with extreme LAZ values
#'  laz <- subset(laz,laz_x!=1)
#'
#'  laz$tr <- factor(laz$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
#'
#'
#'  #Run paired ttest function for water vs. control comparison:
#'  washb_ttest(Y=laz$laz,tr=laz$tr,strat=laz$block, contrast=c("Control","Water"))






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
