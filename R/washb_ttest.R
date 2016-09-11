#' washb_ttest
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
#'
#' @export
#'
#' @examples




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
