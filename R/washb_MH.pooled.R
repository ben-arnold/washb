#' washb_MH.pool
#'
#' Mantel-Haenszel Pooled estimates of the prevalence ratio (PR) or the prevalence
#' difference (PD) using randomization block as the stratification variable.
#'
#' Function to call the M-H estimator for two different arms of the study this relies on
#' teh rma.mh() function in the metafor package.
#'
#'
#'
#' Estimate the Mantel-Haenszel prevalence ratio note: strata with no outcomes (i.e., missing PR) are dropped.
#' This is consistent with a fixed-effects regression analysis, in which those strata would not contribute to the estimates.
#' The arguments Y,tr,strat, below need to be from the same dataset:
#'
#'
#' @param Y binary outcome variable (here: diar7d)
#' @param tr binary treatment group variable, comparison group first
#' @param strat stratification variable (here: block)
#' @param contrast vector of length 2 that includes the tr groups to contrast (control(reference arm) and then intervention)
#' @param measure measure of effect. RR = prev ratio, RD = prev difference
#'
#' @return to be written
#' @export
#'
#' @examples
#'  to be written
#'


mh.pool <- function(Y,tr,strat,contrast,measure="RR") {
  require(metafor)
  mhdat <- data.frame(Y=Y[tr==contrast[1]|tr==contrast[2]],
                      tr=tr[tr==contrast[1]|tr==contrast[2]],
                      strat=strat[tr==contrast[1]|tr==contrast[2]])
  mhdat$tr <- factor(mhdat$tr,levels=contrast[2:1])
  mhtab <- table(mhdat$tr,mhdat$Y,mhdat$strat)
  mhtab <- mhtab[,c(2:1),] # re-order to be consistent w/ metafor table orientation

  # suppress warning about yi/vi values being NA -- we know there are sparse tables
  # and a straum with no cases in it is effectively dropped from any fixed-effect estimator
  # the warning thrown by metafor's rma.mh just makes the output unnecessarily noisy
  muffw <- function(w) if( any( grepl( "Some yi/vi values are NA", w) ) ) invokeRestart( "muffleWarning" )

  # MH-estimation
  mh.est <- withCallingHandlers( rma.mh(ai=mhtab[1,1,],bi=mhtab[1,2,],ci=mhtab[2,1,],di=mhtab[2,2,],measure=measure,drop00=TRUE,level=95), warning = muffw)

  # format the output
  # these two additional objects include log likelihood and AIC (currently not saved): mh.est$fit.stats[1,1],mh.est$fit.stats[3,1]
  res <- c(mh.est$b,mh.est$se,mh.est$ci.lb,mh.est$ci.ub,mh.est$zval,mh.est$pval)
  if(measure=="RR") {
    names(res) <- c("logPR","se.logPR","ci.lb","ci.ub","Z","p")
  } else{
    names(res) <- c("RD","se.RD","ci.lb","ci.ub","Z","p")
  }
  return(res)
}

