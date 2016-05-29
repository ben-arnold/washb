
#' washb_ITT.unadj
#'
#' Unadjusted estimates for continuous (paired t-test) or binomial (mantel-haenszel) outcomes
#'
#' unadjusted ITT estimates of differences between arms, calculated with either a paired T-test
#' (continuous outcomes) or a Mantel-Haenszel pooled estimator (binary outcomes).
#' The arguments Y,tr,strat, below need to be from the same dataset
#'
#' @param Y binary outcome variable (here: diar7d)
#' @param tr binary treatment group variable, comparison group first
#' @param strat stratification variable (here: block)
#' @param contrast vector of length 2 that includes the tr groups to contrast
#' @param binomial logicial. If TRUE, then the M-H estimator is used for binary outcomes
#' @param measure measure of effect for the M-H estimator. RR = prev ratio, RD = prev difference. ignored unless binomial==TRUE.
#'
#'
#' @return to be written
#' @export
#'
#' @examples
#'  to be written
#'



ITT.unadj <- function(Y,tr,strat,contrast,binomial=FALSE,measure="RR") {

  if(binomial==TRUE) {
    est <- mh.pool(Y=Y,tr=tr,strat=strat,contrast=contrast,measure=measure)
  } else{
    est <- paired.ttest(Y=Y,tr=tr,strat=strat,contrast=contrast)
  }
  return(est)
}
