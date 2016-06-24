
#' washb_permute
#'
#' WASH Benefits Wilcoxon Signed Rank permutation test function for two treatment arms conditional on randomization block.
#' Conducts a permutation test of the independence of Y and tr, conditional on randomization block
#' using the Wilcoxon rank-sum test statistic
#'
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable (ideally a factor), comparison group first
#' @param pair Pair-matched randomization ID variable (in WASH Benefits: block)
#' @param contrast Vector of length 2 that includes the groups to contrast, e.g., c("Control","Water")
#' @param nreps Number of permutations to run.
#' @param seed Number for psuedo-random number generation in R
#'
#' @references  (Gail 1996, Feng 2001, Rosenbaum 2002)
#' Ben: Add full citations
#'
#' @return to be written
#' @export
#'
#' @examples
#' to be written


washb_permute <- function(Y,tr,pair,contrast,nreps=100000,seed=NULL) {

  #Handle missing data?

  require(coin)
  require(plyr)
  pd <- data.frame(Y=Y,tr=tr,pair=pair)
  pd <- subset(pd,tr==contrast[1]|tr==contrast[2])
  pd$tr <- factor(pd$tr,levels=contrast[1:2])
  pd <- ddply(pd,c("pair","tr"),summarise,Y=mean(Y))
  if(!is.null(seed)) set.seed(seed)
  W <- wilcoxsign_test(Y~tr|pair,data=pd,distribution = approximate(B=nreps),zero.method="Pratt" )
  #W <- wilcoxsign_test(Y~tr,data=pd,distribution = approximate(B=nreps),zero.method="Pratt" )

  show(W)

  # now pull out some of the useful information, for convenience since coin() uses S4
  Ho <- qperm(W,seq(0,1,by=0.01))
  p.value <- pvalue(W)[1]
  Z <- statistic(W)

  list(p.value=p.value,Z=Z,Ho=Ho,W=W)
}
