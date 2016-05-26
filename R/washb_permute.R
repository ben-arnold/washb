
#' washb_permute
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable (ideally a factor), comparison group first
#' @param pair Pair-matched randomization ID variable (in WASH Benefits: block)
#' @param contrast Vector of length 2 that includes the groups to contrast, e.g., c("Control","Water")
#' @param nreps Number of permutations to run.
#' @param seed Number for psuedo-random number generation in R
#'
#' @return
#' @export
#'
#' @examples


washb_permute <- function(Y,tr,pair,contrast,nreps=100000,seed=NULL) {
  # conduct a permutation test of the indepdence of Y and tr, conditional on randomization block
  # using the Wilcoxon rank-sum test statistic
  # Y  : outcome variable
  # tr : treatment assignment, factor
  # pair : randomization block, factor
  # contrast : string with 2 levels of tr that should be compared in the permutation test
  # nreps : number of permutations to run to approximate the null (default=100,000)
  # seed : a seed for pseudo-random number generation (for reproducible results)
  require(coin)
  require(plyr)
  pd <- data.frame(Y=Y,tr=tr,block=pair)
  pd <- subset(pd,tr==contrast[1]|tr==contrast[2])
  pd$tr <- factor(pd$tr,levels=contrast[1:2])
  pd <- ddply(pd,c("block","tr"),summarise,Y=mean(Y))
  if(!is.null(seed)) set.seed(seed)
  W <- wilcoxsign_test(Y~tr|block,data=pd,distribution = approximate(B=nreps),zero.method="Pratt" )
  show(W)

  # now pull out some of the useful information, for convenience since coin() uses S4
  Ho <- qperm(W,seq(0,1,by=0.01))
  p.value <- pvalue(W)[1]
  Z <- statistic(W)

  list(p.value=p.value,Z=Z,Ho=Ho,W=W)
}
