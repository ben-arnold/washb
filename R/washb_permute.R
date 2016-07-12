
#' washb_permute
#'
#' WASH Benefits Wilcoxon Signed Rank permutation test function for two treatment arms conditional on randomization block.
#' Conducts a permutation test of the independence of Y and tr, conditional on randomization block
#' using the Wilcoxon rank-sum test statistic
#'
#' @usage
#' washb_permute(Y,tr,pair,contrast,nreps=100000,seed=NULL)
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable (ideally a factor), comparison group first
#' @param pair Pair-matched randomization ID variable (in WASH Benefits: block)
#' @param contrast Vector of length 2 that includes the groups to contrast, e.g., c("Control","Water")
#' @param nreps Number of permutations to run.
#' @param seed Number for psuedo-random number generation in R
#'
#' @references
#' 1.	Gail, M. H., Mark, S. D., Carroll, R. J., Green, S. B. & Pee, D. On Design Considerations and Randomization-Based Inference for Community Intervention Trials. Statist. Med. 15, 1069–1092 (1996).
#' [Link](http://onlinelibrary.wiley.com/doi/10.1002/(SICI)1097-0258(19960615)15:11%3c1069::AID-SIM220%3e3.0.CO%3b2-Q/full)
#' 2.	Braun, T. M. & Feng, Z. Optimal Permutation Tests for the Analysis of Group Randomized Trials. Journal of the American Statistical Association 96, 1424–1432 (2001).
#' [Link](http://www.tandfonline.com/doi/abs/10.1198/016214501753382336)
#' 3.	Rosenbaum, P. R. Covariance Adjustment in Randomized Experiments and Observational Studies. Statist. Sci. 17, 286–327 (2002).
#' [Link](http://projecteuclid.org/euclid.ss/1042727942)
#'
#' @return
#' A list of four slices (replace "~" with the assigned object name):
#' ~$p.value  The permutation test p-value
#' ~$Z  The z-statistic
#' ~$Ho
#' ~$W The full output of the wilcoxsign_test function called within the washb_permute function.
#' @export
#'
#' @examples
#' #Unadjusted permutation test of Bangladesh diarrheal disease primary outcome
#'
#' #######################
#' #Load and clean data
#' #######################
#' data(washb_bd_diar)
#' d<-washb_bd_diar
#' d$block <- as.factor(d$block)
#'
#' # Subset the Data to Follow-up data only
#' ad <- subset(d,svy>0)
#'
#' # Exclude:
#' # * siblings who were born after enrollment
#' # * siblings who were >36 mos at enrollment
#' # * children with missing outcome data
#' ad <- subset(ad,sibnewbirth==0)
#' ad <- subset(ad,gt36mos==0)
#' ad <- subset(ad,!is.na(ad$diar7d))
#'
#' # re-order the tr factor for convenience
#' ad$tr <- factor(ad$tr,levels=c("Water","Sanitation","Handwashing","Nutrition","WSH","Nutrition + WSH","Control"))
#'
#' #Unadjusted permutation test of Bangladesh diarrheal disease outcome.
#'
#' # Hypothesis 1 permutation tests (Intervention arms vs. control arms)
#' set.seed(242524)
#' permute.C.W <- washb_permute(Y=ad$diar7d,tr=ad$tr,pair=ad$block,contrast=c("Control","Water"),nreps=100000)
#' permute.C.S <- washb_permute(Y=ad$diar7d,tr=ad$tr,pair=ad$block,contrast=c("Control","Sanitation"),nreps=100000)
#' permute.C.H <- washb_permute(Y=ad$diar7d,tr=ad$tr,pair=ad$block,contrast=c("Control","Handwashing"),nreps=100000)
#' permute.C.WSH <- washb_permute(Y=ad$diar7d,tr=ad$tr,pair=ad$block,contrast=c("Control","WSH"),nreps=100000)
#' permute.C.N   <- washb_permute(Y=ad$diar7d,tr=ad$tr,pair=ad$block,contrast=c("Control","Nutrition"),nreps=100000)
#' permute.C.NWSH <- washb_permute(Y=ad$diar7d,tr=ad$tr,pair=ad$block,contrast=c("Control","Nutrition + WSH"),nreps=100000)
#'
#' # put objects in the standard format
#' h1res <- list(permute.C.W,permute.C.S,permute.C.H,permute.C.WSH,permute.C.N,permute.C.NWSH)
#' diar_h1_pval_unadj <- as.matrix(sapply(h1res,function(x) x$p.value),nrow=6)
#' rownames(diar_h1_pval_unadj) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
#'
#' # print results
#' diar_h1_pval_unadj


washb_permute <- function(Y,tr,pair,contrast,nreps=100000,seed=NULL) {

#Insure that 'pair' is a factor
  if(class(pair)!="factor"){
    pair<-factor(pair)
  }

  require(coin)
  require(plyr)
  pd <- data.frame(Y=Y,tr=tr,pair=pair)
  pd <- subset(pd,tr==contrast[1]|tr==contrast[2])
  pd$tr <- factor(pd$tr,levels=contrast[1:2])
  pd <- ddply(pd,c("pair","tr"),summarise,Y=mean(Y))
  if(!is.null(seed)) set.seed(seed)
  W <- wilcoxsign_test(Y~tr|pair,data=pd,distribution = approximate(B=nreps),zero.method="Pratt" )

  show(W)

  # now pull out some of the useful information, for convenience since coin() uses S4
  Ho <- qperm(W,seq(0,1,by=0.01))
  p.value <- pvalue(W)[1]
  Z <- statistic(W)

  list(p.value=p.value,Z=Z,Ho=Ho,W=W)
}
