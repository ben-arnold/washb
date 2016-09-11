#' Target maximum likelihood estimation function for WASH Benefits study.
#' washb_tmle
#'
#' NOTE: washb_tmle has not been fully implemented in this package version. Do not use yet.
#'
#'   # Targeted maximum likelihood estimation (TMLE) for the WASH Benefits intention to treat (ITT) analyses
# depends on the tmle() package, as well as the Wprescreen() and design.matrix() functions.
#'
#'  The function does the following:
# * it pre-screens covariates W for those with a LR test P<0.2
# * using the covariates selected in the screening step, it then estimates TMLE
# * it prints the results, and returns an object of class tmle
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable, comparison group first
#' @param strat Stratification variable (here: block)
#' @param W Data frame that includes adjustment covariates
#' @param id ID variable for independent units (e.g., cluster)
#' @param contrast Vector of length 2 that includes the tr groups to contrast
#' @param prtr Vector of length 2 that includes the probability of receiving each treatment in the contrast argument for the main trial, the control is double sized (allocation 2:1) so control vs. active intervention should be prtr=c(0.67,0.33). in contrasts where the arms are equally sized (allocation 1:1), then prtr=c(0.5,0.5).
#' @param modelfit String variable of value "glm" for a glm prediction of E(Y|A,W) and E(A|W), or "sl" for a SuperLearner prediction.
#' @param family Outcome family: gaussian (continuous outcomes, like LAZ) or binomial (binary outcomes like diarrhea or stunting)
#' @param SL.Library Library of algorithms to include in the SuperLearner (pre-specified defaults are encoded above)
#' @param seed A seed for the pseudo-random CV split for perfectly reproducible results
#'
#'
#' @return To be filled
#'
#'
#' @export
#'
#' @examples
#'
#'




washb_tmle <- function(Y,tr,strat,W,id,contrast,prtr=c(0.5,0.5),modelfit="glm", family="gaussian",SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),seed=NULL) {



  require(tmle)
  require(SuperLearner)
  tmledat <- data.frame(
    id=id[tr==contrast[1]|tr==contrast[2]],
    Y=Y[tr==contrast[1]|tr==contrast[2]],
    tr=tr[tr==contrast[1]|tr==contrast[2]],
    strat=strat[tr==contrast[1]|tr==contrast[2]],
    W[tr==contrast[1]|tr==contrast[2],]
  )
  tmledat$tr <- factor(tmledat$tr,levels=contrast[1:2])

  # restrict to complete cases
  n.orig <- dim(tmledat)[1]
  tmledat <- tmledat[complete.cases(tmledat),]
  n.sub  <- dim(tmledat)[1]
  if(n.orig>n.sub) cat("\n-----------------------------------------\nDropping",n.orig-n.sub,"observations due to missing values in 1 or more variables\n","Final sample size:",n.sub,"\n-----------------------------------------\n")

  # create indicators for the randomization strata
  strats <- model.matrix(~as.factor(tmledat$strat))[,-c(1)]
  colnames(strats) <- paste("block",2:90,sep="")

  # pre-screen the covariates
  # see Wprescreen() in the base functions
  cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
  Wscreen <- Wprescreen(Y=tmledat$Y,Ws=tmledat[,5:ncol(tmledat)],family=family)
  Wselect <- subset(tmledat[,5:ncol(tmledat)],select=Wscreen)

  # for covariates that are factors, create indicator variables
  # because the tmle() function cannot handle factors in W
  # see design.matrix() in the base functions
  Wselect <- design.matrix(Wselect)

  # specify the probability of treatment, depending on the type of contrast
  # for control vs. intervention, the control prob should be 2/3 and tr prob should be 1/3 (allocation 2:1)
  # for intervention arm 1 vs intervantion arm 2, prob should be 1/2 for both (allocation 1:1)
  tr.p <- ifelse(tmledat$tr==contrast[1],prtr[1],prtr[2])

  # re-parse the variables to use them in tmle()
  tmle.Y <- tmledat$Y
  tmle.A <- ifelse(tmledat$tr==contrast[2],1,0)
  tmle.W <- data.frame(Wselect,strats)
  tmle.id <- tmledat$id

  # estimate the adjusted difference with TMLE
  if(!is.null(seed)) set.seed(seed)
  tmle.fit <- tmle(Y=tmle.Y,
                   A=tmle.A,
                   W=tmle.W,
                   id=tmle.id,
                   Q.SL.library=SL.library,
                   g1W=tr.p,
                   family=family
  )
  cat("\n-----------------------------------------\nEstimation Results:\n-----------------------------------------\n")
  print(summary(tmle.fit))

  return(tmle.fit)
}
