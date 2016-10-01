#' Target maximum likelihood estimation function for the WASH Benefits study
#'
#' Estimate intention-to-treat parameters using targeted maximum likelihood estimation (TMLE)
#'
#' @details
#' The \code{washb_tmle} function is a convenience wrapper for the \code{\link[tmle]{tmle}} function. It estimates intention-to-treat effects in a trial using targeted maximum likelihood estimation. If the trial is pair-matched, be sure to specify the pair id in the \code{id} argument. If adjustment covariates (\code{W}) are specified, then by default they are pre-screened and the subset that is associated with the outcome based on a likelihood ratio test are used in the estimation. There are some other important defaults to be aware of. First, if the \code{prtr=NULL} (the default), then the function estimates the treatment mechanism rather than treating it as known (recommended). Second, you can specify \code{modelfit="sl"} to use the \code{\link[SuperLearner]{SuperLearner}} algorithm to fit the outcome and treatment models in the TMLE (the default).  Alternatively, you can specify \code{modelfit="glm"} to fit the outcome and treatment models using main effects regression (either linear or binomial, depending on the family). If you use super learning to fit models, then you can also specify the algorithm libraries using the \code{Q.SL.library} and \code{g.SL.library} arguments (pre-specified defaults are provided).
#'
#' Note: this function depends on the \code{\link[tmle]{tmle}} package, the \code{\link[SuperLearner]{SuperLearner}} package, as well as the internal washb_prescreen() and design_matrix() functions.
#'
#' Note: In the near future this function will allow for outcome censoring (missing data) using the \code{delta} argument.
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable, comparison group first
#' @param W Data frame that includes adjustment covariates
#' @param id ID variable for independent units. For pair-matched designs, this is the matched pair.
#' @param family Outcome family: \code{gaussian} (continuous outcomes, like LAZ) or \code{binomial} (binary outcomes like diarrhea or stunting)
#' @param contrast Vector of length 2 that includes the tr groups to contrast
#' @param prtr Vector of length 2 that includes the probability of receiving each treatment in the contrast argument for the main trial, the control is double sized (allocation 2:1) so control vs. active intervention should be \code{prtr=c(0.67,0.33)}. in contrasts where the arms are equally sized (allocation 1:1), then \code{prtr=c(0.5,0.5)}. If \code{prtr=NULL} (the default), then the treatment mechanism Pr(A|W) is estimated, which is usually more efficient (preferred).
#' @param modelfit String variable of value \code{"sl"} for SuperLearner prediction or \code{"glm"} for GLM prediction of E(Y|A,W) and Pr(A|W).
#' @param Q.SL.Library Library of algorithms to include in the SuperLearner for the outcome model
#' @param g.SL.library Library of algorithms to include in the SuperLearner for the treatment model (ignored if prtr is specified)
#' @param seed A seed for the pseudo-random cross-validation split (use for perfectly reproducible results if \code{modelfit="sl"}).
#'
#'
#' @return A \code{tmle()} fit object (see the \code{\link[tmle]{tmle}} package for details).
#'
#' @export
#'
#' @examples
#' # TBD
#'


washb_tmle <- function(Y,tr,W=NULL,id,family="gaussian",contrast,prtr=NULL,modelfit="sl",Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),g.SL.library=Q.SL.library, seed=NULL) {

  require(tmle)
  require(SuperLearner)

  # Make a data.frame that includes the relevant covariates
  if(is.null(W)){
    tmledat <- data.frame(
      id=id[tr==contrast[1]|tr==contrast[2]],
      Y=Y[tr==contrast[1]|tr==contrast[2]],
      tr=tr[tr==contrast[1]|tr==contrast[2]]
    )
  } else{
    tmledat <- data.frame(
      id=id[tr==contrast[1]|tr==contrast[2]],
      Y=Y[tr==contrast[1]|tr==contrast[2]],
      tr=tr[tr==contrast[1]|tr==contrast[2]],
      W[tr==contrast[1]|tr==contrast[2],]
    )
  }

  tmledat$tr <- factor(tmledat$tr,levels=contrast[1:2])

  # restrict to complete cases (print the number of dropped observations, if any)
  n_orig <- dim(tmledat)[1]
  tmledat <- tmledat[complete.cases(tmledat),]
  n_sub  <- dim(tmledat)[1]
  if(n_orig>n_sub) cat("\n-----------------------------------------\nDropping",n_orig-n_sub,"observations due to missing\nvalues in one or more variables\n"," Final sample size:",n_sub,"\n-----------------------------------------\n")

  # if specified, create a vector of treatment probabilities, depending on the type of contrast
  # for control vs. intervention, the control prob should be 2/3 and tr prob should be 1/3 (allocation 2:1)
  # for intervention arm 1 vs intervantion arm 2, prob should be 1/2 for both (allocation 1:1)
  if(!is.null(prtr)) {
    tr_p <- ifelse(tmledat$tr==contrast[1],prtr[1],prtr[2])
    g.SL.library <- NULL
  } else {
    tr_p <- NULL
  }

  # pre-screen the covariates (if specified)
  # see washb_prescreen() in the base functions
  if(!is.null(W)){

    cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    Wscreen <- washb_prescreen(Y=tmledat$Y,Ws=tmledat[,4:ncol(tmledat)],family=family,pval=0.2,print=TRUE)
    Wselect <- subset(tmledat[,4:ncol(tmledat)],select=Wscreen)
    cat("\n-----------------------------------------\n")

    # for covariates that are factors, create indicator variables
    # because the tmle() function cannot handle factors in W
    # this relies on an internal function called design_matrix included in the washb package
    Wselect <- design_matrix(Wselect)

  } else{
    # if W is null, create two empty variables so that tmle() can run
    # (does not affect estimation)
    Wselect <- data.frame(w1=rep(1,length(tmledat$Y)),w2=rep(1,length(tmledat$Y)))
    Q.SL.library <- c("SL.glm")
    g.SL.library <- c("SL.glm")
  }

  # re-parse the variables to use them in tmle()
  tmle_Y <- tmledat$Y
  tmle_A <- ifelse(tmledat$tr==contrast[2],1,0)
  tmle_id <- tmledat$id

  # If the E(Y|A,W) and Pr(A|W) models are fit with GLM, make formula objects
  if(modelfit=="sl") {
    Qform <- NULL
    gform <- NULL
  }
  if(modelfit=="glm") {
    Qform <- as.formula(paste("Y~A+",paste(names(Wselect),collapse=" + ")))
    gform <- as.formula(paste("A~",paste(names(Wselect),collapse=" + ")))
    Q.SL.library <- NULL
    g.SL.library <- NULL
  }

  # estimate the ITT using TMLE
  if(!is.null(seed)) set.seed(seed)
  tmle_fit <- tmle(Y=tmle_Y,
                   A=tmle_A,
                   W=Wselect,
                   id=tmle_id,
                   family=family,
                   Qform=Qform,
                   Q.SL.library=Q.SL.library,
                   g1W=tr_p,
                   gform=gform,
                   g.SL.library=g.SL.library
  )
  cat("\n-----------------------------------------\nEstimation Results:\n-----------------------------------------\n")
  print(summary(tmle_fit))
  cat("\n-----------------------------------------\n")

  return(tmle_fit)
}
