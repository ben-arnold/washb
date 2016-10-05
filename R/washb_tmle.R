#' Target maximum likelihood estimation of intention-to-treat effects in the WASH Benefits trials
#'
#' Estimate intention-to-treat parameters using targeted maximum likelihood estimation (TMLE), potentially adjusted for covariates and missing outcomes
#'
#' @details
#' The \code{washb_tmle} function is mainly a convenience wrapper for the \code{\link[tmle]{tmle}} function. It estimates intention-to-treat effects in a trial using targeted maximum likelihood estimation (TMLE). In brief, the function does the following: it restricts the data to complete observations in the two arms listed in the \code{contrast} argument, it pre-screens covariates (\code{W}), if specified, to select those that have a univariate association with the outcome, and then it estimates the intention-to-treat effect using TMLE.  If \code{family='binomial'}, then the function returns effects on the absolute, relative, and odds ratio scale. If \code{Delta} is specified (i.e., observations with missing outcomes are included), then the function will adjust the effects for missingness using inverse probability of censoring weights, with the weights estimated using super learning of \code{Pr(Delta|A,W)}.
#'
#' If the analysis is pair-matched (as for primary outcomes), be sure to specify the pair ID in the \code{id} argument. Do not include pair IDs in the adjustment covariate set.
#'
#' If adjustment covariates (\code{W}) are specified, then by default they are pre-screened and the subset that is associated with the outcome based on a likelihood ratio test are used in the estimation. There are some other important defaults to be aware of. First, if the argument \code{prtr=NULL} (the default), then the function estimates the treatment mechanism rather than treating it as known. Estimating the treatment mechanism theoretically increases precision and empirically we have found this as well (in the Bangladesh primary analysis). Second, the function uses the \code{\link[SuperLearner]{SuperLearner}} algorithm to adjust for covariates. The default algorithm library includes the simple mean, GLM, Bayes GLM with non-informative priors, generalized additive models (degree 2), and lasso (glmnet). You can type \code{listWrappers()} to see the full set of algorithms implemented in the super learner. If you just wish to use a main effects GLM model to adjust for the covariates, then you can specify \code{Q.SL.library="SL.glm"}.
#'
#' If you want to adjust for missing outcomes in the analysis, then you need to include observations that have a missing outcome (\code{Y}) with \code{Delta=0} for those observations. Observations with missing outcomes should have treatment (\code{tr}) and covariate (\code{W}) information, which are used to create weights for \code{Pr(Delta|A,W)}.
#'
#' Note: this function depends on the \code{\link[tmle]{tmle}} package, the \code{\link[SuperLearner]{SuperLearner}} package, as well as the internal washb_prescreen() and design_matrix() functions.
#'
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable, comparison group first
#' @param W Data frame that includes adjustment covariates
#' @param id ID variable for independent units. For pair-matched designs, this is the matched pair.
#' @param pair (Optional if there is no missingness in pair-matching) The matched pair unit (In WASH Benefits, blocks). This argument it used to drop pair levels if there is missingness in one ore more levels of the contrast. There treatment missingness in the Kenya blocks (not all blocks contain all interventions) but not in the Bangladesh blocks,
#' @param Delta indicator of missing outcome. 1 - observed, 0 - missing.
#' @param family Outcome family: \code{gaussian} (continuous outcomes, like LAZ) or \code{binomial} (binary outcomes like diarrhea or stunting)
#' @param contrast Vector of length 2 that includes the treatment groups to contrast (e.g., \code{contrast=c('Control','Nutrition')}).
#' @param prtr Vector of length 2 that includes the probability of receiving each treatment in the contrast argument for the main trial, the control is double sized (allocation 2:1) so control vs. active intervention should be \code{prtr=c(0.67,0.33)}. in contrasts where the arms are equally sized (allocation 1:1), then \code{prtr=c(0.5,0.5)}. If \code{prtr=NULL} (the default), then the treatment mechanism Pr(A|W) is estimated, which is usually more efficient (higher precision of the estimates).
#' @param Q.SL.Library Library of algorithms to include in the SuperLearner for the outcome model
#' @param g.SL.library Library of algorithms to include in the SuperLearner for the treatment model Pr(A|W) (ignored if prtr is specified), and for the missingness model Pr(Delta|A,W) (if Delta is specified)
#' @param pval The p-value threshold used to pre-screen covariates (\code{W}) based on a likelihood ratio test in a univariate regression with the outcome (\code{Y}). Variables with a univariate association p-value below this threshold will be used in the final model. Defaults to 0.2.
#' @param seed A seed for the pseudo-random cross-validation split used in model selection (use for perfectly reproducible results).
#' @param print Logical for printed output, defaults to true. If false, no output will be printed to the console if the returned object is saved to an R object.
#'
#' @return A \code{tmle()} fit object (see the \code{\link[tmle]{tmle}} package for details). The \code{$estimates} list includes parameter estimates along with variance estimates and confidence intervals.
#'
#' @references
#' Gruber S, van der Laan M. tmle: An R Package for Targeted Maximum Likelihood Estimation. J Stat Softw. 2012;51: 1–35. \link{https://www.jstatsoft.org/article/view/v051i13}
#'
#' Balzer LB, van der Laan MJ, Petersen ML, SEARCH Collaboration. Adaptive pre-specification in randomized trials with and without pair-matching. Stat Med. 2016; doi:10.1002/sim.7023 \link{http://onlinelibrary.wiley.com/doi/10.1002/sim.7023/abstract}
#'
#' @examples
#' #TBD
#'
#'@export
#'
#'
#' @examples


washb_tmle <- function(Y,tr,W=NULL,id,pair=NULL, Delta = rep(1,length(Y)),family="gaussian",contrast,prtr=NULL,Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),g.SL.library=Q.SL.library, pval=0.2, seed=NULL, print=TRUE) {

  require(tmle)
  require(SuperLearner)
  #Create empty variable used in subgroup analysis
  Subgroups=NULL

  # Make a data.frame, restricted to the 2 arms in the contrast
  if(is.null(W)){
    if(is.null(pair)){
    tmledat <- data.frame(
      id=id[tr==contrast[1]|tr==contrast[2]],
      Y=Y[tr==contrast[1]|tr==contrast[2]],
      Delta=Delta[tr==contrast[1]|tr==contrast[2]],
      tr=tr[tr==contrast[1]|tr==contrast[2]]
    )
    } else{
      tmledat <- data.frame(
        id=id[tr==contrast[1]|tr==contrast[2]],
        Y=Y[tr==contrast[1]|tr==contrast[2]],
        Delta=Delta[tr==contrast[1]|tr==contrast[2]],
        tr=tr[tr==contrast[1]|tr==contrast[2]],
        pair=pair[tr==contrast[1]|tr==contrast[2]]
      )
    }
  } else{
    if(is.null(pair)){
    tmledat <- data.frame(
      id=id[tr==contrast[1]|tr==contrast[2]],
      Y=Y[tr==contrast[1]|tr==contrast[2]],
      Delta=Delta[tr==contrast[1]|tr==contrast[2]],
      tr=tr[tr==contrast[1]|tr==contrast[2]],
      W[tr==contrast[1]|tr==contrast[2],]
    )
    } else{
      tmledat <- data.frame(
        id=id[tr==contrast[1]|tr==contrast[2]],
        Y=Y[tr==contrast[1]|tr==contrast[2]],
        Delta=Delta[tr==contrast[1]|tr==contrast[2]],
        tr=tr[tr==contrast[1]|tr==contrast[2]],
        W[tr==contrast[1]|tr==contrast[2],],
        pair=pair[tr==contrast[1]|tr==contrast[2]]
      )
    }
  }

  tmledat$tr <- factor(tmledat$tr,levels=contrast[1:2])




  #####
  #Block Dropping
  #####
  if(!is.null(pair)){
    #Drop blocks missing comparing arm 1
    n.orig <- dim(tmledat)[1]
    miss<-NULL
    activeOnly<-((subset(tmledat,tr==contrast[1])))
    nomiss<-sort(unique(activeOnly$pair))
    miss1<-(unique(pair)[which(!( unique(pair)%in%(nomiss) ))])

    #Drop blocks missing comparing arm 2
    activeOnly2<-((subset(tmledat,tr==contrast[2])))
    nomiss2<-sort(unique(activeOnly2$pair))
    miss2<-(unique(pair)[which(!( unique(pair)%in%(nomiss2) ))])
    miss<-append(miss1,miss2)
    tmledat<-subset(tmledat,!(pair %in% miss))
    n.sub  <- dim(tmledat)[1]
    if(print==TRUE)if(n.orig>n.sub) cat("\n-----------------------------------------\n","Starting N:  ",n.orig,"\nN after block dropping: ",n.sub)
    if(print==TRUE)if(n.orig>n.sub) cat("\n-----------------------------------------\n","Pairs/blocks dropped due to missingness in at least one treatment level:\n",sort(unique(miss)),"\n\nDropping",n.orig-n.sub,"observations due to missing pairs.","\n-----------------------------------------\n")
  }



  # restrict to complete cases (print the number of dropped observations, if any)
  n_orig <- dim(tmledat)[1]
  tmledat <- tmledat[complete.cases(tmledat),]
  n_sub  <- dim(tmledat)[1]
  if(print==TRUE){if(n_orig>n_sub) cat("\n-----------------------------------------\nDropping",n_orig-n_sub,"observations due to missing\nvalues in one or more variables\n"," Final sample size:",n_sub,"\n-----------------------------------------\n")}

  # if specified, create a vector of treatment probabilities, depending on the type of contrast
  # for control vs. intervention, the control prob should be 2/3 and tr prob should be 1/3 (allocation 2:1)
  # for intervention arm 1 vs intervantion arm 2, prob should be 1/2 for both (allocation 1:1)
  if(is.null(prtr)) {
    tr_p <- NULL
  } else {
    tr_p <- ifelse(tmledat$tr==contrast[1],prtr[1],prtr[2])
    g.SL.library <- NULL
  }

  # pre-screen the covariates (if specified)
  # see washb_prescreen() in the base functions
  if(is.null(W)){
    # if W is null, create two empty variables so that tmle() can run
    # (does not affect estimation)
    Wselect <- data.frame(w1=rep(1,length(tmledat$Y)),w2=rep(1,length(tmledat$Y)))
    Q.SL.library <- c("SL.glm")
    g.SL.library <- c("SL.glm")
  } else{
    if(print==TRUE){cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")}
    Wscreen <- washb_prescreen(Y=tmledat$Y,Ws=tmledat[,5:ncol(tmledat)],family=family,pval=pval,print=print)

    if(print==TRUE){cat("\n-----------------------------------------\n")}

    # for covariates that are factors, create indicator variables
    # because the tmle() function cannot handle factors in W
    # this relies on an internal function called design_matrix included in the washb package
    if(length(Wscreen)>0){
      Wselect <- subset(tmledat[,4:ncol(tmledat)],select=Wscreen)
      Wselect <- design_matrix(Wselect)
    } else{
      if(print==TRUE){cat("\n\nNo covariates were associated with the outcome\nProceeding with no adjustment...")}
      # create a data frame with 2 columns of 1s -- allows tmle() to run, but does no adjustment
      Wselect <- data.frame(w1=rep(1,length(tmledat$Y)),w2=rep(1,length(tmledat$Y)))
    }

  }

  # re-parse the variables to use them in tmle()
  tmle_Y <- tmledat$Y
  tmle_A <- ifelse(tmledat$tr==contrast[2],1,0)
  tmle_Delta <- tmledat$Delta
  tmle_id <- tmledat$id

  # estimate the ITT using TMLE
  if(!is.null(seed)) set.seed(seed)
  tmle_fit <- tmle(Y=tmle_Y,
                   A=tmle_A,
                   W=Wselect,
                   Delta=tmle_Delta,
                   id=tmle_id,
                   family=family,
                   Q.SL.library=Q.SL.library,
                   g1W=tr_p,
                   g.SL.library=g.SL.library
  )
  if(print==TRUE){
  cat("\n-----------------------------------------\nEstimation Results:\n-----------------------------------------\n")
  print(summary(tmle_fit))
  cat("\n-----------------------------------------\n")
  }
  return(tmle_fit)
}
