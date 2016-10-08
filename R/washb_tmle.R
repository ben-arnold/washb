#' Target maximum likelihood estimation of intention-to-treat effects in the WASH Benefits trials
#'
#' Estimate intention-to-treat parameters using targeted maximum likelihood estimation (TMLE), potentially adjusted for covariates and missing outcomes
#'
#' @details
#' The \code{washb_tmle} function is mainly a convenience wrapper for the \code{\link[tmle]{tmle}} function. It estimates intention-to-treat effects in a trial using targeted maximum likelihood estimation (TMLE). In brief, the function does the following: it restricts the data to complete observations in the two arms listed in the \code{contrast} argument, it pre-screens covariates (\code{W}), if specified, to select those that have a univariate association with the outcome, and then it estimates the intention-to-treat effect using TMLE.  If \code{family='binomial'}, then the function returns effects on the absolute, relative, and odds ratio scale. If \code{Delta} is specified (i.e., observations with missing outcomes are included), then the function will adjust the effects for missingness using inverse probability of censoring weights, with the weights estimated using super learning of \code{Pr(Delta|A,W)}.
#'
#' If the analysis is pair-matched (as for primary outcomes), be sure to specify the pair ID in the \code{id} argument. Do not include pair IDs in the adjustment covariate set.
#'
#' If adjustment covariates (\code{W}) are specified, then by default they are pre-screened and the subset that is associated with the outcome based on a likelihood ratio test are used in the estimation. There are some other important defaults to be aware of. First, the \code{washb_tmle} function estimates the treatment mechanism even though it is a randomized trial. There are two reasons for this -- one theoretical and one practical. The theoretical reason is that estimating the treatment mechanism gains efficiency (see Balzer et al. 2016); the practical reason is that unless the analysis is conducted at the cluster level (i.e., providing cluster means to the \code{washb_tmle} function), then the empirical treatment probabilities differ slightly due to varying cluster sizes. Estimating the treatment mechanism ensures that the variance calculation correctly accounts for the empirical treatment probabilities in the data.
#'
#'Another default is that \code{washb_tmle} uses the \code{\link[SuperLearner]{SuperLearner}} algorithm to adjust for covariates and to predict the treatment mechanism and censoring mechanism (if adjusting for missing outcomes). The default algorithm library includes the simple mean, main terms GLM, main terms Bayes GLM with non-informative priors, generalized additive models (degree 2), and lasso (glmnet). These are the pre-specified algorithms from the original trial statistical analysis plan. You can type \code{listWrappers()} to see the full set of algorithms implemented in the super learner. If you just wish to use a main effects GLM model to adjust for the covariates, then you can specify \code{Q.SL.library="SL.glm"}.  If you are dealing with very small sample sizes (e.g., in a substudy), then you may wish to use even simpler libraries, such as a set of univariate regressions (as in Balzer et al. 2016).
#'
#'Finally, by default the function uses the same algorithm library to predict the outcome (\code{Q.SL.library}) and the treatment and censoring mechanisms (\code{g.SL.library}). You can specify a different library for the treatment and censoring mechanisms using the \code{g.SL.library} argument.
#'
#' If you want to adjust for missing outcomes in the analysis, then you need to include observations that have a missing outcome (\code{Y}) with \code{Delta=0} for those observations. Observations with missing outcomes should have treatment (\code{tr}) and covariate (\code{W}) information, which are used to create weights for \code{Pr(Delta|A,W)}.
#'
#' Note: this function depends on the \code{\link[tmle]{tmle}} package, the \code{\link[SuperLearner]{SuperLearner}} package, as well as the internal washb_prescreen() and design_matrix() functions.
#'
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable, comparison group first
#' @param W Data frame that includes adjustment covariates
#' @param id ID variable for independent units. For pair-matched designs, this is the matched pair and should be the same as the \code{pair} argument. For analyses that are not pair-matched, then it should typically be the cluster.
#' @param pair An optional ID variable to identify the matched pair unit (In WASH Benefits, blocks) if conducting a matched-pair analysis. This argument is used to drop pairs that are missing one or more treatment groups. Incomplete pairs is not an issue in the overall Bangladesh trial (there were no incomplete blocks), but is an issue in the Kenya trial where there were some incomplete blocks.
#' @param Delta indicator of missing outcome. 1 - observed, 0 - missing.
#' @param family Outcome family: \code{gaussian} (continuous outcomes, like LAZ) or \code{binomial} (binary outcomes like diarrhea or stunting)
#' @param contrast Vector of length 2 that includes the treatment groups to contrast (e.g., \code{contrast=c('Control','Nutrition')}).
#' @param Q.SL.Library Library of algorithms to include in the SuperLearner for the outcome model
#' @param g.SL.library Library of algorithms to include in the SuperLearner for the treatment model Pr(A|W) (ignored if prtr is specified), and for the missingness model Pr(Delta|A,W) (if Delta is specified)
#' @param pval The p-value threshold used to pre-screen covariates (\code{W}) based on a likelihood ratio test in a univariate regression with the outcome (\code{Y}). Variables with a univariate association p-value below this threshold will be used in the final model. Defaults to 0.2.
#' @param seed A seed for the pseudo-random cross-validation split used in model selection (use for perfectly reproducible results).
#' @param print Logical for printed output, defaults to true. If false, no output will be printed to the console if the returned object is saved to an R object.
#'
#' @return A \code{tmle()} fit object (see the \code{\link[tmle]{tmle}} package for details). The \code{$estimates} list includes parameter estimates along with variance estimates and confidence intervals.
#'
#' @references
#' Gruber S, van der Laan M. tmle: An R Package for Targeted Maximum Likelihood Estimation. J Stat Softw. 2012;51: 1–35. (https://www.jstatsoft.org/article/view/v051i13)
#'
#' Balzer LB, van der Laan MJ, Petersen ML, SEARCH Collaboration. Adaptive pre-specification in randomized trials with and without pair-matching. Stat Med. 2016; doi:10.1002/sim.7023 (http://onlinelibrary.wiley.com/doi/10.1002/sim.7023/abstract)
#'
#' @examples
#' #TBD
#'
#'@export
#'
#'

washb_tmle <- function(Y,tr,W=NULL,id = 1:length(Y), pair=NULL, Delta = rep(1,length(Y)), family="gaussian",contrast,Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"),g.SL.library=Q.SL.library, pval=0.2, seed=NULL, print=TRUE) {

  require(tmle)
  require(SuperLearner)


  # for pair matched analyses, ensure that the pair-and the ID variables are the same
  # throw an error and message if they are not the same
  if(!is.null(pair)){
    cat("\n-----------------------------------------","\nBy specifying the pair argument,\nyou have indicated that this is a matched pair analysis.\n\nNote: the analysis will only include pairs\nthat have a contrast in the treatment variable.","\n-----------------------------------------")

    fnargs <- as.list( match.call() )
    if(length(fnargs$id)==0) fnargs$id <- "(unspecified), which defaults to 1:length(Y)"

    if(fnargs$pair[[length(fnargs$pair)]] != fnargs$id[[length(fnargs$id)]] ) {
      stop(paste("\nBy specifying the pair argument, you have indicated this is a pair-matched analysis.\n\nTo get correct variance, the variable you pass to id\nmust be the same as the variable you pass to pair. \n\nIf you are not doing a pair-matched analysis, then specify pair=NULL\n(or leave it unspecified).\n\n   You specified pair=",fnargs$pair[[length(fnargs$pair)]],"and id=",fnargs$id[[length(fnargs$id)]]))
    }

  }


  # Make a data.frame, restricted to the 2 arms in the contrast
  if(is.null(W)){
    if(is.null(pair)){
      tmledat <- data.frame(id,Y,Delta,tr)
    } else{
      tmledat <- data.frame(id,pair,Y,Delta,tr)
    }
  } else{
    if(is.null(pair)){
      tmledat <- data.frame(id,Y,Delta,tr,W)
    } else{
      tmledat <- data.frame(id,pair,Y,Delta,tr,W)
    }
  }
  tmledat <- subset(tmledat,tr==contrast[1]|tr==contrast[2])
  tmledat$tr <- factor(tmledat$tr,levels=contrast[1:2])

  # if pair is specified, drop any pairs that have no treatment contrast in them
  if(!is.null(pair)){
    #Drop blocks missing contrast arm 1
    n.orig <- dim(tmledat)[1]
    miss<-NULL
    activeOnly<-((subset(tmledat,tr==contrast[1])))
    nomiss<-sort(unique(activeOnly$pair))
    miss1<-(unique(pair)[which(!( unique(pair)%in%(nomiss) ))])

    #Drop blocks missing contrast arm 2
    activeOnly2<-((subset(tmledat,tr==contrast[2])))
    nomiss2<-sort(unique(activeOnly2$pair))
    miss2<-(unique(pair)[which(!( unique(pair)%in%(nomiss2) ))])
    miss<-append(miss1,miss2)
    tmledat<-subset(tmledat,!(pair %in% miss))
    n.sub  <- dim(tmledat)[1]
    if( (print==TRUE) & (n.orig>n.sub) ) {
      cat("\n-----------------------------------------","\nThere were",length(unique(miss)),"pairs dropped because they were\nmissing at least one treatment level.\nThis is the list of their IDs:\n",sort(unique(miss)))
      cat("\n-----------------------------------------","\nStarting N:  ",n.orig,"\nN after dropping incomplete blocks: ",n.sub,"\n\nTotal of",n.orig-n.sub,"observations dropped\n because of unmatched pairs.","\n-----------------------------------------\n")
    }
  }



  # restrict to complete cases (print the number of dropped observations, if any)
  n_orig <- dim(tmledat)[1]
  tmledat <- tmledat[complete.cases(tmledat),]
  n_sub  <- dim(tmledat)[1]
  if( (print==TRUE) & (n_orig>n_sub)) {
    cat("\n-----------------------------------------\nTotal of",n_orig-n_sub,"observations dropped due to missing\nvalues in one or more variables\n"," Final sample size:",n_sub,"\n-----------------------------------------\n")
    }


  # pre-screen the covariates (if specified)
  # see washb_prescreen() in the base functions
  if(!is.null(W)){

    if(print==TRUE){
      cat("\n-----------------------------------------\nPre-screening the adjustment covariates\nusing a univariate liklihood ratio test:\n-----------------------------------------\n")
      }

    Wscreen <- washb_prescreen(Y=tmledat$Y,Ws=tmledat[,names(W)],family=family,pval=pval,print=print)

    if(print==TRUE){
      cat("\n-----------------------------------------\n")
      }

    # format selected covariates (if any) for TMLE
    # for covariates that are factors, create indicator variables
    # because the tmle() function cannot handle factors in W
    # this relies on an internal function called design_matrix included in the washb package
    if(length(Wscreen)>0){
      Wselect <- tmledat[,Wscreen]
      Wselect <- design_matrix(Wselect)

      # if 1 or fewer covariates are selected, but glmnet is included in the library,
      # remove it from the library because glmnet (lasso) requires 2 or more covariates to run
      if(ncol(Wselect)<=1 & (length(grep("SL.glmnet",Q.SL.library)) + length(grep("SL.glmnet",g.SL.library)) >0)){
        cat("\nNOTE: Dropping SL.glmnet from the library\nbecause there is only 1 covariate selected and glmnet\nrequires 2+ covariates to run\n")
        Q.SL.library <- Q.SL.library[-grep("SL.glmnet",Q.SL.library)]
        g.SL.library <- g.SL.library[-grep("SL.glmnet",g.SL.library)]
      }

    } else{
        cat("\n\nSince no covariates were associated with the outcome,\nthe estimates below are unadjusted...")
        if(n_orig>n_sub){
          cat("\n\nIn this case, since",n_orig-n_sub,"observations were dropped\ndue to missing covariates,\nthose observations were not included in the analysis.\nIt would be best to re-estimate the effect without covariates (W=NULL)\nto avoid unnecessarily dropping these observations.")
        }
        W <- NULL
    }
  }

  # if W is null or if no covariates were selected
  # create a data frame with 2 columns of 1s -- allows tmle() to run, but does no adjustment
  # and set the SL libraries to GLM only
  if(is.null(W)) {
    Wselect <- data.frame(w1=rep(1,length(tmledat$Y)),w2=rep(1,length(tmledat$Y)))
    Q.SL.library <- c("SL.glm")
    g.SL.library <- c("SL.glm")
  }


  # re-parse the variables and final-format to use them in tmle()
  tmle_Y <- tmledat$Y
  tmle_A <- ifelse(tmledat$tr==contrast[2],1,0)
  tmle_Delta <- tmledat$Delta
  tmle_id <- as.numeric(tmledat$id)

  # estimate the ITT using TMLE
  if(!is.null(seed)) set.seed(seed)
  tmle_fit <- tmle(Y=tmle_Y,
                   A=tmle_A,
                   W=Wselect,
                   Delta=tmle_Delta,
                   id=tmle_id,
                   family=family,
                   Q.SL.library=Q.SL.library,
                   g.SL.library=g.SL.library
  )
  if(print==TRUE){
    cat("\n-----------------------------------------\nEstimation Results:\n-----------------------------------------\n")
    print(summary(tmle_fit))
    cat("\n-----------------------------------------\n")
  }
  return(tmle_fit)
}
