
#' Generalized linear model function for the WASH Benefits study
#'
#' Estimate intention-to-treat parameters with generalized linear models and robust standard errors
#'
#' @details
#' \code{washb_glm_lasso_boot} fits a generalized linear model to estimate intention-to-treat (ITT) effects in a trial. The \code{contrast} argument enables you to specify the arms that you wish to compare (reference group in the first argument, comparison group in the second). To estimate adjusted effects, you can pass a data.frame of adjustment covariates to the \code{W} argument -- by default, covariates are pre-screened to only include those that are associated with the outcome based on a likelihood ratio test. To over-ride the pre-screening algorithm for some or all covariates, use the \code{forcedW} argment.
#'
#' If the design is pair-matched (all primary outcome analyses in WASH B should be pair matched), use the \code{pair} argument to specify an id variable for pairs, and specify the same variable in the id argument to get correct SEs. If the design is not pair-matched, then the id argument should identify the smallest independent unit in the trial (e.g., cluster). The function computes robust standard errors. Note that this function automatically drops observations from the analysis if they are from a pair with no treatment contrast -- this can happen with incomplete randomization blocks in the Kenya trial.
#'
#' Note that for binary outcomes such as diarrhea, the glm model is fit with a log link,
#'  \code{family=binomial(link='log')}, to estimate prevalence ratios rather than the canonical logit link, \code{family='binomial'},
#'  to estimate the odds ratio. Occasionally, a glm model with a non-canonical link function like \code{family=binomial(link='log')} will fail to converge, particularly if the data are sparse.
#'  If this occurs, use a modified poisson regression to estimate prevalence ratio using the argument \code{family=poisson(link='log')}.
#'  See Zou 2004 (https://www.ncbi.nlm.nih.gov/pubmed/15033648) for details.
#'
#'  The function also makes it straight forward to estimate conditional (i.e., subgroup) effects using the optional \code{V} argument.
#'
#' @usage
#' washb_glm_lasso_boot(Y,tr,pair,W=NULL, forcedW=NULL, V=NULL, id,contrast,family="gaussian", pval=0.2, print=TRUE)
#'
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable (ideally a factor), comparison group first
#' @param pair Pair-matched randomization ID variable (in WASH Benefits: block)
#' @param W Optional data frame that includes adjustment covariates (for adjusted estimates)
#' @param forcedW Optional vector of variable names to force as adjustment covariates (no screening)
#' @param V Optional variable name for subgroup analyses, which is interacted with 'tr'.
#' @param id ID variable for independent units (cluster ID)
#' @param contrast Vector of length 2 that includes the groups to contrast, e.g., c("Control","Water")
#' @param family GLM model family (gaussian, binomial, poisson, and negative binomial). Use \code{family=binonial(link='log')} (no quotes) to return prevalence ratios instead of odds ratios when the outcome is binary.  Use "neg.binom" for a Negative binomial model.
#' @param FECR (default is \code{NULL}). Estimate the fecal egg count reduction (FECR) proportion by specifying either \code{FECR='arithmetic'} to estimate it on the artithmetic mean scale or \code{FECR='geometric'} on the geometric mean scale. If \code{FECR='geometric'} ensure that you use log-transformed eggs per gram. When estimating the FECR, also ensure that you specify \code{family='gaussian'} (see details).
#' @param pval The p-value threshold: any variables with a p-value from the likelihood ratio test below this threshold will be returned. Defaults to 0.2
#' @param print Logical for whether to print function output, defaults to TRUE.
#' @param verbose Logical for whether to print names and descriptions of returned list objects
#'
#' @return Returns a list of the risk ratios or risk differences, the variance-covariance matrix, and a vector indexing the rows of observations
#'         used to fit the glm model
#' @export
#'
#' @examples
#'
#' #washb_glm_lasso_boot function applied to the Bangladesh diarrheal disease outcome to determine both unadjusted and adjusted
#' #prevalence ratios between intervention and control arms.
#'
#'
#' #Cleans and merge the enrollment and diarrhea data:
#' library(washb)
#' data(washb_bangladesh_enrol)
#' washb_bangladesh_enrol <- washb_bangladesh_enrol
#' data(washb_bangladesh_diar)
#' washb_bangladesh_diar <- washb_bangladesh_diar
#'
#' # drop svydate and month because they are superceded in the child level diarrhea data
#' #washb_bangladesh_enrol$svydate <- NULL
#' #washb_bangladesh_enrol$month <- NULL
#'
#' # merge the baseline dataset to the follow-up dataset
#' ad <- merge(washb_bangladesh_enrol,washb_bangladesh_diar,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
#'
#' # subset to the relevant measurement
#' # Year 1 or Year 2
#' ad <- subset(ad,svy==1|svy==2)
#'
#' #subset the diarrhea to children <36 mos at enrollment
#' ### (exlude new births that are not target children)
#' ad <- subset(ad,sibnewbirth==0)
#' ad <- subset(ad,gt36mos==0)
#'
#' # Exclude children with missing data
#' ad <- subset(ad,!is.na(ad$diar7d))
#'
#' #Re-order the tr factor for convenience
#' ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
#'
#' #Ensure that month is coded as a factor
#' ad$month <- factor(ad$month)
#'
#' #Sort the data for perfect replication when using V-fold cross-validation
#' ad <- ad[order(ad$block,ad$clusterid,ad$dataid,ad$childid),]
#'
#' ###Create vector of contrasts for each hypothesis to facilitate comparisons between arms.
#' #Hypothesis 1: Each intervention arm vs. Control
#' h1.contrasts <- list(
#'   c("Control","Water"),
#'   c("Control","Sanitation"),
#'   c("Control","Handwashing"),
#'   c("Control","WSH"),
#'   c("Control","Nutrition"),
#'   c("Control","Nutrition + WSH")
#' )
#'
#'
#'
#' ###Unadjusted GLM estimates for diarrheal disease outcome.
#' #As an example, the following code applies the washb_glm_lasso_boot function to compare 7-day recall diarrheal disease prevalence between the sanitation and control arms. Notice that the glm model is fit with a log link, `family=binomial(link='log')`, to estimate prevalence ratios rather than with a logit link, `family="binomial"`, to estimate the odds ratio.
#'
#' Diar.glm.C.S <- washb_glm_lasso_boot(Y=ad$diar7d,tr=ad$tr,pair=ad$block, id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'))
#'
#' #On top of the function's auto-printed output, the washb_glm_lasso_boot function contains a number of objects. For example, `'objectname'$vcv` returns the variance-covariance matrix.
#'
#'
#' #All returned objects are:
#` #'objectname'$TR` to return the treatment effect.
#' #'objectname$fit` to return full glm model estimates.
#' #'objectname$vcv` to return the variance-covariance matrix.
#' #'objectname$rowdropped` to return the vector list of observations included in the model fit.
#' #'objectname$lincom` to return subgroup-specific conditional relative risk estimates if a subgroup V is specified.


washb_glm_lasso_boot <- function(Y,tr,pair=NULL,W, forcedW=NULL, V=NULL, id,contrast,
                                 family="gaussian", pval=0.2, print=TRUE, verbose=FALSE, B=200){
  #options(scipen=20)
  require(tidyverse)


  #Create empty variable used in subgroup analysis
  Subgroups=NULL



  #Make sure W is a dataframe and "tr" is not a covariate
  if(!is.null(W)){
    W<-data.frame(W)
    if(sum("tr" %in% colnames(W))>0){
      colnames(W)[which(colnames(W)=="tr")] <- "trW"
    }
  }



  # Make a data.frame, then restrict to the 2 arms in the contrast
  if(!is.null(pair)){
    if(!is.null(W)){
      glmdat <- data.frame(id,Y,tr,pair,W)
    }else{
      glmdat <- data.frame(id,Y,tr,pair)
    }
    glmdat$tr    <- factor(glmdat$tr,levels=contrast[1:2])
    glmdat$pair <- factor(glmdat$pair)
  }else{
    if(!is.null(W)){
      glmdat <- data.frame(id,Y,tr,W)
    }else{
      glmdat <- data.frame(id,Y,tr)
    }
    glmdat$tr    <- factor(glmdat$tr,levels=contrast[1:2])
  }

  glmdat <- subset(glmdat,tr==contrast[1]|tr==contrast[2])
  glmdat$tr <- factor(glmdat$tr,levels=contrast[1:2])


  #####
  #Block Dropping
  #####
  if(!is.null(pair)){
    #Drop blocks missing comparing arm 1
    n.orig <- dim(glmdat)[1]
    miss<-NULL
    activeOnly<-((subset(glmdat,tr==contrast[1])))
    nomiss<-sort(unique(activeOnly$pair))
    miss1<-(unique(pair)[which(!( unique(pair)%in%(nomiss) ))])

    #Drop blocks missing comparing arm 2
    activeOnly2<-((subset(glmdat,tr==contrast[2])))
    nomiss2<-sort(unique(activeOnly2$pair))
    #print(which(!( unique(glmdat$pair)%in%(nomiss2) )))
    miss2<-(unique(pair)[which(!( unique(pair)%in%(nomiss2) ))])
    miss<-append(miss1,miss2)
    glmdat<-subset(glmdat,!(pair %in% miss))
    n.sub  <- dim(glmdat)[1]
    if(print==TRUE)if(n.orig>n.sub) cat("\n-----------------------------------------\n","Starting N:  ",n.orig,"\nN after block dropping: ",n.sub)
    if(print==TRUE)if(n.orig>n.sub) cat("\n-----------------------------------------\n","Pairs/blocks dropped due to missingness in at least one treatment level:\n",sort(unique(miss)),"\n\nDropping",n.orig-n.sub,"observations due to missing pairs.","\n-----------------------------------------\n")
  }


  # restrict to complete cases and save a vector indexing observations dropped
  n.orig <- dim(glmdat)[1]
  rowdropped<-rep(1,nrow(glmdat))
  rowdropped[which(complete.cases(glmdat))]<-0
  glmdat <- glmdat[complete.cases(glmdat),]
  n.sub  <- dim(glmdat)[1]
  if(print==TRUE)if(n.orig>n.sub) cat("\n-----------------------------------------\nDropping",n.orig-n.sub,"observations due to missing values in 1 or more variables\n","Final sample size:",n.sub,"\n-----------------------------------------\n")



  #extract colnames of W
  if(!is.null(W)){colnamesW<-names(W)}
  #split W into screened and forced adjustment covariates
  if(!is.null(W)){
    if(!is.null(V)){
      forcedW=c(V,forcedW)
    }
    if(!is.null(forcedW)){
      screenW<-subset(glmdat, select=colnamesW)
      toexclude <- names(screenW) %in% forcedW
      if(length(which(toexclude==TRUE))!=length(forcedW)) stop("A forcedW variable name is not a variable within the W data frame.")
      screenW=screenW[!toexclude]
      if(ncol(screenW)==0){screenW<-NULL}
      if(print==TRUE){
        cat("\n-----------------------------------------\nInclude the following adjustment covariates without screening:\n-----------------------------------------\n")
        print(forcedW, sep="\n")
      }
    }else{
      screenW<-subset(glmdat, select=colnamesW)
    }
  }else{
    screenW<-NULL
  }

  colnamesW <- colnames(screenW)

  # if(!is.null(pair)){
  #   if(!is.null(forcedW)){
  #       dmat <- subset(glmdat,select=c("Y","tr",forcedW,Wscreen,"pair"))
  #   } else {
  #       dmat <- subset(glmdat,select=c("Y","tr",Wscreen,"pair"))
  #   }
  # }else{
  #   if(!is.null(forcedW)){
  #       dmat <- subset(glmdat,select=c("Y","tr",forcedW,Wscreen))
  #   } else {
  #       dmat <- subset(glmdat,select=c("Y","tr",Wscreen))
  #   }
  # }

  if(!is.null(pair)){pair = "pair"}
  dmat <- subset(glmdat,select=c("Y","tr","id",forcedW,colnamesW,pair))

       if(!is.null(V)){
         stop("Interaction/subgroup variable V not yet supported")
       }else{


#
#          data=dmat
#          clusterid="id"
#          Ws=colnamesW
#          B = 200
#          confint.level = 0.95
#          n.cores = 1

        suppressWarnings(fit <- cowboy_glm(data=dmat, clusterid="id", Ws=colnamesW, forcedW=forcedW, pair=pair,
                                          family = family,
                                          #temp!
                                          B = B,
                                          confint.level = 0.95, n.cores = 1))

         if(family=="gaussian"){
           modelfit <- data.frame(
             est=fit$boot.coefs[2],
             ci.lb =fit$percentile.interval[2,1],
             ci.ub =fit$percentile.interval[2,2],
             se = fit$boot.sds[2]
           )
         }else{
           modelfit <- data.frame(
             RR=exp(fit$boot.coefs[2]),
             est=fit$boot.coefs[2],
             ci.lb =exp(fit$percentile.interval[2,1]),
             ci.ub =exp(fit$percentile.interval[2,2]),
             se = fit$boot.sds[2]
           )
         }


      }

      return(list(TR=modelfit, fit=fit))

}

