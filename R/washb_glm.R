


#' Generalized linear model function for WASH Benefits study.
#' washb_glm
#'
#' Note that in the WASH Benefits diarrheal disease primary outcome analysis, the glm model is fit with a log link,
#'  `family=binomial(link='log')`, to estimate prevalence ratios rather than with a logit link, `family="binomial"`,
#'  to estimate the odds ratio.
#'
#'  Occasionally, a glm model with a non-canonical link function like `family=binomial(link='log')` will fail to converge.
#'  If this occurs, use a modified poisson regression to estimate prevalence ratio using the argument `family=poisson(link='log')`.
#'  See [Zou 2004](http://www.uvm.edu/~rsingle/stat380/F04/possible/Zou-AJE-2004_PoissonRegBinaryData.pdf) for details.
#'
#' @usage
#' washb_glm(Y,tr,pair,W=NULL, forcedW=NULL, V=NULL, id,contrast,family="gaussian", pval=0.2, print=TRUE)
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
#' @param family GLM model family (gaussian, binomial, poisson, and negative binomial). Use "binonial(link='log')" to return prevalence ratios instead of odds ratios when the outcome is binary.  Use "neg.binom" for a Negative binomial model.
#' @param pval The p-value threshold: any variables with a p-value from the lielihood ratio test below this threshold will be returned. Defaults to 0.2
#' @param print Logical for whether to print function output, defaults to TRUE.
#'
#' @return Returns a list of the risk ratios or risk differences, the variance-covariance matrix, and a vector indexing the rows of observations
#'         used to fit the glm model
#' @export
#'
#' @examples
#'
#' #washb_glm function applied to the Bangladesh diarrheal disease outcome to determine both unadjusted and adjusted
#' #prevalence ratios between intervention and control arms.
#'
#'
#' Cleans and merge the enrollment and diarrhea data:
#' library(washb)
#' data(washb_bd_enrol)
#' data(washb_bd_diar)
#'
#' # drop svydate and month because they are superceded in the child level diarrhea data
#' #washb_bd_enrol$svydate <- NULL
#' #washb_bd_enrol$month <- NULL
#'
#' # merge the baseline dataset to the follow-up dataset
#' ad <- merge(washb_bd_enrol,washb_bd_diar,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
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
#' #As an example, the following code applies the washb_glm function to compare 7-day recall diarrheal disease prevalence between the sanitation and control arms. Notice that the glm model is fit with a log link, `family=binomial(link='log')`, to estimate prevalence ratios rather than with a logit link, `family="binomial"`, to estimate the odds ratio.
#'
#' Diar.glm.C.S <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, id=ad$clusterid, contrast=c("Control","Sanitation"), family=binomial(link='log'))
#'
#' On top of the function's auto-printed output, the washb_glm function contains a number of objects. For example, `'objectname'$RDfit` returns the risk difference instead of the risk ratio for all covariates (not applicable when the glm  model is gaussian).
#' Diar.glm.C.S$RDfit[1:2,]
#'
#' #Note, the `[1:2,]` index at the end is added here so that only the intercept and treatment effect estimates are printed to save space. Running the code `Diar.glm.C.W$RDfit` outputs all 89 estimated point estimates and confidence intervals for the 89 pair-matched block dummy variables (because there are 90 blocks).
#'
#' #All returned objects are:
#` 'objectname'$TR` to return the treatment effect.
#' 'objectname$fit` to return full glm model estimates.
#' 'objectname$RDfit` to return the risk difference of the treatment (and all covariates, including block pairs).
#' 'objectname$vcv` to return the variance-covariance matrix.
#' 'objectname$rowdropped` to return the vector list of observations included in the model fit.
#' 'objectname$lincom` to return subgroup-specific conditional relative risk estimates if a subgroup V is specified.
#' 'objectname$lincomRD` to return subgroup-specific conditional risk difference estimates if a subgroup V is specified.





washb_glm <- function(Y,tr,pair,W=NULL, forcedW=NULL, V=NULL, id,contrast,family="gaussian", pval=0.2, print=TRUE) {
  require(sandwich)
  require(lmtest)
  options(scipen=20)
  #Create empty variable used in subgroup analysis
  Subgroups=NULL

  if(!is.null(W)){
    glmdat <- data.frame(
      id=id[tr==contrast[1]|tr==contrast[2]],
      Y=Y[tr==contrast[1]|tr==contrast[2]],
      tr=tr[tr==contrast[1]|tr==contrast[2]],
      pair=pair[tr==contrast[1]|tr==contrast[2]],
      W[tr==contrast[1]|tr==contrast[2],]
      )
      #Fix variable name error if W is a single variable
      if(ncol(W)==1){
       colnames(glmdat)[5]<-  colnames(W)
        }
    }else{
    glmdat <- data.frame(
      id=id[tr==contrast[1]|tr==contrast[2]],
      Y=Y[tr==contrast[1]|tr==contrast[2]],
      tr=tr[tr==contrast[1]|tr==contrast[2]],
      pair=pair[tr==contrast[1]|tr==contrast[2]]
    )
  }
  glmdat$tr    <- factor(glmdat$tr,levels=contrast[1:2])
  glmdat$pair <- factor(glmdat$pair)


  # restrict to complete cases and save a vector indexing observations dropped
  n.orig <- dim(glmdat)[1]
  rowdropped<-rep(1,nrow(glmdat))
  rowdropped[which(complete.cases(glmdat))]<-0
  glmdat <- glmdat[complete.cases(glmdat),]
  n.sub  <- dim(glmdat)[1]
  if(print==TRUE)if(n.orig>n.sub) cat("\n-----------------------------------------\nDropping",n.orig-n.sub,"observations due to missing values in 1 or more variables\n","Final sample size:",n.sub,"\n-----------------------------------------\n")



  #split W into screened and forced adjustment covariates
  if(!is.null(W)){
    if(!is.null(V)){
      forcedW=c(V,forcedW)
    }
    if(!is.null(forcedW)){
      screenW<-subset(glmdat, select=colnames(W))
      toexclude <- names(screenW) %in% forcedW
      if(length(which(toexclude==TRUE))!=length(forcedW)) stop("A forcedW variable name is not a variable within the W data frame.")
      screenW=screenW[!toexclude]
      if(ncol(screenW)==0){screenW<-NULL}
      if(print==TRUE){
        cat("\n-----------------------------------------\nInclude the following adjustment covariates without screening:\n-----------------------------------------\n")
        print(forcedW, sep="\n")
        }
    }else{
      screenW<-subset(glmdat, select=colnames(W))
    }
  }else{
    screenW<-NULL
  }

  if(!is.null(screenW)){
    # pre-screen the covariates
    # see Wprescreen() in the base functions
    if(print==TRUE)cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    suppressWarnings(Wscreen <- washb_prescreen(Y=glmdat$Y,Ws=screenW,family=family, pval=pval, print=print))
  }else{
    Wscreen=NULL
  }
    if(!is.null(forcedW)){
      if(!is.null(Wscreen)){
        dmat <- subset(glmdat,select=c("Y","tr",forcedW,Wscreen,"pair"))
        }else{
        dmat <- subset(glmdat,select=c("Y","tr",forcedW,"pair"))
        }
    } else {
      if(!is.null(Wscreen)){
        dmat <- subset(glmdat,select=c("Y","tr",Wscreen,"pair"))
      }else{
        dmat <- subset(glmdat,select=c("Y","tr","pair"))
        }
      }

  if(family[1]=="binomial"|family[1]=="poisson"){

    if(!is.null(V)){
      colnames(dmat)[which(colnames(dmat)==V)]<-"V"
      if( class(dmat$V)=="factor") Subgroups<-levels(dmat$tr:dmat$V)
      if( class(dmat$V)!="factor") warning('V is not a factor variable within the W covariate data frame. An interaction term will be added to the model but not linear combination of coefficients will be calculated.')
      suppressWarnings(fit <- glm(Y~tr*V+. ,family=family,data=dmat))
      vcovCL <- sandwichSE(dmat,fm=fit,cluster=glmdat$id)
      rfit <- coeftest(fit, vcovCL)

      #fit OLS risk difference model
      fit.rd<-lm(Y~tr*V+.,data=dmat)
      vcovCL.rd <- sandwichSE(dmat,fm=fit.rd,cluster=glmdat$id)
      RDfit <- coeftest(fit.rd, vcovCL.rd)
    }else{
      suppressWarnings(fit <- glm(Y~.,family=family,data=dmat))
      vcovCL <- sandwichSE(dmat,fm=fit,cluster=glmdat$id)
      rfit <- coeftest(fit, vcovCL)

      #fit OLS risk difference model
      fit.rd<-lm(Y~.,data=dmat)
      vcovCL.rd <- sandwichSE(dmat,fm=fit.rd,cluster=glmdat$id)
      RDfit <- coeftest(fit.rd, vcovCL.rd)
    }

    modelfit<-washb_glmFormat(glmModel=fit, glmModelRD=fit.rd, rfit=rfit, RDfit=RDfit, dmat=dmat, rowdropped=rowdropped, contrast=contrast, pair=pair, vcovCL=vcovCL, vcovCL.rd=vcovCL.rd, family=family, V=V, Subgroups=Subgroups, print=print)
    return(modelfit)
  } else{
      if(family[1]=="gaussian"){
        if(!is.null(V)){
          colnames(dmat)[which(colnames(dmat)==V)]<-"V"
          Subgroups<-levels(dmat$tr:dmat$V)
          if( class(dmat$V)!="factor") warning('V is not a factor variable within the W covariate data frame. An interaction term will be added to the model but not linear combination of coefficients will be calculated.')
          suppressWarnings(fit <- glm(Y~tr*V+. ,family=family,data=dmat))
        }else{
          suppressWarnings(fit <- glm(Y~.,family=family,data=dmat))
        }

        vcovCL <- sandwichSE(dmat,fm=fit,cluster=glmdat$id)
        rfit <- coeftest(fit, vcovCL)

        modelfit<-washb_glmFormat(glmModel=fit, rfit=rfit, dmat=dmat, rowdropped=rowdropped, contrast=contrast, pair=pair, vcovCL=vcovCL, family=family, V=V, Subgroups=Subgroups, print=print)
        return(modelfit)

      }else{
        if(family[1]=="neg.binom"){
          require(MASS)
          if (!requireNamespace("MASS", quietly = TRUE)) {
            stop("MASS needed for this function to work. Please install it.",
                 call. = FALSE)
            }

      if(!is.null(V)){
        colnames(dmat)[which(colnames(dmat)==V)]<-"V"
        Subgroups<-levels(dmat$tr:dmat$V)
        if( class(dmat$V)!="factor") warning('V is not a factor variable within the W covariate data frame. An interaction term will be added to the model but not linear combination of coefficients will be calculated.')
        suppressWarnings(fit <- glm.nb(Y~tr*V+. ,data=dmat))
        vcovCL <- sandwichSE(dmat,fm=fit,cluster=glmdat$id)
        rfit <- coeftest(fit, vcovCL)

        #fit OLS risk difference model
        fit.rd<-lm(Y~tr*V+.,data=dmat)
        vcovCL.rd <- sandwichSE(dmat,fm=fit.rd,cluster=glmdat$id)
        RDfit <- coeftest(fit.rd, vcovCL.rd)
      }else{
        suppressWarnings(fit<- glm.nb(Y ~., data = dmat))
        vcovCL <- sandwichSE(dmat,fm=fit,cluster=glmdat$id)
        rfit <- coeftest(fit, vcovCL)

        #fit OLS risk difference model
        fit.rd<-lm(Y~.,data=dmat)
        vcovCL.rd <- sandwichSE(dmat,fm=fit.rd,cluster=glmdat$id)
        RDfit <- coeftest(fit.rd, vcovCL.rd)
      }

      modelfit<-washb_glmFormat(glmModel=fit, glmModelRD=fit.rd,rfit=rfit, RDfit=RDfit, dmat=dmat, rowdropped=rowdropped, contrast=contrast, pair=pair, vcovCL=vcovCL, vcovCL.rd=vcovCL.rd, family=family, V=V, Subgroups=Subgroups, print=print)

      if(print==TRUE)cat("\n-----------------------------------------\nAssess whether conditional mean is equal to conditional variance:\n-----------------------------------------\n")

      if(!is.null(V)){
        pois <- glm(Y ~ tr*V+., family = "poisson", data = dmat)
      }else{
        pois <- glm(Y ~ ., family = "poisson", data = dmat)
      }
      X2 <- 2 * (logLik(fit) - logLik(pois))
      Pois_LRtest<-pchisq(X2, df = 1, lower.tail=FALSE)
      if(print==TRUE){
        cat("\nLog-likelihood ratio test P-value:\n")
        cat("\nIf <0.05, negative binomial model is more appropriate than a Poisson model.\n\n")
        print(Pois_LRtest)
      }
      modelfit<-c(modelfit, Pois_LRtest)
      return(modelfit)
      }else{
        stop('Error in family specified. Must choose Gaussian, Poisson, Binomial, Binomial(link-log), or neg.binom.')
      }
    }
  }
}

