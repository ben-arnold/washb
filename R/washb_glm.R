


#' Generalized linear model function for WASH Benefits study.
#' washb_glm
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable (ideally a factor), comparison group first
#' @param pair Pair-matched randomization ID variable (in WASH Benefits: block)
#' @param W Optional data frame that includes adjustment covariates (for adjusted estimates)
#' @param forcedW Optional vector of variable names to force as adjustment covariates (no screening)
#' @param V Optional vector of variable names for subgroup analyses, which are interacted with 'tr'.
#' @param id ID variable for independent units (cluster ID)
#' @param contrast Vector of length 2 that includes the groups to contrast, e.g., c("Control","Water")
#' @param family GLM model family (gaussian, binomial, poisson, and negative binomial). Use "binonial(link='log')" to return prevalence ratios instead of odds ratios when the outcome is binary.  Use "neg.binom" for a Negative binomial model.
#' @param pval The p-value threshold: any variables with a p-value from the lielihood ratio test below this threshold will be returned. Defaults to 0.2
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
#' #Need to flesh out glm use in the vignette and then add below:



washb_glm <- function(Y,tr,pair,W=NULL, forcedW=NULL, V=NULL, id,contrast,family=gaussian, pval=0.2) {
  # Y     : outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
  # tr    : binary treatment group variable, comparison group first
  # pair  : Pair-matched randomization ID variable (in WASH Benefits: block)
  # W     : (optional) data frame that includes adjustment covariates
  # id    : id variable for independent units (e.g., cluster)
  # contrast : vector of length 2 that includes the tr groups to contrast
  # family : glm family (gaussian,binomial,poisson, or "neg.binom" for negative binomial)
  require(sandwich)
  require(lmtest)
  require(MASS)
  options(scipen=999)

  #add subgroup to forcedW


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
  } else{
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
  if(n.orig>n.sub) cat("\n-----------------------------------------\nDropping",n.orig-n.sub,"observations due to missing values in 1 or more variables\n","Final sample size:",n.sub,"\n-----------------------------------------\n")



  #split W into screened and forced adjustment covariates
  if(!is.null(W)){
    if(!is.null(V)){
      forcedW=c(forcedW,V)
    }
    if(!is.null(forcedW)){
      screenW<-subset(glmdat, select=colnames(W))
      toexclude <- names(screenW) %in% forcedW
      screenW=screenW[!toexclude]
      cat("\n-----------------------------------------\nInclude the following adjustment covariates:\n-----------------------------------------\n")
      print(forcedW, sep="\n")
    }else{
      screenW<-subset(glmdat, select=colnames(W))
    }
  }else{
    screenW<-NULL
  }

  if(!is.null(screenW)){
    # pre-screen the covariates
    # see Wprescreen() in the base functions
    cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    suppressWarnings(Wscreen <- washb_prescreen(Y=glmdat$Y,Ws=screenW,family=family, pval=pval))

    if(!is.null(forcedW)){
      if(!is.null(Wscreen)){
        dmat <- subset(glmdat,select=c("Y","tr",V,Wscreen,forcedW,"pair"))
        }else{
        dmat <- subset(glmdat,select=c("Y","tr",V,forcedW,"pair"))
        }
    } else {
      if(!is.null(Wscreen)){
        dmat <- subset(glmdat,select=c("Y","tr",V,Wscreen,"pair"))
      }else{
        dmat <- subset(glmdat,select=c("Y","tr",V,"pair"))
      }
    }
  } else {
    dmat <- subset(glmdat,select=c("Y","tr",V,"pair"))
  }

  if(family[1]=="binomial"|family[1]=="poisson"){

    if(!is.null(V)){
      Subgroups<-levels(dmat$tr:dmat$V)
      colnames(dmat)[which(colnames(dmat)==V)]<-"V"
      suppressWarnings(fit <- glm(Y~tr*V+. ,family=family,data=dmat))
      #fit<-update(fit, ~.+tr*V)
      vcovCL <- sandwichSE(dmat,fm=fit,cluster=glmdat$id)
      rfit <- coeftest(fit, vcovCL)
    }else{
      suppressWarnings(fit <- glm(Y~.,family=family,data=dmat))
      vcovCL <- sandwichSE(dmat,fm=fit,cluster=glmdat$id)
      rfit <- coeftest(fit, vcovCL)
    }

    #fit new model here with identity link after declaring new family[2]=(link=identity)
      #Assign non-zero start values
      #start<-rep(.5,length(fit$coefficients))

    family.rd<-family
    family.rd$link<-"identity"
    #Note: commented code below has same fit as log-link. Why? Debug or use alternate code
    #fit.rd<-glm(Y~.,family=family.rd,data=dmat)
    #temp avoid RD error by fitting OLS model:
    fit.rd<-lm(Y~.,data=dmat)
    #if(family[1]=="binomial"){fit.rd<-glm(Y~.,family=binomial(link='identity'), start=start,data=dmat)}
    #if(family[1]=="poisson"){fit.rd<-glm(Y~.,family=poisson(link="identity"),data=dmat)}
    vcovCL.rd <- sandwichSE(dmat,fm=fit.rd,cluster=glmdat$id)
    RDfit <- coeftest(fit.rd, vcovCL.rd)

    cat("\n-----------------------------------------\n",paste("GLM Fit:",contrast[1],"vs.",contrast[2]),"\n-----------------------------------------\n")

    modelfit<-washb_glmFormat(rfit=rfit, RDfit=RDfit, dmat=dmat, rowdropped=rowdropped, pair=pair, vcovCL=vcovCL, family=family, V=V, Subgroups=Subgroups)
    return(modelfit)
  } else{
      if(family[1]=="gaussian"){
        suppressWarnings(fit <- glm(Y~.,family=family,data=dmat))
        vcovCL <- sandwichSE(dmat,fm=fit,cluster=glmdat$id)
        rfit <- coeftest(fit, vcovCL)

        coef<-round(exp(rfit[,1]),4)
        out<-data.frame(coef, round((confint.default(fit,level=0.95)),4))
        #out<-out[2:(length(X)-(length(unique(pair))-1)),]
        colnames(out)<-c("Coef.","2.5%","97.5%")

        cat("\n-----------------------------------------\n",paste("GLM Fit:",contrast[1],"vs.",contrast[2]),"\n-----------------------------------------\n")

        modelfit<-washb_glmFormat(rfit=rfit, dmat=dmat, rowdropped=rowdropped, pair=pair, vcovCL=vcovCL, family=family)
        return(modelfit)

      }else{
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("MASS needed for this function to work. Please install it.",
           call. = FALSE)
    }else{
      suppressWarnings(fit<- glm.nb(Y ~., data = dmat))
      vcovCL <- sandwichSE(dmat,fm=fit,cluster=glmdat$id)
      rfit <- coeftest(fit, vcovCL)

      cat("\n-----------------------------------------\n",paste("GLM Fit:",contrast[1],"vs.",contrast[2]),"\n-----------------------------------------\n")
      modelfit<-washb_glmFormat(rfit=rfit, dmat=dmat, rowdropped=rowdropped, pair=pair, vcovCL=vcovCL, family=family)

      cat("\n-----------------------------------------\nAssess whether conditional mean is equal to conditional variance:\n-----------------------------------------\n")

      pois <- glm(Y ~ ., family = "poisson", data = dmat)
      X2 <- 2 * (logLik(fit) - logLik(pois))
      cat("\nLog-likelihood ratio test P-value:\n")
      cat("\nIf <0.05, negative binomial model is more appropriate than a Poisson model.\n\n")
      print(pchisq(X2, df = 1, lower.tail=FALSE))

      return(modelfit)
      }
    }
  }
}

