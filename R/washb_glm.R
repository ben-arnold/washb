



#' Title
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param tr Binary treatment group variable (ideally a factor), comparison group first
#' @param pair Pair-matched randomization ID variable (in WASH Benefits: block)
#' @param W Optional data frame that includes adjustment covariates (for adjusted estimates)
#' @param forcedW Optional vector of variable names to force as adjustment covariates (no screening)
#' @param id ID variable for independent units (cluster ID)
#' @param contrast Vector of length 2 that includes the groups to contrast, e.g., c("Control","Water")
#' @param family GLM model family (gaussian, binomial, poisson). Negative binomial will be added in the near future.
#'
#' @return
#' @export
#'
#' @examples
washb_glm <- function(Y,tr,pair,W=NULL, forcedW=NULL, id,contrast,family=gaussian) {
  # Y     : outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
  # tr    : binary treatment group variable, comparison group first
  # pair  : Pair-matched randomization ID variable (in WASH Benefits: block)
  # W     : (optional) data frame that includes adjustment covariates
  # id    : id variable for independent units (e.g., cluster)
  # contrast : vector of length 2 that includes the tr groups to contrast
  # family : glm family (gaussian,binomial,poisson)
  require(sandwich)
  require(lmtest)

  if(!is.null(W)){
    glmdat <- data.frame(
      id=id[tr==contrast[1]|tr==contrast[2]],
      Y=Y[tr==contrast[1]|tr==contrast[2]],
      tr=tr[tr==contrast[1]|tr==contrast[2]],
      pair=pair[tr==contrast[1]|tr==contrast[2]],
      W[tr==contrast[1]|tr==contrast[2],]
    )
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

  # restrict to complete cases
  n.orig <- dim(glmdat)[1]
  glmdat <- glmdat[complete.cases(glmdat),]
  n.sub  <- dim(glmdat)[1]
  if(n.orig>n.sub) cat("\n-----------------------------------------\nDropping",n.orig-n.sub,"observations due to missing values in 1 or more variables\n","Final sample size:",n.sub,"\n-----------------------------------------\n")

  #split W into screened and forced adjustment covariates
  if(!is.null(W)){
    if(!is.null(forcedW)){
      screenW<-subset(glmdat, select=colnames(W))
      toexclude <- names(screenW) %in% forcedW
      screenW=screenW[!toexclude]
    }else{
      screenW<-subset(glmdat, select=colnames(W))
    }
  }else{
    screenW<-NULL
  }
  #forceWdata<-subset(glmdat, select=forcedW)

  if(!is.null(screenW)){
    # pre-screen the covariates
    # see Wprescreen() in the base functions
    cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    Wscreen <- Wprescreen(Y=glmdat$Y,Ws=screenW,family=family)

    if(!is.null(forcedW)){
      dmat <- subset(glmdat,select=c("Y","tr",Wscreen,forcedW,"pair"))
    } else {
      dmat <- subset(glmdat,select=c("Y","tr",Wscreen,"pair"))
    }
  } else {
    dmat <- subset(glmdat,select=c("Y","tr","pair"))
  }
  fit <- glm(Y~.,family=family,data=dmat)
  vcovCL <- cl(dmat,fm=fit,cluster=glmdat$id)
  rfit <- coeftest(fit, vcovCL)
  cat("\n-----------------------------------------\n",paste("GLM Fit:",contrast[1],"vs.",contrast[2]),"\n-----------------------------------------\n")
  print(round(rfit[2,],5))
  return(rfit)
}

