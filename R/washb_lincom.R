#' Estimate linear combinations of GLM regression coefficients.
#'
#' Function to get Estimates, SEs, CIs, and P-values for Pr>|Z| from a linear combination of regression coefficients.
#'
#' @param lc : Index vector of coefficients from glm modellinear combination of coefficients
#' @param fit : Model object returned from coeftest (class=coeftest) command within the washb_glm function. Accessed with $fit from washb_glm output.
#' @param vcv : variance-covariance matrix of coefficients. Accessed with $vcv from washb_glm output.
#' @param measure measure of effect. RR = risk ratio, RD = risk difference
#' @param flag Internal argument used to flag and suppress printing if the washb_lincom function is called within another function.
#'
#' @return Returns a list of the risk ratios or risk differences, the variance-covariance matrix, and a vector indexing the rows of observations
#'         used to fit the glm model
#' @export
#'
#' @keywords internal
#'
#' @examples
#' library(washb)
#' data(washb_anthro)
#' data(washb_bd_enrol)
#'
#'  # drop svydate and month because they are superceded in the child level diarrhea data
#' washb_bd_enrol$svydate <- NULL
#' washb_bd_enrol$month <- NULL
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
#' #Estimate subgroup analysis glm with washb_glm
#' glm.C.N.byChildType <- washb_glm(Y=ad$diar7d,tr=ad$tr,pair=ad$block, W=W_tchild, V="tchild", id=ad$clusterid, contrast=c("Control","Nutrition"), family=binomial(link='log'), print=FALSE)
#'
#'
#' #The lincom function is called within washb_glm, so for standard subgroup analysis, there is no need to use the lincom()
#' #function separately. But it can be used on its own to create a custom combination of regression coefficients
#'
#'
#' #Examine the treatment effect across subgroups with `objectname'$lincom
#' glm.C.N.byChildType$lincom
#'
#' #Below is an example of using the `lincom()` function to externally replicate the target child/sibling subgroup analysis of the diarrheal disease outcome. First, recall the glm model contrasting diarrheal disease prevalence ratio between target child and sibling fitted to diarrheal disease:
#' #Use the lincom function to verify the subgroup analysis calculated within the `washb_glm()` function. `washb_lincom()` takes as argements the fit and variance-covariance matrix returned by the glm function, and an index vector `lc`.
#' #`lc`= vector indexing coefficients to be linearly combined. If a single coefficient is chosen, it will return the same coefficient and confidence interval as the glm coefficient. Example:
#' #Create lc vector of 0's equal in length to the number of coefficients from the glm model.
#' lc=rep(0,nrow(glm.C.N.byChildType$fit))
#' #Examine model coefficients (minus the pair-matched block estimates) to determine the position of coefficients to combine.
#' glm.C.N.byChildType$fit[1:6,]
#' #Replace the second and fourth position in the vector with 1 (the positions of the treatment coefficient and interaction term in the model)
#' lc[c(2,4)]<-1
#'
#' #Run the lincom function and compare output to the $lincom output from the GLM model.
#' washb_lincom(lc=lc,fit=glm.C.N.byChildType$fit,vcv=glm.C.N.byChildType$vcv, measure="RR")
#'
#' # The `lincom()` function can also be used to calculate risk difference
#' #Calculate risk difference using the risk difference output from the washb_glm function:
#' washb_lincom(lc=lc,fit=glm.C.N.byChildType$RDfit,vcv=glm.C.N.byChildType$vcvRD, measure="RD")
#' #Compare to RD by subgroup from the washb_glm function:
#' glm.C.N.byChildType$lincomRD



washb_lincom <- function(lc=NULL,fit,vcv, measure="RR", flag=NULL) {
    x<-fit

    if(!is.null(lc)){
      coef<-paste(rownames(x)[which(lc!=0)], collapse=" + ")
    }else{
      stop("Specify lc")
      }

  if(measure=="RD"){
     est <- (t(lc)%*%x[,1])
     se  <- sqrt( t(lc)%*%vcv%*%lc )
     Z   <- (est)/se
     P   <- 2*pnorm(-abs(Z))
     lb <- est-1.96*se
     ub <- est+1.96*se
     res <- matrix(c(est,se,lb,ub,Z,P),nrow=1)
     res[1:6]<-round(res[1:6],4)
     colnames(res) <- c("est","se.est","est.lb","est.ub","Z","P")
  }else{
    est <- exp(t(lc)%*%x[,4])
    se  <- sqrt( t(lc)%*%vcv%*%lc )
    Z   <- log(est)/se
    P   <- 2*pnorm(-abs(Z))
    lb <- exp(log(est)-1.96*se)
    ub <- exp(log(est)+1.96*se)
    res <- matrix(c(est,se,lb,ub,Z,P),nrow=1)
    res[1:6]<-round(res[1:6],4)
    colnames(res) <- c("RR","se.RR","RR.lb","RR.ub","Z","P")
  }


  if(is.null(flag)){
  cat("\nLinear combination of coefficients:\n")
  print(coef)
  }
  return(res)
}
