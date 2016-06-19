
#' washb_glmFormat
#'
#'Internal package function used to format the output of glm objects.
#'
#' @param fit glm fit object to be formatted
#' @param rfit output from coeftest with fit and sandwich SE
#' @param dmat dataframe used within the washb_glm function to fit data
#' @param pair Pair-matched randomization ID variable (in WASH Benefits: block)
#' @param vcovCL sandwichSE function output
#' @param family GLM model family (gaussian, binomial, poisson, or negative binomial). Use "neg.binom" for Negative binomial.
#'
#' @return Returns a list of the risk ratios or risk differences, the variance-covariance matrix, and a vector indexing the rows of observations
#'         used to fit the glm model
#' @export
#'
#' @examples
#'




washb_glmFormat <- function(fit, rfit, dmat, pair, vcovCL, family="gaussian") {

    #Get complete cases that will be used in the glm fit
    rowdropped<-rep(1,nrow(dmat))
    rowdropped[which(complete.cases(dmat))]<-0

    if(family=="binomial"|family=="poisson"|family=="neg.binom"){
    expcoef<-round(exp(rfit[,1]),4)
    RR<-data.frame(expcoef, round(exp(confint.default(fit,level=0.95)),4))
    }else{
      RR<-data.frame(round(rfit[,1],4), round(confint.default(fit,level=0.95),4))
    }
    #out<-out[2:(length(X)-(length(unique(pair))-1)),]
    colnames(RR)<-c("RR","2.5%","97.5%")

    #print(round(rfit[2,],5))
    print(RR[2,])

    cat("\n RR of covariates\n")

    print(RR[3:(length(expcoef)-(length(unique(pair))-1)),])

    cat("\n Type \"fit$fit\" to return full glm output.")
    cat("\n Type \"fit$RR\" to return the relative risk of the full model, including pair-matched blocks.")
    cat("\n Type \"fit$cov\" to return the variance-covariance matrix.")
    cat("\n Type \"fit$rowdropped\" to return the vector list of observations included in the model fit")

    fit=list(RR=RR, rfit=rfit, cov=vcovCL, rowdropped=rowdropped)
    return(fit)
}
