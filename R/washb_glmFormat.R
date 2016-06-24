
#' washb_glmFormat
#'
#'Internal package function used to format the output of glm objects.
#'
#' @param fit glm fit object to be formatted
#' @param rfit output from coeftest with fit and sandwich SE
#' @param dmat dataframe used within the washb_glm function to fit data
#' @param rowdropped dummy vector indexing rows of data dropped because a var contained missing data, to be read through the function and added to the formatted output.
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




washb_glmFormat <- function(fit, rfit, dmat, rowdropped, pair, vcovCL, family="gaussian") {


    if(family=="binomial"|family=="poisson"|family=="neg.binom"){
    expcoef<-round(exp(rfit[,1]),4)
    RR<-data.frame(expcoef, round(exp(confint.default(fit,level=0.95)),4))
    }else{
      RR<-data.frame(round(rfit[,1],4), round(confint.default(fit,level=0.95),4))
    }
    #out<-out[2:(length(X)-(length(unique(pair))-1)),]

    if(family=="binomial") {
      colnames(RR)<-c("PR","2.5%","97.5%")
    } else{
      if(family=="poisson") {
        colnames(RR)<-c("CIR","2.5%","97.5%")
      }
      if(family=="neg.binom") {
        colnames(RR)<-c("IRR","2.5%","97.5%")
      }
      else{
        colnames(RR)<-c("Coef.","2.5%","97.5%")
    }

      if(family!="gaussian"){
    print(RR[2,])
    cat("\n RR of covariates\n")

    print(RR[3:(nrow(RR)-(length(unique(pair))-1)),])

    cat("\n Type \"`modelname'$fit\" to return full glm output.")
    cat("\n Type \"`modelname'$coef\" to return the relative risk of the full model, including pair-matched blocks.")
    cat("\n Type \"`modelname'$cov\" to return the variance-covariance matrix.")
    cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
      }else{
        print(RR[2,])
        cat("\n Coef of covariates\n")

        print(RR[3:(nrow(RR)-(length(unique(pair))-1)),])

        cat("\n Type \"`modelname'$fit\" to return full glm output.")
        cat("\n Type \"`modelname'$coef\" to return the coefficients and 95% CI's of the full model, including pair-matched blocks.")
        cat("\n Type \"`modelname'$cov\" to return the variance-covariance matrix.")
        cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
      }
    modelfit=list(coef=RR, rfit=rfit, cov=vcovCL, rowdropped=rowdropped)
    return(modelfit)
  }
}
