
#' washb_glmFormat
#'
#'Internal package function used to format the output of glm objects.
#'
#' @param rfit output from coeftest with fit and sandwich SE
#' @param RDfit glm fit object with the identity link used to estimate risk differences.
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




washb_glmFormat <- function(rfit, RDfit=RDfit, dmat, rowdropped, pair, vcovCL, family="gaussian") {


    if(family[1]=="binomial"|family[1]=="poisson"|family[1]=="neg.binom"){
    expcoef<-round(exp(rfit[,1]),4)
    RR<-data.frame(round(exp(rfit[,1]),4), round(exp(rfit[,1]-1.96*rfit[,2]),4),round(exp(rfit[,1]+1.96*rfit[,2]),4))
    RD<-data.frame(round((RDfit[,1]),4), round((RDfit[,1]-1.96*RDfit[,2]),4),round((RDfit[,1]+1.96*RDfit[,2]),4))
    colnames(RD)<-c("RD","2.5%","97.5%")
    }else{
      if(family[1]=="gaussian"){
      RR<-data.frame(round((rfit[,1]),4), round((rfit[,1]-1.96*rfit[,2]),4),round((rfit[,1]+1.96*rfit[,2]),4))
      }else{
      cat("\nError: arguement \"family\" is not a valid option.\n")
    }}

    if(family[1]=="binomial") {
      colnames(RR)<-c("PR","2.5%","97.5%")
    } else{
      if(family[1]=="poisson") {
        colnames(RR)<-c("CIR","2.5%","97.5%")
      }
      if(family[1]=="neg.binom") {
        colnames(RR)<-c("IRR","2.5%","97.5%")
      }
      else{
        colnames(RR)<-c("Coef.","2.5%","97.5%")
    }}


  print(RR[2,])
      if(family[1]=="gaussian"){
    if(ncol(dmat)>3){
      cat("\n Coef of covariates\n")
      print(RR[3:(nrow(RR)-(length(unique(pair))+1)),])
    }
      cat("\n Type \"`modelname'$fit\" to return raw glm output, without coefficients transformed into relative risks, and \nwith standard errors and p-values..")
      cat("\n Type \"`modelname'$coef\" to return the coefficients and 95% CI's  of the full model, including pair-matched blocks.")
      cat("\n Type \"`modelname'$cov\" to return the variance-covariance matrix.")
      cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
    }else{
      if(ncol(dmat)>3){
        cat("\n RR of covariates\n")
        print(RR[3:(nrow(RR)-(length(unique(pair))+1)),])
      }
        cat("\n\n RD of treatment\n")
        print(RD[2,])

        cat("\n Type \"`modelname'$fit\" to return full glm output.")
        cat("\n Type \"`modelname'$RR\" to return the relative risks and 95% CI's of the full model, including pair-matched blocks.")
        cat("\n Type \"`modelname'$RD\" to return the risk difference of the treatment (and all covariates, including block pairs).")
        cat("\n Type \"`modelname'$cov\" to return the variance-covariance matrix.")
        cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
      }


  if(family[1]=="gaussian"){
  modelfit=list(coef=RR, fit=rfit, cov=vcovCL, rowdropped=rowdropped)
  }else{
    modelfit=list(RR=RR, RD=RD, fit=rfit, RDfit=RDfit, cov=vcovCL, rowdropped=rowdropped)
  }

  return(modelfit)
}
