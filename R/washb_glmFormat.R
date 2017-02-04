
#' An internal formatting function for results output in glm objects
#'
#'Internal package function used to format the output of glm objects.
#'
#' @param glmModel glm model fit passed through the function and added to the output list
#' @param rfit output from coeftest with fit and sandwich SE
#' @param dmat dataframe used within the washb_glm function to fit data
#' @param rowdropped dummy vector indexing rows of data dropped because a var contained missing data, to be read through the function and added to the formatted output.
#' @param contrast Vector of length 2 that includes the groups to contrast, e.g., c("Control","Water")
#' @param pair Pair-matched randomization ID variable (in WASH Benefits: block)
#' @param vcovCL sandwichSE function output
#' @param family GLM model family (gaussian, binomial, poisson, or negative binomial). Use "neg.binom" for Negative binomial.
#' @param V Optional vector of variable names for subgroup analyses, which are interacted with 'tr'.
#' @param Subgroups Names of subgroups created by the interaction between treatment and V factor.
#' @param print Logical for whether to print function output
#' @param verbose Logical for whether to print names and descriptions of returned list objects
#'
#' @return Returns a list of the risk ratios or risk differences, the variance-covariance matrix, and a vector indexing the rows of observations
#'         used to fit the glm model
#' @export
#'
#' @keywords internal
#' @examples
#'




washb_glmFormat <- function(glmModel=glmModel, rfit, dmat, rowdropped, contrast, pair, vcovCL, family=family, V=NULL, Subgroups=NULL, print=print, verbose=verbose) {



  #Create formatted dataframe with risk ratios and risk differences and corresponding 95% CI.
    if(family[1]=="binomial"|family[1]=="poisson"|family[1]=="neg.binom"){
    expcoef<-round(exp(rfit[,1]),8)
    RR<-data.frame(round(exp(rfit[,1]),8), round(exp(rfit[,1]-1.96*rfit[,2]),8),round(exp(rfit[,1]+1.96*rfit[,2]),8))
    }else{
      if(family[1]=="gaussian"){
      RR<-data.frame(round((rfit[,1]),8), round((rfit[,1]-1.96*rfit[,2]),8),round((rfit[,1]+1.96*rfit[,2]),8))
      }else{
      stop("\nError: argument \"family\" is not a valid option.\n")
      }}

    #Create data.frame headers
    if(family[1]=="binomial") {
      if(family[2]!="log"|length(family)==1){
        colnames(RR)<-c("OR","2.5%","97.5%")
      }else{
        colnames(RR)<-c("PR","2.5%","97.5%")
      }
    } else{
      if(family[1]=="poisson"|family[1]=="neg.binom") {
          if(family[2]!="log"|length(family)==1){
            colnames(RR)<-c("IRR","2.5%","97.5%")
          }else{
            colnames(RR)<-c("PR","2.5%","97.5%")
          }
      }else{
        colnames(RR)<-c("Coef.","2.5%","97.5%")
      }}



  #Create matrix holding RR, 95% CI and log-linear fit
  if (family[1]=="gaussian"){
    fit<-cbind(RR,(rfit[,2:4]))
    TR<-fit[2,]
  }else{
    fit<-cbind(RR,(rfit[,]))
    TR<-fit[2,]
  }


  #Create linear comparisons by subgroup if V is specified
  if(!is.null(V)){
    #sort rfit output to put interactions before pair factors
    if(class(dmat$V)!="factor"){
      fit<-fit[c(1:3,nrow(fit),4:(nrow(fit)-1)),]
    }

    if(class(dmat$V)=="factor"){

      lincom<-(matrix(0,nrow=length(levels(dmat$V)),ncol=6))
      lincom_index<-matrix(0,nrow=length(levels(dmat$V)),ncol=nrow(fit))

      for(i in 1:length(levels(dmat$V))){
        temp<-rep(0, nrow(fit))
        temp[2]=1
        if(i!=1){
          temp[nrow(fit)-length(levels(dmat$V))+i]<-1
        }

        lincom_index[i,]<-temp

        if(family[1]=="gaussian"){lincom[i,] <- suppressWarnings(washb_lincom(lc=lincom_index[i,],fit=fit,vcv=vcovCL, measure="RD",flag=1))}
        if(family[1]!="gaussian"){
          lincom[i,] <- suppressWarnings(washb_lincom(lc=lincom_index[i,],fit=fit,vcv=vcovCL, measure="RR",flag=1))
        }
      }

      lincom<-data.frame(levels(dmat$V),lincom)
      colnames(lincom) <- c("Tr vs. C by Subgroup","est","se.est","est.lb","est.ub","Z","P")
      }
    }



  if(print==TRUE){
    #Print formatted glm model output.
    if(!is.null(V)&class(dmat$V)=="factor"){
      cat("\n\n-----------------------------------------\n",paste("GLM Fit:",contrast[2],"vs.",contrast[1])," by Subgroup: \'",V,"\'\n-----------------------------------------\n")
      print(lincom)
    }else{
      cat("\n\n-----------------------------------------\n",paste("GLM Fit:",contrast[2],"vs.",contrast[1]),"\n-----------------------------------------\n")
      print(TR)
    }


    if(!is.null(V)){
      cat("\n\n-----------------------------------------\n Significance of effect modification variables \n-----------------------------------------\n")
        print(fit[(nrow(fit)-length(levels(dmat$V))+2):nrow(fit),c(colnames(RR)[1],"Pr(>|z|)")])
      }

    if(ncol(dmat)>3&is.null(V)|ncol(dmat)>4){
      if(family[1]=="gaussian"){cat("\n Coef of covariates\n")}
      if(family[1]!="gaussian"){cat("\n RR of covariates\n")}
      if(is.null(pair)){
        print(RR[2:(nrow(RR)-(length(unique(dmat$pair))-1)),])
      }else{
        print(RR[3:(nrow(RR)),])
      }
    }

    if(verbose==TRUE){

    cat("\n Type \"`modelname'$TR\" to return the treatment effect.")
    cat("\n Type \"`modelname'$fit\" to return full glm model estimates.")
    cat("\n Type \"`modelname'$vcv\" to return the variance-covariance matrix.")
    cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
    if(!is.null(V)){
      cat("\n Type \"`modelname'$lincom\" to return subgroup-specific conditional relative risk estimates if a subgroup V is specified")
    }
    cat("\n Type \"`modelname'$glmModel\" to return the glm model fit to be used with predict() to return model predictions of the outcome")
      }
    }


    if(!is.null(V)&class(dmat$V)=="factor"){
      modelfit=list(TR=TR, fit=fit, vcv=vcovCL, rowdropped=rowdropped, glmModel=glmModel, lincom=lincom)
    }else{
      modelfit=list(TR=TR, fit=fit, vcv=vcovCL, rowdropped=rowdropped, glmModel=glmModel)
      }

  return(modelfit)
}
