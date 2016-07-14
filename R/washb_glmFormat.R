
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
#' @param vcovCL.rd sandwichSE function output for OLS RD model
#' @param family GLM model family (gaussian, binomial, poisson, or negative binomial). Use "neg.binom" for Negative binomial.
#' @param V Optional vector of variable names for subgroup analyses, which are interacted with 'tr'.
#' @param Subgroups Names of subgroups created by the interaction between treatment and V factor.
#'
#' @return Returns a list of the risk ratios or risk differences, the variance-covariance matrix, and a vector indexing the rows of observations
#'         used to fit the glm model
#' @export
#'
#' @examples
#'




washb_glmFormat <- function(rfit, RDfit=NULL, dmat, rowdropped, pair, vcovCL, vcovCL.rd=NULL, family="gaussian", V=NULL, Subgroups=NULL) {

  #Create linear comparisons by subgroup if V is specified
  if(!is.null(V)){
    #sort rfit output to put interactions before pair factors
    rfit<-rfit[c(1:(length(levels(dmat$V))+1),(nrow(rfit)+2-length(unique(dmat$V))):(nrow(rfit)),(length(levels(dmat$V))+2):(nrow(rfit)-length(unique(pair))+2-length(unique(dmat$V))),(nrow(rfit)-length(unique(pair))+3-length(unique(dmat$V))):(nrow(rfit)+1-length(unique(dmat$V)))),]
    RDfit<-RDfit[c(1:(length(levels(dmat$V))+1),(nrow(rfit)+2-length(unique(dmat$V))):(nrow(rfit)),(length(levels(dmat$V))+2):(nrow(rfit)-length(unique(pair))+2-length(unique(dmat$V))),(nrow(rfit)-length(unique(pair))+3-length(unique(dmat$V))):(nrow(rfit)+1-length(unique(dmat$V)))),]

    lincom<-matrix(0,nrow=length(levels(dmat$V)),ncol=6)
    lincomRD<-matrix(0,nrow=length(levels(dmat$V)),ncol=6)
    lincom_index<-matrix(0,nrow=length(levels(dmat$V)),ncol=nrow(rfit))

    for(i in 1:length(levels(dmat$V))){
        temp<-rep(0, length(Subgroups))
        temp[2]=1
        if(i!=1){
          temp[i+length(levels(dmat$V))]<- temp[i+1]<-1
          }

        lincom_index[i,1:(length(Subgroups))]<-temp


      lincom[i,] <- suppressWarnings(washb_lincom(lincom_index[i,],rfit,vcovCL,flag=1))
      lincomRD[i,] <- suppressWarnings(washb_lincom(lincom_index[i,],RDfit,vcovCL.rd,flag=1))
    }

    lincom<-data.frame(levels(dmat$V),round(lincom,6))
    lincomRD<-data.frame(levels(dmat$V),lincomRD)
    colnames(lincomRD) <- colnames(lincom) <- c("Tr vs. C by Subgroup","est","se.est","est.lb","est.ub","Z","P")
  }


  #Create formatted dataframe with risk ratios and risk differences and corresponding 95% CI.
    if(family[1]=="binomial"|family[1]=="poisson"|family[1]=="neg.binom"){
    expcoef<-round(exp(rfit[,1]),4)
    RR<-data.frame(round(exp(rfit[,1]),4), round(exp(rfit[,1]-1.96*rfit[,2]),4),round(exp(rfit[,1]+1.96*rfit[,2]),4))
    RD<-data.frame(round((RDfit[,1]),4), round((RDfit[,1]-1.96*RDfit[,2]),4),round((RDfit[,1]+1.96*RDfit[,2]),4))
    colnames(RD)<-c("RD","2.5%","97.5%")
    }else{
      if(family[1]=="gaussian"){
      RR<-data.frame(round((rfit[,1]),4), round((rfit[,1]-1.96*rfit[,2]),4),round((rfit[,1]+1.96*rfit[,2]),4))
      }else{
      cat("\nError: argument \"family\" is not a valid option.\n")
    }}

    if(family[1]=="binomial") {
      if(family[2]=="log"){
        colnames(RR)<-c("PR","2.5%","97.5%")
      }else{
        colnames(RR)<-c("OR","2.5%","97.5%")
      }
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

  #Print formatted glm model output.
  if(!is.null(V)){
   cat("\n-----------------------------------------\n",paste("GLM Fit:",contrast[1],"vs.",contrast[2])," by Subgroup: /'",V,"/'\n-----------------------------------------\n")
   print(lincom)
  }else{
    cat("\n-----------------------------------------\n",paste("GLM Fit:",contrast[1],"vs.",contrast[2]),"\n-----------------------------------------\n")
    print(RR[2,])
  }


  if(family[1]!="gaussian"){
    cat("\n\n RD of treatment\n")
     if(!is.null(V)){
       print(lincomRD)
    }else{
      print(RD[2,])
    }
  }

  if(ncol(dmat)>3){
    if(family[1]=="gaussian"){cat("\n Coef of covariates\n")}
    if(family[1]!="gaussian"){cat("\n RR of covariates\n")}
    print(RR[2:(nrow(RR)-(length(unique(pair))+1)),])
  }

  cat("\n Type \"`modelname'$TR\" to return the treatment effect.")
  cat("\n Type \"`modelname'$fit\" to return full glm model estimates.")
  if(family[1]!="gaussian"){
    cat("\n Type \"`modelname'$RDfit\" to return the risk difference of the treatment (and all covariates, including block pairs).")
    }
  cat("\n Type \"`modelname'$vcv\" to return the variance-covariance matrix.")
  cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
  if(!is.null(V)){
    cat("\n Type \"`modelname'$lincom\" to return subgroup-specific conditional relative risk estimates if a subgroup V is specified")
    if(family[1]!="gaussian"){
    cat("\n Type \"`modelname'$lincomRD\" to return subgroup-specific conditional risk difference estimates if a subgroup V is specified")
        }}

  #Create matriz holding RR, 95% CI and log-linear fit
  if (family[1]=="gaussian"){
    fit<-cbind(RR,(rfit[,2:4]))
    TR<-fit[2,]
  }else{
    fit<-cbind(RR,(rfit))
    TR<-fit[2,]
    RDfit<-cbind(RD,(RDfit))
  }


  if(family[1]=="gaussian"){
  modelfit=list(TR=TR, fit=fit, vcv=vcovCL, rowdropped=rowdropped, lincom=lincom)
  }else{
    modelfit=list(TR=TR, fit=fit, RDfit=RDfit, vcv=vcovCL, vcvRD=vcovCL.rd, rowdropped=rowdropped, lincom=lincom, lincomRD=lincomRD)
  }

  return(modelfit)
}
