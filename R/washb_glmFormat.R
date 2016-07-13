
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
#' @param V Optional vector of variable names for subgroup analyses, which are interacted with 'tr'.
#' @param Subgroups Names of subgroups created by the interaction between treatment and V factor.
#'
#' @return Returns a list of the risk ratios or risk differences, the variance-covariance matrix, and a vector indexing the rows of observations
#'         used to fit the glm model
#' @export
#'
#' @examples
#'




washb_glmFormat <- function(rfit, RDfit=NULL, dmat, rowdropped, pair, vcovCL, family="gaussian", V=NULL, Subgroups=NULL) {

  #format interactions and call lincom function if V is specified
  if(!is.null(V)){
  #sort rfit output to put interactions before pair factors
  rfit<-rfit[c(1:(length(levels(dmat$V))+1),(nrow(rfit)+2-length(unique(dmat$V))):(nrow(rfit)),(length(levels(dmat$V))+2):(nrow(rfit)-length(unique(pair))+2-length(unique(dmat$V))),(nrow(rfit)-length(unique(pair))+3-length(unique(dmat$V))):(nrow(rfit)+1-length(unique(dmat$V)))),]

  lincom<-matrix(0,nrow=length(Subgroups),ncol=6)
  lincom_index<-matrix(0,nrow=length(Subgroups),ncol=nrow(rfit))

  for(i in 2:length(Subgroups)){
    #assign(paste(Subgroups[i], i, sep = ""), i)
    #assign(paste("lincom_ind", i, sep = ""), rep(0,(nrow(rfit))))
   temp<-rep(0, length(Subgroups))
   if(!is.na(charmatch(levels(dmat$tr)[2], Subgroups[i]))){temp[1]=1}

   for(j in 2:length(levels(dmat$V))){
     if((agrepl(levels(dmat$V)[j], Subgroups[i])==T)){temp[j]=1}
   }

   for(k in 2:length(levels(dmat$V))){
     if((agrepl(levels(dmat$V)[k], Subgroups[i])==T) & !is.na(charmatch(levels(dmat$tr)[2], Subgroups[i]))){temp[k+length(levels(dmat$V))]=1}
     }

   lincom_index[i,1:(length(Subgroups))]<-temp
  }


  #lincom[i,] <- suppressWarnings(washb_lincom(ctrlV0,rfit,vcovCL,flag=1))
  }

  library(gdata)
  startsWith(Subgroups,"Control")
  charmatch(contrast[1], Subgroups[1]) # returns 2
  "Control" %in% Subgroups

  levels(dmat$V)[1] %in% Subgroups

  trV1<-ctrlV1<-trV0<-ctrlV0<-rep(0,(nrow(rfit)))
  trV1[2:4]<-c(1,1,1)
  trV0[2:4]<-c(1,0,0)
  ctrlV1[2:4]<-c(0,1,0)
  ctrlV0[2:4]<-c(0,0,0)

  trV2[2:4]<-c(1,0,1,0,1)
  trV1[2:4]<-c(1,1,0,1,0)
  trV0[2:4]<-c(1,0,0,0,0)
  ctrlV2[2:4]<-c(0,0,1,0,0)
  ctrlV1[2:4]<-c(0,1,0,0,0)
  ctrlV0[2:4]<-c(0,0,0,0,0)


#To do: investigate if there is a way to keep the printed output when externally calling the lincom function,
#but suppress it here, so that the formatted output isn't messed up.
  lincom[1,] <- suppressWarnings(washb_lincom(ctrlV0,rfit,vcovCL,flag=1))
  lincom[2,] <- suppressWarnings(washb_lincom(trV0,rfit,vcovCL,flag=1))
  lincom[3,] <- suppressWarnings(washb_lincom(ctrlV1,rfit,vcovCL,flag=1))
  lincom[4,] <- suppressWarnings(washb_lincom(trV1,rfit,vcovCL,flag=1))
  lincom[,]<-round(lincom[,],8)
  lincom<-data.frame(Subgroups,lincom)
  lincom[,1]<-as.character(lincom[,1])
  lincom[1,1]<-paste(as.character(lincom[1,1]),"(ref)")
  colnames(lincom) <- c("Subgroups","est","se.est","est.lb","est.ub","Z","P")

  }
  #exp(rfit[1,1]+rfit[2,1]+rfit[3,1])
  #exp(rfit[2,1]+rfit[3,1]+rfit[4,1])
  #testRD<-matrix(0,nrow=4,ncol=6)
  #testRD[1,]<- suppressWarnings(lincom(ctrlV0,RDfit,vcovCL.rd,  measure="RD"))
  #testRD[2,]<- suppressWarnings(lincom(trV0,RDfit,vcovCL.rd,  measure="RD"))
  #testRD[3,]<- suppressWarnings(lincom(ctrlV1,RDfit,vcovCL.rd,  measure="RD"))
  #testRD[4,]<- suppressWarnings(lincom(trV1,RDfit,vcovCL.rd,  measure="RD"))

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

  if(ncol(dmat)>3){
    if(family[1]=="gaussian"){cat("\n Coef of covariates\n")}
    if(family[1]!="gaussian"){cat("\n RR of covariates\n")}
    print(RR[3:(nrow(RR)-(length(unique(pair))+1)),])
  }

  if(family[1]!="gaussian"){
    cat("\n\n RD of treatment\n")
    print(RD[2,])
  }

  cat("\n Type \"`modelname'$fit\" to return full glm output.")
  if(family[1]=="gaussian"){
    cat("\n Type \"`modelname'$coef\" to return the coefficients and 95% CI's  of the full model, including pair-matched blocks.")
    }else{
    cat("\n Type \"`modelname'$RR\" to return the relative risks and 95% CI's of the full model, including pair-matched blocks.")
    cat("\n Type \"`modelname'$RD\" to return the risk difference of the treatment (and all covariates, including block pairs).")
    }
  cat("\n Type \"`modelname'$vcv\" to return the variance-covariance matrix.")
  cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
  if(!is.null(V)){cat("\n Type \"`modelname'$lincom\" to return getting subgroup-specific conditional estimates if a subgroup V is specified")}

  #    if(family[1]=="gaussian"){
  #  if(ncol(dmat)>3){
  #    cat("\n Coef of covariates\n")
  #    print(RR[3:(nrow(RR)-(length(unique(pair))+1)),])
  #  }
  #    cat("\n Type \"`modelname'$fit\" to return raw glm output, without coefficients transformed into relative risks, and \nwith standard errors and p-values..")
  #    cat("\n Type \"`modelname'$coef\" to return the coefficients and 95% CI's  of the full model, including pair-matched blocks.")
  #    cat("\n Type \"`modelname'$vcv\" to return the variance-covariance matrix.")
  #    cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
  #    cat("\n Type \"`modelname'$lincom\" to return getting subgroup-specific conditional estimates if a subgroup V is specified")
  #  }else{
  #    if(ncol(dmat)>3){
  #      cat("\n RR of covariates\n")
  #      print(RR[3:(nrow(RR)-(length(unique(pair))+1)),])
  #    }
  #      cat("\n\n RD of treatment\n")
  #      print(RD[2,])

   #     cat("\n Type \"`modelname'$fit\" to return full glm output.")
    #    cat("\n Type \"`modelname'$RR\" to return the relative risks and 95% CI's of the full model, including pair-matched blocks.")
     #   cat("\n Type \"`modelname'$RD\" to return the risk difference of the treatment (and all covariates, including block pairs).")
      #  cat("\n Type \"`modelname'$vcv\" to return the variance-covariance matrix.")
      #  cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
      #  cat("\n Type \"`modelname'$lincom\" to return getting subgroup-specific conditional estimates if a subgroup V is specified")
      #}


  if(family[1]=="gaussian"){
  modelfit=list(coef=RR, fit=rfit, vcv=vcovCL, rowdropped=rowdropped, lincom=lincom)
  }else{
    modelfit=list(RR=RR, RD=RD, fit=rfit, RDfit=RDfit, vcv=vcovCL, rowdropped=rowdropped, lincom=lincom)
  }

  return(modelfit)
}
