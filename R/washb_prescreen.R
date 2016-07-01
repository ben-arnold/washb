
#' washb_prescreen
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param Ws data frame that includes candidate adjustment covariates to screen
#' @param family GLM model family (gaussian, binomial, poisson, or negative binomial). Use "neg.binom" for Negative binomial.
#' @param pval The p-value threshold: any variables with a p-value from the lielihood ratio test below this threshold will be returned. Defaults to 0.2
#'
#' @return
#'  Function returns the list of variable names with a likelihood ratio test p-value <0.2.
#' @export
#'
#' @examples
#'
#' #Prescreen function applied to the Bangladesh diarrheal disease outcome.
#' #The function will test a matrix of covariates and return those related to child diarrheal disease with
#' #a <0.2 p-value from a likelihood ratio test.
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
#' ###Subset to a new dataframe the variables to be screened:
#' Ws <- subset(ad,select=c("fracode","month","agedays","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))
#'
#' ###Run the washb_prescreen function
#' prescreened_varnames<-washb_prescreen(Y=ad$diar7d,Ws,family="binomial")
#'



washb_prescreen <- function(Y,Ws,family="gaussian", pval=0.2) {
  # Y   : outcome variable of interest
  # Ws  : data frame of candidate covariates to screen
  # family : exponential model family (gaussian for continuous outcomes, binomial for binary outcomes, poisson for counts, and neg.binom for negative binomial models)
  require(lmtest)
  require(MASS)

  #Check pvalue
  if(pval>0.99|pval<0){
    stop("P-value threshold not set between 0 and 1.")
  }

  dat <- data.frame(Ws,Y)
  dat <- dat[complete.cases(dat),]
  nW <- ncol(Ws)
  LRp <- rep(NA,nW)
  if(family[1]!="neg.binom"){
    for(i in 1:nW) {
      dat$W <- dat[,i]
      fit1 <- glm(Y~W,data=dat,family=family)
      fit0 <- glm(Y~1,data=dat,family=family)
      LRp[i] <- lrtest(fit1,fit0)[2,5]
    }
  }else{
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("Pkg needed for this function to work. Please install it.",
           call. = FALSE)
    }else{
      for(i in 1:nW) {
        dat$W <- dat[,i]
        fit1 <- glm.nb(Y~W,data=dat)
        fit0 <- glm.nb(Y~1,data=dat)
        LRp[i] <- lrtest(fit1,fit0)[2,5]
        }
      }
    }
  p20 <- ifelse(LRp<pval,1,0)
  cat("\nLikelihood Ratio Test P-values:\n")
  print(cbind(names(Ws),paste("P =",sprintf("%1.3f",LRp))))
  cat("\n\nCovariates selected (P<0.20):\n")
  print(cbind(names(Ws)[p20==1],paste("P =",sprintf("%1.3f",LRp[p20==1]))))
  return(names(Ws)[p20==1])
}
