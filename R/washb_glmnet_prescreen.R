
#' Pre-screen covariates using a likelihood ratio test.
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param Ws data frame that includes candidate adjustment covariates to screen
#' @param family GLM model family (gaussian, binomial, poisson, or negative binomial). Use "neg.binom" for Negative binomial.
#' @param pval The p-value threshold: any variables with a p-value from the lielihood ratio test below this threshold will be returned. Defaults to 0.2
#' @param print Logical for whether to print function output, defaults to TRUE.
#'
#' @return
#'  Function returns the list of variable names with a likelihood ratio test p-value <0.2 (unless a custom p-value is specified).
#' @export
#'
#' @examples
#'
#' #LASSO Prescreen function applied to the Bangladesh diarrheal disease outcome.
#' #The function will test a matrix of covariates and return those related to child diarrheal disease (not dropped from LASSO model).
#'
#' #Load diarrhea data:
#' library(washb)
#' data(washb_bangladesh_enrol)
#' washb_bangladesh_enrol <- washb_bangladesh_enrol
#' data(washb_bangladesh_diar)
#' washb_bangladesh_diar <- washb_bangladesh_diar
#'
#'  # drop svydate and month because they are superceded in the child level diarrhea data
#' washb_bangladesh_enrol$svydate <- NULL
#' washb_bangladesh_enrol$month <- NULL
#'
#' # merge the baseline dataset to the follow-up dataset
#' ad <- merge(washb_bangladesh_enrol,washb_bangladesh_diar,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
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
#' ###Run the washb_glmnet_prescreen function
#' prescreened_varnames<-washb_glmnet_prescreen(Y=ad$diar7d,Ws,family="binomial")



washb_glmnet_prescreen <- function(Y,Ws,family="gaussian", print=TRUE) {
  # Y   : outcome variable of interest
  # Ws  : data frame of candidate covariates to screen
  # family : exponential model family (gaussian for continuous outcomes, binomial for binary outcomes, poisson for counts, and neg.binom for negative binomial models)
  require(stringr)
  require(glmnet)

  # ensure Ws are a data frame
  Ws <- as.data.frame(Ws)

  colnames(Ws) <- paste0(colnames(Ws),"_lev") #add a "lev" to the end of variables so when converted to indicators can see: "original variable_factor level" in the variable name

  dat <- data.frame(Y,Ws)
  dat <- dat[complete.cases(dat),]
  X <- model.matrix(~-1 + ., dat[,-1])


  if(family[[1]]!="neg.binom"){

    fitCV <- glmnet::cv.glmnet(x = X, y = dat[,1], lambda = NULL, type.measure = "deviance",
                               nfolds = 5, family = family, alpha = 1,
                               nlambda = 100)
    whichVariable <- (as.numeric(coef(fitCV$glmnet.fit, s = fitCV$lambda.min))[-1] !=0)

    selected_vars <- colnames(X)[whichVariable]
    #cat(selected_vars,"\n")
    #Right now, the variables are indicator variables... select the original variable names for regression analysis so all factor levels are included
    if(length(selected_vars)>0){
     try(selected_vars <- unique(stringr::str_split(selected_vars,"_lev", simplify = T)[,1]))
    }else{
      selected_vars <-NULL
    }


  }else{
      stop("Negative binomial not supported yet")
  }

  return(selected_vars)

}


