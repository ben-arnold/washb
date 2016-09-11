#' washb_mh
#'
#' Mantel-Haenszel Pooled estimates of the prevalence ratio (PR) or the prevalence
#' difference (PD) using randomization block as the stratification variable.
#'
#' The function calls the M-H estimator for two different arms of the study. It relies on
#' the rma.mh() function in the metafor package.
#'
#' Estimate the Mantel-Haenszel prevalence ratio note: strata with no outcomes (i.e., missing PR) are dropped.
#' This is consistent with a fixed-effects regression analysis, in which those strata would not contribute to the estimates.
#' The arguments Y,tr,strat, below need to be from the same dataset.
#'
#'
#' @usage
#' washb_mh(Y,tr,strat,contrast,measure="RR")
#'
#'
#' @param Y binary outcome variable (here: diar7d)
#' @param tr binary treatment group variable, comparison group first
#' @param strat stratification variable (here: block)
#' @param contrast vector of length 2 that includes the tr groups to contrast (control(reference arm) and then intervention)
#' @param measure measure of effect. RR = prev ratio, RD = prev difference
#'
#' @return
#' res: Matrix of RD, se.RD, ci.lb, ci.ub, Z, p-value.
#'
#' @export
#'
#' @examples
#' #Prescreen function applied to the Bangladesh diarrheal disease outcome.
#' #The function will test a matrix of covariates and return those related to child diarrheal disease with
#' #a <0.2 p-value from a likelihood ratio test.
#'
#' #Load  diarrhea data
#' library(washb)
#' data(washb_bd_diar)
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
#' ###Create vector of contrasts for each hypothesis to facilitate comparisons between arms.
#' #Hypothesis 1: Each intervention arm vs. Control
#' h1.contrasts <- list(
#'   c("Control","Water"),
#'   c("Control","Sanitation"),
#'   c("Control","Handwashing"),
#'   c("Control","WSH"),
#'   c("Control","Nutrition"),
#'   c("Control","Nutrition + WSH")
#' )
#'
#' #Apply washb_mh to the water vs. control arm contrast.
#' washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RR")
#'
#' #Return the risk difference instead of the risk ration:
#' washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RD")
#'
#'
#' #Use sapply command to efficiently apply the function to all the treatment arm contrasts
#'
#' diff.h1 <- t(sapply(h1.contrasts,washb_mh,Y=ad$diar7d,tr=ad$tr,strat=ad$block,measure="RR"))
#' rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
#' print(diff.h1)
#'


washb_mh <- function(Y,tr,strat,contrast,measure="RR") {
  require(metafor)
  mhdat <- data.frame(Y=Y[tr==contrast[1]|tr==contrast[2]],
                      tr=tr[tr==contrast[1]|tr==contrast[2]],
                      strat=strat[tr==contrast[1]|tr==contrast[2]])
  mhdat$tr <- factor(mhdat$tr,levels=contrast[2:1])


  if(contrast[1]=="Control"|contrast[2]=="Control"){
    activeOnly<-((subset(mhdat,tr=="Control")))
    nomissblock1<-(unique(activeOnly$strat))
    nomiss<-sort((nomissblock1))
    mhdat<-mhdat[which((mhdat$strat %in% nomiss)),]
  }


  mhtab <- table(mhdat$tr,mhdat$Y,mhdat$strat)
  mhtab <- mhtab[,c(2:1),] # re-order to be consistent w/ metafor table orientation


  # suppress warning about yi/vi values being NA -- we know there are sparse tables
  # and a straum with no cases in it is effectively dropped from any fixed-effect estimator
  # the warning thrown by metafor's rma.mh just makes the output unnecessarily noisy
  muffw <- function(w) if( any( grepl( "Some yi/vi values are NA", w) ) ) invokeRestart( "muffleWarning" )

  # MH-estimation
  mh.est <- withCallingHandlers( rma.mh(ai=mhtab[1,1,],bi=mhtab[1,2,],ci=mhtab[2,1,],di=mhtab[2,2,],measure=measure,drop00=TRUE,level=95), warning = muffw)

  # format and return the output
  # these two additional objects include log likelihood and AIC (currently not saved): mh.est$fit.stats[1,1],mh.est$fit.stats[3,1]
  if(measure=="RR") {
    res <- c(exp(mh.est$b),exp(mh.est$ci.lb),exp(mh.est$ci.ub),mh.est$b,mh.est$se,mh.est$zval,mh.est$pval)
    names(res) <- c("PR,","ci.lb","ci.ub","logPR","se.logPR","Z","p")
  } else{
    res <- c(mh.est$b,mh.est$se,mh.est$ci.lb,mh.est$ci.ub,mh.est$zval,mh.est$pval)
    names(res) <- c("RD","se.RD","ci.lb","ci.ub","Z","p")
  }
  return(res)
}

