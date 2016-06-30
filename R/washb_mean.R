
#' Means estimated with robust standard errors for the WASH Benefits trials
#'
#' Calculate means for a variable along with robust sandwich SEs and 95\% confidence intervals that account for clustering within id
#'
#' @param Y Outcome variable
#' @param id ID variable for independent units (in WASH Benefits: cluster ID)
#' @param print Logical. If \code{print=TRUE} (default) the function will print the results.
#'
#' @return Returns a 1x6 matrix that includes the number of observations, outcome mean, standard deviation, robust SE for the mean, lower 95\% CI, upper 95\% CI
#' @details This function is most useful for calculating variable means and confidence intervals -- for example, calculating average compliance (uptake) within a given intervention arm, or calculating the average LAZ by arm or measurement round. In the WASH Benefits trials, the independent unit is typically the cluster, so the \code{id} argument should identify the cluster ID.  If you wish to actually compare means between groups using a difference, prevalence ratio, or incidence ratio (depending on the outcome), use \code{\link[washb]{washb_glm}}, washb_ttest(TBA for continuous outcomes), or washb_mh (TBA for binary outcomes).
#' @return matrix containing the following columns: N (number of observations used to calculate mean), Mean, SD, Robust SE (sandwich estimator), and the lower and upper 95% Confidence interval bounds.
#' @export
#'
#' @examples
#'
#'  #Example using the washb_mean function on child LAZ score
#'
#'  #Load in Bandladesh anthropometry data and enrollment data
#'  data(washb_bd_anthro)
#'  data(washb_bd_enrol)
#'
#'  # drop svydate and month because they are superceded in the child level diarrhea data
#'  washb_bd_enrol$svydate <- NULL
#'  washb_bd_enrol$month <- NULL
#'  ad <- merge(washb_bd_enrol,washb_bd_anthro,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)
#'  ad <- subset(ad,svy==2)
#'  ad <- subset(ad,tchild=="Target child")
#'
#   # Drop children with extreme LAZ values
#'  ad <- subset(ad,laz_x!=1)
#'
#'  #Make sure the treatment group variables are set as factors:
#'  ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
#'
#'  #Run washb_mean function on child LAZ score outcome:
#'  washb_mean(Y=ad$laz,id=ad$clusterid,print=FALSE)
#'
#'  #Run the function to calculate child LAZ by select intervention arms
#'
#'  #Subset data to only handwashing arm:
#'  H<-ad[which(ad$tr=="Handwashing"),]
#'
#'  #Run function:
#'  washb_mean(Y=H$laz,id=H$clusterid,print=FALSE)
#'
#'  #Subset data to only WSH arm:
#'  WSH<-ad[which(ad$tr=="WSH"),]
#'
#'  #Run function:
#'  washb_mean(Y=WSH$laz,id=WSH$clusterid,print=FALSE)





washb_mean <- function(Y,id,print=TRUE) {

  # subset data to relevant treatment arm and restrict to complete cases
  mudat <- data.frame(id=id,Y=Y)
  n.orig <- dim(mudat)[1]
  mudat <- mudat[complete.cases(mudat),]
  n.sub  <- dim(mudat)[1]
  if(n.orig>n.sub) cat("\n-----------------------------------------\nDropping",n.orig-n.sub,"observations\ndue to missing values in the outcome\n","Final sample size:",n.sub,"\n-----------------------------------------\n")

  # use a GLM intercept model to get mean with robust sandwich SE and 95% CI
  fit <- glm(Y~1,family=gaussian,data=mudat)
  vcovCL <- sandwichSE(mudat,fm=fit,cluster=mudat$id)
  rfit <- coeftest(fit, vcovCL)

  # summarize the mean and robust 95% CIs
  lb <- rfit[1,1]-1.96*rfit[1,2]
  ub <- rfit[1,1]+1.96*rfit[1,2]
  mean_ci <- matrix(c(n.sub, rfit[1,1], sd(mudat$Y), rfit[1,2], lb, ub),nrow=1,ncol=6)
  colnames(mean_ci) <- c("N", "Mean", "SD", "Robust SE", "Lower 95%CI", "Upper 95%CI")

  # print results
  if(print==TRUE) print(mean_ci)

  # return results
  return(mean_ci)

}
