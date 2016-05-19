
#' washb_mean
#'
#' Calculate means for a variable along with robust sandwich SEs and 95\% confidence intervals that account for clustering within id
#'
#' @param Y Outcome variable
#' @param id ID variable for independent units (in WASH Benefits: cluster ID)
#' @param print Logical. If equal to 'TRUE' it will print the results.
#'
#' @return Returns a 1x5 matrix that includes the number of observation, outcome mean, robust SE, lower 95\% CI, upper 95\% CI
#' @export
#'
#' @examples
washb_mean <- function(Y,id,print=FALSE) {

  # subset data to relevant treatment arm and restrict to complete cases
  mudat <- data.frame(id=id,Y=Y)
  n.orig <- dim(mudat)[1]
  mudat <- mudat[complete.cases(mudat),]
  n.sub  <- dim(mudat)[1]
  if(n.orig>n.sub) cat("\n-----------------------------------------\nDropping",n.orig-n.sub,"observations due to missing values in the outcome\n","Final sample size:",n.sub,"\n-----------------------------------------\n")

  # use a GLM intercept model to get mean with robust sandwich SE and 95% CI
  fit <- glm(Y~1,family=gaussian,data=mudat)
  vcovCL <- cl(mudat,fm=fit,cluster=mudat$id)
  rfit <- coeftest(fit, vcovCL)

  # summarize the mean and robust 95% CIs
  lb <- rfit[1,1]-1.96*rfit[1,2]
  ub <- rfit[1,1]+1.96*rfit[1,2]
  mean_ci <- matrix(c(n.sub,rfit[1,1],rfit[1,2],lb,ub),nrow=1,ncol=5)
  colnames(mean_ci) <- c("N","Mean","Robust SE","Lower 95%CI","Upper 95%CI")

  # print results
  if(print==TRUE) print(mean_ci)

  # return results
  return(mean_ci)

}
