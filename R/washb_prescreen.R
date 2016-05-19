
#' Title
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param Ws data frame that includes candidate adjustment covariates to screen
#' @param family GLM model family (gaussian, binomial, poisson). Negative binomial will be added in the near future.
#'
#' @return
#' @export
#'
#' @examples



washb_prescreen <- function(Y,Ws,family="gaussian") {
  # Y   : outcome variable of interest
  # Ws  : data frame of candidate covariates to screen
  # family : exponential model family (gaussian for continuous outcomes, binomial for binary outcomes)
  require(lmtest)
  dat <- data.frame(Ws,Y)
  dat <- dat[complete.cases(dat),]
  nW <- ncol(Ws)
  LRp <- rep(NA,nW)
  for(i in 1:nW) {
    dat$W <- dat[,i]
    fit1 <- glm(Y~W,data=dat,family=family)
    fit0 <- glm(Y~1,data=dat,family=family)
    LRp[i] <- lrtest(fit1,fit0)[2,5]
  }
  p20 <- ifelse(LRp<0.2,1,0)
  cat("\nLikelihood Ratio Test P-values:\n")
  print(cbind(names(Ws),paste("P =",sprintf("%1.3f",LRp))))
  cat("\n\nCovariates selected (P<0.20):\n")
  print(cbind(names(Ws)[p20==1],paste("P =",sprintf("%1.3f",LRp[p20==1]))))
  return(names(Ws)[p20==1])
}
