
#' washb_prescreen
#'
#' @param Y Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#' @param Ws data frame that includes candidate adjustment covariates to screen
#' @param family GLM model family (gaussian, binomial, poisson, or negative binomial). Use "neg.binom" for Negative binomial.
#'
#' @return to be written
#' @export
#'
#' @examples
#'
#'  Stochastically generated dataset:
#'  n <- 250
#'  set.seed(12345)
#'  Cov <- matrix(rnorm(n*3), ncol=3)
#'  colnames(Cov) <- paste("W",1:3, sep="")
#'  Outcome <- rbinom(n,1, plogis(0.2*Cov[,1] + 0.1*Cov[,2] + 0.2*Cov[,3]^2 ))
#'
#'  #Call function:
#'  washb_prescreen(Y=Outcome,Ws=Covariates,family="binomial")
#'
#'  #Output:
#'  Likelihood Ratio Test P-values:
#'      [,1]
#'  [1,] "P = 0.948"
#'  [2,] "P = 0.770"
#'  [3,] "P = 0.958"
#'
#'
#'Covariates selected (P<0.20):
#'      [,1]
#'  [1,] "P = "
#'
#'
#'  #Continuous outcome:
#'  Outcome <- rnorm(n)
#'  washb_prescreen(Y=Outcome,Ws=Cov,family="gaussian")
#'
#'  Likelihood Ratio Test P-values:
#'      [,1]
#'  [1,] "P = 0.139"
#'  [2,] "P = 0.756"
#'  [3,] "P = 0.619"
#'
#'  Covariates selected (P<0.20):
#'      [,1]
#'  [1,] "P = 0.139"
#'



washb_prescreen <- function(Y,Ws,family="gaussian") {
  # Y   : outcome variable of interest
  # Ws  : data frame of candidate covariates to screen
  # family : exponential model family (gaussian for continuous outcomes, binomial for binary outcomes, poisson for counts, and neg.binom for negative binomial models)
  require(lmtest)
  require(MASS)
  dat <- data.frame(Ws,Y)
  dat <- dat[complete.cases(dat),]
  nW <- ncol(Ws)
  LRp <- rep(NA,nW)
  if(family!="neg.binom"){
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
  p20 <- ifelse(LRp<0.2,1,0)
  cat("\nLikelihood Ratio Test P-values:\n")
  print(cbind(names(Ws),paste("P =",sprintf("%1.3f",LRp))))
  cat("\n\nCovariates selected (P<0.20):\n")
  print(cbind(names(Ws)[p20==1],paste("P =",sprintf("%1.3f",LRp[p20==1]))))
  return(names(Ws)[p20==1])
}
