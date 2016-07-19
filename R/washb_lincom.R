#' washb_lincom
#'
#' Function to get Estimates, SEs, CIs, and P-values for Pr>|Z| from a linear combination of regression coefficients.
#'
#' @param lc : Index vector of coefficients from glm modellinear combination of coefficients
#' @param varlist : Character vector of variables to include. Alternative to lc. If lc is specified, this arguement is ignored.
#' @param fit : Model object returned from coeftest (class=coeftest) command within the washb_glm function. Accessed with $fit from washb_glm output.
#' @param vcv : variance-covariance matrix of coefficients. Accessed with $vcv from washb_glm output.
#' @param measure measure of effect. RR = risk ratio, RD = risk difference
#' @param flag Internal argument used to flag and suppress printing if the washb_lincom function is called within another function.
#'
#' @return Returns a list of the risk ratios or risk differences, the variance-covariance matrix, and a vector indexing the rows of observations
#'         used to fit the glm model
#' @export
#'
#' @examples
#' library(washb)
#' data(washb_anthroClean)



washb_lincom <- function(lc=NULL,varlist=NULL,fit,vcv, measure="RR", flag=NULL) {
    x<-fit

    if(!is.null(lc)){
      coef<-paste(rownames(x)[which(lc!=0)], collapse=" + ")
    }else{
      if(!is.null(varlist)){
      tocombine <- which(rownames(x) %in% varlist)
      lc=rep(0,nrow(x))
      lc[tocombine]<-1
    }else{
      stop("Specify either lc or varlist.")
      }
    }

  if(measure=="RD"){
     est <- (t(lc)%*%x[,1])
     se  <- sqrt( t(lc)%*%vcv%*%lc )
     Z   <- (est)/se
     P   <- 2*pnorm(-abs(Z))
     lb <- est-1.96*se
     ub <- est+1.96*se
     res <- matrix(c(est,se,lb,ub,Z,P),nrow=1)
     res[1:6]<-round(res[1:6],4)
     colnames(res) <- c("est","se.est","est.lb","est.ub","Z","P")
  }else{
    est <- exp(t(lc)%*%x[,4])
    se  <- sqrt( t(lc)%*%vcv%*%lc )
    Z   <- log(est)/se
    P   <- 2*pnorm(-abs(Z))
    lb <- exp(log(est)-1.96*se)
    ub <- exp(log(est)+1.96*se)
    res <- matrix(c(est,se,lb,ub,Z,P),nrow=1)
    res[1:6]<-round(res[1:6],4)
    colnames(res) <- c("RR","se.RR","RR.lb","RR.ub","Z","P")
  }


  if(is.null(flag)){
  cat("\nLinear combination of coefficients:\n")
  print(coef)
  }
  return(res)
}
