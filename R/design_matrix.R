
#' Convert a vector, matrix or data frame with factors into a design matrix
#'
#' An internal, convenience function that automatically transforms a vector, matrix, or data.frame with factors into a design matrix with indicator variables and an ommitted category. This is handy for using the SuperLearner and tmle packages
#'
#' @param W A vector, matrix, or data.frame that includes numeric or factor variables.
#'
#' @return A design matrix version of \code{W} where factor variables have been converted into columns of indicator variables with the first level excluded.
#' @details The \code{design_matrix} function is used by \code{agecurveAb} and \code{tmleAb} as a data processing tool for functions that do not readily accommodate factor variables in a data.frame object. In particular, the \code{SuperLearner()} and \code{tmle()} functions typically have difficulty with factor variables and so this function transforms data before calling those functions.
#'
#' @keywords internal
#'
#' @export
#'
#'
design_matrix <- function(W) {
  # W : data frame of covariates that might include factors
  if(class(W)!="matrix" & class(W)!="data.frame"){

    W <- data.frame(W)
    if(is.null(ncol(W)) | ncol(W)==0) {
      stop("Something is wrong with W.\nTo be safe, please try specifying it as class=data.frame.")
    }

  }
  ncolW <- ncol(W)
  flist <- numeric()
  for(i in 1:ncolW) {
    if(class(W[,i])!="factor"){
      next
    } else {
      flist <- c(flist,i)
      # strip out extra levels
      W[,i] <- factor(W[,i])
      # create a design matrix, remove the first level
      mm <- model.matrix(~-1+W[,i])
      mW <- mm[,-c(1)]
      # format the names of the indicator variables
      # and add them to the design matrix
      levs <- gsub(" ","",levels(W[,i]) )[-c(1)]
      if(length(levs)<2) mW <- matrix(mW,ncol=1)
      colnames(mW) <- paste(names(W)[i],levs,sep="")
      W <- data.frame(W,mW)
    }
  }
  # now drop the factors that have been replaced by indicators (if any)
  if(length(flist)>0) {
    W <- subset(W,select=-c(flist))
  }
  # return results
  return(W)
}

