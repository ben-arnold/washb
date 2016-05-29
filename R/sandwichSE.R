
#' sandwichSE
#'
#' @param dat Data used to fit the model
#' @param fm Model fit (object)
#' @param cluster Vector of cluster ID's
#'
#' @return to be written
#' @export
#'
#' @examples
#'  to be written
#'



sandwichSE   <- function(dat,fm, cluster){
  # dat: data used to fit the model
  # fm : model fit (object)
  # cluster : vector of cluster IDs
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)
}
