

#' Run glm with Lasso prescreening built into boostrapped confidence intervals
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

# data(opposites)
# res<-clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)
# summary(res)

# library(ClusterBootstrap)
# data(opposites)
# res<-clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)
# summary(res)

# data=opposites
# model="SCORE~Time*COG"
#
# clusterid="id"
# family = "binomial"
# B = 50
# confint.level = 0.95
# n.cores = 1

# data=dmat
# clusterid="id"
# Ws=colnames(W)
# forcedW=NULL
# pair=NULL
#   #temp!
#   B = 10
#   confint.level = 0.95
#   n.cores = 1

# data=dmat
# clusterid="id"
# Ws=colnames(W)
# forcedW=NULL
# pair=NULL
# #temp!
# B = 10
# confint.level = 0.95
# n.cores = 1


cowboy_glm <- function (data, clusterid="id", Ws, forcedW=NULL, pair=NULL,
                        family = "gaussian", B = 200, confint.level = 0.95, n.cores = 1){
  require(ClusterBootstrap)

  tt_cores <- detectCores()
  if (n.cores > tt_cores) {
    message(sprintf("Note: \"n.cores\" was set to %d, but only %d are available. Using all cores.",
                    n.cores, tt_cores))
  }

  set.seed(12345)

  #fit full and prescreened model
  bfull <- paste(c("tr",Ws, forcedW,pair), collapse="+")
  prescreened_Ws<-washb_glmnet_prescreen(Y=Y,W,family=family)
  b <- paste(c("tr",prescreened_Ws, forcedW,pair), collapse="+")
  full_model <-as.formula(paste("Y ~ ",bfull,sep = ""))
  model <-as.formula(paste("Y ~ ",b,sep = ""))
  res.or <- glm(full_model, family = family, data = data)
  res.or.screen <- glm(model, family = family, data = data)
  confint.pboundaries = c((1 - confint.level)/2, 1 - (1 - confint.level)/2)
  confint.Zboundaries = qnorm(confint.pboundaries)
  n <- nrow(data)
  p <- length(res.or$coef)

  coefs <- matrix(NA, nrow = B, ncol = p)

  #arguments <- as.list(match.call())
  #clusterid <- eval(arguments$clusterid, data)
  clusterid <- data %>% select(!!(clusterid))
  cluster <- as.character(clusterid)
  clusters <- unique(cluster)
  Obsno <- split(1:n, cluster)
  f = matrix(clusters, length(clusters), B)
  ff = matrix(f, prod(dim(f)), 1)
  fff = sample(ff)
  f = matrix(fff, length(clusters), B)
  # if (is.numeric(n.cores) & n.cores > 0) {
  #   if (n.cores == 1) {

  #temp
  i=1
      for(i in 1:B){
        set.seed(i)
        j <- f[, i]
        obs <- unlist(Obsno[j])

        dboot=data[obs, ]

        prescreened_Ws<-washb_glmnet_prescreen(Y=dboot$Y, dboot %>% select(!!(Ws)),family=family)
        b <- paste(c("tr",prescreened_Ws, forcedW,pair), collapse="+")
        model <-as.formula(paste("Y ~ ",b,sep = ""))
        bootcoef <- tryCatch(coef(glm(model, family = family,data = dboot)), error = function(x) rep(as.numeric(NA),p))

        coefs[i, which(names(res.or$coef) %in% names(bootcoef))] <- bootcoef
      }
  #   }
  #   if (n.cores > 1) {
  #     cl <- makeCluster(max(min(n.cores, tt_cores, 2)))
  #     previous_RNGkind <- RNGkind()[1]
  #     RNGkind("L'Ecuyer-CMRG")
  #     nextRNGStream(.Random.seed)
  #     clusterExport(cl, varlist = c("f", "Obsno",
  #                                   "model", "family", "data",
  #                                   "p", "res.or", "clusbootglm_sample_glm"),
  #                   envir = environment())
  #     splitclusters <- 1:B
  #     out <- parSapplyLB(cl, splitclusters, function(x) clusbootglm_sample_glm(f,
  #                                                                              x, Obsno, model, family, data, p, res.or))
  #     coefs <- t(out)
  #     stopCluster(cl)
  #     RNGkind(previous_RNGkind)
  #   }
  # }


  #XXXXXX  Check if this is needed:
  invalid.samples <- colSums(is.na(coefs))
  names(invalid.samples) <- colnames(coefs) <- names(res.or$coef)
  samples.with.NA.coef <- which(is.na(rowSums(coefs)))
  sdcoefs <- apply(coefs, 2, sd, na.rm = TRUE)
  ci_percentile <- ClusterBootstrap:::confint_percentile(coefs, confint.pboundaries)
  ci_parametric <- ClusterBootstrap:::confint_parametric(sdcoefs, res.or$coef, confint.Zboundaries)
  #get error with bias correction and acceleration interval (for skewed data)
  #ci_BCa <- ClusterBootstrap:::confint_BCa(B, invalid.samples, model, data, clusterid, family, coefs, res.or$coef, p, confint.Zboundaries)
  ci_BCa <- matrix(NA,1,1)
  # rownames(ci_BCa) <- dimnames(ci_parametric)[[1]]
  # colnames(ci_BCa) <- dimnames(ci_percentile)[[2]]
  rownames(ci_percentile) <- dimnames(ci_parametric)[[1]]
  colnames(ci_parametric) <- dimnames(ci_percentile)[[2]]
  result <- list(call = match.call(), model = model, family = family,
                 B = B, coefficients = coefs, data = data, bootstrap.matrix = f,
                 subject.vector = clusterid, lm.coefs = res.or$coef, boot.coefs = colMeans(coefs, na.rm = TRUE),
                 boot.sds = sdcoefs, ci.level = confint.level,
                 percentile.interval = ci_percentile, parametric.interval = ci_parametric,
                 BCa.interval = ci_BCa, samples.with.NA.coef = samples.with.NA.coef,
                 failed.bootstrap.samples = invalid.samples)
  class(result) <- "clusbootglm"
  return(result)
}
