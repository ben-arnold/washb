# # #Fake data
# # df <- data.frame(
# #   CTmean.Ac = rnorm(100, 3),
# #   tr = rep(c("Control", "Nutrition + WSH"), each=500),
# #   id=1:1000,
# #   sex = rep(c("Male", "Female"))
# # )
# #
# # # re-parse the variables and final-format to use them in tmle()
# # set.seed(12345)
# # tmle_A <- ifelse(df$tr==contrasts[2],1,0)
# # tmle_Y <- rnorm(1000, 3) + 2*tmle_A +  +  rnorm(1000)
# # tmle_Delta <- rep(1, nrow(df))
# # tmle_id <- as.numeric(df$dataid)
# # tmle_id <- 1:nrow(df)
# #
# # df <- data.frame(
# #   CTmean.Ac = tmle_Y,
# #   tr = df$tr,
# #   id=df$id,
# #   sex = df$sex
# # )
#
# library(washb)
# library(tidyverse)
# df<- read.csv("C:/Users/andre/Downloads/to-andrew.csv")
# head(df)
# df$clusterid
#
#
# Y=df$CTmean.Al
# tr=df$tr
# W=df %>% select(sex)
# id=df$clusterid
# family="gaussian"
# contrast=c("Control", "WSH")
#  FECR="arithmetic"
#  print=TRUE
#
# pair=NULL
# W=df %>% select(sex, momage, momheight)
# forcedW=NULL
# V="sex"
# pval=0.2
# verbose=FALSE
#
#
# tmleRES <- washb_tmle(Y=df$CTmean.Al, tr=df$tr,W=df %>% select(sex, momage, momheight), family="gaussian",contrast=c("Control", "WSH"),  Q.SL.library = c("SL.glm"), g.SL.library = "SL.mean", FECR="arithmetic", seed=123, print=TRUE)
# glmRES <- washb_glm(Y=df$CTmean.Al, tr=df$tr,W=df %>% select(sex, momage, momheight), id=df$clusterid, family="gaussian",contrast=c("Control", "WSH"), FECR="arithmetic",  print=TRUE)
#
# test_V <- washb_glm(Y=df$CTmean.Al, tr=df$tr,W=df %>% select(sex, momage, momheight), V="sex", id=df$clusterid, family="gaussian",contrast=c("Control", "WSH"), FECR=NULL,  print=TRUE)
# test_V$lincom
#
#
# tmleRES.g <- washb_tmle(Y=df$CTmean.Ac, tr=df$tr,W=df %>% select(sex), id=df$clusterid, family="gaussian",contrast=c("Control", "WSH"),  Q.SL.library = c("SL.glm"), g.SL.library = "SL.mean", FECR="geometric", seed=123, print=TRUE)
# glmRES.g <- washb_glm(Y=df$CTmean.Ac, tr=df$tr,W=df %>% select(sex), id=df$clusterid, family="gaussian",contrast=c("Control", "WSH"), FECR="geometric",  print=TRUE)
#
#
#
#
#
#
# #Simulate data with non-linear x-y relationship and clustered obs
# set.seed(1234)
#
# n<-1000
# x <- rnorm(n)
# id <- factor(rep(1:100, each=10))
# sex = rep(c("Male", "Female"), 500)
# tr = rep(c("Control", "Nutrition + WSH"), each=500)
# df <- data.frame(x, id, tr, sex)
# df <- df %>% group_by(id) %>% mutate(clust_mean=rnorm(1, 0, 10)) %>%
#   ungroup() %>%
#   mutate(clust_mean=clust_mean + rnorm(1,0, 1),
#          dummy=1)
#
# df$Y <-   df$x + df$clust_mean + 10*as.numeric(factor(df$tr)) + 2*as.numeric(factor(df$sex)) + rnorm(1000, 0, 10)
#
# tmleRES <- washb_tmle(Y=df$Y, tr=df$tr,W=df %>% select(x, sex),id= df$id, family="gaussian",contrast=c("Control", "Nutrition + WSH"),  Q.SL.library = c("SL.glm"), g.SL.library = "SL.mean", FECR="arithmetic", seed=123, print=TRUE)
# glmRES <- washb_glm(Y=df$Y, tr=df$tr,W=df %>% select(x, sex), id=df$id, family="gaussian",contrast=c("Control", "Nutrition + WSH"), FECR="arithmetic",  print=TRUE)
# tmleRES <- washb_tmle(Y=df$Y, tr=df$tr,W=NULL,id= df$id, family="gaussian",contrast=c("Control", "Nutrition + WSH"),  Q.SL.library = c("SL.glm"), g.SL.library = "SL.mean", FECR="arithmetic", seed=123, print=TRUE)
# glmRES <- washb_glm(Y=df$Y, tr=df$tr,W=NULL, id=df$id, family="gaussian",contrast=c("Control", "Nutrition + WSH"), FECR="arithmetic",  print=TRUE)
# glmRES <- washb_glm(Y=df$Y, tr=df$tr,W=NULL, id=df$id, family="gaussian",contrast=c("Control", "Nutrition + WSH"), FECR="arithmetic",  print=TRUE)
#
#
#
# df$Y <-  log(2*df$x^2 - 5*df$x + df$clust_mean + 10*as.numeric(factor(df$tr)) + 2*as.numeric(factor(df$sex))  + rnorm(1000, 0, 10))
#
# tmleRES.g <- washb_tmle(Y=df$Y, tr=df$tr,W=df %>% select(x, sex), id=df$id, family="gaussian",contrast=c("Control", "Nutrition + WSH"),  Q.SL.library = c("SL.glm"), g.SL.library = "SL.mean", FECR="geometric", seed=123, print=TRUE)
# glmRES.g <- washb_glm(Y=df$Y, tr=df$tr,W=df %>% select(x, sex), id=df$id, family="gaussian",contrast=c("Control", "Nutrition + WSH"), FECR="geometric",  print=TRUE)
#
#
#
#
#
# #
# # library(tidyverse)
# # library(washb)
# # library(msm)
# # library(tmle)
# #
# # outcomes <- read.csv('C:/Users/andre/downloads/qdata.csv')
# #
# # load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData")
# # FECR='arithmetic'
# # d$dataid <- as.numeric(d$dataid)
# #
# # df <- left_join(d, outcomes, by = c( "dataid"))
# # Y <- outcomes$CTmean.Ac[!is.na(outcomes$CTmean.Ac)]
# # df$CTmean.Ac <- rep(Y,10)[1:nrow(df)]
# #
# # df <- df %>% filter(!is.na(CTmean.Ac))
# #
# #
# #
# #
# # summary(outcomes$dataid)
# # summary(d$dataid)
# # summary(df$dataid)
# #
# # summary(outcomes$CTmean.Ac)
# # summary(df$CTmean.Ac)
# #
# # colnames(df)
# #
# # #Fake data
# # df <- data.frame(
# #   CTmean.Ac = rnorm(100, 3),
# #   tr = rep(c("Control", "Nutrition + WSH"), each=500),
# #   id=1:1000,
# #   sex = rep(c("Male", "Female"))
# # )
# #
# # # re-parse the variables and final-format to use them in tmle()
# # set.seed(12345)
# # tmle_A <- ifelse(df$tr==contrasts[2],1,0)
# # tmle_Y <- df$CTmean.Ac + 2*tmle_A +  rnorm(1000)
# # tmle_Delta <- rep(1, nrow(df))
# # tmle_id <- as.numeric(df$dataid)
# # tmle_id <- 1:nrow(df)
# #
# # df <- data.frame(
# #   CTmean.Ac = tmle_Y,
# #   tr = df$tr,
# #   id=df$id,
# #   sex = df$sex
# # )
# #
# #
# # tmleRES <- washb_tmle(Y=df$CTmean.Ac, tr=df$tr,W=df %>% select(sex), family="gaussian",contrast=c("Control", "Nutrition + WSH"),  Q.SL.library = c("SL.glm"), g.SL.library = "SL.mean", FECR="arithmetic", seed=123, print=TRUE)
# # glmRES <- washb_glm(Y=df$CTmean.Ac, tr=df$tr,W=df %>% select(sex), id=df$id, family="gaussian",contrast=c("Control", "Nutrition + WSH"), FECR="arithmetic",  print=TRUE)
# #
# #
# #
# #
# # tmleRES.g <- washb_tmle(Y=df$CTmean.Ac, tr=df$tr,W=df %>% select(sex), family="gaussian",contrast=c("Control", "Nutrition + WSH"),  Q.SL.library = c("SL.glm"), g.SL.library = "SL.mean", FECR="geometric", seed=123, print=TRUE)
# # glmRES.g <- washb_glm(Y=df$CTmean.Ac, tr=df$tr,W=df %>% select(sex), id=df$id, family="gaussian",contrast=c("Control", "Nutrition + WSH"), FECR="geometric",  print=TRUE)
# #
# #
# #
# # contrasts <- c("Control", "Nutrition + WSH")
# #
# #
# #
# # # estimate the ITT using TMLE
# # Q.SL.library <- c("SL.glm")
# # g.SL.library <- c("SL.mean")
# # tmle_fit <- tmle(Y=tmle_Y,
# #                  A=tmle_A,
# #                  W=df %>% subset(., select=sex),
# #                  Delta=tmle_Delta,
# #                  id=tmle_id,
# #                  family="gaussian",
# #                  Q.SL.library=Q.SL.library,
# #                  g.SL.library=g.SL.library)
# # tmle_fit
# #
# # # retreive basic building blocks from the empirical data and the
# # # TMLE estimation results
# # n_id <- length(unique(tmle_id))
# # pDelta0 <- tmle_fit$g.Delta$g1W[,1]
# # pDelta1 <- tmle_fit$g.Delta$g1W[,2]
# # g1   <- tmle_fit$g$g1W
# # Qst0 <- tmle_fit$Qstar[,1]
# # Qst1 <- tmle_fit$Qstar[,2]
# # Ey0  <- mean(Qst0)
# # Ey1  <- mean(Qst1)
# #
# # # estimate the influence curve
# # # for repeated observations within id, collapse the influence curve
# # # then estimate the variance-covariance matrix for EY0 and EY1
# # IC0 <- (tmle_Delta/pDelta0) * ((1-tmle_A)/(1-g1)) * (tmle_Y - Qst0) + Qst0 - Ey0
# # IC1 <- (tmle_Delta/pDelta1) * (tmle_A/g1) * (tmle_Y - Qst1) + Qst1 - Ey1
# # if (n_id < length(id)) {
# #   IC0 <- as.vector(by(IC0, tmle_id, mean))
# #   IC1 <- as.vector(by(IC1, tmle_id, mean))
# # }
# # vc <- (1/n_id)*var(cbind(IC0,IC1))
# #
# # # and   FECR = exp(EY1)/exp(EY0)-1 on the geometric mean scale
# # if(FECR=='arithmetic') {
# #   fecr    <- (Ey1/Ey0) - 1
# #   fderiv  <- c(-Ey1/(Ey0^2),1/Ey0)
# # }
# # if(FECR=='geometric') {
# #   fecr    <- (exp(Ey1)/exp(Ey0)) - 1
# #   fderiv  <- c(-exp(Ey1)/exp(Ey0),exp(Ey1)/exp(Ey0))
# # }
# # fecr_se <- as.vector(sqrt(t(fderiv)%*%vc%*%fderiv))
# # fecr_lb <- fecr-1.96*fecr_se
# # fecr_ub <- fecr+1.96*fecr_se
# # fecr_p  <- 2*(1-pnorm(abs(fecr/fecr_se)))
# # fecr_lb
# # fecr_ub
# # fecr_p
# #
# #
# #
# # #-----------------------------------------------------------------------------
# # #GLM method
# # #-----------------------------------------------------------------------------
# #
# # df <- data.frame(
# #   CTmean.Ac = tmle_Y,
# #   tr = tmle_A,
# #   sex = df$sex
# # )
# #
# # glm.fit <- glm(CTmean.Ac ~ tr + sex, data=df)
# #
# # library(sandwich)
# # fecr_se <- deltamethod(g = ~x2-1, mean = coef(glm.fit), cov = vcovCL(glm.fit, tmle_id), ses=TRUE)
# #
# #
# #
# # df1<-df0 <- df
# # df1$tr <- 1
# # df0$tr <- 0
# # Qst1<-predict(glm.fit, type="response", newdata = df1)
# # Qst0<-predict(glm.fit, type="response", newdata = df0)
# #
# # Ey1  <- mean(Qst1, na.rm=T)
# # Ey0  <- mean(Qst0, na.rm=T)
# #
# # #get covariance
# # #vc <- vcov(glm.fit)[1:2,1:2]
# #
# #
# # # b0 = glm.fit$coefficients[1]
# # # b1 = glm.fit$coefficients[2]
# #
# # # retreive basic building blocks from the empirical data and the
# # # TMLE estimation results
# # n_id <- length(1:nrow(df))
# #
# #
# #
# # # estimate the influence curve
# # # for repeated observations within id, collapse the influence curve
# # # then estimate the variance-covariance matrix for EY0 and EY1
# # IC0 <- ((1-tmle_A)/(1-0.5)) * (tmle_Y - Qst0) + Qst0 - Ey0
# # IC1 <- (tmle_A/0.5) * (tmle_Y - Qst1) + Qst1 - Ey1
# # summary(IC0)
# # summary(IC1)
# #
# # IC0 <- ((1-tmle_A)) * (tmle_Y - Qst0) + Qst0 - Ey0
# # IC1 <- (tmle_A) * (tmle_Y - Qst1) + Qst1 - Ey1
# # summary(IC0)
# # summary(IC1)
# #
# #
# #
# #
# #
# # if (n_id < length(id)) {
# #   IC0 <- as.vector(by(IC0, tmle_id, mean))
# #   IC1 <- as.vector(by(IC1, tmle_id, mean))
# # }
# # vc <- (1/n_id)*var(cbind(IC0,IC1))
# #
# #
# #
# #
# # # use the delta method to get the SE & 95% CI for the FECR
# # # where FECR = (EY0-EY1)/EY0 = (EY1/EY0)-1 on the arithmetic mean scale
# # # and   FECR = exp(EY1)/exp(EY0)-1 on the geometric mean scale
# # if(FECR=='arithmetic') {
# #   fecr    <- (Ey1/Ey0) - 1
# #   fderiv  <- c(-Ey1/(Ey0^2),1/Ey0)
# # }
# # if(FECR=='geometric') {
# #   fecr    <- (exp(Ey1)/exp(Ey0)) - 1
# #   fderiv  <- c(-exp(Ey1)/exp(Ey0),exp(Ey1)/exp(Ey0))
# # }
# # #fecr_se <- as.vector(sqrt(t(fderiv)%*%vc%*%fderiv))
# # fecr_lb <- fecr-1.96*fecr_se
# # fecr_ub <- fecr+1.96*fecr_se
# # fecr_p  <- 2*(1-pnorm(abs(fecr/fecr_se)))
# # fecr_lb
# # fecr_ub
# # fecr_p
# #
# #
# #
# # XXXXXXXXXXXXXXXXXXXXXXXXXX
# # To do: figure out why TMLE estimates
# # have such few (~5) predictions
# # Figure out for GLM how to calc the influence curve
# # Compare TMLE and GLM Ey1 and Ey0
# # XXXXXXXXXXXXXXXXXXXXXXX
# #
# #
# # # g = as.formula(~  (exp(x1+x2+x3+x4) - exp(x1+x2)) / (exp(x1+x3) - exp(x1)))
# # #
# # # delta.se <- deltamethod(g = g, mean = coef(glm.fit), cov = vcov(glm.fit))
# # #
# # #
# # # print=T
# # #
# # # # retreive basic building blocks from the empirical data and the
# # # # TMLE estimation results
# # # n_id <- length(unique(tmle_id))
# # # pDelta0 <- tmle_fit$g.Delta$g1W[,1]
# # # pDelta1 <- tmle_fit$g.Delta$g1W[,2]
# # # g1   <- tmle_fit$g$g1W
# # # Qst0 <- tmle_fit$Qstar[,1]
# # # Qst1 <- tmle_fit$Qstar[,2]
# # # Ey0  <- mean(Qst0)
# # # Ey1  <- mean(Qst1)
# # #
# # # # estimate the influence curve
# # # # for repeated observations within id, collapse the influence curve
# # # # then estimate the variance-covariance matrix for EY0 and EY1
# # # IC0 <- (tmle_Delta/pDelta0) * ((1-tmle_A)/(1-g1)) * (tmle_Y - Qst0) + Qst0 - Ey0
# # # IC1 <- (tmle_Delta/pDelta1) * (tmle_A/g1) * (tmle_Y - Qst1) + Qst1 - Ey1
# # # if (n_id < length(id)) {
# # #   IC0 <- as.vector(by(IC0, tmle_id, mean))
# # #   IC1 <- as.vector(by(IC1, tmle_id, mean))
# # # }
# # # vc <- (1/n_id)*var(cbind(IC0,IC1))
# # #
# # # # use the delta method to get the SE & 95% CI for the FECR
# # # # where FECR = (EY0-EY1)/EY0 = (EY1/EY0)-1 on the arithmetic mean scale
# # # # and   FECR = exp(EY1)/exp(EY0)-1 on the geometric mean scale
# # # if(FECR=='arithmetic') {
# # #   fecr    <- (Ey1/Ey0) - 1
# # #   fderiv  <- c(-Ey1/(Ey0^2),1/Ey0)
# # # }
# # # if(FECR=='geometric') {
# # #   fecr    <- (exp(Ey1)/exp(Ey0)) - 1
# # #   fderiv  <- c(-exp(Ey1)/exp(Ey0),exp(Ey1)/exp(Ey0))
# # # }
# # # fecr_se <- as.vector(sqrt(t(fderiv)%*%vc%*%fderiv))
# # # fecr_lb <- fecr-1.96*fecr_se
# # # fecr_ub <- fecr+1.96*fecr_se
# # # fecr_p  <- 2*(1-pnorm(abs(fecr/fecr_se)))
# # #
# # #
# # # # add FECR results to the tmle_fit$estimates list
# # # tmle_fit$estimates$FECR <- list(psi=fecr,var.psi=fecr_se^2,CI=c(fecr_lb,fecr_ub),pvalue=fecr_p,method=FECR)
# # #
# # # # print results
# # # if(print==TRUE){
# # #   cat(paste("\n-----------------------------------------\nFecal egg count reduction (EY1/EY0)-1,\nestimated using ",FECR," means","\n-----------------------------------------\n",sep=""))
# # #   cat(paste("FECR (95% CI) : ",sprintf("%1.3f",fecr)," (",sprintf("%1.3f",fecr_lb),", ",sprintf("%1.3f",fecr_ub),")",sep=""))
# # #   cat("\n     SE(FECR) :",sprintf("%1.4f",fecr_se))
# # #   cat("\n      p-value :",sprintf("%1.4f",fecr_p))
# # #   cat("\n-----------------------------------------\n")
# # # }
# # #
# # #
# # #
# # #
