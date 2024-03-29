% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/washb_mh.R
\name{washb_mh}
\alias{washb_mh}
\title{Mantel-Haenszel pooled estimates of the prevalence ratio (PR) or the prevalence
difference (PD) in the WASH Benefits trials}
\usage{
washb_mh(Y,tr,strat,contrast,measure="RR")
}
\arguments{
\item{Y}{binary outcome variable (here: diar7d)}

\item{tr}{binary treatment group variable, comparison group first}

\item{strat}{stratification variable (here: block)}

\item{contrast}{vector of length 2 that includes the tr groups to contrast (control(reference arm) and then intervention)}

\item{measure}{measure of effect. RR = prev ratio, RD = prev difference}
}
\value{
res: Matrix of RD, se.RD, ci.lb, ci.ub, Z, p-value.
}
\description{
Mantel-Haenszel pooled estimates of the prevalence ratio (PR) or the prevalence
difference (PD) using randomization block as the stratification variable.
}
\details{
The function calls the M-H estimator for two different arms of the study. It relies on
the rma.mh() function in the metafor package.

Estimate the Mantel-Haenszel prevalence ratio note: strata with no outcomes (i.e., missing PR) are dropped.
This is consistent with a fixed-effects regression analysis, in which those strata would not contribute to the estimates.
The arguments Y,tr,strat, below need to be from the same dataset.
}
\examples{
#Prescreen function applied to the Bangladesh diarrheal disease outcome.
#The function will test a matrix of covariates and return those related to child diarrheal disease with
#a <0.2 p-value from a likelihood ratio test.

#Load  diarrhea data
library(washb)
data(washb_bangladesh_enrol)
washb_bangladesh_enrol <- washb_bangladesh_enrol
data(washb_bangladesh_diar)
washb_bangladesh_diar <- washb_bangladesh_diar

 # drop svydate and month because they are superceded in the child level diarrhea data
washb_bangladesh_enrol$svydate <- NULL
washb_bangladesh_enrol$month <- NULL

# merge the baseline dataset to the follow-up dataset
ad <- merge(washb_bangladesh_enrol,washb_bangladesh_diar,by=c("dataid","clusterid","block","tr"),all.x=F,all.y=T)

# subset to the relevant measurement
# Year 1 or Year 2
ad <- subset(ad,svy==1|svy==2)

#subset the diarrhea to children <36 mos at enrollment
### (exlude new births that are not target children)
ad <- subset(ad,sibnewbirth==0)
ad <- subset(ad,gt36mos==0)

# Exclude children with missing data
ad <- subset(ad,!is.na(ad$diar7d))

#Re-order the tr factor for convenience
ad$tr <- factor(ad$tr,levels=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

###Create vector of contrasts for each hypothesis to facilitate comparisons between arms.
#Hypothesis 1: Each intervention arm vs. Control
h1.contrasts <- list(
  c("Control","Water"),
  c("Control","Sanitation"),
  c("Control","Handwashing"),
  c("Control","WSH"),
  c("Control","Nutrition"),
  c("Control","Nutrition + WSH")
)

#Apply washb_mh to the water vs. control arm contrast.
washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RR")

#Return the risk difference instead of the risk ration:
washb_mh(Y=ad$diar7d,tr=ad$tr, contrast=c("Control","Water"), strat=ad$block,measure="RD")


#Use sapply command to efficiently apply the function to all the treatment arm contrasts

diff.h1 <- t(sapply(h1.contrasts,washb_mh,Y=ad$diar7d,tr=ad$tr,strat=ad$block,measure="RR"))
rownames(diff.h1) <- c("Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
print(diff.h1)

}
