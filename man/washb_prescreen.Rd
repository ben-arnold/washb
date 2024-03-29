% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/washb_prescreen.R
\name{washb_prescreen}
\alias{washb_prescreen}
\title{Pre-screen covariates using a likelihood ratio test.}
\usage{
washb_prescreen(Y, Ws, family = "gaussian", pval = 0.2, print = TRUE)
}
\arguments{
\item{Y}{Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)}

\item{Ws}{data frame that includes candidate adjustment covariates to screen}

\item{family}{GLM model family (gaussian, binomial, poisson, or negative binomial). Use "neg.binom" for Negative binomial.}

\item{pval}{The p-value threshold: any variables with a p-value from the lielihood ratio test below this threshold will be returned. Defaults to 0.2}

\item{print}{Logical for whether to print function output, defaults to TRUE.}
}
\value{
Function returns the list of variable names with a likelihood ratio test p-value <0.2 (unless a custom p-value is specified).
}
\description{
Pre-screen covariates using a likelihood ratio test.
}
\examples{

#Prescreen function applied to the Bangladesh diarrheal disease outcome.
#The function will test a matrix of covariates and return those related to child diarrheal disease with
#a <0.2 p-value from a likelihood ratio test.

#Load diarrhea data:
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

#Ensure that month is coded as a factor
ad$month <- factor(ad$month)

#Sort the data for perfect replication when using V-fold cross-validation
ad <- ad[order(ad$block,ad$clusterid,ad$dataid,ad$childid),]


###Subset to a new dataframe the variables to be screened:
Ws <- subset(ad,select=c("fracode","month","agedays","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile"))

###Run the washb_prescreen function
prescreened_varnames<-washb_prescreen(Y=ad$diar7d,Ws,family="binomial")

###Rerun the function with a stricter p=value
prescreened_varname2s<-washb_prescreen(Y=ad$diar7d,Ws,family="binomial", pval=0.5)

}
