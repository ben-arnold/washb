#' Diarrheal disease data from Wash Benefits Bangladesh Children
#'
#' Bangladesh diarrhea dataset, created by 3-bangladesh-dm-diar.do
#'
#' @format A data frame with 9342 rows and 46 variables:
#' \describe{
#'   \item{dataid}{Unique compound ID}
#'   \item{childid}{Child ID}
#'   \item{tchild}{Target child in birth cohort}
#'   \item{clusterid}{Cluster ID}
#'   \item{block}{Randomization block ID}
#'   \item{tr}{Treatment assignment}
#'   \item{svy}{Survey round (0,1,2)}
#'   \item{svydate}{Survey date}
#'   \item{month}{Month of measurement}
#'   \item{sex}{Sex (1=male)}
#'   \item{dob}{Date of birth}
#'   \item{agedays}{Age in days}
#'   \item{ageyrs}{Age in years}
#'   \item{enrolage}{Age at enrollment, years}
#'   \item{newbirth}{New birth}
#'   \item{sibnewbirth}{Sibling (non-index child) new birth}
#'   \item{gt36mos}{Older than 36 mos at enrollment}
#'   \item{d3plus2d}{3+ stools in 24 hr, 2d recall}
#'   \item{d3plus7d}{3+ stools in 24 hr, 7d recall}
#'   \item{dloose2d}{loose or watery stool, 2d recall}
#'   \item{dloose7d}{loose or watery stool, 7d recall}
#'   \item{dblood2d}{blood in stool, 2d recall}
#'   \item{dblood7d}{blood in stool, 7d recall}
#'   \item{diar2d}{Diarrhea case, 2d recall}
#'   \item{diar7d}{Diarrhea case, 7d recall}
#'   \item{bruise2d}{Bruising, 2d recall}
#'   \item{bruise7d}{Bruising, 7d recall}
#'   \item{tooth2d}{Toothache, 2d recall}
#'   \item{tooth7d}{Toothache, 7d recall}
#'
#'
#'   ...
#' }
#' @source
"washb_bd_diar"


library(foreign)
read
