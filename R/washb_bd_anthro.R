#' Anthropometry data from Wash Benefits Bangladesh Children
#'
#' A dataset containing circumference, length, weight, and age data for
#' Bangladesh Wash Benefits children.
#'
#' @format A data frame with 9342 rows and 46 variables:
#' \describe{
#'   \item{dataid}{Unique observation ID}
#'   \item{childid}{Child ID}
#'   \item{dataid}{Compound ID}
#'   \item{childid}{Child ID}
#'   \item{motherid}{Mother ID}
#'   \item{tchild}{Target child in birth cohort}
#'   \item{clusterid}{Cluster ID}
#'   \item{block}{Randomization block ID}
#'   \item{tr}{Treatment assignment}
#'   \item{svy}{Survey round (0,1,2)}
#'   \item{anthrodate}{Date of anthro measurement}
#'   \item{month}{Month of measurement}
#'   \item{c404}{Maternal weight meas 1}
#'   \item{c405}{Maternal weight meas 2}
#'   \item{c406}{Maternal weight meas 3}
#'   \item{c408}{Child weight w/ mother meas 1}
#'   \item{c409}{Child weight w/ mother meas 2}
#'   \item{c410}{Child weight w/ mother meas 3}
#'   \item{c414}{Child length meas 1}
#'   \item{c415}{Child length meas 2}
#'   \item{c416}{Child length meas 3}
#'   \item{c418}{Head circumference meas 1}
#'   \item{c419}{Head circumference meas 2}
#'   \item{c420}{Head circumference meas 3}
#'   \item{sex}{Sex}
#'   \item{birthord}{Birth order (target child)}
#'   \item{aged}{Age in days (anthro meas)}
#'   \item{agem}{Age in months (anthro meas)}
#'   \item{agey}{Age in years (anthro meas)}
#'   \item{weight}{Child weight (median), kg}
#'   \item{length}{Child length (median), cm}
#'   \item{headcir}{Child head circumference (median), cm}
#'   \item{laz}{Length/height-for-age Z-score}
#'   \item{laz_x}{abs(LAZ)>6, set to missing}
#'   \item{lazminus2}{Stunted (LAZ<-2)}
#'   \item{lazminus3}{Severely Stunted (LAZ<-3)}
#'   \item{waz}{Weight-for-age Z-score}
#'   \item{waz_x}{WAZ < -6 or WAZ > 5, set to missing}
#'   \item{wazminus2}{Underweight (WAZ<-2)}
#'   \item{wazminus3}{Severely Underweight (WAZ<-3)}
#'   \item{whz}{Weight-for-length/height Z-score}
#'   \item{whz_x}{abs(WHZ)>5, set to missing}
#'   \item{whzminus2}{Wasted (WHZ<-2)}
#'   \item{whzminus3}{Severely Wasted (WHZ<-3)}
#'   \item{bmiz}{BMI-for-age Z-score}
#'   \item{bmiz_x}{abs(BMIZ)>5, set to missing}
#'   \item{hcz}{Head circumference-for-age z-score}
#'   \item{hcz_x}{abs(HCZ)>5, set to missing}
#' }
#' @source NA
"washb_bd_anthro"
