#' Ante-mortem Tooth Loss Data
#' 
#' Data from four primate genera on tooth loss and its relationship to age and sex. Used for measurement error example in the textbook.
#' 
#' @format A data frame of 1450 observations and the following 9 variables:
#' \enumerate{
#'   \item tooth_class : One of Anterior, Posterior, or Premolar
#'   \item specimen : Unique identifier for specimen
#'   \item num_amtl : Number of teeth missing of given class
#'   \item sockets : number of observable sockets that could be scored for missing teeth
#'   \item age : Estimated age of speciment at death
#'   \item stdev_age : Assigned uncertainty of age at death
#'   \item prob_male : Estimate of sex of specimen
#'   \item genus : Specimen genus, one of Homo, Pan, Papio, or Pongo
#'   \item population : Region specimen originates from
#' }
#' 
#' @usage data(AMTL_short)
#' data(AMTL)
#' 
#' @name AMTL
#' @aliases AMTL_short
#' @docType data
#' @keywords data
NULL
