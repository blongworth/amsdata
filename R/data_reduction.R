# Functions to assist in AMS data reduction

# TODO:
# Propagate normalization error
# Find standards
# Produce normalized data for wheel per target

#' @importFrom dplyr mutate select `%>%`
NULL

#' Calculate per-block fields
#'
#' @param data A data table of raw AMS data
#'
#' @return The data table with fields for corrected 14/12,
#' internal error, and d13C added
#' @export
#'
calcRaw <- function(data) {
	data %>%
	  mutate(
	    cor1412 = doCor1412(he1412, he1312),
	    sig1412 = calcSig1412(CntTotH, CntTotS, CntTotGT, cor1412),
	    d13c = calcd13c(he1312)
	  )
}

#' Correct raw 14/12 for fractionation using 13/12 ratio
#'
#' Uses SNICSer method
#'
#' @param he1412 Raw measured 14/12
#' @param he1312 Raw measured 13/12
#'
#' @return 14/12C ratios corrected for fractionation
#' @export
#'
doCor1412 <- function(he1412, he1312) {
	he1412 / he1312 ^ 2
}

# Calc internal error for a measurement
calcSig1412 <- function(CntTotGT, cor1412, CntTotH = 1, CntTotS = 1) {
RelErrSq <- (CntTotH - CntTotS) * CntTotH ^ 2 / CntTotS ^ 4 +
             CntTotH ^ 2 / CntTotGT / CntTotS ^ 2
cor1412 * sqrt(RelErrSq)
}


#' Calculate d13C
#'
#' @param r_unk 13/12 C ratio of the unknowns
#' @param r_vpdb 13/12 C ratio of VPDB
#'
#' @return A vector of d13C values
#' @export
#'
calc_d13c <- function(r_unk, r_vpdb = 0.0112372) {
	1000 * ((r_unk - r_vpdb) / r_vpdb)
}

#' Propagate errors
#'
#' Add errors in quadrature for a vector of errors
#'
#' @param err A vector of errors
#'
#' @return A propagated error for the vector.
#' @export
#'
prop_err <- function(err) {
  sqrt(sum(err^2))/length(err)
}

#' Calculate external (deviation of runs) error for a target
#'
#' @param normRat A vector of normalized ratios for a target
#'
#' @return The external error
#' @export
#'
normRunExtErr <- function(normRat) {
  n <- length(normRat)
  meanRat <- mean(normRat)
  sqrt((sum((normRat - meanRat)^2))/(n * (n - 1)))
}

#' Calculate internal (counting) error for a target.
#'
#' @param counts A vector of counts for each run of a target.
#'
#' @return The internal error
#' @export
#'
normRunIntErr <- function(counts) {
  1 / sqrt(sum(counts))
}

#' Mean ratio for a target, weighted by error
#'
#' @param normRat A vector of normalized ratios.
#' @param normRatErr A vector of errors in measurements of the ratio.
#'
#' @return The weighted mean ratio for the target.
#' @export
#'
meanRat <- function(normRat, normRatErr) {
  (sum(normRat/normRatErr^2)/sum(1/normRatErr^2))
}


# Find mean of stds
normStds <- function(cor1412std, defstd) {
  # both vectors, same length or 1 element vector if all standards are same
  mean(cor1412std/defstd)
}

# Normalize to mean of standards
norm1412 <- function(cor1412, meanstd) {
  cor1412/meanstd
}


