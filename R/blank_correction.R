## Functions for Blank correction

#' Apply large blank
#'
#' @param fmmeas Normalized Fm of the sample
#' @param fmblank Normalized Fm of the blank
#' @param fmstd Accepted Fm of the standard used for normalization
#'
#' @return Blank corrected Fm
#' @export
#'
doLBC <- function(fmmeas, fmblank, fmstd) {
	fmmeas - fmblank * (fmstd - fmmeas) / fmstd
}

#' Determine error in blankfm
#'
#' Uses the "error floor" method from SNICSer
#'
#' @param blankfm A vector of normalized Fm's of blanks.
#' @param blankerr An optional vector of normalized Fm errors of blanks.
#'
#' @return An error for the given blanks.
#' @export
#'
blankErr <- function(blankfm, blankerr = NULL) {
  if (is.null(blankerr)) {
    sd <- sd(blankfm)
  } else {
    if (length(blankfm) != length(blankerr)) {
      stop("blankfm and blankerr must be of equal length")
    }

    if (length(blankfm) == 1) {
      sd <- blankerr
    } else {
      merr <- mean(blankerr)
      sd <- sd(blankfm)
      sd <- ifelse(merr > sd, merr, sd)
    }
  }
  mfm <- mean(blankfm)
  ifelse(sd > mfm / 2, sd, mfm / 2)
}

#' Propagate large blank error
#'
#' @param fmmeas Normalized Fm of the sample(s)
#' @param fmblank Normalized Fm of the blank
#' @param fmstd Accepted Fm of the standard used for normalization
#' @param fmmeaserr Measurement error of the sample(s)
#' @param fmblankerr Measurement error of the blank
#'
#' @return A vector of propagated errors.
#' @export
#'
doLBCerr <- function(fmmeas, fmblank, fmstd, fmmeaserr, fmblankerr) {
  # Check inputs: fmblank, fmstd, fmblankerr should be scalar, others dbl.
	sqrt(fmmeaserr ^ 2 * (1 + fmblank / fmstd) ^ 2 +
	     fmblankerr ^ 2 * ((fmmeas - fmstd) / fmstd) ^ 2)
}


#' Apply mass balance blank correction
#'
#' Note, use total mass - blank mass for mass of the sample
#' if appropriate
#'
#' @param fmmeas Normalized Fm of the sample(s)
#' @param fmblank Normalized Fm of the blank
#' @param massmeas Mass of the sample(s)
#' @param massblank Mass of the blank
#'
#' @return A vector of mass balance corrected Fm.
#' @export
#'
doMBC <- function(fmmeas, fmblank, massmeas, massblank) {
  # Check inputs
  # LBC leads to negative fm's, so testing inputs is tricky
  # How should we handle these cases, data-wise?
  stopifnot(exprs = {
    is.numeric(c(fmmeas, fmblank, massmeas, massblank))
    #fmmeas >= 0
    #fmblank >= 0
    #massblank >=0
    #massmeas > massblank
  })

    fmmeas + (fmmeas - fmblank) * massblank / (massmeas - massblank)
}

#' Propagate mass balance blank correction error
#'
#' Note, use total mass - blank mass for mass of the sample
#' if appropriate
#'
#' @param fmmeas Normalized Fm of the sample(s)
#' @param fmblank Normalized Fm of the blank
#' @param massmeas Mass of the sample(s)
#' @param massblank Mass of the blank
#' @param fmmeaserr Measurement error of the sample(s)
#' @param fmblankerr Measurement error of the blank
#' @param massmeaserr Mass error of the sample(s)
#' @param massblankerr Mass error of the blank
#'
#' @return A vector of errors for blank corrected samples
#' @export
#'
doMBCerr <- function(fmmeas, fmblank, massmeas, massblank,
                  fmmeaserr, fmblankerr, massmeaserr, massblankerr) {
    sqrt(fmmeaserr ^ 2 * (1 + massblank / (massmeas - massblank)) ^ 2 +
         (massmeaserr ^ 2 + massblankerr ^ 2) * ((fmmeas - fmblank) * massblank / (massmeas - massblank) ^ 2) ^ 2 +
         fmblankerr ^ 2 * (massblank / (massmeas - massblank)) ^ 2 +
         massblankerr ^ 2 * ((fmmeas - fmblank) / (massmeas - massblank)) ^ 2)
}
