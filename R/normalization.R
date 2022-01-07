## Functions for normalization by run or by target

#' Normalize a run using the measured and consensus value of a standard
#'
#' @param sample Ratio to be normalized.
#' @param standard Measured ratio of the standard.
#' @param stdrat Consensus value of the standard.
#'
#' @return The normalized ratio of the sample.
#' @export
#'
norm_run <- function(sample, standard, stdrat = 1.0398) {
  sample/standard * stdrat
}

#' Calculate normalization error
#'
#' @param sample The normalized Fm of the sample
#' @param standard The normalized Fm of the standard
#' @param sample_err The error of the sample
#' @param standard_err The error of the standard
#' @param stdrat The known ratio of the standard
#' @param stdrat_err The error of the known ratio of standard
#'
#' @return The propagated error of the normalized sample
#' @export
#'
norm_run_err <- function(sample, standard, sample_err, standard_err,
                     stdrat = 1.0398, stdrat_err = 0.0006) {
  sqrt(sample_err^2/sample^2 +
         standard_err^2/standard^2 +
         stdrat_err^2/stdrat^2)
}
