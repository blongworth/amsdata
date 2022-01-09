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

#' Calculate normalized ratio of a run, SNICSer style
#'
#' Snicser does things in this order:
#'
#' 1. 13C correction (prior to this fn)
#' 2. Divide ratio of stds by known standard Fm
#' 3. Average standard ratios
#' 4. Divide run ratio by normalized avg std ratio
#'
#' @param run_val The ratio of the run to normalize
#' @param std_vals A vector of standard ratios to normalize with
#' @param std_rat The standard ratio(s). May be one value for all stds
#' used or a vector of values for each standard in std_vals, defaults
#' to OX-I
#'
#' @return The normalized ratio of the run
#' @export
#'
norm_snicser <- function(run_val, std_vals, std_rat = 1.0398) {
  norm_std_rat <- mean(std_vals/std_rat)
  norm_run <- run_val/norm_std_rat
}

#' Calculate error of a run, SNICSer style
#'
#' @param run_val The ratio of the run to normalize
#' @param run_err The error of the run to normalize
#' @param std_vals A vector of standard ratios to normalize with
#' @param std_rat The standard ratio(s). May be one value for all stds
#' used or a vector of values for each standard in std_vals, defaults
#' to OX-I
#'
#' @return The error in the normalized ratio of the run
#' @export
#'
#' @examples
norm_snicser_err <- function(run_val, run_err, std_vals, std_rat = 1.0398) {
  nstd <- length(std_vals)
  norm_std_rat <- mean(std_vals / std_rat)
  norm_run <- run_val / norm_std_rat
  sum_sig <- sqrt(sum((std_vals / std_rat)^2))
  sum_sq <- sum((std_vals/std_rat-norm_std_rat)^2)
  std_err <- sqrt(sum_sq/nstd)
  run_err <- pmax(norm_run * sum_sig / norm_std_rat / nstd,
                  norm_run * std_err / norm_std_rat)
  norm_run_err <- sqrt(run_err^2 + norm_run^2 * run_err^2 / run_val^2)
  norm_run_err
}
