#' Find nearest standards
#'
#' @param run run index
#' @param stds indexes of standard runs
#' @param n number of standard runs
#'
#' @return A vector of nearest standard run indexes
#' @export
#'
nearest_standards <- function(run, stds, n) {
  stopifnot(run > 0, stds > 0, n > 0, length(stds) > 0)
  dist <- order(abs(run - stds))
  sorted <- stds[dist]
  sorted[1:n]
}

#' Create an index of standard runs
#'
#'
#' @param type A character vector of sample type ids where "S" denotes a normalizing standard
#' @param run The index of a runs to ignore (to avoid self-normalization)
#'
#' @return The indexes of standard runs
#' @export
#'
find_standard_runs <- function(types, drop = NULL) {
  if (!is.null(drop)) types[drop] <- NA
  which(types == "S")
}
