
# TODO: How does snicser handle ties?
# I think the bubble sort in SNICSer's FindNearestStandards will always pick the
# first element in a tie, which is the same as these functions


#' Find nearest standards in time
#'
#' @param types A character vector of sample type ids where "S" denotes a normalizing standard
#' @param times A vector of run timestamps
#' @param run The index of the run to find standards for
#' @param n number of standard runs
#' @param self Allow self normalization
#'
#' @return
#' @export
#'
#' @examples
nearest_standards_time <- function(types, times, run, n, self = FALSE) {
  stopifnot(length(types) == length(times))
  drop <- if (self) NULL else run
  stds <- find_standard_runs(types, drop = drop)
  nearest <- nearest_standards(times[run], times[stds], n)
  which(times %in% nearest)
}

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
#' @param types A character vector of sample type ids where "S" denotes a normalizing standard
#' @param drop The index of a runs to ignore (to avoid self-normalization)
#'
#' @return The indexes of standard runs
#' @export
#'
find_standard_runs <- function(types, drop = NULL) {
  if (!is.null(drop)) types[drop] <- NA
  which(types == "S")
}
