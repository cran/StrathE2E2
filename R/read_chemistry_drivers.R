#
# read_chemistry_drivers.R
#
#' Read the default set of chemistry drivers
#'
#' returns drivers in a named list
#'
#' @param model.path path to model
#'
#' @return chemistry driver set
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_chemistry_drivers <- function(model.path) {

	chemistry <- get.model.file(model.path, DRIVING_DATA_DIR, file.pattern=CHEMISTRY_DRIVERS)

	names(chemistry) <- c("month",
		"so_nitrate","so_ammonia","so_phyt","so_detritus",
		"d_nitrate","d_ammonia","d_phyt","d_detritus",
		"si_nitrate","si_ammonia","si_phyt","si_detritus",
		"rivnitrate","rivammonia","rivdetritus",
		"so_atmnitrate","so_atmammonia",
		"si_atmnitrate","si_atmammonia",
		"si_othernitrate","si_otherammonia")

	chemistry
}

