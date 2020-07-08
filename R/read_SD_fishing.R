#
# read_SD_fishing.R
#
#' return list of annealing parameters
#'
#' @param model.path model path
#' @param file fishing control file
#'
#' @return list of annealing parameters for determining scaling factors
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_SD_fishing <- function(model.path, file=SD_CONTROL_FILE_FISHING) {

	annealing_control_data <- readcsv(model.path, SD_CONTROL_DIR, file)

	# Set the axis min and max for the display plot
	axmin	<- annealing_control_data[1,1]
	axmax	<- annealing_control_data[2,1]

	# cv for jiggling the fishing fleet parameters
	deltaH	<- annealing_control_data[3,1]

	annealing.parms <- list(
		axmin		= axmin,
		axmax		= axmax,
		deltaH		= deltaH
	)
}

