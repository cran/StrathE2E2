#
# read_SD_sensitivity.R
#
#' return list of annealing parameters
#'
#' @param model.path model path
#' @param file sensitivity analysis control file, default 'sensitivity.csv'
#'
#' @return list of annealing parameters for OAT sensitivity analysis
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_SD_sensitivity <- function(model.path, file=SD_CONTROL_FILE_SENSITIVITY) {

	annealing_control_data <- readcsv(model.path, SD_CONTROL_DIR, file)

	#Set the axis min and max for the display plot
	axmin		<- annealing_control_data[1,1]
	axmax		<- annealing_control_data[2,1]

	annealing.parms <- list(
		axmin		= axmin,
		axmax		= axmax
	)
}
