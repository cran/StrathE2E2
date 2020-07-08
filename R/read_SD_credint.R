#
# read_SD_credint.R
#
#' return list of annealing parameters
#'
#' @param model.path model path
#' @param file credible intervals control filename, default 'monte_carlo.csv'
#'
#' @return list of annealing parameters for fitting ecology model
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_SD_credint <- function(model.path, file=SD_CONTROL_FILE_CREDINT) {

	annealing_control_data <- readcsv(model.path, SD_CONTROL_DIR, file)

	axmin		<- annealing_control_data[1,1]
	axmax		<- annealing_control_data[2,1]
	Prefsd		<- annealing_control_data[3,1]	# Preference parameter sd
	u_sd		<- annealing_control_data[4,1]	# Maximum uptake rate sd
	h_sd		<- annealing_control_data[5,1]	# Half saturation density sd
	biogeo_sd	<- annealing_control_data[6,1]	# microbial parameter sd
	mort_sd		<- annealing_control_data[7,1]	# density dependent mortality rate sd
	ressd		<- annealing_control_data[8,1]	# other parameters sd

	annealing.parms <- list(
		axmin		= axmin,
		axmax		= axmax,
		Prefsd		= Prefsd,
		u_sd		= u_sd,
		h_sd		= h_sd,
		biogeo_sd	= biogeo_sd,
		mort_sd		= mort_sd,
		ressd		= ressd
	)
}

