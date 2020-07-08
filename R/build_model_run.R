#
# build_model_run.R
#
#' build model run
#'
#' build run time information from number of years
#'
#' @param nyears number of years to run the model
#'
#' @return model run object
#'
#' @noRd
#
# ------------------------------------------------------------------------------

build_model_run <- function(nyears) {

	output.interval	<- 1

	run	<- list(
		nyears		= nyears,				# for easy access
		ndays		= nyears*360+1,
		drndays		= nyears*12,
		times		= seq(0, nyears*360, by=output.interval),
		drtimes		= seq(15, nyears*360-15, by=30),
		sprectimes	= seq(0, nyears*360, by=1),
		daynum		= seq(0, 360, by=1)
	)

	run
}

