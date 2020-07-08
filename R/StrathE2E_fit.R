#
# StrathE2E_fit.R
#
#' run the StrathE2E model and return objective error
#'
#' @param model current model configuration
#' @param build previously built model
#' @param quiet set to true to quieten output from the model run
#'
#' @return fit to target data
#'
#' @noRd
#
# ------------------------------------------------------------------------------

StrathE2E_fit <- function(model, build, quiet=TRUE) {

	pkg.env$quiet <- quiet			# quietens down read/write notes

	# Run the model:
	output <- StrathE2E.run(build)

	model.path		<- elt(model, "setup", "model.path")

	aggregates		<- aggregate_model_output(model, output)
	annual.target.data	<- read_annual_target_data(model.path)
	model.target.results	<- derive_model_target_results(model, build, output, aggregates, annual.target.data)
	fit.to.target.data	<- calculate_error_function(model, model.target.results)

	fit.to.target.data
}

