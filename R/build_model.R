#
# build_model.R
#
#' Build model for invocation of StrathE2E
#'
#' adjust initial state taking notice of physical parameters
#'
#' @param model model list object
#' @param nyears number of years to run the model
#'
#' @return build object
#'
#' @noRd
#
# ------------------------------------------------------------------------------

build_model <- function(model, nyears=10) {

	setup			<- elt(model, "setup")
	data			<- elt(model, "data")

	physical.parameters	<- elt(data, "physical.parameters")
	fixed.parameters	<- elt(data, "fixed.parameters")
	fitted.parameters	<- elt(data, "fitted.parameters")
	physics.drivers		<- elt(data, "physics.drivers")
	chemistry.drivers	<- elt(data, "chemistry.drivers")
	biological.events	<- elt(data, "biological.events")
	initial.state		<- elt(data, "initial.state")

	# generate or make adjustments to any variables down the line:
	run			<- build_model_run(nyears)
	initial.state		<- build_initial_state(initial.state, physical.parameters)
	drivers			<- build_annual_drivers(run, fixed.parameters, physical.parameters, physics.drivers, chemistry.drivers, biological.events)
	forcings		<- interpolate_drivers(run, drivers)
	uptakes			<- calculate_uptakes(fitted.parameters)

	fleet.output		<- fishing_fleet_model(model, nyears)			# run the fishing fleet model
	model.parameters	<- build_model_parameters(model, uptakes, fleet.output)	# all parameters going through to C model

	build <- list(
		run			= run,
		initial.state		= initial.state,
		drivers			= drivers,
		forcings		= forcings,
		uptakes			= uptakes,
		fleet.output		= fleet.output,
		model.parameters	= model.parameters
	)

	build
}

