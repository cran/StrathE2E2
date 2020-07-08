#
# read_physics_drivers.R
#
#' Read the default set of physics drivers
#'
#' returns drivers in a named list
#'
#' @param model.path path to model
#'
#' @return physics driver set
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_physics_drivers <- function(model.path) {

	physics <- get.model.file(model.path, DRIVING_DATA_DIR, file.pattern=PHYSICS_DRIVERS)

	names(physics) <- c(
		"month","sslight","so_logespm","si_logespm","so_temp","d_temp","si_temp","rivervol",
		"logkvert","mixlscale","upwelling","so_inflow","d_inflow","si_inflow","si_outflow","so_si_flow",
		"s1_pdist", "s2_pdist", "s3_pdist", "d1_pdist","d2_pdist","d3_pdist","Inshore_waveheight"
	)

	physics
}

