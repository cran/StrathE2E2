#
# read_biological_event_timings.R
#
#' Read the biological event timing parameters
#'
#' returns event timings
#'
#' @param model.path path to model
#'
#' @return events
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_biological_event_timings <- function(model.path) {

	events <- get.model.file(model.path, PARAMETERS_DIR, file.pattern=BIOLOGICAL_EVENTS)

	events
}

