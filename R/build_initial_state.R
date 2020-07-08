#
# build_initial_state.R
#
#' adjust initial state taking notice of physical parameters
#'
#' @param initial.state initial state variables
#' @param physical.parms model physical parameters
#'
#' @return adjusted initial state variables
#'
#' @noRd
#
# ------------------------------------------------------------------------------

build_initial_state <- function(initial.state, physical.parms) {

	# Unpack:
	x_detritus_s1		<- elt(initial.state, "x_detritus_s1")
	x_detritus_s2		<- elt(initial.state, "x_detritus_s2")
	x_detritus_s3		<- elt(initial.state, "x_detritus_s3")
	x_detritus_d1		<- elt(initial.state, "x_detritus_d1")
	x_detritus_d2		<- elt(initial.state, "x_detritus_d2")
	x_detritus_d3		<- elt(initial.state, "x_detritus_d3")
	xR_detritus_s1		<- elt(initial.state, "xR_detritus_s1")
	xR_detritus_s2		<- elt(initial.state, "xR_detritus_s2")
	xR_detritus_s3		<- elt(initial.state, "xR_detritus_s3")
	xR_detritus_d1		<- elt(initial.state, "xR_detritus_d1")
	xR_detritus_d2		<- elt(initial.state, "xR_detritus_d2")
	xR_detritus_d3		<- elt(initial.state, "xR_detritus_d3")

	x_xR_detritus_s1	<- elt(physical.parms, "x_xR_detritus_s1")
	x_xR_detritus_s2	<- elt(physical.parms, "x_xR_detritus_s2")
	x_xR_detritus_s3	<- elt(physical.parms, "x_xR_detritus_s3")
	x_xR_detritus_d1	<- elt(physical.parms, "x_xR_detritus_d1")
	x_xR_detritus_d2	<- elt(physical.parms, "x_xR_detritus_d2")
	x_xR_detritus_d3	<- elt(physical.parms, "x_xR_detritus_d3")
	x_poros_s1		<- elt(physical.parms, "x_poros_s1")
	x_poros_s2		<- elt(physical.parms, "x_poros_s2")
	x_poros_s3		<- elt(physical.parms, "x_poros_s3")
	x_poros_d1		<- elt(physical.parms, "x_poros_d1")
	x_poros_d2		<- elt(physical.parms, "x_poros_d2")
	x_poros_d3		<- elt(physical.parms, "x_poros_d3")

	# Check whether the saved values of xR_detritus match the values from the configuration and if not inject them into the data

	if (x_xR_detritus_s1 < xR_detritus_s1 || x_xR_detritus_s1 > xR_detritus_s1) { initial.state$xR_detritus_s1 <-x_xR_detritus_s1 }
	if (x_xR_detritus_s2 < xR_detritus_s2 || x_xR_detritus_s2 > xR_detritus_s2) { initial.state$xR_detritus_s2 <-x_xR_detritus_s2 }
	if (x_xR_detritus_s3 < xR_detritus_s3 || x_xR_detritus_s3 > xR_detritus_s3) { initial.state$xR_detritus_s3 <-x_xR_detritus_s3 }

	if (x_xR_detritus_d1 < xR_detritus_d1 || x_xR_detritus_d1 > xR_detritus_d1) { initial.state$xR_detritus_d1 <-x_xR_detritus_d1 }
	if (x_xR_detritus_d2 < xR_detritus_d2 || x_xR_detritus_d2 > xR_detritus_d2) { initial.state$xR_detritus_d2 <-x_xR_detritus_d2 }
	if (x_xR_detritus_d3 < xR_detritus_d3 || x_xR_detritus_d3 > xR_detritus_d3) { initial.state$xR_detritus_d3 <-x_xR_detritus_d3 }

	if (x_poros_s1==0 && x_detritus_s1>0) { initial.state$x_detritus_s1 <- 0 }
	if (x_poros_s2==0 && x_detritus_s2>0) { initial.state$x_detritus_s2 <- 0 }
	if (x_poros_s3==0 && x_detritus_s3>0) { initial.state$x_detritus_s3 <- 0 }

	if (x_poros_d1==0 && x_detritus_d1>0) { initial.state$x_detritus_d1 <- 0 }
	if (x_poros_d2==0 && x_detritus_d2>0) { initial.state$x_detritus_d2 <- 0 }
	if (x_poros_d3==0 && x_detritus_d3>0) { initial.state$x_detritus_d3 <- 0 } 

	initial.state
}

