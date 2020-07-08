#
# extract_final_year_of_run_plus_volumetric_data.R
#
#' extract the final year of model output and add columns of the volumetric data needed to process the results
#'
#' @param model current model configuration
#' @param results model results object
#'
#' @return final year of model output
#'
#' @noRd
#
# ------------------------------------------------------------------------------

extract_final_year_of_run_plus_volumetric_data <- function(model, results) {

	resultsdir	<- elt(model, "setup", "resultsdir")
	identifier	<- elt(model, "setup", "model.ident")
	physical.parms  <- elt(model, "data", "physical.parameters")

	out		<- elt(results, "output")
	aggregates	<- elt(results, "aggregates")
	nyears		<- elt(results, "build", "run", "nyears")
	ndays		<- elt(results, "build", "run", "ndays")

	# build up final year data:
	agg		<- as.data.frame(aggregates)
	lastyear1	<- out[(((nyears-1)*360+1):ndays),]
	lastyear2	<- agg[(((nyears-1)*360+1):ndays),]
	lastyear	<- as.data.frame(c(lastyear1, lastyear2))

	#lastyear	<- out[(((nyears-1)*360+1):ndays),]

	# need to add last year of aggregates here!

	lastyear$time		<- seq(0,360,by=1)

	lastyear$shallowprop	<- elt(physical.parms, "x_shallowprop")
	lastyear$depth_si	<- elt(physical.parms, "si_depth")
	lastyear$depth_so	<- elt(physical.parms, "so_depth")
	lastyear$depth_d	<- elt(physical.parms, "d_depth")

	lastyear$area_s0	<- elt(physical.parms, "x_area_s0")
	lastyear$area_s1	<- elt(physical.parms, "x_area_s1")
	lastyear$area_s2	<- elt(physical.parms, "x_area_s2")
	lastyear$area_s3	<- elt(physical.parms, "x_area_s3")
	lastyear$area_d0	<- elt(physical.parms, "x_area_d0")
	lastyear$area_d1	<- elt(physical.parms, "x_area_d1")
	lastyear$area_d2	<- elt(physical.parms, "x_area_d2")
	lastyear$area_d3	<- elt(physical.parms, "x_area_d3")

	lastyear$depth_s1	<- elt(physical.parms, "x_depth_s1")
	lastyear$depth_s2	<- elt(physical.parms, "x_depth_s2")
	lastyear$depth_s3	<- elt(physical.parms, "x_depth_s3")
	lastyear$depth_d1	<- elt(physical.parms, "x_depth_d1")
	lastyear$depth_d2	<- elt(physical.parms, "x_depth_d2")
	lastyear$depth_d3	<- elt(physical.parms, "x_depth_d3")

	lastyear$poros_s1	<- elt(physical.parms, "x_poros_s1")
	lastyear$poros_s2	<- elt(physical.parms, "x_poros_s2")
	lastyear$poros_s3	<- elt(physical.parms, "x_poros_s3")
	lastyear$poros_d1	<- elt(physical.parms, "x_poros_d1")
	lastyear$poros_d2	<- elt(physical.parms, "x_poros_d2")
	lastyear$poros_d3	<- elt(physical.parms, "x_poros_d3")


	#Print the data to a csv file
	#-----------------------------------------------------------------
	csvfile <- csvname(resultsdir, "model_final_year_results", identifier)
	writecsv(lastyear, csvfile)

	lastyear
}


