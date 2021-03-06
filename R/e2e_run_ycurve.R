#
# e2e_run_ycurve.R
#
#' Run a set of models to generate fishery yield curve data for either planktivorous or demersal fish.
#'
#' Perform a set of StrathE2E model runs along a sequence of values of either planktivorous or demersal fish harvest ratio multiplier, saving the annual average whole-domain biomass, annual landings and annual discards of all exploitable food web guilds from each run 
#' plus the annual average biomasses of all the other living components of the food web. 
#' 
#' The baseline for the sequence of runs (harvest ratio multiplier = 1.0) is a model name and variant as loaded by the e2e_read() function.
#'
#' The planktivorous or demersal fish yield curve can be generated for a given fixed setting of the other (demersal or planktivorous) fish harvest ratio multiplier (default = 1.0). All other
#' conditions are held constant as in the baseline model configuration.
#'
#' The yield curve represents the catch that would be generated from the stationary state of the model attained with long-term repeating annual cycles of all driving data. Hence it is important that each
#' simulation is run for long enough that the model attains its stationary state, which may be some distance from the baseline model initial conditions.
#' It is recommended that each run is at least 50 years.
#'
#' The data on annual average biomass and annual integrated catches stored in the returned data object (and optionally a csv file) can subsequently be plotted using the function e2e_plot_ycurve(). Users can easily plot any of the other 
#' saved data using their own plotting code.
#'
#' If csv output is selected then the resulting files in the current user results folder have names Yield_curve_data_PFHRmult-*.csv or Yield_curve_data_DFHRmult-*.csv, depending on the 'selection' argument, and * represents the model.ident text set in the prior e2e_read() function call.
#'
#' @param model R-list object defining a baseline model and configuration compiled by the e2e_read() function.
#' @param selection Text string from a list identifying the fish guild for which a yield curve is to be generated. Select from: "PLANKTIV", "DEMERSAL", Remember to include the phrase within "" quotes.
#' @param nyears Number of years to run the StrathE2E model at each level of harvest ratio. Default = 50.
#' @param HRvector A vector of ascending order unique multiplier values to be applied to the baseline fish harvest ratio of the guild to be analysed. The values do not have to be evenly spaced (default = c(0,0.5,1.0,1.5,2.0,2.5,3.0)).
#' @param HRfixed Single value of the multiplier to be applied as the alternative (planktivorous or demersal) fish harvest ratio for all runs (default=1.0).
#' @param csv.output Logical. If TRUE then enable writing of csv output files (default=FALSE).
#'
#' @return Dataframe of annual average biomss, annual landings and annual discards of all exploitable guilds, plus average biomasses of other food web components, for each level of harvest ratio multiplier.
#'
#' @seealso \code{\link{e2e_read}}, \code{\link{e2e_run}}, \code{\link{e2e_plot_ycurve}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Load the 1970-1999 version of the North Sea model supplied with the package :
#'     model <- e2e_read("North_Sea", "1970-1999", model.ident="70-99base")
#' # In this example csv output is directed to a temporary folder since results.path os not set.
#'
#' # In this illustrative example the StrathE2E() model is run for only 3 years to enable quick
#' # return of results. In a real simulation nyear would be at least 50.
#' # This example illustrates that the vector of planktivorous fish harvest ratio multiplers
#' # does not have to be evenly spaced.
#'     hr <- c(0,0.5,0.75,1.0,1.25,2.0,3.0) 
#'     pf_yield_data <- e2e_run_ycurve(model,selection="PLANKTIV", nyears=3, HRvector=hr,
#'                                     HRfixed=1,csv.output=FALSE)
#'
#' # View the column names of the results dataframe:
#'     names(pf_yield_data)
#'
#' # Plotting the results...
#' # The planktivorous fish yield curve can be plotted using the function:
#'     e2e_plot_ycurve(model, selection="PLANKTIV", results=pf_yield_data,
#'                     title="Planktivorous yield with baseline demersal fishing")
#' }
#
# ---------------------------------------------------------------------
# |                                                                   |
# | Authors: Mike Heath                                               |
# | Department of Mathematics and Statistics                          |
# | University of Strathclyde, Glasgow                                |
# |                                                                   |
# | Date of this version: May 2020                                    |
# |                                                                   |
# ---------------------------------------------------------------------
#

e2e_run_ycurve <- function(model, selection, nyears=50, HRvector=c(0,0.5,1.0,1.5,2.0,2.5,3.0), HRfixed=1, csv.output=FALSE ) {

   oo <- options()
   on.exit(options(oo))

yield_data<-NULL

if(selection=="PLANKTIV"){
	yield_data <- generate_pf_yield_curve_data(model=model,nyears=nyears,PFHRvector=HRvector,DFHR=HRfixed,csv.output=csv.output)

} else if(selection=="DEMERSAL"){
	yield_data <- generate_df_yield_curve_data(model=model,nyears=nyears,DFHRvector=HRvector,PFHR=HRfixed,csv.output=csv.output )

} else {
	stop("Error: unknown selection '", selection, "' !\n")
}

return(yield_data)

}

#-----------------------------------------------------------------


