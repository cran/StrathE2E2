#
# e2e_get_senscrit.R
#
#' Get documentation on the model outputs available as the basis for sensitivity analysis.
#'
#' Provides access to documentation on outputs from the model which may be deployed as the basis for a sensitivity analysis using e2e_run_sens().
#'
#' The function e2e_run_sens() performs a Morris Method sensitivity analysis on the model. The default criterion for assessing the model sensitivity
#' is the likelihood of the observed target data set on the state of the ecosystem given each set of model drivers and parameters. 
#' However, a function argument allows other criteria to be chosen as the basis for the analysis from the list of annually averaged or integrated variables saved in the output objects:
#' \itemize{
#'   \item results$final.year.output$mass_results_wholedomain (whole-domain annual averages of stage variables over the final year of a model run), and 
#'   \item results$final.year.output$annual_flux_results_wholedomain (whole-domian annual integrals of fluxes between state variables over the final year of a model run).
#' }
#' The criterion is chosen by setting a value for the argument outID. The default outID=0 selects the likelihood of the observed target data. Other values in the range 1 to 247 select annualy averaged mass or 
#' annually integrated flux outputs. The function presented here provides a list of all the available outputs that may be used and their outID values.
#' 
#' @param id Single character (in "") or integer value denoting the class or id of output criterion to be downloaded. Choose single value from 0:247 = unique criterion, A = All (default), L = Likelihood, M = Annual average mass, F = Annual integrated fluxes.
#'
#' @return screen display and dataframe of parameter documentation.
#'
#' @seealso \code{\link{e2e_run_sens}}
#'
#' @export
#'
#' @examples
#'     crit_list <- e2e_get_senscrit("M")   # Get a list of annual average mass criteria      
#'     crit_list <- e2e_get_senscrit("F")   # Get a list of annual integrated flux criteria
#'     crit_list <- e2e_get_senscrit(24)    # Get details of criteria matching id=24
#'     crit_list <- e2e_get_senscrit()      # Get a list of all available criteria
#'
# ---------------------------------------------------------------------
# |                                                                   |
# | Author: Mike Heath                                                |
# | Department of Mathematics and Statistics                          |
# | University of Strathclyde, Glasgow                                |
# |                                                                   |
# | Date of this version: November 2020                               |
# |                                                                   |
# ---------------------------------------------------------------------

e2e_get_senscrit <- function(id="A") {

   oo <- options()
   on.exit(options(oo))


allowed.list <- c("L","M","F","A",seq(0,247))
if(id %in% allowed.list==FALSE){
	message("The argument 'id' must be one of the following...")
	message("    value 0 to 247 = specific model output")
	message("    A = all available model outputs (default)")
	message("    L = likelihood of observed target data")
	message("    M = annual average mass outputs")
	message("    F = annual integrated flux outputs")
	message("    unrecognised value entered - reverting to the default")
        id<-"A"
}

# mass_results_descriptions and annual_flux_descriptions are vectors available interannly via internal.R
mass_results_descriptions_x <- mass_results_descriptions[1:31]
annual_flux_descriptions_x  <- annual_flux_descriptions[1:216]

criterion_descriptions<-data.frame("Type"=c("Likelihood_test",rep("Annual_average_mass",31),rep("Annual_integrated_flux",216)),
                         "Description"=c("Likelihood",mass_results_descriptions_x,annual_flux_descriptions_x),
                         "outID"=seq(0,247))

if(id=="A") selected_class<-criterion_descriptions

if(id=="L") selected_class<-criterion_descriptions[1,]

if(id=="M") selected_class<-criterion_descriptions[2:32,]

if(id=="F") selected_class<-criterion_descriptions[33:248,]

if(id<248) selected_class<-criterion_descriptions[id+1,]

print(selected_class, row.names=FALSE)

return(selected_class)

}



