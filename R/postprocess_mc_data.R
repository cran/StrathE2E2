#
# postprocess_mc_data.R
#
#' Post-process raw Monte Carlo outputs from data saved in csv files
#'
#' Read CSV data files created by either the Monte Carlo run function e2e_run_mc() or the merging of several parallel runs by the function e2e_merge_outputs()
#' and process to generate credible interval files. Both the e2e_run_mc() and e2e_merge_outputs() functions permit post-processing of raw data as part of 
#' their own workflow, so this function will rarely be required.
#'
#' By default, the function writes output to CSV files, but this can be disabled by a function argument. The function always returns all of the processed data as
#' a list object. The function takes a few minutes to run as it has a lot of work to do, especially in processing al of the daily output variables.
#'
#' The model.ident identifier and folder path for input files must be set in a e2e_read() function call. If enabled, output files will be direceted to the same folder.
#'
#' @param model Model object for the data to be processed
#' @param csv.output Logical. If FALSE the disable writing of CSV output files - useful for testing (default=TRUE)
#'
#' @return List object of the processed data. If csv.output=TRUE then CSV files of the processed data. 
#'
#' @noRd
#
# ------------------------------------------------------------------------------

postprocess_mc_data <- function(model, csv.output=TRUE) {

	setup		<- elt(model, "setup")
	model.name	<- elt(setup, "model.name")
	model.variant	<- elt(setup, "model.variant")
	model.ident	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

	cred.path		<- makepath(resultsdir, CREDINT_DIR)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	read_cumfile<-function(intype="", credpath=cred.path, modelident=model.ident){

		cumfile<- csvname(credpath, intype, modelident)
		check.exists(cumfile)
		message("Reading ",intype," data file")
		cumdata<- readcsv(cumfile)

	return(cumdata)

	}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        parameterstore <- read_cumfile(intype="CredInt_cumulative_parameters")

	lastyearstore  <- read_cumfile(intype="CredInt_cumulative_lastyear")

	optresultsstore <- read_cumfile(intype="CredInt_cumulative_targetresults")

	monthlystore <- read_cumfile(intype="CredInt_cumulative_monthly")

	inshoreaamassstore <- read_cumfile(intype="CredInt_cumulative_inshoreaamass")

	offshoreaamassstore <- read_cumfile(intype="CredInt_cumulative_offshoreaamass")

	wholeaamassstore <- read_cumfile(intype="CredInt_cumulative_wholeaamass")

	inshoreannualfluxstore <- read_cumfile(intype="CredInt_cumulative_inshoreannualflux")

	offshoreannualfluxstore <- read_cumfile(intype="CredInt_cumulative_offshoreannualflux") 

	wholeannualfluxstore <- read_cumfile(intype="CredInt_cumulative_wholeannualflux")

	networkstore <- read_cumfile(intype="CredInt_cumulative_network")

#------------------------------------------------------------------------------------------

	#PROCESS THE RESULTS TO GET THE CREDIBLE INTERVAL DATA 

	message("Processing raw output to generate credible interval data.... ")

	#Now process the stored outputs to create the credible intervals for all of the model outputs. All are returned as part of a list object and saved to files if csv.output=TRUE
	# csv.output is set individually for each new function call
	# plotting of the cumulative likelihood curves is hard-turned-off within each of these functions. Plotting is useful for diagnostics but for production runs
	# its just a distraction slows down the process a little bit.

	message("Daily mass outputs ...")
	CI_daily <- CredInt_make_daily_results(model, lastyearstore, csv.output=csv.output)

	message("Daily flux outputs ...")
	CI_dflux <- CredInt_make_daily_flux_results(model, lastyearstore, csv.output=csv.output)

	message("Migration outputs ...")
	CI_migr  <- CredInt_make_daily_migration_results(model, lastyearstore, csv.output=csv.output)

	message("Monthly outputs ...")
	CI_month <- CredInt_make_monthly_results(model, monthlystore, csv.output=csv.output)

	message("Annual average mass outputs ...")
	CI_aamass<- CredInt_make_aamass_results(model, inshoreaamassstore, offshoreaamassstore, wholeaamassstore, csv.output=csv.output)

	message("Annual flux outputs ...")
	CI_aflux <- CredInt_make_annual_flux_results(model, inshoreannualfluxstore, offshoreannualfluxstore, wholeannualfluxstore, csv.output=csv.output)

	message("Network index outputs ...")
	CI_netw  <- CredInt_make_network_results(model, networkstore, csv.output=csv.output)

	message("Target data outputs ...")
	CI_targ  <- CredInt_make_target_results(model, optresultsstore, csv.output=csv.output)

	message("Parameter value outputs ...")
	CI_parms <- CredInt_make_parameter_results(model, parameterstore, csv.output=csv.output)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	output_data <- list(parameter_record = parameterstore,
			    CI_daily_results = CI_daily,
			    CI_monthly_results = CI_month,
			    CI_annual_avmass = CI_aamass,
			    CI_daily_fluxes = CI_dflux,
			    CI_annual_fluxes = CI_aflux,
			    CI_migration_fluxes = CI_migr,
			    CI_network_indices = CI_netw,
			    CI_target_data  = CI_targ,
			    CI_parameter_values = CI_parms)

	message("Processing completed")


	return(output_data)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
