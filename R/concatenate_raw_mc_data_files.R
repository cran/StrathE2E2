#
# concatenate_raw_mc_data_files.R
#
#' Combine two or more sets of raw output data from Monte-Carlo runs performed on separate machine/processors.
#'
#' The function e2e_run_mc() is extremely time consuming so it makes sense to share the 
#' load across multiple processors in parallel and combine the results afterwards. This function concatenates the raw outputs from multiple separate runs
#' of e2e_run_mc() into a single file. This is not as simple as merely concatenating the files as it is necessary to keep track of 
#' the unique identities of the sets of samples, and discard the baseline run for all but one of the files to be combined.
#'
#' e2e_run_mc() generates 11 different output files per run each with 
#' an identifying text string set by an argument of the e2e_read() function. This concatenation function combines batches of each of these types of files.
#'
#' The files to be combined must be transferred into the same folder, and this is where the new combined file will be placed.
#' The path to locate the files is set in a e2e_read() function call. If not specified it is assumed that the files are located in the
#' default /results/Modelname/Variantname/CredInt folder in the current user workspace.
#'
#' An identifying text string for the new combined files is set by the 'model.ident' argument in a e2e_read() function call.
#'
#' The list of files to be combined (any number > 1) is defined by a vector of their individual "model.ident" identifiers ("ident.list" argument). 
#' The baseline model run from the first-named model.ident in the vector will be preserved in the combined datasets. The baseline will be deleted from all subsequent moded.ident's in the list. 
#' 
#' When combining the files, the function creates a seamless sequence of iteration identifiers through the combined data, beginning from 1 for the first (baseline) run
#' of the first set.
#'
#' If for any reason there is a need to combine separate batches of multiple e2e_run_mc() run results, then 
#' post-processing can be delayed with the 'postprocess' argument until the last concatenation when all the data have been gathered
#' together. Stand-alone postprocess can be performed by entering just a single model.ident in the list for this function. This wil force processing without any merging.
#'
#' @param model R-list object defining the location and model.ident for the combined data, compiled by the e2e_read() function.
#' @param ident.list A vector of text variables corresponding to the "model.ident" identifiers for each of the files to concatenated (list must be length 1 or greater).
#' @param postprocess Logical. If TRUE process the results through to final data. If FALSE just produce the combined raw results. The reason for NOT processing would be if there are further run results still to be combined with the set produced by this function. Defaut=TRUE.
#' @param csv.output Logical. If TRUE write output to csv files. If FALSE then csv output disabled - useful for testing.
#'
#' @return Depends on argument settings postprocess and csv.output. If csv.output=TRUE but postprocess=FALSE then CSV files of the merged raw data files, and returns a dataframe of the merged raw parameter values use din the combined Monte-Carlo runs. If postprocess=TRUE, then CSV files of teh combined raw data plus the processed data, and the return data object is a list including the raw parameter data and the processed data.
#'
#' @noRd
#
# ------------------------------------------------------------------------------

concatenate_raw_mc_data_files <- function(model, ident.list, postprocess=TRUE, csv.output=TRUE) {

	setup		<- elt(model, "setup")
	combined.ident	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

	credpath		<- makepath(resultsdir, CREDINT_DIR)

        Nfiles<-length(ident.list)

	if(Nfiles==1) {
		message("Only one input files in ident.list ... ")
			if(postprocess==TRUE){
				message("     ... so processing the data from just one file")
			}
			if(postprocess==FALSE){
				message("     ... and postprocess=FALSE, so nothing to do")
				stop()
			}
	}

	if(Nfiles==0) {
		message("No input files in ident.list so terminate... ")
		stop()
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	read_and_merge<-function(intype="",cred.path=credpath,id.list=ident.list,nf=Nfiles,out.ident=combined.ident){

#intype="CredInt_cumulative_parameters"
#cred.path=credpath
#id.list=ident.list
#nf=Nfiles
#out.ident=combined.ident



	NIT<-0
	mergefile<- csvname(cred.path, intype, out.ident)
	message("Reading ",intype," data files")
	for(qq in 1:nf) {

	cumfile<- csvname(cred.path, intype, id.list[qq])
	cumdata<- readcsv(cumfile)

	if(qq==1){
		mergecumdata <- cumdata
		NIT<-max(mergecumdata$iteration)
		rm(cumdata)
	}
		
	if(qq>1){
		subcumdata<-subset(cumdata,cumdata$iteration>1)          # Discard the first run as this is a replicate baseline
		rm(cumdata)
		subcumdata$iteration <- (subcumdata$iteration -1) + NIT   # Renumber the iterations
		mergecumdata<-rbind(mergecumdata,subcumdata)              # Concatenate the data objects
		rm(subcumdata)
		NIT<-max(mergecumdata$iteration)
	}

	}

	if(nf>1 & csv.output==TRUE){
		writecsv(mergecumdata,	csvname(cred.path, intype,out.ident),	row.names=FALSE)
	}

	return(mergecumdata)

	}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        parameterstore <- read_and_merge(intype="CredInt_cumulative_parameters")

	lastyearstore  <- read_and_merge(intype="CredInt_cumulative_lastyear")

	optresultsstore <- read_and_merge(intype="CredInt_cumulative_targetresults")

	monthlystore <- read_and_merge(intype="CredInt_cumulative_monthly")

	inshoreaamassstore <- read_and_merge(intype="CredInt_cumulative_inshoreaamass")

	offshoreaamassstore <- read_and_merge(intype="CredInt_cumulative_offshoreaamass")

	wholeaamassstore <- read_and_merge(intype="CredInt_cumulative_wholeaamass")

	inshoreannualfluxstore <- read_and_merge(intype="CredInt_cumulative_inshoreannualflux")

	offshoreannualfluxstore <- read_and_merge(intype="CredInt_cumulative_offshoreannualflux") 

	wholeannualfluxstore <- read_and_merge(intype="CredInt_cumulative_wholeannualflux")

	networkstore <- read_and_merge(intype="CredInt_cumulative_network")

#------------------------------------------------------------------------------------------

	#PROCESS THE FINAL RESULTS TO GET THE CREDIBLE INTERVAL DATA IF REQUIRED

	if(postprocess==TRUE){

	message("Processing raw output to generate credible interval data.... ")

	#Now process the stored outputs to create the credible intervals for all of the model outputs. All are returned as part of a list object and saved to files if csv.output=TRUE
	# csv.output is set individually for each new function call
	# plotting of the cmulative likelihood curves is hard-turned-off within each of these functions. Plotting is useful for diagnostics but for production runs
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

	}  # end of if postprocess=TRUE statement

	if(postprocess==FALSE){
	output_data <- NULL
	}

	return(output_data)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
