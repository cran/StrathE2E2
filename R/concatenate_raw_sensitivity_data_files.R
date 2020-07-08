#
# concatenate_raw_sensitivity_data_files.R
#
#' Combine two or more sets of raw output data from sensitivity analysis runs performed on separate machine/processors.
#'
#' The function e2e_run_sens() is extremely time consuming so it makes sense to share the 
#' load across multiple processors in parallel and combine the results afterwards. This function concatenates the raw outputs from multiple separate runs
#' of e2e_run_sens() into a single file. This is not as simple as merely concatenating the files as it is necessary to keep track of 
#' the unique identities of the sets of trajectories.
#'
#' e2e_run_sens() generates two output files per run - OAT_results-*.csv, and OAT_parameter_values-*.csv, where * is
#' an identifying text string set by an argument of the e2e_read() function. This concatenation function combines both of these types of files.
#'
#' The files to be combined must be transferred into the same folder, and this is where the new combined file will be placed.
#' The path to locate the files is set in a e2e_read() function call. If not specified it is assumed that the files are located in the
#' default /results/Modelname/Variantname folder in the current user workspace.
#'
#' An identifying text string for the new combined files is set by the 'model.ident' argument in a e2e_read() function call.
#'
#' The list of files to be combined (any number > 1) is defined by a vector of their individual "model.ident" identifiers ("ident.list" argument). 
#' The first-named model.ident in the vector MUST correspond to a run of the e2e_run_sens() function with the argument coldstart=TRUE. This forces the
#' first trajectory of sensitivity test to be performed on the maximum-likelihood parameter set loaded with the e2e_read() function. Thus is important
#' for the post-processing stage of the analysis which needs to be performed on the combined results.
#' 
#' When combining the files, the function creates a seamless sequence of trajectory identifiers through the combined data, beginning from 1 for the first (maximum likelihood) trajectory
#' of the first set.
#'
#' If for any reason there is a need to combine separate batches of multiple e2e_run_sens() run results, then 
#' post-processing can be delayed with the 'postprocess' argument until the last concatenation when all the data have been gathered
#' together. Stand-alone postprocess can be performed using the function e2e_process_sens().
#'
#' A separate function e2e_plot_sens() produces a graphical representation of the post-processed results of the combined data.
#'
#' @param model Model object for the raw data to be combined
#' @param ident.list A vector of text variables corresponding to the "model.ident" identifiers for each of the files to concatenated (list must be length 2 or greater)
#' @param postprocess (TRUE or FALSE, default=TRUE) Process the results through to a final sorted list of parameter sensitivies for plotting, or just produce the combined raw results. The reason for NOT processing would be if there are further run results stil to be combined with the set produced by this function.
#'
#' @return csv files of parameter vectors for each model run, likelihoods and Elementary Effects for each run. If the argument postprocess=TRUE then also a dtaframe and CSV file of the parameter list sorted by EE_mean, from the combination of multiple input data sets
#'
#' @noRd
#
# ------------------------------------------------------------------------------

concatenate_raw_sensitivity_data_files <- function(model, ident.list, postprocess=TRUE) {

	setup		<- elt(model, "setup")
	combined.ident	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

        Nfiles<-length(ident.list)

	if(Nfiles<2) {
		print("Less than 2 input files in the ident.list so function terminated")
		stop()
	}

	for(qq in 1:Nfiles) {

	sensfile<- csvname(resultsdir, "OAT_results", ident.list[qq])
	parmfile<- csvname(resultsdir, "OAT_parameter_values", ident.list[qq])
	print(paste("Reading input data for model.ident = ",ident.list[qq],sep=""))
	results_df_out<- readcsv(sensfile)
	OAT_parmvalues<- readcsv(parmfile)

	if(qq==1){
		results_df_out_comb <- results_df_out
		OAT_parmvalues_comb<-OAT_parmvalues
		NPARMST<-(nrow(results_df_out[which(results_df_out$trajectoryid==1),]))-1
		NTRAJT<-(nrow(results_df_out))/(NPARMST+1)
	}
		

	if(qq>1){
		NPARMS<-(nrow(results_df_out[which(results_df_out$trajectoryid==1),]))-1
			if((NPARMST==NPARMS)==FALSE){
				print("Error: mismatch in parameter count between input files")
				stop()
			}
		NTRAJ<-(nrow(results_df_out))/(NPARMS+1)
	
		results_df_out$trajectoryid <- results_df_out$trajectoryid + NTRAJT
		NTRAJT<-NTRAJT+NTRAJ
		results_df_out_comb <- rbind(results_df_out_comb, results_df_out)
		OAT_parmvalues_comb <- rbind(OAT_parmvalues_comb, OAT_parmvalues)
	}

	}

	filename <- csvname(resultsdir, "OAT_results", combined.ident)
	writecsv(results_df_out_comb, filename, row.names=FALSE)

	filename <- csvname(resultsdir, "OAT_parameter_values", combined.ident)
	writecsv(OAT_parmvalues_comb, filename, row.names=FALSE)


	#PROCESS THE FINAL RESULTS TO GET THE SORTED PARAMETER SENSITIVITY FILE IF REQUIRED
	Sorted_SENS_results<-NULL
	if(postprocess==TRUE){
	        model$setup$model.ident <- combined.ident
		Sorted_SENS_results<-process_sensitivity_analysis_results(model, results_df_out_comb)
        }

	return(Sorted_SENS_results)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
