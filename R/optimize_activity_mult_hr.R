#
# optimize_activity_mult_hr.R
#
#' Optimise fishing gear activity multipliers to maximum the likelihood of known harvest ratios.
#'
#' Launches a simulated annealing process to find the set of fishing fleet model gear activity multipliers
#' producing the maximum likelihood of a set of known harvest ratios using only the fishing fleet model,
#' given specified effort-harvest ratio scaling parameters.
#' Note that there is a related function optimize_activity_mult_eco() which uses the ecology model to
#' find the activity multipliers which maximise the likelihood of observed target data on the state of the ecosytem, given values of the
#' ecology model parameters and the scaling values linking effort to harvest ratio.
#'
#' The known harvest ratios which act as the target for the optimization process are located in the file
#' ../Modelname/Variantname/Target/zonal_harvest_r_*.csv, where * is an identifier.
#'
#' Simulated annealing is an iterative random-walk type process which searches the parameter space
#' of a model to locate the combination which maximises the likelihood of a set of observed
#' data corresponding to a suite of derived outputs. Parameter combinations which result in an improved likelihood
#' may be rejected according to a probability ('temperature') which decreases as the iterations progress. This is to  
#' avoid becoming stuck at local likelihood-maxima. The rate at which the 'temperature' decreases is set
#' by a 'cooling' parameter (fraction of previous temperature at each iteration, 0<value<1). The task here is a bit harder than normal
#' because of the potentially large overlap in the selectivity patterns of the fishing gears with respect to the living guilds in the ecosystem. So there are
#' some non-standard features to try and avoid local maxima.
#'
#' Model configuration and initial values of the ecology model parameters need to be
#' assembled by a prior call of the e2e_read() function.
#' 
#' NOTE that the models.path argument in the e2e_read() function call needs to point to a user workspace folder, not the default
#' North Sea model provided with the package. This is because the annealing function needs write-access to the model /Param folder,
#' but the /extdata/Models folder in the package installation is read-only.
#' To use the annealing function on the North Sea model, use the e2e_copy() function to make a copy of the
#' North Sea model in the user workspace.
#'
#' The initial coefficient of variation for searching the parameter space is set by a function argument. The CV decreases in steps with increasing iterations.
#' The function repeats the iteration process multiple times. The repeats are referred to as 'trajectories'. Each trajectory follows a different pathway through the parameter space.
#' At the end of the process, the best-fit set of activity multipliers is selected from across all the trajectories.
#'
#' To cope with the overlap in selectivity of the fishing gears, the function uses a set of linkage parameters specified in the the file /Param/fishing_gear_linkages.csv
#' These parameters force selected gears activities to vary in concert +/- some variation, rather than independently. The parameters for the gear linkages are located in the file
#' ../Modelname/Variantname/Param/fishing_gear_linkages.csv. The table of linkages specifies which gear activity rates are forced to 
#' vary in concert during the fitting process, as opposed to varying independently. The value of the linkage coefficient defines the 
#' scaling of changes in the activity rate of a dependent gear relative to its linked independent gear. For example, if gear 8 is permitted to vary 
#' independently (value in column "Gear to which linked" = NA and "Linkage coefficient" = NA). If gear 9 is dependent on gear 8 then the activity rate 
#' of gear 9 this would be specified by e.g. "Gear to which linked" = 8 and "Linkage coefficient" = 0.645. This would force the activity of gear 9 to be 
#' set at gear8_activity * (0.645 +/- a random variation defined by the argument deltaG). 
#'
#' The function produces a real-time graphical summary of the progress of the fitting procedure, displaying
#' the likelihoods of the proposed and accepted parameter sets at each iteration.
#' Y-axis (likelihood of the target data) range of the real time plot can be varied during the run by 
#' editing the setup file "optimize_fishing.csv"
#'
#' At the end of the procedure a new version of the gear activity multipliers file is exported
#' to the folder /Param of the model version, with a user defined identifier specified by the model.ident argument
#' in the e2e_read() function. The histories of proposed and accepted parameter combinations
#' are saved as csv files in the results folder.
#'
#' To preserve the new activity multipliers and incorporate them into the fishing fleet model parameterisation
#' the multiplier values need to be applied to the activity rates specified in the data input file /Param/fishing_activity_*.csv.
#' Manually update the values in fishing_activity_*.csv, by multiplying the existing values by the new multipliers emerging from the annealing process.
#'
#' If the edited file fishing_activity_*.csv is saved with a new identifier (*) then in order to use it in a subsequent run of the
#' StrathE2E model (using the e2e_run() function) it will be necessary to edit the MODEL_SETUP.csv file in the relevant /Models/variant folder to point to the new file.
#'
#' @param model R-list object generated by the e2e_read() function which defined the model configuration.
#' @param n_traj Number of repeats (trajectories) of the simulated annealing process (default=100).
#' @param n_iter Number of iterations of the fishing fleet model per trajectory (default=3000).
#' @param start_temperature Initial value of the simulated annealing temperature parameter (default=0.5). Suggested values in the range 0.0005 - 5. Higher values increase the probability of rejecting parameter combinations producing an improvement in likelihood.
#' @param cooling Rate at which the simulated annealing temperature declines with iterations (default=0.985). Suggested values in the range 0.9 - 0.985
#' @param deltaHi Initial coefficient of variation for jiggling the activity multiplier values (default=0.2).
#' @param attenuationstep Number of itereations between down-steps of the jiggling factor applied to the multiplier values. The jiggling rate is attenuated by a factor of 2 every xx iterations (default=500).
#' @param deltaG Coefficient of variation for jiggling the gear linkage values (default=0.25).
#' @param csv.output Logical. If FALSE then disables writing of csv output files - useful for testing, (default=TRUE)
#' @param runtime.plot Logical. If FALSE then disables runtime plotting of the progress of the run - useful for testing, (default=TRUE).
#'
#' @return A list object containing fitted gear activity multipliers, statistics of the likelihood weighted distributions of activity multipliers and the fit to harvest ratios, and the final accepted parameter values. Optionally (by default), CSV files of these data and the final accepted parameter values. The latter are returned to the model parameter folder in a format to be read back into the model.
#'
#' @importFrom stats runif
#' @importFrom grDevices dev.off
#'
#' @noRd
#
# ------------------------------------------------------------------------------

optimize_activity_mult_hr <- function(model, n_traj=100, n_iter=3000, start_temperature=0.5, cooling=0.985, deltaHi=0.20, attenuationstep=500, deltaG=0.25, csv.output=TRUE, runtime.plot=TRUE) {

	if(runtime.plot==TRUE){
	start_par = par()$mfrow
	dev.off()
	on.exit(par(mfrow = start_par))
	}

	pkg.env$csv.output <- csv.output	# controls writing of CSV files

	setup				<- elt(model, "setup")
	read.only			<- elt(setup, "read.only")
	model.path			<- elt(model, "setup", "model.path")
	model.ident			<- elt(model, "setup", "model.ident")
	resultsdir			<- elt(model, "setup", "resultsdir")

	data				<- elt(model, "data")
	fleet.model			<- elt(data, "fleet.model")
	gear_codes			<- elt(fleet.model, "gear_codes")
	gear_mult			<- elt(fleet.model, "gear_mult")
	gear_activity			<- elt(fleet.model, "gear_activity")
	gear_group_rel_power		<- elt(fleet.model, "gear_group_rel_power")
	HRscale_vector_multiplier	<- elt(fleet.model, "HRscale_vector_multiplier")

	if (read.only & csv.output==TRUE) {
		message("Warning: cannot write fitted parameters back to the model input folders - model is read-only")
		message("Warning: to fix this, make a copy of the model using e2e_copy() into your own workspace.")
		stop("Model is not writable!")
	}

	print(date())


	if(length(which(HRscale_vector_multiplier<1|HRscale_vector_multiplier>1))>0) {
		message("**************************************************************************")
		message("WARNING - one or more baseline Harvest Ratio scaling values differs from 1")
		message("**************************************************************************")
	}

	#End of model setup

	#Read the gear linkages and target data
	gear_linkages	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FISHING_FLEET_GEAR_LINKAGES)
	target_HRs	<- get.model.file(model.path, TARGET_DATA_DIR, file.pattern=FISHING_FLEET_TARGET_HR)

	fleet_vector_target<-c(
		target_HRs[1,2],target_HRs[1,3],
		target_HRs[2,2],target_HRs[2,3],
		target_HRs[3,2],target_HRs[3,3],
		target_HRs[4,2],target_HRs[4,3],
		target_HRs[5,2],target_HRs[5,3],
		target_HRs[6,2],target_HRs[6,3],
		target_HRs[7,2],target_HRs[7,3],
		target_HRs[8,2],target_HRs[8,3],
		target_HRs[9,2],target_HRs[9,3],
		target_HRs[10,2],target_HRs[10,3]
	)

	ignoredata<-which(target_HRs[,4]==0)
	if(length(ignoredata)>0){
		for(jjjj in 1:length(ignoredata)){
			if(jjjj==1) ignorevector<-c( (((ignoredata[jjjj]-1)*2)+1),(((ignoredata[jjjj]-1)*2)+2) )
			if(jjjj>1) ignorevector<-c(ignorevector, (((ignoredata[jjjj]-1)*2)+1),(((ignoredata[jjjj]-1)*2)+2) )
		}
		fleet_vector_target[ignorevector]<-NA
	}

	MAINstore<-data.frame(G1=0,G2=0,G3=0,G4=0,G5=0,G6=0,G7=0,G8=0,G9=0,G10=0,G11=0,G12=0,lik=0)
		colnames(MAINstore)<-c(as.character(gear_codes),"lik")

	HRstore<-data.frame(Plank.fish_i=0,Plank.fish_o=0,
                            Dem.fish_i=0,Dem.fish_o=0,
                            Mig.fish_i=0,Mig.fish_o=0,
			    Benth.s.d_i=0,Benth.s.d_o=0,
			    Benth.c.s_i=0,Benth.c.s_o=0,
			    Carn.zoo_i=0,Carn.zoo_o=0,
			    Birds_i=0,Birds_o=0,
			    Pinnipeds_i=0,Pinnipeds_o=0,
			    Cetaceans_i=0,Cetaceans_o=0,
			    Macrophytes_i=0,Macrophytes_o=0,lik=0)

	global_best<-0

	gear_mult_initial <- gear_mult

	for(traj in 1:n_traj){

		temperature<-start_temperature 

		gear_mult_START<-gear_mult_initial

		#Set up a dataframe to hold the proposed and accepted gear_multipliers

		GEARmult_proposal<-data.frame(G1=0,G2=0,G3=0,G4=0,G5=0,G6=0,G7=0,G8=0,G9=0,G10=0,G11=0,G12=0,lik=0)
		GEARmult_proposal[1,1:12]<-gear_mult_START
		GEARmult_accepted<-GEARmult_proposal

		acceptedstore_annual_obj<-1e-60
		proposalstore_annual_obj<-1e-60

		n_acceptances<-0   # Counter for the number of parameter acceptances which have occurred

		#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		#ITERATE THE ANNEALING PROCESS.....

		lastbest<-1e-60
		for (kkk in 1:n_iter){

			if(kkk>0) deltaH<-deltaHi
			if(kkk>(attenuationstep)) deltaH<-deltaHi/2
			if(kkk>(attenuationstep*2)) deltaH<-deltaHi/4
			if(kkk>(attenuationstep*3)) deltaH<-deltaHi/8
			if(kkk>(attenuationstep*4)) deltaH<-deltaHi/16
			if(kkk>(attenuationstep*5)) deltaH<-deltaHi/32
			if(kkk>(attenuationstep*6)) deltaH<-deltaHi/64
			if(kkk>(attenuationstep*7)) deltaH<-deltaHi/128

			#FIRST RUN THROUGH THE MODEL WITH THE INITIAL GEAR ACTIVITY RATES.....

			if(kkk>1){

				#Jiggle all of the last accepted gear mults...
				for(www in 1:12){ 
     					gear_mult[www] <- max(0,rnorm(1,GEARmult_accepted[(kkk-1),www],deltaH*GEARmult_accepted[(kkk-1),www]))
				}

				#Now skim through and over-ride the assigned mult values for gears that are linked to another...
				for(www in 1:12){
					if(is.na(gear_linkages[www,3])==FALSE){
						link_gear<-gear_linkages[www,3]
						link_coef<-gear_linkages[www,4]
						gear_mult[www]<-gear_mult[link_gear] * max(0,rnorm(1,link_coef,deltaG*link_coef))
					}
				}

				GEARmult_proposal[kkk,]<-c(gear_mult,0)
				GEARmult_accepted[kkk,]<-GEARmult_accepted[(kkk-1),]
			}    

			model$data$fleet.model$gear_mult <- gear_mult

			build <- build_model(model)

			#Grab the vector of parameter values that needs to be included in the ecology model input
			fleet_vector <- elt(build, "fleet.output", "fleet_vector")
			fleet_vector_proposal <- fleet_vector[1:20]

			#Now calculate a likelihood for the target HRs

			HRtol<-0.2*fleet_vector_target

			chi <- ((fleet_vector_target-fleet_vector_proposal)^2) / (2*(HRtol^2))

			validtarg<-length(which(is.na(fleet_vector_target)==FALSE))
			p <- exp(-1*( (sum(chi[1:20],na.rm=TRUE))/validtarg) )  

			proposalstore_annual_obj<-c(proposalstore_annual_obj,p)
			acceptedstore_annual_obj<-c(acceptedstore_annual_obj,acceptedstore_annual_obj[length(acceptedstore_annual_obj)])
		        GEARmult_proposal[kkk,13]<-p

			#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

			temperature<-temperature*cooling

			#-------------------------------------------------------------------
			#Now the Metropolis algorithm.....

			lik_ratio<-exp(((log(p)) - log(lastbest))/temperature)

			rand<-runif(1,0,1)

			#--------------------------------

			if(lik_ratio>rand){
				n_acceptances<-n_acceptances+1
				GEARmult_accepted[kkk,]<-GEARmult_proposal[(kkk),]
				fleet_vector_accepted<-fleet_vector_proposal
				lastbest<-p
			}

#----------------------------------------------------------------------------------------------------------

			#Plot or update the time series of proposed and accepted likelihoods so far....
			if(runtime.plot==TRUE){
			if(kkk>1){

                                stp<-c((kkk-1),kkk)

				if(kkk==2){
				axmin<-0              # Axis range hard-wired here - no need to be real-time adjustable
				axmax<-1.15
				par(mfrow=c(1,1))
#				plot(seq(1,nrow(GEARmult_proposal)),GEARmult_proposal[,13],ylim=c(axmin,axmax),xlim=c(1,n_iter),xlab="Iterations",ylab="Target data likelihood",type="l",col="grey",main=(paste("Trajectory ",traj,sep="")))
				plot(stp,GEARmult_proposal[stp,13],ylim=c(axmin,axmax),xlim=c(1,n_iter),xlab="Iterations",ylab="Target data likelihood",type="l",col="grey",main=(paste("Trajectory ",traj,sep="")))
                		legend("topleft",c("accepted","proposed"),bg="transparent",col=c("black","grey"),lty=c(1,1),lwd=c(3,1),pt.cex=c(1,1))
				abline(h=1,lty="dashed")
				}

				if(kkk>2){
#				points(seq(1,nrow(GEARmult_proposal)),GEARmult_proposal[,13],type="l",col="grey")
				points(stp,GEARmult_proposal[stp,13],type="l",col="grey")
				} 

#				points(seq(1,nrow(GEARmult_accepted)),GEARmult_accepted[,13],type="l",col="black",lwd=3)
				points(stp,GEARmult_accepted[stp,13],type="l",col="black",lwd=3)

			}
			}


		}

#----------------------------------------------------------------------------------------------------------


		MAINstore[traj,]<-as.numeric(GEARmult_accepted[nrow(GEARmult_accepted),])
		HRstore[traj,]<-c(fleet_vector_accepted, GEARmult_accepted[(nrow(GEARmult_accepted)),13] )

		lasttrajlik<-GEARmult_accepted[(nrow(GEARmult_accepted)),13]
		if(lasttrajlik>global_best) global_best<-lasttrajlik
		message("Trajectory: ",traj," of ",n_traj,"; likelihood: ",sprintf("%.7f",lasttrajlik),"; global max likelihood: ",sprintf("%.7f",global_best))

	}


	#.........................................................................................................

	#Find the best overall fit gearmult values
	gearmult_BEST<-MAINstore[which(MAINstore[,13]==max(MAINstore[,13])),]

	# Send the best fit set of gear mults into a model parameter file structure and export to the /Param folder
	new_parameters<-extract_ACTmult_to_parmsfolder(model, gearmult_BEST,csv.output=csv.output)


	#Now convert the absolute values of gearmults into relative values - ie relative to the INITIAL gearmults
	GMstore_relative<-MAINstore    # 12 columns of gearmult data and column 13 = likelihood
	for(qqq in 1:nrow(MAINstore)){
		GMstore_relative[qqq,(1:12)]<-(MAINstore[qqq,(1:12)]-gear_mult_initial)/gear_mult_initial
	}

	#Now convert the absolute values of HR into relative values - ie relative to the TARGET HRs
	HRstore_relative<-HRstore    # 20 columns of HR data and column 21 = likelihood
	for(qqq in 1:nrow(HRstore)){
		HRstore_relative[qqq,(1:20)]<-(HRstore[qqq,(1:20)]-fleet_vector_target)/fleet_vector_target
	}



	#Assemble a list object containing the data generated by the annealing process
	output_data<-list(gear_mult_results = MAINstore,
			  gear_mult_rel_initial = GMstore_relative,
			  harvest_ratio_results = HRstore,
			  harvest_ratio_rel_target = HRstore_relative,
			  new_parameter_data = new_parameters)

	#Write the four dataframes out as csv files
	filename <- csvname(resultsdir, "activity_optim_gearmult_history", model.ident)
	writecsv(MAINstore, filename, row.names=FALSE)

	filename <- csvname(resultsdir, "activity_optim_gearmult_relinitial_history", model.ident)
	writecsv(GMstore_relative, filename, row.names=FALSE)

	filename <- csvname(resultsdir, "activity_optim_harvestratio_history", model.ident)
	writecsv(HRstore, filename, row.names=FALSE)

	filename <- csvname(resultsdir, "activity_optim_harvestratio_reltarget_history", model.ident)
	writecsv(HRstore_relative, filename, row.names=FALSE)


	return(output_data)

}


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


