#
# process_sensitivity_analysis_results.R
#
#' post-process data from a one-at-a-time sensitivity analysis using
#' the Morris Method for factorial sampling of the ecology model
#' parameters, the fishing fleet model parameters, and the
#' environmental forcings
#'
#' @param model full model object
#' @param results model results object
#'
#' @importFrom stats sd
#'
#' @noRd
#
# ------------------------------------------------------------------------------

process_sensitivity_analysis_results <- function(model, results) {

	resultsdir <- elt(model, "setup", "resultsdir")
	model.ident	<- elt(model, "setup", "model.ident")

	parameter.details.data.file <- system.file("extdata/Internal.data", "StrathE2E_parameter_list.csv", package="StrathE2E2", mustWork = TRUE)
	parameter.details <- read.csv(parameter.details.data.file,header=TRUE)

	results_df_out <- results

	senscritname <- names(results_df_out)[6]
	if(senscritname=="likelihood") senscritname<-"Target data likelihood"        

	NPARMS<-(nrow(results_df_out[which(results_df_out$trajectoryid==1),]))-1
	NTRAJ<-(nrow(results_df_out))/(NPARMS+1)

	for(jj in 1:NTRAJ) {
		baserow<-((jj-1)*(NPARMS+1))+1
		datastart<-baserow+1
		dataend  <-baserow+NPARMS

		base<-results_df_out[baserow,]
		data<-results_df_out[datastart:dataend,]

	}

	jj<-2
	baserow<-((jj-1)*(NPARMS+1))+1
	datastart<-baserow+1
	dataend  <-baserow+NPARMS
	base<-results_df_out[baserow,]
	data<-results_df_out[datastart:dataend,]
	dz<-which(data[,7]==0)

	#list of all parameter ids
	parslist<-data$parameterid

	#dz = the rowid in data for params which are zero
	#id of the zero parameters
	zeropars<-data$parameterid[dz]

	#Grab the first trajectory as the basis for a dataframe to build the results

	SENS_results<-results_df_out[2:(NPARMS+1),1:2]
	SENS_results$fixfit<-0
        SENS_results$criterion <- senscritname 
	SENS_results$EEmean<-NA
	SENS_results$EEsd<-NA

	#vector of the the ids of the time series drivers
#	driverparms<-c(1001:1053)
	driverparms<-parameter.details$parameterid[which(parameter.details$fixfit==4)]

	#vector of physical setup parameters
#	physicalparms<-c(1:4,28:56)
	physicalparms<-parameter.details$parameterid[which(parameter.details$fixfit==5)]

	#vector of the the ids of the fishing parameters
#	harvestrates<-c(57:76)
	harvestrates<-parameter.details$parameterid[which(parameter.details$fixfit==3)]

#	fishingparms<-c(77:123,286:306)
	fishingparms<-parameter.details$parameterid[which(parameter.details$fixfit==2)]

	#vector of the the ids of the fixed parameters
#	fixedpars<-c(307:314,480:507,570:585)
	fixedpars<-parameter.details$parameterid[which(parameter.details$fixfit==1)]

	driverrows<-rep(0,length(harvestrates))
	for(jjk in 1:length(driverparms)){
		driverrows[jjk]<-which(SENS_results$parameterid==driverparms[jjk])
	}

	physicalrows<-rep(0,length(physicalparms))
	for(jjk in 1:length(physicalparms)){
		physicalrows[jjk]<-which(SENS_results$parameterid==physicalparms[jjk])
	}

	harvestrows<-rep(0,length(harvestrates))
	for(jjk in 1:length(harvestrates)){
		harvestrows[jjk]<-which(SENS_results$parameterid==harvestrates[jjk])
	}

	fishingrows<-rep(0,length(fishingparms))
	for(jjk in 1:length(fishingparms)){
		fishingrows[jjk]<-which(SENS_results$parameterid==fishingparms[jjk])
	}

	fixedrows<-rep(0,length(fixedpars))
	for(jjk in 1:length(fixedpars)){
		fixedrows[jjk]<-which(SENS_results$parameterid==fixedpars[jjk])
	}

	#find the rows correspoinding to each parameter type
	SENS_results$fixfit[fixedrows]<-1
	SENS_results$fixfit[fishingrows]<-2
	SENS_results$fixfit[harvestrows]<-3
	SENS_results$fixfit[driverrows]<-4
	SENS_results$fixfit[physicalrows]<-5
	#Default valkue = 0 = the fitted parameters

	#Now loop through the parameters and calculate the mean and sed of abs(EE)

	counter<-0
	for(jj in parslist){
		counter<-counter+1

		if(length(which(zeropars==jj))==0){
			parsub<-results_df_out[which(results_df_out$parameterid==jj),]
			SENS_results$EEmean[counter]<-mean( (parsub$EE))
			SENS_results$EEsd[counter]<-sd( (parsub$EE))
		}
	}

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	Sorted_SENS_results<-SENS_results[order(-sqrt(SENS_results$EEmean^2)),]
	Sorted_SENS_results$EEmean_non_0<-NA
	for(jjk in 1:(nrow(Sorted_SENS_results))){
		if(is.na(Sorted_SENS_results$EEsd[jjk] ) == FALSE) {
			if(Sorted_SENS_results$EEsd[jjk] >  (sqrt(NTRAJ)/2) * sqrt(Sorted_SENS_results$EEmean[jjk]^2) ) {
				Sorted_SENS_results$EEmean_non_0[jjk]<- "ns"
			} else {
				Sorted_SENS_results$EEmean_non_0[jjk]<- "sig"
			}
		}
	}

	Sorted_SENS_results$Ntrajectories <- NTRAJ
	Sorted_SENS_results$Parameter.class <- ""
	Sorted_SENS_results$Parameter.description <- ""
	Sorted_SENS_results$Model.guild.or.feature <- ""


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#Attach details of each parameter to the sorted file
	#Load the details from the Interal.data folder inside the package

		for(jjj in 1:(nrow(Sorted_SENS_results))){
			details.row <- which(parameter.details$parameterid==Sorted_SENS_results$parameterid[jjj])
			if(details.row>0){
			Sorted_SENS_results$Parameter.class[jjj]<-as.character(parameter.details$Parameter.class[details.row])
			Sorted_SENS_results$Parameter.description[jjj]<-as.character(parameter.details$Parameter.description[details.row])
			Sorted_SENS_results$Model.guild.or.feature[jjj]<-as.character(parameter.details$Model.guild.or.feature[details.row])
			}
		}

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#Output the sorted data file to csv
	csvfile <- csvname(resultsdir, "sorted_parameter_elementary_effects", model.ident)
	writecsv(Sorted_SENS_results, csvfile, row.names=FALSE)

	return(Sorted_SENS_results)

}

