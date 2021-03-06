#
# boxplot_annual_compare_observations_with_ci.R
#
#' Box-and-whisker plots of observed annual target data on the state of the ecosystem with equivalent properties derived from a Monte Carlo simulation.
#'
#' Creates a multi-panel plot comparing a range of observational data on the state of the ecosystem with the distribution of credible values
#' of equivalent properties derived from the final year of a model runs generated by the e2e_run_mc() function.
#'
#' For details of how the distribution of credible output values from StrathE2E are calculated see help(e2e_run_mc)
#'
#' The function plots a multi-panel page of box-and-whisker plots showing the medians and variability ranges (quartiles as box-and-whisker) of a range of observational data on properties of an ecosystem
#' (shown in black), alongside comparable box-and-whisker plots (shown in red) of equivalent measures derived from the final years of an ensemble of model runs generted by a Monte Carlo methodology (e2e_run_mc() function).
#'
#' Individual panels of the plot represent groups of similar or related ecosystem properties - annual productions, annual fishery landings, annual P:B ratios, annual food consumtion and diet compositions, nutrient concentrations, and inshore : offshore abundanace ratios.
#'
#' The observational data to be plotted are loaded from the folder Modelname/Variantname/Target/annual_observed_*.csv as part of a e2e_read() function call and are built into the R-list object generated by e2e_read().
#' Column 3 of annual_observed_* (header "Use_1.yes_0.no") is a flag to set whether any given row is used in calculating the likelihood of the observed data given the model setup. Un-used rows of data are omitted from the box and whisker plotting panels.
#'
#' Optionally the function can read an example data set for one of the two North Sea model variants supplied with the package.
#'
#' The corresponding measures derived from the final year of a model run generated by the e2e_run_mc() function call are located in /results/Modelname/Variantname/CredInt_processed_targetresults-*.csv
#'
#' @param model R-list object defining the model configuration compiled by the e2e_read() function.
#' @param use.example Logical. If TRUE use pre-computed example data from the internal North Sea model rather than user-generated data, (default=FALSE).
#'
#' @return Graphical display in a new graphics window.
#'
#' @importFrom graphics grconvertX grconvertY
#'
#' @noRd
#
# ---------------------------------------------------------------------
# |                                                                   |
# | Authors: Mike Heath, Ian Thurlbeck                                |
# | Department of Mathematics and Statistics                          |
# | University of Strathclyde, Glasgow                                |
# |                                                                   |
# | Date of this version: May 2020                                    |
# |                                                                   |
# ---------------------------------------------------------------------

boxplot_annual_compare_observations_with_ci <- function(model, use.example=FALSE) {

start_par = par()$mfrow
on.exit(par(mfrow = start_par))

	resultsdir		<- elt(model, "setup", "resultsdir")
	model.ident		<- elt(model, "setup", "model.ident")
	model.path		<- elt(model, "setup", "model.path")
	model.name 		<- elt(model, "setup", "model.name")
	model.variant 		<- elt(model, "setup", "model.variant")

#Read the target data
	annualtargetdata	<- read_annual_target_data(model.path)

corefilename<-"CredInt_processed_targetresults"

if(use.example==TRUE){
	credintervaldata <- get.example.results(model.name, model.variant, corefilename, CREDINT_DIR)
}

if(use.example==FALSE){
	credpath	<- makepath(resultsdir, CREDINT_DIR)
	credfile	<- csvname(credpath, corefilename, model.ident)
	message("Reading credible interval processed data from '", credfile, "'")

	if (! file.exists(credfile)) {
		message("Error: cannot find credible interval output file: ", credfile)
		stop("Please run the Monte Carlo function!\n")
	}

	#Read the credible interval data
	credintervaldata	<- readcsv(credfile, row.names=1)	# first column is row names
}



	sim_targetdata<-annualtargetdata[,1:2]
	names(sim_targetdata)<-c("observed","observed_sd")

	annualtargetnames<-rep("xx",nrow(annualtargetdata))

	annualtargetnames[which(annualtargetdata[,4]=="Obs_TAPP")] <- "Total phyt."
	annualtargetnames[which(annualtargetdata[,4]=="Obs_NP")] <- "New primary phyt."
	annualtargetnames[which(annualtargetdata[,4]=="Obs_KelpP")] <- "Macrophyte carbon"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_OmnizooP")] <- "Omniv.zooplankton"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_CarnzooP")] <- "Carniv.zooplankton"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_PFishP")] <- "Planktiv.fish"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_DFishP")] <- "Demersal fish"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_BensuspP")] <- "Susp/dep.benthos"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_BencarnP")] <- "Carn/scav.benthos"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_birdP")] <- "Seabirds"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_sealP")] <- "Pinnipeds"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_cetaP")] <- "Cetaceans"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_maxbenthslar")] <- "Susp/dep.benthos larv"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_maxbenthclar")] <- "Carn/scav.benthos larv"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Conpfishfish")] <- "Plank.fish by fish"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Condfishfish")] <- "Dem.fish by fish"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Conzoofish")] <- "Zooplankton by fish"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Conzoocarnz")] <- "Omniv.zoo by carniv.zoo."
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Conbenfish")] <- "Benthos by fish"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Contotal_bird")] <- "Total by birds"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Proppfishbird")] <- "Plank.fish in bird diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Propdfishbird")] <- "Dem.fish in bird diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Propmfishbird")] <- "Mig.fish in bird diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Propdiscbird")] <- "Disc. in bird diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Contotal_seal")] <- "Total by pinnipeds"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Proppfishseal")] <- "Plank.fish in pinn. diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Propdfishseal")] <- "Dem.fish in pinn. diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Propmfishseal")] <- "Mig.fish in pinn. diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Contotal_ceta")] <- "Total by cetaceans"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Proppfishceta")] <- "Plank.fish in cet. diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Propdfishceta")] <- "Dem.fish in cet. diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Propmfishceta")] <- "Mig.fish in cet. diet"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Pland_livewt")] <- "Plank.fish landings"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Dland_livewt")] <- "Dem.fish landings"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Mland_livewt")] <- "Mig.fish landings"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Bsland_livewt")] <- "Susp/dep.benthos landings" 
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Bcland_livewt")] <- "Carn/scav.benthos landings"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Zcland_livewt")] <- "Pel.invert. landings"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Kland_livewt")] <- "Macrop. harvest"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_kelp_pb")] <- "Macrop. P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_benslar_pb")] <- "Susp/dep.benthos larv. P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_benclar_pb")] <- "Carn/scav.benthos larv. P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_bens_pb")] <- "Susp/dep.benthos P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_benc_pb")] <- "Carn/scav.benthos P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_omni_pb")] <- "Omniv.zooplankton P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_carn_pb")] <- "Carniv.zooplankton P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_fishplar_pb")] <- "Plank.fish larvae P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_fishdlar_pb")] <- "Dem.fish larvae P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_fishp_pb")] <- "Plank.fish P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_fishd_pb")] <- "Dem.fish P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_fishm_pb")] <- "Mig.fish P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_bird_pb")] <- "Bird P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_seal_pb")] <- "Pinniped P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_ceta_pb")] <- "Cetacean P/B"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_exud_C_kelp")] <- "Prop. macrop. prod. exuded"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_kelp_NC")] <- "Macrop. N/C ratio"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Denitrif")] <- "Denitrification"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Dfdiscardp")] <- "Dem.fish discard/catch"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_s_x_ammonia")] <- "Sand porewater ammonia"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_d_x_ammonia")] <- "Mud porewater ammonia"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_s_x_nitrate")] <- "Sand porewater nitrate"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_d_x_nitrate")] <- "Mud porewater nitrate"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_s_x_TON")] <- "Sand %TON"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_d_x_TON")] <- "Mud %TON"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_NDJF_s_nitrate")] <- "Winter surf.nitrate"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_MJJA_s_nitrate")] <- "Summer surf.nitrate"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_NDJF_d_nitrate")] <- "Winter deep nitrate"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_MJJA_d_nitrate")] <- "Summer deep nitrate"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_NDJF_s_ammonia")] <- "Winter surf.ammonia"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_MJJA_s_ammonia")] <- "Summer surf.ammonia"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_NDJF_d_ammonia")] <- "Winter deep ammonia"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_MJJA_d_ammonia")] <- "Summer deep ammonia"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_carn_io_ratio")] <- "Carniv.zooplanton"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_omni_io_ratio")] <- "Omniv.zooplankton"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_phyt_io_ratio")] <- "Surf.phytoplankton"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_nit_io_ratio")] <- "Surf.nitrate"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_amm_io_ratio")] <- "Surf.ammonia"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_pfish_io_ratio")] <- "Plank.fish"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_dfish_io_ratio")] <- "Dem.fish"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_birddisc")] <- "Bird by-catch"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_sealdisc")] <- "Pinniped by-catch"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_cetadisc")] <- "Cetacean by-catch"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_kelp_beachcast")] <- "Macrop. beach-cast"
	annualtargetnames[which(annualtargetdata[,4]=="Obs_Ctland_livewt")] <- "Cetacean landings"

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



	#Simulate the target data distributions from the standard devitaions

	sim_targetdata$model<-0
	for(jjjj in 1:nrow(sim_targetdata)){
		sim_targetdata$model[jjjj]<-credintervaldata[1,jjjj]
	}


	nmeasures<-nrow(sim_targetdata)
	ntargobs<-1000

	annualtarget<-array(0,dim=c(ntargobs,nmeasures))
	#dimensions observations,parameter 
	for(kkk in 1:ntargobs){
		rand<-rnorm(nmeasures,0,1)
		annualtarget[kkk,1:nmeasures]<-sim_targetdata[,1]+(rand*sim_targetdata[,2])
	}
	colnames(annualtarget)<-annualtargetnames
	#~~~~~~~~~~~~~

	#Assign the new sanitized names to teh credit data too
	names(credintervaldata) <- annualtargetnames

	#~~~~~~~~~~~~~

	#Assign the data to families

	set2plot1.1<-c(
		which(annualtargetdata[,4]=="Obs_KelpP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_TAPP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_NP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Denitrif" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_OmnizooP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_CarnzooP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_BensusP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_BencarnP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_PFishP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_DFishP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_birdP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_sealP" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_cetaP" & annualtargetdata[,3]==1)
	)

	#IDs for Annual fishery landings and by-catch (live weights)
	set2plot1.2<-c(
		which(annualtargetdata[,4]=="Obs_Pland_livewt" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Dland_livewt" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Mland_livewt" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Bsland_livewt" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Bcland_livewt" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Zcland_livewt" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Ctland_livewt" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Kland_livewt" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_cetadisc" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_sealdisc" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_birddisc" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Dfdiscardp" & annualtargetdata[,3]==1)
	)

	#IDs for annual consumption rates
	set2plot2<-c(
		which(annualtargetdata[,4]=="Obs_Conzoocarnz" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Conzoofish" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Conbenfish" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Conpfishfish" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Condfishfish" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Contotal_bird" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Contotal_seal" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Contotal_ceta" & annualtargetdata[,3]==1),

		which(annualtargetdata[,4]=="Obs_Proppfishbird" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Propdfishbird" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Propmfishbird" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Propdiscbird" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Proppfishseal" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Propdfishseal" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Propmfishseal" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Proppfishceta" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Propdfishceta" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_Propmfishceta" & annualtargetdata[,3]==1)
	)

	#Annual PB and other ratios
	set2plot3<-c(
		which(annualtargetdata[,4]=="Obs_kelp_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_kelp_NC" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_exud_C_kelp" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_kelp_beachcast" & annualtargetdata[,3]==1),

		which(annualtargetdata[,4]=="Obs_omni_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_benslar_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_benclar_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_fishplar_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_fishdlar_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_carn_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_bens_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_benc_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_fishp_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_fishd_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_fishm_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_bird_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_seal_pb" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_ceta_pb" & annualtargetdata[,3]==1)
	)

	#Average nutrient concentaryions in water and sediments
	set2plot4<-c(
		which(annualtargetdata[,4]=="Obs_s_x_ammonia" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_d_x_ammonia" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_s_x_nitrate" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_d_x_nitrate" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_s_x_TON" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_d_x_TON" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_NDJF_s_nitrate" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_MJJA_s_nitrate" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_NDJF_d_nitrate" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_MJJA_d_nitrate" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_NDJF_s_ammonia" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_MJJA_s_ammonia" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_NDJF_d_ammonia" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_MJJA_d_ammonia" & annualtargetdata[,3]==1)
	)

	#Inshore:offshore ratios
	set2plot5<-c(
		which(annualtargetdata[,4]=="Obs_amm_io_ratio" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_nit_io_ratio" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_phyt_io_ratio" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_omni_io_ratio" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_carn_io_ratio" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_pfish_io_ratio" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_dfish_io_ratio" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_bird_io_ratio" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_seal_io_ratio" & annualtargetdata[,3]==1),
		which(annualtargetdata[,4]=="Obs_ceta_io_ratio" & annualtargetdata[,3]==1)
	)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#Function to make each plot panel

	makeplot_ao<-function(annualtarget,credintervaldata,family,ymi,yma,axlabel,lgplot=TRUE){	

	#Find target data values which are negative and set them to NA
	for(qqqqq in 1:length(family)){
		if(length(which(annualtarget[,family[qqqqq]]<0)>0)){
			annualtarget[which(annualtarget[,family[qqqqq]]<0),family[qqqqq]] <- (NA)
	}}

	#Set y axis limits

	#Barrier to lower limits
	lowbarrier<-ymi/100

	#Global minimum of both data sets
			min12<-min(c(min(credintervaldata[2,family],na.rm=TRUE),min(annualtarget[,family],na.rm=TRUE) ))
			if(min12>lowbarrier){
				yaxmin<-min(ymi,min12,na.rm=TRUE)
			} else {
				yaxmin<-lowbarrier
			}

	#Global maximum of both data sets
			max12<-max(c(max(credintervaldata[6,family],na.rm=TRUE),max(annualtarget[,family],na.rm=TRUE) ))
			yaxmax<-max(yma,max12,na.rm=TRUE)

	if(lgplot==TRUE){
	boxplot(as.data.frame(log10(annualtarget[,family])),range=0,boxwex=0.3,
		ylim=c(log10(yaxmin),log10(yaxmax)),horizontal=TRUE,las=1,
		yaxt="n",xaxt="n")
		axis(1,padj=-0.75)
		nl<-length(colnames(annualtarget)[family])
		labs<-colnames(annualtarget)[family]
		axis(side=2, at=(seq(1,nl,2))+0.25, las=1,labels=labs[seq(1,nl,2)], cex.axis=0.9)
		axis(side=2, at=(seq(2,nl,2))+0.25, las=1,labels=labs[seq(2,nl,2)], cex.axis=0.9)
	}

	if(lgplot==FALSE){
	boxplot(as.data.frame((annualtarget[,family])),range=0,boxwex=0.3,
		ylim=c((yaxmin),(yaxmax)),horizontal=TRUE,las=1,
		yaxt="n",xaxt="n")
		axis(1,padj=-0.75)
		nl<-length(colnames(annualtarget)[family])
		labs<-colnames(annualtarget)[family]
		axis(side=2, at=(seq(1,nl,2))+0.25, las=1,labels=labs[seq(1,nl,2)], cex.axis=0.9)
		axis(side=2, at=(seq(2,nl,2))+0.25, las=1,labels=labs[seq(2,nl,2)], cex.axis=0.9)
	}

	mtext(axlabel,cex=0.75,side=1,line=1.7)

	plot_ci_data_ao(credintervaldata, yaxmin,yaxmax,family,lgplot=lgplot,1)

	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#Function to over-plot the modelci data as box and whiskers
	plot_ci_data_ao<-function(statdata, yaxmin,yaxmax, var.list,lgplot=TRUE, txtscale){

   		tmpmodeldata2plot<-(statdata[2:6,var.list])

		#Reset any data less than or equal to zero to 0.1*preferred axis minimum
   		for(jj in 1:length(var.list)) {
		tmpmodeldata2plot[(which(tmpmodeldata2plot[,jj]<=0)),jj] <- yaxmin*0.1
		}
		
		modeldata2plot<-tmpmodeldata2plot[,1]
   		for(jj in 2:length(var.list)) {
			modeldata2plot<-c(modeldata2plot,tmpmodeldata2plot[,jj])
		}

   		if(lgplot==TRUE) modeldata2plot<-log10(modeldata2plot)
   		array2plot<- array(dim=c(5,length(var.list)),modeldata2plot)
		colnames(array2plot)<-colnames(statdata)[var.list]
   		bxpdata<-list(stats=array2plot,n=rep(100,length(var.list)),conf=NULL,out=numeric(length=0))
   		bxp(bxpdata,add=TRUE,boxwex=0.3,at=seq(1,length(var.list))+0.35,horizontal=TRUE,yaxt="n",xaxt="n",boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red")
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#Now the actual plotting

	par(mfrow=c(3,2))
	par(mar=c(3,12,0.4,2.75))

#..................................................................


	if(length(set2plot1.1)>0){

	axlabel<-bquote("Production log"[10] ~ "mMN.m"^-2 * ".y"^-1)
	makeplot_ao(annualtarget, credintervaldata, set2plot1.1, 0.001, 2000, axlabel,lgplot=TRUE)

	}
#..................................................................

	if(length(set2plot1.2)>0){

	axlabel<-bquote("Catch log"[10] ~ "mMN.m"^-2 * ".y"^-1)
	makeplot_ao(annualtarget, credintervaldata, set2plot1.2, 0.00001, 100, axlabel,lgplot=TRUE)

	}
#..................................................................

	if(length(set2plot2)>0){

	axlabel<-bquote("Consumption log"[10] ~ "mMN.m"^-2 * ".y"^-1)
	makeplot_ao(annualtarget, credintervaldata, set2plot2, 0.0001, 200, axlabel,lgplot=TRUE)

	}
#..................................................................

	if(length(set2plot3)>0){

	axlabel<-bquote("log"[10] ~ "Annual ratio")
	makeplot_ao(annualtarget, credintervaldata, set2plot3, 0.01, 100, axlabel,lgplot=TRUE)

	}
#..................................................................

	if(length(set2plot4)>0){

	axlabel<-bquote("Conc. log"[10] ~ "mMN.m"^-3 ~ "or % by weight")
	makeplot_ao(annualtarget, credintervaldata, set2plot4, 0.01, 1000, axlabel,lgplot=TRUE)

	}
#..................................................................


	if(length(set2plot5)>0){

	axlabel<-bquote("Inshore:offshore ratios")
	makeplot_ao(annualtarget, credintervaldata, set2plot5, 0, 8, axlabel,lgplot=FALSE)

	}


	legend(grconvertX(0.45, "ndc", "user"), grconvertY(0.13, "ndc", "user"),
	"model", fill = "red", ncol=1, bty="n", xpd = NA)
	legend(grconvertX(0.45, "ndc", "user"), grconvertY(0.10, "ndc", "user"),
	"observations", fill = "black", ncol=1, bty="n", xpd = NA)

#..................................................................

}

