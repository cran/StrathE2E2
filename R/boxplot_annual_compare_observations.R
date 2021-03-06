#
# boxplot_annual_compare_observations.R
#
#' Box-and-whisker plots of observed annual target data on the state of the ecosystem with equivalent properties derived from a StrathE2E run.
#'
#' Multi-panel plot comparing a range of observational data on the state of the ecosystem with equivalent properties derived from the final year of a model run generated by the e2e_run() function.
#'
#' The function plots a multi-panel page of box-and-whisker plots showing the medians and variability ranges (quartiles as box-and-whisker) of a range of observational data on properties of an ecosystem
#' alongside single-value data on equivalent measures derived from the final year of a StrathE2E model run.
#' Individual panels of the plot represent groups of similar or related ecosystem properties - annual productions, annual fishery landings, annual P:B ratios, annual food consumtion and diet compositions, nutrient concentrations, and inshore : offshore abundanace ratios.
#'
#' The observational data to be plotted are loaded from the folder Modelname/Variantname/Target/annual_observed_*.csv as part of a e2e_read() function call and are built into the R-list object generated by e2e_read().
#' Column 3 of annual_observed_* (header "Use_1.yes_0.no") is a flag to set whether any given row is used in calculating the likelihood o fthe observed data given the model setup. Un-used rows of data are omitted from the box and whisker plotting panels.
#'
#' The corresponding measures derived from the final year of a model run generated by the StrathE2E() function call are located in /results/Modelname/Variantname/model_target_annualresults-*.csv, and in the R-list object generated by e2e_run().
#'
#' The likelihood of the observed data given the model configuration, driving data and parameters, is located in /results/Modelname/Variantname/model_likelihood_results-base-*.csv, and in the R-list object generated by e2e_run().
#'
#' @param model R-list object defining the model configuration compiled by the e2e_read() function.
#' @param use.saved Logical. If TRUE use data from a prior user-defined run held as csv files data in the current results folder, (default=FALSE).
#' @param results R-list object of baseline model output generated by the e2e_run(), (default=NULL).
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


boxplot_annual_compare_observations <- function(model, use.saved=FALSE, results=NULL) {

start_par = par()$mfrow
on.exit(par(mfrow = start_par))

if(use.saved==TRUE & is.list(results)==TRUE){
        stop("use.saved is TRUE but a dataframe object has also been specified ! \n")
}

	setup		<- elt(model, "setup")

if(use.saved==TRUE) {
	resultsdir	<- elt(model, "setup", "resultsdir")
	model.ident	<- elt(model, "setup", "model.ident")
	datafile	<- csvname(resultsdir, "model_target_annualresults_plus_chi", model.ident)
	print(paste("Using data held in a file ",datafile," from a past model run"))
	check.exists(datafile)
	opt_results<-readcsv(datafile)
}


if(use.saved==FALSE & is.list(results)==TRUE){
	opt_results	<- elt(results, "final.year.outputs", "opt_results")
}

# opt_results is a dataframe produced by the programme which calculates the model outputs corresponding to each observed variable

#Column 1 is the observed value, column 2 is the sd of observed data, column 3 is the fitted model value

#Column 4 is a flag to say whether the observed value was used in the fitting 1=yes, 0=no

#----------------------------------------------------------------------

sim_targetdata<-opt_results[,1:3]
names(sim_targetdata)<-c("observed","observed_sd","model")

annualtargetnames<-rep("xx",nrow(opt_results))

annualtargetnames[which(opt_results[,6]=="Obs_TAPP")] <- "Total phyt."
annualtargetnames[which(opt_results[,6]=="Obs_NP")] <- "New primary phyt."
annualtargetnames[which(opt_results[,6]=="Obs_KelpP")] <- "Kelp carbon"
annualtargetnames[which(opt_results[,6]=="Obs_OmnizooP")] <- "Omniv.zooplankton"
annualtargetnames[which(opt_results[,6]=="Obs_CarnzooP")] <- "Carniv.zooplankton"
annualtargetnames[which(opt_results[,6]=="Obs_PFishP")] <- "Planktiv.fish"
annualtargetnames[which(opt_results[,6]=="Obs_DFishP")] <- "Demersal fish"
annualtargetnames[which(opt_results[,6]=="Obs_BensuspP")] <- "Susp/dep.benthos"
annualtargetnames[which(opt_results[,6]=="Obs_BencarnP")] <- "Carn/scav.benthos"
annualtargetnames[which(opt_results[,6]=="Obs_birdP")] <- "Seabird"
annualtargetnames[which(opt_results[,6]=="Obs_sealP")] <- "Seal"
annualtargetnames[which(opt_results[,6]=="Obs_cetaP")] <- "Cetacean"
annualtargetnames[which(opt_results[,6]=="Obs_maxbenthslar")] <- "Susp/dep.benthos larv"
annualtargetnames[which(opt_results[,6]=="Obs_maxbenthclar")] <- "Carn/scav.benthos larv"
annualtargetnames[which(opt_results[,6]=="Obs_Conpfishfish")] <- "Pel.fish by fish"
annualtargetnames[which(opt_results[,6]=="Obs_Condfishfish")] <- "Dem.fish by fish"
annualtargetnames[which(opt_results[,6]=="Obs_Conzoofish")] <- "Zooplankton by fish"
annualtargetnames[which(opt_results[,6]=="Obs_Conzoocarnz")] <- "Meso-zoo by carniv.zoo."
annualtargetnames[which(opt_results[,6]=="Obs_Conbenfish")] <- "Benthos by fish"
annualtargetnames[which(opt_results[,6]=="Obs_Contotal_bird")] <- "Total by birds"
annualtargetnames[which(opt_results[,6]=="Obs_Proppfishbird")] <- "Plank.fish in bird diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propdfishbird")] <- "Dem.fish in bird diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propmfishbird")] <- "Mig.fish in bird diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propdiscbird")] <- "Disc. in bird diet"
annualtargetnames[which(opt_results[,6]=="Obs_Contotal_seal")] <- "Total by seals"
annualtargetnames[which(opt_results[,6]=="Obs_Proppfishseal")] <- "Plank.fish in seal diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propdfishseal")] <- "Dem.fish in seal diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propmfishseal")] <- "Mig.fish in seal diet"
annualtargetnames[which(opt_results[,6]=="Obs_Contotal_ceta")] <- "Total by cetaceans"
annualtargetnames[which(opt_results[,6]=="Obs_Proppfishceta")] <- "Plank.fish in cet. diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propdfishceta")] <- "Dem.fish in cet. diet"
annualtargetnames[which(opt_results[,6]=="Obs_Propmfishceta")] <- "Mig.fish in cet. diet"
annualtargetnames[which(opt_results[,6]=="Obs_Pland_livewt")] <- "Plank.fish landings"
annualtargetnames[which(opt_results[,6]=="Obs_Dland_livewt")] <- "Dem.fish landings"
annualtargetnames[which(opt_results[,6]=="Obs_Mland_livewt")] <- "Mig.fish landings"
annualtargetnames[which(opt_results[,6]=="Obs_Bsland_livewt")] <- "Susp/dep.benthos landings" 
annualtargetnames[which(opt_results[,6]=="Obs_Bcland_livewt")] <- "Carn/scav.benthos landings"
annualtargetnames[which(opt_results[,6]=="Obs_Zcland_livewt")] <- "Pel.invert. landings"
annualtargetnames[which(opt_results[,6]=="Obs_Kland_livewt")] <- "Kelp harvest"
annualtargetnames[which(opt_results[,6]=="Obs_kelp_pb")] <- "Kelp P/B"
annualtargetnames[which(opt_results[,6]=="Obs_benslar_pb")] <- "Susp/dep.benthos larv. P/B"
annualtargetnames[which(opt_results[,6]=="Obs_benclar_pb")] <- "Carn/scav.benthos larv. P/B"
annualtargetnames[which(opt_results[,6]=="Obs_bens_pb")] <- "Susp/dep.benthos P/B"
annualtargetnames[which(opt_results[,6]=="Obs_benc_pb")] <- "Carn/scav.benthos P/B"
annualtargetnames[which(opt_results[,6]=="Obs_omni_pb")] <- "Omniv.zooplankton P/B"
annualtargetnames[which(opt_results[,6]=="Obs_carn_pb")] <- "Carniv.zooplankton P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishplar_pb")] <- "Plank.fish larvae P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishdlar_pb")] <- "Dem.fish larvae P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishp_pb")] <- "Plank.fish P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishd_pb")] <- "Dem.fish P/B"
annualtargetnames[which(opt_results[,6]=="Obs_fishm_pb")] <- "Mig.fish P/B"
annualtargetnames[which(opt_results[,6]=="Obs_bird_pb")] <- "Bird P/B"
annualtargetnames[which(opt_results[,6]=="Obs_seal_pb")] <- "Seal P/B"
annualtargetnames[which(opt_results[,6]=="Obs_ceta_pb")] <- "Cetacean P/B"
annualtargetnames[which(opt_results[,6]=="Obs_exud_C_kelp")] <- "Prop. kelp prod. exuded"
annualtargetnames[which(opt_results[,6]=="Obs_kelp_NC")] <- "Kelp N/C ratio"
annualtargetnames[which(opt_results[,6]=="Obs_Denitrif")] <- "Denitrification"
annualtargetnames[which(opt_results[,6]=="Obs_Dfdiscardp")] <- "Dem.fish discard/catch"
annualtargetnames[which(opt_results[,6]=="Obs_s_x_ammonia")] <- "Sand porewater ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_d_x_ammonia")] <- "Mud porewater ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_s_x_nitrate")] <- "Sand porewater nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_d_x_nitrate")] <- "Mud porewater nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_s_x_TON")] <- "Sand %TON"
annualtargetnames[which(opt_results[,6]=="Obs_d_x_TON")] <- "Mud %TON"
annualtargetnames[which(opt_results[,6]=="Obs_NDJF_s_nitrate")] <- "Winter surf.nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_MJJA_s_nitrate")] <- "Summer surf.nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_NDJF_d_nitrate")] <- "Winter deep nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_MJJA_d_nitrate")] <- "Summer deep nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_NDJF_s_ammonia")] <- "Winter surf.ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_MJJA_s_ammonia")] <- "Summer surf.ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_NDJF_d_ammonia")] <- "Winter deep ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_MJJA_d_ammonia")] <- "Summer deep ammonia"
annualtargetnames[which(opt_results[,6]=="Obs_carn_io_ratio")] <- "Carniv.zooplanton"
annualtargetnames[which(opt_results[,6]=="Obs_omni_io_ratio")] <- "Omniv.zooplankton"
annualtargetnames[which(opt_results[,6]=="Obs_phyt_io_ratio")] <- "Surf.phytoplankton"
annualtargetnames[which(opt_results[,6]=="Obs_nit_io_ratio")] <- "Surf.nitrate"
annualtargetnames[which(opt_results[,6]=="Obs_amm_io_ratio")] <- "Surf.ammomnia"
annualtargetnames[which(opt_results[,6]=="Obs_pfish_io_ratio")] <- "Plank.fish"
annualtargetnames[which(opt_results[,6]=="Obs_dfish_io_ratio")] <- "Dem.fish"
annualtargetnames[which(opt_results[,6]=="Obs_birddisc")] <- "Bird by-catch"
annualtargetnames[which(opt_results[,6]=="Obs_sealdisc")] <- "Seal by-catch"
annualtargetnames[which(opt_results[,6]=="Obs_cetadisc")] <- "Cetacean by-catch"
annualtargetnames[which(opt_results[,6]=="Obs_kelp_beachcast")] <- "Kelp beach-cast"
annualtargetnames[which(opt_results[,6]=="Obs_Ctland_livewt")] <- "Cetacean landings"



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

ntargobs<-10
modeltarget<-array(0,dim=c(ntargobs,nmeasures))
#dimensions observations,parameter 
for(kkk in 1:ntargobs){
rand<-rnorm(nmeasures,0,1)
modeltarget[kkk,1:nmeasures]<-sim_targetdata[,3]+(rand*0)
}
colnames(modeltarget)<-annualtargetnames
#~~~~~~~~~~~~~

#IDs for Annual production rates
set2plot1.1<-c(
   which(opt_results[,6]=="Obs_KelpP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_TAPP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Denitrif" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_OmnizooP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_CarnzooP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_BensusP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_BencarnP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_PFishP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_DFishP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_birdP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_sealP" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_cetaP" & opt_results[,4]==1)   )

#IDs for Annual fishery landings and by-catch (live weights)
set2plot1.2<-c(
   which(opt_results[,6]=="Obs_Pland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Dland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Mland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Bsland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Bcland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Zcland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Ctland_livewt" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Kland_livewt" & opt_results[,4]==1)  ,
   which(opt_results[,6]=="Obs_cetadisc" & opt_results[,4]==1)  ,
   which(opt_results[,6]=="Obs_sealdisc" & opt_results[,4]==1)  ,
   which(opt_results[,6]=="Obs_birddisc" & opt_results[,4]==1)  ,
   which(opt_results[,6]=="Obs_Dfdiscardp" & opt_results[,4]==1)  )

#IDs for annual consumption rates
set2plot2<-c(
   which(opt_results[,6]=="Obs_Conzoocarnz" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Conzoofish" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Conbenfish" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Conpfishfish" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Condfishfish" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Contotal_bird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Contotal_seal" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Contotal_ceta" & opt_results[,4]==1) ,

   which(opt_results[,6]=="Obs_Proppfishbird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propdfishbird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propmfishbird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propdiscbird" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Proppfishseal" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propdfishseal" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propmfishseal" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Proppfishceta" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propdfishceta" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_Propmfishceta" & opt_results[,4]==1) )

#Annual PB and other ratios
set2plot3<-c(
   which(opt_results[,6]=="Obs_kelp_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_kelp_NC" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_exud_C_kelp" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_kelp_beachcast" & opt_results[,4]==1) ,

   which(opt_results[,6]=="Obs_omni_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_benslar_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_benclar_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishplar_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishdlar_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_carn_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_bens_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_benc_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishp_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishd_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_fishm_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_bird_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_seal_pb" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_ceta_pb" & opt_results[,4]==1) )

#Average nutrient concentaryions in water and sediments
set2plot4<-c(
   which(opt_results[,6]=="Obs_s_x_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_d_x_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_s_x_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_d_x_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_s_x_TON" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_d_x_TON" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NDJF_s_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_MJJA_s_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NDJF_d_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_MJJA_d_nitrate" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NDJF_s_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_MJJA_s_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_NDJF_d_ammonia" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_MJJA_d_ammonia" & opt_results[,4]==1) )

#Inshore:offshore ratios
set2plot5<-c(
   which(opt_results[,6]=="Obs_amm_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_nit_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_phyt_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_omni_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_carn_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_pfish_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_dfish_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_bird_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_seal_io_ratio" & opt_results[,4]==1) ,
   which(opt_results[,6]=="Obs_ceta_io_ratio" & opt_results[,4]==1) )


#..................................................................
#..................................................................


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

makeplot_aso<-function(annualtarget, modeltarget, family, ymi, yma, axlabel, lgplot=TRUE){

#Find target data values which are negative and set them to NA
for(qqqqq in 1:length(family)){
if(length(which(annualtarget[,family[qqqqq]]<0)>0)){
annualtarget[which(annualtarget[,family[qqqqq]]<0),family[qqqqq]] <- (NA)
}}

#Set y axis limits
	#Barrier to lower limits
	lowbarrier<-ymi/100

#Global minimum of both data sets
	min12<-min(c(min(modeltarget[,family],na.rm=TRUE),min(annualtarget[,family],na.rm=TRUE) ))
		if(min12>lowbarrier){
			yaxmin<-min(ymi,min12,na.rm=TRUE)
		} else {
			yaxmin<-lowbarrier
		}

#Global maximum of both data sets
	max12<-max(c(max(modeltarget[,family],na.rm=TRUE),max(annualtarget[,family],na.rm=TRUE) ))
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


if(lgplot==TRUE){
	boxplot(as.data.frame(log10(modeltarget[,family])),add=TRUE,
		at=seq(1,length(family))+0.35,horizontal=TRUE,par(lty=1),yaxt="n",xaxt="n",
		range=0,boxwex=0.3,
		boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red")
}

if(lgplot==FALSE){
	boxplot(as.data.frame((modeltarget[,family])),add=TRUE,
		at=seq(1,length(family))+0.35,horizontal=TRUE,par(lty=1),yaxt="n",xaxt="n",
		range=0,boxwex=0.3,
		boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red")
}

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



	par(mfrow=c(3,2))
	par(mar=c(3,12,0.4,2.75))

#..................................................................

	if(length(set2plot1.1)>0){

	axlabel<-bquote("Production log"[10] ~ "mMN.m"^-2 * ".y"^-1)
	makeplot_aso(annualtarget, modeltarget, set2plot1.1, 0.001, 2000, axlabel,lgplot=TRUE)

	}
#..................................................................


	if(length(set2plot1.2)>0){

	axlabel<-bquote("Catch log"[10] ~ "mMN.m"^-2 * ".y"^-1)
	makeplot_aso(annualtarget, modeltarget, set2plot1.2, 0.00001, 100, axlabel,lgplot=TRUE)

	}
#..................................................................

	if(length(set2plot2)>0){

	axlabel<-bquote("Consumption log"[10] ~ "mMN.m"^-2 * ".y"^-1)
	makeplot_aso(annualtarget, modeltarget, set2plot2, 0.0001, 200, axlabel,lgplot=TRUE)

	}
#..................................................................

	if(length(set2plot3)>0){

	axlabel<-bquote("log"[10] ~ "Annual ratio")
	makeplot_aso(annualtarget, modeltarget, set2plot3, 0.01, 100, axlabel,lgplot=TRUE)

	}
#..................................................................

	if(length(set2plot4)>0){

	axlabel<-bquote("Conc. log"[10] ~ "mMN.m"^-3 ~ "or % by weight")
	makeplot_aso(annualtarget, modeltarget, set2plot4, 0.01, 1000, axlabel,lgplot=TRUE)

	}
#..................................................................


	if(length(set2plot5)>0){

	axlabel<-bquote("Inshore:offshore ratios")
	makeplot_aso(annualtarget, modeltarget, set2plot5, 0, 8, axlabel,lgplot=FALSE)

	}

	legend(grconvertX(0.45, "ndc", "user"), grconvertY(0.13, "ndc", "user"),
	"model", fill = "red", ncol=1, bty="n", xpd = NA)
	legend(grconvertX(0.45, "ndc", "user"), grconvertY(0.10, "ndc", "user"),
	"observations", fill = "black", ncol=1, bty="n", xpd = NA)

#..................................................................



}

