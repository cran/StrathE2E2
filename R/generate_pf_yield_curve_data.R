#
# generate_pf_yield_curve_data.R
#
#' Run a set of models to generate fishery yield curve data for planktivorous fish.
#'
#' Perform a set of e2e_run() model runs along a sequence of values of planktivorous fish harvest ratio multiplier, saving the annual average whole-domain biomass, annual landings and annual discards of planktivorous and demersal fish and carn/scav feeding benthos from each run 
#' plus the annual average biomasses of all the other living components of the food web. 
#' 
#' The baseline for the sequence of runs (harvest ratio multiplier = 1.0) is a model name and variant as loaded my the e2e_read() function.
#'
#' The planktivorous fish yield curve can be generated for a given setting of demersal fish harvest ratio multiplier (default = 1.0). All other
#' conditions are held constant as in the baseline model configuration.
#'
#' The yield curve represents the catch that would be generated from the stationary state of the model attained with long-term repeating annual cycles of all driving data. Hence it is important that each
#' simulation is run for long enough that the model attains its stationary state, which may be some distance from the baseline model initial conditions.
#' It is recommended that each run is at least 50 years.
#'
#' The planktivorous fish biomass and catches stored in the returned data object (and optionally a csv file) can subsequently be plotted using the function plot_pf_yield_curve(). Users can easily plot any of the other 
#' saved data using their own plotting code.
#'
#' @param model R-list object defining a baseline model configuration compiled by the e2e_read() function.
#' @param nyears Number of years to run the e2e_run() model at each level of harvest ratio. Default = 50.
#' @param PFHRvector A vector of ascending order unique multiplier values to be applied to the baseline planktivorous fish harvest ratio (default = c(0,0.5,1.0,1.5,2.0,2.5,3.0)).
#' @param DFHR Single value of the multiplier to be applied as the demersal fish harvest ratio for all runs (default=1.0).
#' @param csv.output Logical. If FALSE then disables writing output to a csv file (default=TRUE).
#'
#' @return Dataframe of annual average biomss, annual landings and annual discards of planktivorous fish, plus annual average biomasses of al other food web components, for each level of harvest ratio multiplier.
#'
#' @noRd
#
# ------------------------------------------------------------------------------

generate_pf_yield_curve_data <- function(model,nyears=50,PFHRvector=c(0,0.5,1.0,1.5,2.0,2.5,3.0),DFHR=1,csv.output=TRUE) {

# Check that the object PFHRvector is a vector of length >1
if(is.vector(PFHRvector)==FALSE || length(PFHRvector)<2 ){
	stop("Planktivorous fish harvest ratio multiplier needs to be a vector of length >1")
}

#Check that PFHRvector is an ascending order set of unique values
test<-1
for(jj in 2:(length(PFHRvector))){
if(PFHRvector[jj] <= PFHRvector[jj-1]) test<-0 
}
if(test==0 ){
	stop("Planktivorous fish harvest ratio multiplier must be a vector of ascending order unique values")
}

csv.local<-TRUE
if(csv.output==FALSE) csv.local<-FALSE

	# Unpack:
	setup		<- elt(model, "setup")
	identifier	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

Nsteps<-(length(PFHRvector))

datastore<-data.frame(PlankFishHRmult=PFHRvector,DemFishHRmult=rep(DFHR,Nsteps),
		      PlankFishbiom=rep(NA,Nsteps),PlankFishland=rep(NA,Nsteps),PlankFishdisc=rep(NA,Nsteps),
		      DemFishbiom=rep(NA,Nsteps),DemFishland=rep(NA,Nsteps),DemFishdisc=rep(NA,Nsteps),
		      MigFishbiom=rep(NA,Nsteps),MigFishland=rep(NA,Nsteps),MigFishdisc=rep(NA,Nsteps),
		      Benthsuspdepbiom=rep(NA,Nsteps),Benthsuspdepland=rep(NA,Nsteps),Benthsuspdepdisc=rep(NA,Nsteps),
	              Benthcarnscavbiom=rep(NA,Nsteps),Benthcarnscavland=rep(NA,Nsteps),Benthcarnscavdisc=rep(NA,Nsteps),
		      CarnivZoobiom=rep(NA,Nsteps),CarnivZooland=rep(NA,Nsteps),CarnivZoodisc=rep(NA,Nsteps),
		      Birdbiom=rep(NA,Nsteps),Birdland=rep(NA,Nsteps),Birddisc=rep(NA,Nsteps),
		      Pinnipedbiom=rep(NA,Nsteps),Pinnipedland=rep(NA,Nsteps),Pinnipeddisc=rep(NA,Nsteps),
		      Cetaceanbiom=rep(NA,Nsteps),Cetaceanland=rep(NA,Nsteps),Cetaceandisc=rep(NA,Nsteps),
		      Macrophytebiom=rep(NA,Nsteps),Macrophyteland=rep(NA,Nsteps),Macrophytedisc=rep(NA,Nsteps),
		      Phytoplanktonbiom=rep(NA,Nsteps),OmnivZoobiom=rep(NA,Nsteps))

for(kkk in 1:(Nsteps)){

message("Run ",kkk,"; pf harvest ratio multiplier = ",PFHRvector[kkk])

model$data$fleet.model$HRscale_vector_multiplier[1]<-PFHRvector[kkk]

model$data$fleet.model$HRscale_vector_multiplier[2]<-DFHR

results<-e2e_run(model,nyears=nyears,csv.output=FALSE)

PF_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Planktivorous_fish")
DF_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Demersal_fish")
Bcs_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Benthos_carn/scav_feeders")
KP_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Macrophyte_nitrogen")
Phyts_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Surface_layer_phytoplankton")
Phytd_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Deep_layer_phytoplankton")
OZ_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Omnivorous_zooplankton")
CZ_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Carnivorous_zooplankton")
Bsd_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Benthos_susp/dep_feeders")
MF_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Migratory_fish")
BD_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Birds")
SL_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Pinnipeds")
CT_browid<-which(results$final.year.outputs$mass_results_wholedomain$Description=="Cetaceans")

PF_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Plank.fish_landings_live_weight")
DF_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Dem.fish_landings_live_weight")
Bcs_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Benthosc/s_landings_live_weight")
MF_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Mig.fish_landings_live_weight")
Bsd_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Benthoss/d_landings_live_weight")
CZ_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Carniv.zooplankton_landings_live_weight")
KP_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Macrophyte_landings_live_weight")
BD_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Bird_landings_live_weight")
SL_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Pinniped_landings_live_weight")
CT_lrowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Cetacean_landings_live_weight")

PF_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Plank.fish_discards")
DF_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Dem.fish_discards")
Bcs_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Benthosc/s_discards")
MF_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Mig.fish_discards")
Bsd_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Benthoss/d_discards")
CZ_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Carniv.zooplankton_discards")
KP_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Macrophyte_discards")
BD_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Bird_discards")
SL_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Pinniped_discards")
CT_drowid<-which(results$final.year.outputs$annual_flux_results_wholedomain$Description=="Cetacean_discards")


datastore$PlankFishbiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[PF_browid,1]
datastore$PlankFishland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[PF_lrowid,1]
datastore$PlankFishdisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[PF_drowid,1]

datastore$DemFishbiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[DF_browid,1]
datastore$DemFishland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[DF_lrowid,1]
datastore$DemFishdisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[DF_drowid,1]

datastore$MigFishbiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[MF_browid,1]
datastore$MigFishland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[MF_lrowid,1]
datastore$MigFishdisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[MF_drowid,1]

datastore$Benthsuspdepbiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[Bsd_browid,1]
datastore$Benthsuspdepland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[Bsd_lrowid,1]
datastore$Benthsuspdepdisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[Bsd_drowid,1]

datastore$Benthcarnscavbiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[Bcs_browid,1]
datastore$Benthcarnscavland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[Bcs_lrowid,1]
datastore$Benthcarnscavdisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[Bcs_drowid,1]

datastore$CarnivZoobiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[CZ_browid,1]
datastore$CarnivZooland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[CZ_lrowid,1]
datastore$CarnivZoodisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[CZ_drowid,1]

datastore$Birdbiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[BD_browid,1]
datastore$Birdland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[BD_lrowid,1]
datastore$Birddisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[BD_drowid,1]

datastore$Pinnipedbiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[SL_browid,1]
datastore$Pinnipedland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[SL_lrowid,1]
datastore$Pinnipeddisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[SL_drowid,1]

datastore$Cetaceanbiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[CT_browid,1]
datastore$Cetaceanland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[CT_lrowid,1]
datastore$Cetaceandisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[CT_drowid,1]

datastore$Macrophytebiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[KP_browid,1]
datastore$Macrophyteland[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[KP_lrowid,1]
datastore$Macrophytedisc[kkk]<-results$final.year.outputs$annual_flux_results_wholedomain[KP_drowid,1]


datastore$Phytoplanktonbiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[Phyts_browid,1]+results$final.year.outputs$mass_results_wholedomain[Phytd_browid,1]
datastore$OmnivZoobiom[kkk]<-results$final.year.outputs$mass_results_wholedomain[OZ_browid,1]

}

#Print the data to a csv file
#-----------------------------------------------------------------

	if(csv.local==TRUE){
	filename = csvname(resultsdir, "Yield_curve_data_PFHRmult", identifier)
	writecsv.local(datastore, filename, row.names=FALSE)
	}

return(datastore)

}


