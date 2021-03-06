#
# plot_inshore_vs_offshore_anavmass.R
#
#' Plots of annual average inshore and offshore mass densities generated a StrathE2E simulation.
#'
#' Create a plot showing the distribution of annual average inshore and offshore mass densities over the final year of a simulation generated by the e2e_run() function.
#'
#' The format of the output is a single panel with rows symbols. Each row represents a different
#' state variable in the model (bacteria and detritus, phytoplankton etc etc). Each row consists of two symbols, one (red) for the offshore
#' zone and the other (blue) for the inshore zone. The data represented are values of the mass densities (mMn/m2) of each variable, averaged
#' over the final year of a model run.
#'
#' @param model R-list object defining the model configuration compiled by the e2e_read() function
#' @param use.saved Logical. If TRUE use data from a prior user-defined run held as csv files data in the current results folder, (default=FALSE).
#' @param results R-list object of baseline model output generated by the e2e_run(), (default=NULL).
#'
#' @return Graphical display in a new graphics window
#'
#' @importFrom graphics text
#'
#' @noRd
#
# ------------------------------------------------------------------------------

plot_inshore_vs_offshore_anavmass <- function(model, use.saved=FALSE, results=NULL) {

start_par = par()$mfrow
on.exit(par(mfrow = start_par))

if(use.saved==FALSE & is.list(results)==FALSE){
        stop("no source of data has been specified ! \n")
}

if(use.saved==TRUE & is.list(results)==TRUE){
        stop("use.saved is TRUE but a results list object has also been specified ! \n")
}

if(use.saved==FALSE & is.list(results)==TRUE){
	O_rawresults	<- elt(results, "final.year.outputs", "mass_results_offshore")
	I_rawresults	<- elt(results, "final.year.outputs", "mass_results_inshore")
}

if(use.saved==TRUE & is.list(results)==FALSE){
	resultsdir	<- elt(model, "setup", "resultsdir")
	model.ident	<- elt(model, "setup", "model.ident")
#	model.name 	<- elt(model, "setup", "model.name")
#	model.variant 	<- elt(model, "setup", "model.variant")
	datafile_o	<- csvname(resultsdir, "OFFSHORE_model_anav_biomass", model.ident)
	datafile_i	<- csvname(resultsdir, "INSHORE_model_anav_biomass", model.ident)
	message("Reading data from '", datafile_o, "'")
	message("Reading data from '", datafile_i, "'")
	O_rawresults<-readcsv(datafile_o)
	I_rawresults<-readcsv(datafile_i)
}

shallowprop <- O_rawresults[which(O_rawresults$Description == "Area_proportion_of_inshore_zone"),1]

#Set up two dataframes to hold the area density data

O_results<-data.frame("total_labile_detritus_bd"=rep(NA,6),
		"kelp_bd"=rep(NA,6),
		"phyt_bd"=rep(NA,6),
		"benth_susp_bd"=rep(NA,6),
		"omni_bd"=rep(NA,6),
		"benth_carn_bd"=rep(NA,6),
		"carn_bd"=rep(NA,6),	
		"plankfish_bd"=rep(NA,6),
		"migfish_bd"=rep(NA,6),
		"demfish_bd"=rep(NA,6),
		"bird_bd"=rep(NA,6),
		"seal_bd"=rep(NA,6),
		"ceta_bd"=rep(NA,6))
I_results<-O_results

# Rows in the raw input data....
#1                    Surface_layer_detritus
#2                       Deep_layer_detritus
#3  Sediment_labile_plus_refractory_detritus
#4              Sediment_refractory_detritus
#5                          Fishery_discards
#6                                   Corpses
#7                         Macrophyte_debris
#8                     Surface_layer_ammonia
#9                        Deep_layer_ammonia
#10               Sediment_porewater_ammonia
#11                    Surface_layer_nitrate
#12                       Deep_layer_nitrate
#13               Sediment_porewater_nitrate
#14                      Macrophyte_nitrogen
#15              Surface_layer_phytoplankton
#16                 Deep_layer_phytoplankton
#17                   Omnivorous_zooplankton
#18                  Carnivorous_zooplankton
#19          Benthos_susp/dep_feeders_larvae
#20                 Benthos_susp/dep_feeders
#21         Benthos_carn/scav_feeders_larvae
#22                Benthos_carn/scav_feeders
#23                Planktivorous_fish_larvae
#24                       Planktivorous_fish
#25                           Migratory_fish
#26                     Demersal_fish_larvae
#27                            Demersal_fish
#28                                    Birds
#29                                Pinnipeds
#30                                Cetaceans

xRdetritus_i <- I_rawresults[which(I_rawresults$Description=="Sediment_refractory_detritus"),1]
xRdetritus_o <- O_rawresults[which(O_rawresults$Description=="Sediment_refractory_detritus"),1]
xLdetritus_i <- I_rawresults[which(I_rawresults$Description== "Sediment_labile_plus_refractory_detritus"),1]-xRdetritus_i
xLdetritus_o <- O_rawresults[which(O_rawresults$Description== "Sediment_labile_plus_refractory_detritus"),1]-xRdetritus_o

total_labile_detritus_i <- xLdetritus_i + I_rawresults[which(I_rawresults$Description=="Surface_layer_detritus"),1] +
					  I_rawresults[which(I_rawresults$Description=="Fishery_discards"),1] +
					  I_rawresults[which(I_rawresults$Description=="Corpses"),1] +
					  I_rawresults[which(I_rawresults$Description=="Macrophyte_debris"),1]

total_labile_detritus_o <- xLdetritus_o + O_rawresults[which(O_rawresults$Description=="Surface_layer_detritus"),1] +
					 + O_rawresults[which(O_rawresults$Description=="Fishery_discards"),1] +
					 + O_rawresults[which(O_rawresults$Description=="Corpses"),1] +
					 + O_rawresults[which(O_rawresults$Description=="Deep_layer_detritus"),1]


I_results$total_labile_detritus_bd <- total_labile_detritus_i/(shallowprop)
O_results$total_labile_detritus_bd <- total_labile_detritus_o/(1-shallowprop)

I_results$kelp_bd <- (I_rawresults[which(I_rawresults$Description=="Macrophyte_nitrogen"),1])/shallowprop

I_results$phyt_bd <- (I_rawresults[which(I_rawresults$Description=="Surface_layer_phytoplankton"),1])/shallowprop
O_results$phyt_bd <- ( (O_rawresults[which(O_rawresults$Description=="Surface_layer_phytoplankton"),1]) +
                     + (O_rawresults[which(O_rawresults$Description=="Deep_layer_phytoplankton"),1]) ) /(1-shallowprop)

I_results$benth_susp_bd <- ( (I_rawresults[which(I_rawresults$Description=="Benthos_susp/dep_feeders"),1]) +
                            + (I_rawresults[which(I_rawresults$Description=="Benthos_susp/dep_feeders_larvae"),1]) ) /shallowprop
O_results$benth_susp_bd <- ( (O_rawresults[which(O_rawresults$Description=="Benthos_susp/dep_feeders"),1]) +
                     + (O_rawresults[which(O_rawresults$Description=="Benthos_susp/dep_feeders_larvae"),1]) ) /(1-shallowprop)

I_results$benth_carn_bd <- ( (I_rawresults[which(I_rawresults$Description=="Benthos_carn/scav_feeders"),1]) +
                            + (I_rawresults[which(I_rawresults$Description=="Benthos_carn/scav_feeders_larvae"),1]) ) /shallowprop
O_results$benth_carn_bd <- ( (O_rawresults[which(O_rawresults$Description=="Benthos_carn/scav_feeders"),1]) +
                     + (O_rawresults[which(O_rawresults$Description=="Benthos_carn/scav_feeders_larvae"),1]) ) /(1-shallowprop)

I_results$plankfish_bd <- ( (I_rawresults[which(I_rawresults$Description=="Planktivorous_fish"),1]) +
                            + (I_rawresults[which(I_rawresults$Description=="Planktivorous_fish_larvae"),1]) ) /shallowprop
O_results$plankfish_bd <- ( (O_rawresults[which(O_rawresults$Description=="Planktivorous_fish"),1]) +
                     + (O_rawresults[which(O_rawresults$Description=="Planktivorous_fish_larvae"),1]) ) /(1-shallowprop)

I_results$demfish_bd <- ( (I_rawresults[which(I_rawresults$Description=="Demersal_fish"),1]) +
                            + (I_rawresults[which(I_rawresults$Description=="Demersal_fish_larvae"),1]) ) /shallowprop
O_results$demfish_bd <- ( (O_rawresults[which(O_rawresults$Description=="Demersal_fish"),1]) +
                     + (O_rawresults[which(O_rawresults$Description=="Demersal_fish_larvae"),1]) ) /(1-shallowprop)

I_results$migfish_bd <- ( (I_rawresults[which(I_rawresults$Description=="Migratory_fish"),1]) ) /shallowprop
O_results$migfish_bd <- ( (O_rawresults[which(O_rawresults$Description=="Migratory_fish"),1]) ) /(1-shallowprop)

I_results$omni_bd <- ( (I_rawresults[which(I_rawresults$Description=="Omnivorous_zooplankton"),1]) ) /shallowprop
O_results$omni_bd <- ( (O_rawresults[which(O_rawresults$Description=="Omnivorous_zooplankton"),1]) ) /(1-shallowprop)

I_results$carn_bd <- ( (I_rawresults[which(I_rawresults$Description=="Carnivorous_zooplankton"),1]) ) /shallowprop
O_results$carn_bd <- ( (O_rawresults[which(O_rawresults$Description=="Carnivorous_zooplankton"),1]) ) /(1-shallowprop)

I_results$bird_bd <- ( (I_rawresults[which(I_rawresults$Description=="Birds"),1]) ) /shallowprop
O_results$bird_bd <- ( (O_rawresults[which(O_rawresults$Description=="Birds"),1]) ) /(1-shallowprop)

I_results$seal_bd <- ( (I_rawresults[which(I_rawresults$Description=="Pinnipeds"),1]) ) /shallowprop
O_results$seal_bd <- ( (O_rawresults[which(O_rawresults$Description=="Pinnipeds"),1]) ) /(1-shallowprop)

I_results$ceta_bd <- ( (I_rawresults[which(I_rawresults$Description=="Cetaceans"),1]) ) /shallowprop
O_results$ceta_bd <- ( (O_rawresults[which(O_rawresults$Description=="Cetaceans"),1]) ) /(1-shallowprop)

	Nind<-13

	#Plot the inshore and offshore results

	#plotlabels<-c("bact.&.detritus","phyt","s/d.benth","omniv.zoo","c/s.benth","carn.zoo","plankt.fish","mig.fish","dem.fish","bird.&.mam")
	plotlabels<-c("Bact.&.detritus","Macrophytes","Phytoplankton","S/d.benthos","Omniv.zoo","C/s.benthos","Carn.zoo","Plankt.fish","Mig.fish","Dem.fish","Birds","Pinnipeds","Cetaceans")

	modeldata2plot<-log10(O_results[2:6,1])
	for(jj in 2:Nind) { modeldata2plot<-c(modeldata2plot,log10(O_results[2:6,jj]))}
	array2plot<- array(dim=c(5,Nind),modeldata2plot)
	colnames(array2plot)<-plotlabels
	bxpdata_O<-list(stats=array2plot,n=rep(100,Nind),conf=NULL,out=numeric(length=0),names=plotlabels)

	modeldata2plot<-log10(I_results[2:6,1])
	for(jj in 2:Nind) { modeldata2plot<-c(modeldata2plot,log10(I_results[2:6,jj]))}
	array2plot<- array(dim=c(5,Nind),modeldata2plot)
	colnames(array2plot)<-plotlabels
	bxpdata_I<-list(stats=array2plot,n=rep(100,Nind),conf=NULL,out=numeric(length=0),names=plotlabels)

        combdata<-rbind(bxpdata_O$stats,bxpdata_I$stats)
        combdata[1:5,2]<-NA  # reset offshore macrophytes to NA instead of -Inf
	ulm<-max(combdata[c(5,10),],na.rm=TRUE)
	llm<-min(combdata[c(2,7),],na.rm=TRUE)

	ymin<-floor(llm)
	ymax<-ceiling(ulm)

	par(mfrow=c(1,1))
	par(mar=c(5,8,1,1))
	bxp(bxpdata_O,boxwex=0.35,at=seq(1,Nind),ylim=c(ymin,ymax),cex.axis=1.2,show.names=TRUE,horizontal=TRUE,las=1,boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red")
	#axis(side=1,las=1,cex.axis=1.2)
	#axis(side=1,las=1,cex.axis=1.2,at=c(-1,0,-1,2))
#	mtext("Log(annual average biomass density)",cex=1.2,side=1,line=3)
	mtext(bquote("Annual average biomass log"[10] ~ "mMN.m"^-2),cex=1.2,side=1,line=3)
	bxp(bxpdata_I,add=TRUE,boxwex=0.35,at=seq(1,Nind)+0.35,yaxt="n",xaxt="n",horizontal=TRUE,boxcol="blue",whiskcol="blue",whisklty="solid",medcol="blue",staplecol="blue")

	legend("bottomleft",box.lty=0,bg="transparent",c("Inshore","Offshore"),col=c("blue","red"),lty=c(1,1),lwd=c(2,2),pt.cex=c(1,1),cex=c(0.9,0.9))

	abline(h=1.7,lty="dashed")
	abline(h=2.7,lty="dashed")
	abline(h=3.7,lty="dashed")
	abline(h=4.7,lty="dashed")
	abline(h=5.7,lty="dashed")
	abline(h=6.7,lty="dashed")
	abline(h=7.7,lty="dashed")
	abline(h=8.7,lty="dashed")
	abline(h=9.7,lty="dashed")
	abline(h=10.7,lty="dashed")
	abline(h=11.7,lty="dashed")
	abline(h=12.7,lty="dashed")

}

