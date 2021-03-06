#
# plot_final_year_migration_data_with_ci.R
#
#' Plots of active migration fluxes over the final year of a simulation generated by the Monte Carlo function e2e_run_mc().
#'
#' Create a multi-panel plot showing the credible intervals of inshore-offshore net active migration fluxes over the final year of a simulation
#' generated by the Monte Carlo function e2e_run_mc().
#'
#' Daily interval post-processed data from the e2e_run_mc() function are stored in the file
#' /results/Modelname/Variantname/CredInt/CredInt_processed_daily_migrations-*.csv, where * represents the model run identifier
#' (model.ident) text embedded in the R-list object created by the e2e_read() function.
#'
#' Optionally the function can read an example data set for one of the two North Sea model variants supplied with the package.
#'
#' Each panel of the plot shows a time-series of the net flux densities (mMN per m2 sea surface area per day) of one
#' of the migratory guilds in the model (all three guilds of fish, birds, pinnipeds and cetaceans) between the inshore and offshore zones
#' of the model, over the final year of a run. These migration fluxes are the dynamic product of gradients in the ratio of food
#' concentration to predator concentration across the inshore-offshore boundary.
#'
#' In each plot the x-axis represents day of the year over an annual cycle. The y-axis represents flux density of a migratory guild
#' (mMN/m2/d). Results from the maximum likelihood model are shown by a red line. The median of the credible values distribution
#' is shown my a solid black line. A grey-shaded area indicates the 50% credible interval (spanning quartiles of the cumulative likelihood
#' of simulated values). Black dashed lines span the 99% credible interval.
#'
#' Positive values of the net migration flux indicate net movement from the offshore to inshore zone. Negative values indicate net
#' movement from inshore to offshore.
#'
#' For details of how the distribution of credible output values from StrathE2E are calculated see the help information for the e2e_run_mc() function.
#'
#' @param model R-list object defining the model configuration compiled by the e2e_read() function
#' @param use.example Logical. If TRUE use pre-computed example data from the internal North Sea model rather than user-generated data (default=FALSE)
#'
#' @return Graphical display in a new graphics window
#'
#' @importFrom graphics polygon
#' @importFrom graphics text
#'
#' @noRd
#
# ------------------------------------------------------------------------------

plot_final_year_migration_data_with_ci <- function(model, use.example=FALSE) {

start_par = par()$mfrow
on.exit(par(mfrow = start_par))

	resultsdir	<- elt(model, "setup", "resultsdir")
	model.ident	<- elt(model, "setup", "model.ident")
	model.name 	<- elt(model, "setup", "model.name")
	model.variant 	<- elt(model, "setup", "model.variant")

corefilename<-"CredInt_processed_daily_migrations"

if(use.example==TRUE){
	Result_data_store <- get.example.results(model.name, model.variant, corefilename, CREDINT_DIR)
}

if(use.example==FALSE){
	credpath	<- makepath(resultsdir, CREDINT_DIR)
	credfile	<- csvname(credpath, corefilename, model.ident)
	message("Reading credible interval processed data from '", credfile, "'")

	Result_data_store<- readcsv(credfile, row.names=1)
}

#	Pelfishmig_propbiom_id	<-grep("Planktivorous_fish_off_in_net_migration_propbiom",row.names(Result_data_store))
#	Migfishmig_propbiom_id	<-grep("Migratory_fish_off_in_net_migration_propbiom",row.names(Result_data_store))
#	Demfishmig_propbiom_id	<-grep("Demersal_fish_off_in_net_migration_propbiom",row.names(Result_data_store))

#	Birdmig_propbiom_id	<-grep("Birds_off_in_net_migration_propbiom",row.names(Result_data_store))
#	Sealmig_propbiom_id	<-grep("Pinnipeds_off_in_net_migration_propbiom",row.names(Result_data_store))
#	Cetamig_propbiom_id	<-grep("Cetaceans_off_in_net_migration_propbiom",row.names(Result_data_store))


	Pelfishmig_propbiom_id	<-grep("Planktivorous_fish_offshore_inshore_net_migration",row.names(Result_data_store))
	Migfishmig_propbiom_id	<-grep("Migratory_fish_offshore_inshore_net_migration",row.names(Result_data_store))
	Demfishmig_propbiom_id	<-grep("Demersal_fish_offshore_inshore_net_migration",row.names(Result_data_store))

	Birdmig_propbiom_id	<-grep("Birds_offshore_inshore_net_migration",row.names(Result_data_store))
	Sealmig_propbiom_id	<-grep("Pinnipeds_offshore_inshore_net_migration",row.names(Result_data_store))
	Cetamig_propbiom_id	<-grep("Cetaceans_offshore_inshore_net_migration",row.names(Result_data_store))

#	y_all_max<-max(rbind(Result_data_store[Pelfishmig_propbiom_id,],
#		Result_data_store[Migfishmig_propbiom_id,],
#		Result_data_store[Demfishmig_propbiom_id,],
#		Result_data_store[Birdmig_propbiom_id,],
#		Result_data_store[Sealmig_propbiom_id,],
#		Result_data_store[Cetamig_propbiom_id,]))

#	y_all_min<-min(rbind(Result_data_store[Pelfishmig_propbiom_id,],
#		Result_data_store[Migfishmig_propbiom_id,],
#		Result_data_store[Demfishmig_propbiom_id,],
#		Result_data_store[Birdmig_propbiom_id,],
#		Result_data_store[Sealmig_propbiom_id,],
#		Result_data_store[Cetamig_propbiom_id,]))

	y_max1<-max(Result_data_store[Pelfishmig_propbiom_id,])
	y_max2<-max(Result_data_store[Migfishmig_propbiom_id,])
	y_max3<-max(Result_data_store[Demfishmig_propbiom_id,])
	y_max4<-max(Result_data_store[Birdmig_propbiom_id,])
	y_max5<-max(Result_data_store[Sealmig_propbiom_id,])
	y_max6<-max(Result_data_store[Cetamig_propbiom_id,])

	y_min1<-min(Result_data_store[Pelfishmig_propbiom_id,])
	y_min2<-min(Result_data_store[Migfishmig_propbiom_id,])
	y_min3<-min(Result_data_store[Demfishmig_propbiom_id,])
	y_min4<-min(Result_data_store[Birdmig_propbiom_id,])
	y_min5<-min(Result_data_store[Sealmig_propbiom_id,])
	y_min6<-min(Result_data_store[Cetamig_propbiom_id,])


	#In each set:
	#Row1 = maximum likelihood model result
	#Row2 = 0.005 centile
	#Row3 = lower quartile
	#Row4 = median
	#Row5 = upper quartile
	#Row6 = 0.995 centile

	#-----------------------------------------------
	tsplotMIG<-function(tspmain,axtitle,VtoGet,ymin,ymax){
		par(mar=c(3.5,5.4,2.1,0.7))

		yml<-as.numeric(Result_data_store[VtoGet[1],])
		yll<-as.numeric(Result_data_store[VtoGet[2],])
		ylq<-as.numeric(Result_data_store[VtoGet[3],])
		ymd<-as.numeric(Result_data_store[VtoGet[4],])
		yuq<-as.numeric(Result_data_store[VtoGet[5],])
		yul<-as.numeric(Result_data_store[VtoGet[6],])

		poly_y<-c(ylq,yuq[361:1],ylq[1])
		poly_x<-c(seq(0,360),seq(360,0,by=-1),0)

		if(ymax>0) y_max<-ymax
		if(ymax<0) y_max<-0
		if(ymax==0) y_max<-1.05*max(yul)

		if(ymin>0) y_min<-0
		if(ymin<0) y_min<-1.05*ymin
		if(ymin==0) y_min<-0

		plot(seq(0,360),yul,ylim=c(y_min,y_max),type="l",lty="dashed",lwd=1.1,col="black",xaxt="n",yaxt="n",ann=FALSE,main=tspmain)
		polygon(poly_x,poly_y,col="grey",border="grey")
		lines(seq(0,360),yll,type="l",lty="dashed",lwd=1.1,col="black")
		lines(seq(0,360),ymd,type="l",lwd=2,col="black")
		lines(seq(0,360),yml,type="l",lwd=1.1,col="red")
		abline(a=0,b=0)

		axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=1)
		axis(side=2,las=1,cex.axis=0.9)
		mtext("Days",cex=0.8,side=1,line=2)
		mtext(axtitle,cex=0.8,side=2,line=3.95)
		mtext(tspmain,cex=0.85,side=3,line=0.5)
	}

	#MIGRATION FLUXES

	unitsize<-0.55
	unitline<- -0.2
	unitadj<- -1.25
	par(mfrow=c(2,3))

	VtoGet<-Pelfishmig_propbiom_id
	tspmain<-"Planktivorous fish"
	axtitle<-"Active migration flux"
	tsplotMIG(tspmain,axtitle,VtoGet,y_min1,y_max1)
	legend("topright",  bty="n","Offshore to inshore",cex=0.9)
	legend("bottomright",  bty="n","Inshore to offshore",cex=0.9)
#		mtext(bquote("mMN.m"^-2 ~ ".d"^-1),cex=unitsize,side=3,line=unitline,adj=unitadj)


	VtoGet<-Migfishmig_propbiom_id
	tspmain<-"Migratory fish"
	axtitle<-"Active migration flux"
	tsplotMIG(tspmain,axtitle,VtoGet,y_min2,y_max2)
#		mtext(bquote("mMN.m"^-2 ~ ".d"^-1),cex=unitsize,side=3,line=unitline,adj=unitadj)

	VtoGet<-Demfishmig_propbiom_id
	tspmain<-"Demersal fish"
	axtitle<-"Active migration flux"
	tsplotMIG(tspmain,axtitle,VtoGet,y_min3,y_max3)
#		mtext(bquote("mMN.m"^-2 ~ ".d"^-1),cex=unitsize,side=3,line=unitline,adj=unitadj)

	VtoGet<-Birdmig_propbiom_id
	tspmain<-"Birds"
	axtitle<-"Active migration flux"
	tsplotMIG(tspmain,axtitle,VtoGet,y_min4,y_max4)
#		mtext(bquote("mMN.m"^-2 ~ ".d"^-1),cex=unitsize,side=3,line=unitline,adj=unitadj)

	VtoGet<-Sealmig_propbiom_id
	tspmain<-"Pinnipeds"
	axtitle<-"Active migration flux"
	tsplotMIG(tspmain,axtitle,VtoGet,y_min5,y_max5)
#		mtext(bquote("mMN.m"^-2 ~ ".d"^-1),cex=unitsize,side=3,line=unitline,adj=unitadj)

	VtoGet<-Cetamig_propbiom_id
	tspmain<-"Cetaceans"
	axtitle<-"Active migration flux"
	tsplotMIG(tspmain,axtitle,VtoGet,y_min6,y_max6)
#		mtext(bquote("mMN.m"^-2 ~ ".d"^-1),cex=unitsize,side=3,line=unitline,adj=unitadj)

}

