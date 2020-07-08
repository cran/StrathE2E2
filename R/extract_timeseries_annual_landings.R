#
# extract_timeseries_annual_landings.R
#
#' read designated model
#'
#' returns a model object with run and data slots
#'
#' @param model current model
#' @param build model build object
#' @param out model output
#'
#' @return inshore/offshore annual landings
#'
#' @noRd
#
# ------------------------------------------------------------------------------

extract_timeseries_annual_landings <- function(model, build, out) {

	setup		<- elt(model, "setup")
	identifier	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

	run		<- elt(build, "run")
	nyears		<- elt(run, "nyears")

	#Print some of the full time series data to a csv file
	#-----------------------------------------------------------------

	offshore_annual_group_land_disc<-data.frame(year=seq(1,nyears))

	offshore_annual_group_land_disc$PFland<-rep(0,nyears)
	offshore_annual_group_land_disc$DFQland<-rep(0,nyears)
	offshore_annual_group_land_disc$DFNQland<-rep(0,nyears)
	offshore_annual_group_land_disc$MFland<-rep(0,nyears)
	offshore_annual_group_land_disc$SBland<-rep(0,nyears)
	offshore_annual_group_land_disc$CBland<-rep(0,nyears)
	offshore_annual_group_land_disc$CZland<-rep(0,nyears)
	offshore_annual_group_land_disc$BDland<-rep(0,nyears)
	offshore_annual_group_land_disc$SLland<-rep(0,nyears)
	offshore_annual_group_land_disc$CTland<-rep(0,nyears)
	offshore_annual_group_land_disc$KPland<-rep(0,nyears)

	offshore_annual_group_land_disc$PFdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$DFQdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$DFNQdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$MFdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$SBdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$CBdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$CZdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$BDdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$SLdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$CTdisc<-rep(0,nyears)
	offshore_annual_group_land_disc$KPdisc<-rep(0,nyears)

	inshore_annual_group_land_disc <- offshore_annual_group_land_disc

	for(ik in 1:nyears){

		offshore_annual_group_land_disc$PFland[ik] <- out$landp_o[ (1+(ik*360)) ] - out$landp_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$DFQland[ik] <- out$landd_quota_o[ (1+(ik*360)) ] - out$landd_quota_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$DFNQland[ik] <- out$landd_nonquota_o[ (1+(ik*360)) ] - out$landd_nonquota_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$MFland[ik] <- out$landm_o[ (1+(ik*360)) ] - out$landm_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$SBland[ik] <- out$landsb_o[ (1+(ik*360)) ] - out$landsb_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$CBland[ik] <- out$landcb_o[ (1+(ik*360)) ] - out$landcb_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$CZland[ik] <- out$landcz_o[ (1+(ik*360)) ] - out$landcz_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$BDland[ik] <- out$landbd_o[ (1+(ik*360)) ] - out$landbd_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$SLland[ik] <- out$landsl_o[ (1+(ik*360)) ] - out$landsl_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$CTland[ik] <- out$landct_o[ (1+(ik*360)) ] - out$landct_o[ (1+(ik-1)*360) ]
		#No offshore landings of kelp

		offshore_annual_group_land_disc$PFdisc[ik] <- out$discpel_o[ (1+(ik*360)) ] - out$discpel_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$DFQdisc[ik] <- out$discdem_quota_o[ (1+(ik*360)) ] - out$discdem_quota_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$DFNQdisc[ik] <- out$discdem_nonquota_o[ (1+(ik*360)) ] - out$discdem_nonquota_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$MFdisc[ik] <- out$discmig_o[ (1+(ik*360)) ] - out$discmig_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$SBdisc[ik] <- out$discsb_o[ (1+(ik*360)) ] - out$discsb_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$CBdisc[ik] <- out$disccb_o[ (1+(ik*360)) ] - out$disccb_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$CZdisc[ik] <- out$disccz_o[ (1+(ik*360)) ] - out$disccz_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$BDdisc[ik] <- out$discbd_o[ (1+(ik*360)) ] - out$discbd_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$SLdisc[ik] <- out$discsl_o[ (1+(ik*360)) ] - out$discsl_o[ (1+(ik-1)*360) ]
		offshore_annual_group_land_disc$CTdisc[ik] <- out$discct_o[ (1+(ik*360)) ] - out$discct_o[ (1+(ik-1)*360) ]
		#No offshore discards of kelp


		inshore_annual_group_land_disc$PFland[ik] <- out$landp_i[ (1+(ik*360)) ] - out$landp_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$DFQland[ik] <- out$landd_quota_i[ (1+(ik*360)) ] - out$landd_quota_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$DFNQland[ik] <- out$landd_nonquota_i[ (1+(ik*360)) ] - out$landd_nonquota_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$MFland[ik] <- out$landm_i[ (1+(ik*360)) ] - out$landm_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$SBland[ik] <- out$landsb_i[ (1+(ik*360)) ] - out$landsb_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$CBland[ik] <- out$landcb_i[ (1+(ik*360)) ] - out$landcb_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$CZland[ik] <- out$landcz_i[ (1+(ik*360)) ] - out$landcz_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$BDland[ik] <- out$landbd_i[ (1+(ik*360)) ] - out$landbd_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$SLland[ik] <- out$landsl_i[ (1+(ik*360)) ] - out$landsl_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$CTland[ik] <- out$landct_i[ (1+(ik*360)) ] - out$landct_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$KPland[ik] <- out$landkp_i[ (1+(ik*360)) ] - out$landkp_i[ (1+(ik-1)*360) ]

		inshore_annual_group_land_disc$PFdisc[ik] <- out$discpel_i[ (1+(ik*360)) ] - out$discpel_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$DFQdisc[ik] <- out$discdem_quota_i[ (1+(ik*360)) ] - out$discdem_quota_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$DFNQdisc[ik] <- out$discdem_nonquota_i[ (1+(ik*360)) ] - out$discdem_nonquota_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$MFdisc[ik] <- out$discmig_i[ (1+(ik*360)) ] - out$discmig_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$SBdisc[ik] <- out$discsb_i[ (1+(ik*360)) ] - out$discsb_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$CBdisc[ik] <- out$disccb_i[ (1+(ik*360)) ] - out$disccb_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$CZdisc[ik] <- out$disccz_i[ (1+(ik*360)) ] - out$disccz_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$BDdisc[ik] <- out$discbd_i[ (1+(ik*360)) ] - out$discbd_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$SLdisc[ik] <- out$discsl_i[ (1+(ik*360)) ] - out$discsl_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$CTdisc[ik] <- out$discct_i[ (1+(ik*360)) ] - out$discct_i[ (1+(ik-1)*360) ]
		inshore_annual_group_land_disc$KPdisc[ik] <- out$disckp_i[ (1+(ik*360)) ] - out$disckp_i[ (1+(ik-1)*360) ]
	}

	filename = csvname(resultsdir, "model_inshore_annual_landings_discards", identifier)
	writecsv(inshore_annual_group_land_disc, filename, row.names=FALSE)

	filename = csvname(resultsdir, "model_offshore_annual_landings_discards", identifier)
	writecsv(offshore_annual_group_land_disc, filename, row.names=FALSE)

	list(
		offshore_annual_group_land_disc	= offshore_annual_group_land_disc,
		inshore_annual_group_land_disc = inshore_annual_group_land_disc
	)
}


