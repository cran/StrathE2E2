#
# build_annual_drivers.R
#
#' uses monthly values of time-series drivers over a climatological annual cycle
#' and makes them into functions for #interpolating values at any given time.
#'
#' returns set of interpolating functions
#'
#' @param run model run object
#' @param fixed.parms fixed parameters
#' @param physical.parms physical parameters
#' @param physics.drivers physics drivers
#' @param chemistry.drivers chemistry drivers
#' @param biological.events biological timing events
#'
#' @return driver functions
#'
#' @importFrom stats rnorm approxfun
#'
#' @noRd
#
# ------------------------------------------------------------------------------

build_annual_drivers <- function(run, fixed.parms, physical.parms, physics.drivers, chemistry.drivers, biological.events) {

	# run:
	nyears		<- elt(run, "nyears")
	ndays		<- elt(run, "ndays")
	drndays		<- elt(run, "drndays")
	drtimes		<- elt(run, "drtimes")
	sprectimes	<- elt(run, "sprectimes")

	# physical.parms:
	P_an_fec	<- elt(fixed.parms, "P_an_fec")
	D_an_fec	<- elt(fixed.parms, "D_an_fec")
	BC_an_fec	<- elt(fixed.parms, "BC_an_fec")
	BS_an_fec	<- elt(fixed.parms, "BS_an_fec")

	# physical.parms:
	so_depth	<- elt(physical.parms, "so_depth")
	si_depth	<- elt(physical.parms, "si_depth")
	d_depth		<- elt(physical.parms, "d_depth")
	x_shallowprop	<- elt(physical.parms, "x_shallowprop")

	physics		<- physics.drivers
	boundconc	<- chemistry.drivers
	BETdata		<- biological.events

	#Annual fecundities need to have alreday been read in from the fixed parameter file

	#Set the fish and benthos spawning dates and durations, and recruitment dates and durations, and fecundities (max gonad wt /unit total wt)
	#-----------------------------------------------------------------------------------------------------------------------------

	#Pelagic fish
	P_sp_start<- BETdata[1,1]
	P_sp_dur<-BETdata[2,1]
	P_rec_start<-BETdata[3,1]
	P_rec_dur<-BETdata[4,1]
	#P_an_fec<-HWPdata[37,1]

	#Demersal fish
	D_sp_start<- BETdata[5,1]
	D_sp_dur<-BETdata[6,1]
	D_rec_start<-BETdata[7,1]
	D_rec_dur<-BETdata[8,1]
	#D_an_fec<-HWPdata[38,1]

	BS_sp_start<-BETdata[9,1]
	BS_sp_dur<-BETdata[10,1]
	BS_rec_start<-BETdata[11,1]
	BS_rec_dur<-BETdata[12,1]
	#BS_an_fec<-HWPdata[39,1]

	BC_sp_start<-BETdata[13,1]
	BC_sp_dur<-BETdata[14,1]
	BC_rec_start<-BETdata[15,1]
	BC_rec_dur<-BETdata[16,1]
	#BC_an_fec<-HWPdata[40,1]


	#Set the migratory fish imigration and emigration time and rates
	#---------------------------------------------------------------

	#SWITCH (value 0 or 1) to turn off or on the code for imigration and emigration
	migfish_oceanscale    <- BETdata[17,1]

	#Compute the nitrogen mass of the large scale population supplying imigrants to the model domain (mMN)
	migfish_oceanbio    <- BETdata[18,1]           # Tonnes wet weight
	migfish_C_ww        <- BETdata[19,1]             # gC/gWW
	ssarea              <- BETdata[20,1]*1000000    # model domain surface area m2 (North Sea = km2)
	migfish_oceanpop    <- (migfish_oceanbio*1000000*migfish_C_ww/12)*1000*(16/106)    # units mMN
	#migfish_oceanpop/ssarea   # Test.... mMN/m2

	#Proportion of the large scale popualtion migrating into the model domain each year
	migfish_ocean_prop_entering    <- BETdata[21,1]

	#Start date for imigration
	migfish_im_start    <- BETdata[22,1]

	#End day for imigration (must be later than start day even if migration is disabled)
	migfish_im_end    <- BETdata[23,1]

	#Proportion of peak population within the model domain which remains and doe snot emigrate
	migfish_ocean_prop_staying    <- BETdata[24,1]

	#Start date for emigration (emigration can be earlier in the year than imigration - ie immigrants are present across 1 January)
	migfish_em_start    <- BETdata[25,1]

	#End day for emigration (must be later than start day even if migration is disabled)
	migfish_em_end    <- BETdata[26,1]

	#.....................................



	#]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]


	#Generate time dependent interpolation functions for each driver

	#The drivers needed are.....
	#Light at the sea surface
	#SPM in the surface layer"
	#vertical exchange coefficient derived from "logkvert","mixlscale"
	#sinflow
	#dinflow
	#rivervol
	#stemp
	#dtemp
	#Proportion upwelling
	#Surface outflow
	#Deep outflow
	#atmnitrate flux
	#atmammonia flux

	#Boundary concentration values of..
	#snitrate
	#sammonia
	#surface phyto
	#surface flag
	#surface uzoo
	#surface detritus
	#dnitrate
	#dammonia
	#deep phyto
	#deep flag
	#deep uzoo
	#deep detritus
	#rivnitrate
	#rivammona
	#rivdetritus




	pdriver<-data.frame(rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),
                    rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),
                    rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),
                    rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays))
	#names(pdriver)<-c("sslight","logespm","v_dif","s_inflow","d_inflow","prop_up",
	#                  "river","stemp","dtemp","atm_nit","atm_amm","s1_pd","s2_pd","s3_pd","d1_pd","d2_pd","d3_pd")

	names(pdriver)<-c("sslight","so_logespm","si_logespm","so_temp","d_temp","si_temp","river",
                  "v_dif","prop_up","so_inflow","d_inflow","si_inflow","si_outflow","so_si_flow",
                  "so_atm_nit","so_atm_amm","si_atm_nit","si_atm_amm",
                  "d1_pd","d2_pd","d3_pd","s1_pd","s2_pd","s3_pd","s_wave")





	cdriver<-data.frame(rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays),rep(0,drndays))
	#names(cdriver)<-c("snit","samm","sphyt","sflag","suzoo","sdet","dnit","damm","dphyt","dflag","duzoo","ddet","riv_nit","riv_amm","riv_det")

	names(cdriver)<-c("so_nit","so_amm","so_phyt","so_det",
                    "d_nit","d_amm","d_phyt","d_det",
                    "si_nit","si_amm","si_phyt","si_det",
                    "riv_nit","riv_amm","riv_det")



	#bdriver<-data.frame(rep(0,ndays),rep(0,ndays),rep(0,ndays),rep(0,ndays))
	#names(bdriver)<-c("pfish_sp","dfish_sp","pfish_rec","dfish_rec")
	bdriver<-data.frame(rep(0,ndays),rep(0,ndays),rep(0,ndays),rep(0,ndays),rep(0,ndays),rep(0,ndays),rep(0,ndays),rep(0,ndays),rep(0,ndays),rep(0,ndays))
	names(bdriver)<-c("pfish_sp","dfish_sp","pfish_rec","dfish_rec","bc_sp","bc_rec","bs_sp","bs_rec","migfish_im","migfish_em")

	#Light.... and SPM

	pdriver$sslight[1:drndays]<-rep(physics$sslight,nyears)
	driversslight <- approxfun(drtimes, pdriver$sslight, rule=2)

	pdriver$so_logespm[1:drndays]<-rep(physics$so_logespm,nyears)
	driverso_logespm <- approxfun(drtimes, pdriver$so_logespm, rule=2)

	pdriver$si_logespm[1:drndays]<-rep(physics$si_logespm,nyears)
	driversi_logespm <- approxfun(drtimes, pdriver$si_logespm, rule=2)


	#Offshore Surface temperature....
	pdriver$so_temp[1:drndays]<-rep(physics$so_temp,nyears)
	driverso_temp  <- approxfun(drtimes, pdriver$so_temp, rule=2)

	#Deep temperature....
	pdriver$d_temp[1:drndays]<-rep(physics$d_temp,nyears)
	driverd_temp  <- approxfun(drtimes, pdriver$d_temp, rule=2)

	#Inshore Surface temperature....
	pdriver$si_temp[1:drndays]<-rep(physics$si_temp,nyears)
	driversi_temp  <- approxfun(drtimes, pdriver$si_temp, rule=2)





	#Vertical diffusion...
	physics$vdif<-(10^(physics$logkvert))/(physics$mixlscale*(so_depth+d_depth))
	pdriver$v_dif[1:drndays]<-rep(physics$vdif,nyears)
	driverv_dif <- approxfun(drtimes, pdriver$v_dif, rule=2)


	#Offshore Surface inflow volume.....
	pdriver$so_inflow[1:drndays]<-rep(physics$so_inflow*so_depth*(1-x_shallowprop),nyears)
	driverso_inflow  <- approxfun(drtimes, pdriver$so_inflow, rule=2)

	#Offshore Deep inflow volume...
	pdriver$d_inflow[1:drndays]<-rep(physics$d_inflow*d_depth*(1-x_shallowprop),nyears)
	driverd_inflow  <- approxfun(drtimes, pdriver$d_inflow, rule=2)

	#Inshore Surface inflow volume.....
	pdriver$si_inflow[1:drndays]<-rep(physics$si_inflow*si_depth*(x_shallowprop),nyears)
	driversi_inflow  <- approxfun(drtimes, pdriver$si_inflow, rule=2)

	#Inshore Surface outflow volume.....
	pdriver$si_outflow[1:drndays]<-rep(physics$si_outflow*si_depth*(x_shallowprop),nyears)
	driversi_outflow  <- approxfun(drtimes, pdriver$si_outflow, rule=2)


	#ORIGINAL VERSION - NEEDS TO BE ATTENUATED TO ACCOUNT FOR MIXING LENGTH SCALE IN INSHORE WATERS
	##Offshore-inshore flow volume.....
	#pdriver$so_si_flow[1:drndays]<-rep(physics$so_si_flow*si_depth*(x_shallowprop),nyears)
	#driverso_si_flow  <- approxfun(drtimes, pdriver$so_si_flow, rule=2)

	#REVISED VERSION - ACCOUNTING FOR MIXING LENGTH SCALE IN INSHORE WATERS
	#ASSUMES INFLOW VOLUME IS ATTENUATED BY THE SQUARE OF THE VOLUME OF INSHORE WATERS
	#Offshore-inshore flow volume.....
	attenuated_so_si_inflow <- 1/((1/physics$so_si_flow)^2)
	pdriver$so_si_flow[1:drndays]<-rep(attenuated_so_si_inflow*si_depth*(x_shallowprop),nyears)
	driverso_si_flow  <- approxfun(drtimes, pdriver$so_si_flow, rule=2)


	#Upwelling fraction of deep inflow....
	pdriver$prop_up[1:drndays]<-rep(physics$upwelling,nyears)
	drivers_upwell <- approxfun(drtimes, (pdriver$prop_up*pdriver$d_inflow), rule=2)

	#Derived outflow from si to so - NOT NEEDED HERE ANY MORE
	#driversi_so_flow <- approxfun(drtimes, pdriver$so_si_flow + pdriver$si_inflow - pdriver$si_outflow, rule=2)

	#Derived surface and deep boundary outflow volumes......  - NOT NEEDED HERE ANY MORE
	#driverso_outflow <- approxfun(drtimes, (pdriver$so_inflow + pdriver$prop_up*pdriver$d_inflow + pdriver$si_inflow - pdriver$si_outflow ), rule=2)
	#KEEP THIS
	driverd_outflow <-  approxfun(drtimes, (1-pdriver$prop_up)*pdriver$d_inflow, rule=2)

	#~~~~~~~~~~~~~~~

	#Some checking to see if we have volume conservation
	# ttt<-seq(1,360)
	# driverso_inflow(ttt)
	# driverd_inflow(ttt)
	# driversi_inflow(ttt)
	
	# driverso_outflow(ttt)
	# driverd_outflow(ttt)
	# driversi_outflow(ttt)

 	#Volume conservation for the whole system
	# plot( (driverso_inflow(ttt) + driverd_inflow(ttt) + driversi_inflow(ttt) - driverso_outflow(ttt) - driverd_outflow(ttt) - driversi_outflow(ttt)),type="l")
	# abline(h=0)
	# sum( ( (driverso_inflow(ttt) + driverd_inflow(ttt) + driversi_inflow(ttt) - driverso_outflow(ttt) - driverd_outflow(ttt) - driversi_outflow(ttt)) ) )
	
 	#Volume conservation for the offshore surface layer
	# plot( (driverso_inflow(ttt) + drivers_upwell(ttt) + driversi_so_flow(ttt) - driverso_outflow(ttt) - driverso_si_flow(ttt)),type="l")
	# abline(h=0)
	# sum( ( (driverso_inflow(ttt) + drivers_upwell(ttt) + driversi_so_flow(ttt) - driverso_outflow(ttt) - driverso_si_flow(ttt)) ) )

 	#Volume conservation forthe inshore zone
	# plot( (driversi_inflow(ttt) + driverso_si_flow(ttt) - driversi_outflow(ttt) - driversi_so_flow(ttt)),type="l")
	# abline(h=0)
	# sum( ( (driversi_inflow(ttt) + driverso_si_flow(ttt) - driversi_outflow(ttt) - driversi_so_flow(ttt)) ) )

 	#Volume conservation fof the ofshore deep layer
	# plot( (driverd_inflow(ttt) - drivers_upwell(ttt) - driverd_outflow(ttt)) , type="l")
	# abline(h=0)
	# sum( ( (driverd_inflow(ttt) - drivers_upwell(ttt) - driverd_outflow(ttt)) ) )
	
	#~~~~~~~~~~~~~~~


	
	#River volume inflow volume...
	pdriver$river[1:drndays]<-rep(physics$rivervol*si_depth*(x_shallowprop),nyears)
	driverriver  <- approxfun(drtimes, pdriver$river, rule=2)



	#Sediment natural disturbance fraction per day deep sediments
	pdriver$d1_pd[1:drndays]<-rep(physics$d1_pdist,nyears)
	driver_d1_pd <- approxfun(drtimes, pdriver$d1_pd, rule=2)

	#Sediment natural disturbance fraction per day deep sediments
	pdriver$d2_pd[1:drndays]<-rep(physics$d2_pdist,nyears)
	driver_d2_pd <- approxfun(drtimes, pdriver$d2_pd, rule=2)

	#Sediment natural disturbance fraction per day deep sediments
	pdriver$d3_pd[1:drndays]<-rep(physics$d3_pdist,nyears)
	driver_d3_pd <- approxfun(drtimes, pdriver$d3_pd, rule=2)



	#Sediment natural disturbance fraction per day shallow sediments
	pdriver$s1_pd[1:drndays]<-rep(physics$s1_pdist,nyears)
	driver_s1_pd <- approxfun(drtimes, pdriver$s1_pd, rule=2)

	#Sediment natural disturbance fraction per day shallow sediments
	pdriver$s2_pd[1:drndays]<-rep(physics$s2_pdist,nyears)
	driver_s2_pd <- approxfun(drtimes, pdriver$s2_pd, rule=2)

	#Sediment natural disturbance fraction per day shallow sediments
	pdriver$s3_pd[1:drndays]<-rep(physics$s3_pdist,nyears)
	driver_s3_pd <- approxfun(drtimes, pdriver$s3_pd, rule=2)

	#Inshore wave height
	pdriver$s_wave[1:drndays]<-rep(physics$Inshore_waveheight,nyears)
	drivers_wave <- approxfun(drtimes, pdriver$s_wave, rule=2)


	#Offshore Atmospheric nitrate flux
	pdriver$so_atm_nit[1:drndays]<-rep(boundconc$so_atmnitrate,nyears)
	driverso_atm_nit <- approxfun(drtimes, pdriver$so_atm_nit, rule=2)

	#Offshore Atmospheric ammonia flux
	pdriver$so_atm_amm[1:drndays]<-rep(boundconc$so_atmammonia,nyears)
	driverso_atm_amm <- approxfun(drtimes, pdriver$so_atm_amm, rule=2)

	#Inshore Atmospheric PLUS OTHER nitrate flux
	pdriver$si_atm_nit[1:drndays]<-rep((boundconc$si_atmnitrate+boundconc$si_othernitrate),nyears)
	driversi_atm_nit <- approxfun(drtimes, pdriver$si_atm_nit, rule=2)

	#Inshore Atmospheric PLUS OTHER ammonia flux
	pdriver$si_atm_amm[1:drndays]<-rep((boundconc$si_atmammonia+boundconc$si_otherammonia),nyears)
	driversi_atm_amm <- approxfun(drtimes, pdriver$si_atm_amm, rule=2)




	#Boundary offshore surface nitrate concentrations
	cdriver$so_nit[1:drndays]<-rep(boundconc$so_nitrate,nyears)
	driverboundso_nit <- approxfun(drtimes, cdriver$so_nit, rule=2)

	#Boundary offshore surface ammonia concentrations
	cdriver$so_amm[1:drndays]<-rep(boundconc$so_ammonia,nyears)
	driverboundso_amm <- approxfun(drtimes, cdriver$so_amm, rule=2)

	#Boundary offshore surface phyt concentrations
	cdriver$so_phyt[1:drndays]<-rep(boundconc$so_phyt,nyears)
	driverboundso_phyt <- approxfun(drtimes, cdriver$so_phyt, rule=2)

	#Boundary offshore surface detritus concentrations
	cdriver$so_det[1:drndays]<-rep(boundconc$so_detritus,nyears)
	driverboundso_det <- approxfun(drtimes, cdriver$so_det, rule=2)

	
	#Boundary offshore deep nitrate concentrations
	cdriver$d_nit[1:drndays]<-rep(boundconc$d_nitrate,nyears)
	driverboundd_nit <- approxfun(drtimes, cdriver$d_nit, rule=2)

	#Boundary offshore deep ammonia concentrations
	cdriver$d_amm[1:drndays]<-rep(boundconc$d_ammonia,nyears)
	driverboundd_amm <- approxfun(drtimes, cdriver$d_amm, rule=2)

	#Boundary offshore deep phyt concentrations
	cdriver$d_phyt[1:drndays]<-rep(boundconc$d_phyt,nyears)
	driverboundd_phyt <- approxfun(drtimes, cdriver$d_phyt, rule=2)

	#Boundary offshore deep detritus concentrations
	cdriver$d_det[1:drndays]<-rep(boundconc$d_detritus,nyears)
	driverboundd_det <- approxfun(drtimes, cdriver$d_det, rule=2)


	#Boundary inshore surface nitrate concentrations
	cdriver$si_nit[1:drndays]<-rep(boundconc$si_nitrate,nyears)
	driverboundsi_nit <- approxfun(drtimes, cdriver$si_nit, rule=2)

	#Boundary inshore surface ammonia concentrations
	cdriver$si_amm[1:drndays]<-rep(boundconc$si_ammonia,nyears)
	driverboundsi_amm <- approxfun(drtimes, cdriver$si_amm, rule=2)

	#Boundary inshore surface phyt concentrations
	cdriver$si_phyt[1:drndays]<-rep(boundconc$si_phyt,nyears)
	driverboundsi_phyt <- approxfun(drtimes, cdriver$si_phyt, rule=2)

	#Boundary inshore surface detritus concentrations
	cdriver$si_det[1:drndays]<-rep(boundconc$si_detritus,nyears)
	driverboundsi_det <- approxfun(drtimes, cdriver$si_det, rule=2)





	#Boundary river nitrate concentrations
	cdriver$riv_nit[1:drndays]<-rep(boundconc$rivnitrate,nyears)
	driverboundriv_nit <- approxfun(drtimes, cdriver$riv_nit, rule=2)

	#Boundary river ammonia concentrations
	cdriver$riv_amm[1:drndays]<-rep(boundconc$rivammonia,nyears)
	driverboundriv_amm <- approxfun(drtimes, cdriver$riv_amm, rule=2)

	#Boundary river detritus concentrations
	cdriver$riv_det[1:drndays]<-rep(boundconc$rivdetritus,nyears)
	driverboundriv_det <- approxfun(drtimes, cdriver$riv_det, rule=2)




	#Set the fish spawning and recruitment rate driving data

	#Spawning first
	pspawn<-rep(0,360)
	dspawn<-rep(0,360)
	#pspawn[P_sp_start:(P_sp_start+P_sp_dur)]<-P_an_fec/P_sp_dur
	#dspawn[D_sp_start:(D_sp_start+D_sp_dur)]<-D_an_fec/D_sp_dur
	pspawn[P_sp_start:(P_sp_start+P_sp_dur)]<-1/P_sp_dur         # fecundity transferred to parameter list
	dspawn[D_sp_start:(D_sp_start+D_sp_dur)]<-1/D_sp_dur         # fecundity transferred to parameter list
	bdriver$pfish_sp[1:ndays-1]<-rep(pspawn,nyears)
	bdriver$dfish_sp[1:ndays-1]<-rep(dspawn,nyears)

	#Set recruitment
	prec<-rep(0,360)
	drec<-rep(0,360)
	prec[P_rec_start:(P_rec_start+P_rec_dur)]<-0.1
	drec[D_rec_start:(D_rec_start+D_rec_dur)]<-0.1
	bdriver$pfish_rec[1:ndays-1]<-rep(prec,nyears)
	bdriver$dfish_rec[1:ndays-1]<-rep(drec,nyears)
	driverpfish_sp <- approxfun(sprectimes, bdriver$pfish_sp, rule=2)
	driverdfish_sp <- approxfun(sprectimes, bdriver$dfish_sp, rule=2)
	driverpfish_rec <- approxfun(sprectimes, bdriver$pfish_rec, rule=2)
	driverdfish_rec <- approxfun(sprectimes, bdriver$dfish_rec, rule=2)

	#Set the benthos spawning and recruitment rate driving data
	
	#Spawning first
	bcspawn<-rep(0,360)
	bsspawn<-rep(0,360)
	#bcspawn[BC_sp_start:(BC_sp_start+BC_sp_dur)]<-BC_an_fec/BC_sp_dur
	#bsspawn[BS_sp_start:(BS_sp_start+BS_sp_dur)]<-BS_an_fec/BS_sp_dur
	bcspawn[BC_sp_start:(BC_sp_start+BC_sp_dur)]<-1/BC_sp_dur         # fecundity transferred to parameter list
	bsspawn[BS_sp_start:(BS_sp_start+BS_sp_dur)]<-1/BS_sp_dur         # fecundity transferred to parameter list
	bdriver$bc_sp[1:ndays-1]<-rep(bcspawn,nyears)
	bdriver$bs_sp[1:ndays-1]<-rep(bsspawn,nyears)

	#Set recruitment
	#bcrec<-rep(0.05,360)
	#bsrec<-rep(0.05,360)
	#bcrec[BC_sp_start:(BC_sp_start+BC_sp_dur)]<-0.0
	#bcrec[BC_rec_start:(BC_rec_start+BC_rec_dur)]<-0.05
	#bsrec[BS_sp_start:(BS_sp_start+BS_sp_dur)]<-0.0
	#bsrec[BS_rec_start:(BS_rec_start+BS_rec_dur)]<-0.05

	bcrec<-rep(0,360)
	bsrec<-rep(0,360)
	bcrec[BC_rec_start:(BC_rec_start+BC_rec_dur)]<-0.1
	bsrec[BS_rec_start:(BS_rec_start+BS_rec_dur)]<-0.1

	bdriver$bc_rec[1:ndays-1]<-rep(bcrec,nyears)
	bdriver$bs_rec[1:ndays-1]<-rep(bsrec,nyears)
	driverbc_sp <- approxfun(sprectimes, bdriver$bc_sp, rule=2)
	driverbs_sp <- approxfun(sprectimes, bdriver$bs_sp, rule=2)
	driverbc_rec <- approxfun(sprectimes, bdriver$bc_rec, rule=2)
	driverbs_rec <- approxfun(sprectimes, bdriver$bs_rec, rule=2)



	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	# Set up the migratory fish imigration rate driving functions

	migfish_imig    <- rep(0,360)
	migfish_imig[migfish_im_start:migfish_im_end]<-(migfish_oceanpop*migfish_oceanscale/ssarea)*migfish_ocean_prop_entering/(migfish_im_end-migfish_im_start)
	bdriver$migfish_im[1:ndays-1]<- rep(migfish_imig,nyears)
	driver_migfish_imig <- approxfun(sprectimes, bdriver$migfish_im, rule=2)

	# ........................

	# Set up the migratory fish emigration rate driving functions
	
	migfish_emig    <- rep(0,360)
	migfish_emig[migfish_em_start:migfish_em_end]<-migfish_oceanscale*(-log(migfish_ocean_prop_staying))/(migfish_em_end-migfish_em_start)
	bdriver$migfish_em[1:ndays-1]<- rep(migfish_emig,nyears)
	driver_migfish_emig <- approxfun(sprectimes, bdriver$migfish_em, rule=2)
	
	# ........................

	drivers <- list(
		driversslight		= driversslight,
        	driverso_logespm	= driverso_logespm,
        	driversi_logespm	= driversi_logespm,
        	driverso_temp 		= driverso_temp,
        	driverd_temp 		= driverd_temp,
        	driversi_temp 		= driversi_temp,
        	driverv_dif		= driverv_dif,
        	driverso_inflow 	= driverso_inflow,
        	driverd_inflow 		= driverd_inflow,
        	driversi_inflow 	= driversi_inflow,
        	driversi_outflow 	= driversi_outflow,
        	driverso_si_flow 	= driverso_si_flow,
        	drivers_upwell		= drivers_upwell,
        	driverd_outflow		= driverd_outflow,
        	driverriver 		= driverriver,
        	driver_d1_pd		= driver_d1_pd,
        	driver_d2_pd		= driver_d2_pd,
        	driver_d3_pd		= driver_d3_pd,
        	driver_s1_pd		= driver_s1_pd,
        	driver_s2_pd		= driver_s2_pd,
        	driver_s3_pd		= driver_s3_pd,
		drivers_wave		= drivers_wave,
        	driverso_atm_nit	= driverso_atm_nit,
        	driverso_atm_amm	= driverso_atm_amm,
        	driversi_atm_nit	= driversi_atm_nit,
        	driversi_atm_amm	= driversi_atm_amm,
        	driverboundso_nit	= driverboundso_nit,
        	driverboundso_amm	= driverboundso_amm,
        	driverboundso_phyt	= driverboundso_phyt,
        	driverboundso_det	= driverboundso_det,
        	driverboundd_nit	= driverboundd_nit,
        	driverboundd_amm	= driverboundd_amm,
        	driverboundd_phyt	= driverboundd_phyt,
        	driverboundd_det	= driverboundd_det,
        	driverboundsi_nit	= driverboundsi_nit,
        	driverboundsi_amm	= driverboundsi_amm,
        	driverboundsi_phyt	= driverboundsi_phyt,
        	driverboundsi_det	= driverboundsi_det,
        	driverboundriv_nit	= driverboundriv_nit,
        	driverboundriv_amm	= driverboundriv_amm,
        	driverboundriv_det	= driverboundriv_det,
        	driverpfish_sp		= driverpfish_sp,
        	driverdfish_sp		= driverdfish_sp,
        	driverpfish_rec		= driverpfish_rec,
        	driverdfish_rec		= driverdfish_rec,
        	driverbc_sp		= driverbc_sp,
        	driverbs_sp		= driverbs_sp,
        	driverbc_rec		= driverbc_rec,
	        driverbs_rec		= driverbs_rec,
        	driver_migfish_imig	= driver_migfish_imig,
        	driver_migfish_emig	= driver_migfish_emig
        )
}


