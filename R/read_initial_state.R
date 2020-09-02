#
# read_initial_state.R
#
#' read and set the initial values of the state variables used in the model
#'
#' Initial values are set by specifying the initial surface water nitrate concentration, and
#' then the ratios of all the other state variables to this value.
#'
#' @param model.path path to model files
#'
#' @return initial state variables
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_initial_state <- function(model.path) {

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Subroutine to load and set the initial values of the state variable sin the model.
	#This process also sets names fof each o fthe state and derived variables in the main modle routine
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#The file of initial state values is read from the directory /parameters

	IRdata <- get.model.file(model.path, PARAMETERS_DIR, file.pattern=INITIAL_STATE, header=FALSE)		# no header!

	# Check whether the saved values of xR_detritus match the values from the configuration and if not inject them into the data

	#if(x_xR_detritus_s1 < IRdata[9, 2] || x_xR_detritus_s1 > IRdata[9, 2]) {IRdata[9, 2]<-x_xR_detritus_s1}	# xR_detritus_s1
	#if(x_xR_detritus_s2 < IRdata[10,2] || x_xR_detritus_s2 > IRdata[10,2]) {IRdata[10,2]<-x_xR_detritus_s2}	# xR_detritus_s2
	#if(x_xR_detritus_s3 < IRdata[11,2] || x_xR_detritus_s3 > IRdata[11,2]) {IRdata[11,2]<-x_xR_detritus_s3}	# xR_detritus_s3

	#if(x_xR_detritus_d1 < IRdata[12,2] || x_xR_detritus_d1 > IRdata[12,2]) {IRdata[12,2]<-x_xR_detritus_d1}	# xR_detritus_d1
	#if(x_xR_detritus_d2 < IRdata[13,2] || x_xR_detritus_d2 > IRdata[13,2]) {IRdata[13,2]<-x_xR_detritus_d2}	# xR_detritus_d2
	#if(x_xR_detritus_d3 < IRdata[14,2] || x_xR_detritus_d3 > IRdata[14,2]) {IRdata[14,2]<-x_xR_detritus_d3}	# xR_detritus_d3

	#if(x_poros_s1==0 && IRdata[3,2]>0) {IRdata[3,2]<-0}	# x_detritus_s1
	#if(x_poros_s2==0 && IRdata[4,2]>0) {IRdata[4,2]<-0}	# x_detritus_s2
	#if(x_poros_s3==0 && IRdata[5,2]>0) {IRdata[5,2]<-0}	# x_detritus_s3

	#if(x_poros_d1==0 && IRdata[6,2]>0) {IRdata[6,2]<-0}	# x_detritus_d1
	#if(x_poros_d2==0 && IRdata[7,2]>0) {IRdata[7,2]<-0}	# x_detritus_d2
	#if(x_poros_d3==0 && IRdata[8,2]>0) {IRdata[8,2]<-0}	# x_detritus_d3

	initial.state <- list(
		detritus_so=IRdata[1,2],
		detritus_d=IRdata[2,2],

		x_detritus_s1=IRdata[3,2],
		x_detritus_s2=IRdata[4,2],
		x_detritus_s3=IRdata[5,2],
		x_detritus_d1=IRdata[6,2],
		x_detritus_d2=IRdata[7,2],
		x_detritus_d3=IRdata[8,2],

		xR_detritus_s1=IRdata[9,2],
		xR_detritus_s2=IRdata[10,2],
		xR_detritus_s3=IRdata[11,2],
		xR_detritus_d1=IRdata[12,2],
		xR_detritus_d2=IRdata[13,2],
		xR_detritus_d3=IRdata[14,2],

		discard_o=IRdata[15,2],

		corpse_s1=IRdata[16,2],
		corpse_s2=IRdata[17,2],
		corpse_s3=IRdata[18,2],
		corpse_d1=IRdata[19,2],
		corpse_d2=IRdata[20,2],
		corpse_d3=IRdata[21,2],

		ammonia_so=IRdata[22,2],
		ammonia_d=IRdata[23,2],

		x_ammonia_s1=IRdata[24,2],
		x_ammonia_s2=IRdata[25,2],
		x_ammonia_s3=IRdata[26,2],
		x_ammonia_d1=IRdata[27,2],
		x_ammonia_d2=IRdata[28,2],
		x_ammonia_d3=IRdata[29,2],

		nitrate_so=IRdata[30,2],
		nitrate_d=IRdata[31,2],

		x_nitrate_s1=IRdata[32,2],
		x_nitrate_s2=IRdata[33,2],
		x_nitrate_s3=IRdata[34,2],
		x_nitrate_d1=IRdata[35,2],
		x_nitrate_d2=IRdata[36,2],
		x_nitrate_d3=IRdata[37,2],

		phyt_so=IRdata[38,2],
		phyt_d=IRdata[39,2],
		omni_o=IRdata[40,2],
		carn_o=IRdata[41,2],
		benthslar_o=IRdata[42,2],
		benths_o=IRdata[43,2],
		benthclar_o=IRdata[44,2],
		benthc_o=IRdata[45,2],
		fishp_o=IRdata[46,2],
		fishplar_o=IRdata[47,2],
		fishd_o=IRdata[48,2],
		fishdlar_o=IRdata[49,2],
		fishm_o=IRdata[50,2],
		bird_o=IRdata[51,2],

		detritus_si=IRdata[52,2],
		ammonia_si=IRdata[53,2],
		nitrate_si=IRdata[54,2],
		phyt_si=IRdata[55,2],
		benthslar_i=IRdata[56,2],
		benthclar_i=IRdata[57,2],
		benths_i=IRdata[58,2],
		benthc_i=IRdata[59,2],
		discard_i=IRdata[60,2],
		omni_i=IRdata[61,2],
		carn_i=IRdata[62,2],
		fishplar_i=IRdata[63,2],
		fishdlar_i=IRdata[64,2],
		fishp_i=IRdata[65,2],
		fishm_i=IRdata[66,2],
		fishd_i=IRdata[67,2],
		bird_i=IRdata[68,2],
		seal_o=IRdata[69,2],
		seal_i=IRdata[70,2],
		ceta_o=IRdata[71,2],
		ceta_i=IRdata[72,2],

		corpse_s0=IRdata[73,2],
		corpse_d0=IRdata[74,2],

		kelpC= IRdata[75,2],
		kelpN= IRdata[76,2],

		kelpdebris= IRdata[77,2],

		#Then add on all the derived variables which are zero at the start of the model run

		#BIOLOGICAL PRODUCTION RATES

		netpprod_o=0,
		netpprod_i=0,
		NNCP_o=0,
		NNCP_i=0,
		phytgrossprod_o=0,
		phytgrossprod_i=0,

		kelpCprod_i=0,
		kelpCexud_i=0,
		kelpNprod_i=0,

		omnigrossprod_o=0,
		omnigrossprod_i=0,

		carngrossprod_o=0,
		carngrossprod_i=0,
		pfishlargrossprod_o=0,
		pfishlargrossprod_i=0,
		dfishlargrossprod_o=0,
		dfishlargrossprod_i=0,
		pfishgrossprod_o=0,
		pfishgrossprod_i=0,
		mfishgrossprod_o=0,
		mfishgrossprod_i=0,
		dfishgrossprod_o=0,
		dfishgrossprod_i=0,
		benthslargrossprod_o=0,
		benthslargrossprod_i=0,
		benthclargrossprod_o=0,
		benthclargrossprod_i=0,
		benthsgrossprod_o=0,
		benthsgrossprod_i=0,
		benthcgrossprod_o=0,
		benthcgrossprod_i=0,

		birdgrossprod_o=0,
		birdgrossprod_i=0,

		sealgrossprod_o=0,
		sealgrossprod_i=0,

		cetagrossprod_o=0,
		cetagrossprod_i=0,

		wcdenitrif_o=0,
		wcdenitrif_i=0,
		seddenitrif_o=0,
		seddenitrif_i=0,

		# DETRITUS and GEOCHEMICAL FLUXES

		fluxsedamm_wcamm=0,
		fluxwcdet_wcamm=0,
		fluxomni_wcamm=0,
		fluxcarn_wcamm=0,
		fluxpfishlar_wcamm=0,
		fluxdfishlar_wcamm=0,
		fluxpfish_wcamm=0,
		fluxmfish_wcamm=0,
		fluxdfish_wcamm=0,
		fluxbenthslar_wcamm=0,
		fluxbenthclar_wcamm=0,
		fluxbenths_wcamm=0,
		fluxbenthc_wcamm=0,

		fluxbird_wcamm=0,

		fluxseal_wcamm=0,

		fluxceta_wcamm=0,

		fluxxdet_sedamm=0,
		fluxxRdet_sedamm=0,
		fluxwcamm_wcnit=0,
		fluxsednit_wcnit=0,
		fluxsedamm_sednit=0,
		fluxxdet_wcdet=0,

		fluxkelpdebris_wcdet=0,

		fluxcorp_wcdet=0,
		fluxphyt_wcdet=0,
		fluxomni_wcdet=0,
		fluxcarn_wcdet=0,
		fluxpfishlar_wcdet=0,
		fluxdfishlar_wcdet=0,
		fluxpfish_wcdet=0,
		fluxmfish_wcdet=0,
		fluxdfish_wcdet=0,
		fluxbenthslar_wcdet=0,
		fluxbenthclar_wcdet=0,
		fluxbenths_wcdet=0,
		fluxbenthc_wcdet=0,

		fluxbird_wcdet=0,

		fluxseal_wcdet=0,

		fluxceta_wcdet=0,

		fluxwcdet_xdet=0,
		fluxcorp_xdet=0,
		fluxbenths_xdet=0,
		fluxbenthc_xdet=0,
		fluxxdet_xRdet=0,


		fluxkelpdebris_xRdet=0,


		fluxcorp_xRdet=0,

		fluxkelp_kelpdebris=0,


		fluxdisc_corp=0,
		fluxpfish_corp=0,
		fluxmfish_corp=0,
		fluxdfish_corp=0,
		fluxbenths_corp=0,
		fluxbenthc_corp=0,

		fluxbird_corp=0,

		fluxseal_corp=0,

		fluxceta_corp=0,

		# FOOD WEB FLUXES

		fluxwcamm_kelp=0,
		fluxwcnit_kelp=0,

		fluxwcamm_phyt_o=0,
		fluxwcamm_phyt_i=0,

		fluxwcnit_phyt_o=0,
		fluxwcnit_phyt_i=0,

		fluxwcdet_omni=0,
		fluxphyt_omni=0,
		fluxbenthslar_omni=0,
		fluxbenthclar_omni=0,

		fluxomni_carn=0,
		fluxpfishlar_carn=0,
		fluxdfishlar_carn=0,
		fluxbenthslar_carn=0,
		fluxbenthclar_carn=0,

		fluxomni_pfishlar=0,
		fluxbenthslar_pfishlar=0,
		fluxbenthclar_pfishlar=0,

		fluxomni_dfishlar=0,
		fluxbenthslar_dfishlar=0,
		fluxbenthclar_dfishlar=0,

		fluxomni_pfish=0,
		fluxcarn_pfish=0,
		fluxpfishlar_pfish=0,
		fluxdfishlar_pfish=0,
		fluxbenthslar_pfish=0,
		fluxbenthclar_pfish=0,

		fluxomni_mfish=0,
		fluxcarn_mfish=0,
		fluxpfishlar_mfish=0,
		fluxdfishlar_mfish=0,
		fluxbenthslar_mfish=0,
		fluxbenthclar_mfish=0,

		fluxcorp_dfish=0,
		fluxdisc_dfish=0,
		fluxcarn_dfish=0,
		fluxpfishlar_dfish=0,
		fluxdfishlar_dfish=0,
		fluxpfish_dfish=0,
		fluxmfish_dfish=0,
		fluxdfish_dfish=0,
		fluxbenths_dfish=0,
		fluxbenthc_dfish=0,

		fluxwcdet_benthslar=0,
		fluxphyt_benthslar=0,

		fluxwcdet_benthclar=0,
		fluxphyt_benthclar=0,

		fluxwcdet_benths=0,
		fluxxdet_benths=0,
		fluxxRdet_benths=0,
		fluxphyt_benths=0,

		fluxkelp_benthc=0,
		fluxkelpdebris_benthc=0,
		fluxcorp_benthc=0,
		fluxbenths_benthc=0,

		fluxcorp_bird=0,
		fluxdisc_bird=0,
		##    fluxomni_bird=0,
		fluxcarn_bird=0,
		fluxpfish_bird=0,
		fluxmfish_bird=0,
		fluxdfish_bird=0,
		fluxbenths_bird=0,
		fluxbenthc_bird=0,

		fluxcorp_seal=0,
		fluxdisc_seal=0,
		##    fluxomni_seal=0,
		fluxcarn_seal=0,
		fluxpfish_seal=0,
		fluxmfish_seal=0,
		fluxdfish_seal=0,
		fluxbenths_seal=0,
		fluxbenthc_seal=0,
		fluxbird_seal=0,

		##    fluxcorp_ceta=0,
		fluxdisc_ceta=0,
		fluxomni_ceta=0,
		fluxcarn_ceta=0,
		fluxpfish_ceta=0,
		fluxmfish_ceta=0,
		fluxdfish_ceta=0,
		fluxbenths_ceta=0,
		fluxbenthc_ceta=0,
		fluxbird_ceta=0,
		fluxseal_ceta=0,

		# SPAWNING AND RECRUITMENT FLUXES

		Bs_spawn=0,
		Bs_recruit=0,
		Bc_spawn=0,
		Bc_recruit=0,
		Pfish_spawn=0,
		Pfish_recruit=0,
		Dfish_spawn=0,
		Dfish_recruit=0,

		#DENITRIFICATION

		fluxwcnit_Ngas=0,
		fluxsednit_Ngas=0,

		# ADVECTION MIXING AND MIGRATION FLUXES

		fluxkelpdebris_beachexport=0,

		fluxAMMoutflow_o=0,
		fluxNIToutflow_o=0,
		fluxAMMoutflow_i=0,
		fluxNIToutflow_i=0,
		fluxPHYToutflow_o=0,
		fluxDEToutflow_o=0,
		fluxPHYToutflow_i=0,
		fluxDEToutflow_i=0,
		mfish_emigration=0,
		fluxsedboundary_o=0,
		fluxsedboundary_i=0,
		fluxAMMinflow_o=0,
		fluxNITinflow_o=0,
		fluxAMMinflow_i=0,
		fluxNITinflow_i=0,
		fluxPHYTinflow_o=0,
		fluxDETinflow_o=0,
		fluxPHYTinflow_i=0,
		fluxDETinflow_i=0,
		mfish_imigration=0,
		atmosAMMinput_o=0,
		atmosNITinput_o=0,
		atmosAMMinput_i=0,
		atmosNITinput_i=0,
		rivAMMinflow=0,
		rivNITinflow=0,
		rivPARTinflow=0,

		#EXCHANGES BETWEEN INSHORE AND OFFSHORE

		DINflux_i_o=0,
		DINflux_o_i=0,
		PARTflux_i_o=0,
		PARTflux_o_i=0,



		activemigpelfish_i_o=0,
		activemigmigfish_i_o=0,
		activemigdemfish_i_o=0,
		activemigbird_i_o=0,
		activemigseal_i_o=0,
		activemigceta_i_o=0,


		activemigpelfish_o_i=0,
		activemigmigfish_o_i=0,
		activemigdemfish_o_i=0,
		activemigbird_o_i=0,
		activemigseal_o_i=0,
		activemigceta_o_i=0,


		vertnitflux=0,
		horiznitflux=0,

		# LIVE WEIGHT LANDINGS AND DISCARD FLUXES

		landp_o=0,
		landd_quota_o=0,
		landd_nonquota_o=0,
		landm_o=0,
		landsb_o=0,
		landcb_o=0,
		landcz_o=0,
		landbd_o=0,
		landsl_o=0,
		landct_o=0,

		discpel_o=0,
		discdem_quota_o=0,
		discdem_nonquota_o=0,
		discmig_o=0,
		discsb_o=0,
		disccb_o=0,
		disccz_o=0,
		discbd_o=0,
		discsl_o=0,
		discct_o=0,

		landp_i=0,
		landd_quota_i=0,
		landd_nonquota_i=0,
		landm_i=0,
		landsb_i=0,
		landcb_i=0,
		landcz_i=0,
		landbd_i=0,
		landsl_i=0,
		landct_i=0,
		landkp_i=0,

		discpel_i=0,
		discdem_quota_i=0,
		discdem_nonquota_i=0,
		discmig_i=0,
		discsb_i=0,
		disccb_i=0,
		disccz_i=0,
		discbd_i=0,
		discsl_i=0,
		discct_i=0,
		disckp_i=0,

		# FLUXES TO OFFAL FROm EACH GROUP
		# FISHERY EXPORT FROM THE MODEL - LIVE WEIGHT LANDED - OFFAL FLUX
		# all offal from processing fish, invertebrates, birds and mammals goes into the discard pool
		# offal from processing kelp at sea goes into the kelp debris pool

		offalpel_o=0,
		offaldem_quota_o=0,
		offaldem_nonquota_o=0,
		offalmig_o=0,
		offalsb_o=0,
		offalcb_o=0,
		offalcz_o=0,
		offalbd_o=0,
		offalsl_o=0,
		offalct_o=0,

		offalpel_i=0,
		offaldem_quota_i=0,
		offaldem_nonquota_i=0,
		offalmig_i=0,
		offalsb_i=0,
		offalcb_i=0,
		offalcz_i=0,
		offalbd_i=0,
		offalsl_i=0,
		offalct_i=0,
		offalkp_i=0,

		omninetprod_o=0,
		omninetprod_i=0,

		carnnetprod_o=0,
		carnnetprod_i=0,
		pfishlarnetprod_o=0,
		pfishlarnetprod_i=0,
		dfishlarnetprod_o=0,
		dfishlarnetprod_i=0,
		pfishnetprod_o=0,
		pfishnetprod_i=0,
		mfishnetprod_o=0,
		mfishnetprod_i=0,
		dfishnetprod_o=0,
		dfishnetprod_i=0,
		benthslarnetprod_o=0,
		benthslarnetprod_i=0,
		benthclarnetprod_o=0,
		benthclarnetprod_i=0,
		benthsnetprod_o=0,
		benthsnetprod_i=0,
		benthcnetprod_o=0,
		benthcnetprod_i=0,

		birdnetprod_o=0,
		birdnetprod_i=0,

		sealnetprod_o=0,
		sealnetprod_i=0,

		cetanetprod_o=0,
		cetanetprod_i=0
	)
}

