#
# aggregate_model_output.R
#
#' 
#' create some aggregates of various columns in the output dataframe from the model
#'
#' @param model model object
#' @param output model output
#'
#' @return aggregated data
#'
#' @noRd
#
# ------------------------------------------------------------------------------

aggregate_model_output <- function(model, output) {

	# Unpack:
	data		<- elt(model, "data")
	physical.parms	<- elt(data, "physical.parameters")

	x_poros_s1	<- elt(physical.parms, "x_poros_s1")
	x_poros_s2	<- elt(physical.parms, "x_poros_s2")
	x_poros_s3	<- elt(physical.parms, "x_poros_s3")
	x_poros_d1	<- elt(physical.parms, "x_poros_d1")
	x_poros_d2	<- elt(physical.parms, "x_poros_d2")
	x_poros_d3	<- elt(physical.parms, "x_poros_d3")

	x_area_s1	<- elt(physical.parms, "x_area_s1")
	x_area_s2	<- elt(physical.parms, "x_area_s2")
	x_area_s3	<- elt(physical.parms, "x_area_s3")
	x_area_d1	<- elt(physical.parms, "x_area_d1")
	x_area_d2	<- elt(physical.parms, "x_area_d2")
	x_area_d3	<- elt(physical.parms, "x_area_d3")

	x_depth_s1	<- elt(physical.parms, "x_depth_s1")
	x_depth_s2	<- elt(physical.parms, "x_depth_s2")
	x_depth_s3	<- elt(physical.parms, "x_depth_s3")
	x_depth_d1	<- elt(physical.parms, "x_depth_d1")
	x_depth_d2	<- elt(physical.parms, "x_depth_d2")
	x_depth_d3	<- elt(physical.parms, "x_depth_d3")

	out		<- output

	aggregates	<- list()

	#Create a column for the total nitrogen mass in the system
	aggregates$totalN <-
		elt(out, "detritus_so")+
		elt(out, "detritus_d")+
		elt(out, "x_detritus_s1")+
		elt(out, "x_detritus_s2")+
		elt(out, "x_detritus_s3")+
		elt(out, "x_detritus_d1")+
		elt(out, "x_detritus_d2")+
		elt(out, "x_detritus_d3")+
		elt(out, "xR_detritus_s1")+
		elt(out, "xR_detritus_s2")+
		elt(out, "xR_detritus_s3")+
		elt(out, "xR_detritus_d1")+
		elt(out, "xR_detritus_d2")+
		elt(out, "xR_detritus_d3")+
		elt(out, "discard_o")+
		elt(out, "corpse_s0")+
		elt(out, "corpse_s1")+
		elt(out, "corpse_s2")+
		elt(out, "corpse_s3")+
		elt(out, "corpse_d0")+
		elt(out, "corpse_d1")+
		elt(out, "corpse_d2")+
		elt(out, "corpse_d3")+
		elt(out, "ammonia_so")+
		elt(out, "ammonia_d")+
		elt(out, "x_ammonia_s1")+
		elt(out, "x_ammonia_s2")+
		elt(out, "x_ammonia_s3")+
		elt(out, "x_ammonia_d1")+
		elt(out, "x_ammonia_d2")+
		elt(out, "x_ammonia_d3")+
		elt(out, "nitrate_so")+
		elt(out, "nitrate_d")+
		elt(out, "x_nitrate_s1")+
		elt(out, "x_nitrate_s2")+
		elt(out, "x_nitrate_s3")+
		elt(out, "x_nitrate_d1")+
		elt(out, "x_nitrate_d2")+
		elt(out, "x_nitrate_d3")+
		elt(out, "kelpN")+
		elt(out, "kelpdebris")+
		elt(out, "phyt_so")+
		elt(out, "phyt_d")+
		elt(out, "herb_o")+
		elt(out, "herb_i")+
		elt(out, "carn_o")+
		elt(out, "carn_i")+
		elt(out, "benthslar_o")+
		elt(out, "benthclar_o")+
		elt(out, "benths_o")+
		elt(out, "benthc_o")+
		elt(out, "fishplar_o")+
		elt(out, "fishplar_i")+
		elt(out, "fishp_o")+
		elt(out, "fishp_i")+
		elt(out, "fishdlar_o")+
		elt(out, "fishdlar_i")+
		elt(out, "fishd_o")+
		elt(out, "fishd_i")+
		elt(out, "fishm_o")+
		elt(out, "fishm_i")+
		elt(out, "bird_o")+
		elt(out, "bird_i")+
		elt(out, "seal_o")+
		elt(out, "seal_i")+
		elt(out, "ceta_o")+
		elt(out, "ceta_i")+
		elt(out, "detritus_si")+
		elt(out, "ammonia_si")+
		elt(out, "nitrate_si")+
		elt(out, "phyt_si")+
		elt(out, "benthslar_i")+
		elt(out, "benthclar_i")+
		elt(out, "benths_i")+
		elt(out, "benthc_i")+
		elt(out, "discard_i")

	aggregates$totalN_o <-
		elt(out, "detritus_so")+
		elt(out, "detritus_d")+
		elt(out, "x_detritus_d1")+
		elt(out, "x_detritus_d2")+
		elt(out, "x_detritus_d3")+
		elt(out, "xR_detritus_d1")+
		elt(out, "xR_detritus_d2")+
		elt(out, "xR_detritus_d3")+
		elt(out, "discard_o")+
		elt(out, "corpse_d0")+
		elt(out, "corpse_d1")+
		elt(out, "corpse_d2")+
		elt(out, "corpse_d3")+
		elt(out, "ammonia_so")+
		elt(out, "ammonia_d")+
		elt(out, "x_ammonia_d1")+
		elt(out, "x_ammonia_d2")+
		elt(out, "x_ammonia_d3")+
		elt(out, "nitrate_so")+
		elt(out, "nitrate_d")+
		elt(out, "x_nitrate_d1")+
		elt(out, "x_nitrate_d2")+
		elt(out, "x_nitrate_d3")+
		elt(out, "phyt_so")+
		elt(out, "phyt_d")+
		elt(out, "herb_o")+
		elt(out, "carn_o")+
		elt(out, "benthslar_o")+
		elt(out, "benthclar_o")+
		elt(out, "benths_o")+
		elt(out, "benthc_o")+
		elt(out, "fishplar_o")+
		elt(out, "fishp_o")+
		elt(out, "fishdlar_o")+
		elt(out, "fishd_o")+
		elt(out, "fishm_o")+
		elt(out, "bird_o")+
		elt(out, "seal_o")+
		elt(out, "ceta_o")


	aggregates$totalN_i <-
		elt(out, "x_detritus_s1")+
		elt(out, "x_detritus_s2")+
		elt(out, "x_detritus_s3")+
		elt(out, "xR_detritus_s1")+
		elt(out, "xR_detritus_s2")+
		elt(out, "xR_detritus_s3")+
		elt(out, "corpse_s0")+
		elt(out, "corpse_s1")+
		elt(out, "corpse_s2")+
		elt(out, "corpse_s3")+
		elt(out, "x_ammonia_s1")+
		elt(out, "x_ammonia_s2")+
		elt(out, "x_ammonia_s3")+
		elt(out, "x_nitrate_s1")+
		elt(out, "x_nitrate_s2")+
		elt(out, "x_nitrate_s3")+
		elt(out, "herb_i")+
		elt(out, "carn_i")+
		elt(out, "fishplar_i")+
		elt(out, "fishp_i")+
		elt(out, "fishdlar_i")+
		elt(out, "fishd_i")+
		elt(out, "fishm_i")+
		elt(out, "bird_i")+
		elt(out, "seal_i")+
		elt(out, "ceta_i")+
		elt(out, "detritus_si")+
		elt(out, "ammonia_si")+
		elt(out, "nitrate_si")+
		elt(out, "kelpN")+
		elt(out, "kelpdebris")+
		elt(out, "phyt_si")+
		elt(out, "benthslar_i")+
		elt(out, "benthclar_i")+
		elt(out, "benths_i")+
		elt(out, "benthc_i")+
		elt(out, "discard_i")


	#Create columns to aggregate across sediment caregories

	aggregates$x_detritus <-
		elt(out, "x_detritus_s1")+
		elt(out, "x_detritus_s2")+
		elt(out, "x_detritus_s3")+
		elt(out, "x_detritus_d1")+
		elt(out, "x_detritus_d2")+
		elt(out, "x_detritus_d3")+
		elt(out, "xR_detritus_s1")+
		elt(out, "xR_detritus_s2")+
		elt(out, "xR_detritus_s3")+
		elt(out, "xR_detritus_d1")+
		elt(out, "xR_detritus_d2")+
		elt(out, "xR_detritus_d3")

	aggregates$x_detritus_o<-elt(out, "x_detritus_d1")+ elt(out, "x_detritus_d2")+ elt(out, "x_detritus_d3")+
		elt(out, "xR_detritus_d1")+ elt(out, "xR_detritus_d2")+ elt(out, "xR_detritus_d3")

	aggregates$x_detritus_i<-elt(out, "x_detritus_s1")+ elt(out, "x_detritus_s2")+ elt(out, "x_detritus_s3")+
		elt(out, "xR_detritus_s1")+ elt(out, "xR_detritus_s2")+ elt(out, "xR_detritus_s3")


	aggregates$corpse<-elt(out, "corpse_s0")+elt(out, "corpse_s1")+ elt(out, "corpse_s2")+ elt(out, "corpse_s3")+elt(out, "corpse_d0")+ elt(out, "corpse_d1")+ elt(out, "corpse_d2")+ elt(out, "corpse_d3")
	aggregates$corpse_o<-elt(out, "corpse_d0")+elt(out, "corpse_d1")+ elt(out, "corpse_d2")+ elt(out, "corpse_d3")
	aggregates$corpse_i<-elt(out, "corpse_s0")+elt(out, "corpse_s1")+ elt(out, "corpse_s2")+ elt(out, "corpse_s3")

	aggregates$x_ammonia<-elt(out, "x_ammonia_s1")+ elt(out, "x_ammonia_s2")+ elt(out, "x_ammonia_s3")+ elt(out, "x_ammonia_d1")+ elt(out, "x_ammonia_d2")+ elt(out, "x_ammonia_d3")
	aggregates$x_ammonia_o<-elt(out, "x_ammonia_d1")+ elt(out, "x_ammonia_d2")+ elt(out, "x_ammonia_d3")
	aggregates$x_ammonia_i<-elt(out, "x_ammonia_s1")+ elt(out, "x_ammonia_s2")+ elt(out, "x_ammonia_s3")

	aggregates$x_nitrate<-elt(out, "x_nitrate_s1")+ elt(out, "x_nitrate_s2")+ elt(out, "x_nitrate_s3")+ elt(out, "x_nitrate_d1")+ elt(out, "x_nitrate_d2")+ elt(out, "x_nitrate_d3")
	aggregates$x_nitrate_o<-elt(out, "x_nitrate_d1")+ elt(out, "x_nitrate_d2")+ elt(out, "x_nitrate_d3")
	aggregates$x_nitrate_i<-elt(out, "x_nitrate_s1")+ elt(out, "x_nitrate_s2")+ elt(out, "x_nitrate_s3")

	#Create state variable aggregates across inshore and offshore

	aggregates$s_detritus<-elt(out, "detritus_so")+elt(out, "detritus_si")
	aggregates$s_ammonia<-elt(out, "ammonia_so")+elt(out, "ammonia_si")
	aggregates$s_nitrate<-elt(out, "nitrate_so")+elt(out, "nitrate_si")
	aggregates$s_phyt<-elt(out, "phyt_so")+elt(out, "phyt_si")
	aggregates$benthslar<-elt(out, "benthslar_o")+elt(out, "benthslar_i")
	aggregates$benthclar<-elt(out, "benthclar_o")+elt(out, "benthclar_i")
	aggregates$benths<-elt(out, "benths_o")+elt(out, "benths_i")
	aggregates$benthc<-elt(out, "benthc_o")+elt(out, "benthc_i")
	aggregates$discard<-elt(out, "discard_o")+elt(out, "discard_i")

	aggregates$herb<-elt(out, "herb_o")+elt(out, "herb_i")

	aggregates$carn<-elt(out, "carn_o")+elt(out, "carn_i")

	aggregates$fishp<-elt(out, "fishp_o")+elt(out, "fishp_i")
	aggregates$fishd<-elt(out, "fishd_o")+elt(out, "fishd_i")
	aggregates$fishm<-elt(out, "fishm_o")+elt(out, "fishm_i")
	aggregates$bird<-elt(out, "bird_o")+elt(out, "bird_i")

	aggregates$seal<-elt(out, "seal_o")+elt(out, "seal_i")
	aggregates$ceta<-elt(out, "ceta_o")+elt(out, "ceta_i")

	aggregates$fishplar<-elt(out, "fishplar_o")+elt(out, "fishplar_i")
	aggregates$fishdlar<-elt(out, "fishdlar_o")+elt(out, "fishdlar_i")

	#Create aggregates for flux terms which are ouput as separate for o and i

	aggregates$NNCP			<- elt(out, "NNCP_o")+ elt(out, "NNCP_i")

	aggregates$netpprod <- out$netpprod_o + out$netpprod_i


	aggregates$fluxwcamm_phyt <- out$fluxwcamm_phyt_o  +  out$fluxwcamm_phyt_i

	aggregates$fluxwcnit_phyt <- out$fluxwcnit_phyt_o  +  out$fluxwcnit_phyt_i


	aggregates$phytgrossprod <- out$phytgrossprod_o + out$phytgrossprod_i

	aggregates$herbgrossprod <- out$herbgrossprod_o + out$herbgrossprod_i
	aggregates$carngrossprod <- out$carngrossprod_o + out$carngrossprod_i
	aggregates$pfishlargrossprod <- out$pfishlargrossprod_o + out$pfishlargrossprod_i
	aggregates$dfishlargrossprod <- out$dfishlargrossprod_o + out$dfishlargrossprod_i
	aggregates$pfishgrossprod <- out$pfishgrossprod_o + out$pfishgrossprod_i
	aggregates$mfishgrossprod <- out$mfishgrossprod_o + out$mfishgrossprod_i
	aggregates$dfishgrossprod <- out$dfishgrossprod_o + out$dfishgrossprod_i
	aggregates$benthslargrossprod <- out$benthslargrossprod_o + out$benthslargrossprod_i
	aggregates$benthclargrossprod <- out$benthclargrossprod_o + out$benthclargrossprod_i
	aggregates$benthsgrossprod <- out$benthsgrossprod_o + out$benthsgrossprod_i
	aggregates$benthcgrossprod <- out$benthcgrossprod_o + out$benthcgrossprod_i
	aggregates$birdgrossprod <- out$birdgrossprod_o + out$birdgrossprod_i
	aggregates$sealgrossprod <- out$sealgrossprod_o + out$sealgrossprod_i
	aggregates$cetagrossprod <- out$cetagrossprod_o + out$cetagrossprod_i

	aggregates$herbnetprod <- out$herbnetprod_o + out$herbnetprod_i
	aggregates$carnnetprod <- out$carnnetprod_o + out$carnnetprod_i
	aggregates$pfishlarnetprod <- out$pfishlarnetprod_o + out$pfishlarnetprod_i
	aggregates$dfishlarnetprod <- out$dfishlarnetprod_o + out$dfishlarnetprod_i
	aggregates$pfishnetprod <- out$pfishnetprod_o + out$pfishnetprod_i
	aggregates$mfishnetprod <- out$mfishnetprod_o + out$mfishnetprod_i
	aggregates$dfishnetprod <- out$dfishnetprod_o + out$dfishnetprod_i
	aggregates$benthslarnetprod <- out$benthslarnetprod_o + out$benthslarnetprod_i
	aggregates$benthclarnetprod <- out$benthclarnetprod_o + out$benthclarnetprod_i
	aggregates$benthsnetprod <- out$benthsnetprod_o + out$benthsnetprod_i
	aggregates$benthcnetprod <- out$benthcnetprod_o + out$benthcnetprod_i
	aggregates$birdnetprod <- out$birdnetprod_o + out$birdnetprod_i
	aggregates$sealnetprod <- out$sealnetprod_o + out$sealnetprod_i
	aggregates$cetanetprod <- out$cetanetprod_o + out$cetanetprod_i

	aggregates$wcdenitrif		<- elt(out, "wcdenitrif_o")+ elt(out, "wcdenitrif_i")
	aggregates$seddenitrif		<- elt(out, "seddenitrif_o")+ elt(out, "seddenitrif_i")
	aggregates$fluxsedboundary	<- elt(out, "fluxsedboundary_o")+ elt(out, "fluxsedboundary_i")
	aggregates$DIN_NET_flux_o_i	<- elt(out, "DINflux_o_i") - elt(out, "DINflux_i_o")
	aggregates$PART_NET_flux_o_i	<- elt(out, "PARTflux_o_i") - elt(out, "PARTflux_i_o")

	aggregates$NET_activemigpelfish_o_i  <- out$activemigpelfish_o_i - out$activemigpelfish_i_o
	aggregates$NET_activemigmigfish_o_i  <- out$activemigmigfish_o_i - out$activemigmigfish_i_o
	aggregates$NET_activemigdemfish_o_i  <- out$activemigdemfish_o_i - out$activemigdemfish_i_o
	aggregates$NET_activemigbird_o_i  <- out$activemigbird_o_i - out$activemigbird_i_o
	aggregates$NET_activemigseal_o_i  <- out$activemigseal_o_i - out$activemigseal_i_o
	aggregates$NET_activemigceta_o_i  <- out$activemigceta_o_i - out$activemigceta_i_o
	aggregates$NET_mfish_ext_o    <- out$mfish_imigration - out$mfish_emigration

	aggregates$fluxDINinflow   <- out$fluxAMMinflow_o  + out$fluxAMMinflow_i + out$fluxNITinflow_o  + out$fluxNITinflow_i
	aggregates$fluxDINoutflow  <- out$fluxAMMoutflow_o + out$fluxAMMoutflow_i + out$fluxNIToutflow_o + out$fluxNIToutflow_i
	aggregates$fluxPARTinflow  <- out$fluxPHYTinflow_o + out$fluxPHYTinflow_i + out$fluxDETinflow_o + out$fluxDETinflow_i
	aggregates$fluxPARToutflow <- out$fluxPHYToutflow_o+ out$fluxPHYToutflow_i + out$fluxDEToutflow_o+ out$fluxDEToutflow_i
	aggregates$atmosDINinput   <- out$atmosAMMinput_o  + out$atmosAMMinput_i + out$atmosNITinput_o  + out$atmosNITinput_i
	aggregates$rivDINinflow   <- out$rivAMMinflow  + out$rivNITinflow


	#Create columns for whole domain live landed weights 

	aggregates$landp<-elt(out, "landp_o")+elt(out, "landp_i")

	aggregates$landd<-elt(out, "landd_quota_o")+elt(out, "landd_nonquota_o")+elt(out, "landd_quota_i")+elt(out, "landd_nonquota_i")
	aggregates$landd_o<-elt(out, "landd_quota_o")+elt(out, "landd_nonquota_o")
	aggregates$landd_i<-elt(out, "landd_quota_i")+elt(out, "landd_nonquota_i")

	aggregates$landd_quota<-elt(out, "landd_quota_o")+elt(out, "landd_quota_i")
	aggregates$landd_nonquota<-elt(out, "landd_nonquota_o")+elt(out, "landd_nonquota_i")

	aggregates$landm<-elt(out, "landm_o")+elt(out, "landm_i")

	aggregates$landsb<-elt(out, "landsb_o")+elt(out, "landsb_i")

	aggregates$landcb<-elt(out, "landcb_o")+elt(out, "landcb_i")

	aggregates$landcz<-elt(out, "landcz_o")+elt(out, "landcz_i")

	aggregates$landbd<-out$landbd_o+out$landbd_i

	aggregates$landsl<-out$landsl_o+out$landsl_i

	aggregates$landct<-out$landct_o+out$landct_i

	#Create columns for whole domian discards 

	aggregates$discpel<-out$discpel_o+out$discpel_i

	aggregates$discdem<-out$discdem_quota_o+out$discdem_nonquota_o+out$discdem_quota_i+out$discdem_nonquota_i
	aggregates$discdem_o<-out$discdem_quota_o+out$discdem_nonquota_o
	aggregates$discdem_i<-out$discdem_quota_i+out$discdem_nonquota_i

	aggregates$discdem_quota<-out$discdem_quota_o+out$discdem_quota_i
	aggregates$discdem_nonquota<-out$discdem_nonquota_o+out$discdem_nonquota_i

	aggregates$discmig<-out$discmig_o+out$discmig_i

	aggregates$discsb<-out$discsb_o+out$discsb_i

	aggregates$disccb<-out$disccb_o+out$disccb_i

	aggregates$disccz<-out$disccz_o+out$disccz_i

	aggregates$discbd<-out$discbd_o+out$discbd_i

	aggregates$discsl<-out$discsl_o+out$discsl_i
	aggregates$discct<-out$discct_o+out$discct_i


	#Create columns for whole domain offal production

	aggregates$offalpel<-out$offalpel_o+out$offalpel_i

	aggregates$offaldem<-out$offaldem_quota_o+out$offaldem_nonquota_o+out$offaldem_quota_i+out$offaldem_nonquota_i
	aggregates$offaldem_o<-out$offaldem_quota_o+out$offaldem_nonquota_o
	aggregates$offaldem_i<-out$offaldem_quota_i+out$offaldem_nonquota_i

	aggregates$offaldem_quota<-out$offaldem_quota_o+out$offaldem_quota_i
	aggregates$offaldem_nonquota<-out$offaldem_nonquota_o+out$offaldem_nonquota_i

	aggregates$offalmig<-out$offalmig_o+out$offalmig_i

	aggregates$offalsb<-out$offalsb_o+out$offalsb_i

	aggregates$offalcb<-out$offalcb_o+out$offalcb_i

	aggregates$offalcz<-out$offalcz_o+out$offalcz_i

	aggregates$offalbd<-out$offalbd_o+out$offalbd_i

	aggregates$offalsl<-out$offalsl_o+out$offalsl_i
	aggregates$offalct<-out$offalct_o+out$offalct_i

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	aggregates$x_poros<- ((x_poros_s1*x_area_s1) +
		(x_poros_s2*x_area_s2) +
		(x_poros_s3*x_area_s3) +
		(x_poros_d1*x_area_d1) +
		(x_poros_d2*x_area_d2) +
		(x_poros_d3*x_area_d3))

	aggregates$x_depth<- ((x_depth_s1*x_area_s1) +
		(x_depth_s2*x_area_s2) +
		(x_depth_s3*x_area_s3) +
		(x_depth_d1*x_area_d1) +
		(x_depth_d2*x_area_d2) +
		(x_depth_d3*x_area_d3))


	aggregates$x_poros_o<-((x_poros_d1*x_area_d1) +
		(x_poros_d2*x_area_d2) +
		(x_poros_d3*x_area_d3))/(x_area_d1+x_area_d2+x_area_d3)

	aggregates$x_poros_i<- ((x_poros_s1*x_area_s1) +
		(x_poros_s2*x_area_s2) +
		(x_poros_s3*x_area_s3))/(x_area_s1+x_area_s2+x_area_s3)

	aggregates$x_depth_o<- ((x_depth_d1*x_area_d1) +
		(x_depth_d2*x_area_d2) +
		(x_depth_d3*x_area_d3))/(x_area_d1+x_area_d2+x_area_d3)

	aggregates$x_depth_i<- ((x_depth_s1*x_area_s1) +
		(x_depth_s2*x_area_s2) +
		(x_depth_s3*x_area_s3))/(x_area_s1+x_area_s2+x_area_s3)

	aggregates
}

