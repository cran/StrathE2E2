#
# derive_annual_results_inshore
#
#' derive set of results for inshore only and write to files
#'
#' @param model model object
#' @param build model build object
#' @param output model output
#' @param aggregates aggregated model output
#'
#' @noRd
#
# ------------------------------------------------------------------------------

derive_annual_results_inshore <- function(model, build, output, aggregates) {

	# Unpack:
	setup		<- elt(model, "setup")
	identifier	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

	run		<- elt(build, "run")
	ndays		<- elt(run, "ndays")
	nyears		<- elt(run, "nyears")

	data		<- elt(model, "data")
	physical.parms	<- elt(data, "physical.parameters")

	# extract physical parameters:
	so_depth		<- elt(physical.parms, "so_depth")
	d_depth			<- elt(physical.parms, "d_depth")
	si_depth		<- elt(physical.parms, "si_depth")
	x_depth_s1		<- elt(physical.parms, "x_depth_s1")
	x_depth_s2		<- elt(physical.parms, "x_depth_s2")
	x_depth_s3		<- elt(physical.parms, "x_depth_s3")
	x_depth_d1		<- elt(physical.parms, "x_depth_d1")
	x_depth_d2		<- elt(physical.parms, "x_depth_d2")
	x_depth_d3		<- elt(physical.parms, "x_depth_d3")
	x_area_s0		<- elt(physical.parms, "x_area_s0")
	x_area_s1		<- elt(physical.parms, "x_area_s1")
	x_area_s2		<- elt(physical.parms, "x_area_s2")
	x_area_s3		<- elt(physical.parms, "x_area_s3")
	x_area_d0		<- elt(physical.parms, "x_area_d0")
	x_area_d1		<- elt(physical.parms, "x_area_d1")
	x_area_d2		<- elt(physical.parms, "x_area_d2")
	x_area_d3		<- elt(physical.parms, "x_area_d3")
	x_poros_s1		<- elt(physical.parms, "x_poros_s1")
	x_poros_s2		<- elt(physical.parms, "x_poros_s2")
	x_poros_s3		<- elt(physical.parms, "x_poros_s3")
	x_poros_d1		<- elt(physical.parms, "x_poros_d1")
	x_poros_d2		<- elt(physical.parms, "x_poros_d2")
	x_poros_d3		<- elt(physical.parms, "x_poros_d3")
	x_shallowprop		<- elt(physical.parms, "x_shallowprop")

	# extract output:
	detritus_si		<- elt(output, "detritus_si")
	xR_detritus_s1		<- elt(output, "xR_detritus_s1")
	xR_detritus_s2		<- elt(output, "xR_detritus_s2")
	xR_detritus_s3		<- elt(output, "xR_detritus_s3")
	discard_i		<- elt(output, "discard_i")
	kelpdebris		<- elt(output, "kelpdebris")
	ammonia_si		<- elt(output, "ammonia_si")
	nitrate_si		<- elt(output, "nitrate_si")
	kelpN			<- elt(output, "kelpN")
	phyt_si			<- elt(output, "phyt_si")
	omni_i			<- elt(output, "omni_i")
	carn_i			<- elt(output, "carn_i")
	benthslar_i		<- elt(output, "benthslar_i")
	benths_i		<- elt(output, "benths_i")
	benthclar_i		<- elt(output, "benthclar_i")
	benthc_i		<- elt(output, "benthc_i")
	fishplar_i		<- elt(output, "fishplar_i")
	fishp_i			<- elt(output, "fishp_i")
	fishm_i			<- elt(output, "fishm_i")
	fishdlar_i		<- elt(output, "fishdlar_i")
	fishd_i			<- elt(output, "fishd_i")
	bird_i			<- elt(output, "bird_i")
	seal_i			<- elt(output, "seal_i")
	ceta_i			<- elt(output, "ceta_i")
	fluxAMMinflow_i		<- elt(output, "fluxAMMinflow_i")
	fluxNITinflow_i		<- elt(output, "fluxNITinflow_i")
	fluxAMMoutflow_i	<- elt(output, "fluxAMMoutflow_i")
	fluxNIToutflow_i	<- elt(output, "fluxNIToutflow_i")
	fluxPHYTinflow_i	<- elt(output, "fluxPHYTinflow_i")
	fluxDETinflow_i		<- elt(output, "fluxDETinflow_i")
	fluxPHYToutflow_i	<- elt(output, "fluxPHYToutflow_i")
	fluxDEToutflow_i	<- elt(output, "fluxDEToutflow_i")
	atmosAMMinput_i		<- elt(output, "atmosAMMinput_i")
	atmosNITinput_i		<- elt(output, "atmosNITinput_i")
	rivAMMinflow		<- elt(output, "rivAMMinflow")
	rivNITinflow		<- elt(output, "rivNITinflow")
	rivPARTinflow		<- elt(output, "rivPARTinflow")
	fluxsedboundary_i	<- elt(output, "fluxsedboundary_i")
	fluxkelpdebris_beachexport <- elt(output, "fluxkelpdebris_beachexport")
	netpprod_i		<- elt(output, "netpprod_i")
	nitrate_d		<- elt(output, "nitrate_d")
	NNCP_i			<- elt(output, "NNCP_i")
	fluxwcnit_phyt_i	<- elt(output, "fluxwcnit_phyt_i")
	fluxwcamm_phyt_i	<- elt(output, "fluxwcamm_phyt_i")
	fluxwcnit_kelp		<- elt(output, "fluxwcnit_kelp")
	fluxwcamm_kelp		<- elt(output, "fluxwcamm_kelp")
	kelpNprod_i		<- elt(output, "kelpNprod_i")
	phytgrossprod_i		<- elt(output, "phytgrossprod_i")
	omnigrossprod_i		<- elt(output, "omnigrossprod_i")
	carngrossprod_i		<- elt(output, "carngrossprod_i")
	pfishlargrossprod_i	<- elt(output, "pfishlargrossprod_i")
	dfishlargrossprod_i	<- elt(output, "dfishlargrossprod_i")
	pfishgrossprod_i	<- elt(output, "pfishgrossprod_i")
	mfishgrossprod_i	<- elt(output, "mfishgrossprod_i")
	dfishgrossprod_i	<- elt(output, "dfishgrossprod_i")
	benthslargrossprod_i	<- elt(output, "benthslargrossprod_i")
	benthclargrossprod_i	<- elt(output, "benthclargrossprod_i")
	benthsgrossprod_i	<- elt(output, "benthsgrossprod_i")
	benthcgrossprod_i	<- elt(output, "benthcgrossprod_i")
	birdgrossprod_i		<- elt(output, "birdgrossprod_i")
	sealgrossprod_i		<- elt(output, "sealgrossprod_i")
	cetagrossprod_i		<- elt(output, "cetagrossprod_i")
	omninetprod_i		<- elt(output, "omninetprod_i")
	carnnetprod_i		<- elt(output, "carnnetprod_i")
	pfishlarnetprod_i	<- elt(output, "pfishlarnetprod_i")
	dfishlarnetprod_i	<- elt(output, "dfishlarnetprod_i")
	pfishnetprod_i		<- elt(output, "pfishnetprod_i")
	mfishnetprod_i		<- elt(output, "mfishnetprod_i")
	dfishnetprod_i		<- elt(output, "dfishnetprod_i")
	benthslarnetprod_i	<- elt(output, "benthslarnetprod_i")
	benthclarnetprod_i	<- elt(output, "benthclarnetprod_i")
	benthsnetprod_i		<- elt(output, "benthsnetprod_i")
	benthcnetprod_i		<- elt(output, "benthcnetprod_i")
	birdnetprod_i		<- elt(output, "birdnetprod_i")
	sealnetprod_i		<- elt(output, "sealnetprod_i")
	cetanetprod_i		<- elt(output, "cetanetprod_i")
	fluxkelp_kelpdebris	<- elt(output, "fluxkelp_kelpdebris")
	fluxkelp_benthc		<- elt(output, "fluxkelp_benthc")
	fluxkelpdebris_benthc	<- elt(output, "fluxkelpdebris_benthc")
	landp_i			<- elt(output, "landp_i")
	landm_i			<- elt(output, "landm_i")
	landd_quota_i		<- elt(output, "landd_quota_i")
	landd_nonquota_i	<- elt(output, "landd_nonquota_i")
	landsb_i		<- elt(output, "landsb_i")
	landcb_i		<- elt(output, "landcb_i")
	landcz_i		<- elt(output, "landcz_i")
	landbd_i		<- elt(output, "landbd_i")
	landsl_i		<- elt(output, "landsl_i")
	landct_i		<- elt(output, "landct_i")
	landkp_i		<- elt(output, "landkp_i")
	discpel_i		<- elt(output, "discpel_i")
	discmig_i		<- elt(output, "discmig_i")
	discdem_quota_i		<- elt(output, "discdem_quota_i")
	discdem_nonquota_i	<- elt(output, "discdem_nonquota_i")
	discsb_i		<- elt(output, "discsb_i")
	disccb_i		<- elt(output, "disccb_i")
	disccz_i		<- elt(output, "disccz_i")
	discbd_i		<- elt(output, "discbd_i")
	discsl_i		<- elt(output, "discsl_i")
	discct_i		<- elt(output, "discct_i")
	disckp_i		<- elt(output, "disckp_i")
	offalpel_i		<- elt(output, "offalpel_i")
	offalmig_i		<- elt(output, "offalmig_i")
	offaldem_quota_i	<- elt(output, "offaldem_quota_i")
	offaldem_nonquota_i	<- elt(output, "offaldem_nonquota_i")
	offalsb_i		<- elt(output, "offalsb_i")
	offalcb_i		<- elt(output, "offalcb_i")
	offalcz_i		<- elt(output, "offalcz_i")
	offalbd_i		<- elt(output, "offalbd_i")
	offalsl_i		<- elt(output, "offalsl_i")
	offalct_i		<- elt(output, "offalct_i")
	offalkp_i		<- elt(output, "offalkp_i")

	# extract aggregates:
	x_detritus_i		<- elt(aggregates, "x_detritus_i")
	corpse_i		<- elt(aggregates, "corpse_i")
	x_ammonia_i		<- elt(aggregates, "x_ammonia_i")
	x_nitrate_i		<- elt(aggregates, "x_nitrate_i")
	totalN_i		<- elt(aggregates, "totalN_i")
	DIN_NET_flux_o_i	<- elt(aggregates, "DIN_NET_flux_o_i")
	PART_NET_flux_o_i	<- elt(aggregates, "PART_NET_flux_o_i")
	NET_activemigpelfish_o_i<- elt(aggregates, "NET_activemigpelfish_o_i")
	NET_activemigmigfish_o_i<- elt(aggregates, "NET_activemigmigfish_o_i")
	NET_activemigdemfish_o_i<- elt(aggregates, "NET_activemigdemfish_o_i")
	NET_activemigbird_o_i	<- elt(aggregates, "NET_activemigbird_o_i")
	NET_activemigseal_o_i	<- elt(aggregates, "NET_activemigseal_o_i")
	NET_activemigceta_o_i	<- elt(aggregates, "NET_activemigceta_o_i")
	landd_i			<- elt(aggregates, "landd_i")
	discdem_i		<- elt(aggregates, "discdem_i")
	offaldem_i		<- elt(aggregates, "offaldem_i")

#Derive a load of  stuff from the output and write to a file

#Extract annual average biomass in the final year for the INSHORE model domain

aamass_s_detritus<-mean(detritus_si[((nyears-1)*360+1):ndays])
aamass_d_detritus<-NA

aamass_x_detritus<-mean(x_detritus_i[((nyears-1)*360+1):ndays]) #Includes both labile and refractory detritus
aamass_xR_detritus<-mean(xR_detritus_s1[((nyears-1)*360+1):ndays]
                       + xR_detritus_s2[((nyears-1)*360+1):ndays]
                       + xR_detritus_s3[((nyears-1)*360+1):ndays])
aamass_discard<-mean(discard_i[((nyears-1)*360+1):ndays])
aamass_corpse<-mean(corpse_i[((nyears-1)*360+1):ndays])

aamass_kelpdebris<-mean(kelpdebris[((nyears-1)*360+1):ndays])

aamass_s_ammonia<-mean(ammonia_si[((nyears-1)*360+1):ndays])
aamass_d_ammonia<-NA

aamass_x_ammonia<-mean(x_ammonia_i[((nyears-1)*360+1):ndays])
aamass_s_nitrate<-mean(nitrate_si[((nyears-1)*360+1):ndays])
aamass_d_nitrate<-NA

aamass_x_nitrate<-mean(x_nitrate_i[((nyears-1)*360+1):ndays])

aamass_kelpN<-mean(kelpN[((nyears-1)*360+1):ndays])

aamass_s_phyt<-mean(phyt_si[((nyears-1)*360+1):ndays])
aamass_d_phyt<-NA

aamass_omni<-mean(omni_i[((nyears-1)*360+1):ndays])
aamass_carn<-mean(carn_i[((nyears-1)*360+1):ndays])
aamass_benthslar<-mean(benthslar_i[((nyears-1)*360+1):ndays])
aamass_benths<-mean(benths_i[((nyears-1)*360+1):ndays])
aamass_benthclar<-mean(benthclar_i[((nyears-1)*360+1):ndays])
aamass_benthc<-mean(benthc_i[((nyears-1)*360+1):ndays])
aamass_fishplar<-mean(fishplar_i[((nyears-1)*360+1):ndays])
aamass_fishp<-mean(fishp_i[((nyears-1)*360+1):ndays])
aamass_fishm<-mean(fishm_i[((nyears-1)*360+1):ndays])
aamass_fishdlar<-mean(fishdlar_i[((nyears-1)*360+1):ndays])
aamass_fishd<-mean(fishd_i[((nyears-1)*360+1):ndays])
aamass_bird<-mean(bird_i[((nyears-1)*360+1):ndays])

aamass_seal<-mean(seal_i[((nyears-1)*360+1):ndays])
aamass_ceta<-mean(ceta_i[((nyears-1)*360+1):ndays])

aamass_totalN<-mean(totalN_i[((nyears-1)*360+1):ndays])

#.........................................

mass_results<-data.frame(rep(0,55))

mass_results[1,1]<-aamass_s_detritus
mass_results[2,1]<-aamass_d_detritus
mass_results[3,1]<-aamass_x_detritus
mass_results[4,1]<-aamass_xR_detritus
mass_results[5,1]<-aamass_discard
mass_results[6,1]<-aamass_corpse

mass_results[7,1]<-aamass_kelpdebris

mass_results[8,1]<-aamass_s_ammonia
mass_results[9,1]<-aamass_d_ammonia
mass_results[10,1]<-aamass_x_ammonia
mass_results[11,1]<-aamass_s_nitrate
mass_results[12,1]<-aamass_d_nitrate
mass_results[13,1]<-aamass_x_nitrate

mass_results[14,1]<-aamass_kelpN

mass_results[15,1]<-aamass_s_phyt
mass_results[16,1]<-aamass_d_phyt
mass_results[17,1]<-aamass_omni
mass_results[18,1]<-aamass_carn
mass_results[19,1]<-aamass_benthslar
mass_results[20,1]<-aamass_benths
mass_results[21,1]<-aamass_benthclar
mass_results[22,1]<-aamass_benthc
mass_results[23,1]<-aamass_fishplar
mass_results[24,1]<-aamass_fishp
mass_results[25,1]<-aamass_fishm
mass_results[26,1]<-aamass_fishdlar
mass_results[27,1]<-aamass_fishd
mass_results[28,1]<-aamass_bird

mass_results[29,1]<-aamass_seal
mass_results[30,1]<-aamass_ceta


mass_results[31,1]<-aamass_totalN

mass_results[32,1]<-x_shallowprop
mass_results[33,1]<-si_depth
mass_results[34,1]<-so_depth
mass_results[35,1]<-d_depth

mass_results[36,1]<-x_area_s0

mass_results[37,1]<-x_area_s1
mass_results[38,1]<-x_area_s2
mass_results[39,1]<-x_area_s3

mass_results[40,1]<-x_area_d0

mass_results[41,1]<-x_area_d1
mass_results[42,1]<-x_area_d2
mass_results[43,1]<-x_area_d3
mass_results[44,1]<-x_depth_s1
mass_results[45,1]<-x_depth_s2
mass_results[46,1]<-x_depth_s3
mass_results[47,1]<-x_depth_d1
mass_results[48,1]<-x_depth_d2
mass_results[49,1]<-x_depth_d3
mass_results[50,1]<-x_poros_s1
mass_results[51,1]<-x_poros_s2
mass_results[52,1]<-x_poros_s3
mass_results[53,1]<-x_poros_d1
mass_results[54,1]<-x_poros_d2
mass_results[55,1]<-x_poros_d3

mass_results[,2]<-"mMN_in_the_whole model_domain_(1m2)"

mass_results[c(32,36:43,50:55),2]<-"dimensionless"
mass_results[c(33:35,44:49),2]<-"m"

mass_results[,3]<-mass_results_descriptions

colnames(mass_results)<-c("Model_annual_mean","Units","Description")

mass_results


#Print the data to a csv file
#-----------------------------------------------------------------

filename = csvname(resultsdir, "INSHORE_model_anav_biomass", identifier)
writecsv(mass_results, filename, row.names=FALSE)


#-------------------------------------------------------------------------------------------------------

#copy the mass_results dataframe as a template for the annual maximum data
maxmass_results<-mass_results
maxmass_results[1:31,1]<-0


aamaxmass_s_detritus<-max(detritus_si[((nyears-1)*360+1):ndays])
#aamaxmass_d_detritus<-max(detritus_d[((nyears-1)*360+1):ndays])
aamaxmass_d_detritus<-NA

aamaxmass_x_detritus<-max(x_detritus_i[((nyears-1)*360+1):ndays]) #Includes both labile and refractory detritus
aamaxmass_xR_detritus<-max(xR_detritus_s1[((nyears-1)*360+1):ndays]
                       + xR_detritus_s2[((nyears-1)*360+1):ndays]
                       + xR_detritus_s3[((nyears-1)*360+1):ndays])
aamaxmass_discard<-max(discard_i[((nyears-1)*360+1):ndays])
aamaxmass_corpse<-max(corpse_i[((nyears-1)*360+1):ndays])

aamaxmass_kelpdebris<-max(kelpdebris[((nyears-1)*360+1):ndays])

aamaxmass_s_ammonia<-max(ammonia_si[((nyears-1)*360+1):ndays])
aamaxmass_d_ammonia<-NA

aamaxmass_x_ammonia<-max(x_ammonia_i[((nyears-1)*360+1):ndays])
aamaxmass_s_nitrate<-max(nitrate_si[((nyears-1)*360+1):ndays])
aamaxmass_d_nitrate<-NA

aamaxmass_x_nitrate<-max(x_nitrate_i[((nyears-1)*360+1):ndays])

aamaxmass_kelpN<-max(kelpN[((nyears-1)*360+1):ndays])

aamaxmass_s_phyt<-max(phyt_si[((nyears-1)*360+1):ndays])
aamaxmass_d_phyt<-NA

aamaxmass_omni<-max(omni_i[((nyears-1)*360+1):ndays])
aamaxmass_carn<-max(carn_i[((nyears-1)*360+1):ndays])
aamaxmass_benthslar<-max(benthslar_i[((nyears-1)*360+1):ndays])
aamaxmass_benths<-max(benths_i[((nyears-1)*360+1):ndays])
aamaxmass_benthclar<-max(benthclar_i[((nyears-1)*360+1):ndays])
aamaxmass_benthc<-max(benthc_i[((nyears-1)*360+1):ndays])
aamaxmass_fishplar<-max(fishplar_i[((nyears-1)*360+1):ndays])
aamaxmass_fishp<-max(fishp_i[((nyears-1)*360+1):ndays])
aamaxmass_fishm<-max(fishm_i[((nyears-1)*360+1):ndays])
aamaxmass_fishdlar<-max(fishdlar_i[((nyears-1)*360+1):ndays])
aamaxmass_fishd<-max(fishd_i[((nyears-1)*360+1):ndays])
aamaxmass_bird<-max(bird_i[((nyears-1)*360+1):ndays])

aamaxmass_seal<-max(seal_i[((nyears-1)*360+1):ndays])
aamaxmass_ceta<-max(ceta_i[((nyears-1)*360+1):ndays])

aamaxmass_totalN<-max(totalN_i[((nyears-1)*360+1):ndays])


#..........................................

maxmass_results[1,1]<-aamaxmass_s_detritus
maxmass_results[2,1]<-aamaxmass_d_detritus
maxmass_results[3,1]<-aamaxmass_x_detritus
maxmass_results[4,1]<-aamaxmass_xR_detritus
maxmass_results[5,1]<-aamaxmass_discard
maxmass_results[6,1]<-aamaxmass_corpse

maxmass_results[7,1]<-aamaxmass_kelpdebris

maxmass_results[8,1]<-aamaxmass_s_ammonia
maxmass_results[9,1]<-aamaxmass_d_ammonia
maxmass_results[10,1]<-aamaxmass_x_ammonia
maxmass_results[11,1]<-aamaxmass_s_nitrate
maxmass_results[12,1]<-aamaxmass_d_nitrate
maxmass_results[13,1]<-aamaxmass_x_nitrate

maxmass_results[14,1]<-aamaxmass_kelpN

maxmass_results[15,1]<-aamaxmass_s_phyt
maxmass_results[16,1]<-aamaxmass_d_phyt
maxmass_results[17,1]<-aamaxmass_omni
maxmass_results[18,1]<-aamaxmass_carn
maxmass_results[19,1]<-aamaxmass_benthslar
maxmass_results[20,1]<-aamaxmass_benths
maxmass_results[21,1]<-aamaxmass_benthclar
maxmass_results[22,1]<-aamaxmass_benthc
maxmass_results[23,1]<-aamaxmass_fishplar
maxmass_results[24,1]<-aamaxmass_fishp
maxmass_results[25,1]<-aamaxmass_fishm
maxmass_results[26,1]<-aamaxmass_fishdlar
maxmass_results[27,1]<-aamaxmass_fishd
maxmass_results[28,1]<-aamaxmass_bird

maxmass_results[29,1]<-aamaxmass_seal
maxmass_results[30,1]<-aamaxmass_ceta


maxmass_results[31,1]<-aamaxmass_totalN


colnames(maxmass_results)<-c("Model_annual_maximum","Units","Description")


maxmass_results


#Print the data to a csv file
#-----------------------------------------------------------------
filename = csvname(resultsdir, "INSHORE_model_maximum_biomass", identifier)        
writecsv(maxmass_results, filename, row.names=FALSE)

#-------------------------------------------------------------------------------------------------------


#copy the mass_results dataframe as a template for the annual minimum data
minmass_results<-mass_results
minmass_results[1:31,1]<-0


aaminmass_s_detritus<-min(detritus_si[((nyears-1)*360+1):ndays])
aaminmass_d_detritus<-NA

aaminmass_x_detritus<-min(x_detritus_i[((nyears-1)*360+1):ndays]) #Includes both labile and refractory detritus
aaminmass_xR_detritus<-min(xR_detritus_s1[((nyears-1)*360+1):ndays]
                       + xR_detritus_s2[((nyears-1)*360+1):ndays]
                       + xR_detritus_s3[((nyears-1)*360+1):ndays])
aaminmass_discard<-min(discard_i[((nyears-1)*360+1):ndays])
aaminmass_corpse<-min(corpse_i[((nyears-1)*360+1):ndays])

aaminmass_kelpdebris<-min(kelpdebris[((nyears-1)*360+1):ndays])

aaminmass_s_ammonia<-min(ammonia_si[((nyears-1)*360+1):ndays])
aaminmass_d_ammonia<-NA

aaminmass_x_ammonia<-min(x_ammonia_i[((nyears-1)*360+1):ndays])
aaminmass_s_nitrate<-min(nitrate_si[((nyears-1)*360+1):ndays])
aaminmass_d_nitrate<-NA

aaminmass_x_nitrate<-min(x_nitrate_i[((nyears-1)*360+1):ndays])

aaminmass_kelpN<-min(kelpN[((nyears-1)*360+1):ndays])

aaminmass_s_phyt<-min(phyt_si[((nyears-1)*360+1):ndays])
aaminmass_d_phyt<-NA

aaminmass_omni<-min(omni_i[((nyears-1)*360+1):ndays])
aaminmass_carn<-min(carn_i[((nyears-1)*360+1):ndays])
aaminmass_benthslar<-min(benthslar_i[((nyears-1)*360+1):ndays])
aaminmass_benths<-min(benths_i[((nyears-1)*360+1):ndays])
aaminmass_benthclar<-min(benthclar_i[((nyears-1)*360+1):ndays])
aaminmass_benthc<-min(benthc_i[((nyears-1)*360+1):ndays])
aaminmass_fishplar<-min(fishplar_i[((nyears-1)*360+1):ndays])
aaminmass_fishp<-min(fishp_i[((nyears-1)*360+1):ndays])
aaminmass_fishm<-min(fishm_i[((nyears-1)*360+1):ndays])
aaminmass_fishdlar<-min(fishdlar_i[((nyears-1)*360+1):ndays])
aaminmass_fishd<-min(fishd_i[((nyears-1)*360+1):ndays])
aaminmass_bird<-min(bird_i[((nyears-1)*360+1):ndays])

aaminmass_seal<-min(seal_i[((nyears-1)*360+1):ndays])
aaminmass_ceta<-min(ceta_i[((nyears-1)*360+1):ndays])

aaminmass_totalN<-min(totalN_i[((nyears-1)*360+1):ndays])


#..........................................

minmass_results[1,1]<-aaminmass_s_detritus
minmass_results[2,1]<-aaminmass_d_detritus
minmass_results[3,1]<-aaminmass_x_detritus
minmass_results[4,1]<-aaminmass_xR_detritus
minmass_results[5,1]<-aaminmass_discard
minmass_results[6,1]<-aaminmass_corpse

minmass_results[7,1]<-aaminmass_kelpdebris

minmass_results[8,1]<-aaminmass_s_ammonia
minmass_results[9,1]<-aaminmass_d_ammonia
minmass_results[10,1]<-aaminmass_x_ammonia
minmass_results[11,1]<-aaminmass_s_nitrate
minmass_results[12,1]<-aaminmass_d_nitrate
minmass_results[13,1]<-aaminmass_x_nitrate

minmass_results[14,1]<-aaminmass_kelpN

minmass_results[15,1]<-aaminmass_s_phyt
minmass_results[16,1]<-aaminmass_d_phyt
minmass_results[17,1]<-aaminmass_omni
minmass_results[18,1]<-aaminmass_carn
minmass_results[19,1]<-aaminmass_benthslar
minmass_results[20,1]<-aaminmass_benths
minmass_results[21,1]<-aaminmass_benthclar
minmass_results[22,1]<-aaminmass_benthc
minmass_results[23,1]<-aaminmass_fishplar
minmass_results[24,1]<-aaminmass_fishp
minmass_results[25,1]<-aaminmass_fishm
minmass_results[26,1]<-aaminmass_fishdlar
minmass_results[27,1]<-aaminmass_fishd
minmass_results[28,1]<-aaminmass_bird

minmass_results[29,1]<-aaminmass_seal
minmass_results[30,1]<-aaminmass_ceta


minmass_results[31,1]<-aaminmass_totalN


colnames(minmass_results)<-c("Model_annual_minimum","Units","Description")


minmass_results


#Print the data to a csv file
#-----------------------------------------------------------------
filename = csvname(resultsdir, "INSHORE_model_minimum_biomass", identifier)        
writecsv(minmass_results, filename, row.names=FALSE)

#-------------------------------------------------------------------------------------------------------





#Extract all of the derived rate variables for the final year

DINinflow<-(fluxAMMinflow_i[ndays]-fluxAMMinflow_i[((nyears-1)*360+1)]) + (fluxNITinflow_i[ndays]-fluxNITinflow_i[((nyears-1)*360+1)])
DINoutflow<-(fluxAMMoutflow_i[ndays]-fluxAMMoutflow_i[((nyears-1)*360+1)]) + (fluxNIToutflow_i[ndays]-fluxNIToutflow_i[((nyears-1)*360+1)]) 
PARTinflow<-(fluxPHYTinflow_i[ndays]-fluxPHYTinflow_i[((nyears-1)*360+1)]) + (fluxDETinflow_i[ndays]-fluxDETinflow_i[((nyears-1)*360+1)])
PARToutflow<-(fluxPHYToutflow_i[ndays]-fluxPHYToutflow_i[((nyears-1)*360+1)]) + (fluxDEToutflow_i[ndays]-fluxDEToutflow_i[((nyears-1)*360+1)])
atmosphereDINinput<-(atmosAMMinput_i[ndays]-atmosAMMinput_i[((nyears-1)*360+1)]) + (atmosNITinput_i[ndays]-atmosNITinput_i[((nyears-1)*360+1)])

riverDINinflow<-(rivAMMinflow[ndays]-rivAMMinflow[((nyears-1)*360+1)]) + (rivNITinflow[ndays]-rivNITinflow[((nyears-1)*360+1)])
riverPARTinflow<-rivPARTinflow[ndays]-rivPARTinflow[((nyears-1)*360+1)]

sumDINinflow<-(fluxAMMinflow_i[ndays-90]-fluxAMMinflow_i[((nyears-1)*360+1+89)]) + (fluxNITinflow_i[ndays-90]-fluxNITinflow_i[((nyears-1)*360+1+89)])
sumDINoutflow<-(fluxAMMoutflow_i[ndays-90]-fluxAMMoutflow_i[((nyears-1)*360+1+89)]) + (fluxNIToutflow_i[ndays-90]-fluxNIToutflow_i[((nyears-1)*360+1+89)])
sumPARTinflow<-(fluxPHYTinflow_i[ndays-90]-fluxPHYTinflow_i[((nyears-1)*360+1+89)]) + (fluxDETinflow_i[ndays-90]-fluxDETinflow_i[((nyears-1)*360+1+89)]) 
sumPARToutflow<-(fluxPHYToutflow_i[ndays-90]-fluxPHYToutflow_i[((nyears-1)*360+1+89)]) + (fluxDEToutflow_i[ndays-90]-fluxDEToutflow_i[((nyears-1)*360+1+89)])
sumriverDINinflow<-(rivAMMinflow[ndays-90]-rivAMMinflow[((nyears-1)*360+1+89)]) + (rivNITinflow[ndays-90]-rivNITinflow[((nyears-1)*360+1+89)])
sumatmosDINinput<-(atmosAMMinput_i[ndays-90]-atmosAMMinput_i[((nyears-1)*360+1+89)]) + (atmosNITinput_i[ndays-90]-atmosNITinput_i[((nyears-1)*360+1+89)])
surfvertnitflux<-NA
surfhoriznitflux<-NA

sumriverNITinflow<-rivNITinflow[ndays-90]-rivNITinflow[((nyears-1)*360+1+89)]       # April - September inclusive
sumatmosNITinput<-(atmosNITinput_i[ndays-90]-atmosNITinput_i[((nyears-1)*360+1+89)])    # April - September inclusive

Flux_sedboundary <- (fluxsedboundary_i[ndays]-fluxsedboundary_i[((nyears-1)*360+1)]) 

kelp_beachcast<-((fluxkelpdebris_beachexport[ndays]-fluxkelpdebris_beachexport[((nyears-1)*360+1)]))   


DIN_NET_flux_o_i   <-  DIN_NET_flux_o_i[ndays]-DIN_NET_flux_o_i[((nyears-1)*360+1)]
PART_NET_flux_o_i  <-  PART_NET_flux_o_i[ndays]-PART_NET_flux_o_i[((nyears-1)*360+1)]
NET_activemigpelfish_o_i  <-  NET_activemigpelfish_o_i[ndays]-NET_activemigpelfish_o_i[((nyears-1)*360+1)]

NET_activemigmigfish_o_i  <-  NET_activemigmigfish_o_i[ndays]-NET_activemigmigfish_o_i[((nyears-1)*360+1)]
NET_activemigdemfish_o_i  <-  NET_activemigdemfish_o_i[ndays]-NET_activemigdemfish_o_i[((nyears-1)*360+1)]
NET_activemigbird_o_i  <-  NET_activemigbird_o_i[ndays]-NET_activemigbird_o_i[((nyears-1)*360+1)]
NET_activemigseal_o_i  <-  NET_activemigseal_o_i[ndays]-NET_activemigseal_o_i[((nyears-1)*360+1)]
NET_activemigceta_o_i  <-  NET_activemigceta_o_i[ndays]-NET_activemigceta_o_i[((nyears-1)*360+1)]

NET_mfish_ext_o<-NA

#Mfish_annual_imig <-    mfish_imigration[ndays]-mfish_imigration[((nyears-1)*360+1)]
#Mfish_annual_emig  <-   mfish_emigration[ndays]-mfish_emigration[((nyears-1)*360+1)]
Mfish_annual_imig <- NA
Mfish_annual_emig  <- NA



NETPrimaryP<-netpprod_i[ndays]-netpprod_i[((nyears-1)*360+1)]
MMP<-(max(nitrate_si[((nyears-1)*360+1):ndays])) - (min(nitrate_si[((nyears-1)*360+1):ndays]))
NNCP<-NNCP_i[ndays]-NNCP_i[((nyears-1)*360+1)]
PhytNitUp<-fluxwcnit_phyt_i[ndays]-fluxwcnit_phyt_i[((nyears-1)*360+1)]
PhytAmmUp<-fluxwcamm_phyt_i[ndays]-fluxwcamm_phyt_i[((nyears-1)*360+1)]
PNP<-MMP+sumriverNITinflow+sumatmosNITinput
fratio<-PNP/NETPrimaryP
Tfratio<-PhytNitUp/(PhytNitUp+PhytAmmUp)

KelpNitUp<-fluxwcnit_kelp[ndays]-fluxwcnit_kelp[((nyears-1)*360+1)]
KelpAmmUp<-fluxwcamm_kelp[ndays]-fluxwcamm_kelp[((nyears-1)*360+1)]


KelpNprod       <-   kelpNprod_i[ndays]      -     kelpNprod_i[((nyears-1)*360+1)]

Phytgrossprod        <-   phytgrossprod_i[ndays]      -     phytgrossprod_i[((nyears-1)*360+1)]

omnigrossprod        <-   omnigrossprod_i[ndays]      -     omnigrossprod_i[((nyears-1)*360+1)]
Carngrossprod        <-   carngrossprod_i[ndays]      -     carngrossprod_i[((nyears-1)*360+1)]
Fishplargrossprod    <-   pfishlargrossprod_i[ndays]  -     pfishlargrossprod_i[((nyears-1)*360+1)]
Fishdlargrossprod    <-   dfishlargrossprod_i[ndays]  -     dfishlargrossprod_i[((nyears-1)*360+1)]
Fishpgrossprod       <-   pfishgrossprod_i[ndays]     -     pfishgrossprod_i[((nyears-1)*360+1)]
Fishmgrossprod       <-   mfishgrossprod_i[ndays]     -     mfishgrossprod_i[((nyears-1)*360+1)]
Fishdgrossprod       <-   dfishgrossprod_i[ndays]     -     dfishgrossprod_i[((nyears-1)*360+1)]
Benthslargrossprod   <-   benthslargrossprod_i[ndays] -     benthslargrossprod_i[((nyears-1)*360+1)]
Benthclargrossprod   <-   benthclargrossprod_i[ndays] -     benthclargrossprod_i[((nyears-1)*360+1)]
Benthsgrossprod      <-   benthsgrossprod_i[ndays]    -     benthsgrossprod_i[((nyears-1)*360+1)]
Benthcgrossprod      <-   benthcgrossprod_i[ndays]    -     benthcgrossprod_i[((nyears-1)*360+1)]
Birdgrossprod        <-   birdgrossprod_i[ndays]      -     birdgrossprod_i[((nyears-1)*360+1)]
Sealgrossprod        <-   sealgrossprod_i[ndays]      -     sealgrossprod_i[((nyears-1)*360+1)]
Cetagrossprod        <-   cetagrossprod_i[ndays]      -     cetagrossprod_i[((nyears-1)*360+1)]


omninetprod        <-   omninetprod_i[ndays]      -     omninetprod_i[((nyears-1)*360+1)]
Carnnetprod        <-   carnnetprod_i[ndays]      -     carnnetprod_i[((nyears-1)*360+1)]
Fishplarnetprod    <-   pfishlarnetprod_i[ndays]  -     pfishlarnetprod_i[((nyears-1)*360+1)]
Fishdlarnetprod    <-   dfishlarnetprod_i[ndays]  -     dfishlarnetprod_i[((nyears-1)*360+1)]
Fishpnetprod       <-   pfishnetprod_i[ndays]     -     pfishnetprod_i[((nyears-1)*360+1)]
Fishmnetprod       <-   mfishnetprod_i[ndays]     -     mfishnetprod_i[((nyears-1)*360+1)]
Fishdnetprod       <-   dfishnetprod_i[ndays]     -     dfishnetprod_i[((nyears-1)*360+1)]
Benthslarnetprod   <-   benthslarnetprod_i[ndays] -     benthslarnetprod_i[((nyears-1)*360+1)]
Benthclarnetprod   <-   benthclarnetprod_i[ndays] -     benthclarnetprod_i[((nyears-1)*360+1)]
Benthsnetprod      <-   benthsnetprod_i[ndays]    -     benthsnetprod_i[((nyears-1)*360+1)]
Benthcnetprod      <-   benthcnetprod_i[ndays]    -     benthcnetprod_i[((nyears-1)*360+1)]
Birdnetprod        <-   birdnetprod_i[ndays]      -     birdnetprod_i[((nyears-1)*360+1)]
Sealnetprod        <-   sealnetprod_i[ndays]      -     sealnetprod_i[((nyears-1)*360+1)]
Cetanetprod        <-   cetanetprod_i[ndays]      -     cetanetprod_i[((nyears-1)*360+1)]



WCdetritusprod      <- NA

SEDdetritusprod     <- NA

Kelpdebrisprod     <-   fluxkelp_kelpdebris[ndays]      -     fluxkelp_kelpdebris[((nyears-1)*360+1)]

Corpseprod <- NA

#Includes the water column feeding flux of benthos
Fluxpartwc_sed <- NA

#Fluxdisc_corp       <-   fluxdisc_corp[ndays]      -     fluxdisc_corp[((nyears-1)*360+1)]
Fluxdisc_corp       <- NA

#Excludes mineralisation of detritus
Pelagammprod        <- NA

#Excludes mineralisation of detritus
Benthammprod        <- NA

#WCmineralisation    <-   fluxwcdet_wcamm[ndays]      -     fluxwcdet_wcamm[((nyears-1)*360+1)]
WCmineralisation    <- NA

#SEDmineralisation   <-   fluxxdet_sedamm[ndays]      -     fluxxdet_sedamm[((nyears-1)*360+1)]+
#                        +fluxxRdet_sedamm[ndays]      -     fluxxRdet_sedamm[((nyears-1)*360+1)]
SEDmineralisation   <- NA

#WCnitrification     <-   fluxwcamm_wcnit[ndays]      -     fluxwcamm_wcnit[((nyears-1)*360+1)]
WCnitrification     <- NA

#SEDnitrification    <-   fluxsedamm_sednit[ndays]      -     fluxsedamm_sednit[((nyears-1)*360+1)]
SEDnitrification    <- NA

#WCdenitrification   <-   wcdenitrif[ndays]      -     wcdenitrif[((nyears-1)*360+1)]
WCdenitrification   <- NA

#SEDdenitrification  <-   seddenitrif[ndays]      -     seddenitrif[((nyears-1)*360+1)]
SEDdenitrification  <- NA

#Includes excretion by benthos
SEDWCammflux        <- NA


#SEDWCnitflux        <-   fluxsednit_wcnit[ndays]      -     fluxsednit_wcnit[((nyears-1)*360+1)]
SEDWCnitflux        <- NA


Fluxdet_omni           <- NA
Fluxphyt_omni          <- NA
Fluxbenthslar_omni          <- NA
Fluxbenthclar_omni          <- NA


Fluxomni_carn           <- NA
Fluxpfishlar_carn       <- NA
Fluxdfishlar_carn       <- NA
Fluxbenthslar_carn       <- NA
Fluxbenthclar_carn       <- NA


Fluxomni_pfishlar           <-  NA
Fluxbenthslar_pfishlar        <-  NA
Fluxbenthclar_pfishlar        <-  NA



Fluxomni_dfishlar           <-  NA
Fluxbenthslar_dfishlar        <- NA
Fluxbenthclar_dfishlar        <- NA



Fluxomni_pfish            <-  NA
Fluxcarn_pfish            <-  NA
Fluxpfishlar_pfish        <-  NA
Fluxdfishlar_pfish        <-  NA
Fluxbenthslar_pfish         <- NA
Fluxbenthclar_pfish         <-  NA



Fluxomni_mfish            <- NA
Fluxcarn_mfish            <- NA
Fluxpfishlar_mfish        <- NA
Fluxdfishlar_mfish        <- NA
Fluxbenthslar_mfish       <- NA
Fluxbenthclar_mfish       <- NA



Fluxcorp_dfish            <-  NA
Fluxdisc_dfish            <-  NA
Fluxcarn_dfish            <-  NA
Fluxpfishlar_dfish        <-  NA
Fluxdfishlar_dfish        <-  NA
Fluxpfish_dfish           <-  NA
Fluxmfish_dfish           <-  NA
Fluxdfish_dfish           <-  NA
Fluxbenths_dfish          <-  NA
Fluxbenthc_dfish          <-  NA


Fluxdet_benthslar            <-  NA
Fluxphyt_benthslar           <-  NA

Fluxdet_benthclar            <- NA
Fluxphyt_benthclar           <- NA

Fluxdet_benths             <- NA
Fluxseddet_benths          <- NA
Fluxphyt_benths            <- NA

Fluxkelp_benthc            <-   fluxkelp_benthc[ndays]      -     fluxkelp_benthc[((nyears-1)*360+1)]    
Fluxkelpdebris_benthc            <-   fluxkelpdebris_benthc[ndays]      -     fluxkelpdebris_benthc[((nyears-1)*360+1)]    

Fluxcorp_benthc            <- NA
Fluxbenths_benthc          <- NA

Fluxcorp_bird            <-  NA
Fluxdisc_bird            <-  NA
#Fluxomni_bird            <- NA
Fluxcarn_bird            <-  NA
Fluxpfish_bird           <-  NA
Fluxmfish_bird           <-  NA
Fluxdfish_bird           <-  NA
Fluxbenths_bird          <-  NA
Fluxbenthc_bird          <-  NA


     
Fluxcorp_seal            <- NA
Fluxdisc_seal            <- NA
Fluxcarn_seal            <- NA
Fluxpfish_seal           <- NA
Fluxmfish_seal           <- NA
Fluxdfish_seal           <- NA
Fluxbenths_seal          <- NA
Fluxbenthc_seal          <- NA
Fluxbird_seal          <-   NA


Fluxdisc_ceta            <-  NA
Fluxomni_ceta            <-  NA
Fluxcarn_ceta            <-  NA
Fluxpfish_ceta           <-  NA
Fluxmfish_ceta           <-  NA
Fluxdfish_ceta           <-  NA
Fluxbenths_ceta          <-  NA
Fluxbenthc_ceta          <-  NA
Fluxbird_ceta            <-  NA
Fluxseal_ceta            <-  NA

HTLP<-   omninetprod +
       + Carnnetprod +
       + Benthsnetprod +
       + Benthcnetprod +
       + Benthslarnetprod +
       + Benthclarnetprod +
       + Fishplarnetprod +
       + Fishdlarnetprod +
       + Fishpnetprod +
       + Fishmnetprod +
       + Fishdnetprod +
       + Birdnetprod +
       + Sealnetprod +
       + Cetanetprod 


export_from_2prod<- NA

Pfish_annual_spawn   <- NA
Pfish_annual_recruit <- NA
Dfish_annual_spawn   <- NA
Dfish_annual_recruit <- NA



Benths_annual_spawn   <- NA
Benths_annual_recruit <- NA
Benthc_annual_spawn   <- NA
Benthc_annual_recruit <- NA



FishpLand_livewt<-landp_i[ndays]-landp_i[((nyears-1)*360+1)]
FishmLand_livewt<-landm_i[ndays]-landm_i[((nyears-1)*360+1)]
FishdLand_livewt<-landd_i[ndays]-landd_i[((nyears-1)*360+1)]
Fishd_qLand_livewt<-landd_quota_i[ndays]-landd_quota_i[((nyears-1)*360+1)]
Fishd_nqLand_livewt<-landd_nonquota_i[ndays]-landd_nonquota_i[((nyears-1)*360+1)]
BenthsLand_livewt <- landsb_i[ndays]-landsb_i[((nyears-1)*360+1)]
BenthcLand_livewt <- landcb_i[ndays]-landcb_i[((nyears-1)*360+1)]
CarnzLand_livewt <- landcz_i[ndays]-landcz_i[((nyears-1)*360+1)]
BirdLand_livewt <- landbd_i[ndays]-landbd_i[((nyears-1)*360+1)]
SealLand_livewt <- landsl_i[ndays]-landsl_i[((nyears-1)*360+1)]
CetaLand_livewt <- landct_i[ndays]-landct_i[((nyears-1)*360+1)]
KelpLand_livewt <- landkp_i[ndays]-landkp_i[((nyears-1)*360+1)]

FishpDiscard<-discpel_i[ndays]-discpel_i[((nyears-1)*360+1)]
FishmDiscard<-discmig_i[ndays]-discmig_i[((nyears-1)*360+1)]
FishdDiscard<-discdem_i[ndays]-discdem_i[((nyears-1)*360+1)]
Fishd_qDiscard<-discdem_quota_i[ndays]-discdem_quota_i[((nyears-1)*360+1)]
Fishd_nqDiscard<-discdem_nonquota_i[ndays]-discdem_nonquota_i[((nyears-1)*360+1)]
BenthsDiscard <- discsb_i[ndays]-discsb_i[((nyears-1)*360+1)]
BenthcDiscard <- disccb_i[ndays]-disccb_i[((nyears-1)*360+1)]
CarnzDiscard  <- disccz_i[ndays]-disccz_i[((nyears-1)*360+1)]
BirdDiscard   <- discbd_i[ndays]-discbd_i[((nyears-1)*360+1)]
SealDiscard   <- discsl_i[ndays]-discsl_i[((nyears-1)*360+1)]
CetaDiscard   <- discct_i[ndays]-discct_i[((nyears-1)*360+1)]
KelpDiscard   <- disckp_i[ndays]-disckp_i[((nyears-1)*360+1)]
#KelpDiscard   <- NA

FishpOffal<-offalpel_i[ndays]-offalpel_i[((nyears-1)*360+1)]
FishmOffal<-offalmig_i[ndays]-offalmig_i[((nyears-1)*360+1)]
FishdOffal<-offaldem_i[ndays]-offaldem_i[((nyears-1)*360+1)]
Fishd_qOffal<-offaldem_quota_i[ndays]-offaldem_quota_i[((nyears-1)*360+1)]
Fishd_nqOffal<-offaldem_nonquota_i[ndays]-offaldem_nonquota_i[((nyears-1)*360+1)]
BenthsOffal <- offalsb_i[ndays]-offalsb_i[((nyears-1)*360+1)]
BenthcOffal <- offalcb_i[ndays]-offalcb_i[((nyears-1)*360+1)]
CarnzOffal  <- offalcz_i[ndays]-offalcz_i[((nyears-1)*360+1)]
BirdOffal   <- offalbd_i[ndays]-offalbd_i[((nyears-1)*360+1)]
SealOffal   <- offalsl_i[ndays]-offalsl_i[((nyears-1)*360+1)]
CetaOffal   <- offalct_i[ndays]-offalct_i[((nyears-1)*360+1)]
KelpOffal   <- offalkp_i[ndays]-offalkp_i[((nyears-1)*360+1)]
#KelpOffal   <- NA

FishpLand_processedwt<- FishpLand_livewt - FishpOffal
FishmLand_processedwt<- FishmLand_livewt - FishmOffal
FishdLand_processedwt<- FishdLand_livewt - FishdOffal
Fishd_qLand_processedwt<- Fishd_qLand_livewt - Fishd_qOffal
Fishd_nqLand_processedwt<- Fishd_nqLand_livewt - Fishd_nqOffal
BenthsLand_processedwt <- BenthsLand_livewt - BenthsOffal
BenthcLand_processedwt <- BenthcLand_livewt - BenthcOffal
CarnzLand_processedwt <- CarnzLand_livewt - CarnzOffal
BirdLand_processedwt <- BirdLand_livewt - BirdOffal
SealLand_processedwt <- SealLand_livewt - SealOffal
CetaLand_processedwt <- CetaLand_livewt - CetaOffal
KelpLand_processedwt <- KelpLand_livewt - KelpOffal
#KelpLand_processedwt <- NA



annual_flux_results<-data.frame(rep(0,240))

#EXTRACT THE RESULTS INTO A DATA FRAME
#COLUMN 1 IS IN NITROGEN UNITS (mM N/m2/y)

#DERIVED ANNUAL VALUES IN UNITS OF mM N/m2 and fluxes in mMN/m2/y

annual_flux_results[1,1]<-DINinflow
annual_flux_results[2,1]<-DINoutflow
annual_flux_results[3,1]<-PARTinflow
annual_flux_results[4,1]<-PARToutflow
annual_flux_results[5,1]<-atmosphereDINinput
annual_flux_results[6,1]<-riverDINinflow
annual_flux_results[7,1]<-riverPARTinflow
annual_flux_results[8,1]<-sumDINinflow
annual_flux_results[9,1]<-sumDINoutflow
annual_flux_results[10,1]<-sumPARTinflow
annual_flux_results[11,1]<-sumPARToutflow
annual_flux_results[12,1]<-sumriverDINinflow
annual_flux_results[13,1]<-sumatmosDINinput
annual_flux_results[14,1]<-surfvertnitflux
annual_flux_results[15,1]<-surfhoriznitflux
annual_flux_results[16,1]<-Flux_sedboundary

annual_flux_results[17,1]<-kelp_beachcast


annual_flux_results[18,1]<-DIN_NET_flux_o_i
annual_flux_results[19,1]<-PART_NET_flux_o_i
annual_flux_results[20,1]<-NET_activemigpelfish_o_i

annual_flux_results[21,1]<-NET_activemigmigfish_o_i
annual_flux_results[22,1]<-NET_activemigdemfish_o_i
annual_flux_results[23,1]<-NET_activemigbird_o_i
annual_flux_results[24,1]<-NET_activemigseal_o_i
annual_flux_results[25,1]<-NET_activemigceta_o_i

annual_flux_results[26,1]<-NET_mfish_ext_o 
annual_flux_results[27,1]<-Mfish_annual_imig
annual_flux_results[28,1]<-Mfish_annual_emig
annual_flux_results[29,1]<-NETPrimaryP
annual_flux_results[30,1]<-MMP
annual_flux_results[31,1]<-PNP
annual_flux_results[32,1]<-NNCP
annual_flux_results[33,1]<-PhytNitUp
annual_flux_results[34,1]<-PhytAmmUp
annual_flux_results[35,1]<-fratio
annual_flux_results[36,1]<-Tfratio

annual_flux_results[37,1]<-KelpNitUp
annual_flux_results[38,1]<-KelpAmmUp

annual_flux_results[39,1]<-KelpNprod


annual_flux_results[40,1]<-Phytgrossprod

annual_flux_results[41,1]<-omnigrossprod
annual_flux_results[42,1]<-Carngrossprod
annual_flux_results[43,1]<-Fishplargrossprod
annual_flux_results[44,1]<-Fishdlargrossprod
annual_flux_results[45,1]<-Fishpgrossprod 
annual_flux_results[46,1]<-Fishmgrossprod 
annual_flux_results[47,1]<-Fishdgrossprod 
annual_flux_results[48,1]<-Benthslargrossprod 
annual_flux_results[49,1]<-Benthclargrossprod 
annual_flux_results[50,1]<-Benthsgrossprod    
annual_flux_results[51,1]<-Benthcgrossprod    
annual_flux_results[52,1]<-Birdgrossprod
annual_flux_results[53,1]<-Sealgrossprod
annual_flux_results[54,1]<-Cetagrossprod

annual_flux_results[55,1]<-omninetprod
annual_flux_results[56,1]<-Carnnetprod
annual_flux_results[57,1]<-Fishplarnetprod
annual_flux_results[58,1]<-Fishdlarnetprod
annual_flux_results[59,1]<-Fishpnetprod 
annual_flux_results[60,1]<-Fishmnetprod 
annual_flux_results[61,1]<-Fishdnetprod 
annual_flux_results[62,1]<-Benthslarnetprod 
annual_flux_results[63,1]<-Benthclarnetprod 
annual_flux_results[64,1]<-Benthsnetprod    
annual_flux_results[65,1]<-Benthcnetprod    
annual_flux_results[66,1]<-Birdnetprod
annual_flux_results[67,1]<-Sealnetprod
annual_flux_results[68,1]<-Cetanetprod


      
annual_flux_results[69,1]<-WCdetritusprod  
annual_flux_results[70,1]<-SEDdetritusprod 
annual_flux_results[71,1]<-Corpseprod      
annual_flux_results[72,1]<-Fluxpartwc_sed  
annual_flux_results[73,1]<-Fluxdisc_corp   
annual_flux_results[74,1]<-Pelagammprod    
annual_flux_results[75,1]<-Benthammprod    
annual_flux_results[76,1]<-WCmineralisation
annual_flux_results[77,1]<-SEDmineralisation
annual_flux_results[78,1]<-WCnitrification  
annual_flux_results[79,1]<-SEDnitrification 
annual_flux_results[80,1]<-WCdenitrification
annual_flux_results[81,1]<-SEDdenitrification
annual_flux_results[82,1]<-SEDWCammflux      
annual_flux_results[83,1]<-SEDWCnitflux      
annual_flux_results[84,1]<-Fluxdet_omni    
annual_flux_results[85,1]<-Fluxphyt_omni   
annual_flux_results[86,1]<-Fluxbenthslar_omni 
annual_flux_results[87,1]<-Fluxbenthclar_omni 
annual_flux_results[88,1]<-Fluxomni_carn      
annual_flux_results[89,1]<-Fluxpfishlar_carn  
annual_flux_results[90,1]<-Fluxdfishlar_carn  
annual_flux_results[91,1]<-Fluxbenthslar_carn   
annual_flux_results[92,1]<-Fluxbenthclar_carn   
annual_flux_results[93,1]<-Fluxomni_pfishlar   
annual_flux_results[94,1]<-Fluxbenthslar_pfishlar
annual_flux_results[95,1]<-Fluxbenthclar_pfishlar
annual_flux_results[96,1]<-Fluxomni_dfishlar   
annual_flux_results[97,1]<-Fluxbenthslar_dfishlar
annual_flux_results[98,1]<-Fluxbenthclar_dfishlar
annual_flux_results[99,1]<-Fluxomni_pfish       
annual_flux_results[100,1]<-Fluxcarn_pfish       
annual_flux_results[101,1]<-Fluxpfishlar_pfish   
annual_flux_results[102,1]<-Fluxdfishlar_pfish   
annual_flux_results[103,1]<-Fluxbenthslar_pfish    
annual_flux_results[104,1]<-Fluxbenthclar_pfish    
annual_flux_results[105,1]<-Fluxomni_mfish      
annual_flux_results[106,1]<-Fluxcarn_mfish      
annual_flux_results[107,1]<-Fluxpfishlar_mfish  
annual_flux_results[108,1]<-Fluxdfishlar_mfish  
annual_flux_results[109,1]<-Fluxbenthslar_mfish 
annual_flux_results[110,1]<-Fluxbenthclar_mfish 
annual_flux_results[111,1]<-Fluxcorp_dfish      
annual_flux_results[112,1]<-Fluxdisc_dfish
annual_flux_results[113,1]<-Fluxcarn_dfish      
annual_flux_results[114,1]<-Fluxpfishlar_dfish  
annual_flux_results[115,1]<-Fluxdfishlar_dfish  
annual_flux_results[116,1]<-Fluxpfish_dfish     
annual_flux_results[117,1]<-Fluxmfish_dfish     
annual_flux_results[118,1]<-Fluxdfish_dfish     
annual_flux_results[119,1]<-Fluxbenths_dfish    
annual_flux_results[120,1]<-Fluxbenthc_dfish    
annual_flux_results[121,1]<-Fluxdet_benthslar   
annual_flux_results[122,1]<-Fluxphyt_benthslar  
annual_flux_results[123,1]<-Fluxdet_benthclar   
annual_flux_results[124,1]<-Fluxphyt_benthclar  
annual_flux_results[125,1]<-Fluxdet_benths      
annual_flux_results[126,1]<-Fluxseddet_benths   
annual_flux_results[127,1]<-Fluxphyt_benths     
annual_flux_results[128,1]<-Fluxkelpdebris_benthc     
annual_flux_results[129,1]<-Fluxcorp_benthc     
annual_flux_results[130,1]<-Fluxkelp_benthc     
annual_flux_results[131,1]<-Fluxbenths_benthc   
annual_flux_results[132,1]<-Fluxcorp_bird       
annual_flux_results[133,1]<-Fluxdisc_bird       

#annual_flux_results[xxx,1]<-Fluxomni_bird       

annual_flux_results[134,1]<-Fluxcarn_bird       
annual_flux_results[135,1]<-Fluxpfish_bird      
annual_flux_results[136,1]<-Fluxmfish_bird      
annual_flux_results[137,1]<-Fluxdfish_bird      
annual_flux_results[138,1]<-Fluxbenths_bird     
annual_flux_results[139,1]<-Fluxbenthc_bird     
annual_flux_results[140,1]<-Fluxcorp_seal       
annual_flux_results[141,1]<-Fluxdisc_seal       
annual_flux_results[142,1]<-Fluxcarn_seal       
annual_flux_results[143,1]<-Fluxpfish_seal      
annual_flux_results[144,1]<-Fluxmfish_seal      
annual_flux_results[145,1]<-Fluxdfish_seal      
annual_flux_results[146,1]<-Fluxbenths_seal     
annual_flux_results[147,1]<-Fluxbenthc_seal     
annual_flux_results[148,1]<-Fluxbird_seal     
annual_flux_results[149,1]<-Fluxdisc_ceta       
annual_flux_results[150,1]<-Fluxomni_ceta       
annual_flux_results[151,1]<-Fluxcarn_ceta       
annual_flux_results[152,1]<-Fluxpfish_ceta      
annual_flux_results[153,1]<-Fluxmfish_ceta      
annual_flux_results[154,1]<-Fluxdfish_ceta      
annual_flux_results[155,1]<-Fluxbenths_ceta     
annual_flux_results[156,1]<-Fluxbenthc_ceta     
annual_flux_results[157,1]<-Fluxbird_ceta     
annual_flux_results[158,1]<-Fluxseal_ceta     

annual_flux_results[159,1]<-HTLP
annual_flux_results[160,1]<-export_from_2prod
annual_flux_results[161,1]<-Pfish_annual_spawn   
annual_flux_results[162,1]<-Pfish_annual_recruit 
annual_flux_results[163,1]<-Dfish_annual_spawn   
annual_flux_results[164,1]<-Dfish_annual_recruit 
annual_flux_results[165,1]<-Benths_annual_spawn   
annual_flux_results[166,1]<-Benths_annual_recruit 
annual_flux_results[167,1]<-Benthc_annual_spawn   
annual_flux_results[168,1]<-Benthc_annual_recruit 

annual_flux_results[169,1]<-FishpLand_livewt
annual_flux_results[170,1]<-FishmLand_livewt
annual_flux_results[171,1]<-FishdLand_livewt
annual_flux_results[172,1]<-Fishd_qLand_livewt
annual_flux_results[173,1]<-Fishd_nqLand_livewt
annual_flux_results[174,1]<-BenthsLand_livewt
annual_flux_results[175,1]<-BenthcLand_livewt 
annual_flux_results[176,1]<-CarnzLand_livewt 
annual_flux_results[177,1]<-BirdLand_livewt 
annual_flux_results[178,1]<-SealLand_livewt 
annual_flux_results[179,1]<-CetaLand_livewt 
annual_flux_results[180,1]<-KelpLand_livewt 

annual_flux_results[181,1]<-FishpDiscard
annual_flux_results[182,1]<-FishmDiscard
annual_flux_results[183,1]<-FishdDiscard
annual_flux_results[184,1]<-Fishd_qDiscard
annual_flux_results[185,1]<-Fishd_nqDiscard
annual_flux_results[186,1]<-BenthsDiscard
annual_flux_results[187,1]<-BenthcDiscard
annual_flux_results[188,1]<-CarnzDiscard 
annual_flux_results[189,1]<-BirdDiscard
annual_flux_results[190,1]<-SealDiscard
annual_flux_results[191,1]<-CetaDiscard
annual_flux_results[192,1]<-KelpDiscard

annual_flux_results[193,1]<-FishpOffal
annual_flux_results[194,1]<-FishmOffal
annual_flux_results[195,1]<-FishdOffal
annual_flux_results[196,1]<-Fishd_qOffal
annual_flux_results[197,1]<-Fishd_nqOffal
annual_flux_results[198,1]<-BenthsOffal
annual_flux_results[199,1]<-BenthcOffal
annual_flux_results[200,1]<-CarnzOffal 
annual_flux_results[201,1]<-BirdOffal
annual_flux_results[202,1]<-SealOffal
annual_flux_results[203,1]<-CetaOffal
annual_flux_results[204,1]<-KelpOffal

annual_flux_results[205,1]<-FishpLand_processedwt
annual_flux_results[206,1]<-FishmLand_processedwt
annual_flux_results[207,1]<-FishdLand_processedwt
annual_flux_results[208,1]<-Fishd_qLand_processedwt
annual_flux_results[209,1]<-Fishd_nqLand_processedwt
annual_flux_results[210,1]<-BenthsLand_processedwt
annual_flux_results[211,1]<-BenthcLand_processedwt 
annual_flux_results[212,1]<-CarnzLand_processedwt 
annual_flux_results[213,1]<-BirdLand_processedwt 
annual_flux_results[214,1]<-SealLand_processedwt 
annual_flux_results[215,1]<-CetaLand_processedwt 
annual_flux_results[216,1]<-KelpLand_processedwt 

annual_flux_results[217,1]<-x_shallowprop
annual_flux_results[218,1]<-si_depth
annual_flux_results[219,1]<-so_depth
annual_flux_results[220,1]<-d_depth

annual_flux_results[221,1]<-x_area_s0

annual_flux_results[222,1]<-x_area_s1
annual_flux_results[223,1]<-x_area_s2
annual_flux_results[224,1]<-x_area_s3

annual_flux_results[225,1]<-x_area_d0

annual_flux_results[226,1]<-x_area_d1
annual_flux_results[227,1]<-x_area_d2
annual_flux_results[228,1]<-x_area_d3
annual_flux_results[229,1]<-x_depth_s1
annual_flux_results[230,1]<-x_depth_s2
annual_flux_results[231,1]<-x_depth_s3
annual_flux_results[232,1]<-x_depth_d1
annual_flux_results[233,1]<-x_depth_d2
annual_flux_results[234,1]<-x_depth_d3
annual_flux_results[235,1]<-x_poros_s1
annual_flux_results[236,1]<-x_poros_s2
annual_flux_results[237,1]<-x_poros_s3
annual_flux_results[238,1]<-x_poros_d1
annual_flux_results[239,1]<-x_poros_d2
annual_flux_results[240,1]<-x_poros_d3



annual_flux_results[,2]<-rep("mMN/whole_model_domain_(1m2)/y",240)
annual_flux_results[8:13,2]<-("mMN/whole_model_domain_(1m2)/summer_period_AMJJAS")
annual_flux_results[c(217,221:228,235:240),2]<-"dimensionless"
annual_flux_results[c(218:220,229:234),2]<-"m"


annual_flux_results[,3]<-annual_flux_descriptions

names(annual_flux_results)<-c("Model_annual_flux","Units","Description")


#Print the data to a csv file
#-----------------------------------------------------------------
	filename = csvname(resultsdir, "INSHORE_model_annualresults", identifier)
	writecsv(annual_flux_results, filename, row.names=FALSE)

	list(
		mass_results		= mass_results,
		maxmass_results		= maxmass_results,
		minmass_results		= minmass_results,
		annual_flux_results	= annual_flux_results
	)
}

