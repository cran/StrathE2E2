#
# derive_annual_results_wholedomain.R
#
#' extract annual average biomass in the final year for the whole model domain
#'
#' @param model model object
#' @param build model build object
#' @param output  model output
#' @param aggregates aggregated model output
#'
#' @noRd
#
# ------------------------------------------------------------------------------

derive_annual_results_wholedomain <- function(model, build, output, aggregates) {

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
	bx_depth		<- elt(physical.parms, "bx_depth")
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
	x_rock_s1		<- elt(physical.parms, "x_rock_s1")
	x_rock_s2		<- elt(physical.parms, "x_rock_s2")
	x_rock_s3		<- elt(physical.parms, "x_rock_s3")
	x_rock_d1		<- elt(physical.parms, "x_rock_d1")
	x_rock_d2		<- elt(physical.parms, "x_rock_d2")
	x_rock_d3		<- elt(physical.parms, "x_rock_d3")
	x_nonrock_s		<- elt(physical.parms, "x_nonrock_s")
	x_nonrock_d		<- elt(physical.parms, "x_nonrock_d")
	x_poros_s1		<- elt(physical.parms, "x_poros_s1")
	x_poros_s2		<- elt(physical.parms, "x_poros_s2")
	x_poros_s3		<- elt(physical.parms, "x_poros_s3")
	x_poros_d1		<- elt(physical.parms, "x_poros_d1")
	x_poros_d2		<- elt(physical.parms, "x_poros_d2")
	x_poros_d3		<- elt(physical.parms, "x_poros_d3")
	Kxw_s1			<- elt(physical.parms, "Kxw_s1")
	Kxw_s2			<- elt(physical.parms, "Kxw_s2")
	Kxw_s3			<- elt(physical.parms, "Kxw_s3")
	Kxw_d1			<- elt(physical.parms, "Kxw_d1")
	Kxw_d2			<- elt(physical.parms, "Kxw_d2")
	Kxw_d3			<- elt(physical.parms, "Kxw_d3")
	xbioturb_depth_s1	<- elt(physical.parms, "xbioturb_depth_s1")
	xbioturb_depth_s2	<- elt(physical.parms, "xbioturb_depth_s2")
	xbioturb_depth_s3	<- elt(physical.parms, "xbioturb_depth_s3")
	xbioturb_depth_d1	<- elt(physical.parms, "xbioturb_depth_d1")
	xbioturb_depth_d2	<- elt(physical.parms, "xbioturb_depth_d2")
	xbioturb_depth_d3	<- elt(physical.parms, "xbioturb_depth_d3")
	xerosion_depth_s1	<- elt(physical.parms, "xerosion_depth_s1")
	xerosion_depth_s2	<- elt(physical.parms, "xerosion_depth_s2")
	xerosion_depth_s3	<- elt(physical.parms, "xerosion_depth_s3")
	xerosion_depth_d1	<- elt(physical.parms, "xerosion_depth_d1")
	xerosion_depth_d2	<- elt(physical.parms, "xerosion_depth_d2")
	xerosion_depth_d3	<- elt(physical.parms, "xerosion_depth_d3")
	xlightSPM_intercept	<- elt(physical.parms, "xlightSPM_intercept")
	xlightSPM_slope		<- elt(physical.parms, "xlightSPM_slope")
	xinshore_phyt_prop_depth<- elt(physical.parms, "xinshore_phyt_prop_depth")
	xinshore_kelp_prop_depth<- elt(physical.parms, "xinshore_kelp_prop_depth")
	x_xR_detritus_s1	<- elt(physical.parms, "x_xR_detritus_s1")
	x_xR_detritus_s2	<- elt(physical.parms, "x_xR_detritus_s2")
	x_xR_detritus_s3	<- elt(physical.parms, "x_xR_detritus_s3")
	x_xR_detritus_d1	<- elt(physical.parms, "x_xR_detritus_d1")
	x_xR_detritus_d2	<- elt(physical.parms, "x_xR_detritus_d2")
	x_xR_detritus_d3	<- elt(physical.parms, "x_xR_detritus_d3")
	ref_Kxw			<- elt(physical.parms, "ref_Kxw")
	x_shallowprop		<- elt(physical.parms, "x_shallowprop")
	habitat_areas		<- elt(physical.parms, "habitat_areas")

	# extract output:
	detritus_d		<- elt(output, "detritus_d")
	xR_detritus_s1		<- elt(output, "xR_detritus_s1")
	xR_detritus_s2		<- elt(output, "xR_detritus_s2")
	xR_detritus_s3		<- elt(output, "xR_detritus_s3")
	xR_detritus_d1		<- elt(output, "xR_detritus_d1")
	xR_detritus_d2		<- elt(output, "xR_detritus_d2")
	xR_detritus_d3		<- elt(output, "xR_detritus_d3")
	kelpdebris		<- elt(output, "kelpdebris")
	ammonia_d		<- elt(output, "ammonia_d")
	nitrate_d		<- elt(output, "nitrate_d")
	kelpN			<- elt(output, "kelpN")
	phyt_d			<- elt(output, "phyt_d")
	rivPARTinflow		<- elt(output, "rivPARTinflow")
	vertnitflux		<- elt(output, "vertnitflux")
	horiznitflux		<- elt(output, "horiznitflux")
	fluxkelpdebris_beachexport		<- elt(output, "fluxkelpdebris_beachexport")
	mfish_imigration	<- elt(output, "mfish_imigration")
	mfish_emigration	<- elt(output, "mfish_emigration")
	fluxwcnit_kelp		<- elt(output, "fluxwcnit_kelp")
	fluxwcamm_kelp		<- elt(output, "fluxwcamm_kelp")
	kelpNprod_i		<- elt(output, "kelpNprod_i")
	fluxcorp_wcdet		<- elt(output, "fluxcorp_wcdet")
	fluxkelpdebris_wcdet	<- elt(output, "fluxkelpdebris_wcdet")
	fluxphyt_wcdet		<- elt(output, "fluxphyt_wcdet")
	fluxomni_wcdet		<- elt(output, "fluxomni_wcdet")
	fluxcarn_wcdet		<- elt(output, "fluxcarn_wcdet")
	fluxpfishlar_wcdet	<- elt(output, "fluxpfishlar_wcdet")
	fluxdfishlar_wcdet	<- elt(output, "fluxdfishlar_wcdet")
	fluxpfish_wcdet		<- elt(output, "fluxpfish_wcdet")
	fluxmfish_wcdet		<- elt(output, "fluxmfish_wcdet")
	fluxdfish_wcdet		<- elt(output, "fluxdfish_wcdet")
	fluxbenthslar_wcdet	<- elt(output, "fluxbenthslar_wcdet")
	fluxbenthclar_wcdet	<- elt(output, "fluxbenthclar_wcdet")
	fluxbenths_wcdet	<- elt(output, "fluxbenths_wcdet")
	fluxbenthc_wcdet	<- elt(output, "fluxbenthc_wcdet")
	fluxbird_wcdet		<- elt(output, "fluxbird_wcdet")
	fluxseal_wcdet		<- elt(output, "fluxseal_wcdet")
	fluxceta_wcdet		<- elt(output, "fluxceta_wcdet")
	fluxcorp_xdet		<- elt(output, "fluxcorp_xdet")
	fluxbenths_xdet		<- elt(output, "fluxbenths_xdet")
	fluxbenthc_xdet		<- elt(output, "fluxbenthc_xdet")
	fluxkelp_kelpdebris	<- elt(output, "fluxkelp_kelpdebris")
	fluxdisc_corp		<- elt(output, "fluxdisc_corp")
	fluxpfish_corp		<- elt(output, "fluxpfish_corp")
	fluxmfish_corp		<- elt(output, "fluxmfish_corp")
	fluxdfish_corp		<- elt(output, "fluxdfish_corp")
	fluxbenths_corp		<- elt(output, "fluxbenths_corp")
	fluxbenthc_corp		<- elt(output, "fluxbenthc_corp")
	fluxbird_corp		<- elt(output, "fluxbird_corp")
	fluxseal_corp		<- elt(output, "fluxseal_corp")
	fluxceta_corp		<- elt(output, "fluxceta_corp")
	fluxwcdet_xdet		<- elt(output, "fluxwcdet_xdet")
	fluxphyt_benths		<- elt(output, "fluxphyt_benths")
	fluxwcdet_benths	<- elt(output, "fluxwcdet_benths")
	fluxomni_wcamm		<- elt(output, "fluxomni_wcamm")
	fluxcarn_wcamm		<- elt(output, "fluxcarn_wcamm")
	fluxpfishlar_wcamm	<- elt(output, "fluxpfishlar_wcamm")
	fluxdfishlar_wcamm	<- elt(output, "fluxdfishlar_wcamm")
	fluxpfish_wcamm		<- elt(output, "fluxpfish_wcamm")
	fluxmfish_wcamm		<- elt(output, "fluxmfish_wcamm")
	fluxdfish_wcamm		<- elt(output, "fluxdfish_wcamm")
	fluxbenthslar_wcamm	<- elt(output, "fluxbenthslar_wcamm")
	fluxbenthclar_wcamm	<- elt(output, "fluxbenthclar_wcamm")
	fluxbird_wcamm		<- elt(output, "fluxbird_wcamm")
	fluxseal_wcamm		<- elt(output, "fluxseal_wcamm")
	fluxceta_wcamm		<- elt(output, "fluxceta_wcamm")
	fluxbenths_wcamm	<- elt(output, "fluxbenths_wcamm")
	fluxbenthc_wcamm	<- elt(output, "fluxbenthc_wcamm")
	fluxwcdet_wcamm		<- elt(output, "fluxwcdet_wcamm")
	fluxxdet_sedamm		<- elt(output, "fluxxdet_sedamm")
	fluxxRdet_sedamm	<- elt(output, "fluxxRdet_sedamm")
	fluxwcamm_wcnit		<- elt(output, "fluxwcamm_wcnit")
	fluxsedamm_sednit	<- elt(output, "fluxsedamm_sednit")
	fluxsedamm_wcamm	<- elt(output, "fluxsedamm_wcamm")
	fluxsednit_wcnit	<- elt(output, "fluxsednit_wcnit")
	fluxwcdet_omni		<- elt(output, "fluxwcdet_omni")
	fluxphyt_omni		<- elt(output, "fluxphyt_omni")
	fluxbenthslar_omni	<- elt(output, "fluxbenthslar_omni")
	fluxbenthclar_omni	<- elt(output, "fluxbenthclar_omni")
	fluxomni_carn		<- elt(output, "fluxomni_carn")
	fluxpfishlar_carn	<- elt(output, "fluxpfishlar_carn")
	fluxdfishlar_carn	<- elt(output, "fluxdfishlar_carn")
	fluxbenthslar_carn	<- elt(output, "fluxbenthslar_carn")
	fluxbenthclar_carn	<- elt(output, "fluxbenthclar_carn")
	fluxomni_pfishlar	<- elt(output, "fluxomni_pfishlar")
	fluxbenthslar_pfishlar	<- elt(output, "fluxbenthslar_pfishlar")
	fluxbenthclar_pfishlar	<- elt(output, "fluxbenthclar_pfishlar")
	fluxomni_dfishlar	<- elt(output, "fluxomni_dfishlar")
	fluxbenthslar_dfishlar	<- elt(output, "fluxbenthslar_dfishlar")
	fluxbenthclar_dfishlar	<- elt(output, "fluxbenthclar_dfishlar")
	fluxomni_pfish		<- elt(output, "fluxomni_pfish")
	fluxcarn_pfish		<- elt(output, "fluxcarn_pfish")
	fluxpfishlar_pfish	<- elt(output, "fluxpfishlar_pfish")
	fluxdfishlar_pfish	<- elt(output, "fluxdfishlar_pfish")
	fluxbenthslar_pfish	<- elt(output, "fluxbenthslar_pfish")
	fluxbenthclar_pfish	<- elt(output, "fluxbenthclar_pfish")
	fluxomni_mfish		<- elt(output, "fluxomni_mfish")
	fluxcarn_mfish		<- elt(output, "fluxcarn_mfish")
	fluxpfishlar_mfish	<- elt(output, "fluxpfishlar_mfish")
	fluxdfishlar_mfish	<- elt(output, "fluxdfishlar_mfish")
	fluxbenthslar_mfish	<- elt(output, "fluxbenthslar_mfish")
	fluxbenthclar_mfish	<- elt(output, "fluxbenthclar_mfish")
	fluxcorp_dfish		<- elt(output, "fluxcorp_dfish")
	fluxdisc_dfish		<- elt(output, "fluxdisc_dfish")
	fluxcarn_dfish		<- elt(output, "fluxcarn_dfish")
	fluxpfishlar_dfish	<- elt(output, "fluxpfishlar_dfish")
	fluxdfishlar_dfish	<- elt(output, "fluxdfishlar_dfish")
	fluxpfish_dfish		<- elt(output, "fluxpfish_dfish")
	fluxmfish_dfish		<- elt(output, "fluxmfish_dfish")
	fluxdfish_dfish		<- elt(output, "fluxdfish_dfish")
	fluxbenths_dfish	<- elt(output, "fluxbenths_dfish")
	fluxbenthc_dfish	<- elt(output, "fluxbenthc_dfish")
	fluxwcdet_benthslar	<- elt(output, "fluxwcdet_benthslar")
	fluxphyt_benthslar	<- elt(output, "fluxphyt_benthslar")
	fluxwcdet_benthclar	<- elt(output, "fluxwcdet_benthclar")
	fluxphyt_benthclar	<- elt(output, "fluxphyt_benthclar")
	fluxxdet_benths		<- elt(output, "fluxxdet_benths")
	fluxxRdet_benths	<- elt(output, "fluxxRdet_benths")
	fluxkelp_benthc		<- elt(output, "fluxkelp_benthc")
	fluxkelpdebris_benthc	<- elt(output, "fluxkelpdebris_benthc")
	fluxcorp_benthc		<- elt(output, "fluxcorp_benthc")
	fluxbenths_benthc	<- elt(output, "fluxbenths_benthc")
	fluxcorp_bird		<- elt(output, "fluxcorp_bird")
	fluxdisc_bird		<- elt(output, "fluxdisc_bird")
	fluxcarn_bird		<- elt(output, "fluxcarn_bird")
	fluxpfish_bird		<- elt(output, "fluxpfish_bird")
	fluxmfish_bird		<- elt(output, "fluxmfish_bird")
	fluxdfish_bird		<- elt(output, "fluxdfish_bird")
	fluxbenths_bird		<- elt(output, "fluxbenths_bird")
	fluxbenthc_bird		<- elt(output, "fluxbenthc_bird")
	fluxcorp_seal		<- elt(output, "fluxcorp_seal")
	fluxdisc_seal		<- elt(output, "fluxdisc_seal")
	fluxcarn_seal		<- elt(output, "fluxcarn_seal")
	fluxpfish_seal		<- elt(output, "fluxpfish_seal")
	fluxmfish_seal		<- elt(output, "fluxmfish_seal")
	fluxdfish_seal		<- elt(output, "fluxdfish_seal")
	fluxbenths_seal		<- elt(output, "fluxbenths_seal")
	fluxbenthc_seal		<- elt(output, "fluxbenthc_seal")
	fluxbird_seal		<- elt(output, "fluxbird_seal")
	fluxdisc_ceta		<- elt(output, "fluxdisc_ceta")
	fluxomni_ceta		<- elt(output, "fluxomni_ceta")
	fluxcarn_ceta		<- elt(output, "fluxcarn_ceta")
	fluxpfish_ceta		<- elt(output, "fluxpfish_ceta")
	fluxmfish_ceta		<- elt(output, "fluxmfish_ceta")
	fluxdfish_ceta		<- elt(output, "fluxdfish_ceta")
	fluxbenths_ceta		<- elt(output, "fluxbenths_ceta")
	fluxbenthc_ceta		<- elt(output, "fluxbenthc_ceta")
	fluxbird_ceta		<- elt(output, "fluxbird_ceta")
	fluxseal_ceta		<- elt(output, "fluxseal_ceta")
	Pfish_spawn		<- elt(output, "Pfish_spawn")
	Pfish_recruit		<- elt(output, "Pfish_recruit")
	Dfish_spawn		<- elt(output, "Dfish_spawn")
	Dfish_recruit		<- elt(output, "Dfish_recruit")
	Bs_spawn		<- elt(output, "Bs_spawn")
	Bs_recruit		<- elt(output, "Bs_recruit")
	Bc_spawn		<- elt(output, "Bc_spawn")
	Bc_recruit		<- elt(output, "Bc_recruit")
	landkp_i		<- elt(output, "landkp_i")
	disckp_i		<- elt(output, "disckp_i")
	offalkp_i		<- elt(output, "offalkp_i")

	rivNITinflow		<- elt(output, "rivNITinflow")
	atmosNITinput		<- elt(output, "atmosNITinput_o") + elt(output, "atmosNITinput_i")


	# extract aggregates:
	s_detritus		<- elt(aggregates, "s_detritus")
	x_detritus		<- elt(aggregates, "x_detritus")
	discard			<- elt(aggregates, "discard")
	corpse			<- elt(aggregates, "corpse")
	s_ammonia		<- elt(aggregates, "s_ammonia")
	x_ammonia		<- elt(aggregates, "x_ammonia")
	s_nitrate		<- elt(aggregates, "s_nitrate")
	x_nitrate		<- elt(aggregates, "x_nitrate")
	s_phyt			<- elt(aggregates, "s_phyt")
	omni			<- elt(aggregates, "omni")
	carn			<- elt(aggregates, "carn")
	benthslar		<- elt(aggregates, "benthslar")
	benths			<- elt(aggregates, "benths")
	benthclar		<- elt(aggregates, "benthclar")
	benthc			<- elt(aggregates, "benthc")
	fishplar		<- elt(aggregates, "fishplar")
	fishp			<- elt(aggregates, "fishp")
	fishm			<- elt(aggregates, "fishm")
	fishdlar		<- elt(aggregates, "fishdlar")
	fishd			<- elt(aggregates, "fishd")
	bird			<- elt(aggregates, "bird")
	seal			<- elt(aggregates, "seal")
	ceta			<- elt(aggregates, "ceta")
	totalN			<- elt(aggregates, "totalN")
	fluxDINinflow		<- elt(aggregates, "fluxDINinflow")
	fluxDINoutflow		<- elt(aggregates, "fluxDINoutflow")
	fluxPARTinflow		<- elt(aggregates, "fluxPARTinflow")
	fluxPARToutflow		<- elt(aggregates, "fluxPARToutflow")
	atmosDINinput		<- elt(aggregates, "atmosDINinput")
	rivDINinflow		<- elt(aggregates, "rivDINinflow")
	fluxsedboundary		<- elt(aggregates, "fluxsedboundary")
	DIN_NET_flux_o_i	<- elt(aggregates, "DIN_NET_flux_o_i")
	PART_NET_flux_o_i	<- elt(aggregates, "PART_NET_flux_o_i")
	NET_activemigpelfish_o_i<- elt(aggregates, "NET_activemigpelfish_o_i")
	NET_activemigmigfish_o_i<- elt(aggregates, "NET_activemigmigfish_o_i")
	NET_activemigdemfish_o_i<- elt(aggregates, "NET_activemigdemfish_o_i")
	NET_activemigbird_o_i	<- elt(aggregates, "NET_activemigbird_o_i")
	NET_activemigseal_o_i	<- elt(aggregates, "NET_activemigseal_o_i")
	NET_activemigceta_o_i	<- elt(aggregates, "NET_activemigceta_o_i")
	NET_mfish_ext_o		<- elt(aggregates, "NET_mfish_ext_o")
	netpprod		<- elt(aggregates, "netpprod")
	NNCP			<- elt(aggregates, "NNCP")
	fluxwcnit_phyt		<- elt(aggregates, "fluxwcnit_phyt")
	fluxwcamm_phyt		<- elt(aggregates, "fluxwcamm_phyt")
	phytgrossprod		<- elt(aggregates, "phytgrossprod")
	omnigrossprod		<- elt(aggregates, "omnigrossprod")
	carngrossprod		<- elt(aggregates, "carngrossprod")
	pfishlargrossprod	<- elt(aggregates, "pfishlargrossprod")
	dfishlargrossprod	<- elt(aggregates, "dfishlargrossprod")
	pfishgrossprod		<- elt(aggregates, "pfishgrossprod")
	mfishgrossprod		<- elt(aggregates, "mfishgrossprod")
	dfishgrossprod		<- elt(aggregates, "dfishgrossprod")
	benthslargrossprod	<- elt(aggregates, "benthslargrossprod")
	benthclargrossprod	<- elt(aggregates, "benthclargrossprod")
	benthsgrossprod		<- elt(aggregates, "benthsgrossprod")
	benthcgrossprod		<- elt(aggregates, "benthcgrossprod")
	birdgrossprod		<- elt(aggregates, "birdgrossprod")
	sealgrossprod		<- elt(aggregates, "sealgrossprod")
	cetagrossprod		<- elt(aggregates, "cetagrossprod")
	omninetprod		<- elt(aggregates, "omninetprod")
	carnnetprod		<- elt(aggregates, "carnnetprod")
	pfishlarnetprod		<- elt(aggregates, "pfishlarnetprod")
	dfishlarnetprod		<- elt(aggregates, "dfishlarnetprod")
	pfishnetprod		<- elt(aggregates, "pfishnetprod")
	mfishnetprod		<- elt(aggregates, "mfishnetprod")
	dfishnetprod		<- elt(aggregates, "dfishnetprod")
	benthslarnetprod	<- elt(aggregates, "benthslarnetprod")
	benthclarnetprod	<- elt(aggregates, "benthclarnetprod")
	benthsnetprod		<- elt(aggregates, "benthsnetprod")
	benthcnetprod		<- elt(aggregates, "benthcnetprod")
	birdnetprod		<- elt(aggregates, "birdnetprod")
	sealnetprod		<- elt(aggregates, "sealnetprod")
	cetanetprod		<- elt(aggregates, "cetanetprod")
	wcdenitrif		<- elt(aggregates, "wcdenitrif")
	seddenitrif		<- elt(aggregates, "seddenitrif")
	landp			<- elt(aggregates, "landp")
	landm			<- elt(aggregates, "landm")
	landd			<- elt(aggregates, "landd")
	landd_quota		<- elt(aggregates, "landd_quota")
	landd_nonquota		<- elt(aggregates, "landd_nonquota")
	landsb			<- elt(aggregates, "landsb")
	landcb			<- elt(aggregates, "landcb")
	landcz			<- elt(aggregates, "landcz")
	landbd			<- elt(aggregates, "landbd")
	landsl			<- elt(aggregates, "landsl")
	landct			<- elt(aggregates, "landct")
	discpel			<- elt(aggregates, "discpel")
	discmig			<- elt(aggregates, "discmig")
	discdem			<- elt(aggregates, "discdem")
	discdem_quota		<- elt(aggregates, "discdem_quota")
	discdem_nonquota	<- elt(aggregates, "discdem_nonquota")
	discsb			<- elt(aggregates, "discsb")
	disccb			<- elt(aggregates, "disccb")
	disccz			<- elt(aggregates, "disccz")
	discbd			<- elt(aggregates, "discbd")
	discsl			<- elt(aggregates, "discsl")
	discct			<- elt(aggregates, "discct")
	offalpel		<- elt(aggregates, "offalpel")
	offalmig		<- elt(aggregates, "offalmig")
	offaldem		<- elt(aggregates, "offaldem")
	offaldem_quota		<- elt(aggregates, "offaldem_quota")
	offaldem_nonquota	<- elt(aggregates, "offaldem_nonquota")
	offalsb			<- elt(aggregates, "offalsb")
	offalcb			<- elt(aggregates, "offalcb")
	offalcz			<- elt(aggregates, "offalcz")
	offalbd			<- elt(aggregates, "offalbd")
	offalsl			<- elt(aggregates, "offalsl")
	offalct			<- elt(aggregates, "offalct")

#Derive a load of  stuff from the output and write to a file



#Extract annual average biomass in the final year for the whole model domain

aamass_s_detritus<-mean(s_detritus[((nyears-1)*360+1):ndays])
aamass_d_detritus<-mean(detritus_d[((nyears-1)*360+1):ndays])
aamass_x_detritus<-mean(x_detritus[((nyears-1)*360+1):ndays]) #Includes both labile and refractory detritus
aamass_xR_detritus<-mean(xR_detritus_s1[((nyears-1)*360+1):ndays]
                       + xR_detritus_s2[((nyears-1)*360+1):ndays]
                       + xR_detritus_s3[((nyears-1)*360+1):ndays]
                       + xR_detritus_d1[((nyears-1)*360+1):ndays]
                       + xR_detritus_d2[((nyears-1)*360+1):ndays]
                       + xR_detritus_d3[((nyears-1)*360+1):ndays])
aamass_discard<-mean(discard[((nyears-1)*360+1):ndays])
aamass_corpse<-mean(corpse[((nyears-1)*360+1):ndays])

aamass_kelpdebris<-mean(kelpdebris[((nyears-1)*360+1):ndays])

aamass_s_ammonia<-mean(s_ammonia[((nyears-1)*360+1):ndays])
aamass_d_ammonia<-mean(ammonia_d[((nyears-1)*360+1):ndays])
aamass_x_ammonia<-mean(x_ammonia[((nyears-1)*360+1):ndays])
aamass_s_nitrate<-mean(s_nitrate[((nyears-1)*360+1):ndays])
aamass_d_nitrate<-mean(nitrate_d[((nyears-1)*360+1):ndays])
aamass_x_nitrate<-mean(x_nitrate[((nyears-1)*360+1):ndays])

aamass_kelpN<-mean(kelpN[((nyears-1)*360+1):ndays])

aamass_s_phyt<-mean(s_phyt[((nyears-1)*360+1):ndays])
aamass_d_phyt<-mean(phyt_d[((nyears-1)*360+1):ndays])
aamass_omni<-mean(omni[((nyears-1)*360+1):ndays])
aamass_carn<-mean(carn[((nyears-1)*360+1):ndays])
aamass_benthslar<-mean(benthslar[((nyears-1)*360+1):ndays])
aamass_benths<-mean(benths[((nyears-1)*360+1):ndays])
aamass_benthclar<-mean(benthclar[((nyears-1)*360+1):ndays])
aamass_benthc<-mean(benthc[((nyears-1)*360+1):ndays])
aamass_fishplar<-mean(fishplar[((nyears-1)*360+1):ndays])
aamass_fishp<-mean(fishp[((nyears-1)*360+1):ndays])
aamass_fishm<-mean(fishm[((nyears-1)*360+1):ndays])
aamass_fishdlar<-mean(fishdlar[((nyears-1)*360+1):ndays])
aamass_fishd<-mean(fishd[((nyears-1)*360+1):ndays])
aamass_bird<-mean(bird[((nyears-1)*360+1):ndays])

aamass_seal<-mean(seal[((nyears-1)*360+1):ndays])
aamass_ceta<-mean(ceta[((nyears-1)*360+1):ndays])

aamass_totalN<-mean(totalN[((nyears-1)*360+1):ndays])

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


#Text descriptions of units set here for each row of output data
mass_results[,2]<-"mMN_in_the_whole_model_domain_(1m2)"
mass_results[c(32,36:43,50:55),2]<-"dimensionless"
mass_results[c(33:35,44:49),2]<-"m"

mass_results[,3]<-mass_results_descriptions
#The vector of text descriptions for each row of output is set in internal.R

colnames(mass_results)<-c("Model_annual_mean","Units","Description")

mass_results


#Print the data to a csv file
#-----------------------------------------------------------------
filename = csvname(resultsdir, "WHOLEDOMAIN_model_anav_biomass", identifier)
writecsv(mass_results, filename, row.names=FALSE)

#-------------------------------------------------------------------------------------------------------

#copy the mass_results dataframe as a template for the annual maximum data
maxmass_results<-mass_results
maxmass_results[1:31,1]<-0


aamaxmass_s_detritus<-max(s_detritus[((nyears-1)*360+1):ndays])
aamaxmass_d_detritus<-max(detritus_d[((nyears-1)*360+1):ndays])
aamaxmass_x_detritus<-max(x_detritus[((nyears-1)*360+1):ndays]) #Includes both labile and refractory detritus
aamaxmass_xR_detritus<-max(xR_detritus_s1[((nyears-1)*360+1):ndays]
                       + xR_detritus_s2[((nyears-1)*360+1):ndays]
                       + xR_detritus_s3[((nyears-1)*360+1):ndays]
                       + xR_detritus_d1[((nyears-1)*360+1):ndays]
                       + xR_detritus_d2[((nyears-1)*360+1):ndays]
                       + xR_detritus_d3[((nyears-1)*360+1):ndays])
aamaxmass_discard<-max(discard[((nyears-1)*360+1):ndays])
aamaxmass_corpse<-max(corpse[((nyears-1)*360+1):ndays])

aamaxmass_kelpdebris<-max(kelpdebris[((nyears-1)*360+1):ndays])

aamaxmass_s_ammonia<-max(s_ammonia[((nyears-1)*360+1):ndays])
aamaxmass_d_ammonia<-max(ammonia_d[((nyears-1)*360+1):ndays])
aamaxmass_x_ammonia<-max(x_ammonia[((nyears-1)*360+1):ndays])
aamaxmass_s_nitrate<-max(s_nitrate[((nyears-1)*360+1):ndays])
aamaxmass_d_nitrate<-max(nitrate_d[((nyears-1)*360+1):ndays])
aamaxmass_x_nitrate<-max(x_nitrate[((nyears-1)*360+1):ndays])

aamaxmass_kelpN<-max(kelpN[((nyears-1)*360+1):ndays])

aamaxmass_s_phyt<-max(s_phyt[((nyears-1)*360+1):ndays])
aamaxmass_d_phyt<-max(phyt_d[((nyears-1)*360+1):ndays])
aamaxmass_omni<-max(omni[((nyears-1)*360+1):ndays])
aamaxmass_carn<-max(carn[((nyears-1)*360+1):ndays])
aamaxmass_benthslar<-max(benthslar[((nyears-1)*360+1):ndays])
aamaxmass_benths<-max(benths[((nyears-1)*360+1):ndays])
aamaxmass_benthclar<-max(benthclar[((nyears-1)*360+1):ndays])
aamaxmass_benthc<-max(benthc[((nyears-1)*360+1):ndays])
aamaxmass_fishplar<-max(fishplar[((nyears-1)*360+1):ndays])
aamaxmass_fishp<-max(fishp[((nyears-1)*360+1):ndays])
aamaxmass_fishm<-max(fishm[((nyears-1)*360+1):ndays])
aamaxmass_fishdlar<-max(fishdlar[((nyears-1)*360+1):ndays])
aamaxmass_fishd<-max(fishd[((nyears-1)*360+1):ndays])
aamaxmass_bird<-max(bird[((nyears-1)*360+1):ndays])

aamaxmass_seal<-max(seal[((nyears-1)*360+1):ndays])
aamaxmass_ceta<-max(ceta[((nyears-1)*360+1):ndays])

aamaxmass_totalN<-max(totalN[((nyears-1)*360+1):ndays])


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
filename = csvname(resultsdir, "WHOLEDOMAIN_model_maximum_biomass", identifier)
writecsv(maxmass_results, filename, row.names=FALSE)

#-------------------------------------------------------------------------------------------------------


#copy the mass_results dataframe as a template for the annual minimum data
minmass_results<-mass_results
minmass_results[1:31,1]<-0


aaminmass_s_detritus<-min(s_detritus[((nyears-1)*360+1):ndays])
aaminmass_d_detritus<-min(detritus_d[((nyears-1)*360+1):ndays])
aaminmass_x_detritus<-min(x_detritus[((nyears-1)*360+1):ndays]) #Includes both labile and refractory detritus
aaminmass_xR_detritus<-min(xR_detritus_s1[((nyears-1)*360+1):ndays]
                       + xR_detritus_s2[((nyears-1)*360+1):ndays]
                       + xR_detritus_s3[((nyears-1)*360+1):ndays]
                       + xR_detritus_d1[((nyears-1)*360+1):ndays]
                       + xR_detritus_d2[((nyears-1)*360+1):ndays]
                       + xR_detritus_d3[((nyears-1)*360+1):ndays])
aaminmass_discard<-min(discard[((nyears-1)*360+1):ndays])
aaminmass_corpse<-min(corpse[((nyears-1)*360+1):ndays])

aaminmass_kelpdebris<-min(kelpdebris[((nyears-1)*360+1):ndays])

aaminmass_s_ammonia<-min(s_ammonia[((nyears-1)*360+1):ndays])
aaminmass_d_ammonia<-min(ammonia_d[((nyears-1)*360+1):ndays])
aaminmass_x_ammonia<-min(x_ammonia[((nyears-1)*360+1):ndays])
aaminmass_s_nitrate<-min(s_nitrate[((nyears-1)*360+1):ndays])
aaminmass_d_nitrate<-min(nitrate_d[((nyears-1)*360+1):ndays])
aaminmass_x_nitrate<-min(x_nitrate[((nyears-1)*360+1):ndays])

aaminmass_kelpN<-min(kelpN[((nyears-1)*360+1):ndays])

aaminmass_s_phyt<-min(s_phyt[((nyears-1)*360+1):ndays])
aaminmass_d_phyt<-min(phyt_d[((nyears-1)*360+1):ndays])
aaminmass_omni<-min(omni[((nyears-1)*360+1):ndays])
aaminmass_carn<-min(carn[((nyears-1)*360+1):ndays])
aaminmass_benthslar<-min(benthslar[((nyears-1)*360+1):ndays])
aaminmass_benths<-min(benths[((nyears-1)*360+1):ndays])
aaminmass_benthclar<-min(benthclar[((nyears-1)*360+1):ndays])
aaminmass_benthc<-min(benthc[((nyears-1)*360+1):ndays])
aaminmass_fishplar<-min(fishplar[((nyears-1)*360+1):ndays])
aaminmass_fishp<-min(fishp[((nyears-1)*360+1):ndays])
aaminmass_fishm<-min(fishm[((nyears-1)*360+1):ndays])
aaminmass_fishdlar<-min(fishdlar[((nyears-1)*360+1):ndays])
aaminmass_fishd<-min(fishd[((nyears-1)*360+1):ndays])
aaminmass_bird<-min(bird[((nyears-1)*360+1):ndays])

aaminmass_seal<-min(seal[((nyears-1)*360+1):ndays])
aaminmass_ceta<-min(ceta[((nyears-1)*360+1):ndays])

aaminmass_totalN<-min(totalN[((nyears-1)*360+1):ndays])


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
filename = csvname(resultsdir, "WHOLEDOMAIN_model_minimum_biomass", identifier)
writecsv(minmass_results, filename, row.names=FALSE)

#-------------------------------------------------------------------------------------------------------





#Extract all of the derived rate variables for the final year




DINinflow<-(fluxDINinflow[ndays]-fluxDINinflow[((nyears-1)*360+1)])                 # January - December
DINoutflow<-(fluxDINoutflow[ndays]-fluxDINoutflow[((nyears-1)*360+1)])              # January - December 
PARTinflow<-(fluxPARTinflow[ndays]-fluxPARTinflow[((nyears-1)*360+1)])              # January - December
PARToutflow<-(fluxPARToutflow[ndays]-fluxPARToutflow[((nyears-1)*360+1)])           # January - December
atmosphereDINinput<-(atmosDINinput[ndays]-atmosDINinput[((nyears-1)*360+1)])        # January - December
riverDINinflow<-rivDINinflow[ndays]-rivDINinflow[((nyears-1)*360+1)]                # January - December
riverPARTinflow<-rivPARTinflow[ndays]-rivPARTinflow[((nyears-1)*360+1)]             # January - December
sumDINinflow<-(fluxDINinflow[ndays-90]-fluxDINinflow[((nyears-1)*360+1+89)])        # April - September inclusive
sumDINoutflow<-(fluxDINoutflow[ndays-90]-fluxDINoutflow[((nyears-1)*360+1+89)])     # April - September inclusive
sumPARTinflow<-(fluxPARTinflow[ndays-90]-fluxPARTinflow[((nyears-1)*360+1+89)])     # April - September inclusive
sumPARToutflow<-(fluxPARToutflow[ndays-90]-fluxPARToutflow[((nyears-1)*360+1+89)])  # April - September inclusive
sumriverDINinflow<-rivDINinflow[ndays-90]-rivDINinflow[((nyears-1)*360+1+89)]       # April - September inclusive
sumatmosDINinput<-(atmosDINinput[ndays-90]-atmosDINinput[((nyears-1)*360+1+89)])    # April - September inclusive
surfvertnitflux<-vertnitflux[ndays]-vertnitflux[((nyears-1)*360+1)]
surfhoriznitflux<-horiznitflux[ndays]-horiznitflux[((nyears-1)*360+1)]

sumriverNITinflow<-rivNITinflow[ndays-90]-rivNITinflow[((nyears-1)*360+1+89)]       # April - September inclusive
sumatmosNITinput<-(atmosNITinput[ndays-90]-atmosNITinput[((nyears-1)*360+1+89)])    # April - September inclusive


Flux_sedboundary <- (fluxsedboundary[ndays]-fluxsedboundary[((nyears-1)*360+1)]) 

kelp_beachcast<-((fluxkelpdebris_beachexport[ndays]-fluxkelpdebris_beachexport[((nyears-1)*360+1)]))   


DIN_NET_flux_o_i   <-  DIN_NET_flux_o_i[ndays]-DIN_NET_flux_o_i[((nyears-1)*360+1)]
PART_NET_flux_o_i  <-  PART_NET_flux_o_i[ndays]-PART_NET_flux_o_i[((nyears-1)*360+1)]
NET_activemigpelfish_o_i  <-  NET_activemigpelfish_o_i[ndays]-NET_activemigpelfish_o_i[((nyears-1)*360+1)]

NET_activemigmigfish_o_i  <-  NET_activemigmigfish_o_i[ndays]-NET_activemigmigfish_o_i[((nyears-1)*360+1)]
NET_activemigdemfish_o_i  <-  NET_activemigdemfish_o_i[ndays]-NET_activemigdemfish_o_i[((nyears-1)*360+1)]
NET_activemigbird_o_i  <-  NET_activemigbird_o_i[ndays]-NET_activemigbird_o_i[((nyears-1)*360+1)]
NET_activemigseal_o_i  <-  NET_activemigseal_o_i[ndays]-NET_activemigseal_o_i[((nyears-1)*360+1)]
NET_activemigceta_o_i  <-  NET_activemigceta_o_i[ndays]-NET_activemigceta_o_i[((nyears-1)*360+1)]

NET_mfish_ext_o    <-  NET_mfish_ext_o[ndays]-NET_mfish_ext_o[((nyears-1)*360+1)]

Mfish_annual_imig <-    mfish_imigration[ndays]-mfish_imigration[((nyears-1)*360+1)]
Mfish_annual_emig  <-   mfish_emigration[ndays]-mfish_emigration[((nyears-1)*360+1)]

NETPrimaryP<-netpprod[ndays]-netpprod[((nyears-1)*360+1)]
MMP<-(max(s_nitrate[((nyears-1)*360+1):ndays]+nitrate_d[((nyears-1)*360+1):ndays])) - (min(s_nitrate[((nyears-1)*360+1):ndays]+nitrate_d[((nyears-1)*360+1):ndays]))
NNCP<-NNCP[ndays]-NNCP[((nyears-1)*360+1)]
PhytNitUp<-fluxwcnit_phyt[ndays]-fluxwcnit_phyt[((nyears-1)*360+1)]
PhytAmmUp<-fluxwcamm_phyt[ndays]-fluxwcamm_phyt[((nyears-1)*360+1)]
PNP<-MMP+sumriverNITinflow+sumatmosNITinput
fratio<-PNP/NETPrimaryP
Tfratio<-PhytNitUp/(PhytNitUp+PhytAmmUp)

KelpNitUp<-fluxwcnit_kelp[ndays]-fluxwcnit_kelp[((nyears-1)*360+1)]
KelpAmmUp<-fluxwcamm_kelp[ndays]-fluxwcamm_kelp[((nyears-1)*360+1)]


KelpNprod       <-   kelpNprod_i[ndays]      -     kelpNprod_i[((nyears-1)*360+1)]

Phytgrossprod        <-   phytgrossprod[ndays]      -     phytgrossprod[((nyears-1)*360+1)]

omnigrossprod        <-   omnigrossprod[ndays]      -     omnigrossprod[((nyears-1)*360+1)]
Carngrossprod        <-   carngrossprod[ndays]      -     carngrossprod[((nyears-1)*360+1)]
Fishplargrossprod    <-   pfishlargrossprod[ndays]  -     pfishlargrossprod[((nyears-1)*360+1)]
Fishdlargrossprod    <-   dfishlargrossprod[ndays]  -     dfishlargrossprod[((nyears-1)*360+1)]
Fishpgrossprod       <-   pfishgrossprod[ndays]     -     pfishgrossprod[((nyears-1)*360+1)]
Fishmgrossprod       <-   mfishgrossprod[ndays]     -     mfishgrossprod[((nyears-1)*360+1)]
Fishdgrossprod       <-   dfishgrossprod[ndays]     -     dfishgrossprod[((nyears-1)*360+1)]
Benthslargrossprod   <-   benthslargrossprod[ndays] -     benthslargrossprod[((nyears-1)*360+1)]
Benthclargrossprod   <-   benthclargrossprod[ndays] -     benthclargrossprod[((nyears-1)*360+1)]
Benthsgrossprod      <-   benthsgrossprod[ndays]    -     benthsgrossprod[((nyears-1)*360+1)]
Benthcgrossprod      <-   benthcgrossprod[ndays]    -     benthcgrossprod[((nyears-1)*360+1)]
Birdgrossprod        <-   birdgrossprod[ndays]      -     birdgrossprod[((nyears-1)*360+1)]
Sealgrossprod        <-   sealgrossprod[ndays]      -     sealgrossprod[((nyears-1)*360+1)]
Cetagrossprod        <-   cetagrossprod[ndays]      -     cetagrossprod[((nyears-1)*360+1)]

omninetprod        <-   omninetprod[ndays]      -     omninetprod[((nyears-1)*360+1)]
Carnnetprod        <-   carnnetprod[ndays]      -     carnnetprod[((nyears-1)*360+1)]
Fishplarnetprod    <-   pfishlarnetprod[ndays]  -     pfishlarnetprod[((nyears-1)*360+1)]
Fishdlarnetprod    <-   dfishlarnetprod[ndays]  -     dfishlarnetprod[((nyears-1)*360+1)]
Fishpnetprod       <-   pfishnetprod[ndays]     -     pfishnetprod[((nyears-1)*360+1)]
Fishmnetprod       <-   mfishnetprod[ndays]     -     mfishnetprod[((nyears-1)*360+1)]
Fishdnetprod       <-   dfishnetprod[ndays]     -     dfishnetprod[((nyears-1)*360+1)]
Benthslarnetprod   <-   benthslarnetprod[ndays] -     benthslarnetprod[((nyears-1)*360+1)]
Benthclarnetprod   <-   benthclarnetprod[ndays] -     benthclarnetprod[((nyears-1)*360+1)]
Benthsnetprod      <-   benthsnetprod[ndays]    -     benthsnetprod[((nyears-1)*360+1)]
Benthcnetprod      <-   benthcnetprod[ndays]    -     benthcnetprod[((nyears-1)*360+1)]
Birdnetprod        <-   birdnetprod[ndays]      -     birdnetprod[((nyears-1)*360+1)]
Sealnetprod        <-   sealnetprod[ndays]      -     sealnetprod[((nyears-1)*360+1)]
Cetanetprod        <-   cetanetprod[ndays]      -     cetanetprod[((nyears-1)*360+1)]

WCdetritusprod      <-   fluxcorp_wcdet[ndays]      -     fluxcorp_wcdet[((nyears-1)*360+1)] +
                        +fluxkelpdebris_wcdet[ndays]      -     fluxkelpdebris_wcdet[((nyears-1)*360+1)] +
                        +fluxphyt_wcdet[ndays]      -     fluxphyt_wcdet[((nyears-1)*360+1)] +
                        +fluxomni_wcdet[ndays]      -     fluxomni_wcdet[((nyears-1)*360+1)] +
                        +fluxcarn_wcdet[ndays]      -     fluxcarn_wcdet[((nyears-1)*360+1)] +
                        +fluxpfishlar_wcdet[ndays]      -     fluxpfishlar_wcdet[((nyears-1)*360+1)] +
                        +fluxdfishlar_wcdet[ndays]      -     fluxdfishlar_wcdet[((nyears-1)*360+1)] +
                        +fluxpfish_wcdet[ndays]      -     fluxpfish_wcdet[((nyears-1)*360+1)] +
                        +fluxmfish_wcdet[ndays]      -     fluxmfish_wcdet[((nyears-1)*360+1)] +
                        +fluxdfish_wcdet[ndays]      -     fluxdfish_wcdet[((nyears-1)*360+1)] +
                        +fluxbenthslar_wcdet[ndays]      -     fluxbenthslar_wcdet[((nyears-1)*360+1)] +
                        +fluxbenthclar_wcdet[ndays]      -     fluxbenthclar_wcdet[((nyears-1)*360+1)] +
                        +fluxbenths_wcdet[ndays]      -     fluxbenths_wcdet[((nyears-1)*360+1)] +
                        +fluxbenthc_wcdet[ndays]      -     fluxbenthc_wcdet[((nyears-1)*360+1)] +
                        +fluxbird_wcdet[ndays]      -     fluxbird_wcdet[((nyears-1)*360+1)] +
                        +fluxseal_wcdet[ndays]      -     fluxseal_wcdet[((nyears-1)*360+1)] +
                        +fluxceta_wcdet[ndays]      -     fluxceta_wcdet[((nyears-1)*360+1)]


SEDdetritusprod     <-   fluxcorp_xdet[ndays]      -     fluxcorp_xdet[((nyears-1)*360+1)] +
                        +fluxbenths_xdet[ndays]      -     fluxbenths_xdet[((nyears-1)*360+1)] +
                        +fluxbenthc_xdet[ndays]      -     fluxbenthc_xdet[((nyears-1)*360+1)]

Kelpdebrisprod     <-   fluxkelp_kelpdebris[ndays]      -     fluxkelp_kelpdebris[((nyears-1)*360+1)]


Corpseprod          <-   fluxdisc_corp[ndays]      -     fluxdisc_corp[((nyears-1)*360+1)] +
                        +fluxpfish_corp[ndays]      -     fluxpfish_corp[((nyears-1)*360+1)] +
                        +fluxmfish_corp[ndays]      -     fluxmfish_corp[((nyears-1)*360+1)] +
                        +fluxdfish_corp[ndays]      -     fluxdfish_corp[((nyears-1)*360+1)] +
                        +fluxbenths_corp[ndays]      -     fluxbenths_corp[((nyears-1)*360+1)] +
                        +fluxbenthc_corp[ndays]      -     fluxbenthc_corp[((nyears-1)*360+1)] +
                        +fluxbird_corp[ndays]      -     fluxbird_corp[((nyears-1)*360+1)] +
                        +fluxseal_corp[ndays]      -     fluxseal_corp[((nyears-1)*360+1)] +
                        +fluxceta_corp[ndays]      -     fluxceta_corp[((nyears-1)*360+1)]


Fluxpartwc_sed      <-   fluxwcdet_xdet[ndays]      -     fluxwcdet_xdet[((nyears-1)*360+1)] +
                        +fluxdisc_corp[ndays]      -     fluxdisc_corp[((nyears-1)*360+1)] +
                        +fluxpfish_corp[ndays]      -     fluxpfish_corp[((nyears-1)*360+1)] +
                        +fluxmfish_corp[ndays]      -     fluxmfish_corp[((nyears-1)*360+1)] +
                        +fluxdfish_corp[ndays]      -     fluxdfish_corp[((nyears-1)*360+1)] +
                        +fluxbird_corp[ndays]      -     fluxbird_corp[((nyears-1)*360+1)] +
                        +fluxseal_corp[ndays]      -     fluxseal_corp[((nyears-1)*360+1)] +
                        +fluxceta_corp[ndays]      -     fluxceta_corp[((nyears-1)*360+1)] +
                        +fluxphyt_benths[ndays]      -     fluxphyt_benths[((nyears-1)*360+1)]+
                        +fluxwcdet_benths[ndays]      -     fluxwcdet_benths[((nyears-1)*360+1)]
#Includes the water column feeding flux of benthos

Fluxdisc_corp       <-   fluxdisc_corp[ndays]      -     fluxdisc_corp[((nyears-1)*360+1)]

Pelagammprod        <-   fluxomni_wcamm[ndays]      -     fluxomni_wcamm[((nyears-1)*360+1)] +
                        +fluxcarn_wcamm[ndays]      -     fluxcarn_wcamm[((nyears-1)*360+1)] +
                        +fluxpfishlar_wcamm[ndays]      -     fluxpfishlar_wcamm[((nyears-1)*360+1)] +
                        +fluxdfishlar_wcamm[ndays]      -     fluxdfishlar_wcamm[((nyears-1)*360+1)] +
                        +fluxpfish_wcamm[ndays]      -     fluxpfish_wcamm[((nyears-1)*360+1)] +
                        +fluxmfish_wcamm[ndays]      -     fluxmfish_wcamm[((nyears-1)*360+1)] +
                        +fluxdfish_wcamm[ndays]      -     fluxdfish_wcamm[((nyears-1)*360+1)] +
                        +fluxbenthslar_wcamm[ndays]      -     fluxbenthslar_wcamm[((nyears-1)*360+1)] +
                        +fluxbenthclar_wcamm[ndays]      -     fluxbenthclar_wcamm[((nyears-1)*360+1)] +
                        +fluxbird_wcamm[ndays]      -     fluxbird_wcamm[((nyears-1)*360+1)] + 
                        +fluxseal_wcamm[ndays]      -     fluxseal_wcamm[((nyears-1)*360+1)] +
                        +fluxceta_wcamm[ndays]      -     fluxceta_wcamm[((nyears-1)*360+1)] 
#Excludes mineralisation of detritus

Benthammprod        <-   fluxbenths_wcamm[ndays]      -     fluxbenths_wcamm[((nyears-1)*360+1)]+
                        +fluxbenthc_wcamm[ndays]      -     fluxbenthc_wcamm[((nyears-1)*360+1)]
#Excludes mineralisation of detritus


WCmineralisation    <-   fluxwcdet_wcamm[ndays]      -     fluxwcdet_wcamm[((nyears-1)*360+1)]

SEDmineralisation   <-   fluxxdet_sedamm[ndays]      -     fluxxdet_sedamm[((nyears-1)*360+1)]+
                        +fluxxRdet_sedamm[ndays]      -     fluxxRdet_sedamm[((nyears-1)*360+1)]


WCnitrification     <-   fluxwcamm_wcnit[ndays]      -     fluxwcamm_wcnit[((nyears-1)*360+1)]

SEDnitrification    <-   fluxsedamm_sednit[ndays]      -     fluxsedamm_sednit[((nyears-1)*360+1)]

WCdenitrification   <-   wcdenitrif[ndays]      -     wcdenitrif[((nyears-1)*360+1)]

SEDdenitrification  <-   seddenitrif[ndays]      -     seddenitrif[((nyears-1)*360+1)]

SEDWCammflux        <-   fluxsedamm_wcamm[ndays]      -     fluxsedamm_wcamm[((nyears-1)*360+1)]+
                        +fluxbenths_wcamm[ndays]      -     fluxbenths_wcamm[((nyears-1)*360+1)]+
                        +fluxbenthc_wcamm[ndays]      -     fluxbenthc_wcamm[((nyears-1)*360+1)]
#Includes excretion by benthos

SEDWCnitflux        <-   fluxsednit_wcnit[ndays]      -     fluxsednit_wcnit[((nyears-1)*360+1)]






Fluxdet_omni           <-   fluxwcdet_omni[ndays]      -     fluxwcdet_omni[((nyears-1)*360+1)]
Fluxphyt_omni          <-   fluxphyt_omni[ndays]      -     fluxphyt_omni[((nyears-1)*360+1)]
Fluxbenthslar_omni          <-   fluxbenthslar_omni[ndays]      -     fluxbenthslar_omni[((nyears-1)*360+1)]
Fluxbenthclar_omni          <-   fluxbenthclar_omni[ndays]      -     fluxbenthclar_omni[((nyears-1)*360+1)]

Fluxomni_carn           <-   fluxomni_carn[ndays]      -     fluxomni_carn[((nyears-1)*360+1)]
Fluxpfishlar_carn       <-   fluxpfishlar_carn[ndays]      -     fluxpfishlar_carn[((nyears-1)*360+1)]
Fluxdfishlar_carn       <-   fluxdfishlar_carn[ndays]      -     fluxdfishlar_carn[((nyears-1)*360+1)]   
Fluxbenthslar_carn       <-   fluxbenthslar_carn[ndays]      -     fluxbenthslar_carn[((nyears-1)*360+1)]    
Fluxbenthclar_carn       <-   fluxbenthclar_carn[ndays]      -     fluxbenthclar_carn[((nyears-1)*360+1)]    

Fluxomni_pfishlar           <-   fluxomni_pfishlar[ndays]      -     fluxomni_pfishlar[((nyears-1)*360+1)]   
Fluxbenthslar_pfishlar        <-   fluxbenthslar_pfishlar[ndays]      -     fluxbenthslar_pfishlar[((nyears-1)*360+1)]   
Fluxbenthclar_pfishlar        <-   fluxbenthclar_pfishlar[ndays]      -     fluxbenthclar_pfishlar[((nyears-1)*360+1)]   

Fluxomni_dfishlar           <-   fluxomni_dfishlar[ndays]      -     fluxomni_dfishlar[((nyears-1)*360+1)]   
Fluxbenthslar_dfishlar        <-   fluxbenthslar_dfishlar[ndays]      -     fluxbenthslar_dfishlar[((nyears-1)*360+1)]   
Fluxbenthclar_dfishlar        <-   fluxbenthclar_dfishlar[ndays]      -     fluxbenthclar_dfishlar[((nyears-1)*360+1)]   

Fluxomni_pfish            <-   fluxomni_pfish[ndays]      -     fluxomni_pfish[((nyears-1)*360+1)]     
Fluxcarn_pfish            <-   fluxcarn_pfish[ndays]      -     fluxcarn_pfish[((nyears-1)*360+1)]     
Fluxpfishlar_pfish        <-   fluxpfishlar_pfish[ndays]      -     fluxpfishlar_pfish[((nyears-1)*360+1)]     
Fluxdfishlar_pfish        <-   fluxdfishlar_pfish[ndays]      -     fluxdfishlar_pfish[((nyears-1)*360+1)]     
Fluxbenthslar_pfish         <-   fluxbenthslar_pfish[ndays]      -     fluxbenthslar_pfish[((nyears-1)*360+1)]     
Fluxbenthclar_pfish         <-   fluxbenthclar_pfish[ndays]      -     fluxbenthclar_pfish[((nyears-1)*360+1)]     

Fluxomni_mfish            <-   fluxomni_mfish[ndays]      -     fluxomni_mfish[((nyears-1)*360+1)]     
Fluxcarn_mfish            <-   fluxcarn_mfish[ndays]      -     fluxcarn_mfish[((nyears-1)*360+1)]     
Fluxpfishlar_mfish        <-   fluxpfishlar_mfish[ndays]      -     fluxpfishlar_mfish[((nyears-1)*360+1)]     
Fluxdfishlar_mfish        <-   fluxdfishlar_mfish[ndays]      -     fluxdfishlar_mfish[((nyears-1)*360+1)]     
Fluxbenthslar_mfish       <-   fluxbenthslar_mfish[ndays]      -     fluxbenthslar_mfish[((nyears-1)*360+1)]     
Fluxbenthclar_mfish       <-   fluxbenthclar_mfish[ndays]      -     fluxbenthclar_mfish[((nyears-1)*360+1)]     

Fluxcorp_dfish            <-   fluxcorp_dfish[ndays]      -     fluxcorp_dfish[((nyears-1)*360+1)]     
Fluxdisc_dfish            <-   fluxdisc_dfish[ndays]      -     fluxdisc_dfish[((nyears-1)*360+1)]     
Fluxcarn_dfish            <-   fluxcarn_dfish[ndays]      -     fluxcarn_dfish[((nyears-1)*360+1)]     
Fluxpfishlar_dfish        <-   fluxpfishlar_dfish[ndays]      -     fluxpfishlar_dfish[((nyears-1)*360+1)]     
Fluxdfishlar_dfish        <-   fluxdfishlar_dfish[ndays]      -     fluxdfishlar_dfish[((nyears-1)*360+1)]     
Fluxpfish_dfish           <-   fluxpfish_dfish[ndays]      -     fluxpfish_dfish[((nyears-1)*360+1)]     
Fluxmfish_dfish           <-   fluxmfish_dfish[ndays]      -     fluxmfish_dfish[((nyears-1)*360+1)]     
Fluxdfish_dfish           <-   fluxdfish_dfish[ndays]      -     fluxdfish_dfish[((nyears-1)*360+1)]     
Fluxbenths_dfish          <-   fluxbenths_dfish[ndays]      -     fluxbenths_dfish[((nyears-1)*360+1)]     
Fluxbenthc_dfish          <-   fluxbenthc_dfish[ndays]      -     fluxbenthc_dfish[((nyears-1)*360+1)]     

Fluxdet_benthslar            <-   fluxwcdet_benthslar[ndays]      -     fluxwcdet_benthslar[((nyears-1)*360+1)]  
Fluxphyt_benthslar           <-   fluxphyt_benthslar[ndays]      -     fluxphyt_benthslar[((nyears-1)*360+1)]    

Fluxdet_benthclar            <-   fluxwcdet_benthclar[ndays]      -     fluxwcdet_benthclar[((nyears-1)*360+1)]  
Fluxphyt_benthclar           <-   fluxphyt_benthclar[ndays]      -     fluxphyt_benthclar[((nyears-1)*360+1)]  

Fluxdet_benths             <-   fluxwcdet_benths[ndays]      -     fluxwcdet_benths[((nyears-1)*360+1)]    
Fluxseddet_benths          <-   fluxxdet_benths[ndays]      -     fluxxdet_benths[((nyears-1)*360+1)] +   
                               +fluxxRdet_benths[ndays]      -     fluxxRdet_benths[((nyears-1)*360+1)]    

Fluxphyt_benths            <-   fluxphyt_benths[ndays]      -     fluxphyt_benths[((nyears-1)*360+1)]    

Fluxkelp_benthc            <-   fluxkelp_benthc[ndays]      -     fluxkelp_benthc[((nyears-1)*360+1)]    
Fluxkelpdebris_benthc            <-   fluxkelpdebris_benthc[ndays]      -     fluxkelpdebris_benthc[((nyears-1)*360+1)]    
Fluxcorp_benthc            <-   fluxcorp_benthc[ndays]      -     fluxcorp_benthc[((nyears-1)*360+1)]    
Fluxbenths_benthc          <-   fluxbenths_benthc[ndays]      -     fluxbenths_benthc[((nyears-1)*360+1)]    

Fluxcorp_bird            <-   fluxcorp_bird[ndays]      -     fluxcorp_bird[((nyears-1)*360+1)]      
Fluxdisc_bird            <-   fluxdisc_bird[ndays]      -     fluxdisc_bird[((nyears-1)*360+1)]      
#Fluxomni_bird            <-   fluxomni_bird[ndays]      -     fluxomni_bird[((nyears-1)*360+1)]      
Fluxcarn_bird            <-   fluxcarn_bird[ndays]      -     fluxcarn_bird[((nyears-1)*360+1)]      
Fluxpfish_bird           <-   fluxpfish_bird[ndays]      -     fluxpfish_bird[((nyears-1)*360+1)]      
Fluxmfish_bird           <-   fluxmfish_bird[ndays]      -     fluxmfish_bird[((nyears-1)*360+1)]      
Fluxdfish_bird           <-   fluxdfish_bird[ndays]      -     fluxdfish_bird[((nyears-1)*360+1)]      
Fluxbenths_bird          <-   fluxbenths_bird[ndays]      -     fluxbenths_bird[((nyears-1)*360+1)]      
Fluxbenthc_bird          <-   fluxbenthc_bird[ndays]      -     fluxbenthc_bird[((nyears-1)*360+1)]      
     
Fluxcorp_seal            <-   fluxcorp_seal[ndays]      -     fluxcorp_seal[((nyears-1)*360+1)]      
Fluxdisc_seal            <-   fluxdisc_seal[ndays]      -     fluxdisc_seal[((nyears-1)*360+1)]      
Fluxcarn_seal            <-   fluxcarn_seal[ndays]      -     fluxcarn_seal[((nyears-1)*360+1)]      
Fluxpfish_seal           <-   fluxpfish_seal[ndays]      -     fluxpfish_seal[((nyears-1)*360+1)]      
Fluxmfish_seal           <-   fluxmfish_seal[ndays]      -     fluxmfish_seal[((nyears-1)*360+1)]      
Fluxdfish_seal           <-   fluxdfish_seal[ndays]      -     fluxdfish_seal[((nyears-1)*360+1)]      
Fluxbenths_seal          <-   fluxbenths_seal[ndays]      -     fluxbenths_seal[((nyears-1)*360+1)]      
Fluxbenthc_seal          <-   fluxbenthc_seal[ndays]      -     fluxbenthc_seal[((nyears-1)*360+1)]      
Fluxbird_seal          <-   fluxbird_seal[ndays]      -     fluxbird_seal[((nyears-1)*360+1)]      

Fluxdisc_ceta            <-   fluxdisc_ceta[ndays]      -     fluxdisc_ceta[((nyears-1)*360+1)]      
Fluxomni_ceta            <-   fluxomni_ceta[ndays]      -     fluxomni_ceta[((nyears-1)*360+1)]      
Fluxcarn_ceta            <-   fluxcarn_ceta[ndays]      -     fluxcarn_ceta[((nyears-1)*360+1)]      
Fluxpfish_ceta           <-   fluxpfish_ceta[ndays]      -     fluxpfish_ceta[((nyears-1)*360+1)]      
Fluxmfish_ceta           <-   fluxmfish_ceta[ndays]      -     fluxmfish_ceta[((nyears-1)*360+1)]      
Fluxdfish_ceta           <-   fluxdfish_ceta[ndays]      -     fluxdfish_ceta[((nyears-1)*360+1)]      
Fluxbenths_ceta          <-   fluxbenths_ceta[ndays]      -     fluxbenths_ceta[((nyears-1)*360+1)]      
Fluxbenthc_ceta          <-   fluxbenthc_ceta[ndays]      -     fluxbenthc_ceta[((nyears-1)*360+1)]      
Fluxbird_ceta            <-   fluxbird_ceta[ndays]      -     fluxbird_ceta[((nyears-1)*360+1)]      
Fluxseal_ceta            <-   fluxseal_ceta[ndays]      -     fluxseal_ceta[((nyears-1)*360+1)]      


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


export_from_2prod<-  Fluxomni_carn +
                   + Fluxomni_pfishlar + 
                   + Fluxomni_dfishlar +
                   + Fluxomni_pfish +
                   + Fluxomni_ceta +
                   + Fluxbenths_dfish +
                   + Fluxbenths_benthc +
                   + Fluxbenths_bird +
                   + Fluxbenths_seal +
                   + Fluxbenths_ceta


Pfish_annual_spawn   <- Pfish_spawn[ndays]-Pfish_spawn[((nyears-1)*360+1)]
Pfish_annual_recruit <- Pfish_recruit[ndays]-Pfish_recruit[((nyears-1)*360+1)]
Dfish_annual_spawn   <- Dfish_spawn[ndays]-Dfish_spawn[((nyears-1)*360+1)]
Dfish_annual_recruit <- Dfish_recruit[ndays]-Dfish_recruit[((nyears-1)*360+1)]

Benths_annual_spawn   <- Bs_spawn[ndays]-Bs_spawn[((nyears-1)*360+1)]
Benths_annual_recruit <- Bs_recruit[ndays]-Bs_recruit[((nyears-1)*360+1)]
Benthc_annual_spawn   <- Bc_spawn[ndays]-Bc_spawn[((nyears-1)*360+1)]
Benthc_annual_recruit <- Bc_recruit[ndays]-Bc_recruit[((nyears-1)*360+1)]

FishpLand_livewt<-landp[ndays]-landp[((nyears-1)*360+1)]
FishmLand_livewt<-landm[ndays]-landm[((nyears-1)*360+1)]
FishdLand_livewt<-landd[ndays]-landd[((nyears-1)*360+1)]
Fishd_qLand_livewt<-landd_quota[ndays]-landd_quota[((nyears-1)*360+1)]
Fishd_nqLand_livewt<-landd_nonquota[ndays]-landd_nonquota[((nyears-1)*360+1)]
BenthsLand_livewt <- landsb[ndays]-landsb[((nyears-1)*360+1)]
BenthcLand_livewt <- landcb[ndays]-landcb[((nyears-1)*360+1)]
CarnzLand_livewt <- landcz[ndays]-landcz[((nyears-1)*360+1)]
BirdLand_livewt <- landbd[ndays]-landbd[((nyears-1)*360+1)]
SealLand_livewt <- landsl[ndays]-landsl[((nyears-1)*360+1)]
CetaLand_livewt <- landct[ndays]-landct[((nyears-1)*360+1)]
KelpLand_livewt <- landkp_i[ndays]-landkp_i[((nyears-1)*360+1)]

FishpDiscard<-discpel[ndays]-discpel[((nyears-1)*360+1)]
FishmDiscard<-discmig[ndays]-discmig[((nyears-1)*360+1)]
FishdDiscard<-discdem[ndays]-discdem[((nyears-1)*360+1)]
Fishd_qDiscard<-discdem_quota[ndays]-discdem_quota[((nyears-1)*360+1)]
Fishd_nqDiscard<-discdem_nonquota[ndays]-discdem_nonquota[((nyears-1)*360+1)]
BenthsDiscard <- discsb[ndays]-discsb[((nyears-1)*360+1)]
BenthcDiscard <- disccb[ndays]-disccb[((nyears-1)*360+1)]
CarnzDiscard  <- disccz[ndays]-disccz[((nyears-1)*360+1)]
BirdDiscard   <- discbd[ndays]-discbd[((nyears-1)*360+1)]
SealDiscard   <- discsl[ndays]-discsl[((nyears-1)*360+1)]
CetaDiscard   <- discct[ndays]-discct[((nyears-1)*360+1)]
KelpDiscard   <- disckp_i[ndays]-disckp_i[((nyears-1)*360+1)]

FishpOffal<-offalpel[ndays]-offalpel[((nyears-1)*360+1)]
FishmOffal<-offalmig[ndays]-offalmig[((nyears-1)*360+1)]
FishdOffal<-offaldem[ndays]-offaldem[((nyears-1)*360+1)]
Fishd_qOffal<-offaldem_quota[ndays]-offaldem_quota[((nyears-1)*360+1)]
Fishd_nqOffal<-offaldem_nonquota[ndays]-offaldem_nonquota[((nyears-1)*360+1)]
BenthsOffal <- offalsb[ndays]-offalsb[((nyears-1)*360+1)]
BenthcOffal <- offalcb[ndays]-offalcb[((nyears-1)*360+1)]
CarnzOffal  <- offalcz[ndays]-offalcz[((nyears-1)*360+1)]
BirdOffal   <- offalbd[ndays]-offalbd[((nyears-1)*360+1)]
SealOffal   <- offalsl[ndays]-offalsl[((nyears-1)*360+1)]
CetaOffal   <- offalct[ndays]-offalct[((nyears-1)*360+1)]
KelpOffal   <- offalkp_i[ndays]-offalkp_i[((nyears-1)*360+1)]

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



#Text descriptions of units set here for each row of output data
annual_flux_results[,2]<-rep("mMN/whole_model_domain_(1m2)/y",240)
annual_flux_results[8:13,2]<-("mMN/whole_model_domain_(1m2)/summer_period_AMJJAS")
annual_flux_results[c(217,221:228,235:240),2]<-"dimensionless"
annual_flux_results[c(218:220,229:234),2]<-"m"


annual_flux_results[,3]<-annual_flux_descriptions
#The vector of text descriptions for each row of output is set in internal.R


names(annual_flux_results)<-c("Model_annual_flux","Units","Description")




#Print the data to a csv file
#-----------------------------------------------------------------
filename = csvname(resultsdir, "WHOLEDOMAIN_model_annualresults", identifier)
writecsv(annual_flux_results, filename, row.names=FALSE)

#-------------------------------------------------------------------------------------------------------

	list(
		mass_results		= mass_results,
		maxmass_results		= maxmass_results,
		minmass_results		= minmass_results,
		annual_flux_results	= annual_flux_results
	)
}

