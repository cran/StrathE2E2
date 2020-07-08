#
# build_model_parameters.R
#
#' Build entire set of model parameters combining fixed, physical, fleet model output, plus some biomass thresholds
#'
#' @param model model object
#' @param uptakes prey uptake levels
#' @param fleet.model.output fleet model output
#'
#' @return model parameter vector
#'
#' @noRd
#
# ------------------------------------------------------------------------------

build_model_parameters <- function(model, uptakes, fleet.model.output) {

	# Unpack:
	data		<- elt(model, "data")
	fixed.parms	<- elt(data, "fixed.parameters")
	xprotect_PF	<- elt(fixed.parms, "xprotect_PF")	# used in calculations
	xprotect_DF	<- elt(fixed.parms, "xprotect_DF")
	xprotect_MF	<- elt(fixed.parms, "xprotect_MF")
	xprotect_SB	<- elt(fixed.parms, "xprotect_SB")
	xprotect_CB	<- elt(fixed.parms, "xprotect_CB")
	xprotect_CZ	<- elt(fixed.parms, "xprotect_CZ")
	xprotect_BD	<- elt(fixed.parms, "xprotect_BD")
	xprotect_SL	<- elt(fixed.parms, "xprotect_SL")
	xprotect_CT	<- elt(fixed.parms, "xprotect_CT")
	xprotect_KP	<- elt(fixed.parms, "xprotect_KP")
	CZ_inedible	<- elt(fixed.parms, "CZ_inedible")

	physical.parms	<- elt(data, "physical.parameters")
	x_shallowprop	<- elt(physical.parms, "x_shallowprop")

	fleet.vector	<- elt(fleet.model.output, "fleet_vector")

	fitted.parms	<- elt(data, "fitted.parameters")

	#--------------------------------------------------------------------------
	#Load all the parameter values into a single vector which will be passed to the model function.
	model.parameters <- c(
	
		#First the parameters for morphological and physical configuration of the model
		#These occupy elements 1:55 of parms
		thik_so			= elt(physical.parms, "so_depth"),
		thik_d			= elt(physical.parms, "d_depth"),
		thik_si			= elt(physical.parms, "si_depth"),
		thik_b			= elt(physical.parms, "bx_depth"),
	
		shallowprop		= x_shallowprop,
		#	volume_s=xs_volume,
		#	volume_d=xd_volume,
	
		area_s0			= elt(physical.parms, "x_area_s0"),
		area_s1			= elt(physical.parms, "x_area_s1"),
		area_s2			= elt(physical.parms, "x_area_s2"),
		area_s3			= elt(physical.parms, "x_area_s3"),

		area_d0			= elt(physical.parms, "x_area_d0"),
		area_d1			= elt(physical.parms, "x_area_d1"),
		area_d2			= elt(physical.parms, "x_area_d2"),
		area_d3			= elt(physical.parms, "x_area_d3"),
	
		rock_s1			= elt(physical.parms, "x_rock_s1"),
		rock_s2			= elt(physical.parms, "x_rock_s2"),
		rock_s3			= elt(physical.parms, "x_rock_s3"),
		rock_d1			= elt(physical.parms, "x_rock_d1"),
		rock_d2			= elt(physical.parms, "x_rock_d2"),
		rock_d3			= elt(physical.parms, "x_rock_d3"),
	
		nonrock_s		= elt(physical.parms, "x_nonrock_s"),
		nonrock_d		= elt(physical.parms, "x_nonrock_d"),
	
		thik_x_s1		= elt(physical.parms, "x_depth_s1"),
		thik_x_s2		= elt(physical.parms, "x_depth_s2"),
		thik_x_s3		= elt(physical.parms, "x_depth_s3"),
		thik_x_d1		= elt(physical.parms, "x_depth_d1"),
		thik_x_d2		= elt(physical.parms, "x_depth_d2"),
		thik_x_d3		= elt(physical.parms, "x_depth_d3"),

		porosity_s1		= elt(physical.parms, "x_poros_s1"),
		porosity_s2		= elt(physical.parms, "x_poros_s2"),
		porosity_s3		= elt(physical.parms, "x_poros_s3"),
		porosity_d1		= elt(physical.parms, "x_poros_d1"),
		porosity_d2		= elt(physical.parms, "x_poros_d2"),
		porosity_d3		= elt(physical.parms, "x_poros_d3"),
	
		sed_wat_dif_s1		= elt(physical.parms, "Kxw_s1"),
		sed_wat_dif_s2		= elt(physical.parms, "Kxw_s2"),
		sed_wat_dif_s3		= elt(physical.parms, "Kxw_s3"),
		sed_wat_dif_d1		= elt(physical.parms, "Kxw_d1"),
		sed_wat_dif_d2		= elt(physical.parms, "Kxw_d2"),
		sed_wat_dif_d3		= elt(physical.parms, "Kxw_d3"),
	
		#Reference value of log10(Kxw) for estimating habitat specific sediment geochemistry rates
		sed_ref_Kxw		= elt(physical.parms, "ref_Kxw"),
	
		#Bioturbation depth and erosion depths of the sediment as proportions of active sediment layer thickness
		bioturb_depth_s1	= elt(physical.parms, "xbioturb_depth_s1"),
		bioturb_depth_s2	= elt(physical.parms, "xbioturb_depth_s2"),
		bioturb_depth_s3	= elt(physical.parms, "xbioturb_depth_s3"),
		bioturb_depth_d1	= elt(physical.parms, "xbioturb_depth_d1"),
		bioturb_depth_d2	= elt(physical.parms, "xbioturb_depth_d2"),
		bioturb_depth_d3	= elt(physical.parms, "xbioturb_depth_d3"),
	
		erosion_depth_s1	= elt(physical.parms, "xerosion_depth_s1"),
		erosion_depth_s2	= elt(physical.parms, "xerosion_depth_s2"),
		erosion_depth_s3	= elt(physical.parms, "xerosion_depth_s3"),
		erosion_depth_d1	= elt(physical.parms, "xerosion_depth_d1"),
		erosion_depth_d2	= elt(physical.parms, "xerosion_depth_d2"),
		erosion_depth_d3	= elt(physical.parms, "xerosion_depth_d3"),
	
		lightSPM_intercept	= elt(physical.parms, "xlightSPM_intercept"),
		lightSPM_slope		= elt(physical.parms, "xlightSPM_slope"),

		inshore_phyt_prop_depth	= elt(physical.parms, "xinshore_phyt_prop_depth"),

		inshore_kelp_prop_depth	= elt(physical.parms, "xinshore_kelp_prop_depth"), 

		# parms so far: 56

		#------ Parameters transferred in from the fishing fleet model ----------
		# The fleet vector has 250 elements, so
		# these occupy elemnts 57:307 of the parms vector
	
		#Vector of parameters from the fishing fleet configuration model
		fleet.vector,
	
		#----- Ecological model parameters -------
	
		#Q10 temperature coefficients for heteroptophs, autotrophs and reference temperature
		qtena			= elt(fixed.parms, "qtenauto"),
		qtenh			= elt(fixed.parms, "qtenhetero"),
		qtenm			= elt(fixed.parms, "qtenmetabol"),
		qtenr			= elt(fixed.parms, "qtenrreft"),
	
		#Light saturation parameter for phytoplankton N uptake
		Lmaxup_phyt		= elt(fixed.parms, "satlight_phyt"),

		#Light saturation parameter for kelp C uptake
		Lmaxup_kelp		= elt(fixed.parms, "satlight_kelp"),

		#Maximum and minimum values of NC ratio in kelp
		NCmax_kelp		= elt(fixed.parms, "kelp_ncmax"),
		NCmin_kelp		= elt(fixed.parms, "kelp_ncmin"),

		#Then the fitted parameters

		#Wave dependent beach-case rate parameter for kelp_debris
		wave_beach_kelpdebris	= elt(fitted.parms, "xwave_kelpdebris"),

		#Maximum uptake rate of carbon by kelp
		umaxC_kelp		= elt(fitted.parms, "uC_kelp"),

		#Density dependent carbon exudation rate parameter for kelp
		exudeC_kelp		= elt(fitted.parms, "ddexudC_kelp"),

		#Density dependent self shading paramater for carbon uptake by kelp
		selfshade_kelp		= elt(fitted.parms, "xkelpshade"),

		#Nutrient uptake by kelp at the reference temperature
		uNIT_kelpt		= elt(uptakes, "uNIT_kelpt"),
		hsNIT_kelp		= elt(uptakes, "hsNIT_kelp"),
		uAMM_kelpt		= elt(uptakes, "uAMM_kelpt"),
		hsAMM_kelp		= elt(uptakes, "hsAMM_kelp"),

		#Nutrient uptake by phytoplankton at the reference temperature
		uNIT_phytt		= elt(uptakes, "uNIT_phytt"),
		hsNIT_phyt		= elt(uptakes, "hsNIT_phyt"),
		uAMM_phytt		= elt(uptakes, "uAMM_phytt"),
		hsAMM_phyt		= elt(uptakes, "hsAMM_phyt"),
	
		#Feeding by mesozooplankton at the reference temperature
		uphyt_herbt		= elt(uptakes, "uphyt_herbt"),
		hsphyt_herb		= elt(uptakes, "hsphyt_herb"),
		udet_herbt		= elt(uptakes, "udet_herbt"),
		hsdet_herb		= elt(uptakes, "hsdet_herb"),
		ubenthslar_herbt	= elt(uptakes, "ubenthslar_herbt"),
		hsbenthslar_herb	= elt(uptakes, "hsbenthslar_herb"),
		ubenthclar_herbt	= elt(uptakes, "ubenthclar_herbt"),
		hsbenthclar_herb	= elt(uptakes, "hsbenthclar_herb"),
	
		#Feeding by carnivorous zooplankton at the reference temperature
		uherb_carnt		= elt(uptakes, "uherb_carnt"),
		hsherb_carn		= elt(uptakes, "hsherb_carn"),
		ubenthslar_carnt	= elt(uptakes, "ubenthslar_carnt"),
		hsbenthslar_carn	= elt(uptakes, "hsbenthslar_carn"),
		ubenthclar_carnt	= elt(uptakes, "ubenthclar_carnt"),
		hsbenthclar_carn	= elt(uptakes, "hsbenthclar_carn"),
		ufishplar_carnt		= elt(uptakes, "ufishplar_carnt"),
		hsfishplar_carn		= elt(uptakes, "hsfishplar_carn"),
		ufishdlar_carnt		= elt(uptakes, "ufishdlar_carnt"),
		hsfishdlar_carn		= elt(uptakes, "hsfishdlar_carn"),
	
		#Feeding by larvae of pelagic fish at the reference temperature
		uherb_fishplart		= elt(uptakes, "uherb_fishplart"),
		hsherb_fishplar		= elt(uptakes, "hsherb_fishplar"),
		ubenthslar_fishplart	= elt(uptakes, "ubenthslar_fishplart"),
		hsbenthslar_fishplar	= elt(uptakes, "hsbenthslar_fishplar"),
		ubenthclar_fishplart	= elt(uptakes, "ubenthclar_fishplart"),
		hsbenthclar_fishplar	= elt(uptakes, "hsbenthclar_fishplar"),
	
		#Feeding by pelagic fish at the reference temperature
		uherb_fishpt		= elt(uptakes, "uherb_fishpt"),
		hsherb_fishp		= elt(uptakes, "hsherb_fishp"),
		ucarn_fishpt		= elt(uptakes, "ucarn_fishpt"),
		hscarn_fishp		= elt(uptakes, "hscarn_fishp"),
		ubenthslar_fishpt	= elt(uptakes, "ubenthslar_fishpt"),
		hsbenthslar_fishp	= elt(uptakes, "hsbenthslar_fishp"),
		ubenthclar_fishpt	= elt(uptakes, "ubenthclar_fishpt"),
		hsbenthclar_fishp	= elt(uptakes, "hsbenthclar_fishp"),
		ufishdlar_fishpt	= elt(uptakes, "ufishdlar_fishpt"),
		hsfishdlar_fishp	= elt(uptakes, "hsfishdlar_fishp"),
		ufishplar_fishpt	= elt(uptakes, "ufishplar_fishpt"),
		hsfishplar_fishp	= elt(uptakes, "hsfishplar_fishp"),
	
		#Feeding by migratory fish at the reference temperature
		uherb_fishmt		= elt(uptakes, "uherb_fishmt"),
		hsherb_fishm		= elt(uptakes, "hsherb_fishm"),
		ucarn_fishmt		= elt(uptakes, "ucarn_fishmt"),
		hscarn_fishm		= elt(uptakes, "hscarn_fishm"),
		ubenthslar_fishmt	= elt(uptakes, "ubenthslar_fishmt"),
		hsbenthslar_fishm	= elt(uptakes, "hsbenthslar_fishm"),
		ubenthclar_fishmt	= elt(uptakes, "ubenthclar_fishmt"),
		hsbenthclar_fishm	= elt(uptakes, "hsbenthclar_fishm"),
		ufishdlar_fishmt	= elt(uptakes, "ufishdlar_fishmt"),
		hsfishdlar_fishm	= elt(uptakes, "hsfishdlar_fishm"),
		ufishplar_fishmt	= elt(uptakes, "ufishplar_fishmt"),
		hsfishplar_fishm	= elt(uptakes, "hsfishplar_fishm"),
	
		#Feeding by larvae of demersal fish at the reference temperature
		uherb_fishdlart		= elt(uptakes, "uherb_fishdlart"),
		hsherb_fishdlar		= elt(uptakes, "hsherb_fishdlar"),
		ubenthslar_fishdlart	= elt(uptakes, "ubenthslar_fishdlart"),
		hsbenthslar_fishdlar	= elt(uptakes, "hsbenthslar_fishdlar"),
		ubenthclar_fishdlart	= elt(uptakes, "ubenthclar_fishdlart"),
		hsbenthclar_fishdlar	= elt(uptakes, "hsbenthclar_fishdlar"),
	
		#Feeding by demersal fish at the reference temperature
		ucarn_fishdt		= elt(uptakes, "ucarn_fishdt"),
		hscarn_fishd		= elt(uptakes, "hscarn_fishd"),
		ubenths_fishdt		= elt(uptakes, "ubenths_fishdt"),
		hsbenths_fishd		= elt(uptakes, "hsbenths_fishd"),
		ubenthc_fishdt		= elt(uptakes, "ubenthc_fishdt"),
		hsbenthc_fishd		= elt(uptakes, "hsbenthc_fishd"),
		ufishplar_fishdt	= elt(uptakes, "ufishplar_fishdt"),
		hsfishplar_fishd	= elt(uptakes, "hsfishplar_fishd"),
		ufishdlar_fishdt	= elt(uptakes, "ufishdlar_fishdt"),
		hsfishdlar_fishd	= elt(uptakes, "hsfishdlar_fishd"),
		ufishp_fishdt		= elt(uptakes, "ufishp_fishdt"),
		hsfishp_fishd		= elt(uptakes, "hsfishp_fishd"),
		ufishm_fishdt		= elt(uptakes, "ufishm_fishdt"),
		hsfishm_fishd		= elt(uptakes, "hsfishm_fishd"),
		ufishd_fishdt		= elt(uptakes, "ufishd_fishdt"),
		hsfishd_fishd		= elt(uptakes, "hsfishd_fishd"),
		udisc_fishdt		= elt(uptakes, "udisc_fishdt"),
		hsdisc_fishd		= elt(uptakes, "hsdisc_fishd"),
		ucorp_fishdt		= elt(uptakes, "ucorp_fishdt"),
		hscorp_fishd		= elt(uptakes, "hscorp_fishd"),
		
		#Feeding by larvae of suspension feeding benthos at the reference temperature
		uphyt_benthslart	= elt(uptakes, "uphyt_benthslart"),
		hsphyt_benthslar	= elt(uptakes, "hsphyt_benthslar"),
		udet_benthslart		= elt(uptakes, "udet_benthslart"),
		hsdet_benthslar		= elt(uptakes, "hsdet_benthslar"),
	
		#Feeding by larvae of carnivore/scavenge feeding benthos at the reference temperature
		uphyt_benthclart	= elt(uptakes, "uphyt_benthclart"),
		hsphyt_benthclar	= elt(uptakes, "hsphyt_benthclar"),
		udet_benthclart		= elt(uptakes, "udet_benthclart"),
		hsdet_benthclar		= elt(uptakes, "hsdet_benthclar"),
	
		#Feeding by suspension feeding benthos at the reference temperature
		uphyt_benthst		= elt(uptakes, "uphyt_benthst"),
		hsphyt_benths		= elt(uptakes, "hsphyt_benths"),
		udet_benthst		= elt(uptakes, "udet_benthst"),
		hsdet_benths		= elt(uptakes, "hsdet_benths"),
		used_benthst		= elt(uptakes, "used_benthst"),
		hssed_benths		= elt(uptakes, "hssed_benths"),
	
		#Feeding by carnivorous benthos at the reference temperature
		ubenths_benthct		= elt(uptakes, "ubenths_benthct"),
		hsbenths_benthc		= elt(uptakes, "hsbenths_benthc"),
		ukelp_benthct		= elt(uptakes, "ukelp_benthct"),
		hskelp_benthc		= elt(uptakes, "hskelp_benthc"),
		ukelpdebris_benthct	= elt(uptakes, "ukelpdebris_benthct"),
		hskelpdebris_benthc	= elt(uptakes, "hskelpdebris_benthc"),
		ucorp_benthct		= elt(uptakes, "ucorp_benthct"),
		hscorp_benthc		= elt(uptakes, "hscorp_benthc"),
	
		#Feeding by birds and mammals - temperature independent
	
		##uherb_bird		= elt(uptakes, "uherb_bird"),
		##hsherb_bird		= elt(uptakes, "hsherb_bird"),
		ucarn_bird		= elt(uptakes, "ucarn_bird"),
		hscarn_bird		= elt(uptakes, "hscarn_bird"),
		ubenths_bird		= elt(uptakes, "ubenths_bird"),
		hsbenths_bird		= elt(uptakes, "hsbenths_bird"),
		ubenthc_bird		= elt(uptakes, "ubenthc_bird"),
		hsbenthc_bird		= elt(uptakes, "hsbenthc_bird"),
	
		ufishp_bird		= elt(uptakes, "ufishp_bird"),
		hsfishp_bird		= elt(uptakes, "hsfishp_bird"),
		ufishm_bird		= elt(uptakes, "ufishm_bird"),
		hsfishm_bird		= elt(uptakes, "hsfishm_bird"),
		ufishd_bird		= elt(uptakes, "ufishd_bird"),
		hsfishd_bird		= elt(uptakes, "hsfishd_bird"),
		udisc_bird		= elt(uptakes, "udisc_bird"),
		hsdisc_bird		= elt(uptakes, "hsdisc_bird"),
		ucorp_bird		= elt(uptakes, "ucorp_bird"),
		hscorp_bird		= elt(uptakes, "hscorp_bird"),
	
		bda_par_bird		= elt(fitted.parms, "bda_par_bird"),

		ucarn_seal		= elt(uptakes, "ucarn_seal"),
		hscarn_seal		= elt(uptakes, "hscarn_seal"),
		ubenths_seal		= elt(uptakes, "ubenths_seal"),
		hsbenths_seal		= elt(uptakes, "hsbenths_seal"),
		ubenthc_seal		= elt(uptakes, "ubenthc_seal"),
		hsbenthc_seal		= elt(uptakes, "hsbenthc_seal"),

		ufishp_seal		= elt(uptakes, "ufishp_seal"),
		hsfishp_seal		= elt(uptakes, "hsfishp_seal"),
		ufishm_seal		= elt(uptakes, "ufishm_seal"),
		hsfishm_seal		= elt(uptakes, "hsfishm_seal"),
		ufishd_seal		= elt(uptakes, "ufishd_seal"),
		hsfishd_seal		= elt(uptakes, "hsfishd_seal"),

		ubird_seal		= elt(uptakes, "ubird_seal"),
		hsbird_seal		= elt(uptakes, "hsbird_seal"),

		udisc_seal		= elt(uptakes, "udisc_seal"),
		hsdisc_seal		= elt(uptakes, "hsdisc_seal"),
		ucorp_seal		= elt(uptakes, "ucorp_seal"),
		hscorp_seal		= elt(uptakes, "hscorp_seal"),

		bdapar_seal		= elt(uptakes, "bdapar_seal"),

		uherb_ceta		= elt(uptakes, "uherb_ceta"),
		hsherb_ceta		= elt(uptakes, "hsherb_ceta"),
		ucarn_ceta		= elt(uptakes, "ucarn_ceta"),
		hscarn_ceta		= elt(uptakes, "hscarn_ceta"),
		ubenths_ceta		= elt(uptakes, "ubenths_ceta"),
		hsbenths_ceta		= elt(uptakes, "hsbenths_ceta"),
		ubenthc_ceta		= elt(uptakes, "ubenthc_ceta"),
		hsbenthc_ceta		= elt(uptakes, "hsbenthc_ceta"),

		ufishp_ceta		= elt(uptakes, "ufishp_ceta"),
		hsfishp_ceta		= elt(uptakes, "hsfishp_ceta"),
		ufishm_ceta		= elt(uptakes, "ufishm_ceta"),
		hsfishm_ceta		= elt(uptakes, "hsfishm_ceta"),
		ufishd_ceta		= elt(uptakes, "ufishd_ceta"),
		hsfishd_ceta		= elt(uptakes, "hsfishd_ceta"),

		ubird_ceta		= elt(uptakes, "ubird_ceta"),
		hsbird_ceta		= elt(uptakes, "hsbird_ceta"),
		useal_ceta		= elt(uptakes, "useal_ceta"),
		hsseal_ceta		= elt(uptakes, "hsseal_ceta"),

		udisc_ceta		= elt(uptakes, "udisc_ceta"),
		hsdisc_ceta		= elt(uptakes, "hsdisc_ceta"),
		## ucorp_ceta=u_ceta*PREF_corp_ceta,hscorp_ceta=h_ceta,

		bdapar_ceta		= elt(uptakes, "bdapar_ceta"),

		#Proportion of consumption converted to growth - temperature independent
		aH			= elt(fixed.parms, "asimH"),
		aC			= elt(fixed.parms, "asimC"),
		aBslar			= elt(fixed.parms, "asimBslar"),
		aBclar			= elt(fixed.parms, "asimBclar"),
		aBs			= elt(fixed.parms, "asimBs"),
		aBc			= elt(fixed.parms, "asimBc"),
		aFplar			= elt(fixed.parms, "asimFplar"),
		aFdlar			= elt(fixed.parms, "asimFdlar"),
		aFp			= elt(fixed.parms, "asimFp"),
		aFm			= elt(fixed.parms, "asimFm"),
		aFd			= elt(fixed.parms, "asimFd"),
		abird			= elt(fixed.parms, "asimbird"),
		aseal			= elt(fixed.parms, "asimseal"),
		aceta			= elt(fixed.parms, "asimceta"),
	
		#Background proportion of biomass converted to ammonia per day at the reference temperature (birds and mammals T independent)
		eHt			= elt(fixed.parms, "excrHt"),
		eCt			= elt(fixed.parms, "excrCt"),
		eBslart			= elt(fixed.parms, "excrBslart"),
		eBclart			= elt(fixed.parms, "excrBclart"),
		eBst			= elt(fixed.parms, "excrBst"),
		eBct			= elt(fixed.parms, "excrBct"),
		eFplart			= elt(fixed.parms, "excrFplart"),
		eFdlart			= elt(fixed.parms, "excrFdlart"),
		eFpt			= elt(fixed.parms, "excrFpt"),
		eFmt			= elt(fixed.parms, "excrFmt"),
		eFdt			= elt(fixed.parms, "excrFdt"),
		ebirdt			= elt(fixed.parms, "excrbird"),
		esealt			= elt(fixed.parms, "excrseal"),
		ecetat			= elt(fixed.parms, "excrceta"),
	
		#Mineralisation, nitrification and denitrification raees per day at the reference temperature
		xmt			= elt(fitted.parms, "xmt"),
		xnst			= elt(fitted.parms, "xnst"),
		xdst			= elt(fitted.parms, "xdst"),
		xndt			= elt(fitted.parms, "xndt"),
		xddt			= elt(fitted.parms, "xddt"),
	
		xqs_p1			= elt(fitted.parms, "xqs_p1"),
		xqs_p2			= elt(fitted.parms, "xqs_p2"),
		xqs_p3			= elt(fitted.parms, "xqs_p3"),
	
		xmsedt			= elt(fitted.parms, "xmsedt"),
		xmsens			= elt(fitted.parms, "xmsens"),
	
		xnsedt			= elt(fitted.parms, "xnsedt"),
		xnsens			= elt(fitted.parms, "xnsens"),
	
		xdsedt			= elt(fitted.parms, "xdsedt"),
		xdsens			= elt(fitted.parms, "xdsens"),
	
		#Density and wave dependent destruction rate paramater for kelp
		xwave_kelp		= elt(fitted.parms, "xxwave_kelp"),

		#Death rates of phytoplankton at the reference temperature
		xxst			= elt(fitted.parms, "xxst"),
		xxdt			= elt(fitted.parms, "xxdt"),
	
		#Death rate per unit biomass for carnivores fish birds and mammals
		xxherb			= elt(fitted.parms, "xxherb"),
		xxcarn			= elt(fitted.parms, "xxcarn"),
		xxbenthslar		= elt(fitted.parms, "xxbenthslar"),
		xxbenthclar		= elt(fitted.parms, "xxbenthclar"),
		xxbenths		= elt(fitted.parms, "xxbenths"),
		xxbenthc		= elt(fitted.parms, "xxbenthc"),
		xxpfishlar		= elt(fitted.parms, "xxpfishlar"),
		xxdfishlar		= elt(fitted.parms, "xxdfishlar"),
		xxpfish			= elt(fitted.parms, "xxpfish"),
		xxmfish			= elt(fitted.parms, "xxmfish"),
		xxdfish			= elt(fitted.parms, "xxdfish"),
		xxbird			= elt(fitted.parms, "xxbird"),
		xxseal			= elt(fitted.parms, "xxseal"),
		xxceta			= elt(fitted.parms, "xxceta"),

		#Proportion of kelp debris becoming detritus per day
		kelpdebris_det		= elt(fitted.parms, "xkelpdebris_det"),

		#Proportion of seabed corpses becoming seabed detritus per day
		xxcorp_det		= elt(fitted.parms, "xxcorp_det"),
	
		#Proportion of discarded fish sinking to become seabed corpses per day
		xdisc_corp		= elt(fitted.parms, "xdisc_corp"),
	
		#deleted
		#Bioturbation scaling factor for deposit feeding benthos - scales xdet uptake/xdet to fraction of sedimnent excavated
		#	xbioturb=xxbioturb,
	
		#Sinking rates for detritus in the surface and deep layers
		#	dsink_s=xdsink_s,dsink_d_Klow=xdsink_d_Klow,dsink_d_Khi=xdsink_d_Khi,
		xdsink_s			= elt(fitted.parms, "xdsink_s"),
		xdsink_d			= elt(fitted.parms, "xdsink_d"),
	
		#Fitting parameter for scaling between whole model region demersal fish N mass and the survey
		#index on which the empirical relationships for pNQ and dsacrad ates of Q and NQ species are based.
		#Expect this to be abouut 2
		xdfdp			= elt(fitted.parms, "xdfdp"),
	
		#Fitted parameters for food gradient dependent fish migration rates
		xpfish_migcoef		= elt(fitted.parms, "xpfish_migcoef"),
		xmfish_migcoef		= elt(fitted.parms, "xmfish_migcoef"),
		xdfish_migcoef		= elt(fitted.parms, "xdfish_migcoef"),
		xbird_migcoef		= elt(fitted.parms, "xbird_migcoef"),
		xseal_migcoef		= elt(fitted.parms, "xseal_migcoef"),
		xceta_migcoef		= elt(fitted.parms, "xceta_migcoef"),
	
		#Protected biomass thresholds
		protect_PF_o = xprotect_PF * (1-x_shallowprop),
		protect_DF_o = xprotect_DF * (1-x_shallowprop),
		protect_MF_o = xprotect_MF * (1-x_shallowprop),
		protect_SB_o = xprotect_SB * (1-x_shallowprop),
		protect_CB_o = xprotect_CB * (1-x_shallowprop),
		protect_CZ_o = xprotect_CZ * (1-x_shallowprop),
		protect_BD_o = xprotect_BD * (1-x_shallowprop),
		protect_SL_o = xprotect_SL * (1-x_shallowprop),
		protect_CT_o = xprotect_CT * (1-x_shallowprop),
	
		protect_PF_i = xprotect_PF * (x_shallowprop),
		protect_DF_i = xprotect_DF * (x_shallowprop),
		protect_MF_i = xprotect_MF * (x_shallowprop),
		protect_SB_i = xprotect_SB * (x_shallowprop),
		protect_CB_i = xprotect_CB * (x_shallowprop),
		protect_CZ_i = xprotect_CZ * (x_shallowprop),
		protect_BD_i = xprotect_BD * (x_shallowprop),
		protect_SL_i = xprotect_SL * (x_shallowprop),
		protect_CT_i = xprotect_CT * (x_shallowprop),
		protect_KP_i = xprotect_KP,
	
		xmax_exploitable_f_PF		= elt(fitted.parms, "xmax_exploitable_f_PF"),
		xmax_exploitable_f_DF		= elt(fitted.parms, "xmax_exploitable_f_DF"),
		xmax_exploitable_f_MF		= elt(fitted.parms, "xmax_exploitable_f_MF"),
		xmax_exploitable_f_SB		= elt(fitted.parms, "xmax_exploitable_f_SB"),
		xmax_exploitable_f_CB		= elt(fitted.parms, "xmax_exploitable_f_CB"),
		xmax_exploitable_f_CZ		= elt(fitted.parms, "xmax_exploitable_f_CZ"),
		xmax_exploitable_f_BD		= elt(fitted.parms, "xmax_exploitable_f_BD"),
		xmax_exploitable_f_SL		= elt(fitted.parms, "xmax_exploitable_f_SL"),
		xmax_exploitable_f_CT		= elt(fitted.parms, "xmax_exploitable_f_CT"),
		xmax_exploitable_f_KP		= elt(fitted.parms, "xmax_exploitable_f_KP"),

		PF_fec				= elt(fixed.parms, "P_an_fec"),			# annual weight specific fecundities added to the end of the paramer list
		DF_fec				= elt(fixed.parms, "D_an_fec"),
		BS_fec				= elt(fixed.parms, "BS_an_fec"),
		BC_fec				= elt(fixed.parms, "BC_an_fec"),

		CZ_inedible_biomass_o		= CZ_inedible * (1-x_shallowprop),		# residual inedible biomass of carnzoo offshore
		CZ_inedible_biomass_i		= CZ_inedible * (x_shallowprop)			# residual inedible biomass of carnzoo inshore
	)

	model.parameters
} 
