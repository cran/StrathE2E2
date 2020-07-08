#
# calculate_uptakes.R
#
#' Apply perturbed preference values to fitted parameters to calculate uptakes
#'
#' @param fitted.parms fitted model parameters
#'
#' @return uptake parameters
#'
#' @noRd
#
# ------------------------------------------------------------------------------

calculate_uptakes <- function(fitted.parms) {

	PREF_NIT_kelp		= elt(fitted.parms, "PREF_NIT_kelp")
	PREF_AMM_kelp		= elt(fitted.parms, "PREF_AMM_kelp")
	PREF_NIT_phyt		= elt(fitted.parms, "PREF_NIT_phyt")
	PREF_AMM_phyt		= elt(fitted.parms, "PREF_AMM_phyt")
	PREF_phyt_herb		= elt(fitted.parms, "PREF_phyt_herb")
	PREF_det_herb		= elt(fitted.parms, "PREF_det_herb")
	PREF_benthslar_herb	= elt(fitted.parms, "PREF_benthslar_herb")
	PREF_benthclar_herb	= elt(fitted.parms, "PREF_benthclar_herb")
	PREF_herb_carn		= elt(fitted.parms, "PREF_herb_carn")
	PREF_benthslar_carn	= elt(fitted.parms, "PREF_benthslar_carn")
	PREF_benthclar_carn	= elt(fitted.parms, "PREF_benthclar_carn")
	PREF_fishplar_carn	= elt(fitted.parms, "PREF_fishplar_carn")
	PREF_fishdlar_carn	= elt(fitted.parms, "PREF_fishdlar_carn")
	PREF_herb_fishplar	= elt(fitted.parms, "PREF_herb_fishplar")
	PREF_benthslar_fishplar	= elt(fitted.parms, "PREF_benthslar_fishplar")
	PREF_benthclar_fishplar	= elt(fitted.parms, "PREF_benthclar_fishplar")
	PREF_herb_fishp		= elt(fitted.parms, "PREF_herb_fishp")
	PREF_carn_fishp		= elt(fitted.parms, "PREF_carn_fishp")
	PREF_benthslar_fishp	= elt(fitted.parms, "PREF_benthslar_fishp")
	PREF_benthclar_fishp	= elt(fitted.parms, "PREF_benthclar_fishp")
	PREF_fishdlar_fishp	= elt(fitted.parms, "PREF_fishdlar_fishp")
	PREF_fishplar_fishp	= elt(fitted.parms, "PREF_fishplar_fishp")
	PREF_herb_fishm		= elt(fitted.parms, "PREF_herb_fishm")
	PREF_carn_fishm		= elt(fitted.parms, "PREF_carn_fishm")
	PREF_benthslar_fishm	= elt(fitted.parms, "PREF_benthslar_fishm")
	PREF_benthclar_fishm	= elt(fitted.parms, "PREF_benthclar_fishm")
	PREF_fishdlar_fishm	= elt(fitted.parms, "PREF_fishdlar_fishm")
	PREF_fishplar_fishm	= elt(fitted.parms, "PREF_fishplar_fishm")
	PREF_herb_fishdlar	= elt(fitted.parms, "PREF_herb_fishdlar")
	PREF_benthslar_fishdlar	= elt(fitted.parms, "PREF_benthslar_fishdlar")
	PREF_benthclar_fishdlar	= elt(fitted.parms, "PREF_benthclar_fishdlar")
	PREF_carn_fishd		= elt(fitted.parms, "PREF_carn_fishd")
	PREF_benths_fishd	= elt(fitted.parms, "PREF_benths_fishd")
	PREF_benthc_fishd	= elt(fitted.parms, "PREF_benthc_fishd")
	PREF_fishplar_fishd	= elt(fitted.parms, "PREF_fishplar_fishd")
	PREF_fishdlar_fishd	= elt(fitted.parms, "PREF_fishdlar_fishd")
	PREF_fishp_fishd	= elt(fitted.parms, "PREF_fishp_fishd")
	PREF_fishm_fishd	= elt(fitted.parms, "PREF_fishm_fishd")
	PREF_fishd_fishd	= elt(fitted.parms, "PREF_fishd_fishd")
	PREF_disc_fishd		= elt(fitted.parms, "PREF_disc_fishd")
	PREF_corp_fishd		= elt(fitted.parms, "PREF_corp_fishd")
	PREF_phyt_benthslar	= elt(fitted.parms, "PREF_phyt_benthslar")
	PREF_det_benthslar	= elt(fitted.parms, "PREF_det_benthslar")
	PREF_phyt_benthclar	= elt(fitted.parms, "PREF_phyt_benthclar")
	PREF_det_benthclar	= elt(fitted.parms, "PREF_det_benthclar")
	PREF_phyt_benths	= elt(fitted.parms, "PREF_phyt_benths")
	PREF_det_benths		= elt(fitted.parms, "PREF_det_benths")
	PREF_sed_benths		= elt(fitted.parms, "PREF_sed_benths")
	PREF_kelp_benthc	= elt(fitted.parms, "PREF_kelp_benthc")
	PREF_kelpdebris_benthc	= elt(fitted.parms, "PREF_kelpdebris_benthc")
	PREF_benths_benthc	= elt(fitted.parms, "PREF_benths_benthc")
	PREF_corp_benthc	= elt(fitted.parms, "PREF_corp_benthc")
	PREF_carn_bird		= elt(fitted.parms, "PREF_carn_bird")
	PREF_benths_bird	= elt(fitted.parms, "PREF_benths_bird")
	PREF_benthc_bird	= elt(fitted.parms, "PREF_benthc_bird")
	PREF_fishp_bird		= elt(fitted.parms, "PREF_fishp_bird")
	PREF_fishm_bird		= elt(fitted.parms, "PREF_fishm_bird")
	PREF_fishd_bird		= elt(fitted.parms, "PREF_fishd_bird")
	PREF_disc_bird		= elt(fitted.parms, "PREF_disc_bird")
	PREF_corp_bird		= elt(fitted.parms, "PREF_corp_bird")
	PREF_carn_seal		= elt(fitted.parms, "PREF_carn_seal")
	PREF_benths_seal	= elt(fitted.parms, "PREF_benths_seal")
	PREF_benthc_seal	= elt(fitted.parms, "PREF_benthc_seal")
	PREF_fishp_seal		= elt(fitted.parms, "PREF_fishp_seal")
	PREF_fishm_seal		= elt(fitted.parms, "PREF_fishm_seal")
	PREF_fishd_seal		= elt(fitted.parms, "PREF_fishd_seal")
	PREF_bird_seal		= elt(fitted.parms, "PREF_bird_seal")
	PREF_disc_seal		= elt(fitted.parms, "PREF_disc_seal")
	PREF_corp_seal		= elt(fitted.parms, "PREF_corp_seal")
	PREF_herb_ceta		= elt(fitted.parms, "PREF_herb_ceta")
	PREF_carn_ceta		= elt(fitted.parms, "PREF_carn_ceta")
	PREF_benths_ceta	= elt(fitted.parms, "PREF_benths_ceta")
	PREF_benthc_ceta	= elt(fitted.parms, "PREF_benthc_ceta")
	PREF_fishp_ceta		= elt(fitted.parms, "PREF_fishp_ceta")
	PREF_fishm_ceta		= elt(fitted.parms, "PREF_fishm_ceta")
	PREF_fishd_ceta		= elt(fitted.parms, "PREF_fishd_ceta")
	PREF_bird_ceta		= elt(fitted.parms, "PREF_bird_ceta")
	PREF_seal_ceta		= elt(fitted.parms, "PREF_seal_ceta")
	PREF_disc_ceta		= elt(fitted.parms, "PREF_disc_ceta")

	#Then we set the rate parameters for each predator at the reference temperature

	u_kelp			= elt(fitted.parms, "u_kelp")
	u_phyt			= elt(fitted.parms, "u_phyt")
	u_herb			= elt(fitted.parms, "u_herb")
	u_carn			= elt(fitted.parms, "u_carn")
	u_fishplar		= elt(fitted.parms, "u_fishplar")
	u_fishp			= elt(fitted.parms, "u_fishp")
	u_fishm			= elt(fitted.parms, "u_fishm")
	u_fishdlar		= elt(fitted.parms, "u_fishdlar")
	u_fishd			= elt(fitted.parms, "u_fishd")
	u_benthslar		= elt(fitted.parms, "u_benthslar")
	u_benthclar		= elt(fitted.parms, "u_benthclar")
	u_benths		= elt(fitted.parms, "u_benths")
	u_benthc		= elt(fitted.parms, "u_benthc")
	u_bird			= elt(fitted.parms, "u_bird")
	u_seal			= elt(fitted.parms, "u_seal")
	u_ceta			= elt(fitted.parms, "u_ceta")

	h_kelp			= elt(fitted.parms, "h_kelp")
	h_phyt			= elt(fitted.parms, "h_phyt")
	h_herb			= elt(fitted.parms, "h_herb")
	h_carn			= elt(fitted.parms, "h_carn")
	h_fishplar		= elt(fitted.parms, "h_fishplar")
	h_fishp			= elt(fitted.parms, "h_fishp")
	h_fishm			= elt(fitted.parms, "h_fishm")
	h_fishdlar		= elt(fitted.parms, "h_fishdlar")
	h_fishd			= elt(fitted.parms, "h_fishd")
	h_benthslar		= elt(fitted.parms, "h_benthslar")
	h_benthclar		= elt(fitted.parms, "h_benthclar")
	h_benths		= elt(fitted.parms, "h_benths")
	h_benthc		= elt(fitted.parms, "h_benthc")
	h_bird			= elt(fitted.parms, "h_bird")
	h_seal			= elt(fitted.parms, "h_seal")
	h_ceta			= elt(fitted.parms, "h_ceta")

	bda_par_bird		= elt(fitted.parms, "bda_par_bird")
	bda_par_seal		= elt(fitted.parms, "bda_par_seal")
	bda_par_ceta		= elt(fitted.parms, "bda_par_ceta")

	uptakes <- c(
		#Nutrient uptake by kelp at the reference temperature
		uNIT_kelpt		= u_kelp*PREF_NIT_kelp,
		hsNIT_kelp		= h_kelp,
		uAMM_kelpt		= u_kelp*PREF_AMM_kelp,
		hsAMM_kelp		= h_kelp,

		#Nutrient uptake by phytoplankton at the reference temperature
		uNIT_phytt		= u_phyt*PREF_NIT_phyt,
		hsNIT_phyt		= h_phyt,
		uAMM_phytt		= u_phyt*PREF_AMM_phyt,
		hsAMM_phyt		= h_phyt,

		#Feeding by mesozooplankton at the reference temperature
		uphyt_herbt		= u_herb*PREF_phyt_herb,
		hsphyt_herb		= h_herb,
		udet_herbt		= u_herb*PREF_det_herb,
		hsdet_herb		= h_herb,
		ubenthslar_herbt	= u_herb*PREF_benthslar_herb,
		hsbenthslar_herb	= h_herb,
		ubenthclar_herbt	= u_herb*PREF_benthclar_herb,
		hsbenthclar_herb	= h_herb,

		#Feeding by carnivorous zooplankton at the reference temperature
		uherb_carnt		= u_carn*PREF_herb_carn,
		hsherb_carn		= h_carn,
		ubenthslar_carnt	= u_carn*PREF_benthslar_carn,
		hsbenthslar_carn	= h_carn,
		ubenthclar_carnt	= u_carn*PREF_benthclar_carn,
		hsbenthclar_carn	= h_carn,
		ufishplar_carnt		= u_carn*PREF_fishplar_carn,
		hsfishplar_carn		= h_carn,
		ufishdlar_carnt		= u_carn*PREF_fishdlar_carn,
		hsfishdlar_carn		= h_carn,

		#Feeding by larvae of pelagic fish at the reference temperature
		uherb_fishplart		= u_fishplar*PREF_herb_fishplar,
		hsherb_fishplar		= h_fishplar,
		ubenthslar_fishplart	= u_fishplar*PREF_benthslar_fishplar,
		hsbenthslar_fishplar	= h_fishplar,
		ubenthclar_fishplart	= u_fishplar*PREF_benthclar_fishplar,
		hsbenthclar_fishplar	= h_fishplar,

		#Feeding by pelagic fish at the reference temperature
		uherb_fishpt		= u_fishp*PREF_herb_fishp,
		hsherb_fishp		= h_fishp,
		ucarn_fishpt		= u_fishp*PREF_carn_fishp,
		hscarn_fishp		= h_fishp,
		ubenthslar_fishpt	= u_fishp*PREF_benthslar_fishp,
		hsbenthslar_fishp	= h_fishp,
		ubenthclar_fishpt	= u_fishp*PREF_benthclar_fishp,
		hsbenthclar_fishp	= h_fishp,
		ufishdlar_fishpt	= u_fishp*PREF_fishdlar_fishp,
		hsfishdlar_fishp	= h_fishp,
		ufishplar_fishpt	= u_fishp*PREF_fishplar_fishp,
		hsfishplar_fishp	= h_fishp,

		#Feeding by migratory fish at the reference temperature
		uherb_fishmt		= u_fishm*PREF_herb_fishm,
		hsherb_fishm		= h_fishm,
		ucarn_fishmt		= u_fishm*PREF_carn_fishm,
		hscarn_fishm		= h_fishm,
		ubenthslar_fishmt	= u_fishm*PREF_benthslar_fishm,
		hsbenthslar_fishm	= h_fishm,
		ubenthclar_fishmt	= u_fishm*PREF_benthclar_fishm,
		hsbenthclar_fishm	= h_fishm,
		ufishdlar_fishmt	= u_fishm*PREF_fishdlar_fishm,
		hsfishdlar_fishm	= h_fishm,
		ufishplar_fishmt	= u_fishm*PREF_fishplar_fishm,
		hsfishplar_fishm	= h_fishm,

		#Feeding by larvae of demersal fish at the reference temperature
		uherb_fishdlart		= u_fishdlar*PREF_herb_fishdlar,
		hsherb_fishdlar		= h_fishdlar,
		ubenthslar_fishdlart	= u_fishdlar*PREF_benthslar_fishdlar,
		hsbenthslar_fishdlar	= h_fishdlar,
		ubenthclar_fishdlart	= u_fishdlar*PREF_benthclar_fishdlar,
		hsbenthclar_fishdlar	= h_fishdlar,

		#Feeding by demersal fish at the reference temperature
		ucarn_fishdt		= u_fishd*PREF_carn_fishd,
		hscarn_fishd		= h_fishd,
		ubenths_fishdt		= u_fishd*PREF_benths_fishd,
		hsbenths_fishd		= h_fishd,
		ubenthc_fishdt		= u_fishd*PREF_benthc_fishd,
		hsbenthc_fishd		= h_fishd,
		ufishplar_fishdt	= u_fishd*PREF_fishplar_fishd,
		hsfishplar_fishd	= h_fishd,
		ufishdlar_fishdt	= u_fishd*PREF_fishdlar_fishd,
		hsfishdlar_fishd	= h_fishd,
		ufishp_fishdt		= u_fishd*PREF_fishp_fishd,
		hsfishp_fishd		= h_fishd,
		ufishm_fishdt		= u_fishd*PREF_fishm_fishd,
		hsfishm_fishd		= h_fishd,
		ufishd_fishdt		= u_fishd*PREF_fishd_fishd,
		hsfishd_fishd		= h_fishd,
		udisc_fishdt		= u_fishd*PREF_disc_fishd,
		hsdisc_fishd		= h_fishd,
		ucorp_fishdt		= u_fishd*PREF_corp_fishd,
		hscorp_fishd		= h_fishd,

		#Feeding by larvae of suspension feeding benthos at the reference temperature
		uphyt_benthslart	= u_benthslar*PREF_phyt_benthslar,
		hsphyt_benthslar	= h_benthslar,
		udet_benthslart		= u_benthslar*PREF_det_benthslar,
		hsdet_benthslar		= h_benthslar,

		#Feeding by larvae of carnivore/scavenge feeding benthos at the reference temperature
		uphyt_benthclart	= u_benthclar*PREF_phyt_benthclar,
		hsphyt_benthclar	= h_benthclar,
		udet_benthclart		= u_benthclar*PREF_det_benthclar,
		hsdet_benthclar		= h_benthclar,

		#Feeding by suspension feeding benthos at the reference temperature
		uphyt_benthst		= u_benths*PREF_phyt_benths,
		hsphyt_benths		= h_benths,
		udet_benthst		= u_benths*PREF_det_benths,
		hsdet_benths		= h_benths,
		used_benthst		= u_benths*PREF_sed_benths,
		hssed_benths		= h_benths,

		#Feeding by carnivorous benthos at the reference temperature
		ubenths_benthct		= u_benthc*PREF_benths_benthc,
		hsbenths_benthc		= h_benthc,
		ukelp_benthct		= u_benthc*PREF_kelp_benthc,
		hskelp_benthc		= h_benthc,
		ukelpdebris_benthct	= u_benthc*PREF_kelpdebris_benthc,
		hskelpdebris_benthc	= h_benthc,
		ucorp_benthct		= u_benthc*PREF_corp_benthc,
		hscorp_benthc		= h_benthc,

		#Feeding by birds and mammals - temperature independent
		#uherb_bird		= u_bird*PREF_herb_bird,
		#hsherb_bird		= h_bird,
		ucarn_bird		= u_bird*PREF_carn_bird,
		hscarn_bird		= h_bird,
		ubenths_bird		= u_bird*PREF_benths_bird,
		hsbenths_bird		= h_bird,
		ubenthc_bird		= u_bird*PREF_benthc_bird,
		hsbenthc_bird		= h_bird,

		ufishp_bird		= u_bird*PREF_fishp_bird,
		hsfishp_bird		= h_bird,
		ufishm_bird		= u_bird*PREF_fishm_bird,
		hsfishm_bird		= h_bird,
		ufishd_bird		= u_bird*PREF_fishd_bird,
		hsfishd_bird		= h_bird,
		udisc_bird		= u_bird*PREF_disc_bird,
		hsdisc_bird		= h_bird,
		ucorp_bird		= u_bird*PREF_corp_bird,
		hscorp_bird		= h_bird,

		bda_par_bird		= bda_par_bird,

		##uherb_seal=u_seal*PREF_herb_seal,hsherb_seal=h_seal,
		ucarn_seal		= u_seal*PREF_carn_seal,
		hscarn_seal		= h_seal,
		ubenths_seal		= u_seal*PREF_benths_seal,
		hsbenths_seal		= h_seal,
		ubenthc_seal		= u_seal*PREF_benthc_seal,
		hsbenthc_seal		= h_seal,

		ufishp_seal		= u_seal*PREF_fishp_seal,
		hsfishp_seal		= h_seal,
		ufishm_seal		= u_seal*PREF_fishm_seal,
		hsfishm_seal		= h_seal,
		ufishd_seal		= u_seal*PREF_fishd_seal,
		hsfishd_seal		= h_seal,

		ubird_seal		= u_seal*PREF_bird_seal,
		hsbird_seal		= h_seal,

		udisc_seal		= u_seal*PREF_disc_seal,
		hsdisc_seal		= h_seal,
		ucorp_seal		= u_seal*PREF_corp_seal,
		hscorp_seal		= h_seal,

		bdapar_seal		= bda_par_seal,

		uherb_ceta		= u_ceta*PREF_herb_ceta,
		hsherb_ceta		= h_ceta,
		ucarn_ceta		= u_ceta*PREF_carn_ceta,
		hscarn_ceta		= h_ceta,
		ubenths_ceta		= u_ceta*PREF_benths_ceta,
		hsbenths_ceta		= h_ceta,
		ubenthc_ceta		= u_ceta*PREF_benthc_ceta,
		hsbenthc_ceta		= h_ceta,

		ufishp_ceta		= u_ceta*PREF_fishp_ceta,
		hsfishp_ceta		= h_ceta,
		ufishm_ceta		= u_ceta*PREF_fishm_ceta,
		hsfishm_ceta		= h_ceta,
		ufishd_ceta		= u_ceta*PREF_fishd_ceta,
		hsfishd_ceta		= h_ceta,

		ubird_ceta		= u_ceta*PREF_bird_ceta,
		hsbird_ceta		= h_ceta,
		useal_ceta		= u_ceta*PREF_seal_ceta,
		hsseal_ceta		= h_ceta,

		udisc_ceta		= u_ceta*PREF_disc_ceta,
		hsdisc_ceta		= h_ceta,
		##ucorp_ceta=u_ceta*PREF_corp_ceta,hscorp_ceta=h_ceta,

		bdapar_ceta		= bda_par_ceta
	)

	#showall("u_ceta", u_ceta)
	#showall("PREF_herb_ceta", PREF_herb_ceta)
	#showall("uherb_ceta", uptakes[["uherb_ceta"]])

	uptakes
}

