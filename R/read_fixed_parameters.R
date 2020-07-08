#
# read_fixed_parameters.R
#
#' load the fixed parameters used in the model
#'
#' @param model.path path to model
#'
#' @return fixed model parameters
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_fixed_parameters <- function(model.path) {

	HWPdata_A <- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FIXED_PARAMETERS_MISCELLANEOUS)
	HWPdata_B <- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FIXED_PARAMETERS_CONSUMER)

	#Various values are hard-wired here...
	#These are:

	#irradiance at maximum carbon uptake by kelp
	satlight_kelp<-HWPdata_A[1,1]

	#Minimum and maximum NC ratio for kelp
	kelp_ncmin<-HWPdata_A[2,1]
	kelp_ncmax<-HWPdata_A[3,1]

	#Parameter linking beaching rate of kelp debris and wave height
	#	wave_kelpdebris<-HWPdata_A[4,1]

	#irradiance at maximum nutrient uptake by phytoplankton
	satlight_phyt<-HWPdata_A[4,1]

	#Autotroph Q10 value
	qtenauto<-HWPdata_A[5,1]

	#Heterotroph uptake Q10 value
	qtenhetero<-HWPdata_A[6,1]

	#Metabolic and bacterial Q10 value
	qtenmetabol<-HWPdata_A[7,1]

	#Q10 reference temperature
	qtenrreft<-HWPdata_A[8,1]


	#Proportion of consumption converted to growth - temperature independent
	#                              zooplankton, carnivores,
	#                              susp_benthos_larvae, carn_benthos_larvae,
	#                              susp_benthos, carn_benthos,
	#                              pel_fash_larvae, dem_fish_larvae,
	#                              pelagic_fish, demersal_fish,
	#                              birds, seals and cetaceans
	asimH     <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="omnivzoo")]	# ZZ could check these I suppose - otherwise you just import NULLs
	asimC     <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="carnzoo")]
	asimFplar <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="fishplar")]
	asimFdlar <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="fishdlar")]
	asimFp    <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="fishp")]
	asimFm    <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="fishm")]
	asimFd    <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="fishd")]
	asimBslar <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="benthslar")]
	asimBclar <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="benthclar")]
	asimBs    <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="benths")]
	asimBc    <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="benthc")]
	asimbird  <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="bird")]
	asimseal  <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="seal")]
	asimceta  <- HWPdata_B$assimilation_efficiency[which(HWPdata_B$consumer=="ceta")]


	#Background proportion of biomass converted to ammonia per day at the reference temperature (birds and mammals T independent)
	#                              zooplankton, carnivores,
	#                              susp_benthos_larvae, carn_benthos_larvae,
	#                              susp_benthos, carn_benthos,
	#                              pel_fash_larvae, dem_fish_larvae,
	#                              pelagic_fish, demersal_fish,
	#                              birds, seals and cetaceans

	excrHt     <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="omnivzoo")]
	excrCt     <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="carnzoo")]
	excrFplart <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="fishplar")]
	excrFdlart <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="fishdlar")]
	excrFpt    <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="fishp")]
	excrFmt    <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="fishm")]
	excrFdt    <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="fishd")]
	excrBslart <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="benthslar")]
	excrBclart <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="benthclar")]
	excrBst    <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="benths")]
	excrBct    <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="benthc")]
	excrbird   <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="bird")]
	excrseal   <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="seal")]
	excrceta   <- HWPdata_B$background_metabolic_rate[which(HWPdata_B$consumer=="ceta")]



	#Thresholds of protected biomass for fish and benthos - everything below these thresholds is inaccessible to the fisheries
	#Units mMN/m2

	xprotect_KP    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="kelp")]
	xprotect_CZ    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="carnzoo")]
	xprotect_PF    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="fishp")]
	xprotect_MF    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="fishm")]
	xprotect_DF    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="fishd")]
	xprotect_SB    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="benths")]
	xprotect_CB    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="benthc")]
	xprotect_BD    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="bird")]
	xprotect_SL    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="seal")]
	xprotect_CT    <- HWPdata_B$threshold_exploitable_biomass[which(HWPdata_B$consumer=="ceta")]


	#Annual weight specific fecundity values

	P_an_fec     <- HWPdata_B$annual_weight_specific_fecundity[which(HWPdata_B$consumer=="fishp")]
	D_an_fec     <- HWPdata_B$annual_weight_specific_fecundity[which(HWPdata_B$consumer=="fishd")]
	BS_an_fec    <- HWPdata_B$annual_weight_specific_fecundity[which(HWPdata_B$consumer=="benths")]
	BC_an_fec    <- HWPdata_B$annual_weight_specific_fecundity[which(HWPdata_B$consumer=="benthc")]

	#Residual inedible biomass - only for carnzoo so they do not get eaten to extinction by pelagic fish
	CZ_inedible  <- HWPdata_B$minimum_inedible_biomass[which(HWPdata_B$consumer=="carnzoo")]

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	fixed.parameters <- list(
		satlight_kelp	= satlight_kelp,
		kelp_ncmin	= kelp_ncmin,
		kelp_ncmax	= kelp_ncmax,
		satlight_phyt	= satlight_phyt,
		qtenauto	= qtenauto,
		qtenhetero	= qtenhetero,
		qtenmetabol	= qtenmetabol,
		qtenrreft	= qtenrreft,

		asimH		= asimH,
		asimC		= asimC,
		asimFplar	= asimFplar,
		asimFdlar	= asimFdlar,
		asimFp		= asimFp,
		asimFm		= asimFm,
		asimFd		= asimFd,
		asimBslar	= asimBslar,
		asimBclar	= asimBclar,
		asimBs		= asimBs,
		asimBc		= asimBc,
		asimbird	= asimbird,
		asimseal	= asimseal,
		asimceta	= asimceta,

		excrHt		= excrHt,
		excrCt		= excrCt,
		excrFplart	= excrFplart,
		excrFdlart	= excrFdlart,
		excrFpt		= excrFpt,
		excrFmt		= excrFmt,
		excrFdt		= excrFdt,
		excrBslart	= excrBslart,
		excrBclart	= excrBclart,
		excrBst		= excrBst,
		excrBct		= excrBct,
		excrbird	= excrbird,
		excrseal	= excrseal,
		excrceta	= excrceta,

		xprotect_KP	= xprotect_KP,
		xprotect_CZ	= xprotect_CZ,
		xprotect_PF	= xprotect_PF,
		xprotect_MF	= xprotect_MF,
		xprotect_DF	= xprotect_DF,
		xprotect_SB	= xprotect_SB,
		xprotect_CB	= xprotect_CB,
		xprotect_BD	= xprotect_BD,
		xprotect_SL	= xprotect_SL,
		xprotect_CT	= xprotect_CT,

		P_an_fec	= P_an_fec,
		D_an_fec	= D_an_fec,
		BS_an_fec	= BS_an_fec,
		BC_an_fec	= BC_an_fec,

		CZ_inedible	= CZ_inedible
	)

	fixed.parameters
}

