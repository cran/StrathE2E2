#
# read_fitted_parameters.R
#
#' Load fitted parameter values for model
#'
#' Parameters which have been established by the simulated annealing scheme are
#' loaded from a csv file which is made from the last line of the 'accepted parameters'
#' file produced by the annealing programme.
#
#' A variety of other parameters which are not optimized by simulated annealing are
#' hard-wired in this subroutine
#'
#' @param model.path path to model
#'
#' @return list of fitted model parameters
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_fitted_parameters <- function(model.path) {

	preference_matrix_input	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FITTED_PARAMETERS_PREFERENCE, row.names=1)	# special
	uptake_mort_input	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FITTED_PARAMETERS_UPTAKE_MORT)
	microbiology_input	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FITTED_PARAMETERS_MICROBIOLOGY)

#showall("preference_matrix_input",preference_matrix_input)
#showall("PREF_NIT_kelp", preference_matrix_input$kelp[which(rownames(preference_matrix_input)=="nitrate")])
	# rownames(preference_matrix_input)
	#  [1] "ammonia"   "nitrate"   "suspdet"   "seddet"    "corpses"   "discards" 
	#  [7] "phyt"      "omnivzoo"  "carnzoo"   "fishplar"  "fishdlar"  "fishp"    
	# [13] "fishm"     "fishd"     "benthslar" "benthclar" "benths"    "benthc"   
	# [19] "bird"      "seal"    "ceta"  

	# make a linear list of prefs from the matrices:

	fitted.parameters <- list(
		# PREFS: 79 pars
		PREF_NIT_kelp		= preference_matrix_input$kelp[which(rownames(preference_matrix_input)=="nitrate")],
		PREF_AMM_kelp		= preference_matrix_input$kelp[which(rownames(preference_matrix_input)=="ammonia")],

		PREF_NIT_phyt		= preference_matrix_input$phyt[which(rownames(preference_matrix_input)=="nitrate")],
		PREF_AMM_phyt		= preference_matrix_input$phyt[which(rownames(preference_matrix_input)=="ammonia")],

		PREF_phyt_omni		= preference_matrix_input$omnivzoo[which(rownames(preference_matrix_input)=="phyt")],
		PREF_det_omni		= preference_matrix_input$omnivzoo[which(rownames(preference_matrix_input)=="suspdet")],
		PREF_benthslar_omni	= preference_matrix_input$omnivzoo[which(rownames(preference_matrix_input)=="benthslar")],
		PREF_benthclar_omni	= preference_matrix_input$omnivzoo[which(rownames(preference_matrix_input)=="benthclar")],

		PREF_omni_carn		= preference_matrix_input$carnzoo[which(rownames(preference_matrix_input)=="omnivzoo")],
		PREF_benthslar_carn	= preference_matrix_input$carnzoo[which(rownames(preference_matrix_input)=="benthslar")],
		PREF_benthclar_carn	= preference_matrix_input$carnzoo[which(rownames(preference_matrix_input)=="benthclar")],
		PREF_fishplar_carn	= preference_matrix_input$carnzoo[which(rownames(preference_matrix_input)=="fishplar")],
		PREF_fishdlar_carn	= preference_matrix_input$carnzoo[which(rownames(preference_matrix_input)=="fishdlar")],

		PREF_omni_fishplar	= preference_matrix_input$fishplar[which(rownames(preference_matrix_input)=="omnivzoo")],
		PREF_benthslar_fishplar	= preference_matrix_input$fishplar[which(rownames(preference_matrix_input)=="benthslar")],
		PREF_benthclar_fishplar	= preference_matrix_input$fishplar[which(rownames(preference_matrix_input)=="benthclar")],

		PREF_omni_fishp		= preference_matrix_input$fishp[which(rownames(preference_matrix_input)=="omnivzoo")],
		PREF_carn_fishp		= preference_matrix_input$fishp[which(rownames(preference_matrix_input)=="carnzoo")],
		PREF_benthslar_fishp	= preference_matrix_input$fishp[which(rownames(preference_matrix_input)=="benthslar")],
		PREF_benthclar_fishp	= preference_matrix_input$fishp[which(rownames(preference_matrix_input)=="benthclar")],
		PREF_fishdlar_fishp	= preference_matrix_input$fishp[which(rownames(preference_matrix_input)=="fishdlar")],
		PREF_fishplar_fishp	= preference_matrix_input$fishp[which(rownames(preference_matrix_input)=="fishplar")],

		PREF_omni_fishm		= preference_matrix_input$fishm[which(rownames(preference_matrix_input)=="omnivzoo")],
		PREF_carn_fishm		= preference_matrix_input$fishm[which(rownames(preference_matrix_input)=="carnzoo")],
		PREF_benthslar_fishm	= preference_matrix_input$fishm[which(rownames(preference_matrix_input)=="benthslar")],
		PREF_benthclar_fishm	= preference_matrix_input$fishm[which(rownames(preference_matrix_input)=="benthclar")],
		PREF_fishdlar_fishm	= preference_matrix_input$fishm[which(rownames(preference_matrix_input)=="fishdlar")],
		PREF_fishplar_fishm	= preference_matrix_input$fishm[which(rownames(preference_matrix_input)=="fishplar")],

		PREF_omni_fishdlar	= preference_matrix_input$fishdlar[which(rownames(preference_matrix_input)=="omnivzoo")],
		PREF_benthslar_fishdlar	= preference_matrix_input$fishdlar[which(rownames(preference_matrix_input)=="benthslar")],
		PREF_benthclar_fishdlar	= preference_matrix_input$fishdlar[which(rownames(preference_matrix_input)=="benthclar")],

		PREF_carn_fishd		= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="carnzoo")],
		PREF_benths_fishd	= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="benths")],
		PREF_benthc_fishd	= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="benthc")],
		PREF_fishplar_fishd	= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="fishplar")],
		PREF_fishdlar_fishd	= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="fishdlar")],
		PREF_fishp_fishd	= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="fishp")],
		PREF_fishm_fishd	= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="fishm")],
		PREF_fishd_fishd	= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="fishd")],
		PREF_disc_fishd		= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="discards")],
		PREF_corp_fishd		= preference_matrix_input$fishd[which(rownames(preference_matrix_input)=="corpses")],

		PREF_phyt_benthslar	= preference_matrix_input$benthslar[which(rownames(preference_matrix_input)=="phyt")],
		PREF_phyt_benthclar	= preference_matrix_input$benthclar[which(rownames(preference_matrix_input)=="phyt")],
		PREF_det_benthslar	= preference_matrix_input$benthslar[which(rownames(preference_matrix_input)=="suspdet")],
		PREF_det_benthclar	= preference_matrix_input$benthclar[which(rownames(preference_matrix_input)=="suspdet")],

		PREF_phyt_benths	= preference_matrix_input$benths[which(rownames(preference_matrix_input)=="phyt")],
		PREF_det_benths		= preference_matrix_input$benths[which(rownames(preference_matrix_input)=="suspdet")],
		PREF_sed_benths		= preference_matrix_input$benths[which(rownames(preference_matrix_input)=="seddet")],

		PREF_kelp_benthc	= preference_matrix_input$benthc[which(rownames(preference_matrix_input)=="kelp")],
		PREF_kelpdebris_benthc	= preference_matrix_input$benthc[which(rownames(preference_matrix_input)=="kelpdebris")],
		PREF_benths_benthc	= preference_matrix_input$benthc[which(rownames(preference_matrix_input)=="benths")],
		PREF_corp_benthc	= preference_matrix_input$benthc[which(rownames(preference_matrix_input)=="corpses")],

		PREF_carn_bird		= preference_matrix_input$bird[which(rownames(preference_matrix_input)=="carnzoo")],
		PREF_benths_bird	= preference_matrix_input$bird[which(rownames(preference_matrix_input)=="benths")],
		PREF_benthc_bird	= preference_matrix_input$bird[which(rownames(preference_matrix_input)=="benthc")],

		PREF_fishp_bird		= preference_matrix_input$bird[which(rownames(preference_matrix_input)=="fishp")],
		PREF_fishm_bird		= preference_matrix_input$bird[which(rownames(preference_matrix_input)=="fishm")],
		PREF_fishd_bird		= preference_matrix_input$bird[which(rownames(preference_matrix_input)=="fishd")],
		PREF_disc_bird		= preference_matrix_input$bird[which(rownames(preference_matrix_input)=="discards")],
		PREF_corp_bird		= preference_matrix_input$bird[which(rownames(preference_matrix_input)=="corpses")],

		PREF_carn_seal		= preference_matrix_input$seal[which(rownames(preference_matrix_input)=="carnzoo")],
		PREF_benths_seal	= preference_matrix_input$seal[which(rownames(preference_matrix_input)=="benths")],
		PREF_benthc_seal	= preference_matrix_input$seal[which(rownames(preference_matrix_input)=="benthc")],

		PREF_fishp_seal		= preference_matrix_input$seal[which(rownames(preference_matrix_input)=="fishp")],
		PREF_fishm_seal		= preference_matrix_input$seal[which(rownames(preference_matrix_input)=="fishm")],
		PREF_fishd_seal		= preference_matrix_input$seal[which(rownames(preference_matrix_input)=="fishd")],
		PREF_bird_seal		= preference_matrix_input$seal[which(rownames(preference_matrix_input)=="bird")],
		PREF_disc_seal		= preference_matrix_input$seal[which(rownames(preference_matrix_input)=="discards")],
		PREF_corp_seal		= preference_matrix_input$seal[which(rownames(preference_matrix_input)=="corpses")],

		PREF_omni_ceta		= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="omnivzoo")],
		PREF_carn_ceta		= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="carnzoo")],
		PREF_benths_ceta	= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="benths")],
		PREF_benthc_ceta	= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="benthc")],

		PREF_fishp_ceta		= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="fishp")],
		PREF_fishm_ceta		= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="fishm")],
		PREF_fishd_ceta		= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="fishd")],
		PREF_bird_ceta		= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="bird")],
		PREF_seal_ceta		= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="seal")],
		PREF_disc_ceta		= preference_matrix_input$ceta[which(rownames(preference_matrix_input)=="discards")],

		# uptake_mort_input: 18+19+17+16 = 70 pars
		# u_store: 18 pars
		uC_kelp			= uptake_mort_input$Cumax[which(uptake_mort_input$consumer=="kelp")],
		ddexudC_kelp		= uptake_mort_input$Cddexud[which(uptake_mort_input$consumer=="kelp")],
		u_kelp			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="kelp")],
		u_phyt			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="phyt_s")],
		u_omni			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="omnivzoo")],
		u_carn			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="carnzoo")],
		u_fishplar		= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="fishplar")],
		u_fishp			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="fishp")],
		u_fishm			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="fishm")],
		u_fishdlar		= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="fishdlar")],
		u_fishd			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="fishd")],
		u_benthslar		= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="benthslar")],
		u_benthclar		= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="benthclar")],
		u_benths		= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="benths")],
		u_benthc		= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="benthc")],
		u_bird			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="bird")],
		u_seal			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="seal")],
		u_ceta			= uptake_mort_input$Numax[which(uptake_mort_input$consumer=="ceta")],

		# h_store: 19 pars
		h_kelp			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="kelp")],
		h_phyt			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="phyt_s")],
		h_omni			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="omnivzoo")],
		h_carn			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="carnzoo")],
		h_fishplar		= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="fishplar")],
		h_fishp			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="fishp")],
		h_fishm			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="fishm")],
		h_fishdlar		= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="fishdlar")],
		h_fishd			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="fishd")],
		h_benthslar		= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="benthslar")],
		h_benthclar		= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="benthclar")],
		h_benths		= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="benths")],
		h_benthc		= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="benthc")],
		h_bird			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="bird")],
		h_seal			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="seal")],
		h_ceta			= uptake_mort_input$Nhsat[which(uptake_mort_input$consumer=="ceta")],
		bda_par_bird		= uptake_mort_input$BdeApar[which(uptake_mort_input$consumer=="bird")],
		bda_par_seal		= uptake_mort_input$BdeApar[which(uptake_mort_input$consumer=="seal")],
		bda_par_ceta		= uptake_mort_input$BdeApar[which(uptake_mort_input$consumer=="ceta")],

		#Mineralisation, nitrification and denitrification rates per day at the reference temperature
		# biogeostore: 22 pars
		xmt			= microbiology_input$Value[1],
		xnst			= microbiology_input$Value[2],
		xdst			= microbiology_input$Value[3],
		xndt			= microbiology_input$Value[4],
		xddt			= microbiology_input$Value[5],

		xqs_p1			= microbiology_input$Value[6],	# proportion of detritus which become refractory when minearlised
		xqs_p2			= microbiology_input$Value[7],	# ratio of refratory to labile detritus minearalisation rates
		xqs_p3			= microbiology_input$Value[8],	# proportion of refractory whiuch becomes labile when re-oxygenated

		xmsedt			= microbiology_input$Value[9],
		xmsens			= microbiology_input$Value[10],

		xnsedt			= microbiology_input$Value[11],
		xnsens			= microbiology_input$Value[12],

		xdsedt			= microbiology_input$Value[13],
		xdsens			= microbiology_input$Value[14],

		#Proportion of discards sinking to become seabed corpses per day - temperature independent
		xdisc_corp		= microbiology_input$Value[15],

		#Proportion of corpse mass converted to detritus per day at the reference temperature
		xxcorp_det		= microbiology_input$Value[16],		# orginally xcorp_det

		xkelpdebris_det		= microbiology_input$Value[17],

		#Sinking rates and their dependence on mixing - temperature independent
		xdsink_s		= microbiology_input$Value[18],
		xdsink_d		= microbiology_input$Value[19],

		#Density dependent self shading parameter for kelp
		xkelpshade		= microbiology_input$Value[20],

		#Wave-dependent beach-cast parameter for kelp debris
		xwave_kelpdebris	= microbiology_input$Value[21],

		#Fitting parameter fopr demersal discard rate - expect this to be about 1.0
		xdfdp			= microbiology_input$Value[22],


		# mortstore: 17 pars
		#Density dependent parameter linking wave height to kelp destruction rate
		xxwave_kelp		= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="kelp")],

		xxst			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="phyt_s")],
		xxdt			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="phyt_d")],

		#Death rate of carnivores fish birds and mammals per unit biomass - temperature independent
		xxomni			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="omnivzoo")],
		xxcarn			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="carnzoo")],
		xxbenthslar		= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="benthslar")],
		xxbenthclar		= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="benthclar")],
		xxbenths		= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="benths")],
		xxbenthc		= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="benthc")],
		xxpfishlar		= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="fishplar")],
		xxdfishlar		= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="fishdlar")],
		xxpfish			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="fishp")],
		xxmfish			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="fishm")],
		xxdfish			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="fishd")],
		xxbird			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="bird")],
		xxseal			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="seal")],
		xxceta			= uptake_mort_input$ddmort[which(uptake_mort_input$consumer=="ceta")],


		# reststore: 16 pars
		#Parameters for food gradient migration rates of pelagic and migratory fish
		xpfish_migcoef 		= uptake_mort_input$migration_coef[which(uptake_mort_input$consumer=="fishp")],
		xmfish_migcoef 		= uptake_mort_input$migration_coef[which(uptake_mort_input$consumer=="fishm")],
		xdfish_migcoef 		= uptake_mort_input$migration_coef[which(uptake_mort_input$consumer=="fishd")],
		xbird_migcoef 		= uptake_mort_input$migration_coef[which(uptake_mort_input$consumer=="bird")],

		xseal_migcoef 		= uptake_mort_input$migration_coef[which(uptake_mort_input$consumer=="seal")],
		xceta_migcoef 		= uptake_mort_input$migration_coef[which(uptake_mort_input$consumer=="ceta")],

		#Maximum proportions of the stock biomass which is accessible to the fisheries
		#Units proportions
		xmax_exploitable_f_KP	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="kelp")],
		xmax_exploitable_f_PF	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="fishp")],
		xmax_exploitable_f_DF	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="fishd")],
		xmax_exploitable_f_MF	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="fishm")],
		xmax_exploitable_f_SB	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="benths")],
		xmax_exploitable_f_CB	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="benthc")],
		xmax_exploitable_f_CZ	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="carnzoo")],
		xmax_exploitable_f_BD	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="bird")],
		xmax_exploitable_f_SL	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="seal")],
		xmax_exploitable_f_CT	= uptake_mort_input$max_exploitable_f[which(uptake_mort_input$consumer=="ceta")]
	)

	fitted.parameters
}

