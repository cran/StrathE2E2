#
# write_fitted_parameters.R
#
#' Save current set of fitted parameters to file
#'
#' @param model model object
#' @param parhistory parameter set history
#'
#' @return list of fitted model parameters
#'
#' @noRd
#
# ------------------------------------------------------------------------------

write_fitted_parameters <- function(model, parhistory, csv.output) {

	setup		<- elt(model, "setup")
	read.only	<- elt(setup, "read.only")
	model.path	<- elt(setup, "model.path")
	identifier	<- elt(setup, "model.ident")

	if (read.only & csv.output==TRUE) {
		message("Warning: cannot write fitted parameters back to the model input folders - model is read-only")
		message("Warning: to fix this, make a copy of the model using e2e_copy() into your own workspace.")
		stop("Model is not writable!")
	}

	#Now read in the externally stored ecology model parameters to act as templates for the fitted parameter file data structures

	preference_matrix_input	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FITTED_PARAMETERS_PREFERENCE, row.names=1)	# special
	uptake_mort_input	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FITTED_PARAMETERS_UPTAKE_MORT)
	microbiology_input	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FITTED_PARAMETERS_MICROBIOLOGY)

	#Copy the original parameter input structures to act as a template for the new data
	preference_matrix_input_NEW<-preference_matrix_input
	uptake_mort_input_NEW<-uptake_mort_input
	microbiology_input_NEW<-microbiology_input

	ROW_TO_USE <- nrow(parhistory)

	#ZZ use: preference_matrix_input["nitrate", "phyt"]
	# pars <- tail(parhistory,1) gets last row from DF
	# becomes
	#	preference_matrix_input_NEW["nitrate", "phyt"] <- pars$PREF_NIT_phyt
	# also ditch _NEW copies - not needed in function

preference_matrix_input_NEW$kelp[which(rownames(preference_matrix_input_NEW)=="nitrate")] <- parhistory$PREF_NIT_kelp[ROW_TO_USE]
preference_matrix_input_NEW$kelp[which(rownames(preference_matrix_input_NEW)=="ammonia")] <- parhistory$PREF_AMM_kelp[ROW_TO_USE]

preference_matrix_input_NEW$phyt[which(rownames(preference_matrix_input_NEW)=="nitrate")] <- parhistory$PREF_NIT_phyt[ROW_TO_USE]
preference_matrix_input_NEW$phyt[which(rownames(preference_matrix_input_NEW)=="ammonia")] <- parhistory$PREF_AMM_phyt[ROW_TO_USE]

preference_matrix_input_NEW$omnivzoo[which(rownames(preference_matrix_input_NEW)=="phyt")] <- parhistory$PREF_phyt_herb[ROW_TO_USE]      
preference_matrix_input_NEW$omnivzoo[which(rownames(preference_matrix_input_NEW)=="suspdet")] <- parhistory$PREF_det_herb[ROW_TO_USE]
preference_matrix_input_NEW$omnivzoo[which(rownames(preference_matrix_input_NEW)=="benthslar")] <- parhistory$PREF_benthslar_herb[ROW_TO_USE]
preference_matrix_input_NEW$omnivzoo[which(rownames(preference_matrix_input_NEW)=="benthclar")] <- parhistory$PREF_benthclar_herb[ROW_TO_USE]

preference_matrix_input_NEW$carnzoo[which(rownames(preference_matrix_input_NEW)=="omnivzoo")] <- parhistory$PREF_herb_carn[ROW_TO_USE]
preference_matrix_input_NEW$carnzoo[which(rownames(preference_matrix_input_NEW)=="benthslar")] <- parhistory$PREF_benthslar_carn[ROW_TO_USE]
preference_matrix_input_NEW$carnzoo[which(rownames(preference_matrix_input_NEW)=="benthclar")] <- parhistory$PREF_benthclar_carn[ROW_TO_USE]
preference_matrix_input_NEW$carnzoo[which(rownames(preference_matrix_input_NEW)=="fishplar")] <- parhistory$PREF_fishplar_carn[ROW_TO_USE]
preference_matrix_input_NEW$carnzoo[which(rownames(preference_matrix_input_NEW)=="fishdlar")] <- parhistory$PREF_fishdlar_carn[ROW_TO_USE]

preference_matrix_input_NEW$fishplar[which(rownames(preference_matrix_input_NEW)=="omnivzoo")] <- parhistory$PREF_herb_fishplar[ROW_TO_USE]   
preference_matrix_input_NEW$fishplar[which(rownames(preference_matrix_input_NEW)=="benthslar")] <- parhistory$PREF_benthslar_fishplar[ROW_TO_USE]
preference_matrix_input_NEW$fishplar[which(rownames(preference_matrix_input_NEW)=="benthclar")] <- parhistory$PREF_benthclar_fishplar[ROW_TO_USE]
               
preference_matrix_input_NEW$fishp[which(rownames(preference_matrix_input_NEW)=="omnivzoo")] <- parhistory$PREF_herb_fishp[ROW_TO_USE] 
preference_matrix_input_NEW$fishp[which(rownames(preference_matrix_input_NEW)=="carnzoo")] <- parhistory$PREF_carn_fishp[ROW_TO_USE]
preference_matrix_input_NEW$fishp[which(rownames(preference_matrix_input_NEW)=="benthslar")] <- parhistory$PREF_benthslar_fishp[ROW_TO_USE]
preference_matrix_input_NEW$fishp[which(rownames(preference_matrix_input_NEW)=="benthclar")] <- parhistory$PREF_benthclar_fishp[ROW_TO_USE]
preference_matrix_input_NEW$fishp[which(rownames(preference_matrix_input_NEW)=="fishdlar")] <- parhistory$PREF_fishdlar_fishp[ROW_TO_USE]
preference_matrix_input_NEW$fishp[which(rownames(preference_matrix_input_NEW)=="fishplar")] <- parhistory$PREF_fishplar_fishp[ROW_TO_USE]

preference_matrix_input_NEW$fishm[which(rownames(preference_matrix_input_NEW)=="omnivzoo")] <- parhistory$PREF_herb_fishm[ROW_TO_USE]  
preference_matrix_input_NEW$fishm[which(rownames(preference_matrix_input_NEW)=="carnzoo")] <- parhistory$PREF_carn_fishm[ROW_TO_USE]
preference_matrix_input_NEW$fishm[which(rownames(preference_matrix_input_NEW)=="benthslar")] <- parhistory$PREF_benthslar_fishm[ROW_TO_USE]
preference_matrix_input_NEW$fishm[which(rownames(preference_matrix_input_NEW)=="benthclar")] <- parhistory$PREF_benthclar_fishm[ROW_TO_USE]
preference_matrix_input_NEW$fishm[which(rownames(preference_matrix_input_NEW)=="fishdlar")] <- parhistory$PREF_fishdlar_fishm[ROW_TO_USE]
preference_matrix_input_NEW$fishm[which(rownames(preference_matrix_input_NEW)=="fishplar")] <- parhistory$PREF_fishplar_fishm[ROW_TO_USE]

preference_matrix_input_NEW$fishdlar[which(rownames(preference_matrix_input_NEW)=="omnivzoo")] <- parhistory$PREF_herb_fishdlar[ROW_TO_USE]      
preference_matrix_input_NEW$fishdlar[which(rownames(preference_matrix_input_NEW)=="benthslar")] <- parhistory$PREF_benthslar_fishdlar[ROW_TO_USE]
preference_matrix_input_NEW$fishdlar[which(rownames(preference_matrix_input_NEW)=="benthclar")] <- parhistory$PREF_benthclar_fishdlar[ROW_TO_USE]

preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="carnzoo")] <- parhistory$PREF_carn_fishd[ROW_TO_USE]
preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="benths")] <- parhistory$PREF_benths_fishd[ROW_TO_USE]
preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="benthc")] <- parhistory$PREF_benthc_fishd[ROW_TO_USE]
preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="fishplar")] <- parhistory$PREF_fishplar_fishd[ROW_TO_USE]
preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="fishdlar")] <- parhistory$PREF_fishdlar_fishd[ROW_TO_USE]
preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="fishp")] <- parhistory$PREF_fishp_fishd[ROW_TO_USE]
preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="fishm")] <- parhistory$PREF_fishm_fishd[ROW_TO_USE]
preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="fishd")] <- parhistory$PREF_fishd_fishd[ROW_TO_USE]
preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="discards")] <- parhistory$PREF_disc_fishd[ROW_TO_USE]
preference_matrix_input_NEW$fishd[which(rownames(preference_matrix_input_NEW)=="corpses")] <- parhistory$PREF_corp_fishd[ROW_TO_USE]

preference_matrix_input_NEW$benthslar[which(rownames(preference_matrix_input_NEW)=="phyt")] <- parhistory$PREF_phyt_benthslar[ROW_TO_USE]
preference_matrix_input_NEW$benthclar[which(rownames(preference_matrix_input_NEW)=="phyt")] <- parhistory$PREF_phyt_benthclar[ROW_TO_USE]
preference_matrix_input_NEW$benthslar[which(rownames(preference_matrix_input_NEW)=="suspdet")] <- parhistory$PREF_det_benthslar[ROW_TO_USE]
preference_matrix_input_NEW$benthclar[which(rownames(preference_matrix_input_NEW)=="suspdet")] <- parhistory$PREF_det_benthclar[ROW_TO_USE]


preference_matrix_input_NEW$benths[which(rownames(preference_matrix_input_NEW)=="phyt")] <- parhistory$PREF_phyt_benths[ROW_TO_USE]
preference_matrix_input_NEW$benths[which(rownames(preference_matrix_input_NEW)=="suspdet")] <- parhistory$PREF_det_benths[ROW_TO_USE]
preference_matrix_input_NEW$benths[which(rownames(preference_matrix_input_NEW)=="seddet")] <- parhistory$PREF_sed_benths[ROW_TO_USE]

preference_matrix_input_NEW$benthc[which(rownames(preference_matrix_input_NEW)=="kelp")] <- parhistory$PREF_kelp_benthc[ROW_TO_USE]
preference_matrix_input_NEW$benthc[which(rownames(preference_matrix_input_NEW)=="kelpdebris")] <- parhistory$PREF_kelpdebris_benthc[ROW_TO_USE]
preference_matrix_input_NEW$benthc[which(rownames(preference_matrix_input_NEW)=="benths")] <- parhistory$PREF_benths_benthc[ROW_TO_USE]
preference_matrix_input_NEW$benthc[which(rownames(preference_matrix_input_NEW)=="corpses")] <- parhistory$PREF_corp_benthc[ROW_TO_USE]

#preference_matrix_input_NEW$bird[which(rownames(preference_matrix_input_NEW)=="omnivzoo")] <- parhistory$PREF_herb_bird[ROW_TO_USE]
preference_matrix_input_NEW$bird[which(rownames(preference_matrix_input_NEW)=="carnzoo")] <- parhistory$PREF_carn_bird[ROW_TO_USE]
preference_matrix_input_NEW$bird[which(rownames(preference_matrix_input_NEW)=="benths")] <- parhistory$PREF_benths_bird[ROW_TO_USE]
preference_matrix_input_NEW$bird[which(rownames(preference_matrix_input_NEW)=="benthc")] <- parhistory$PREF_benthc_bird[ROW_TO_USE]
preference_matrix_input_NEW$bird[which(rownames(preference_matrix_input_NEW)=="fishp")] <- parhistory$PREF_fishp_bird[ROW_TO_USE]
preference_matrix_input_NEW$bird[which(rownames(preference_matrix_input_NEW)=="fishm")] <- parhistory$PREF_fishm_bird[ROW_TO_USE]
preference_matrix_input_NEW$bird[which(rownames(preference_matrix_input_NEW)=="fishd")] <- parhistory$PREF_fishd_bird[ROW_TO_USE]
preference_matrix_input_NEW$bird[which(rownames(preference_matrix_input_NEW)=="discards")] <- parhistory$PREF_disc_bird[ROW_TO_USE]
preference_matrix_input_NEW$bird[which(rownames(preference_matrix_input_NEW)=="corpses")] <- parhistory$PREF_corp_bird[ROW_TO_USE]

#preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="omnivzoo")] <- parhistory$PREF_herb_seal[ROW_TO_USE]
preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="carnzoo")] <- parhistory$PREF_carn_seal[ROW_TO_USE]
preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="benths")] <- parhistory$PREF_benths_seal[ROW_TO_USE]
preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="benthc")] <- parhistory$PREF_benthc_seal[ROW_TO_USE]
preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="fishp")] <- parhistory$PREF_fishp_seal[ROW_TO_USE]
preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="fishm")] <- parhistory$PREF_fishm_seal[ROW_TO_USE]
preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="fishd")] <- parhistory$PREF_fishd_seal[ROW_TO_USE]
preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="discards")] <- parhistory$PREF_disc_seal[ROW_TO_USE]
preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="corpses")] <- parhistory$PREF_corp_seal[ROW_TO_USE]
preference_matrix_input_NEW$seal[which(rownames(preference_matrix_input_NEW)=="bird")] <- parhistory$PREF_bird_seal[ROW_TO_USE]

preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="omnivzoo")] <- parhistory$PREF_herb_ceta[ROW_TO_USE]
preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="carnzoo")] <- parhistory$PREF_carn_ceta[ROW_TO_USE]
preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="benths")] <- parhistory$PREF_benths_ceta[ROW_TO_USE]
preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="benthc")] <- parhistory$PREF_benthc_ceta[ROW_TO_USE]
preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="fishp")] <- parhistory$PREF_fishp_ceta[ROW_TO_USE]
preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="fishm")] <- parhistory$PREF_fishm_ceta[ROW_TO_USE]
preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="fishd")] <- parhistory$PREF_fishd_ceta[ROW_TO_USE]
preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="discards")] <- parhistory$PREF_disc_ceta[ROW_TO_USE]
#preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="corpses")] <- parhistory$PREF_corp_ceta[ROW_TO_USE]
preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="bird")] <- parhistory$PREF_bird_ceta[ROW_TO_USE]
preference_matrix_input_NEW$ceta[which(rownames(preference_matrix_input_NEW)=="seal")] <- parhistory$PREF_seal_ceta[ROW_TO_USE]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Then we set the rate parameters for each predator at the reference temperature

uptake_mort_input_NEW$Cumax[which(uptake_mort_input_NEW$consumer=="kelp")] <- parhistory$uC_kelp[ROW_TO_USE]

uptake_mort_input_NEW$Cddexud[which(uptake_mort_input_NEW$consumer=="kelp")] <- parhistory$ddexudC_kelp[ROW_TO_USE]

uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="kelp")] <- parhistory$u_kelp[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="phyt_s")] <- parhistory$u_phyt[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="omnivzoo")] <- parhistory$u_herb[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="carnzoo")] <- parhistory$u_carn[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="fishplar")] <- parhistory$u_fishplar[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="fishp")] <- parhistory$u_fishp[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="fishm")] <- parhistory$u_fishm[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="fishdlar")] <- parhistory$u_fishdlar[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="fishd")] <- parhistory$u_fishd[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="benthslar")] <- parhistory$u_benthslar[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="benthclar")] <- parhistory$u_benthclar[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="benths")] <- parhistory$u_benths[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="benthc")] <- parhistory$u_benthc[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="bird")] <- parhistory$u_bird[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="seal")] <- parhistory$u_seal[ROW_TO_USE]
uptake_mort_input_NEW$Numax[which(uptake_mort_input_NEW$consumer=="ceta")] <- parhistory$u_ceta[ROW_TO_USE]

uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="kelp")] <- parhistory$h_kelp[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="phyt_s")] <- parhistory$h_phyt[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="omnivzoo")] <- parhistory$h_herb[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="carnzoo")] <- parhistory$h_carn[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="fishplar")] <- parhistory$h_fishplar[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="fishp")] <- parhistory$h_fishp[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="fishm")] <- parhistory$h_fishm[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="fishdlar")] <- parhistory$h_fishdlar[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="fishd")] <- parhistory$h_fishd[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="benthslar")] <- parhistory$h_benthslar[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="benthclar")] <- parhistory$h_benthclar[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="benths")] <- parhistory$h_benths[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="benthc")] <- parhistory$h_benthc[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="bird")] <- parhistory$h_bird[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="seal")] <- parhistory$h_seal[ROW_TO_USE]
uptake_mort_input_NEW$Nhsat[which(uptake_mort_input_NEW$consumer=="ceta")] <- parhistory$h_ceta[ROW_TO_USE]


uptake_mort_input_NEW$BdeApar[which(uptake_mort_input_NEW$consumer=="bird")] <- parhistory$bda_par_bird[ROW_TO_USE]
uptake_mort_input_NEW$BdeApar[which(uptake_mort_input_NEW$consumer=="seal")] <- parhistory$bda_par_seal[ROW_TO_USE]
uptake_mort_input_NEW$BdeApar[which(uptake_mort_input_NEW$consumer=="ceta")] <- parhistory$bda_par_ceta[ROW_TO_USE]



#Parameter for density dependent wave-mediated destruction of kelp
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="kelp")] <- parhistory$xxwave_kelp[ROW_TO_USE]

#Death rates of phytoplankton at the reference temperature
# IN THIS VERSION (NOV 2015) THESE PARAMATERS ARE APPLIED AS A DENSITY DEPENDENT RATE
# RATE RTHAN AS A DENSITY INDEPENDENT RATE AS IN THE ORIGINAL MODEL
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="phyt_s")] <- parhistory$xxst[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="phyt_d")] <- parhistory$xxdt[ROW_TO_USE]

#Death rate of carnivores fish birds and mammals per unit biomass - temperature independent
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="omnivzoo")] <- parhistory$xxherb[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="carnzoo")] <- parhistory$xxcarn[ROW_TO_USE]     
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="benthslar")] <- parhistory$xxbenthslar[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="benthclar")] <- parhistory$xxbenthclar[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="benths")] <- parhistory$xxbenths[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="benthc")] <- parhistory$xxbenthc[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="fishplar")] <- parhistory$xxpfishlar[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="fishdlar")] <- parhistory$xxdfishlar[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="fishp")] <- parhistory$xxpfish[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="fishm")] <- parhistory$xxmfish[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="fishd")] <- parhistory$xxdfish[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="bird")] <- parhistory$xxbird[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="seal")] <- parhistory$xxseal[ROW_TO_USE]
uptake_mort_input_NEW$ddmort[which(uptake_mort_input_NEW$consumer=="ceta")] <- parhistory$xxceta[ROW_TO_USE]

#Parameters for food gradient migration rates of pelagic and migratory fish
uptake_mort_input_NEW$migration_coef[which(uptake_mort_input_NEW$consumer=="fishp")] <- parhistory$xpfish_migcoef[ROW_TO_USE] 
uptake_mort_input_NEW$migration_coef[which(uptake_mort_input_NEW$consumer=="fishm")] <- parhistory$xmfish_migcoef[ROW_TO_USE]
uptake_mort_input_NEW$migration_coef[which(uptake_mort_input_NEW$consumer=="fishd")] <- parhistory$xdfish_migcoef[ROW_TO_USE]
uptake_mort_input_NEW$migration_coef[which(uptake_mort_input_NEW$consumer=="bird")] <- parhistory$xbird_migcoef[ROW_TO_USE]
uptake_mort_input_NEW$migration_coef[which(uptake_mort_input_NEW$consumer=="seal")] <- parhistory$xseal_migcoef[ROW_TO_USE]
uptake_mort_input_NEW$migration_coef[which(uptake_mort_input_NEW$consumer=="ceta")] <- parhistory$xceta_migcoef[ROW_TO_USE]


#Maximum proportions of the stock biomass which is accessible to the fisheries
#Units proportions
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="kelp")] <- parhistory$xmax_exploitable_f_KP[ROW_TO_USE]
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="fishp")] <- parhistory$xmax_exploitable_f_PF[ROW_TO_USE]
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="fishd")] <- parhistory$xmax_exploitable_f_DF[ROW_TO_USE]
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="fishm")] <- parhistory$xmax_exploitable_f_MF[ROW_TO_USE]
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="benths")] <- parhistory$xmax_exploitable_f_SB[ROW_TO_USE]
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="benthc")] <- parhistory$xmax_exploitable_f_CB[ROW_TO_USE]
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="carnzoo")] <- parhistory$xmax_exploitable_f_CZ[ROW_TO_USE]
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="bird")] <- parhistory$xmax_exploitable_f_BD[ROW_TO_USE]
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="seal")] <- parhistory$xmax_exploitable_f_SL[ROW_TO_USE]
uptake_mort_input_NEW$max_exploitable_f[which(uptake_mort_input_NEW$consumer=="ceta")] <- parhistory$xmax_exploitable_f_CT[ROW_TO_USE]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Mineralisation, nitrification and denitrification rates per day at the reference temperature
microbiology_input_NEW$Value[1] <- parhistory$xmt[ROW_TO_USE]
microbiology_input_NEW$Value[2] <- parhistory$xnst[ROW_TO_USE]
microbiology_input_NEW$Value[3] <- parhistory$xdst[ROW_TO_USE]
microbiology_input_NEW$Value[4] <- parhistory$xndt[ROW_TO_USE]
microbiology_input_NEW$Value[5] <- parhistory$xddt[ROW_TO_USE]

microbiology_input_NEW$Value[6] <- parhistory$xqs_p1[ROW_TO_USE]     # proportion of detritus which become refractory when minearlised
microbiology_input_NEW$Value[7] <- parhistory$xqs_p2[ROW_TO_USE]     # ratio of refratory to labile detritus minearalisation rates
microbiology_input_NEW$Value[8] <- parhistory$xqs_p3[ROW_TO_USE]     # proportion of refractory whiuch becomes labile when re-oxygenated

microbiology_input_NEW$Value[9] <- parhistory$xmsedt[ROW_TO_USE]
microbiology_input_NEW$Value[10] <- parhistory$xmsens[ROW_TO_USE]

microbiology_input_NEW$Value[11] <- parhistory$xnsedt[ROW_TO_USE]
microbiology_input_NEW$Value[12] <- parhistory$xnsens[ROW_TO_USE]

microbiology_input_NEW$Value[13] <- parhistory$xdsedt[ROW_TO_USE]
microbiology_input_NEW$Value[14] <- parhistory$xdsens[ROW_TO_USE]

#Proportion of discards sinking to become seabed corpses per day - temperature independent
microbiology_input_NEW$Value[15] <- parhistory$xdisc_corp[ROW_TO_USE]

#Proportion of corpse mass converted to detritus per day at the reference temperature
microbiology_input_NEW$Value[16] <- parhistory$xxcorp_det[ROW_TO_USE]


#Proportion of kelpdebric mass converted to detritus per day at the reference temperature
microbiology_input_NEW$Value[17] <- parhistory$xkelpdebris_det[ROW_TO_USE]



#Sinking rates and their dependence on mixing - temperature independent
microbiology_input_NEW$Value[18] <- parhistory$xdsink_s[ROW_TO_USE]
microbiology_input_NEW$Value[19] <- parhistory$xdsink_d[ROW_TO_USE]

microbiology_input_NEW$Value[20] <- parhistory$xkelpshade[ROW_TO_USE]

microbiology_input_NEW$Value[21] <- parhistory$xwave_kelpdebris[ROW_TO_USE]

#Fitting parameter for demersal discard rate - expect this to be about 1.0
microbiology_input_NEW$Value[22] <- parhistory$xdfdp[ROW_TO_USE]

	#End of export of parameters from parhistory into fitted parameter files
	#----------------------------------------------------------------------------

	#Save the new parameter files...
	parameterpath <- makepath(model.path, PARAMETERS_DIR)

	if(read.only==FALSE & csv.output==TRUE) {
	filename = csvname(parameterpath, "fitted_preference_matrix", identifier)
	writecsv(preference_matrix_input_NEW, filename, row.names=TRUE)
	print(paste("Writing preference matrix file :",filename))
	}

	if(read.only==FALSE & csv.output==TRUE) {
	filename = csvname(parameterpath, "fitted_uptake_mort_rates", identifier)
	writecsv(uptake_mort_input_NEW, filename, row.names=FALSE)
	print(paste("Writing uptake and mortality parameter file :",filename))
	}

	if(read.only==FALSE & csv.output==TRUE) {
	filename = csvname(parameterpath, "fitted_microbiology_others", identifier)
	writecsv(microbiology_input_NEW, filename, row.names=FALSE)
	print(paste("Writing microbiology parameter file :",filename))
	}

# Assemble the three new parameter objects into a list to be returned by the function

	new_parameter_set<-list(new_preference_matrix = preference_matrix_input_NEW,
				new_uptake_mort_rate_parameters = uptake_mort_input_NEW,
				new_microbiology_parameters = microbiology_input_NEW)

	new_parameter_set
}

