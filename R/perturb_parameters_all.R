#
# perturb_parameters_all.R
#
#' perturb parameters a bit
#'
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Subroutine to load and set the parameter values to be use din the model run
#'
#' Parameters which have been established by the simulated annealing scheme are
#' loaded from a csv file which is made from the last line of the 'accepted parameters'
#' file produced by the annealing programme.
#'
#' A variety of other parameters which are not optimized by simulated annealing are
#' hard-wired in this subroutine
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' In this bit of code the parameters can be varied by a random value drawn from a Gaussian distribution. The cv of the
#' distribution is constant across groups of parameters.
#' Intended for use in MCMC
#'
#' VECTORS BUILT UP:
#'	prefstore	~ 67 pars
#'	ustore		~ 13 pars
#'	hstore		~ 14 pars
#'	biogeostore	~ 18 pars
#'	mortstore	~ 14 pars
#'	reststore	~ 14 pars
#'
#' @param datastore parameters to be perturbed
#' @param annealing.parms annealing parameters
#'
#' @return perturbed parameter vector
#'
#' @noRd
#
# ------------------------------------------------------------------------------

perturb_parameters_all <- function(datastore, annealing.parms) {

	#Set the SDs for the different classes of parameters
	Prefsd		<- elt(annealing.parms, "Prefsd")	# Preference parameter sd
	u_sd		<- elt(annealing.parms, "u_sd")		# Maximum uptake rate sd
	h_sd		<- elt(annealing.parms, "h_sd")		# Half saturation density sd
	biogeo_sd	<- elt(annealing.parms, "biogeo_sd")	# microbial parameter sd
	mort_sd		<- elt(annealing.parms, "mort_sd")	# density dependent mortality rate sd
	ressd		<- elt(annealing.parms, "ressd")	# other parameters sd

	# Now pick out the stored parameters

	PREF_NIT_kelp			<- elt(datastore, "PREF_NIT_kelp")
	PREF_AMM_kelp			<- elt(datastore, "PREF_AMM_kelp")

	PREF_NIT_phyt	<- elt(datastore, "PREF_NIT_phyt")
	PREF_AMM_phyt	<- elt(datastore, "PREF_AMM_phyt")

	PREF_phyt_herb	<- elt(datastore, "PREF_phyt_herb")
	PREF_det_herb	<- elt(datastore, "PREF_det_herb")
	PREF_benthslar_herb	<- elt(datastore, "PREF_benthslar_herb")
	PREF_benthclar_herb	<- elt(datastore, "PREF_benthclar_herb")

	PREF_herb_carn	<- elt(datastore, "PREF_herb_carn")
	PREF_benthslar_carn	<- elt(datastore, "PREF_benthslar_carn")
	PREF_benthclar_carn	<- elt(datastore, "PREF_benthclar_carn")
	PREF_fishplar_carn	<- elt(datastore, "PREF_fishplar_carn")
	PREF_fishdlar_carn	<- elt(datastore, "PREF_fishdlar_carn")

	PREF_herb_fishplar	<- elt(datastore, "PREF_herb_fishplar")
	PREF_benthslar_fishplar	<- elt(datastore, "PREF_benthslar_fishplar")
	PREF_benthclar_fishplar	<- elt(datastore, "PREF_benthclar_fishplar")

	PREF_herb_fishp	<- elt(datastore, "PREF_herb_fishp")
	PREF_carn_fishp	<- elt(datastore, "PREF_carn_fishp")
	PREF_benthslar_fishp	<- elt(datastore, "PREF_benthslar_fishp")
	PREF_benthclar_fishp	<- elt(datastore, "PREF_benthclar_fishp")
	PREF_fishdlar_fishp	<- elt(datastore, "PREF_fishdlar_fishp")
	PREF_fishplar_fishp	<- elt(datastore, "PREF_fishplar_fishp")

	PREF_herb_fishm	<- elt(datastore, "PREF_herb_fishm")
	PREF_carn_fishm	<- elt(datastore, "PREF_carn_fishm")
	PREF_benthslar_fishm	<- elt(datastore, "PREF_benthslar_fishm")
	PREF_benthclar_fishm	<- elt(datastore, "PREF_benthclar_fishm")
	PREF_fishdlar_fishm	<- elt(datastore, "PREF_fishdlar_fishm")
	PREF_fishplar_fishm	<- elt(datastore, "PREF_fishplar_fishm")

	PREF_herb_fishdlar	<- elt(datastore, "PREF_herb_fishdlar")
	PREF_benthslar_fishdlar	<- elt(datastore, "PREF_benthslar_fishdlar")
	PREF_benthclar_fishdlar	<- elt(datastore, "PREF_benthclar_fishdlar")

	PREF_carn_fishd	<- elt(datastore, "PREF_carn_fishd")
	PREF_benths_fishd	<- elt(datastore, "PREF_benths_fishd")
	PREF_benthc_fishd	<- elt(datastore, "PREF_benthc_fishd")
	PREF_fishplar_fishd	<- elt(datastore, "PREF_fishplar_fishd")
	PREF_fishdlar_fishd	<- elt(datastore, "PREF_fishdlar_fishd")
	PREF_fishp_fishd	<- elt(datastore, "PREF_fishp_fishd")
	PREF_fishm_fishd	<- elt(datastore, "PREF_fishm_fishd")
	PREF_fishd_fishd	<- elt(datastore, "PREF_fishd_fishd")
	PREF_disc_fishd	<- elt(datastore, "PREF_disc_fishd")
	PREF_corp_fishd	<- elt(datastore, "PREF_corp_fishd")

	PREF_phyt_benthslar	<- elt(datastore, "PREF_phyt_benthslar")
	PREF_phyt_benthclar	<- elt(datastore, "PREF_phyt_benthclar")
	PREF_det_benthslar	<- elt(datastore, "PREF_det_benthslar")
	PREF_det_benthclar	<- elt(datastore, "PREF_det_benthclar")


	PREF_phyt_benths	<- elt(datastore, "PREF_phyt_benths")
	PREF_det_benths	<- elt(datastore, "PREF_det_benths")
	PREF_sed_benths	<- elt(datastore, "PREF_sed_benths")

	PREF_kelp_benthc	<- elt(datastore, "PREF_kelp_benthc")
	PREF_kelpdebris_benthc	<- elt(datastore, "PREF_kelpdebris_benthc")
	PREF_benths_benthc	<- elt(datastore, "PREF_benths_benthc")
	PREF_corp_benthc	<- elt(datastore, "PREF_corp_benthc")

	PREF_carn_bird	<- elt(datastore, "PREF_carn_bird")
	PREF_benths_bird	<- elt(datastore, "PREF_benths_bird")
	PREF_benthc_bird	<- elt(datastore, "PREF_benthc_bird")

	PREF_fishp_bird	<- elt(datastore, "PREF_fishp_bird")
	PREF_fishm_bird	<- elt(datastore, "PREF_fishm_bird")
	PREF_fishd_bird	<- elt(datastore, "PREF_fishd_bird")
	PREF_disc_bird	<- elt(datastore, "PREF_disc_bird")
	PREF_corp_bird	<- elt(datastore, "PREF_corp_bird")


	PREF_carn_seal	<- elt(datastore, "PREF_carn_seal")
	PREF_benths_seal	<- elt(datastore, "PREF_benths_seal")
	PREF_benthc_seal	<- elt(datastore, "PREF_benthc_seal")

	PREF_fishp_seal	<- elt(datastore, "PREF_fishp_seal")
	PREF_fishm_seal	<- elt(datastore, "PREF_fishm_seal")
	PREF_fishd_seal	<- elt(datastore, "PREF_fishd_seal")
	PREF_bird_seal	<- elt(datastore, "PREF_bird_seal")
	PREF_disc_seal	<- elt(datastore, "PREF_disc_seal")
	PREF_corp_seal	<- elt(datastore, "PREF_corp_seal")


	PREF_herb_ceta	<- elt(datastore, "PREF_herb_ceta")
	PREF_carn_ceta	<- elt(datastore, "PREF_carn_ceta")
	PREF_benths_ceta	<- elt(datastore, "PREF_benths_ceta")
	PREF_benthc_ceta	<- elt(datastore, "PREF_benthc_ceta")

	PREF_fishp_ceta	<- elt(datastore, "PREF_fishp_ceta")
	PREF_fishm_ceta	<- elt(datastore, "PREF_fishm_ceta")
	PREF_fishd_ceta	<- elt(datastore, "PREF_fishd_ceta")
	PREF_bird_ceta	<- elt(datastore, "PREF_bird_ceta")
	PREF_seal_ceta	<- elt(datastore, "PREF_seal_ceta")
	PREF_disc_ceta	<- elt(datastore, "PREF_disc_ceta")

	uC_kelp	<- elt(datastore, "uC_kelp")

	ddexudC_kelp	<- elt(datastore, "ddexudC_kelp")

	#Then we set the rate parameters for each predator at the reference temperature

	u_kelp	<- elt(datastore, "u_kelp")

	u_phyt	<- elt(datastore, "u_phyt")
	u_herb	<- elt(datastore, "u_herb")
	u_carn	<- elt(datastore, "u_carn")
	u_fishplar	<- elt(datastore, "u_fishplar")
	u_fishp	<- elt(datastore, "u_fishp")
	u_fishm	<- elt(datastore, "u_fishm")
	u_fishdlar	<- elt(datastore, "u_fishdlar")
	u_fishd	<- elt(datastore, "u_fishd")
	u_benthslar	<- elt(datastore, "u_benthslar")
	u_benthclar	<- elt(datastore, "u_benthclar")
	u_benths	<- elt(datastore, "u_benths")
	u_benthc	<- elt(datastore, "u_benthc")
	u_bird	<- elt(datastore, "u_bird")

	u_seal	<- elt(datastore, "u_seal")
	u_ceta	<- elt(datastore, "u_ceta")

	h_kelp	<- elt(datastore, "h_kelp")

	h_phyt	<- elt(datastore, "h_phyt")
	h_herb	<- elt(datastore, "h_herb")
	h_carn	<- elt(datastore, "h_carn")
	h_fishplar	<- elt(datastore, "h_fishplar")
	h_fishp	<- elt(datastore, "h_fishp")
	h_fishm	<- elt(datastore, "h_fishm")
	h_fishdlar	<- elt(datastore, "h_fishdlar")
	h_fishd	<- elt(datastore, "h_fishd")
	h_benthslar	<- elt(datastore, "h_benthslar")
	h_benthclar	<- elt(datastore, "h_benthclar")
	h_benths	<- elt(datastore, "h_benths")
	h_benthc	<- elt(datastore, "h_benthc")
	h_bird	<- elt(datastore, "h_bird")

	h_seal	<- elt(datastore, "h_seal")
	h_ceta	<- elt(datastore, "h_ceta")

	bda_par_bird	<- elt(datastore, "bda_par_bird")

	bda_par_seal	<- elt(datastore, "bda_par_seal")
	bda_par_ceta	<- elt(datastore, "bda_par_ceta")


	#Mineralisation, nitrification and denitrification rates per day at the reference temperature
	xmt	<- elt(datastore, "xmt")
	xnst	<- elt(datastore, "xnst")
	xdst	<- elt(datastore, "xdst")
	xndt	<- elt(datastore, "xndt")
	xddt	<- elt(datastore, "xddt")

	xqs_p1	<- elt(datastore, "xqs_p1")
	xqs_p2	<- elt(datastore, "xqs_p2")
	xqs_p3	<- elt(datastore, "xqs_p3")

	xmsedt	<- elt(datastore, "xmsedt")
	xmsens	<- elt(datastore, "xmsens")

	xnsedt	<- elt(datastore, "xnsedt")
	xnsens	<- elt(datastore, "xnsens")

	xdsedt	<- elt(datastore, "xdsedt")
	xdsens	<- elt(datastore, "xdsens")


	xxwave_kelp	<- elt(datastore, "xxwave_kelp")

	#Death rates of phytoplankton at the reference temperature
	xxst	<- elt(datastore, "xxst")
	xxdt	<- elt(datastore, "xxdt")

	#Death rate of carnivores fish birds and mammals per unit biomass - temperature independent
	xxherb	<- elt(datastore, "xxherb")
	xxcarn	<- elt(datastore, "xxcarn")
	xxbenthslar	<- elt(datastore, "xxbenthslar")
	xxbenthclar	<- elt(datastore, "xxbenthclar")
	xxbenths	<- elt(datastore, "xxbenths")
	xxbenthc	<- elt(datastore, "xxbenthc")
	xxpfishlar	<- elt(datastore, "xxpfishlar")
	xxdfishlar	<- elt(datastore, "xxdfishlar")
	xxpfish	<- elt(datastore, "xxpfish")
	xxmfish	<- elt(datastore, "xxmfish")
	xxdfish	<- elt(datastore, "xxdfish")
	xxbird	<- elt(datastore, "xxbird")

	xxseal	<- elt(datastore, "xxseal")
	xxceta	<- elt(datastore, "xxceta")


	#Proportion of corpse mass converted to detritus per day at the reference temperature
	xxcorp_det	<- elt(datastore, "xxcorp_det")

	#Proportion of discards sinking to become seabed corpses per day - temperature independent
	xdisc_corp	<- elt(datastore, "xdisc_corp")


	#Proportion of corpse mass converted to detritus per day at the reference temperature
	xkelpdebris_det	<- elt(datastore, "xkelpdebris_det")

	#Sinking rates and their dependence on mixing - temperature independent
	xdsink_s	<- elt(datastore, "xdsink_s")
	#xdsink_d_Klow	<- elt(datastore, "xdsink_d_Klow")
	xdsink_d	<- elt(datastore, "xdsink_d")

	#Density dependent self shading parameter for kelp")
	xkelpshade	<- elt(datastore, "xkelpshade")

	#Wave dependent beach-cast paramater fpor kelp debris
	xwave_kelpdebris	<- elt(datastore, "xwave_kelpdebris")

	#Fitting parameter for scaling between whole model region demersal fish N mass and the survey
	#index on which the empirical relationships for pNQ and dsacrad ates of Q and NQ species are based.
	#Expect this to be abouut 2
	xdfdp	<- elt(datastore, "xdfdp")


	#Parameters for food gradient migration rates of pelagic and migratory fish
	xpfish_migcoef		<- elt(datastore, "xpfish_migcoef")
	xmfish_migcoef		<- elt(datastore, "xmfish_migcoef")
	xdfish_migcoef		<- elt(datastore, "xdfish_migcoef")
	xbird_migcoef		<- elt(datastore, "xbird_migcoef")

	xseal_migcoef		<- elt(datastore, "xseal_migcoef")
	xceta_migcoef		<- elt(datastore, "xceta_migcoef")

	#Maximum proportions of the stock biomass which is accessible to the fisheries
	#Units proportions

	xmax_exploitable_f_KP	<- elt(datastore, "xmax_exploitable_f_KP")

	xmax_exploitable_f_PF	<- elt(datastore, "xmax_exploitable_f_PF")
	xmax_exploitable_f_DF	<- elt(datastore, "xmax_exploitable_f_DF")
	xmax_exploitable_f_MF	<- elt(datastore, "xmax_exploitable_f_MF")
	xmax_exploitable_f_SB	<- elt(datastore, "xmax_exploitable_f_SB")
	xmax_exploitable_f_CB	<- elt(datastore, "xmax_exploitable_f_CB")
	xmax_exploitable_f_CZ	<- elt(datastore, "xmax_exploitable_f_CZ")
	xmax_exploitable_f_BD	<- elt(datastore, "xmax_exploitable_f_BD")

	xmax_exploitable_f_SL	<- elt(datastore, "xmax_exploitable_f_SL")
	xmax_exploitable_f_CT	<- elt(datastore, "xmax_exploitable_f_CT")

	annual_obj	<- elt(datastore, "annual_obj")

#----------------------------------------------------------------------------

#Now randomise the parameters

#First we set up the preferences

if(Prefsd>0){

PREF_NIT_kelpx<-max(0,runif(1,(1-Prefsd)*PREF_NIT_kelp,(1+Prefsd)*PREF_NIT_kelp))
PREF_AMM_kelpx<-max(0,runif(1,(1-Prefsd)*PREF_AMM_kelp,(1+Prefsd)*PREF_AMM_kelp))
#Renormalise
prefsum<-PREF_NIT_kelpx+PREF_AMM_kelpx
PREF_NIT_kelp<-PREF_NIT_kelpx/prefsum
PREF_AMM_kelp<-PREF_AMM_kelpx/prefsum


PREF_NIT_phytx<-max(0,runif(1,(1-Prefsd)*PREF_NIT_phyt,(1+Prefsd)*PREF_NIT_phyt))
PREF_AMM_phytx<-max(0,runif(1,(1-Prefsd)*PREF_AMM_phyt,(1+Prefsd)*PREF_AMM_phyt))
#Renormalise
prefsum<-PREF_NIT_phytx+PREF_AMM_phytx
PREF_NIT_phyt<-PREF_NIT_phytx/prefsum
PREF_AMM_phyt<-PREF_AMM_phytx/prefsum

PREF_phyt_herbx<-max(0,runif(1,(1-Prefsd)*PREF_phyt_herb,(1+Prefsd)*PREF_phyt_herb))
PREF_det_herbx<-max(0,runif(1,(1-Prefsd)*PREF_det_herb,(1+Prefsd)*PREF_det_herb))
PREF_benthslar_herbx<-max(0,runif(1,(1-Prefsd)*PREF_benthslar_herb,(1+Prefsd)*PREF_benthslar_herb))
PREF_benthclar_herbx<-max(0,runif(1,(1-Prefsd)*PREF_benthclar_herb,(1+Prefsd)*PREF_benthclar_herb))
#Renormalise
prefsum<-PREF_phyt_herbx+PREF_det_herbx+PREF_benthslar_herbx+PREF_benthclar_herbx
PREF_phyt_herb<-PREF_phyt_herbx/prefsum
PREF_det_herb<-PREF_det_herbx/prefsum
PREF_benthslar_herb<-PREF_benthslar_herbx/prefsum
PREF_benthclar_herb<-PREF_benthclar_herbx/prefsum

PREF_herb_carnx<-max(0,runif(1,(1-Prefsd)*PREF_herb_carn,(1+Prefsd)*PREF_herb_carn))
PREF_fishplar_carnx<-max(0,runif(1,(1-Prefsd)*PREF_fishplar_carn,(1+Prefsd)*PREF_fishplar_carn))
PREF_fishdlar_carnx<-max(0,runif(1,(1-Prefsd)*PREF_fishdlar_carn,(1+Prefsd)*PREF_fishdlar_carn))
PREF_benthslar_carnx<-max(0,runif(1,(1-Prefsd)*PREF_benthslar_carn,(1+Prefsd)*PREF_benthslar_carn))
PREF_benthclar_carnx<-max(0,runif(1,(1-Prefsd)*PREF_benthclar_carn,(1+Prefsd)*PREF_benthclar_carn))
#Renormalise
prefsum<-PREF_herb_carnx+PREF_fishplar_carnx+PREF_fishdlar_carnx+PREF_benthslar_carnx+PREF_benthclar_carnx
PREF_herb_carn<-PREF_herb_carnx/prefsum
PREF_fishplar_carn<-PREF_fishplar_carnx/prefsum
PREF_fishdlar_carn<-PREF_fishdlar_carnx/prefsum
PREF_benthslar_carn<-PREF_benthslar_carnx/prefsum
PREF_benthclar_carn<-PREF_benthclar_carnx/prefsum


PREF_herb_fishplarx<-max(0,runif(1,(1-Prefsd)*PREF_herb_fishplar,(1+Prefsd)*PREF_herb_fishplar))
PREF_benthslar_fishplarx<-max(0,runif(1,(1-Prefsd)*PREF_benthslar_fishplar,(1+Prefsd)*PREF_benthslar_fishplar))
PREF_benthclar_fishplarx<-max(0,runif(1,(1-Prefsd)*PREF_benthclar_fishplar,(1+Prefsd)*PREF_benthclar_fishplar))
#Renormalise
prefsum<-PREF_herb_fishplarx+PREF_benthslar_fishplarx+PREF_benthclar_fishplarx
PREF_herb_fishplar<-PREF_herb_fishplarx/prefsum
PREF_benthslar_fishplar<-PREF_benthslar_fishplarx/prefsum
PREF_benthclar_fishplar<-PREF_benthclar_fishplarx/prefsum

PREF_herb_fishpx<-max(0,runif(1,(1-Prefsd)*PREF_herb_fishp,(1+Prefsd)*PREF_herb_fishp))
PREF_carn_fishpx<-max(0,runif(1,(1-Prefsd)*PREF_carn_fishp,(1+Prefsd)*PREF_carn_fishp))
PREF_fishdlar_fishpx<-max(0,runif(1,(1-Prefsd)*PREF_fishdlar_fishp,(1+Prefsd)*PREF_fishdlar_fishp))
PREF_fishplar_fishpx<-max(0,runif(1,(1-Prefsd)*PREF_fishplar_fishp,(1+Prefsd)*PREF_fishplar_fishp))
PREF_benthslar_fishpx<-max(0,runif(1,(1-Prefsd)*PREF_benthslar_fishp,(1+Prefsd)*PREF_benthslar_fishp))
PREF_benthclar_fishpx<-max(0,runif(1,(1-Prefsd)*PREF_benthclar_fishp,(1+Prefsd)*PREF_benthclar_fishp))
#Renormalise
prefsum<-PREF_herb_fishpx+PREF_carn_fishpx+PREF_fishdlar_fishpx+PREF_fishplar_fishpx+PREF_benthslar_fishpx+PREF_benthclar_fishpx
PREF_herb_fishp<-PREF_herb_fishpx/prefsum
PREF_carn_fishp<-PREF_carn_fishpx/prefsum
PREF_fishdlar_fishp<-PREF_fishdlar_fishpx/prefsum
PREF_fishplar_fishp<-PREF_fishplar_fishpx/prefsum
PREF_benthslar_fishp<-PREF_benthslar_fishpx/prefsum
PREF_benthclar_fishp<-PREF_benthclar_fishpx/prefsum


PREF_herb_fishmx<-max(0,runif(1,(1-Prefsd)*PREF_herb_fishm,(1+Prefsd)*PREF_herb_fishm)) 
PREF_carn_fishmx<-max(0,runif(1,(1-Prefsd)*PREF_carn_fishm,(1+Prefsd)*PREF_carn_fishm)) 
PREF_fishdlar_fishmx<-max(0,runif(1,(1-Prefsd)*PREF_fishdlar_fishm,(1+Prefsd)*PREF_fishdlar_fishm)) 
PREF_fishplar_fishmx<-max(0,runif(1,(1-Prefsd)*PREF_fishplar_fishm,(1+Prefsd)*PREF_fishplar_fishm)) 
PREF_benthslar_fishmx<-max(0,runif(1,(1-Prefsd)*PREF_benthslar_fishm,(1+Prefsd)*PREF_benthslar_fishm)) 
PREF_benthclar_fishmx<-max(0,runif(1,(1-Prefsd)*PREF_benthclar_fishm,(1+Prefsd)*PREF_benthclar_fishm)) 
#Renormalise
prefsum<-PREF_herb_fishmx+PREF_carn_fishmx+PREF_benthslar_fishmx+PREF_benthclar_fishmx+PREF_fishdlar_fishmx+PREF_fishplar_fishmx
PREF_herb_fishm<-PREF_herb_fishmx/prefsum
PREF_carn_fishm<-PREF_carn_fishmx/prefsum
PREF_fishdlar_fishm<-PREF_fishdlar_fishmx/prefsum
PREF_fishplar_fishm<-PREF_fishplar_fishmx/prefsum
PREF_benthslar_fishm<-PREF_benthslar_fishmx/prefsum
PREF_benthclar_fishm<-PREF_benthclar_fishmx/prefsum


PREF_herb_fishdlarx<-max(0,runif(1,(1-Prefsd)*PREF_herb_fishdlar,(1+Prefsd)*PREF_herb_fishdlar))
PREF_benthslar_fishdlarx<-max(0,runif(1,(1-Prefsd)*PREF_benthslar_fishdlar,(1+Prefsd)*PREF_benthslar_fishdlar))
PREF_benthclar_fishdlarx<-max(0,runif(1,(1-Prefsd)*PREF_benthclar_fishdlar,(1+Prefsd)*PREF_benthclar_fishdlar))
#Renormalise
prefsum<-PREF_herb_fishdlarx+PREF_benthslar_fishdlarx+PREF_benthclar_fishdlarx
PREF_herb_fishdlar<-PREF_herb_fishdlarx/prefsum
PREF_benthslar_fishdlar<-PREF_benthslar_fishdlarx/prefsum
PREF_benthclar_fishdlar<-PREF_benthclar_fishdlarx/prefsum

PREF_carn_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_carn_fishd,(1+Prefsd)*PREF_carn_fishd))
PREF_benths_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_benths_fishd,(1+Prefsd)*PREF_benths_fishd))
PREF_benthc_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_benthc_fishd,(1+Prefsd)*PREF_benthc_fishd))
PREF_fishplar_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_fishplar_fishd,(1+Prefsd)*PREF_fishplar_fishd))
PREF_fishdlar_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_fishdlar_fishd,(1+Prefsd)*PREF_fishdlar_fishd))
PREF_fishp_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_fishp_fishd,(1+Prefsd)*PREF_fishp_fishd))
PREF_fishm_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_fishm_fishd,(1+Prefsd)*PREF_fishm_fishd))
PREF_fishd_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_fishd_fishd,(1+Prefsd)*PREF_fishd_fishd))
PREF_disc_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_disc_fishd,(1+Prefsd)*PREF_disc_fishd))
PREF_corp_fishdx<-max(0,runif(1,(1-Prefsd)*PREF_corp_fishd,(1+Prefsd)*PREF_corp_fishd))
#Renormalise
prefsum<-PREF_carn_fishdx+PREF_benths_fishdx+PREF_benthc_fishdx+PREF_fishplar_fishdx+PREF_fishdlar_fishdx+PREF_fishp_fishdx+PREF_fishm_fishdx+PREF_fishd_fishdx+PREF_disc_fishdx+PREF_corp_fishdx
PREF_carn_fishd<-PREF_carn_fishdx/prefsum
PREF_benths_fishd<-PREF_benths_fishdx/prefsum
PREF_benthc_fishd<-PREF_benthc_fishdx/prefsum
PREF_fishplar_fishd<-PREF_fishplar_fishdx/prefsum
PREF_fishdlar_fishd<-PREF_fishdlar_fishdx/prefsum
PREF_fishp_fishd<-PREF_fishp_fishdx/prefsum
PREF_fishm_fishd<-PREF_fishm_fishdx/prefsum
PREF_fishd_fishd<-PREF_fishd_fishdx/prefsum
PREF_disc_fishd<-PREF_disc_fishdx/prefsum
PREF_corp_fishd<-PREF_corp_fishdx/prefsum

PREF_phyt_benthslarx<-max(0,runif(1,(1-Prefsd)*PREF_phyt_benthslar,(1+Prefsd)*PREF_phyt_benthslar))
PREF_det_benthslarx<-max(0,runif(1,(1-Prefsd)*PREF_det_benthslar,(1+Prefsd)*PREF_det_benthslar))
#Renormalise
prefsum<-PREF_phyt_benthslarx+PREF_det_benthslarx
PREF_phyt_benthslar<-PREF_phyt_benthslarx/prefsum
PREF_det_benthslar<-PREF_det_benthslarx/prefsum

PREF_phyt_benthclarx<-max(0,runif(1,(1-Prefsd)*PREF_phyt_benthclar,(1+Prefsd)*PREF_phyt_benthclar))
PREF_det_benthclarx<-max(0,runif(1,(1-Prefsd)*PREF_det_benthclar,(1+Prefsd)*PREF_det_benthclar))
#Renormalise
prefsum<-PREF_phyt_benthclarx+PREF_det_benthclarx
PREF_phyt_benthclar<-PREF_phyt_benthclarx/prefsum
PREF_det_benthclar<-PREF_det_benthclarx/prefsum


PREF_phyt_benthsx<-max(0,runif(1,(1-Prefsd)*PREF_phyt_benths,(1+Prefsd)*PREF_phyt_benths))
PREF_det_benthsx<-max(0,runif(1,(1-Prefsd)*PREF_det_benths,(1+Prefsd)*PREF_det_benths))
PREF_sed_benthsx<-max(0,runif(1,(1-Prefsd)*PREF_sed_benths,(1+Prefsd)*PREF_sed_benths))
#Renormalise
prefsum<-PREF_phyt_benthsx+PREF_det_benthsx+PREF_sed_benthsx
PREF_phyt_benths<-PREF_phyt_benthsx/prefsum
PREF_det_benths<-PREF_det_benthsx/prefsum
PREF_sed_benths<-PREF_sed_benthsx/prefsum
  PREF_phyt_benths_lim<-0.25
  if(PREF_phyt_benths>PREF_phyt_benths_lim) {
       PREFdif<-PREF_phyt_benths - PREF_phyt_benths_lim
       PREF_phyt_benths<-PREF_phyt_benths_lim
       PREF_det_benths_t <- PREF_det_benths + (PREFdif * PREF_det_benths/(PREF_det_benths+PREF_sed_benths))
       PREF_sed_benths_t <- PREF_sed_benths + (PREFdif * PREF_sed_benths/(PREF_det_benths+PREF_sed_benths))
       PREF_det_benths<-PREF_det_benths_t
       PREF_sed_benths<-PREF_sed_benths_t}


PREF_kelp_benthcx<-max(0,runif(1,(1-Prefsd)*PREF_kelp_benthc,(1+Prefsd)*PREF_kelp_benthc))
PREF_kelpdebris_benthcx<-max(0,runif(1,(1-Prefsd)*PREF_kelpdebris_benthc,(1+Prefsd)*PREF_kelpdebris_benthc))
PREF_benths_benthcx<-max(0,runif(1,(1-Prefsd)*PREF_benths_benthc,(1+Prefsd)*PREF_benths_benthc))
PREF_corp_benthcx<-max(0,runif(1,(1-Prefsd)*PREF_corp_benthc,(1+Prefsd)*PREF_corp_benthc))
#Renormalise
prefsum<-PREF_benths_benthcx+PREF_corp_benthcx+PREF_kelp_benthcx+PREF_kelpdebris_benthcx
PREF_kelp_benthc<-PREF_kelp_benthcx/prefsum
PREF_kelpdebris_benthc<-PREF_kelpdebris_benthcx/prefsum
PREF_benths_benthc<-PREF_benths_benthcx/prefsum
PREF_corp_benthc<-PREF_corp_benthcx/prefsum
  PREF_kelp_benthc_lim<-0.05
  PREF_kelpdebris_benthc_lim<-0.01
  if(PREF_kelp_benthc>PREF_kelp_benthc_lim || PREF_kelpdebris_benthc>PREF_kelpdebris_benthc_lim) {
       PREFdif<-0
       if(PREF_kelp_benthc>PREF_kelp_benthc_lim){
       PREFdif<-PREFdif + (PREF_kelp_benthc - PREF_kelp_benthc_lim)
       PREF_kelp_benthc<-PREF_kelp_benthc_lim }

       if(PREF_kelpdebris_benthc>PREF_kelpdebris_benthc_lim){
       PREFdif<-PREFdif + (PREF_kelpdebris_benthc - PREF_kelpdebris_benthc_lim)
       PREF_kelpdebris_benthc<-PREF_kelpdebris_benthc_lim }

       PREF_benths_benthc_t <- PREF_benths_benthc + (PREFdif * PREF_benths_benthc/(PREF_benths_benthc+PREF_corp_benthc+PREF_kelp_benthc+PREF_kelpdebris_benthc))
       PREF_corp_benthc_t <- PREF_corp_benthc + (PREFdif * PREF_corp_benthc/(PREF_benths_benthc+PREF_corp_benthc+PREF_kelp_benthc+PREF_kelpdebris_benthc))
       PREF_benths_benthc<-PREF_benths_benthc_t
       PREF_corp_benthc<-PREF_corp_benthc_t}


PREF_carn_birdx<-max(0,runif(1,(1-Prefsd)*PREF_carn_bird,(1+Prefsd)*PREF_carn_bird))
PREF_benths_birdx<-max(0,runif(1,(1-Prefsd)*PREF_benths_bird,(1+Prefsd)*PREF_benths_bird))
PREF_benthc_birdx<-max(0,runif(1,(1-Prefsd)*PREF_benthc_bird,(1+Prefsd)*PREF_benthc_bird))
PREF_fishp_birdx<-max(0,runif(1,(1-Prefsd)*PREF_fishp_bird,(1+Prefsd)*PREF_fishp_bird))
PREF_fishm_birdx<-max(0,runif(1,(1-Prefsd)*PREF_fishm_bird,(1+Prefsd)*PREF_fishm_bird))
PREF_fishd_birdx<-max(0,runif(1,(1-Prefsd)*PREF_fishd_bird,(1+Prefsd)*PREF_fishd_bird))
PREF_disc_birdx<-max(0,rnorm(1,(1-Prefsd)*PREF_disc_bird,(1+Prefsd)*PREF_disc_bird))
PREF_corp_birdx<-max(0,rnorm(1,(1-Prefsd)*PREF_corp_bird,(1+Prefsd)*PREF_corp_bird))
#Renormalise
prefsum<-PREF_carn_birdx+PREF_benths_birdx+PREF_benthc_birdx+PREF_fishp_birdx+PREF_fishm_birdx+PREF_fishd_birdx+PREF_disc_birdx+PREF_corp_birdx
PREF_carn_bird<-PREF_carn_birdx/prefsum
PREF_benths_bird<-PREF_benths_birdx/prefsum
PREF_benthc_bird<-PREF_benthc_birdx/prefsum
PREF_fishp_bird<-PREF_fishp_birdx/prefsum
PREF_fishm_bird<-PREF_fishm_birdx/prefsum
PREF_fishd_bird<-PREF_fishd_birdx/prefsum
PREF_disc_bird<-PREF_disc_birdx/prefsum
PREF_corp_bird<-PREF_corp_birdx/prefsum



PREF_carn_sealx<-max(0,runif(1,(1-Prefsd)*PREF_carn_seal,Prefsd*PREF_carn_seal))
PREF_benths_sealx<-max(0,runif(1,(1-Prefsd)*PREF_benths_seal,(1+Prefsd)*PREF_benths_seal))
PREF_benthc_sealx<-max(0,runif(1,(1-Prefsd)*PREF_benthc_seal,(1+Prefsd)*PREF_benthc_seal))
PREF_fishp_sealx<-max(0,runif(1,(1-Prefsd)*PREF_fishp_seal,(1+Prefsd)*PREF_fishp_seal))
PREF_fishm_sealx<-max(0,runif(1,(1-Prefsd)*PREF_fishm_seal,(1+Prefsd)*PREF_fishm_seal))
PREF_fishd_sealx<-max(0,runif(1,(1-Prefsd)*PREF_fishd_seal,(1+Prefsd)*PREF_fishd_seal))
PREF_bird_sealx<-max(0,runif(1,(1-Prefsd)*PREF_bird_seal,(1+Prefsd)*PREF_bird_seal))
PREF_disc_sealx<-max(0,runif(1,(1-Prefsd)*PREF_disc_seal,(1+Prefsd)*PREF_disc_seal))
PREF_corp_sealx<-max(0,runif(1,(1-Prefsd)*PREF_corp_seal,(1+Prefsd)*PREF_corp_seal))
#Renormalise
prefsum<-PREF_carn_sealx+PREF_benths_sealx+PREF_benthc_sealx+PREF_fishp_sealx+PREF_fishm_sealx+PREF_fishd_sealx+PREF_bird_sealx+PREF_disc_sealx+PREF_corp_sealx
PREF_carn_seal<-PREF_carn_sealx/prefsum
PREF_benths_seal<-PREF_benths_sealx/prefsum
PREF_benthc_seal<-PREF_benthc_sealx/prefsum
PREF_fishp_seal<-PREF_fishp_sealx/prefsum
PREF_fishm_seal<-PREF_fishm_sealx/prefsum
PREF_fishd_seal<-PREF_fishd_sealx/prefsum
PREF_bird_seal<-PREF_bird_sealx/prefsum
PREF_disc_seal<-PREF_disc_sealx/prefsum
PREF_corp_seal<-PREF_corp_sealx/prefsum



PREF_herb_cetax<-max(0,runif(1,(1-Prefsd)*PREF_herb_ceta,(1+Prefsd)*PREF_herb_ceta))
PREF_carn_cetax<-max(0,runif(1,(1-Prefsd)*PREF_carn_ceta,(1+Prefsd)*PREF_carn_ceta))
PREF_benths_cetax<-max(0,runif(1,(1-Prefsd)*PREF_benths_ceta,(1+Prefsd)*PREF_benths_ceta))
PREF_benthc_cetax<-max(0,runif(1,(1-Prefsd)*PREF_benthc_ceta,(1+Prefsd)*PREF_benthc_ceta))
PREF_fishp_cetax<-max(0,runif(1,(1-Prefsd)*PREF_fishp_ceta,(1+Prefsd)*PREF_fishp_ceta))
PREF_fishm_cetax<-max(0,runif(1,(1-Prefsd)*PREF_fishm_ceta,(1+Prefsd)*PREF_fishm_ceta))
PREF_fishd_cetax<-max(0,runif(1,(1-Prefsd)*PREF_fishd_ceta,(1+Prefsd)*PREF_fishd_ceta))
PREF_bird_cetax<-max(0,runif(1,(1-Prefsd)*PREF_bird_ceta,(1+Prefsd)*PREF_bird_ceta))
PREF_seal_cetax<-max(0,runif(1,(1-Prefsd)*PREF_seal_ceta,(1+Prefsd)*PREF_seal_ceta))
PREF_disc_cetax<-max(0,runif(1,(1-Prefsd)*PREF_disc_ceta,(1+Prefsd)*PREF_disc_ceta))
#Renormalise
prefsum<-PREF_herb_cetax+PREF_carn_cetax+PREF_benths_cetax+PREF_benthc_cetax+PREF_fishp_cetax+PREF_fishm_cetax+PREF_fishd_cetax+PREF_bird_cetax+PREF_seal_cetax+PREF_disc_cetax
PREF_herb_ceta<-PREF_herb_cetax/prefsum
PREF_carn_ceta<-PREF_carn_cetax/prefsum
PREF_benths_ceta<-PREF_benths_cetax/prefsum
PREF_benthc_ceta<-PREF_benthc_cetax/prefsum
PREF_fishp_ceta<-PREF_fishp_cetax/prefsum
PREF_fishm_ceta<-PREF_fishm_cetax/prefsum
PREF_fishd_ceta<-PREF_fishd_cetax/prefsum
PREF_bird_ceta<-PREF_bird_cetax/prefsum
PREF_seal_ceta<-PREF_seal_cetax/prefsum
PREF_disc_ceta<-PREF_disc_cetax/prefsum

}

prefstore<-list(PREF_NIT_kelp,PREF_AMM_kelp,
             PREF_NIT_phyt,PREF_AMM_phyt,PREF_phyt_herb,PREF_det_herb,PREF_benthslar_herb,PREF_benthclar_herb,PREF_herb_carn,PREF_benthslar_carn,PREF_benthclar_carn,PREF_fishplar_carn,PREF_fishdlar_carn,
             PREF_herb_fishplar,PREF_benthslar_fishplar,PREF_benthclar_fishplar,
             PREF_herb_fishp,PREF_carn_fishp,PREF_benthslar_fishp,PREF_benthclar_fishp,PREF_fishdlar_fishp,PREF_fishplar_fishp,
             PREF_herb_fishm,PREF_carn_fishm,PREF_benthslar_fishm,PREF_benthclar_fishm,PREF_fishdlar_fishm,PREF_fishplar_fishm,
             PREF_herb_fishdlar,PREF_benthslar_fishdlar,PREF_benthclar_fishdlar,
             PREF_carn_fishd,PREF_benths_fishd,PREF_benthc_fishd,PREF_fishplar_fishd,PREF_fishdlar_fishd,PREF_fishp_fishd,PREF_fishm_fishd,PREF_fishd_fishd,PREF_disc_fishd,PREF_corp_fishd,
             PREF_phyt_benthslar,PREF_phyt_benthclar,
             PREF_det_benthslar,PREF_det_benthclar,
             PREF_phyt_benths,PREF_det_benths,PREF_sed_benths,
             PREF_kelp_benthc,PREF_kelpdebris_benthc,PREF_benths_benthc,PREF_corp_benthc,
             PREF_carn_bird,PREF_benths_bird,PREF_benthc_bird,PREF_fishp_bird,PREF_fishm_bird,PREF_fishd_bird,PREF_disc_bird,PREF_corp_bird,
             PREF_carn_seal,PREF_benths_seal,PREF_benthc_seal,PREF_fishp_seal,PREF_fishm_seal,PREF_fishd_seal,PREF_bird_seal,PREF_disc_seal,PREF_corp_seal,
             PREF_herb_ceta,PREF_carn_ceta,PREF_benths_ceta,PREF_benthc_ceta,PREF_fishp_ceta,PREF_fishm_ceta,PREF_fishd_ceta,PREF_bird_ceta,PREF_seal_ceta,PREF_disc_ceta)
names(prefstore)<-c("PREF_NIT_kelp","PREF_AMM_kelp",
            "PREF_NIT_phyt","PREF_AMM_phyt","PREF_phyt_herb","PREF_det_herb","PREF_benthslar_herb","PREF_benthclar_herb","PREF_herb_carn","PREF_benthslar_carn","PREF_benthclar_carn","PREF_fishplar_carn","PREF_fishdlar_carn",
            "PREF_herb_fishplar","PREF_benthslar_fishplar","PREF_benthclar_fishplar",
            "PREF_herb_fishp","PREF_carn_fishp","PREF_benthslar_fishp","PREF_benthclar_fishp","PREF_fishdlar_fishp","PREF_fishplar_fishp",
            "PREF_herb_fishm","PREF_carn_fishm","PREF_benthslar_fishm","PREF_benthclar_fishm","PREF_fishdlar_fishm","PREF_fishplar_fishm",
            "PREF_herb_fishdlar","PREF_benthslar_fishdlar","PREF_benthclar_fishdlar",
            "PREF_carn_fishd","PREF_benths_fishd","PREF_benthc_fishd","PREF_fishplar_fishd","PREF_fishdlar_fishd","PREF_fishp_fishd","PREF_fishm_fishd","PREF_fishd_fishd","PREF_disc_fishd","PREF_corp_fishd",
            "PREF_phyt_benthslar","PREF_phyt_benthclar",
            "PREF_det_benthslar","PREF_det_benthclar",
            "PREF_phyt_benths","PREF_det_benths","PREF_sed_benths",
            "PREF_kelp_benthc","PREF_kelpdebris_benthc","PREF_benths_benthc","PREF_corp_benthc",
            "PREF_carn_bird","PREF_benths_bird","PREF_benthc_bird","PREF_fishp_bird","PREF_fishm_bird","PREF_fishd_bird","PREF_disc_bird","PREF_corp_bird",
            "PREF_carn_seal","PREF_benths_seal","PREF_benthc_seal","PREF_fishp_seal","PREF_fishm_seal","PREF_fishd_seal","PREF_bird_seal","PREF_disc_seal","PREF_corp_seal",
            "PREF_herb_ceta","PREF_carn_ceta","PREF_benths_ceta","PREF_benthc_ceta","PREF_fishp_ceta","PREF_fishm_ceta","PREF_fishd_ceta","PREF_bird_ceta","PREF_seal_ceta","PREF_disc_ceta")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(u_sd>0){

uC_kelp<-max(0,runif(1,(1-u_sd)*uC_kelp,(1+u_sd)*uC_kelp))
ddexudC_kelp<-max(0,runif(1,(1-u_sd)*ddexudC_kelp,(1+u_sd)*ddexudC_kelp))
u_kelp<-max(0,runif(1,(1-u_sd)*u_kelp,(1+u_sd)*u_kelp))


u_phyt<-max(0,runif(1,(1-u_sd)*u_phyt,(1+u_sd)*u_phyt))
u_herb<-max(0,runif(1,(1-u_sd)*u_herb,(1+u_sd)*u_herb))
u_carn<-max(0,runif(1,(1-u_sd)*u_carn,(1+u_sd)*u_carn))
u_fishplar<-max(0,runif(1,(1-u_sd)*u_fishplar,(1+u_sd)*u_fishplar))
u_fishp<-max(0,runif(1,(1-u_sd)*u_fishp,(1+u_sd)*u_fishp))
u_fishm<-max(0,runif(1,(1-u_sd)*u_fishm,(1+u_sd)*u_fishm))
u_fishdlar<-max(0,runif(1,(1-u_sd)*u_fishdlar,(1+u_sd)*u_fishdlar))
u_fishd<-max(0,runif(1,(1-u_sd)*u_fishd,(1+u_sd)*u_fishd))
u_benthslar<-max(0,runif(1,(1-u_sd)*u_benthslar,(1+u_sd)*u_benthslar))
u_benthclar<-max(0,runif(1,(1-u_sd)*u_benthclar,(1+u_sd)*u_benthclar))
u_benths<-max(0,runif(1,(1-u_sd)*u_benths,(1+u_sd)*u_benths))
u_benthc<-max(0,runif(1,(1-u_sd)*u_benthc,(1+u_sd)*u_benthc))
u_bird<-max(0,runif(1,(1-u_sd)*u_bird,(1+u_sd)*u_bird))

u_seal<-max(0,runif(1,(1-u_sd)*u_seal,(1+u_sd)*u_seal))
u_ceta<-max(0,runif(1,(1-u_sd)*u_ceta,(1+u_sd)*u_ceta))

}

ustore<-list(uC_kelp,ddexudC_kelp,u_kelp,u_phyt,u_herb,u_carn,u_fishplar,u_fishp,u_fishm,u_fishdlar,u_fishd,u_benthslar,u_benthclar,u_benths,u_benthc,u_bird,u_seal,u_ceta)
names(ustore)<-c("uC_kelp","ddexudC_kelp","u_kelp","u_phyt","u_herb","u_carn","u_fishplar","u_fishp","u_fishm","u_fishdlar","u_fishd","u_benthslar","u_benthclar","u_benths","u_benthc","u_bird","u_seal","u_ceta")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(h_sd>0){

h_kelp<-max(0,runif(1,(1-h_sd)*h_kelp,h_kelp))
h_phyt<-max(0,runif(1,(1-h_sd)*h_phyt,(1+h_sd)*h_phyt))
h_herb<-max(0,runif(1,(1-h_sd)*h_herb,(1+h_sd)*h_herb))
h_carn<-max(0,runif(1,(1-h_sd)*h_carn,(1+h_sd)*h_carn))
h_fishplar<-max(0,runif(1,(1-h_sd)*h_fishplar,(1+h_sd)*h_fishplar))
h_fishp<-max(0,runif(1,(1-h_sd)*h_fishp,(1+h_sd)*h_fishp))
h_fishm<-max(0,runif(1,(1-h_sd)*h_fishm,(1+h_sd)*h_fishm))
h_fishdlar<-max(0,runif(1,(1-h_sd)*h_fishdlar,(1+h_sd)*h_fishdlar))
h_fishd<-max(0,runif(1,(1-h_sd)*h_fishd,(1+h_sd)*h_fishd))
h_benthslar<-max(0,runif(1,(1-h_sd)*h_benthslar,(1+h_sd)*h_benthslar))
h_benthclar<-max(0,runif(1,(1-h_sd)*h_benthclar,(1+h_sd)*h_benthclar))
h_benths<-max(0,runif(1,(1-h_sd)*h_benths,(1+h_sd)*h_benths))
h_benthc<-max(0,runif(1,(1-h_sd)*h_benthc,(1+h_sd)*h_benthc))
h_bird<-max(0,runif(1,(1-h_sd)*h_bird,(1+h_sd)*h_bird))

h_seal<-max(0,runif(1,(1-h_sd)*h_seal,(1+h_sd)*h_seal))
h_ceta<-max(0,runif(1,(1-h_sd)*h_ceta,(1+h_sd)*h_ceta))

bda_par_bird<-max(0,runif(1,(1-h_sd)*bda_par_bird,(1+h_sd)*bda_par_bird))
bda_par_seal<-max(0,runif(1,(1-h_sd)*bda_par_seal,(1+h_sd)*bda_par_seal))
bda_par_ceta<-max(0,runif(1,(1-h_sd)*bda_par_ceta,(1+h_sd)*bda_par_ceta))

}

hstore<-list(h_kelp,h_phyt,h_herb,h_carn,h_fishplar,h_fishp,h_fishm,h_fishdlar,h_fishd,h_benthslar,h_benthclar,h_benths,h_benthc,h_bird,h_seal,h_ceta,bda_par_bird,bda_par_seal,bda_par_ceta)
names(hstore)<-c("h_kelp","h_phyt","h_herb","h_carn","h_fishplar","h_fishp","h_fishm","h_fishdlar","h_fishd","h_benthslar","h_benthclar","h_benths","h_benthc","h_bird","h_seal","h_ceta","bda_par_bird","bda_par_seal","bda_par_ceta")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(biogeo_sd>0){

xmt<-max(0,runif(1,(1-biogeo_sd)*xmt,(1+biogeo_sd)*xmt))
xnst<-max(0,runif(1,(1-biogeo_sd)*xnst,(1+biogeo_sd)*xnst))
xdst<-max(0,runif(1,(1-biogeo_sd)*xdst,(1+biogeo_sd)*xdst))
xndt<-max(0,runif(1,(1-biogeo_sd)*xndt,(1+biogeo_sd)*xndt))
xddt<-max(0,runif(1,(1-biogeo_sd)*xddt,(1+biogeo_sd)*xddt))

  xqs_p1<-max(0,runif(1,(1-biogeo_sd)*xqs_p1,(1+biogeo_sd)*xqs_p1))
  #xqs_p1 is guessed to be less than qsp1lim so we need to trap cases where the jiggling violates this
  qsp1lim<-0.5
  if(xqs_p1>qsp1lim) {xqs_p1<-qsp1lim}
  if(xqs_p1<0) {xqs_p1<-0}

  xqs_p2<-max(0,runif(1,(1-biogeo_sd)*xqs_p2,(1+biogeo_sd)*xqs_p2))
  #xqs_p2 is guessed to be less than qsp2lim so we need to trap cases where the jiggling violates this
  qsp2lim<-0.001
  if(xqs_p2>qsp2lim) {xqs_p2<-qsp2lim}
  if(xqs_p2<0) {xqs_p2<-0}

  xqs_p3<-max(0,runif(1,(1-biogeo_sd)*xqs_p3,(1+biogeo_sd)*xqs_p3))
  #xqs_p3 is guessed to be less than qsp3lim so we need to trap cases where the jiggling violates this
  qsp3lim<-0.025
  if(xqs_p3<qsp3lim) {xqs_p3<-qsp3lim}
  if(xqs_p3<0) {xqs_p3<-0}

  xmsedt<-max(0,runif(1,(1-biogeo_sd)*xmsedt,(1+biogeo_sd)*xmsedt))


  if(xmsedt>0.015)        {xmsedt<-0.015}
  if((xqs_p1*xmsedt)>0.2) {xqs_p1 <- 0.2/xmsedt}

#xmsens is expected to be negative so convert to positive first...
xmsens<--1*xmsens
xmsens<-max(0,runif(1,(1-biogeo_sd)*xmsens,(1+biogeo_sd)*xmsens))
#convert back to negative
xmsens<--1*xmsens


xnsedt<-max(0,runif(1,(1-biogeo_sd)*xnsedt,(1+biogeo_sd)*xnsedt))

#xnsens is expected to be negative so convert to positive first...
xnsens<--1*xnsens
xnsens<-max(0,runif(1,(1-biogeo_sd)*xnsens,(1+biogeo_sd)*xnsens))
#convert back to negative
xnsens<--1*xnsens

xdsedt<-max(0,runif(1,(1-biogeo_sd)*xdsedt,(1+biogeo_sd)*xdsedt))

#xdsens is expected tpo be positive so nothing to be done here
xdsens<-max(0,runif(1,(1-biogeo_sd)*xdsens,(1+biogeo_sd)*xdsens))



xdsink_s<-max(0,runif(1,(1-biogeo_sd)*xdsink_s,(1+biogeo_sd)*xdsink_s))
xdsink_d<-max(0,runif(1,(1-biogeo_sd)*xdsink_d,(1+biogeo_sd)*xdsink_d))

if(xdsink_s>1) xdsink_s<-1
if(xdsink_d<0) xdsink_s<-0


xkelpdebris_det<- max(0,runif(1,(1-biogeo_sd)*xkelpdebris_det,(1+biogeo_sd)*xkelpdebris_det))


xxcorp_det<-max(0,runif(1,(1-biogeo_sd)*xxcorp_det,(1+biogeo_sd)*xxcorp_det))
if(xxcorp_det>0.5) xxcorp_det<-0.5


xdisc_corp<-max(0,runif(1,(1-biogeo_sd)*xdisc_corp,(1+biogeo_sd)*xdisc_corp))
if(xdisc_corp>0.7) xdisc_corp<-0.7

}

biogeostore<-list(xmt,xnst,xdst,xndt,xddt,xqs_p1,xqs_p2,xqs_p3,xmsedt,xmsens,xnsedt,xnsens,xdsedt,xdsens,xdsink_s,xdsink_d,xkelpdebris_det,xxcorp_det,xdisc_corp)

names(biogeostore)<-c("xmt","xnst","xdst","xndt","xddt","xqs_p1","xqs_p2","xqs_p3","xmsedt","xmsens","xnsedt","xnsens","xdsedt","xdsens","xdsink_s","xdsink_d","xkelpdebris_det","xxcorp_det","xdisc_corp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


if(mort_sd>0){

xxwave_kelp<-max(0,runif(1,(1-mort_sd)*xxwave_kelp,xxwave_kelp))


xxst<-max(0,runif(1,(1-mort_sd)*xxst,(1+mort_sd)*xxst))
xxdt<-max(0,runif(1,(1-mort_sd)*xxdt,(1+mort_sd)*xxdt))
xxherb<-max(0,runif(1,(1-mort_sd)*xxherb,(1+mort_sd)*xxherb))
xxcarn<-max(0,runif(1,(1-mort_sd)*xxcarn,(1+mort_sd)*xxcarn))
xxbenthslar<-max(0,runif(1,(1-mort_sd)*xxbenthslar,(1+mort_sd)*xxbenthslar))
xxbenthclar<-max(0,runif(1,(1-mort_sd)*xxbenthclar,(1+mort_sd)*xxbenthclar))
xxbenths<-max(0,runif(1,(1-mort_sd)*xxbenths,(1+mort_sd)*xxbenths))
xxbenthc<-max(0,runif(1,(1-mort_sd)*xxbenthc,(1+mort_sd)*xxbenthc))
  xxpfishlar<-max(0,runif(1,(1-mort_sd)*xxpfishlar,(1+mort_sd)*xxpfishlar))
  xxdfishlar<-max(0,runif(1,(1-mort_sd)*xxdfishlar,(1+mort_sd)*xxdfishlar))
  xxpfish<-max(0,runif(1,(1-mort_sd)*xxpfish,(1+mort_sd)*xxpfish))
  xxmfish<-max(0,runif(1,(1-mort_sd)*xxmfish,(1+mort_sd)*xxmfish))
  xxdfish<-max(0,runif(1,(1-mort_sd)*xxdfish,(1+mort_sd)*xxdfish))
  xxbird<-max(0,runif(1,(1-mort_sd)*xxbird,(1+mort_sd)*xxbird))

  xxseal<-max(0,runif(1,(1-mort_sd)*xxseal,(1+mort_sd)*xxseal))
  xxceta<-max(0,runif(1,(1-mort_sd)*xxceta,(1+mort_sd)*xxceta))

}

mortstore<-list(xxwave_kelp,xxst,xxdt,xxherb,xxcarn,xxbenthslar,xxbenthclar,xxbenths,xxbenthc,xxpfishlar,xxdfishlar,xxpfish,xxmfish,xxdfish,xxbird,xxseal,xxceta)
names(mortstore)<-c("xxwave_kelp","xxst","xxdt","xxherb","xxcarn","xxbenthslar","xxbenthclar","xxbenths","xxbenthc","xxpfishlar","xxdfishlar","xxpfish","xxmfish","xxdfish","xxbird","xxseal","xxceta")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


if(ressd>0){

 xkelpshade<- max(0,runif(1,(1-ressd)*xkelpshade,(1+ressd)*xkelpshade))

 xwave_kelpdebris<- max(0,runif(1,(1-ressd)*xwave_kelpdebris,(1+ressd)*xwave_kelpdebris))


   xdfdp<-max(0,runif(1,(1-ressd)*xdfdp,(1+ressd)*xdfdp))




         xpfish_migcoef <- max(0,runif(1,(1-ressd)*xpfish_migcoef,(1+ressd)*xpfish_migcoef))
         xmfish_migcoef <- max(0,runif(1,(1-ressd)*xmfish_migcoef,(1+ressd)*xmfish_migcoef))
         xdfish_migcoef <- max(0,runif(1,(1-ressd)*xdfish_migcoef,(1+ressd)*xdfish_migcoef))
         xbird_migcoef <- max(0,runif(1,(1-ressd)*xbird_migcoef,(1+ressd)*xbird_migcoef))
         xseal_migcoef <- max(0,runif(1,(1-ressd)*xseal_migcoef,(1+ressd)*xseal_migcoef))
         xceta_migcoef <- max(0,runif(1,(1-ressd)*xceta_migcoef,(1+ressd)*xceta_migcoef))

if(xpfish_migcoef>0.01) xpfish_migcoef<-0.01
if(xmfish_migcoef>0.01) xmfish_migcoef<-0.01
if(xdfish_migcoef>0.01) xdfish_migcoef<-0.01
if(xbird_migcoef >0.01) xbird_migcoef <-0.01
if(xseal_migcoef >0.01) xseal_migcoef <-0.01
if(xceta_migcoef >0.01) xceta_migcoef <-0.01


xmax_exploitable_f_KP <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_KP,(1+ressd)*xmax_exploitable_f_KP))
if(xmax_exploitable_f_KP>0.5) xmax_exploitable_f_KP<-0.5

xmax_exploitable_f_PF <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_PF,(1+ressd)*xmax_exploitable_f_PF))
if(xmax_exploitable_f_PF>1) xmax_exploitable_f_PF<-1

xmax_exploitable_f_DF <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_DF,(1+ressd)*xmax_exploitable_f_DF))
if(xmax_exploitable_f_DF>1) xmax_exploitable_f_DF<-1

xmax_exploitable_f_MF <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_MF,(1+ressd)*xmax_exploitable_f_MF))
if(xmax_exploitable_f_MF>1) xmax_exploitable_f_MF<-1

xmax_exploitable_f_SB <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_SB,(1+ressd)*xmax_exploitable_f_SB))
if(xmax_exploitable_f_SB>0.5) xmax_exploitable_f_SB<-0.5

xmax_exploitable_f_CB <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_CB,(1+ressd)*xmax_exploitable_f_CB))
if(xmax_exploitable_f_CB>0.5) xmax_exploitable_f_CB<-0.5

xmax_exploitable_f_CZ <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_CZ,(1+ressd)*xmax_exploitable_f_CZ))
if(xmax_exploitable_f_CZ>0.5) xmax_exploitable_f_CZ<-0.5

xmax_exploitable_f_BD <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_BD,(1+ressd)*xmax_exploitable_f_BD))
if(xmax_exploitable_f_BD>0.5) xmax_exploitable_f_BD<-0.5

xmax_exploitable_f_SL <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_SL,(1+ressd)*xmax_exploitable_f_SL))
if(xmax_exploitable_f_SL>0.5) xmax_exploitable_f_SL<-0.5

xmax_exploitable_f_CT <- max(0,runif(1,(1-ressd)*xmax_exploitable_f_CT,(1+ressd)*xmax_exploitable_f_CT))
if(xmax_exploitable_f_CT>0.5) xmax_exploitable_f_CY<-0.5

}

reststore<-list(xkelpshade,xwave_kelpdebris,xdfdp,

         xpfish_migcoef,
         xmfish_migcoef,
         xdfish_migcoef,
         xbird_migcoef,
         xseal_migcoef,
         xceta_migcoef,

             xmax_exploitable_f_KP,
             xmax_exploitable_f_PF,
             xmax_exploitable_f_DF,
             xmax_exploitable_f_MF,
             xmax_exploitable_f_SB,
             xmax_exploitable_f_CB,
             xmax_exploitable_f_CZ,
             xmax_exploitable_f_BD,
             xmax_exploitable_f_SL,
             xmax_exploitable_f_CT)

names(reststore)<-c("xkelpshade","xwave_kelpdebris","xdfdp",
         "xpfish_migcoef",
         "xmfish_migcoef",
         "xdfish_migcoef",
         "xbird_migcoef",
         "xseal_migcoef",
         "xceta_migcoef",
             "xmax_exploitable_f_KP",
             "xmax_exploitable_f_PF",
             "xmax_exploitable_f_DF",
             "xmax_exploitable_f_MF",
             "xmax_exploitable_f_SB",
             "xmax_exploitable_f_CB",
             "xmax_exploitable_f_CZ",
             "xmax_exploitable_f_BD",
             "xmax_exploitable_f_SL",
             "xmax_exploitable_f_CT")

#showall("prefstore", prefstore)
#showall("ustore", ustore)
#showall("hstore", hstore)
#showall("biogeostore", biogeostore)
#showall("mortstore", mortstore)
#showall("reststore", reststore)

	# concat the lists:
	perturbed <- c(
		prefstore,
		ustore,
		hstore,
		biogeostore,
		mortstore,
		reststore,
		"annual_obj" = annual_obj
	)
}

