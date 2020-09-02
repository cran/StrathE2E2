#
# perturb_parameters.R
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
#' @param toppredlock set to TRUE to lock the bird seal and cetacean uptake parameters
#'
#' @return perturbed parameter vector
#'
#' @noRd
#
# ------------------------------------------------------------------------------

perturb_parameters <- function(datastore, annealing.parms, toppredlock=FALSE) {

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

	PREF_NIT_phyt			<- elt(datastore, "PREF_NIT_phyt")
	PREF_AMM_phyt			<- elt(datastore, "PREF_AMM_phyt")

	PREF_phyt_omni			<- elt(datastore, "PREF_phyt_omni")
	PREF_det_omni			<- elt(datastore, "PREF_det_omni")
	PREF_benthslar_omni		<- elt(datastore, "PREF_benthslar_omni")
	PREF_benthclar_omni		<- elt(datastore, "PREF_benthclar_omni")

	PREF_omni_carn			<- elt(datastore, "PREF_omni_carn")
	PREF_benthslar_carn		<- elt(datastore, "PREF_benthslar_carn") 
	PREF_benthclar_carn		<- elt(datastore, "PREF_benthclar_carn") 
	PREF_fishplar_carn		<- elt(datastore, "PREF_fishplar_carn")
	PREF_fishdlar_carn		<- elt(datastore, "PREF_fishdlar_carn")

	PREF_omni_fishplar		<- elt(datastore, "PREF_omni_fishplar")
	PREF_benthslar_fishplar		<- elt(datastore, "PREF_benthslar_fishplar") 
	PREF_benthclar_fishplar		<- elt(datastore, "PREF_benthclar_fishplar") 

	PREF_omni_fishp			<- elt(datastore, "PREF_omni_fishp")
	PREF_carn_fishp			<- elt(datastore, "PREF_carn_fishp")
	PREF_benthslar_fishp		<- elt(datastore, "PREF_benthslar_fishp") 
	PREF_benthclar_fishp		<- elt(datastore, "PREF_benthclar_fishp") 
	PREF_fishdlar_fishp		<- elt(datastore, "PREF_fishdlar_fishp")
	PREF_fishplar_fishp		<- elt(datastore, "PREF_fishplar_fishp")

	PREF_omni_fishm			<- elt(datastore, "PREF_omni_fishm") 
	PREF_carn_fishm			<- elt(datastore, "PREF_carn_fishm") 
	PREF_benthslar_fishm		<- elt(datastore, "PREF_benthslar_fishm") 
	PREF_benthclar_fishm		<- elt(datastore, "PREF_benthclar_fishm") 
	PREF_fishdlar_fishm		<- elt(datastore, "PREF_fishdlar_fishm") 
	PREF_fishplar_fishm		<- elt(datastore, "PREF_fishplar_fishm") 

	PREF_omni_fishdlar		<- elt(datastore, "PREF_omni_fishdlar")
	PREF_benthslar_fishdlar		<- elt(datastore, "PREF_benthslar_fishdlar")
	PREF_benthclar_fishdlar		<- elt(datastore, "PREF_benthclar_fishdlar")

	PREF_carn_fishd			<- elt(datastore, "PREF_carn_fishd")
	PREF_benths_fishd		<- elt(datastore, "PREF_benths_fishd")
	PREF_benthc_fishd		<- elt(datastore, "PREF_benthc_fishd")
	PREF_fishplar_fishd		<- elt(datastore, "PREF_fishplar_fishd")
	PREF_fishdlar_fishd		<- elt(datastore, "PREF_fishdlar_fishd")
	PREF_fishp_fishd		<- elt(datastore, "PREF_fishp_fishd")
	PREF_fishm_fishd		<- elt(datastore, "PREF_fishm_fishd") 
	PREF_fishd_fishd		<- elt(datastore, "PREF_fishd_fishd")
	PREF_disc_fishd			<- elt(datastore, "PREF_disc_fishd")
	PREF_corp_fishd			<- elt(datastore, "PREF_corp_fishd")

	PREF_phyt_benthslar		<- elt(datastore, "PREF_phyt_benthslar") 
	PREF_phyt_benthclar		<- elt(datastore, "PREF_phyt_benthclar") 
	PREF_det_benthslar		<- elt(datastore, "PREF_det_benthslar") 
	PREF_det_benthclar		<- elt(datastore, "PREF_det_benthclar") 


	PREF_phyt_benths		<- elt(datastore, "PREF_phyt_benths")
	PREF_det_benths			<- elt(datastore, "PREF_det_benths")
	PREF_sed_benths			<- elt(datastore, "PREF_sed_benths")

	PREF_kelp_benthc		<- elt(datastore, "PREF_kelp_benthc")
	PREF_kelpdebris_benthc		<- elt(datastore, "PREF_kelpdebris_benthc")
	PREF_benths_benthc		<- elt(datastore, "PREF_benths_benthc")
	PREF_corp_benthc		<- elt(datastore, "PREF_corp_benthc")

	PREF_carn_bird			<- elt(datastore, "PREF_carn_bird")
	PREF_benths_bird		<- elt(datastore, "PREF_benths_bird")
	PREF_benthc_bird		<- elt(datastore, "PREF_benthc_bird")

	PREF_fishp_bird			<- elt(datastore, "PREF_fishp_bird")
	PREF_fishm_bird			<- elt(datastore, "PREF_fishm_bird")
	PREF_fishd_bird			<- elt(datastore, "PREF_fishd_bird")
	PREF_disc_bird			<- elt(datastore, "PREF_disc_bird")
	PREF_corp_bird			<- elt(datastore, "PREF_corp_bird")


	PREF_carn_seal			<- elt(datastore, "PREF_carn_seal")
	PREF_benths_seal		<- elt(datastore, "PREF_benths_seal")
	PREF_benthc_seal		<- elt(datastore, "PREF_benthc_seal")

	PREF_fishp_seal			<- elt(datastore, "PREF_fishp_seal")
	PREF_fishm_seal			<- elt(datastore, "PREF_fishm_seal")
	PREF_fishd_seal			<- elt(datastore, "PREF_fishd_seal")
	PREF_bird_seal			<- elt(datastore, "PREF_bird_seal")
	PREF_disc_seal			<- elt(datastore, "PREF_disc_seal")
	PREF_corp_seal			<- elt(datastore, "PREF_corp_seal")


	PREF_omni_ceta			<- elt(datastore, "PREF_omni_ceta")
	PREF_carn_ceta			<- elt(datastore, "PREF_carn_ceta")
	PREF_benths_ceta		<- elt(datastore, "PREF_benths_ceta")
	PREF_benthc_ceta		<- elt(datastore, "PREF_benthc_ceta")

	PREF_fishp_ceta			<- elt(datastore, "PREF_fishp_ceta")
	PREF_fishm_ceta			<- elt(datastore, "PREF_fishm_ceta")
	PREF_fishd_ceta			<- elt(datastore, "PREF_fishd_ceta")
	PREF_bird_ceta			<- elt(datastore, "PREF_bird_ceta")
	PREF_seal_ceta			<- elt(datastore, "PREF_seal_ceta")
	PREF_disc_ceta			<- elt(datastore, "PREF_disc_ceta")

	uC_kelp				<- elt(datastore, "uC_kelp")

	ddexudC_kelp			<- elt(datastore, "ddexudC_kelp")

	#Then we set the rate parameters for each predator at the reference temperature

	u_kelp				<- elt(datastore, "u_kelp") 

	u_phyt				<- elt(datastore, "u_phyt")
	u_omni				<- elt(datastore, "u_omni")
	u_carn				<- elt(datastore, "u_carn")
	u_fishplar			<- elt(datastore, "u_fishplar")
	u_fishp				<- elt(datastore, "u_fishp")
	u_fishm				<- elt(datastore, "u_fishm")
	u_fishdlar			<- elt(datastore, "u_fishdlar")
	u_fishd				<- elt(datastore, "u_fishd")
	u_benthslar			<- elt(datastore, "u_benthslar")
	u_benthclar			<- elt(datastore, "u_benthclar")
	u_benths			<- elt(datastore, "u_benths")
	u_benthc			<- elt(datastore, "u_benthc")
	u_bird				<- elt(datastore, "u_bird")

	u_seal				<- elt(datastore, "u_seal")
	u_ceta				<- elt(datastore, "u_ceta")

	h_kelp				<- elt(datastore, "h_kelp")

	h_phyt				<- elt(datastore, "h_phyt")
	h_omni				<- elt(datastore, "h_omni")
	h_carn				<- elt(datastore, "h_carn")
	h_fishplar			<- elt(datastore, "h_fishplar")
	h_fishp				<- elt(datastore, "h_fishp")
	h_fishm				<- elt(datastore, "h_fishm")
	h_fishdlar			<- elt(datastore, "h_fishdlar")
	h_fishd				<- elt(datastore, "h_fishd")
	h_benthslar			<- elt(datastore, "h_benthslar")
	h_benthclar			<- elt(datastore, "h_benthclar")
	h_benths			<- elt(datastore, "h_benths")
	h_benthc			<- elt(datastore, "h_benthc")
	h_bird				<- elt(datastore, "h_bird")

	h_seal				<- elt(datastore, "h_seal")
	h_ceta				<- elt(datastore, "h_ceta")

	bda_par_bird			<- elt(datastore, "bda_par_bird")

	bda_par_seal			<- elt(datastore, "bda_par_seal")
	bda_par_ceta			<- elt(datastore, "bda_par_ceta")


	#Mineralisation, nitrification and denitrification rates per day at the reference temperature
	xmt				<- elt(datastore, "xmt")
	xnst				<- elt(datastore, "xnst")
	xdst				<- elt(datastore, "xdst")
	xndt				<- elt(datastore, "xndt")
	xddt				<- elt(datastore, "xddt")

	xqs_p1				<- elt(datastore, "xqs_p1")	# proportion of detritus which become refractory when minearlised
	xqs_p2				<- elt(datastore, "xqs_p2")	# ratio of refratory to labile detritus minearalisation rates
	xqs_p3				<- elt(datastore, "xqs_p3")	# proportion of refractory whiuch becomes labile when re-oxygenated

	xmsedt				<- elt(datastore, "xmsedt")
	xmsens				<- elt(datastore, "xmsens")

	xnsedt				<- elt(datastore, "xnsedt")
	xnsens				<- elt(datastore, "xnsens")

	xdsedt				<- elt(datastore, "xdsedt")
	xdsens				<- elt(datastore, "xdsens")


	xxwave_kelp			<- elt(datastore, "xxwave_kelp")

	#Death rates of phytoplankton at the reference temperature
	# IN THIS VERSION (NOV 2015) THESE PARAMATERS ARE APPLIED AS A DENSITY DEPENDENT RATE
	# RATE RTHAN AS A DENSITY INDEPENDENT RATE AS IN THE ORIGINAL MODEL
	xxst				<- elt(datastore, "xxst")
	xxdt				<- elt(datastore, "xxdt")

	#Death rate of carnivores fish birds and mammals per unit biomass - temperature independent
	xxomni				<- elt(datastore, "xxomni")
	xxcarn				<- elt(datastore, "xxcarn")
	xxbenthslar			<- elt(datastore, "xxbenthslar")
	xxbenthclar			<- elt(datastore, "xxbenthclar")
	xxbenths			<- elt(datastore, "xxbenths")
	xxbenthc			<- elt(datastore, "xxbenthc")
	xxpfishlar			<- elt(datastore, "xxpfishlar")
	xxdfishlar			<- elt(datastore, "xxdfishlar")
	xxpfish				<- elt(datastore, "xxpfish")
	xxmfish				<- elt(datastore, "xxmfish")
	xxdfish				<- elt(datastore, "xxdfish")
	xxbird				<- elt(datastore, "xxbird")

	xxseal				<- elt(datastore, "xxseal")
	xxceta				<- elt(datastore, "xxceta")


	#Proportion of corpse mass converted to detritus per day at the reference temperature
	xxcorp_det			<- elt(datastore, "xxcorp_det")

	#Proportion of discards sinking to become seabed corpses per day - temperature independent
	xdisc_corp			<- elt(datastore, "xdisc_corp")


	#Proportion of corpse mass converted to detritus per day at the reference temperature
	xkelpdebris_det			<- elt(datastore, "xkelpdebris_det")

	#Sinking rates and their dependence on mixing - temperature independent
	xdsink_s			<- elt(datastore, "xdsink_s")
	#	xdsink_d_Klow		<- elt(datastore, "xdsink_d_Klow")
	xdsink_d			<- elt(datastore, "xdsink_d")

	
	#Density dependent self shading parameter for kelp
	xkelpshade			<- elt(datastore, "xkelpshade")

	#Wave dependent beach-cast paramater fpor kelp debris
	xwave_kelpdebris		<- elt(datastore, "xwave_kelpdebris")

	#Fitting parameter for scaling between whole model region demersal fish N mass and the survey
	#index on which the empirical relationships for pNQ and dsacrad ates of Q and NQ species are based.
	#Expect this to be abouut 2
	xdfdp				<- elt(datastore, "xdfdp")


	#Parameters for food gradient migration rates of pelagic and migratory fish
	xpfish_migcoef 			<- elt(datastore, "xpfish_migcoef")
	xmfish_migcoef 			<- elt(datastore, "xmfish_migcoef")
	xdfish_migcoef 			<- elt(datastore, "xdfish_migcoef")
	xbird_migcoef 			<- elt(datastore, "xbird_migcoef")

	xseal_migcoef 			<- elt(datastore, "xseal_migcoef")
	xceta_migcoef 			<- elt(datastore, "xceta_migcoef")

	#Maximum proportions of the stock biomass which is accessible to the fisheries
	#Units proportions

	xmax_exploitable_f_KP		<- elt(datastore, "xmax_exploitable_f_KP")

	xmax_exploitable_f_PF		<- elt(datastore, "xmax_exploitable_f_PF")
	xmax_exploitable_f_DF		<- elt(datastore, "xmax_exploitable_f_DF")
	xmax_exploitable_f_MF		<- elt(datastore, "xmax_exploitable_f_MF")
	xmax_exploitable_f_SB		<- elt(datastore, "xmax_exploitable_f_SB")
	xmax_exploitable_f_CB		<- elt(datastore, "xmax_exploitable_f_CB")
	xmax_exploitable_f_CZ		<- elt(datastore, "xmax_exploitable_f_CZ")
	xmax_exploitable_f_BD		<- elt(datastore, "xmax_exploitable_f_BD")
	xmax_exploitable_f_SL		<- elt(datastore, "xmax_exploitable_f_SL")
	xmax_exploitable_f_CT		<- elt(datastore, "xmax_exploitable_f_CT")

	annual_obj			<- elt(datastore, "annual_obj")

	#----------------------------------------------------------------------------

#Now randomise the parameters

#First we set up the preferences

if(Prefsd>0){

	PREF_NIT_kelpx<-max(0,rnorm(1,PREF_NIT_kelp,Prefsd*PREF_NIT_kelp))
	PREF_AMM_kelpx<-max(0,rnorm(1,PREF_AMM_kelp,Prefsd*PREF_AMM_kelp))
#Renormalise
prefsum<-PREF_NIT_kelpx+PREF_AMM_kelpx
	PREF_NIT_kelp<-PREF_NIT_kelpx/prefsum
	PREF_AMM_kelp<-PREF_AMM_kelpx/prefsum


	PREF_NIT_phytx<-max(0,rnorm(1,PREF_NIT_phyt,Prefsd*PREF_NIT_phyt))
	PREF_AMM_phytx<-max(0,rnorm(1,PREF_AMM_phyt,Prefsd*PREF_AMM_phyt))
#Renormalise
prefsum<-PREF_NIT_phytx+PREF_AMM_phytx
	PREF_NIT_phyt<-PREF_NIT_phytx/prefsum
	PREF_AMM_phyt<-PREF_AMM_phytx/prefsum

	PREF_phyt_omnix<-max(0,rnorm(1,PREF_phyt_omni,Prefsd*PREF_phyt_omni))
	PREF_det_omnix<-max(0,rnorm(1,PREF_det_omni,Prefsd*PREF_det_omni))
	PREF_benthslar_omnix<-max(0,rnorm(1,PREF_benthslar_omni,Prefsd*PREF_benthslar_omni))
	PREF_benthclar_omnix<-max(0,rnorm(1,PREF_benthclar_omni,Prefsd*PREF_benthclar_omni))
#Renormalise
prefsum<-PREF_phyt_omnix+PREF_det_omnix+PREF_benthslar_omnix+PREF_benthclar_omnix
	PREF_phyt_omni<-PREF_phyt_omnix/prefsum
	PREF_det_omni<-PREF_det_omnix/prefsum
	PREF_benthslar_omni<-PREF_benthslar_omnix/prefsum
	PREF_benthclar_omni<-PREF_benthclar_omnix/prefsum

	PREF_omni_carnx<-max(0,rnorm(1,PREF_omni_carn,Prefsd*PREF_omni_carn))
	PREF_fishplar_carnx<-max(0,rnorm(1,PREF_fishplar_carn,Prefsd*PREF_fishplar_carn))
	PREF_fishdlar_carnx<-max(0,rnorm(1,PREF_fishdlar_carn,Prefsd*PREF_fishdlar_carn))
	PREF_benthslar_carnx<-max(0,rnorm(1,PREF_benthslar_carn,Prefsd*PREF_benthslar_carn))
	PREF_benthclar_carnx<-max(0,rnorm(1,PREF_benthclar_carn,Prefsd*PREF_benthclar_carn))
#Renormalise
prefsum<-PREF_omni_carnx+PREF_fishplar_carnx+PREF_fishdlar_carnx+PREF_benthslar_carnx+PREF_benthclar_carnx
	PREF_omni_carn<-PREF_omni_carnx/prefsum
	PREF_fishplar_carn<-PREF_fishplar_carnx/prefsum
	PREF_fishdlar_carn<-PREF_fishdlar_carnx/prefsum
	PREF_benthslar_carn<-PREF_benthslar_carnx/prefsum
	PREF_benthclar_carn<-PREF_benthclar_carnx/prefsum


	PREF_omni_fishplarx<-max(0,rnorm(1,PREF_omni_fishplar,Prefsd*PREF_omni_fishplar))
	PREF_benthslar_fishplarx<-max(0,rnorm(1,PREF_benthslar_fishplar,Prefsd*PREF_benthslar_fishplar))
	PREF_benthclar_fishplarx<-max(0,rnorm(1,PREF_benthclar_fishplar,Prefsd*PREF_benthclar_fishplar))
#Renormalise
prefsum<-PREF_omni_fishplarx+PREF_benthslar_fishplarx+PREF_benthclar_fishplarx
	PREF_omni_fishplar<-PREF_omni_fishplarx/prefsum
	PREF_benthslar_fishplar<-PREF_benthslar_fishplarx/prefsum
	PREF_benthclar_fishplar<-PREF_benthclar_fishplarx/prefsum

	PREF_omni_fishpx<-max(0,rnorm(1,PREF_omni_fishp,Prefsd*PREF_omni_fishp))
	PREF_carn_fishpx<-max(0,rnorm(1,PREF_carn_fishp,Prefsd*PREF_carn_fishp))
	PREF_fishdlar_fishpx<-max(0,rnorm(1,PREF_fishdlar_fishp,Prefsd*PREF_fishdlar_fishp))
	PREF_fishplar_fishpx<-max(0,rnorm(1,PREF_fishplar_fishp,Prefsd*PREF_fishplar_fishp))
	PREF_benthslar_fishpx<-max(0,rnorm(1,PREF_benthslar_fishp,Prefsd*PREF_benthslar_fishp))
	PREF_benthclar_fishpx<-max(0,rnorm(1,PREF_benthclar_fishp,Prefsd*PREF_benthclar_fishp))
#Renormalise
prefsum<-PREF_omni_fishpx+PREF_carn_fishpx+PREF_fishdlar_fishpx+PREF_fishplar_fishpx+PREF_benthslar_fishpx+PREF_benthclar_fishpx
	PREF_omni_fishp<-PREF_omni_fishpx/prefsum
	PREF_carn_fishp<-PREF_carn_fishpx/prefsum
	PREF_fishdlar_fishp<-PREF_fishdlar_fishpx/prefsum
	PREF_fishplar_fishp<-PREF_fishplar_fishpx/prefsum
	PREF_benthslar_fishp<-PREF_benthslar_fishpx/prefsum
	PREF_benthclar_fishp<-PREF_benthclar_fishpx/prefsum


	PREF_omni_fishmx<-max(0,rnorm(1,PREF_omni_fishm,Prefsd*PREF_omni_fishm)) 
	PREF_carn_fishmx<-max(0,rnorm(1,PREF_carn_fishm,Prefsd*PREF_carn_fishm)) 
	PREF_fishdlar_fishmx<-max(0,rnorm(1,PREF_fishdlar_fishm,Prefsd*PREF_fishdlar_fishm))
	PREF_fishplar_fishmx<-max(0,rnorm(1,PREF_fishplar_fishm,Prefsd*PREF_fishplar_fishm))
	PREF_benthslar_fishmx<-max(0,rnorm(1,PREF_benthslar_fishm,Prefsd*PREF_benthslar_fishm))
	PREF_benthclar_fishmx<-max(0,rnorm(1,PREF_benthclar_fishm,Prefsd*PREF_benthclar_fishm))
#Renormalise
prefsum<-PREF_omni_fishmx+PREF_carn_fishmx+PREF_benthslar_fishmx+PREF_benthclar_fishmx+PREF_fishdlar_fishmx+PREF_fishplar_fishmx
	PREF_omni_fishm<-PREF_omni_fishmx/prefsum
	PREF_carn_fishm<-PREF_carn_fishmx/prefsum
	PREF_fishdlar_fishm<-PREF_fishdlar_fishmx/prefsum
	PREF_fishplar_fishm<-PREF_fishplar_fishmx/prefsum
	PREF_benthslar_fishm<-PREF_benthslar_fishmx/prefsum
	PREF_benthclar_fishm<-PREF_benthclar_fishmx/prefsum


	PREF_omni_fishdlarx<-max(0,rnorm(1,PREF_omni_fishdlar,Prefsd*PREF_omni_fishdlar))
	PREF_benthslar_fishdlarx<-max(0,rnorm(1,PREF_benthslar_fishdlar,Prefsd*PREF_benthslar_fishdlar))
	PREF_benthclar_fishdlarx<-max(0,rnorm(1,PREF_benthclar_fishdlar,Prefsd*PREF_benthclar_fishdlar))
#Renormalise
prefsum<-PREF_omni_fishdlarx+PREF_benthslar_fishdlarx+PREF_benthclar_fishdlarx
	PREF_omni_fishdlar<-PREF_omni_fishdlarx/prefsum
	PREF_benthslar_fishdlar<-PREF_benthslar_fishdlarx/prefsum
	PREF_benthclar_fishdlar<-PREF_benthclar_fishdlarx/prefsum

	PREF_carn_fishdx<-max(0,rnorm(1,PREF_carn_fishd,Prefsd*PREF_carn_fishd))
	PREF_benths_fishdx<-max(0,rnorm(1,PREF_benths_fishd,Prefsd*PREF_benths_fishd))
	PREF_benthc_fishdx<-max(0,rnorm(1,PREF_benthc_fishd,Prefsd*PREF_benthc_fishd))
	PREF_fishplar_fishdx<-max(0,rnorm(1,PREF_fishplar_fishd,Prefsd*PREF_fishplar_fishd))
	PREF_fishdlar_fishdx<-max(0,rnorm(1,PREF_fishdlar_fishd,Prefsd*PREF_fishdlar_fishd))
	PREF_fishp_fishdx<-max(0,rnorm(1,PREF_fishp_fishd,Prefsd*PREF_fishp_fishd))
	PREF_fishm_fishdx<-max(0,rnorm(1,PREF_fishm_fishd,Prefsd*PREF_fishm_fishd))
	PREF_fishd_fishdx<-max(0,rnorm(1,PREF_fishd_fishd,Prefsd*PREF_fishd_fishd))
	PREF_disc_fishdx<-max(0,rnorm(1,PREF_disc_fishd,Prefsd*PREF_disc_fishd))
	PREF_corp_fishdx<-max(0,rnorm(1,PREF_corp_fishd,Prefsd*PREF_corp_fishd))
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

	PREF_phyt_benthslarx<-max(0,rnorm(1,PREF_phyt_benthslar,Prefsd*PREF_phyt_benthslar))
	PREF_det_benthslarx<-max(0,rnorm(1,PREF_det_benthslar,Prefsd*PREF_det_benthslar))
#Renormalise
prefsum<-PREF_phyt_benthslarx+PREF_det_benthslarx
	PREF_phyt_benthslar<-PREF_phyt_benthslarx/prefsum
	PREF_det_benthslar<-PREF_det_benthslarx/prefsum

	PREF_phyt_benthclarx<-max(0,rnorm(1,PREF_phyt_benthclar,Prefsd*PREF_phyt_benthclar))
	PREF_det_benthclarx<-max(0,rnorm(1,PREF_det_benthclar,Prefsd*PREF_det_benthclar))
#Renormalise
prefsum<-PREF_phyt_benthclarx+PREF_det_benthclarx
	PREF_phyt_benthclar<-PREF_phyt_benthclarx/prefsum
	PREF_det_benthclar<-PREF_det_benthclarx/prefsum


	PREF_phyt_benthsx<-max(0,rnorm(1,PREF_phyt_benths,Prefsd*PREF_phyt_benths))
	PREF_det_benthsx<-max(0,rnorm(1,PREF_det_benths,Prefsd*PREF_det_benths))
	PREF_sed_benthsx<-max(0,rnorm(1,PREF_sed_benths,Prefsd*PREF_sed_benths))
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


	PREF_kelp_benthcx<-max(0,rnorm(1,PREF_kelp_benthc,Prefsd*PREF_kelp_benthc))
	PREF_kelpdebris_benthcx<-max(0,rnorm(1,PREF_kelpdebris_benthc,Prefsd*PREF_kelpdebris_benthc))
	PREF_benths_benthcx<-max(0,rnorm(1,PREF_benths_benthc,Prefsd*PREF_benths_benthc))
	PREF_corp_benthcx<-max(0,rnorm(1,PREF_corp_benthc,Prefsd*PREF_corp_benthc))
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


	if (toppredlock == FALSE) {
		# not locked
		PREF_carn_birdx<-max(0,rnorm(1,PREF_carn_bird,Prefsd*PREF_carn_bird))
		PREF_benths_birdx<-max(0,rnorm(1,PREF_benths_bird,Prefsd*PREF_benths_bird))
		PREF_benthc_birdx<-max(0,rnorm(1,PREF_benthc_bird,Prefsd*PREF_benthc_bird))
		PREF_fishp_birdx<-max(0,rnorm(1,PREF_fishp_bird,Prefsd*PREF_fishp_bird))
		PREF_fishm_birdx<-max(0,rnorm(1,PREF_fishm_bird,Prefsd*PREF_fishm_bird))
		PREF_fishd_birdx<-max(0,rnorm(1,PREF_fishd_bird,Prefsd*PREF_fishd_bird))
		PREF_disc_birdx<-max(0,rnorm(1,PREF_disc_bird,Prefsd*PREF_disc_bird))
		PREF_corp_birdx<-max(0,rnorm(1,PREF_corp_bird,Prefsd*PREF_corp_bird))
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

		PREF_carn_sealx<-max(0,rnorm(1,PREF_carn_seal,Prefsd*PREF_carn_seal))
		PREF_benths_sealx<-max(0,rnorm(1,PREF_benths_seal,Prefsd*PREF_benths_seal))
		PREF_benthc_sealx<-max(0,rnorm(1,PREF_benthc_seal,Prefsd*PREF_benthc_seal))
		PREF_fishp_sealx<-max(0,rnorm(1,PREF_fishp_seal,Prefsd*PREF_fishp_seal))
		PREF_fishm_sealx<-max(0,rnorm(1,PREF_fishm_seal,Prefsd*PREF_fishm_seal))
		PREF_fishd_sealx<-max(0,rnorm(1,PREF_fishd_seal,Prefsd*PREF_fishd_seal))
		PREF_bird_sealx<-max(0,rnorm(1,PREF_bird_seal,Prefsd*PREF_bird_seal))
		PREF_disc_sealx<-max(0,rnorm(1,PREF_disc_seal,Prefsd*PREF_disc_seal))
		PREF_corp_sealx<-max(0,rnorm(1,PREF_corp_seal,Prefsd*PREF_corp_seal))
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



		PREF_omni_cetax<-max(0,rnorm(1,PREF_omni_ceta,Prefsd*PREF_omni_ceta))
		PREF_carn_cetax<-max(0,rnorm(1,PREF_carn_ceta,Prefsd*PREF_carn_ceta))
		PREF_benths_cetax<-max(0,rnorm(1,PREF_benths_ceta,Prefsd*PREF_benths_ceta))
		PREF_benthc_cetax<-max(0,rnorm(1,PREF_benthc_ceta,Prefsd*PREF_benthc_ceta))
		PREF_fishp_cetax<-max(0,rnorm(1,PREF_fishp_ceta,Prefsd*PREF_fishp_ceta))
		PREF_fishm_cetax<-max(0,rnorm(1,PREF_fishm_ceta,Prefsd*PREF_fishm_ceta))
		PREF_fishd_cetax<-max(0,rnorm(1,PREF_fishd_ceta,Prefsd*PREF_fishd_ceta))
		PREF_bird_cetax<-max(0,rnorm(1,PREF_bird_ceta,Prefsd*PREF_bird_ceta))
		PREF_seal_cetax<-max(0,rnorm(1,PREF_seal_ceta,Prefsd*PREF_seal_ceta))
		PREF_disc_cetax<-max(0,rnorm(1,PREF_disc_ceta,Prefsd*PREF_disc_ceta))
		#Renormalise
		prefsum<-PREF_omni_cetax+PREF_carn_cetax+PREF_benths_cetax+PREF_benthc_cetax+PREF_fishp_cetax+PREF_fishm_cetax+PREF_fishd_cetax+PREF_bird_cetax+PREF_seal_cetax+PREF_disc_cetax
		PREF_omni_ceta<-PREF_omni_cetax/prefsum
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

}

prefstore<-list(PREF_NIT_kelp,PREF_AMM_kelp,
             PREF_NIT_phyt,PREF_AMM_phyt,PREF_phyt_omni,PREF_det_omni,PREF_benthslar_omni,PREF_benthclar_omni,PREF_omni_carn,PREF_benthslar_carn,PREF_benthclar_carn,PREF_fishplar_carn,PREF_fishdlar_carn,
             PREF_omni_fishplar,PREF_benthslar_fishplar,PREF_benthclar_fishplar,
             PREF_omni_fishp,PREF_carn_fishp,PREF_benthslar_fishp,PREF_benthclar_fishp,PREF_fishdlar_fishp,PREF_fishplar_fishp,
             PREF_omni_fishm,PREF_carn_fishm,PREF_benthslar_fishm,PREF_benthclar_fishm,PREF_fishdlar_fishm,PREF_fishplar_fishm,
             PREF_omni_fishdlar,PREF_benthslar_fishdlar,PREF_benthclar_fishdlar,
             PREF_carn_fishd,PREF_benths_fishd,PREF_benthc_fishd,PREF_fishplar_fishd,PREF_fishdlar_fishd,PREF_fishp_fishd,PREF_fishm_fishd,PREF_fishd_fishd,PREF_disc_fishd,PREF_corp_fishd,
             PREF_phyt_benthslar,PREF_phyt_benthclar,
             PREF_det_benthslar,PREF_det_benthclar,
             PREF_phyt_benths,PREF_det_benths,PREF_sed_benths,
             PREF_kelp_benthc,PREF_kelpdebris_benthc,PREF_benths_benthc,PREF_corp_benthc,
             PREF_carn_bird,PREF_benths_bird,PREF_benthc_bird,PREF_fishp_bird,PREF_fishm_bird,PREF_fishd_bird,PREF_disc_bird,PREF_corp_bird,
             PREF_carn_seal,PREF_benths_seal,PREF_benthc_seal,PREF_fishp_seal,PREF_fishm_seal,PREF_fishd_seal,PREF_bird_seal,PREF_disc_seal,PREF_corp_seal,
             PREF_omni_ceta,PREF_carn_ceta,PREF_benths_ceta,PREF_benthc_ceta,PREF_fishp_ceta,PREF_fishm_ceta,PREF_fishd_ceta,PREF_bird_ceta,PREF_seal_ceta,PREF_disc_ceta)
names(prefstore)<-c("PREF_NIT_kelp","PREF_AMM_kelp",
            "PREF_NIT_phyt","PREF_AMM_phyt","PREF_phyt_omni","PREF_det_omni","PREF_benthslar_omni","PREF_benthclar_omni","PREF_omni_carn","PREF_benthslar_carn","PREF_benthclar_carn","PREF_fishplar_carn","PREF_fishdlar_carn",
            "PREF_omni_fishplar","PREF_benthslar_fishplar","PREF_benthclar_fishplar",
            "PREF_omni_fishp","PREF_carn_fishp","PREF_benthslar_fishp","PREF_benthclar_fishp","PREF_fishdlar_fishp","PREF_fishplar_fishp",
            "PREF_omni_fishm","PREF_carn_fishm","PREF_benthslar_fishm","PREF_benthclar_fishm","PREF_fishdlar_fishm","PREF_fishplar_fishm",
            "PREF_omni_fishdlar","PREF_benthslar_fishdlar","PREF_benthclar_fishdlar",
            "PREF_carn_fishd","PREF_benths_fishd","PREF_benthc_fishd","PREF_fishplar_fishd","PREF_fishdlar_fishd","PREF_fishp_fishd","PREF_fishm_fishd","PREF_fishd_fishd","PREF_disc_fishd","PREF_corp_fishd",
            "PREF_phyt_benthslar","PREF_phyt_benthclar",
            "PREF_det_benthslar","PREF_det_benthclar",
            "PREF_phyt_benths","PREF_det_benths","PREF_sed_benths",
            "PREF_kelp_benthc","PREF_kelpdebris_benthc","PREF_benths_benthc","PREF_corp_benthc",
            "PREF_carn_bird","PREF_benths_bird","PREF_benthc_bird","PREF_fishp_bird","PREF_fishm_bird","PREF_fishd_bird","PREF_disc_bird","PREF_corp_bird",
            "PREF_carn_seal","PREF_benths_seal","PREF_benthc_seal","PREF_fishp_seal","PREF_fishm_seal","PREF_fishd_seal","PREF_bird_seal","PREF_disc_seal","PREF_corp_seal",
            "PREF_omni_ceta","PREF_carn_ceta","PREF_benths_ceta","PREF_benthc_ceta","PREF_fishp_ceta","PREF_fishm_ceta","PREF_fishd_ceta","PREF_bird_ceta","PREF_seal_ceta","PREF_disc_ceta")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(u_sd>0){

uC_kelp<-max(0,rnorm(1,uC_kelp,uC_kelp*u_sd))
ddexudC_kelp<-max(0,rnorm(1,ddexudC_kelp,ddexudC_kelp*u_sd))
u_kelp<-max(0,rnorm(1,u_kelp,u_kelp*u_sd))


u_phyt<-max(0,rnorm(1,u_phyt,u_phyt*u_sd))
u_omni<-max(0,rnorm(1,u_omni,u_omni*u_sd))
u_carn<-max(0,rnorm(1,u_carn,u_carn*u_sd))
u_fishplar<-max(0,rnorm(1,u_fishplar,u_fishplar*u_sd))
u_fishp<-max(0,rnorm(1,u_fishp,u_fishp*u_sd))
u_fishm<-max(0,rnorm(1,u_fishm,u_fishm*u_sd))
u_fishdlar<-max(0,rnorm(1,u_fishdlar,u_fishdlar*u_sd))
u_fishd<-max(0,rnorm(1,u_fishd,u_fishd*u_sd))
u_benthslar<-max(0,rnorm(1,u_benthslar,u_benthslar*u_sd))
u_benthclar<-max(0,rnorm(1,u_benthclar,u_benthclar*u_sd))
u_benths<-max(0,rnorm(1,u_benths,u_benths*u_sd))
u_benthc<-max(0,rnorm(1,u_benthc,u_benthc*u_sd))

	if (toppredlock == FALSE) {
		# not locked
		u_bird<-max(0,rnorm(1,u_bird,u_bird*u_sd))
		u_seal<-max(0,rnorm(1,u_seal,u_seal*u_sd))
		u_ceta<-max(0,rnorm(1,u_ceta,u_ceta*u_sd))
	}

#u_phyt<-max(0,rnorm(1,u_phyt,u_phyt*ressd))
#u_omni<-max(0,rnorm(1,u_omni,u_omni*ressd))
#u_carn<-max(0,rnorm(1,u_carn,u_carn*ressd))
#u_fishplar<-max(0,rnorm(1,u_fishplar,u_fishplar*ressd))
#u_fishp<-max(0,rnorm(1,u_fishp,u_fishp*ressd))
#u_fishdlar<-max(0,rnorm(1,u_fishdlar,u_fishdlar*ressd))
#u_fishd<-max(0,rnorm(1,u_fishd,u_fishd*ressd))
#u_benths<-max(0,rnorm(1,u_benths,u_benths*ressd))
#u_bird<-max(0,rnorm(1,u_bird,u_bird*ressd))

}

ustore<-list(uC_kelp,ddexudC_kelp,u_kelp,u_phyt,u_omni,u_carn,u_fishplar,u_fishp,u_fishm,u_fishdlar,u_fishd,u_benthslar,u_benthclar,u_benths,u_benthc,u_bird,u_seal,u_ceta)
names(ustore)<-c("uC_kelp","ddexudC_kelp","u_kelp","u_phyt","u_omni","u_carn","u_fishplar","u_fishp","u_fishm","u_fishdlar","u_fishd","u_benthslar","u_benthclar","u_benths","u_benthc","u_bird","u_seal","u_ceta")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(h_sd>0){

h_kelp<-max(0,rnorm(1,h_kelp,h_kelp*h_sd))
h_phyt<-max(0,rnorm(1,h_phyt,h_phyt*h_sd))
h_omni<-max(0,rnorm(1,h_omni,h_omni*h_sd))
h_carn<-max(0,rnorm(1,h_carn,h_carn*h_sd))
h_fishplar<-max(0,rnorm(1,h_fishplar,h_fishplar*h_sd))
h_fishp<-max(0,rnorm(1,h_fishp,h_fishp*h_sd))
h_fishm<-max(0,rnorm(1,h_fishm,h_fishm*h_sd))
h_fishdlar<-max(0,rnorm(1,h_fishdlar,h_fishdlar*h_sd))
h_fishd<-max(0,rnorm(1,h_fishd,h_fishd*h_sd))
h_benthslar<-max(0,rnorm(1,h_benthslar,h_benthslar*h_sd))
h_benthclar<-max(0,rnorm(1,h_benthclar,h_benthclar*h_sd))
h_benths<-max(0,rnorm(1,h_benths,h_benths*h_sd))
h_benthc<-max(0,rnorm(1,h_benthc,h_benthc*h_sd))

	if (toppredlock == FALSE) {
		# not locked
		h_bird<-max(0,rnorm(1,h_bird,h_bird*h_sd))
		h_seal<-max(0,rnorm(1,h_seal,h_seal*h_sd))
		h_ceta<-max(0,rnorm(1,h_ceta,h_ceta*h_sd))

		bda_par_bird<-max(0,rnorm(1,bda_par_bird,bda_par_bird*h_sd))
		bda_par_seal<-max(0,rnorm(1,bda_par_seal,bda_par_seal*h_sd))
		bda_par_ceta<-max(0,rnorm(1,bda_par_ceta,bda_par_ceta*h_sd))
	}

#h_omni<-max(0,rnorm(1,h_omni,h_omni*ressd))
#h_carn<-max(0,rnorm(1,h_carn,h_carn*ressd))
#h_fishp<-max(0,rnorm(1,h_fishp,h_fishp*ressd))
#h_fishd<-max(0,rnorm(1,h_fishd,h_fishd*ressd))
#h_benths<-max(0,rnorm(1,h_benths,h_benths*ressd))
#h_bird<-max(0,rnorm(1,h_bird,h_bird*ressd))

}

hstore<-list(h_kelp,h_phyt,h_omni,h_carn,h_fishplar,h_fishp,h_fishm,h_fishdlar,h_fishd,h_benthslar,h_benthclar,h_benths,h_benthc,h_bird,h_seal,h_ceta,bda_par_bird,bda_par_seal,bda_par_ceta)
names(hstore)<-c("h_kelp","h_phyt","h_omni","h_carn","h_fishplar","h_fishp","h_fishm","h_fishdlar","h_fishd","h_benthslar","h_benthclar","h_benths","h_benthc","h_bird","h_seal","h_ceta","bda_par_bird","bda_par_seal","bda_par_ceta")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(biogeo_sd>0){

xmt<-max(0,rnorm(1,xmt,xmt*biogeo_sd))
xnst<-max(0,rnorm(1,xnst,xnst*biogeo_sd))
xdst<-max(0,rnorm(1,xdst,xdst*biogeo_sd))
xndt<-max(0,rnorm(1,xndt,xndt*biogeo_sd))
xddt<-max(0,rnorm(1,xddt,xddt*biogeo_sd))

  xqs_p1<-max(0,rnorm(1,xqs_p1,xqs_p1*biogeo_sd))
  #xqs_p1 is guessed to be less than qsp1lim so we need to trap cases where the jiggling violates this
  qsp1lim<-0.5
  if(xqs_p1>qsp1lim) {xqs_p1<-qsp1lim}
  if(xqs_p1<0) {xqs_p1<-0}

  xqs_p2<-max(0,rnorm(1,xqs_p2,xqs_p2*biogeo_sd))
  #xqs_p2 is guessed to be less than qsp2lim so we need to trap cases where the jiggling violates this
  qsp2lim<-0.001
  if(xqs_p2>qsp2lim) {xqs_p2<-qsp2lim}
  if(xqs_p2<0) {xqs_p2<-0}

  xqs_p3<-max(0,rnorm(1,xqs_p3,xqs_p3*biogeo_sd))
  #xqs_p3 is guessed to be less than qsp3lim so we need to trap cases where the jiggling violates this
  qsp3lim<-0.025
  if(xqs_p3<qsp3lim) {xqs_p3<-qsp3lim}
  if(xqs_p3<0) {xqs_p3<-0}

  xmsedt<-max(0,rnorm(1,xmsedt,xmsedt*biogeo_sd))


  if(xmsedt>0.015)        {xmsedt<-0.015}
  if((xqs_p1*xmsedt)>0.2) {xqs_p1 <- 0.2/xmsedt}


#xmsens is expected to be negative so convert to positive first...
xmsens<--1*xmsens
xmsens<-max(0,rnorm(1,xmsens,xmsens*biogeo_sd))
#convert back to negative
xmsens<--1*xmsens


xnsedt<-max(0,rnorm(1,xnsedt,xnsedt*biogeo_sd))

#xnsens is expected to be negative so convert to positive first...
xnsens<--1*xnsens
xnsens<-max(0,rnorm(1,xnsens,xnsens*biogeo_sd))
#convert back to negative
xnsens<--1*xnsens

xdsedt<-max(0,rnorm(1,xdsedt,xdsedt*biogeo_sd))

#xdsens is expected tpo be positive so nothing to be done here
xdsens<-max(0,rnorm(1,xdsens,xdsens*biogeo_sd))



xdsink_s<-max(0,rnorm(1,xdsink_s,xdsink_s*biogeo_sd))
xdsink_d<-max(0,rnorm(1,xdsink_d,xdsink_d*biogeo_sd))

if(xdsink_s>1) xdsink_s<-1
if(xdsink_d<0) xdsink_s<-0


xkelpdebris_det<- max(0,rnorm(1,xkelpdebris_det,xkelpdebris_det*biogeo_sd))


xxcorp_det<-max(0,rnorm(1,xxcorp_det,xxcorp_det*biogeo_sd))
if(xxcorp_det>0.5) xxcorp_det<-0.5


xdisc_corp<-max(0,rnorm(1,xdisc_corp,xdisc_corp*biogeo_sd))
if(xdisc_corp>0.7) xdisc_corp<-0.7

}

biogeostore<-list(xmt,xnst,xdst,xndt,xddt,xqs_p1,xqs_p2,xqs_p3,xmsedt,xmsens,xnsedt,xnsens,xdsedt,xdsens,xdsink_s,xdsink_d,xkelpdebris_det,xxcorp_det,xdisc_corp)
names(biogeostore)<-c("xmt","xnst","xdst","xndt","xddt","xqs_p1","xqs_p2","xqs_p3","xmsedt","xmsens","xnsedt","xnsens","xdsedt","xdsens","xdsink_s","xdsink_d","xkelpdebris_det","xxcorp_det","xdisc_corp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


if(mort_sd>0){

xxwave_kelp<-max(0,rnorm(1,xxwave_kelp,xxwave_kelp*mort_sd))


xxst<-max(0,rnorm(1,xxst,xxst*mort_sd))
xxdt<-max(0,rnorm(1,xxdt,xxdt*mort_sd))
xxomni<-max(0,rnorm(1,xxomni,xxomni*mort_sd))
xxcarn<-max(0,rnorm(1,xxcarn,xxcarn*mort_sd))
xxbenthslar<-max(0,rnorm(1,xxbenthslar,xxbenthslar*mort_sd))
xxbenthclar<-max(0,rnorm(1,xxbenthclar,xxbenthclar*mort_sd))
xxbenths<-max(0,rnorm(1,xxbenths,xxbenths*mort_sd))
xxbenthc<-max(0,rnorm(1,xxbenthc,xxbenthc*mort_sd))
  xxpfishlar<-max(0,rnorm(1,xxpfishlar,xxpfishlar*mort_sd))
  xxdfishlar<-max(0,rnorm(1,xxdfishlar,xxdfishlar*mort_sd))
  xxpfish<-max(0,rnorm(1,xxpfish,xxpfish*mort_sd))
  xxmfish<-max(0,rnorm(1,xxmfish,xxmfish*mort_sd))
  xxdfish<-max(0,rnorm(1,xxdfish,xxdfish*mort_sd))

	if (toppredlock == FALSE) {
		# not locked
		xxbird<-max(0,rnorm(1,xxbird,xxbird*mort_sd))
		xxseal<-max(0,rnorm(1,xxseal,xxseal*mort_sd))
		xxceta<-max(0,rnorm(1,xxceta,xxceta*mort_sd))
	}

}

mortstore<-list(xxwave_kelp,xxst,xxdt,xxomni,xxcarn,xxbenthslar,xxbenthclar,xxbenths,xxbenthc,xxpfishlar,xxdfishlar,xxpfish,xxmfish,xxdfish,xxbird,xxseal,xxceta)
names(mortstore)<-c("xxwave_kelp","xxst","xxdt","xxomni","xxcarn","xxbenthslar","xxbenthclar","xxbenths","xxbenthc","xxpfishlar","xxdfishlar","xxpfish","xxmfish","xxdfish","xxbird","xxseal","xxceta")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(ressd>0){

 xkelpshade<- max(0,rnorm(1,xkelpshade,xkelpshade*ressd))

	xwave_kelpdebris<- max(0,rnorm(1,xwave_kelpdebris,xwave_kelpdebris*ressd))

   xdfdp<-max(0,rnorm(1,xdfdp,xdfdp*ressd))




         xpfish_migcoef <- max(0,rnorm(1,xpfish_migcoef,xpfish_migcoef*ressd))
         xmfish_migcoef <- max(0,rnorm(1,xmfish_migcoef,xmfish_migcoef*ressd))
         xdfish_migcoef <- max(0,rnorm(1,xdfish_migcoef,xdfish_migcoef*ressd))
         xbird_migcoef <- max(0,rnorm(1,xbird_migcoef,xbird_migcoef*ressd))
         xseal_migcoef <- max(0,rnorm(1,xseal_migcoef,xseal_migcoef*ressd))
         xceta_migcoef <- max(0,rnorm(1,xceta_migcoef,xceta_migcoef*ressd))

if(xpfish_migcoef>0.01) xpfish_migcoef<-0.01
if(xmfish_migcoef>0.01) xmfish_migcoef<-0.01
if(xdfish_migcoef>0.01) xdfish_migcoef<-0.01
if(xbird_migcoef >0.01) xbird_migcoef <-0.01
if(xseal_migcoef >0.01) xseal_migcoef <-0.01
if(xceta_migcoef >0.01) xceta_migcoef <-0.01


xmax_exploitable_f_KP <- max(0,rnorm(1,xmax_exploitable_f_KP,xmax_exploitable_f_KP*ressd))
if(xmax_exploitable_f_KP>0.5) xmax_exploitable_f_KP<-0.5

xmax_exploitable_f_PF <- max(0,rnorm(1,xmax_exploitable_f_PF,xmax_exploitable_f_PF*ressd))
if(xmax_exploitable_f_PF>1) xmax_exploitable_f_PF<-1

xmax_exploitable_f_DF <- max(0,rnorm(1,xmax_exploitable_f_DF,xmax_exploitable_f_DF*ressd))
if(xmax_exploitable_f_DF>1) xmax_exploitable_f_DF<-1

xmax_exploitable_f_MF <- max(0,rnorm(1,xmax_exploitable_f_MF,xmax_exploitable_f_MF*ressd))
if(xmax_exploitable_f_MF>1) xmax_exploitable_f_MF<-1

xmax_exploitable_f_SB <- max(0,rnorm(1,xmax_exploitable_f_SB,xmax_exploitable_f_SB*ressd))
if(xmax_exploitable_f_SB>0.5) xmax_exploitable_f_SB<-0.5

xmax_exploitable_f_CB <- max(0,rnorm(1,xmax_exploitable_f_CB,xmax_exploitable_f_CB*ressd))
if(xmax_exploitable_f_CB>0.5) xmax_exploitable_f_CB<-0.5

xmax_exploitable_f_CZ <- max(0,rnorm(1,xmax_exploitable_f_CZ,xmax_exploitable_f_CZ*ressd))
if(xmax_exploitable_f_CZ>0.5) xmax_exploitable_f_CZ<-0.5

xmax_exploitable_f_BD <- max(0,rnorm(1,xmax_exploitable_f_BD,xmax_exploitable_f_BD*ressd))
if(xmax_exploitable_f_BD>0.5) xmax_exploitable_f_BD<-0.5

xmax_exploitable_f_SL <- max(0,rnorm(1,xmax_exploitable_f_SL,xmax_exploitable_f_SL*ressd))
if(xmax_exploitable_f_SL>0.5) xmax_exploitable_f_SL<-0.5

xmax_exploitable_f_CT <- max(0,rnorm(1,xmax_exploitable_f_CT,xmax_exploitable_f_CT*ressd))
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
		"annual_obj"	= annual_obj
	)
}

