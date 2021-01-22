#
# derive_model_target_results.R
#
#' derive annual target results and write to file
#'
#' returns annual target results
#'
#' @param model model object
#' @param build model build object
#' @param output model output
#' @param aggregates aggregated model output
#' @param annualtargetdata annual target data
#'
#' @return target results
#'
#' @noRd
#
# ------------------------------------------------------------------------------

derive_model_target_results <- function(model, build, output, aggregates, annualtargetdata) {

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
	kelpC			<- elt(output, "kelpC")
	kelpN			<- elt(output, "kelpN")
	fishp_i			<- elt(output, "fishp_i")
	fishp_o			<- elt(output, "fishp_o")
	fishd_i			<- elt(output, "fishd_i")
	fishd_o			<- elt(output, "fishd_o")
	omni_i			<- elt(output, "omni_i")
	omni_o			<- elt(output, "omni_o")
	carn_i			<- elt(output, "carn_i")
	carn_o			<- elt(output, "carn_o")
	phyt_si			<- elt(output, "phyt_si")
	phyt_so			<- elt(output, "phyt_so")
	nitrate_si		<- elt(output, "nitrate_si")
	nitrate_so		<- elt(output, "nitrate_so")
	ammonia_si		<- elt(output, "ammonia_si")
	ammonia_so		<- elt(output, "ammonia_so")
	nitrate_d		<- elt(output, "nitrate_d")
	kelpNprod_i		<- elt(output, "kelpNprod_i")
	kelpCprod_i		<- elt(output, "kelpCprod_i")
	kelpCexud_i		<- elt(output, "kelpCexud_i")
	fluxkelpdebris_beachexport <- elt(output, "fluxkelpdebris_beachexport")
	x_ammonia_s1		<- elt(output, "x_ammonia_s1")
	x_ammonia_s2		<- elt(output, "x_ammonia_s2")
	x_ammonia_s3		<- elt(output, "x_ammonia_s3")
	x_ammonia_d1		<- elt(output, "x_ammonia_d1")
	x_ammonia_d2		<- elt(output, "x_ammonia_d2")
	x_ammonia_d3		<- elt(output, "x_ammonia_d3")
	x_nitrate_s1		<- elt(output, "x_nitrate_s1")
	x_nitrate_s2		<- elt(output, "x_nitrate_s2")
	x_nitrate_s3		<- elt(output, "x_nitrate_s3")
	x_nitrate_d1		<- elt(output, "x_nitrate_d1")
	x_nitrate_d2		<- elt(output, "x_nitrate_d2")
	x_nitrate_d3		<- elt(output, "x_nitrate_d3")
	x_detritus_s1		<- elt(output, "x_detritus_s1")
	xR_detritus_s1		<- elt(output, "xR_detritus_s1")
	x_detritus_s2		<- elt(output, "x_detritus_s2")
	xR_detritus_s2		<- elt(output, "xR_detritus_s2")
	x_detritus_s3		<- elt(output, "x_detritus_s3")
	xR_detritus_s3		<- elt(output, "xR_detritus_s3")
	x_detritus_d1		<- elt(output, "x_detritus_d1")
	xR_detritus_d1		<- elt(output, "xR_detritus_d1")
	x_detritus_d2		<- elt(output, "x_detritus_d2")
	xR_detritus_d2		<- elt(output, "xR_detritus_d2")
	x_detritus_d3		<- elt(output, "x_detritus_d3")
	xR_detritus_d3		<- elt(output, "xR_detritus_d3")
	ammonia_d		<- elt(output, "ammonia_d")
	fluxomni_pfishlar	<- elt(output, "fluxomni_pfishlar")
	fluxomni_dfishlar	<- elt(output, "fluxomni_dfishlar")
	fluxomni_pfish		<- elt(output, "fluxomni_pfish")
	fluxomni_carn		<- elt(output, "fluxomni_carn")
	fluxpfishlar_pfish	<- elt(output, "fluxpfishlar_pfish")
	fluxpfishlar_dfish	<- elt(output, "fluxpfishlar_dfish")
	fluxdfishlar_pfish	<- elt(output, "fluxdfishlar_pfish")
	fluxdfishlar_dfish	<- elt(output, "fluxdfishlar_dfish")
	fluxpfish_dfish		<- elt(output, "fluxpfish_dfish")
	fluxdfish_dfish		<- elt(output, "fluxdfish_dfish")
	fluxdisc_dfish		<- elt(output, "fluxdisc_dfish")
	fluxbenths_dfish	<- elt(output, "fluxbenths_dfish")
	fluxbenthc_dfish	<- elt(output, "fluxbenthc_dfish")
	fluxcorp_bird		<- elt(output, "fluxcorp_bird")
	fluxcarn_bird		<- elt(output, "fluxcarn_bird")
	fluxbenths_bird		<- elt(output, "fluxbenths_bird")
	fluxbenthc_bird		<- elt(output, "fluxbenthc_bird")
	fluxpfish_bird		<- elt(output, "fluxpfish_bird")
	fluxdfish_bird		<- elt(output, "fluxdfish_bird")
	fluxmfish_bird		<- elt(output, "fluxmfish_bird")
	fluxdisc_bird		<- elt(output, "fluxdisc_bird")
	fluxcorp_seal		<- elt(output, "fluxcorp_seal")
	fluxdisc_seal		<- elt(output, "fluxdisc_seal")
	fluxcarn_seal		<- elt(output, "fluxcarn_seal")
	fluxbenths_seal		<- elt(output, "fluxbenths_seal")
	fluxbenthc_seal		<- elt(output, "fluxbenthc_seal")
	fluxbird_seal		<- elt(output, "fluxbird_seal")
	fluxpfish_seal		<- elt(output, "fluxpfish_seal")
	fluxdfish_seal		<- elt(output, "fluxdfish_seal")
	fluxmfish_seal		<- elt(output, "fluxmfish_seal")
	fluxomni_ceta		<- elt(output, "fluxomni_ceta")
	fluxcarn_ceta		<- elt(output, "fluxcarn_ceta")
	fluxdisc_ceta		<- elt(output, "fluxdisc_ceta")
	fluxpfish_ceta		<- elt(output, "fluxpfish_ceta")
	fluxdfish_ceta		<- elt(output, "fluxdfish_ceta")
	fluxmfish_ceta		<- elt(output, "fluxmfish_ceta")
	fluxbenths_ceta		<- elt(output, "fluxbenths_ceta")
	fluxbenthc_ceta		<- elt(output, "fluxbenthc_ceta")
	fluxbird_ceta		<- elt(output, "fluxbird_ceta")
	fluxseal_ceta		<- elt(output, "fluxseal_ceta")
	landkp_i		<- elt(output, "landkp_i")

	rivNITinflow		<- elt(output, "rivNITinflow")
	atmosNITinput		<- elt(output, "atmosNITinput_o") + elt(output, "atmosNITinput_i")

	# extract aggregates:
	rivDINinflow		<- elt(aggregates, "rivDINinflow")
	atmosDINinput		<- elt(aggregates, "atmosDINinput")
	fluxDINinflow		<- elt(aggregates, "fluxDINinflow")
	fluxDINoutflow		<- elt(aggregates, "fluxDINoutflow")
	omni			<- elt(aggregates, "omni")
	carn			<- elt(aggregates, "carn")
	benthslar		<- elt(aggregates, "benthslar")
	benthclar		<- elt(aggregates, "benthclar")
	benths			<- elt(aggregates, "benths")
	benthc			<- elt(aggregates, "benthc")
	fishplar		<- elt(aggregates, "fishplar")
	fishp			<- elt(aggregates, "fishp")
	fishdlar		<- elt(aggregates, "fishdlar")
	fishd			<- elt(aggregates, "fishd")
	fishm			<- elt(aggregates, "fishm")
	bird			<- elt(aggregates, "bird")
	seal			<- elt(aggregates, "seal")
	ceta			<- elt(aggregates, "ceta")
	netpprod		<- elt(aggregates, "netpprod")
	s_nitrate		<- elt(aggregates, "s_nitrate")
	omnigrossprod		<- elt(aggregates, "omnigrossprod")
	carngrossprod		<- elt(aggregates, "carngrossprod")
	pfishgrossprod		<- elt(aggregates, "pfishgrossprod")
	pfishlargrossprod	<- elt(aggregates, "pfishlargrossprod")
	dfishgrossprod		<- elt(aggregates, "dfishgrossprod")
	dfishlargrossprod	<- elt(aggregates, "dfishlargrossprod")
	mfishgrossprod		<- elt(aggregates, "mfishgrossprod")
	benthsgrossprod		<- elt(aggregates, "benthsgrossprod")
	benthslargrossprod	<- elt(aggregates, "benthslargrossprod")
	benthcgrossprod		<- elt(aggregates, "benthcgrossprod")
	benthclargrossprod	<- elt(aggregates, "benthclargrossprod")
	birdnetprod		<- elt(aggregates, "birdnetprod")
	sealnetprod		<- elt(aggregates, "sealnetprod")
	cetanetprod		<- elt(aggregates, "cetanetprod")
	s_ammonia		<- elt(aggregates, "s_ammonia")
	landp			<- elt(aggregates, "landp")
	landd			<- elt(aggregates, "landd")
	landm			<- elt(aggregates, "landm")
	landsb			<- elt(aggregates, "landsb")
	landcb			<- elt(aggregates, "landcb")
	landcz			<- elt(aggregates, "landcz")
	discdem			<- elt(aggregates, "discdem")
	discbd			<- elt(aggregates, "discbd")
	discsl			<- elt(aggregates, "discsl")
	discct			<- elt(aggregates, "discct")
	landct			<- elt(aggregates, "landct")
	wcdenitrif		<- elt(aggregates, "wcdenitrif")
	seddenitrif		<- elt(aggregates, "seddenitrif")


#======================================================================

#Requires the prior loading of the dataframe annualtargetdata which should contain the following columns 

#Add a column to the input target dataframe to hold to corresponding derived model values
#Column 1 = Target value
#Column 2 = sd of target value
#Column 3 = switch to determine whether to be included in likelihood evaluation (1=yes, 0 = no)
#Column 4 = name of target value
#Column 5 = units of target value
#Column 6 = description of target value
#Column 7 = Region
#Column 8 = Time period
#Column 9 = source

#======================================================================

#Make a new dataframe and copy into it the contents of annualtargetdata but with a gap in
#the column sequence at index 3 to hold the model results which will be generated in this programme

opt_results<-annualtargetdata[,1:6]
opt_results[,3]<-NA
opt_results[,4]<-annualtargetdata[,3]
opt_results[,5]<-annualtargetdata[,4]
opt_results[,6]<-annualtargetdata[,5]
opt_results[,7]<-annualtargetdata[,6]

names(opt_results) <- c(names(annualtargetdata[1:2]),
                                     "Model_data",
                                     names(annualtargetdata[3:6]))

#The dataframe opt_results will be needed by the routine that derives the likelihood
#of the model output given the observed data

#The routine in this file takes some of the information in opt-results and outputs a
#csv file of just the model results with suitably descritive rownames and the units for
#each variable

#======================================================================




xvolume_si<-si_depth*x_shallowprop
xvolume_so<-so_depth*(1-x_shallowprop)
xd_volume<-d_depth*(1-x_shallowprop)

xs_volume <- xvolume_si + xvolume_so



#First some proportions that are needed for deriving results

total_porewater_vol <- (x_area_s1*x_depth_s1*x_poros_s1 + x_area_s2*x_depth_s2*x_poros_s2 +x_area_s3*x_depth_s3*x_poros_s3 +
                      + x_area_d1*x_depth_d1*x_poros_d1 + x_area_d2*x_depth_d2*x_poros_d2 +x_area_d3*x_depth_d3*x_poros_d3)
x_porevol_s1<-(x_area_s1*x_depth_s1*x_poros_s1)/total_porewater_vol
x_porevol_s2<-(x_area_s2*x_depth_s2*x_poros_s2)/total_porewater_vol
x_porevol_s3<-(x_area_s3*x_depth_s3*x_poros_s3)/total_porewater_vol
x_porevol_d1<-(x_area_d1*x_depth_d1*x_poros_d1)/total_porewater_vol
x_porevol_d2<-(x_area_d2*x_depth_d2*x_poros_d2)/total_porewater_vol
x_porevol_d3<-(x_area_d3*x_depth_d3*x_poros_d3)/total_porewater_vol
x_porevol_s <- x_porevol_s1 +x_porevol_s2 + x_porevol_s3
x_porevol_d <- (1-x_porevol_s)


total_sedsolid_vol <- (x_area_s1*x_depth_s1*(1-x_poros_s1) + x_area_s2*x_depth_s2*(1-x_poros_s2) +x_area_s3*x_depth_s3*(1-x_poros_s3) +
                      + x_area_d1*x_depth_d1*(1-x_poros_d1) + x_area_d2*x_depth_d2*(1-x_poros_d2) +x_area_d3*x_depth_d3*(1-x_poros_d3))
x_sedsolidvol_s1<-(x_area_s1*x_depth_s1*(1-x_poros_s1))/total_sedsolid_vol
x_sedsolidvol_s2<-(x_area_s2*x_depth_s2*(1-x_poros_s2))/total_sedsolid_vol
x_sedsolidvol_s3<-(x_area_s3*x_depth_s3*(1-x_poros_s3))/total_sedsolid_vol
x_sedsolidvol_d1<-(x_area_d1*x_depth_d1*(1-x_poros_d1))/total_sedsolid_vol
x_sedsolidvol_d2<-(x_area_d2*x_depth_d2*(1-x_poros_d2))/total_sedsolid_vol
x_sedsolidvol_d3<-(x_area_d3*x_depth_d3*(1-x_poros_d3))/total_sedsolid_vol
x_sedsolidvol_s <- x_sedsolidvol_s1 +x_sedsolidvol_s2 + x_sedsolidvol_s3
x_sedsolidvol_d <- (1-x_sedsolidvol_s)

#----------------------------------

#Some transport and geochemical fluxes

sumriverDINinflow<-rivDINinflow[ndays-90]-rivDINinflow[((nyears-1)*360+1+89)]       # April - September inclusive
sumatmosDINinput<-(atmosDINinput[ndays-90]-atmosDINinput[((nyears-1)*360+1+89)])    # April - September inclusive
sumDINinflow<-(fluxDINinflow[ndays-90]-fluxDINinflow[((nyears-1)*360+1+89)])        # April - September inclusive
sumDINoutflow<-(fluxDINoutflow[ndays-90]-fluxDINoutflow[((nyears-1)*360+1+89)])     # April - September inclusive

sumriverNITinflow<-rivNITinflow[ndays-90]-rivNITinflow[((nyears-1)*360+1+89)]       # April - September inclusive
sumatmosNITinput<-(atmosNITinput[ndays-90]-atmosNITinput[((nyears-1)*360+1+89)])    # April - September inclusive


#Calculate whole domain annual average mass of a range of state variables
#Except fo rkelp, these variables ar the aggregares of the inshore and offshore mass data

aamass_omni<-mean(omni[((nyears-1)*360+1):ndays])
aamass_carn<-mean(carn[((nyears-1)*360+1):ndays])
aamass_benthslar<-mean(benthslar[((nyears-1)*360+1):ndays])
aamass_benthclar<-mean(benthclar[((nyears-1)*360+1):ndays])
aamass_benths<-mean(benths[((nyears-1)*360+1):ndays])
aamass_benthc<-mean(benthc[((nyears-1)*360+1):ndays])
aamass_fishplar<-mean(fishplar[((nyears-1)*360+1):ndays])
aamass_fishp<-mean(fishp[((nyears-1)*360+1):ndays])
aamass_fishdlar<-mean(fishdlar[((nyears-1)*360+1):ndays])
aamass_fishd<-mean(fishd[((nyears-1)*360+1):ndays])
aamass_fishm<-mean(fishm[((nyears-1)*360+1):ndays])
aamass_bird<-mean(bird[((nyears-1)*360+1):ndays])
aamass_seal<-mean(seal[((nyears-1)*360+1):ndays])
aamass_ceta<-mean(ceta[((nyears-1)*360+1):ndays])


#And concentrations and area densities for inshore and offshore zones respectively...

aamassCmolar_kelp<-((mean(kelpC[((nyears-1)*360+1):ndays]))/x_area_s0)            # mMC/m2 in forest<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
aamassCgram_kelp<- ((mean(kelpC[((nyears-1)*360+1):ndays]))/x_area_s0)*12/1000    # gC/m2 in-forest  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
aamassN_kelp<-     ((mean(kelpN[((nyears-1)*360+1):ndays]))/x_area_s0)            # mMN/m2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

aaconc_fishp_i<-(mean(fishp_i[((nyears-1)*360+1):ndays]))/x_shallowprop
aaconc_fishp_o<-(mean(fishp_o[((nyears-1)*360+1):ndays]))/(1-x_shallowprop)

aaconc_fishd_i<-(mean(fishd_i[((nyears-1)*360+1):ndays]))/x_shallowprop
aaconc_fishd_o<-(mean(fishd_o[((nyears-1)*360+1):ndays]))/(1-x_shallowprop)

aaconc_omni_i<-(mean(omni_i[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_omni_o<-(mean(omni_o[((nyears-1)*360+1):ndays]))/(xvolume_so+xd_volume)

aaconc_carn_i<-(mean(carn_i[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_carn_o<-(mean(carn_o[((nyears-1)*360+1):ndays]))/(xvolume_so+xd_volume)

aaconc_phyt_si<-(mean(phyt_si[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_phyt_so<-(mean(phyt_so[((nyears-1)*360+1):ndays]))/xvolume_so

aaconc_nitrate_si<-(mean(nitrate_si[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_nitrate_so<-(mean(nitrate_so[((nyears-1)*360+1):ndays]))/xvolume_so

aaconc_ammonia_si<-(mean(ammonia_si[((nyears-1)*360+1):ndays]))/xvolume_si
aaconc_ammonia_so<-(mean(ammonia_so[((nyears-1)*360+1):ndays]))/xvolume_so


#And annual production rates.....

TAPP <- netpprod[ndays]-netpprod[((nyears-1)*360+1)]
#showall("netpprod", netpprod)
#showall("TAPP", TAPP)

MMP<-(max(s_nitrate[((nyears-1)*360+1):ndays]+nitrate_d[((nyears-1)*360+1):ndays])) - (min(s_nitrate[((nyears-1)*360+1):ndays]+nitrate_d[((nyears-1)*360+1):ndays]))
PNP <- MMP+sumriverNITinflow+sumatmosNITinput

KelpNP <- ((kelpNprod_i[ndays]-kelpNprod_i[((nyears-1)*360+1)])/x_area_s0)          # Nitrogen production - Units mMN/m2 in forest/y
KelpCP <- ((kelpCprod_i[ndays]-kelpCprod_i[((nyears-1)*360+1)])/x_area_s0)*12/1000  # Carbon gross production - Units gC/m2 in forest/y
KelpCE <- ((kelpCexud_i[ndays]-kelpCexud_i[((nyears-1)*360+1)])/x_area_s0)*12/1000  # Carbon exudation - Units gC/m2 in forest/y
prop_exud_C_kelp  <- KelpCE/KelpCP
kelp_NC           <- aamassN_kelp/aamassCmolar_kelp

Kelp_netCP <- KelpCP-KelpCE                                                             # Carbon net prodtion -  - Units gC/m2 in forest/y

kelp_beachcast<-((fluxkelpdebris_beachexport[ndays]-fluxkelpdebris_beachexport[((nyears-1)*360+1)])/x_area_s0)          # Units mMN/m2 of forest/y

kelp_beachcast_per_annProd <- kelp_beachcast/KelpNP

OmnizoogrossP <- omnigrossprod[ndays]-omnigrossprod[((nyears-1)*360+1)]

CarnzoogrossP <- carngrossprod[ndays]-carngrossprod[((nyears-1)*360+1)]

PFishgrossP<-pfishgrossprod[ndays]-pfishgrossprod[((nyears-1)*360+1)]
PFishlargrossP<-pfishlargrossprod[ndays]-pfishlargrossprod[((nyears-1)*360+1)]

DFishgrossP<-dfishgrossprod[ndays]-dfishgrossprod[((nyears-1)*360+1)]
DFishlargrossP<-dfishlargrossprod[ndays]-dfishlargrossprod[((nyears-1)*360+1)]

MFishgrossP<-mfishgrossprod[ndays]-mfishgrossprod[((nyears-1)*360+1)]

BenthsgrossP<-benthsgrossprod[ndays]-benthsgrossprod[((nyears-1)*360+1)]
BenthslargrossP<-benthslargrossprod[ndays]-benthslargrossprod[((nyears-1)*360+1)]

BenthcgrossP<-benthcgrossprod[ndays]-benthcgrossprod[((nyears-1)*360+1)]
BenthclargrossP<-benthclargrossprod[ndays]-benthclargrossprod[((nyears-1)*360+1)]

BirdnetP<-birdnetprod[ndays]-birdnetprod[((nyears-1)*360+1)]

SealnetP<-sealnetprod[ndays]-sealnetprod[((nyears-1)*360+1)]

CetanetP<-cetanetprod[ndays]-cetanetprod[((nyears-1)*360+1)]


kelpC_pb <- KelpCP/aamassCgram_kelp

benslar_pb<-BenthslargrossP/aamass_benthslar
benclar_pb<-BenthclargrossP/aamass_benthclar

benc_pb<-BenthcgrossP/aamass_benthc
bens_pb<-BenthsgrossP/aamass_benths
omni_pb<-OmnizoogrossP/aamass_omni
carn_pb<-CarnzoogrossP/aamass_carn
pfishlar_pb<-PFishlargrossP/aamass_fishplar
dfishlar_pb<-DFishlargrossP/aamass_fishdlar
pfish_pb<-PFishgrossP/aamass_fishp
dfish_pb<-DFishgrossP/aamass_fishd

mfish_pb<-MFishgrossP/aamass_fishm

bird_pb<-BirdnetP/aamass_bird
seal_pb<-SealnetP/aamass_seal
ceta_pb<-CetanetP/aamass_ceta





aaconc_x_ammonia_s1<-0
aaconc_x_ammonia_s2<-0
aaconc_x_ammonia_s3<-0
if(x_area_s1>0) {aaconc_x_ammonia_s1  <-  (mean(x_ammonia_s1[((nyears-1)*360+1):ndays]))/(x_area_s1*x_depth_s1*(x_poros_s1))} # mMN/m3 porewater
if(x_area_s2>0) {aaconc_x_ammonia_s2  <-  (mean(x_ammonia_s2[((nyears-1)*360+1):ndays]))/(x_area_s2*x_depth_s2*(x_poros_s2))} # mMN/m3 porewater
if(x_area_s3>0) {aaconc_x_ammonia_s3  <-  (mean(x_ammonia_s3[((nyears-1)*360+1):ndays]))/(x_area_s3*x_depth_s3*(x_poros_s3))} # mMN/m3 porewater

aaconc_x_ammonia_d1<-0
aaconc_x_ammonia_d2<-0
aaconc_x_ammonia_d3<-0
if(x_area_d1>0) {aaconc_x_ammonia_d1  <-  (mean(x_ammonia_d1[((nyears-1)*360+1):ndays]))/(x_area_d1*x_depth_d1*(x_poros_d1))} # mMN/m3 porewater
if(x_area_d2>0) {aaconc_x_ammonia_d2  <-  (mean(x_ammonia_d2[((nyears-1)*360+1):ndays]))/(x_area_d2*x_depth_d2*(x_poros_d2))} # mMN/m3 porewater
if(x_area_d3>0) {aaconc_x_ammonia_d3  <-  (mean(x_ammonia_d3[((nyears-1)*360+1):ndays]))/(x_area_d3*x_depth_d3*(x_poros_d3))} # mMN/m3 porewater


aaconc_x_nitrate_s1<-0
aaconc_x_nitrate_s2<-0
aaconc_x_nitrate_s3<-0
if(x_area_s1>0) {aaconc_x_nitrate_s1  <-  (mean(x_nitrate_s1[((nyears-1)*360+1):ndays]))/(x_area_s1*x_depth_s1*(x_poros_s1))} # mMN/m3 porewater
if(x_area_s2>0) {aaconc_x_nitrate_s2  <-  (mean(x_nitrate_s2[((nyears-1)*360+1):ndays]))/(x_area_s2*x_depth_s2*(x_poros_s2))} # mMN/m3 porewater
if(x_area_s3>0) {aaconc_x_nitrate_s3  <-  (mean(x_nitrate_s3[((nyears-1)*360+1):ndays]))/(x_area_s3*x_depth_s3*(x_poros_s3))} # mMN/m3 porewater

aaconc_x_nitrate_d1<-0
aaconc_x_nitrate_d2<-0
aaconc_x_nitrate_d3<-0
if(x_area_d1>0) {aaconc_x_nitrate_d1  <-  (mean(x_nitrate_d1[((nyears-1)*360+1):ndays]))/(x_area_d1*x_depth_d1*(x_poros_d1))} # mMN/m3 porewater
if(x_area_d2>0) {aaconc_x_nitrate_d2  <-  (mean(x_nitrate_d2[((nyears-1)*360+1):ndays]))/(x_area_d2*x_depth_d2*(x_poros_d2))} # mMN/m3 porewater
if(x_area_d3>0) {aaconc_x_nitrate_d3  <-  (mean(x_nitrate_d3[((nyears-1)*360+1):ndays]))/(x_area_d3*x_depth_d3*(x_poros_d3))} # mMN/m3 porewater




#This calculates the model target results assuming target of the average over all habitats in shallow and deep
#aaconc_x_ammonia_s<-0
#if(x_shallowprop>0) {aaconc_x_ammonia_s <- (x_porevol_s1*aaconc_x_ammonia_s1 + x_porevol_s2*aaconc_x_ammonia_s2 + x_porevol_s3*aaconc_x_ammonia_s3)/x_porevol_s }
#aaconc_x_ammonia_d <- (x_porevol_d1*aaconc_x_ammonia_d1 + x_porevol_d2*aaconc_x_ammonia_d2 + x_porevol_d3*aaconc_x_ammonia_d3)/(1-x_porevol_s)


#This calculates the model target results assuming _s = sandy sediment and _d = muddy sediment
aaconc_x_ammonia_s <- (x_porevol_s2*aaconc_x_ammonia_s2 + x_porevol_d2*aaconc_x_ammonia_d2)/(x_porevol_s2+x_porevol_d2)
aaconc_x_ammonia_d <- (x_porevol_s1*aaconc_x_ammonia_s1 + x_porevol_d1*aaconc_x_ammonia_d1)/(x_porevol_s1+x_porevol_d1)

aaconc_x_nitrate_s <- (x_porevol_s2*aaconc_x_nitrate_s2 + x_porevol_d2*aaconc_x_nitrate_d2)/(x_porevol_s2+x_porevol_d2)
aaconc_x_nitrate_d <- (x_porevol_s1*aaconc_x_nitrate_s1 + x_porevol_d1*aaconc_x_nitrate_d1)/(x_porevol_s1+x_porevol_d1)

aaconc_x_TON_s1<-0
aaconc_x_TON_s2<-0
aaconc_x_TON_s3<-0
if(x_poros_s1>0 && x_area_s1>0) {aaconc_x_TON_s1  <-  100*(14/1000)*(mean(x_detritus_s1[((nyears-1)*360+1):ndays]+xR_detritus_s1[((nyears-1)*360+1):ndays]))/(x_area_s1*x_depth_s1*(1-x_poros_s1)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
if(x_poros_s2>0 && x_area_s2>0) {aaconc_x_TON_s2  <-  100*(14/1000)*(mean(x_detritus_s2[((nyears-1)*360+1):ndays]+xR_detritus_s2[((nyears-1)*360+1):ndays]))/(x_area_s2*x_depth_s2*(1-x_poros_s2)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
if(x_poros_s3>0 && x_area_s3>0) {aaconc_x_TON_s3  <-  100*(14/1000)*(mean(x_detritus_s3[((nyears-1)*360+1):ndays]+xR_detritus_s3[((nyears-1)*360+1):ndays]))/(x_area_s3*x_depth_s3*(1-x_poros_s3)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
#This converts the sediment detritus into units of %N by dry wt (100*gN/g-drysediment) (density of dry solid matter = 2.65g/cm3)

aaconc_x_TON_d1<-0
aaconc_x_TON_d2<-0
aaconc_x_TON_d3<-0
if(x_poros_d1>0 && x_area_d1>0) {aaconc_x_TON_d1  <-  100*(14/1000)*(mean(x_detritus_d1[((nyears-1)*360+1):ndays]+xR_detritus_d1[((nyears-1)*360+1):ndays]))/(x_area_d1*x_depth_d1*(1-x_poros_d1)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
if(x_poros_d2>0 && x_area_d2>0) {aaconc_x_TON_d2  <-  100*(14/1000)*(mean(x_detritus_d2[((nyears-1)*360+1):ndays]+xR_detritus_d2[((nyears-1)*360+1):ndays]))/(x_area_d2*x_depth_d2*(1-x_poros_d2)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
if(x_poros_d3>0 && x_area_d3>0) {aaconc_x_TON_d3  <-  100*(14/1000)*(mean(x_detritus_d3[((nyears-1)*360+1):ndays]+xR_detritus_d3[((nyears-1)*360+1):ndays]))/(x_area_d3*x_depth_d3*(1-x_poros_d3)*(2650*1000)) } # N-mg/mg-sed dry wt as a percentage
#This converts the sediment detritus into units of %N by dry wt (100*gN/g-drysediment) (density of dry solid matter = 2.65g/cm3)

#This calculates the model target results assuming target of the average over all habitats in shallow and deep
#aaconc_x_TON_s<-0
#if(x_shallowprop>0) {aaconc_x_TON_s <- (x_sedsolidvol_s1*aaconc_x_TON_s1 + x_sedsolidvol_s2*aaconc_x_TON_s2 + x_sedsolidvol_s3*aaconc_x_ammonia_s3)/x_sedsolidvol_s }
#aaconc_x_TON_d <- (x_sedsolidvol_d1*aaconc_x_TON_d1 + x_sedsolidvol_d2*aaconc_x_TON_d2 + x_sedsolidvol_d3*aaconc_x_TON_d3)/(1-x_sedsolidvol_s)

#This calculates the model target results assuming _s = sandy sediment and _d = muddy sediment
aaconc_x_TON_s <- (x_sedsolidvol_s2*aaconc_x_TON_s2 + x_sedsolidvol_d2*aaconc_x_TON_d2)/(x_sedsolidvol_s2+x_sedsolidvol_d2)
aaconc_x_TON_d <- (x_sedsolidvol_s1*aaconc_x_TON_s1 + x_sedsolidvol_d1*aaconc_x_TON_d1)/(x_sedsolidvol_s1+x_sedsolidvol_d1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Need to find the max monthly average value of the benthic larvae
finaly_benthslar<-benthslar[((nyears-1)*360+1):ndays]
finaly_benthclar<-benthclar[((nyears-1)*360+1):ndays]
ma_benthslar<-rep(0,12)
ma_benthclar<-rep(0,12)
for(jj in 1:12){
ma_benthslar[jj]<-mean(finaly_benthslar[(((jj-1)*30)+1) : (jj*30)])
ma_benthclar[jj]<-mean(finaly_benthclar[(((jj-1)*30)+1) : (jj*30)])
}
maconc_benthslar<-max(ma_benthslar)/(xs_volume+xd_volume)
maconc_benthclar<-max(ma_benthclar)/(xs_volume+xd_volume)
#Here the units are mMN/m3 to correspond with the CPR data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Set the days which correspond to winter - version which works with only one year of model output
NDJF_start_p2 <- ((nyears)*360+1)-60    # Nov-Dec start
NDJF_end_p2   <- ((nyears)*360+1)       # Nov-Dec end
NDJF_start_p1 <- ((nyears-1)*360+1)     # Jan-Feb start
NDJF_end_p1   <- ((nyears-1)*360+1)+60  # Jan-Feb end
winterdays<-c(NDJF_start_p1:NDJF_end_p1,NDJF_start_p2:NDJF_end_p2)

#Set the days which correspond to summer
MJJA_start <- ((nyears-1)*360+1)+120
MJJA_end   <- ndays-120
summerdays<- (MJJA_start : MJJA_end)

#Winter and summer average nutrient concentartions in the water column

waconc_nitrate_s <- (mean(s_nitrate[winterdays]))/xs_volume
waconc_ammonia_s <- (mean(s_ammonia[winterdays]))/xs_volume

saconc_nitrate_s <- (mean(s_nitrate[summerdays]))/xs_volume
saconc_ammonia_s <- (mean(s_ammonia[summerdays]))/xs_volume

waconc_nitrate_d <- (mean(nitrate_d[winterdays]))/xd_volume
waconc_ammonia_d <- (mean(ammonia_d[winterdays]))/xd_volume

saconc_nitrate_d <- (mean(nitrate_d[summerdays]))/xd_volume
saconc_ammonia_d <- (mean(ammonia_d[summerdays]))/xd_volume

#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Various feeding fluxes...

omni_pfishlarflux<-fluxomni_pfishlar[ndays]-fluxomni_pfishlar[((nyears-1)*360+1)]
omni_dfishlarflux<-fluxomni_dfishlar[ndays]-fluxomni_dfishlar[((nyears-1)*360+1)]
omni_pfishflux<-fluxomni_pfish[ndays]-fluxomni_pfish[((nyears-1)*360+1)]

mzoofishflux<-omni_pfishlarflux+omni_dfishlarflux+omni_pfishflux

omni_carnflux<-fluxomni_carn[ndays]-fluxomni_carn[((nyears-1)*360+1)]

pfishlar_pfishflux<-fluxpfishlar_pfish[ndays]-fluxpfishlar_pfish[((nyears-1)*360+1)]
pfishlar_dfishflux<-fluxpfishlar_dfish[ndays]-fluxpfishlar_dfish[((nyears-1)*360+1)]
dfishlar_pfishflux<-fluxdfishlar_pfish[ndays]-fluxdfishlar_pfish[((nyears-1)*360+1)]
dfishlar_dfishflux<-fluxdfishlar_dfish[ndays]-fluxdfishlar_dfish[((nyears-1)*360+1)]
pfish_dfishflux<-fluxpfish_dfish[ndays]-fluxpfish_dfish[((nyears-1)*360+1)]
dfish_dfishflux<-fluxdfish_dfish[ndays]-fluxdfish_dfish[((nyears-1)*360+1)]
disc_dfishflux<-fluxdisc_dfish[ndays]-fluxdisc_dfish[((nyears-1)*360+1)]

pfishfishflux<-pfishlar_pfishflux+pfishlar_dfishflux+pfish_dfishflux
dfishfishflux<-dfishlar_pfishflux+dfishlar_dfishflux+dfish_dfishflux

benths_dfishflux<-fluxbenths_dfish[ndays]-fluxbenths_dfish[((nyears-1)*360+1)]
benthc_dfishflux<-fluxbenthc_dfish[ndays]-fluxbenthc_dfish[((nyears-1)*360+1)]
benfishflux<-benths_dfishflux+benthc_dfishflux


corp_birdflux<-fluxcorp_bird[ndays]-fluxcorp_bird[((nyears-1)*360+1)]
carn_birdflux<-fluxcarn_bird[ndays]-fluxcarn_bird[((nyears-1)*360+1)]
benths_birdflux<-fluxbenths_bird[ndays]-fluxbenths_bird[((nyears-1)*360+1)]
benthc_birdflux<-fluxbenthc_bird[ndays]-fluxbenthc_bird[((nyears-1)*360+1)]
pfish_birdflux<-fluxpfish_bird[ndays]-fluxpfish_bird[((nyears-1)*360+1)]
dfish_birdflux<-fluxdfish_bird[ndays]-fluxdfish_bird[((nyears-1)*360+1)]
mfish_birdflux<-fluxmfish_bird[ndays]-fluxmfish_bird[((nyears-1)*360+1)]
disc_birdflux<-fluxdisc_bird[ndays]-fluxdisc_bird[((nyears-1)*360+1)]
total_birdflux<-  pfish_birdflux + dfish_birdflux + mfish_birdflux + disc_birdflux +
                + corp_birdflux + carn_birdflux + benths_birdflux + benthc_birdflux


corp_sealflux <- fluxcorp_seal[ndays]-fluxcorp_seal[((nyears-1)*360+1)]
disc_sealflux <-fluxdisc_seal[ndays]-fluxdisc_seal[((nyears-1)*360+1)]
carn_sealflux <- fluxcarn_seal[ndays]-fluxcarn_seal[((nyears-1)*360+1)]
benths_sealflux <- fluxbenths_seal[ndays]-fluxbenths_seal[((nyears-1)*360+1)]
benthc_sealflux <- fluxbenthc_seal[ndays]-fluxbenthc_seal[((nyears-1)*360+1)]
bird_sealflux <- fluxbird_seal[ndays]-fluxbird_seal[((nyears-1)*360+1)]
pfish_sealflux<-fluxpfish_seal[ndays]-fluxpfish_seal[((nyears-1)*360+1)]
dfish_sealflux<-fluxdfish_seal[ndays]-fluxdfish_seal[((nyears-1)*360+1)]
mfish_sealflux<-fluxmfish_seal[ndays]-fluxmfish_seal[((nyears-1)*360+1)]
total_sealflux<-  pfish_sealflux + dfish_sealflux + mfish_sealflux +
                + corp_sealflux + disc_sealflux + carn_sealflux + bird_sealflux +
                + benths_sealflux + benthc_sealflux


disc_cetaflux<-fluxdisc_ceta[ndays]-fluxdisc_ceta[((nyears-1)*360+1)]
omni_cetaflux<-fluxomni_ceta[ndays]-fluxomni_ceta[((nyears-1)*360+1)]
carn_cetaflux<-fluxcarn_ceta[ndays]-fluxcarn_ceta[((nyears-1)*360+1)]
pfish_cetaflux<-fluxpfish_ceta[ndays]-fluxpfish_ceta[((nyears-1)*360+1)]
dfish_cetaflux<-fluxdfish_ceta[ndays]-fluxdfish_ceta[((nyears-1)*360+1)]
mfish_cetaflux<-fluxmfish_ceta[ndays]-fluxmfish_ceta[((nyears-1)*360+1)]
benths_cetaflux<-fluxbenths_ceta[ndays]-fluxbenths_ceta[((nyears-1)*360+1)]
benthc_cetaflux<-fluxbenthc_ceta[ndays]-fluxbenthc_ceta[((nyears-1)*360+1)]
bird_cetaflux<-fluxbird_ceta[ndays]-fluxbird_ceta[((nyears-1)*360+1)]
seal_cetaflux<-fluxseal_ceta[ndays]-fluxseal_ceta[((nyears-1)*360+1)]
total_cetaflux<-   pfish_cetaflux + dfish_cetaflux + mfish_cetaflux +
                 + omni_cetaflux + carn_cetaflux + disc_cetaflux +
                 + benths_cetaflux + benthc_cetaflux + bird_cetaflux + seal_cetaflux 


#Landings, discards and bycatch

FishpLand<-landp[ndays]-landp[((nyears-1)*360+1)]
FishdLand<-landd[ndays]-landd[((nyears-1)*360+1)]
FishmLand<-landm[ndays]-landm[((nyears-1)*360+1)]

FishbsLand<-(landsb[ndays]-landsb[((nyears-1)*360+1)])
FishbcLand<-(landcb[ndays]-landcb[((nyears-1)*360+1)])

FishzcLand<-(landcz[ndays]-landcz[((nyears-1)*360+1)])

FishkpLand<-(landkp_i[ndays]-landkp_i[((nyears-1)*360+1)])

FishdDiscard<-discdem[ndays]-discdem[((nyears-1)*360+1)]

BDDiscard<-discbd[ndays]-discbd[((nyears-1)*360+1)]

SLDiscard<-discsl[ndays]-discsl[((nyears-1)*360+1)]

CTDiscard<-discct[ndays]-discct[((nyears-1)*360+1)]

FishctLand<-landct[ndays]-landct[((nyears-1)*360+1)]


#Some other geochemical fluxes

WCDenitrif<-(wcdenitrif[ndays]-wcdenitrif[((nyears-1)*360+1)])
SedDenitrif<-(seddenitrif[ndays]-seddenitrif[((nyears-1)*360+1)])
Denitrif<-WCDenitrif+SedDenitrif


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Now drop all the derived values into the correct rows in the target dataframe
#-----------------------------------------------------------------------------

opt_results$Model_data[which(opt_results$Name=="Obs_TAPP")] <- TAPP

opt_results$Model_data[which(opt_results$Name=="Obs_NP")]  <- PNP

opt_results$Model_data[which(opt_results$Name=="Obs_KelpP")] <- Kelp_netCP      # NET production

opt_results$Model_data[which(opt_results$Name=="Obs_OmnizooP")] <- OmnizoogrossP 

opt_results$Model_data[which(opt_results$Name=="Obs_CarnzooP")] <- CarnzoogrossP

opt_results$Model_data[which(opt_results$Name=="Obs_PFishP")]  <- PFishgrossP+PFishlargrossP

opt_results$Model_data[which(opt_results$Name=="Obs_DFishP")]  <- DFishgrossP+DFishlargrossP

opt_results$Model_data[which(opt_results$Name=="Obs_BensuspP")] <- BenthsgrossP

opt_results$Model_data[which(opt_results$Name=="Obs_BencarnP")] <- BenthcgrossP

opt_results$Model_data[which(opt_results$Name=="Obs_birdP")] <- BirdnetP

opt_results$Model_data[which(opt_results$Name=="Obs_sealP")] <- SealnetP

opt_results$Model_data[which(opt_results$Name=="Obs_cetaP")] <- CetanetP

opt_results$Model_data[which(opt_results$Name=="Obs_maxbenthslar")] <- maconc_benthslar

opt_results$Model_data[which(opt_results$Name=="Obs_maxbenthclar")] <- maconc_benthclar

opt_results$Model_data[which(opt_results$Name=="Obs_Conpfishfish")] <- pfishfishflux

opt_results$Model_data[which(opt_results$Name=="Obs_Condfishfish")] <- dfishfishflux

opt_results$Model_data[which(opt_results$Name=="Obs_Conzoofish")] <- mzoofishflux

opt_results$Model_data[which(opt_results$Name=="Obs_Conzoocarnz")] <- omni_carnflux

opt_results$Model_data[which(opt_results$Name=="Obs_Conbenfish")] <- benfishflux


opt_results$Model_data[which(opt_results$Name=="Obs_Contotal_bird")] <- total_birdflux

opt_results$Model_data[which(opt_results$Name=="Obs_Proppfishbird")] <- pfish_birdflux/total_birdflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propdfishbird")] <- dfish_birdflux/total_birdflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propmfishbird")] <- mfish_birdflux/total_birdflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propdiscbird")]  <- disc_birdflux/total_birdflux




opt_results$Model_data[which(opt_results$Name=="Obs_Contotal_seal")] <- total_sealflux

opt_results$Model_data[which(opt_results$Name=="Obs_Proppfishseal")] <- pfish_sealflux/total_sealflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propdfishseal")] <- dfish_sealflux/total_sealflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propmfishseal")] <- mfish_sealflux/total_sealflux




opt_results$Model_data[which(opt_results$Name=="Obs_Contotal_ceta")] <-  total_cetaflux

opt_results$Model_data[which(opt_results$Name=="Obs_Proppfishceta")] <- pfish_cetaflux/total_cetaflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propdfishceta")] <- dfish_cetaflux/total_cetaflux

opt_results$Model_data[which(opt_results$Name=="Obs_Propmfishceta")] <- mfish_cetaflux/total_cetaflux


opt_results$Model_data[which(opt_results$Name=="Obs_Pland_livewt")] <- FishpLand

opt_results$Model_data[which(opt_results$Name=="Obs_Dland_livewt")] <- FishdLand

opt_results$Model_data[which(opt_results$Name=="Obs_Mland_livewt")] <- FishmLand

opt_results$Model_data[which(opt_results$Name=="Obs_Bsland_livewt")] <- FishbsLand

opt_results$Model_data[which(opt_results$Name=="Obs_Bcland_livewt")] <- FishbcLand

opt_results$Model_data[which(opt_results$Name=="Obs_Zcland_livewt")] <- FishzcLand

opt_results$Model_data[which(opt_results$Name=="Obs_Kland_livewt")] <- FishkpLand


opt_results$Model_data[which(opt_results$Name=="Obs_Ctland_livewt")] <-FishctLand

opt_results$Model_data[which(opt_results$Name=="Obs_kelp_pb")] <- kelpC_pb

opt_results$Model_data[which(opt_results$Name=="Obs_benslar_pb")]<-benslar_pb
opt_results$Model_data[which(opt_results$Name=="Obs_benclar_pb")]<-benclar_pb
opt_results$Model_data[which(opt_results$Name=="Obs_bens_pb")]<-bens_pb
opt_results$Model_data[which(opt_results$Name=="Obs_benc_pb")]<-benc_pb
opt_results$Model_data[which(opt_results$Name=="Obs_omni_pb")]<-omni_pb
opt_results$Model_data[which(opt_results$Name=="Obs_carn_pb")]<-carn_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishplar_pb")]<-pfishlar_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishdlar_pb")]<-dfishlar_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishp_pb")]<-pfish_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishd_pb")]<-dfish_pb
opt_results$Model_data[which(opt_results$Name=="Obs_fishm_pb")]<-mfish_pb
opt_results$Model_data[which(opt_results$Name=="Obs_bird_pb")]<-bird_pb
opt_results$Model_data[which(opt_results$Name=="Obs_seal_pb")]<-seal_pb
opt_results$Model_data[which(opt_results$Name=="Obs_ceta_pb")]<-ceta_pb

opt_results$Model_data[which(opt_results$Name=="Obs_exud_C_kelp")]<-prop_exud_C_kelp
opt_results$Model_data[which(opt_results$Name=="Obs_kelp_NC")]<-kelp_NC
opt_results$Model_data[which(opt_results$Name=="Obs_Denitrif")]<-Denitrif
opt_results$Model_data[which(opt_results$Name=="Obs_Dfdiscardp")]<-FishdDiscard/(FishdDiscard+FishdLand)





opt_results$Model_data[which(opt_results$Name=="Obs_s_x_ammonia")]<-aaconc_x_ammonia_s
opt_results$Model_data[which(opt_results$Name=="Obs_d_x_ammonia")]<-aaconc_x_ammonia_d
opt_results$Model_data[which(opt_results$Name=="Obs_s_x_nitrate")]<-aaconc_x_nitrate_s
opt_results$Model_data[which(opt_results$Name=="Obs_d_x_nitrate")]<-aaconc_x_nitrate_d
opt_results$Model_data[which(opt_results$Name=="Obs_s_x_TON")]<-aaconc_x_TON_s
opt_results$Model_data[which(opt_results$Name=="Obs_d_x_TON")]<-aaconc_x_TON_d
opt_results$Model_data[which(opt_results$Name=="Obs_NDJF_s_nitrate")]<-waconc_nitrate_s	# this one
opt_results$Model_data[which(opt_results$Name=="Obs_MJJA_s_nitrate")]<-saconc_nitrate_s
opt_results$Model_data[which(opt_results$Name=="Obs_NDJF_d_nitrate")]<-waconc_nitrate_d
opt_results$Model_data[which(opt_results$Name=="Obs_MJJA_d_nitrate")]<-saconc_nitrate_d
opt_results$Model_data[which(opt_results$Name=="Obs_NDJF_s_ammonia")]<-waconc_ammonia_s
opt_results$Model_data[which(opt_results$Name=="Obs_MJJA_s_ammonia")]<-saconc_ammonia_s
opt_results$Model_data[which(opt_results$Name=="Obs_NDJF_d_ammonia")]<-waconc_ammonia_d
opt_results$Model_data[which(opt_results$Name=="Obs_MJJA_d_ammonia")]<-saconc_ammonia_d





opt_results$Model_data[which(opt_results$Name=="Obs_carn_io_ratio")]<-aaconc_carn_i/aaconc_carn_o

opt_results$Model_data[which(opt_results$Name=="Obs_omni_io_ratio")]<-aaconc_omni_i/aaconc_omni_o

opt_results$Model_data[which(opt_results$Name=="Obs_phyt_io_ratio")]<-aaconc_phyt_si/aaconc_phyt_so

opt_results$Model_data[which(opt_results$Name=="Obs_nit_io_ratio")]<-aaconc_nitrate_si/aaconc_nitrate_so

opt_results$Model_data[which(opt_results$Name=="Obs_amm_io_ratio")]<-aaconc_ammonia_si/aaconc_ammonia_so

opt_results$Model_data[which(opt_results$Name=="Obs_pfish_io_ratio")]<-aaconc_fishp_i/aaconc_fishp_o

opt_results$Model_data[which(opt_results$Name=="Obs_dfish_io_ratio")]<-aaconc_fishd_i/aaconc_fishd_o




opt_results$Model_data[which(opt_results$Name=="Obs_birddisc")]<-BDDiscard
opt_results$Model_data[which(opt_results$Name=="Obs_sealdisc")]<-SLDiscard
opt_results$Model_data[which(opt_results$Name=="Obs_cetadisc")]<-CTDiscard

opt_results$Model_data[which(opt_results$Name=="Obs_kelp_beachcast")]<-kelp_beachcast_per_annProd

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#opt_results remains in memory and is available if this rioutine is followed by the 
#routine which calculates the likelihood
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Next, we need to prepare a simpler dataframe contining just the model results
#which is output as a csv file

target_results_output <- data.frame(opt_results[,3])
target_results_output[,2] <- opt_results[,6]
target_results_output[,3] <- opt_results[,7]

names(target_results_output)<-names(opt_results[,c(3,6,7)])


#Print the data to a csv file
#-----------------------------------------------------------------
filename = csvname(resultsdir, "model_target_annualresults", identifier)
writecsv(target_results_output, filename, row.names=FALSE)

#-------------------------------------------------------------------------------------------------------

#showall("opt_results", opt_results)

	opt_results
}

