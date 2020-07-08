/* file StrathE2E2_ecology_model_function.c*/

// This code builds on a basic two-vertical-layer food web model (Heath 2012). The key differences are:
// 1. horizontal zonation to include areas of shallow water where the seabed is in contact with the surface water layer,
// 2. allowing for a patches of different sediment types in shallow and deep water,
// 3. an additional fish group - migratory fish (typically mackerel) which leave the model for part ofthe year,
// 4. the original bulk top-predator guild is divided into birds, pinnipeds and cetaceans,
// 5. fish and top predators undertake active horizontal migration in respnse to gradients in food concentration.
// 
// Author: Michael Heath, University of Strathclyde, Scotland
// Date of this verision: Februaruy 2020
// -------------------------------------------------------------------------------
// -------------------------------------------------------------------------------

#include <stdio.h>

#include <R.h>

#include <Rmath.h>

#include <Rdefines.h>

#include <Rinternals.h>

//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------


//        FIRST DEFINE SOME FUNCTIONS THAT ARE GENERIC THROUGHOUT THE MODEL
//------------------------------------------------------------------------

/* _____Function to find minimum of two numbers_____*/

double twomin(double x, double y)
  {
     double min = x;		// make first value the minimum
     {
          if(y < min)
                min = y;	// if second value is smaller than min, overwrite min
     }
     return min;                // return smallest value
}

//------------------------------------------------------------------------

double twomax(double x, double y)
  {
     double max = x;		// make first value the maximum
     {
          if(y > max)
                max = y;	// if second value is larger than max, overwrite max
     }
     return max;                // return largest value
}

//------------------------------------------------------------------------

/* _____Heterotrophic uptake functions_____*/

double  f1(double a,double b,double k1,double k2)
//          prey      pred     umax       hs
{
 return ((b*k1*a)/(a+k2));
}

//------------------------------------------------------------------------

/* _____Phytoplankton uptake functions_____*/

double  f2(double a, double b, double c, double k1, double k2, double k3)
//         nut        phyt      light     umax       hs         Lsat
{
  double x = (c/k3);
  double minimum = twomin(1, x);
  return ((minimum * (b*k1*a))/(a+k2));
}


//------------------------------------------------------------------------


/* _____ Top predator uptake function (Beddington DeAngelis) ______*/


double  f3(double a,double b,double k1,double k2,double k3)
//          prey      pred    umax      hs1        bdap
{
 return ((b*k1*a)/(a+k2+(k3*b)));
}

//------------------------------------------------------------------------

/* _____Kelp carbon uptake function_____*/

double  f4(double b, double c, double k1, double k2, double k3)
//         kelpC      light       Umax      Lsat     selfshade
{
    return (twomin( (b*k1)  ,  (b*k1*(c*exp(-b*k3))/k2 )) ) ;
}


//------------------------------------------------------------------------


//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------

//    SET UP THE INTERFACE BETWEEN THE C-MODEL AND THE SURROUNDING R CODE
//    This involves defining all of the parameters in the vector parms, and all of
//    the timeseries of driving data in the object forc

//    Remember that vector elements in c start from index 0, but from index 1 in R
//------------------------------------------------------------------------


/* _____Define Parameters_____ */

static double parms[585]; //declare empty vector of length 585 - will be filled by ode function  <---- edit here if adding new parameters

// -------- Model morphology and physical configuration parameters --------- //
#define thik_so                 parms[0]
#define thik_d                  parms[1]
#define thik_si                 parms[2]
#define thik_b                  parms[3]
#define shallowprop             parms[4]

#define area_s0                 parms[5]
#define area_s1                 parms[6]
#define area_s2                 parms[7]
#define area_s3                 parms[8]
#define area_d0                 parms[9]
#define area_d1                 parms[10]
#define area_d2                 parms[11]
#define area_d3                 parms[12]

#define rock_s1                 parms[13]
#define rock_s2                 parms[14]
#define rock_s3                 parms[15]
#define rock_d1                 parms[16]
#define rock_d2                 parms[17]
#define rock_d3                 parms[18]
#define nonrock_s               parms[19]
#define nonrock_d               parms[20]
#define thik_x_s1               parms[21]
#define thik_x_s2               parms[22]
#define thik_x_s3               parms[23]
#define thik_x_d1               parms[24]
#define thik_x_d2               parms[25]
#define thik_x_d3               parms[26]
#define porosity_s1             parms[27]
#define porosity_s2             parms[28]
#define porosity_s3             parms[29]
#define porosity_d1             parms[30]
#define porosity_d2             parms[31]
#define porosity_d3             parms[32]
#define sed_wat_dif_s1          parms[33]
#define sed_wat_dif_s2          parms[34]
#define sed_wat_dif_s3          parms[35]
#define sed_wat_dif_d1          parms[36]
#define sed_wat_dif_d2          parms[37]
#define sed_wat_dif_d3          parms[38]
#define sed_ref_Kxw             parms[39]
#define bioturb_depth_s1        parms[40]
#define bioturb_depth_s2        parms[41]
#define bioturb_depth_s3        parms[42]
#define bioturb_depth_d1        parms[43]
#define bioturb_depth_d2        parms[44]
#define bioturb_depth_d3        parms[45]
#define erosion_depth_s1        parms[46]
#define erosion_depth_s2        parms[47]
#define erosion_depth_s3        parms[48]
#define erosion_depth_d1        parms[49]
#define erosion_depth_d2        parms[50]
#define erosion_depth_d3        parms[51]
#define lightSPM_intercept      parms[52]
#define lightSPM_slope          parms[53]
#define inshore_phyt_depth_prop parms[54]
#define inshore_kelp_depth_prop parms[55]

// ------- Parameters streamed in from the fishing fleet model -------- //

// Harvest ratios to be applied to each resource group
#define Fpidaily                parms[56]
#define Fpodaily                parms[57]
#define Fdidaily                parms[58]
#define Fdodaily                parms[59]
#define Fmidaily                parms[60]
#define Fmodaily                parms[61]
#define Fsbidaily               parms[62]
#define Fsbodaily               parms[63]
#define Fcbidaily               parms[64]
#define Fcbodaily               parms[65]
#define Fczidaily               parms[66]
#define Fczodaily               parms[67]
#define Fbdidaily               parms[68]
#define Fbdodaily               parms[69]

#define Fslidaily               parms[70]
#define Fslodaily               parms[71]

#define Fctidaily               parms[72]
#define Fctodaily               parms[73]

#define Fkpidaily               parms[74]
#define Fkpodaily               parms[75]

// Parameters for the density dependent functions defining the proportion of non
// quota species in the demersal fish catch (dependent on 1 January DF biomass)
#define DFpropNQ_scale          parms[76]
#define DFpropNQ_coeff          parms[77]

// Parameters for the density dependent functions defining the proportion of non
// quota species in the demersal fish catch which are undersize or of no value (dependent on 1 January DF biomass)
#define DFpropNQ_undersizescale parms[78]
#define DFpropNQ_undersizecoeff parms[79]

// Parameters for the density dependent functions defining the proportion of 
// quota limited species in the demersal fish catch which are undersize which mimics the LFI (dependent on 1 January DF biomass)
#define DFpropQ_undersizescale  parms[80]
#define DFpropQ_undersizecoeff  parms[81]

// overall discard rates of each resource group
#define Pidiscard               parms[82]
#define Podiscard               parms[83]
#define external_Didiscard      parms[84]
#define external_Dodiscard      parms[85]
#define external_Ddiscard       parms[86]
#define Midiscard               parms[87]
#define Modiscard               parms[88]
#define Bsidiscard              parms[89]
#define Bsodiscard              parms[90]
#define Bcidiscard              parms[91]
#define Bcodiscard              parms[92]
#define Zcidiscard              parms[93]
#define Zcodiscard              parms[94]

#define BDidiscard              parms[95]
#define BDodiscard              parms[96]

#define SLidiscard              parms[97]
#define SLodiscard              parms[98]

#define CTidiscard              parms[99]
#define CTodiscard              parms[100]

#define KPidiscard              parms[101]
#define KPodiscard              parms[102]


// overall processing-gutting-at-sea rates of each resource group
#define Pigutting               parms[103]
#define Pogutting               parms[104]
#define Digutting               parms[105]
#define Dogutting               parms[106]
#define Migutting               parms[107]
#define Mogutting               parms[108]
#define Bsigutting              parms[109]
#define Bsogutting              parms[110]
#define Bcigutting              parms[111]
#define Bcogutting              parms[112]
#define Zcigutting              parms[113]
#define Zcogutting              parms[114]

#define BDigutting              parms[115]
#define BDogutting              parms[116]

#define SLigutting              parms[117]
#define SLogutting              parms[118]

#define CTigutting              parms[119]
#define CTogutting              parms[120]

#define KPigutting              parms[121]
#define KPogutting              parms[122]

// switch defining how to handle the size selectivity for demersal fish
#define DFsize_switch           parms[123]

// switch defining how to handle the overall demersal fish discard rate
#define DFdiscard_switch        parms[124]

// proportional distribution of discards across the seabed types
#define pfish_D_p_s0            parms[125]
#define pfish_D_p_s1            parms[126]
#define pfish_D_p_s2            parms[127]
#define pfish_D_p_s3            parms[128]
#define pfish_D_p_d0            parms[129]
#define pfish_D_p_d1            parms[130]
#define pfish_D_p_d2            parms[131]
#define pfish_D_p_d3            parms[132]

#define dfish_D_p_s0            parms[133]
#define dfish_D_p_s1            parms[134]
#define dfish_D_p_s2            parms[135]
#define dfish_D_p_s3            parms[136]
#define dfish_D_p_d0            parms[137]
#define dfish_D_p_d1            parms[138]
#define dfish_D_p_d2            parms[139]
#define dfish_D_p_d3            parms[140]

#define mfish_D_p_s0            parms[141]
#define mfish_D_p_s1            parms[142]
#define mfish_D_p_s2            parms[143]
#define mfish_D_p_s3            parms[144]
#define mfish_D_p_d0            parms[145]
#define mfish_D_p_d1            parms[146]
#define mfish_D_p_d2            parms[147]
#define mfish_D_p_d3            parms[148]

#define sbfish_D_p_s0           parms[149]
#define sbfish_D_p_s1           parms[150]
#define sbfish_D_p_s2           parms[151]
#define sbfish_D_p_s3           parms[152]
#define sbfish_D_p_d0           parms[153]
#define sbfish_D_p_d1           parms[154]
#define sbfish_D_p_d2           parms[155]
#define sbfish_D_p_d3           parms[156]

#define cbfish_D_p_s0           parms[157]
#define cbfish_D_p_s1           parms[158]
#define cbfish_D_p_s2           parms[159]
#define cbfish_D_p_s3           parms[160]
#define cbfish_D_p_d0           parms[161]
#define cbfish_D_p_d1           parms[162]
#define cbfish_D_p_d2           parms[163]
#define cbfish_D_p_d3           parms[164]

#define czfish_D_p_s0           parms[165]
#define czfish_D_p_s1           parms[166]
#define czfish_D_p_s2           parms[167]
#define czfish_D_p_s3           parms[168]
#define czfish_D_p_d0           parms[169]
#define czfish_D_p_d1           parms[170]
#define czfish_D_p_d2           parms[171]
#define czfish_D_p_d3           parms[172]

#define bird_D_p_s0          parms[173]
#define bird_D_p_s1          parms[174]
#define bird_D_p_s2          parms[175]
#define bird_D_p_s3          parms[176]
#define bird_D_p_d0          parms[177]
#define bird_D_p_d1          parms[178]
#define bird_D_p_d2          parms[179]
#define bird_D_p_d3          parms[180]

#define seal_D_p_s0          parms[181]
#define seal_D_p_s1          parms[182]
#define seal_D_p_s2          parms[183]
#define seal_D_p_s3          parms[184]
#define seal_D_p_d0          parms[185]
#define seal_D_p_d1          parms[186]
#define seal_D_p_d2          parms[187]
#define seal_D_p_d3          parms[188]

#define ceta_D_p_s0          parms[189]
#define ceta_D_p_s1          parms[190]
#define ceta_D_p_s2          parms[191]
#define ceta_D_p_s3          parms[192]
#define ceta_D_p_d0          parms[193]
#define ceta_D_p_d1          parms[194]
#define ceta_D_p_d2          parms[195]
#define ceta_D_p_d3          parms[196]

#define kelp_D_p_s0          parms[197]
#define kelp_D_p_s1          parms[198]
#define kelp_D_p_s2          parms[199]
#define kelp_D_p_s3          parms[200]
#define kelp_D_p_d0          parms[201]
#define kelp_D_p_d1          parms[202]
#define kelp_D_p_d2          parms[203]
#define kelp_D_p_d3          parms[204]

// proportional distribution of offal across the seabed types
#define pfish_G_p_s0            parms[205]
#define pfish_G_p_s1            parms[206]
#define pfish_G_p_s2            parms[207]
#define pfish_G_p_s3            parms[208]
#define pfish_G_p_d0            parms[209]
#define pfish_G_p_d1            parms[210]
#define pfish_G_p_d2            parms[211]
#define pfish_G_p_d3            parms[212]

#define dfish_G_p_s0            parms[213]
#define dfish_G_p_s1            parms[214]
#define dfish_G_p_s2            parms[215]
#define dfish_G_p_s3            parms[216]
#define dfish_G_p_d0            parms[217]
#define dfish_G_p_d1            parms[218]
#define dfish_G_p_d2            parms[219]
#define dfish_G_p_d3            parms[220]

#define mfish_G_p_s0            parms[221]
#define mfish_G_p_s1            parms[222]
#define mfish_G_p_s2            parms[223]
#define mfish_G_p_s3            parms[224]
#define mfish_G_p_d0            parms[225]
#define mfish_G_p_d1            parms[226]
#define mfish_G_p_d2            parms[227]
#define mfish_G_p_d3            parms[228]

#define sbfish_G_p_s0           parms[229]
#define sbfish_G_p_s1           parms[230]
#define sbfish_G_p_s2           parms[231]
#define sbfish_G_p_s3           parms[232]
#define sbfish_G_p_d0           parms[233]
#define sbfish_G_p_d1           parms[234]
#define sbfish_G_p_d2           parms[235]
#define sbfish_G_p_d3           parms[236]

#define cbfish_G_p_s0           parms[237]
#define cbfish_G_p_s1           parms[238]
#define cbfish_G_p_s2           parms[239]
#define cbfish_G_p_s3           parms[240]
#define cbfish_G_p_d0           parms[241]
#define cbfish_G_p_d1           parms[242]
#define cbfish_G_p_d2           parms[243]
#define cbfish_G_p_d3           parms[244]

#define czfish_G_p_s0           parms[245]
#define czfish_G_p_s1           parms[246]
#define czfish_G_p_s2           parms[247]
#define czfish_G_p_s3           parms[248]
#define czfish_G_p_d0           parms[249]
#define czfish_G_p_d1           parms[250]
#define czfish_G_p_d2           parms[251]
#define czfish_G_p_d3           parms[252]

#define bird_G_p_s0          parms[253]
#define bird_G_p_s1          parms[254]
#define bird_G_p_s2          parms[255]
#define bird_G_p_s3          parms[256]
#define bird_G_p_d0          parms[257]
#define bird_G_p_d1          parms[258]
#define bird_G_p_d2          parms[259]
#define bird_G_p_d3          parms[260]

#define seal_G_p_s0          parms[261]
#define seal_G_p_s1          parms[262]
#define seal_G_p_s2          parms[263]
#define seal_G_p_s3          parms[264]
#define seal_G_p_d0          parms[265]
#define seal_G_p_d1          parms[266]
#define seal_G_p_d2          parms[267]
#define seal_G_p_d3          parms[268]

#define ceta_G_p_s0          parms[269]
#define ceta_G_p_s1          parms[270]
#define ceta_G_p_s2          parms[271]
#define ceta_G_p_s3          parms[272]
#define ceta_G_p_d0          parms[273]
#define ceta_G_p_d1          parms[274]
#define ceta_G_p_d2          parms[275]
#define ceta_G_p_d3          parms[276]

#define kelp_G_p_s0          parms[277]
#define kelp_G_p_s1          parms[278]
#define kelp_G_p_s2          parms[279]
#define kelp_G_p_s3          parms[280]
#define kelp_G_p_d0          parms[281]
#define kelp_G_p_d1          parms[282]
#define kelp_G_p_d2          parms[283]
#define kelp_G_p_d3          parms[284]


// proportion of each seabed type abraded per day
#define plough_daily_s0         parms[285]
#define plough_daily_s1         parms[286]
#define plough_daily_s2         parms[287]
#define plough_daily_s3         parms[288]
#define plough_daily_d0         parms[289]
#define plough_daily_d1         parms[290]
#define plough_daily_d2         parms[291]
#define plough_daily_d3         parms[292]

// Damage mortality rate (d-1) per unit ploughed area inflicted on each benthos group by all seabed ploughing gears
#define bensdamage_i            parms[293]
#define bensdamage_o            parms[294]
#define bencdamage_i            parms[295]
#define bencdamage_o            parms[296]

// Offal weight as a proportion fo live weight for catch which is processed at sea
#define offal_prop_live_weight  parms[297]

// Depth to which sediment is ploughed as a fraction of total active sediment layer thickness
#define plough_depth_s0         parms[298]
#define plough_depth_s1         parms[299]
#define plough_depth_s2         parms[300]
#define plough_depth_s3         parms[301]
#define plough_depth_d0         parms[302]
#define plough_depth_d1         parms[303]
#define plough_depth_d2         parms[304]
#define plough_depth_d3         parms[305]

// ---- Ecology model parameters -------- // 

// First some fixed parameters
#define qtena                   parms[306]
#define qtenh                   parms[307]
#define qtenm                   parms[308]
#define qtenr                   parms[309]

#define Lmaxup_phyt             parms[310]
#define Lmaxup_kelp             parms[311]

#define NCmax_kelp              parms[312]
#define NCmin_kelp              parms[313]
#define wave_beach_kelpdebris   parms[314]

// Now the fitted parameters
#define umaxC_kelpt             parms[315]
#define exudeC_kelpt            parms[316]
#define selfshade_kelp          parms[317]

#define uNIT_kelpt              parms[318]
#define hsNIT_kelp              parms[319]
#define uAMM_kelpt              parms[320]
#define hsAMM_kelp              parms[321]

#define uNIT_phytt              parms[322]
#define hsNIT_phyt              parms[323] 
#define uAMM_phytt              parms[324]
#define hsAMM_phyt              parms[325]
#define uphyt_herbt             parms[326]
#define hsphyt_herb             parms[327]
#define udet_herbt              parms[328]
#define hsdet_herb              parms[329]
#define ubenthslar_herbt        parms[330]
#define hsbenthslar_herb        parms[331]
#define ubenthclar_herbt        parms[332]
#define hsbenthclar_herb        parms[333]
#define uherb_carnt             parms[334]
#define hsherb_carn             parms[335]
#define ubenthslar_carnt        parms[336]
#define hsbenthslar_carn        parms[337]
#define ubenthclar_carnt        parms[338]
#define hsbenthclar_carn        parms[339]
#define ufishplar_carnt         parms[340]
#define hsfishplar_carn         parms[341]
#define ufishdlar_carnt         parms[342]
#define hsfishdlar_carn         parms[343]
#define uherb_fishplart         parms[344]
#define hsherb_fishplar         parms[345]
#define ubenthslar_fishplart    parms[346]
#define hsbenthslar_fishplar    parms[347]
#define ubenthclar_fishplart    parms[348]
#define hsbenthclar_fishplar    parms[349]
#define uherb_fishpt            parms[350]
#define hsherb_fishp            parms[351]
#define ucarn_fishpt            parms[352]
#define hscarn_fishp            parms[353]
#define ubenthslar_fishpt       parms[354]
#define hsbenthslar_fishp       parms[355]
#define ubenthclar_fishpt       parms[356]
#define hsbenthclar_fishp       parms[357]
#define ufishdlar_fishpt        parms[358]
#define hsfishdlar_fishp        parms[359]
#define ufishplar_fishpt        parms[360]
#define hsfishplar_fishp        parms[361]
#define uherb_fishmt            parms[362]
#define hsherb_fishm            parms[363]
#define ucarn_fishmt            parms[364]
#define hscarn_fishm            parms[365]
#define ubenthslar_fishmt       parms[366]
#define hsbenthslar_fishm       parms[367]
#define ubenthclar_fishmt       parms[368]
#define hsbenthclar_fishm       parms[369]
#define ufishdlar_fishmt        parms[370]
#define hsfishdlar_fishm        parms[371]
#define ufishplar_fishmt        parms[372]
#define hsfishplar_fishm        parms[373]
#define uherb_fishdlart         parms[374]
#define hsherb_fishdlar         parms[375]
#define ubenthslar_fishdlart    parms[376]
#define hsbenthslar_fishdlar    parms[377]
#define ubenthclar_fishdlart    parms[378]
#define hsbenthclar_fishdlar    parms[379]
#define ucarn_fishdt            parms[380]
#define hscarn_fishd            parms[381]
#define ubenths_fishdt          parms[382]
#define hsbenths_fishd          parms[383]
#define ubenthc_fishdt          parms[384]
#define hsbenthc_fishd          parms[385]
#define ufishplar_fishdt        parms[386]
#define hsfishplar_fishd        parms[387]
#define ufishdlar_fishdt        parms[388]
#define hsfishdlar_fishd        parms[389]
#define ufishp_fishdt           parms[390]
#define hsfishp_fishd           parms[391]
#define ufishm_fishdt           parms[392]
#define hsfishm_fishd           parms[393]
#define ufishd_fishdt           parms[394]
#define hsfishd_fishd           parms[395]
#define udisc_fishdt            parms[396]
#define hsdisc_fishd            parms[397]
#define ucorp_fishdt            parms[398]
#define hscorp_fishd            parms[399]
#define uphyt_benthslart        parms[400]
#define hsphyt_benthslar        parms[401]
#define udet_benthslart         parms[402]
#define hsdet_benthslar         parms[403]
#define uphyt_benthclart        parms[404]
#define hsphyt_benthclar        parms[405]
#define udet_benthclart         parms[406]
#define hsdet_benthclar         parms[407]
#define uphyt_benthst           parms[408]
#define hsphyt_benths           parms[409]
#define udet_benthst            parms[410]
#define hsdet_benths            parms[411]
#define used_benthst            parms[412]
#define hssed_benths            parms[413]

#define ubenths_benthct         parms[414]
#define hsbenths_benthc         parms[415]
#define ukelp_benthct           parms[416]
#define hskelp_benthc           parms[417]
#define ukelpdebris_benthct     parms[418]
#define hskelpdebris_benthc     parms[419]
#define ucorp_benthct           parms[420]
#define hscorp_benthc           parms[421]

//        #define uherb_bird    not used
//        #define hsherb_bird   not used
#define ucarn_bird              parms[422]
#define hscarn_bird             parms[423]
#define ubenths_bird            parms[424]
#define hsbenths_bird           parms[425]
#define ubenthc_bird            parms[426]
#define hsbenthc_bird           parms[427]
#define ufishp_bird             parms[428]
#define hsfishp_bird            parms[429]
#define ufishm_bird             parms[430]
#define hsfishm_bird            parms[431]
#define ufishd_bird             parms[432]
#define hsfishd_bird            parms[433]
#define udisc_bird              parms[434]
#define hsdisc_bird             parms[435]
#define ucorp_bird              parms[436]
#define hscorp_bird             parms[437]
#define bdapar_bird             parms[438]

#define ucarn_seal              parms[439]
#define hscarn_seal             parms[440]
#define ubenths_seal            parms[441]
#define hsbenths_seal           parms[442]
#define ubenthc_seal            parms[443]
#define hsbenthc_seal           parms[444]
#define ufishp_seal             parms[445]
#define hsfishp_seal            parms[446]
#define ufishm_seal             parms[447]
#define hsfishm_seal            parms[448]
#define ufishd_seal             parms[449]
#define hsfishd_seal            parms[450]
#define ubird_seal              parms[451]
#define hsbird_seal             parms[452]
#define udisc_seal              parms[453]
#define hsdisc_seal             parms[454]
#define ucorp_seal              parms[455]
#define hscorp_seal             parms[456]
#define bdapar_seal             parms[457]

#define uherb_ceta              parms[458]
#define hsherb_ceta             parms[459]
#define ucarn_ceta              parms[460]
#define hscarn_ceta             parms[461]
#define ubenths_ceta            parms[462]
#define hsbenths_ceta           parms[463]
#define ubenthc_ceta            parms[464]
#define hsbenthc_ceta           parms[465]
#define ufishp_ceta             parms[466]
#define hsfishp_ceta            parms[467]
#define ufishm_ceta             parms[468]
#define hsfishm_ceta            parms[469]
#define ufishd_ceta             parms[470]
#define hsfishd_ceta            parms[471]
#define ubird_ceta              parms[472]
#define hsbird_ceta             parms[473]
#define useal_ceta              parms[474]
#define hsseal_ceta             parms[475]
#define udisc_ceta              parms[476]
#define hsdisc_ceta             parms[477]
//          #define ucorp_ceta     not used
//          #define hscorp_ceta    not used
#define bdapar_ceta             parms[478]

#define aH                      parms[479]
#define aC                      parms[480]
#define aBslar                  parms[481]
#define aBclar                  parms[482]
#define aBs                     parms[483]
#define aBc                     parms[484]
#define aFplar                  parms[485]
#define aFdlar                  parms[486]
#define aFp                     parms[487]
#define aFm                     parms[488]
#define aFd                     parms[489]

#define abird                   parms[490]
#define aseal                   parms[491]
#define aceta                   parms[492]

#define eHt                     parms[493]
#define eCt                     parms[494]
#define eBslart                 parms[495]
#define eBclart                 parms[496]
#define eBst                    parms[497]
#define eBct                    parms[498]
#define eFplart                 parms[499]
#define eFdlart                 parms[500]
#define eFpt                    parms[501]
#define eFmt                    parms[502]
#define eFdt                    parms[503]

#define ebirdt                  parms[504]
#define esealt                  parms[505]
#define ecetat                  parms[506]

#define mt                      parms[507]
#define nst                     parms[508]
#define dst                     parms[509]
#define ndt                     parms[510]
#define ddt                     parms[511]
#define qs_p1                   parms[512]
#define qs_p2                   parms[513]
#define qs_p3                   parms[514]
#define msedt                   parms[515]
#define msens                   parms[516]
#define nsedt                   parms[517]
#define nsens                   parms[518]
#define dsedt                   parms[519]
#define dsens                   parms[520]

#define xwave_kelp              parms[521]

#define xst                     parms[522]
#define xdt                     parms[523]
#define xherb                   parms[524]
#define xcarn                   parms[525]
#define xbenthslar              parms[526]
#define xbenthclar              parms[527]
#define xbenths                 parms[528]
#define xbenthc                 parms[529]
#define xpfishlar               parms[530]
#define xdfishlar               parms[531]
#define xpfish                  parms[532]
#define xmfish                  parms[533]
#define xdfish                  parms[534]

#define xbird                   parms[535]
#define xseal                   parms[536]
#define xceta                   parms[537]

#define kelpdebris_det         parms[538]

#define corp_det               parms[539]
#define disc_corp               parms[540]
#define dsink_s                 parms[541]
#define dsink_d                 parms[542]

// Fitting parameter for demersal discard rate - expect this to be about 1.0
#define dfdp                    parms[543]

// Fish migration coefficients which are applied to food concentration gradients
#define pfish_migcoef           parms[544]
#define mfish_migcoef           parms[545]
#define dfish_migcoef           parms[546]

#define bird_migcoef            parms[547]
#define seal_migcoef            parms[548]
#define ceta_migcoef            parms[549]

// Proportions of biomass which is protected from fishing
#define protect_PF_o            parms[550]
#define protect_DF_o            parms[551]
#define protect_MF_o            parms[552]
#define protect_SB_o            parms[553]
#define protect_CB_o            parms[554]
#define protect_CZ_o            parms[555]
#define protect_BD_o            parms[556]
#define protect_SL_o            parms[557]
#define protect_CT_o            parms[558]

#define protect_PF_i            parms[559]
#define protect_DF_i            parms[560]
#define protect_MF_i            parms[561]
#define protect_SB_i            parms[562]
#define protect_CB_i            parms[563]
#define protect_CZ_i            parms[564]
#define protect_BD_i            parms[565]
#define protect_SL_i            parms[566]
#define protect_CT_i            parms[567]
#define protect_KP_i            parms[568]

#define max_exploitable_f_PF    parms[569]
#define max_exploitable_f_DF    parms[570]
#define max_exploitable_f_MF    parms[571]
#define max_exploitable_f_SB    parms[572]
#define max_exploitable_f_CB    parms[573]
#define max_exploitable_f_CZ    parms[574]

#define max_exploitable_f_BD    parms[575]
#define max_exploitable_f_SL    parms[576]
#define max_exploitable_f_CT    parms[577]
#define max_exploitable_f_KP    parms[578]

#define PF_fec                  parms[579]
#define DF_fec                  parms[580]
#define BS_fec                  parms[581]
#define BC_fec                  parms[582]

#define CZ_inedible_o             parms[583]
#define CZ_inedible_i             parms[584]

/* _____initializer for the parmameter vector_____ */
// Initialises parms with vector from R and passes them to solver via odec function

void odec(void(* odeparms)(int *, double *))
{
    int N= 585;         // length of parms vector = 585  <---- edit here if adding new parameters
    odeparms(&N, parms);
}

//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------

/* _____DRIVING VARIABLES_____ */
// defined as global vectors. These need to be passed to the solver function from R as additional arguments (...).
// Thus interpolation is completed before model is run, not concurrently.
// If these don't pass to model automatically as global variables then just add them to model input list.
// Drivers are indeed global, but function requires pointers to them.

//static int days=(nyears*360);

static double forc[53]; // passed from ode function

// Driver for surface incoming irradiance
#define driversslight       forc[0]

// Driver for surface layer suspended sediment concentration
#define driverlogespm_o     forc[1]
#define driverlogespm_i     forc[2]

// Drivers for surface and deep temperature
#define driversotemp        forc[3]
#define driverdtemp         forc[4]
#define driversitemp        forc[5]

// Driver for vertical mixing between surface and deep layers
#define driverv_dif         forc[6]

// Define drivers for volume inflows and outflows
#define driverso_inflow     forc[7]
#define driverd_inflow      forc[8]
#define driversi_inflow     forc[9]
#define driverso_outflow    forc[10]
#define driverd_outflow     forc[11]
#define driversi_outflow    forc[12]
#define driverso_si_flow    forc[13]
#define driversi_so_flow    forc[14]
#define drivers_upwell      forc[15]
#define driverriver         forc[16]

// Drivers for boundary (ocean river and atmosphere) concentrations and material fluxes
#define driverboundso_det   forc[17]
#define driverboundd_det    forc[18]
#define driverboundsi_det   forc[19]
#define driverboundso_amm   forc[20]
#define driverboundd_amm    forc[21]
#define driverboundsi_amm   forc[22]
#define driverboundso_nit   forc[23]
#define driverboundd_nit    forc[24]
#define driverboundsi_nit   forc[25]
#define driverboundso_phyt  forc[26]
#define driverboundd_phyt   forc[27]
#define driverboundsi_phyt  forc[28]
#define driverboundriv_amm  forc[29]
#define driverboundriv_nit  forc[30]
#define driverboundriv_det  forc[31]
#define driveratm_amm_o     forc[32]
#define driveratm_nit_o     forc[33]
#define driveratm_amm_i     forc[34]
#define driveratm_nit_i     forc[35]

// drivers for natural disturbance rates of sediment habitats due to currents and waves
#define driver_s1_erosion   forc[36]
#define driver_s2_erosion   forc[37]
#define driver_s3_erosion   forc[38]
#define driver_d1_erosion   forc[39]
#define driver_d2_erosion   forc[40]
#define driver_d3_erosion   forc[41]

// inshore wave height
#define driver_S_wave       forc[42]

// Drivers for fish spawning and recruitment dates
#define driverpfish_sp      forc[43]
#define driverpfish_rec     forc[44]
#define driverdfish_sp      forc[45]
#define driverdfish_rec     forc[46]

// Drivers for benthos spawning and recruitment dates
#define driverbs_sp         forc[47]
#define driverbs_rec        forc[48]
#define driverbc_sp         forc[49]
#define driverbc_rec        forc[50]

// Drivers for migratory fish imigration and emigration dates
#define drivermfish_im      forc[51]
#define drivermfish_em      forc[52]

/* _____forcing function initializer_____ */
// Initialises drivers with list from R and passes them to solver via forcc function

void forcc (void (* odeforcs)(int *, double *))
{
  int N=53;             // <----- edit this value if more driving data series are added
  odeforcs(&N, forc);
}

// END OF THE MODEL INTERFACE WITH R
//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------


/* _____DEFINE DYNAMIC VARIABLES INSIDE OF MODEL AS STATIC DOUBLES_____ */
// this gives them a dedicated "space" on the memory avoiding using the stack. 
// values of these variables will change iteratively within the model.


static double volume_so;
static double volume_si;
static double volume_d;


static double driverwcotemp;
static double driverzodtemp;
static double driverbslotemp;
static double driverbclotemp;

static double kvertattn_o;
static double kvertattn_i;
static double phyt_propsl_o;
static double phyt_propsl_i;
static double kelp_propsl_i;
static double phyt_S_layer_light_o;
static double phyt_S_layer_light_i;
static double kelp_S_layer_light_i;

static double inshore_plough_daily;
static double offshore_plough_daily;

static double xs_o;
static double xs_i;
static double xd;

static double xherb_o;
static double xherb_i;
static double xcarn_o;
static double xcarn_i;
static double xbenthslar_o;
static double xbenthslar_i;
static double xbenthclar_o;
static double xbenthclar_i;
static double xbenths_o;
static double xbenths_i;
static double xbenthc_o;
static double xbenthc_i;
static double xpfishlar_o;
static double xpfishlar_i;
static double xdfishlar_o;
static double xdfishlar_i;
static double xpfish_o;
static double xpfish_i;
static double xdfish_o;
static double xdfish_i;
static double xmfish_o;
static double xmfish_i;

static double xbird_o;
static double xbird_i;
static double xseal_o;
static double xseal_i;
static double xceta_o;
static double xceta_i;

static double wave_kelp_i;
static double selfshade_kelp_i;
static double exudeC_kelpt_i;
static double uC_kelp_i;
static double exC_kelp_i;
static double uNIT_kelp_i;
static double uAMM_kelp_i;

static double uNIT_phyt_o;
static double uAMM_phyt_o;
static double uNIT_phyt_i;
static double uAMM_phyt_i;
static double ubenthslar_o_herb;
static double ubenthclar_o_herb;
static double udet_so_herb;
static double udet_si_herb;
static double udet_d_herb;
static double uphyt_so_herb;
static double uphyt_si_herb;
static double uphyt_d_herb;
static double ubenthslar_i_herb;
static double ubenthclar_i_herb;
static double uherb_o_carn;
static double uherb_i_carn;
static double ubenthslar_i_carn;
static double ubenthclar_i_carn;
static double ubenthslar_o_carn;
static double ubenthclar_o_carn;
static double ufishplar_o_carn;
static double ufishplar_i_carn;
static double ufishdlar_o_carn;
static double ufishdlar_i_carn;
static double uherb_o_fishplar;
static double uherb_i_fishplar;
static double ubenthslar_i_fishplar;
static double ubenthclar_i_fishplar;
static double ubenthslar_o_fishplar;
static double ubenthclar_o_fishplar;
static double uherb_o_fishp;
static double uherb_i_fishp;
static double ucarn_o_fishp;
static double ucarn_i_fishp;
static double ubenthslar_i_fishp;
static double ubenthclar_i_fishp;
static double ubenthslar_o_fishp;
static double ubenthclar_o_fishp;
static double ufishdlar_o_fishp;
static double ufishdlar_i_fishp;
static double ufishplar_o_fishp;
static double ufishplar_i_fishp;
static double uherb_o_fishdlar;
static double uherb_i_fishdlar;
static double ubenthslar_i_fishdlar;
static double ubenthclar_i_fishdlar;
static double ubenthslar_o_fishdlar;
static double ubenthclar_o_fishdlar;
static double uherb_o_fishm;
static double uherb_i_fishm;
static double ucarn_o_fishm;
static double ucarn_i_fishm;
static double ubenthslar_i_fishm;
static double ubenthclar_i_fishm;
static double ubenthslar_o_fishm;
static double ubenthclar_o_fishm;
static double ufishdlar_o_fishm;
static double ufishdlar_i_fishm;
static double ufishplar_o_fishm;
static double ufishplar_i_fishm;
static double ucarn_o_fishd;
static double ucarn_i_fishd;
static double ubenths_i_fishd;
static double ubenthc_i_fishd;
static double ubenths_o_fishd;
static double ubenthc_o_fishd;
static double ufishplar_o_fishd;
static double ufishplar_i_fishd;
static double ufishdlar_o_fishd;
static double ufishdlar_i_fishd;
static double ufishp_o_fishd;
static double ufishp_i_fishd;
static double ufishm_o_fishd;
static double ufishm_i_fishd;
static double ufishd_o_fishd;
static double ufishd_i_fishd;
static double udisc_i_fishd;
static double ucorp_i_fishd;
static double udisc_o_fishd;
static double ucorp_o_fishd;
static double udet_so_benthslar;
static double udet_d_benthslar;
static double uphyt_so_benthslar;
static double uphyt_d_benthslar;
static double udet_si_benthslar;
static double uphyt_si_benthslar;
static double udet_so_benthclar;
static double udet_d_benthclar;
static double uphyt_so_benthclar;
static double uphyt_d_benthclar;
static double udet_si_benthclar;
static double uphyt_si_benthclar;
static double uphyt_si_benths_i;
static double udet_si_benths_i;
static double used_si_benths_i;
static double uphyt_d_benths_o;
static double udet_d_benths_o;
static double used_d_benths_o;
static double ubenths_i_benthc_i;
static double ukelp_i_benthc_i;
static double ukelpdebris_i_benthc_i;
static double ucorp_i_benthc_i;
static double ubenths_o_benthc_o;
static double ucorp_o_benthc_o;

static double ubenths_i_bird;
static double ubenthc_i_bird;
static double ubenths_o_bird;
static double ubenthc_o_bird;
static double udisc_i_bird;
static double ucorp_i_bird;
static double udisc_o_bird;
static double ucorp_o_bird;
// static double uherb_o_bird;   // NOT USED IN THIS CODE
// static double uherb_i_bird;   // NOT USED IN THIS CODE
static double ucarn_o_bird;
static double ucarn_i_bird;
static double ufishp_o_bird;
static double ufishp_i_bird;
static double ufishd_o_bird;
static double ufishd_i_bird;
static double ufishm_o_bird;
static double ufishm_i_bird;


static double ubenths_i_seal;
static double ubenthc_i_seal;
static double ubenths_o_seal;
static double ubenthc_o_seal;
static double udisc_i_seal;
static double ucorp_i_seal;
static double udisc_o_seal;
static double ucorp_o_seal;
static double ucarn_o_seal;
static double ucarn_i_seal;
static double ufishp_o_seal;
static double ufishp_i_seal;
static double ufishd_o_seal;
static double ufishd_i_seal;
static double ufishm_o_seal;
static double ufishm_i_seal;
static double ubird_o_seal;
static double ubird_i_seal;


static double ubenths_i_ceta;
static double ubenths_o_ceta;
static double ubenthc_i_ceta;
static double ubenthc_o_ceta;
static double udisc_i_ceta;
static double udisc_o_ceta;
//  static double ucorp_i_ceta;   // NOT USED IN THIS CODE
//  static double ucorp_o_ceta;   // NOT USED IN THIS CODE
static double uherb_o_ceta;
static double uherb_i_ceta;
static double ucarn_o_ceta;
static double ucarn_i_ceta;
static double ufishp_o_ceta;
static double ufishp_i_ceta;
static double ufishd_o_ceta;
static double ufishd_i_ceta;
static double ufishm_o_ceta;
static double ufishm_i_ceta;
static double ubird_o_ceta;
static double ubird_i_ceta;
static double useal_o_ceta;
static double useal_i_ceta;


static double eH_o;
static double eH_i;
static double eC_o;
static double eC_i;
static double eFplar_o;
static double eFplar_i;
static double eFdlar_o;
static double eFdlar_i;
static double eFp_o;
static double eFp_i;
static double eFm_o;
static double eFm_i;
static double eFd_o;
static double eFd_i;
static double eBslar_o;
static double eBclar_o;
static double eBslar_i;
static double eBclar_i;
static double eBs_o;
static double eBc_o;
static double eBs_i;
static double eBc_i;

static double ebird_o;
static double ebird_i;
static double eseal_o;
static double eseal_i;
static double eceta_o;
static double eceta_i;

static double m_so;
static double n_so;
static double d_so;
static double m_si;
static double n_si;
static double d_si;
static double m_d;
static double n_d;
static double d_d;
static double msed_s1;
static double msed_s2;
static double msed_s3;
static double msed_d1;
static double msed_d2;
static double msed_d3;
static double nsed_s1;
static double nsed_s2;
static double nsed_s3;
static double nsed_d1;
static double nsed_d2;
static double nsed_d3;
static double dsed_s1;
static double dsed_s2;
static double dsed_s3;
static double dsed_d1;
static double dsed_d2;
static double dsed_d3;
static double corp_det_i;
static double corp_det_o;
static double kelpdebris_det_i;
static double prop_herb_surfo;
static double prop_herb_deep;
static double prop_benthslar_surfo;
static double prop_benthclar_surfo;
static double prop_benthslar_deep;
static double prop_benthclar_deep;
static double prop_carn_surfo;
static double prop_carn_deep;
static double prop_fishplar_surfo;
static double prop_fishplar_deep;
static double prop_fishdlar_surfo;
static double prop_fishdlar_deep;

static double Upt_C_kelp_i;
static double Upt_samm_kelp_i;
static double Upt_snit_kelp_i;
static double Upt_samm_sphyt_o;
static double Upt_samm_sphyt_i;
static double Upt_snit_sphyt_o;
static double Upt_snit_sphyt_i;
static double Upt_detritus_so_herb;
static double Upt_detritus_si_herb;
static double Upt_detritus_d_herb;
static double Upt_phyt_so_herb;
static double Upt_phyt_si_herb;
static double Upt_phyt_d_herb;
static double Upt_benthslar_o_herb;
static double Upt_benthslar_i_herb;
static double Upt_benthclar_o_herb;
static double Upt_benthclar_i_herb;
static double Upt_total_herb_o;
static double Upt_total_herb_i;
static double Upt_detritus_so_benthslar;
static double Upt_detritus_d_benthslar;
static double Upt_phyt_so_benthslar;
static double Upt_phyt_d_benthslar;
static double Upt_total_benthslar_o;
static double Upt_detritus_so_benthclar;
static double Upt_detritus_d_benthclar;
static double Upt_phyt_so_benthclar;
static double Upt_phyt_d_benthclar;
static double Upt_total_benthclar_o;
static double Upt_detritus_si_benthslar;
static double Upt_phyt_si_benthslar;
static double Upt_total_benthslar_i;
static double Upt_detritus_si_benthclar;
static double Upt_phyt_si_benthclar;
static double Upt_total_benthclar_i;
static double Upt_herb_o_carn;
static double Upt_herb_i_carn;
static double Upt_fishplar_o_carn;
static double Upt_fishplar_i_carn;
static double Upt_fishdlar_o_carn;
static double Upt_fishdlar_i_carn;
static double Upt_benthslar_o_carn;
static double Upt_benthclar_o_carn;
static double Upt_benthslar_i_carn;
static double Upt_benthclar_i_carn;
static double Upt_total_carn_o;
static double Upt_total_carn_i;
static double Upt_herb_o_fishplar;
static double Upt_herb_i_fishplar;
static double Upt_benthslar_o_fishplar;
static double Upt_benthclar_o_fishplar;
static double Upt_benthslar_i_fishplar;
static double Upt_benthclar_i_fishplar;
static double Upt_total_fishplar_o;
static double Upt_total_fishplar_i;
static double Upt_herb_o_fishdlar;
static double Upt_herb_i_fishdlar;
static double Upt_benthslar_o_fishdlar;
static double Upt_benthclar_o_fishdlar;
static double Upt_benthslar_i_fishdlar;
static double Upt_benthclar_i_fishdlar;
static double Upt_total_fishdlar_o;
static double Upt_total_fishdlar_i;

static double Upt_detritus_si_benths_i;
static double Upt_phyt_si_benths_i;
static double Upt_xTdetritus_s1_benths_i;
static double Upt_xTdetritus_s2_benths_i;
static double Upt_xTdetritus_s3_benths_i;
static double Upt_xdetritus_s1_benths_i;
static double Upt_xdetritus_s2_benths_i;
static double Upt_xdetritus_s3_benths_i;
static double Upt_xRdetritus_s1_benths_i;
static double Upt_xRdetritus_s2_benths_i;
static double Upt_xRdetritus_s3_benths_i;
static double Upt_total_benths_i;
static double Upt_total_benths_s0;
static double Upt_total_benths_s1;
static double Upt_total_benths_s2;
static double Upt_total_benths_s3;
static double Upt_detritus_d_benths_o;
static double Upt_phyt_d_benths_o;
static double Upt_xTdetritus_d1_benths_o;
static double Upt_xTdetritus_d2_benths_o;
static double Upt_xTdetritus_d3_benths_o;
static double Upt_xdetritus_d1_benths_o;
static double Upt_xdetritus_d2_benths_o;
static double Upt_xdetritus_d3_benths_o;
static double Upt_xRdetritus_d1_benths_o;
static double Upt_xRdetritus_d2_benths_o;
static double Upt_xRdetritus_d3_benths_o;
static double Upt_total_benths_o;
static double Upt_total_benths_d0;
static double Upt_total_benths_d1;
static double Upt_total_benths_d2;
static double Upt_total_benths_d3;

static double Upt_benths_i_benthc_i;
static double Upt_kelp_s0_benthc_i;
static double Upt_kelpdebris_s0_benthc_i;
static double Upt_corpse_s0_benthc_i;
static double Upt_corpse_s1_benthc_i;
static double Upt_corpse_s2_benthc_i;
static double Upt_corpse_s3_benthc_i;
static double Upt_total_benthc_s0;
static double Upt_total_benthc_s1;
static double Upt_total_benthc_s2;
static double Upt_total_benthc_s3;
static double Upt_total_benthc_i;
static double Upt_benths_o_benthc_o;
static double Upt_corpse_d0_benthc_o;
static double Upt_corpse_d1_benthc_o;
static double Upt_corpse_d2_benthc_o;
static double Upt_corpse_d3_benthc_o;
static double Upt_total_benthc_d0;
static double Upt_total_benthc_d1;
static double Upt_total_benthc_d2;
static double Upt_total_benthc_d3;
static double Upt_total_benthc_o;

static double Upt_herb_o_fishp;
static double Upt_herb_i_fishp;
static double Upt_carn_o_fishp;
static double Upt_carn_i_fishp;
static double Upt_benthslar_o_fishp;
static double Upt_benthclar_o_fishp;
static double Upt_benthslar_i_fishp;
static double Upt_benthclar_i_fishp;
static double Upt_fishplar_o_fishp;
static double Upt_fishplar_i_fishp;
static double Upt_fishdlar_o_fishp;
static double Upt_fishdlar_i_fishp;
static double Upt_total_fishp_o ;
static double Upt_total_fishp_i ;
static double Upt_herb_o_fishm;
static double Upt_herb_i_fishm;
static double Upt_carn_o_fishm;
static double Upt_carn_i_fishm;
static double Upt_benthslar_o_fishm;
static double Upt_benthclar_o_fishm;
static double Upt_benthslar_i_fishm;
static double Upt_benthclar_i_fishm;
static double Upt_fishplar_o_fishm;
static double Upt_fishplar_i_fishm;
static double Upt_fishdlar_o_fishm;
static double Upt_fishdlar_i_fishm;
static double Upt_total_fishm_o ;
static double Upt_total_fishm_i ;
static double Upt_corpse_s0_fishd;
static double Upt_corpse_s1_fishd;
static double Upt_corpse_s2_fishd;
static double Upt_corpse_s3_fishd;
static double Upt_corpse_d0_fishd;
static double Upt_corpse_d1_fishd;
static double Upt_corpse_d2_fishd;
static double Upt_corpse_d3_fishd;
static double Upt_corpse_o_fishd;
static double Upt_corpse_i_fishd;
static double Upt_disc_o_fishd;
static double Upt_disc_i_fishd;
static double Upt_carn_o_fishd;
static double Upt_carn_i_fishd;
static double Upt_benths_o_fishd;
static double Upt_benths_i_fishd;
static double Upt_benthc_o_fishd;
static double Upt_benthc_i_fishd;
static double Upt_fishplar_o_fishd;
static double Upt_fishplar_i_fishd;
static double Upt_fishdlar_o_fishd;
static double Upt_fishdlar_i_fishd;
static double Upt_fishp_o_fishd;
static double Upt_fishp_i_fishd;
static double Upt_fishm_o_fishd;
static double Upt_fishm_i_fishd;
static double Upt_fishd_o_fishd;
static double Upt_fishd_i_fishd;
static double Upt_total_fishd_o;
static double Upt_total_fishd_i;

static double Upt_corpse_s0_bird;
static double Upt_corpse_s1_bird;
static double Upt_corpse_s2_bird;
static double Upt_corpse_s3_bird;
static double Upt_corpse_d0_bird;
static double Upt_corpse_d1_bird;
static double Upt_corpse_d2_bird;
static double Upt_corpse_d3_bird;
static double Upt_corpse_i_bird;
static double Upt_corpse_o_bird;
//  static double Upt_herb_o_bird;   // NOT USED IN THIS CODE
//  static double Upt_herb_i_bird;   // NOT USED IN THIS CODE
static double Upt_carn_o_bird;
static double Upt_carn_i_bird;
static double Upt_benths_o_bird;
static double Upt_benths_i_bird;
static double Upt_benthc_o_bird;
static double Upt_benthc_i_bird;
static double Upt_disc_o_bird;
static double Upt_disc_i_bird;
static double Upt_fishp_o_bird;
static double Upt_fishp_i_bird;
static double Upt_fishm_o_bird;
static double Upt_fishm_i_bird;
static double Upt_fishd_o_bird;
static double Upt_fishd_i_bird;
static double Upt_total_bird_o;
static double Upt_total_bird_i;


static double Upt_corpse_s0_seal;
static double Upt_corpse_s1_seal;
static double Upt_corpse_s2_seal;
static double Upt_corpse_s3_seal;
static double Upt_corpse_d0_seal;
static double Upt_corpse_d1_seal;
static double Upt_corpse_d2_seal;
static double Upt_corpse_d3_seal;
static double Upt_corpse_i_seal;
static double Upt_corpse_o_seal;
static double Upt_carn_o_seal;
static double Upt_carn_i_seal;
static double Upt_benths_o_seal;
static double Upt_benths_i_seal;
static double Upt_benthc_o_seal;
static double Upt_benthc_i_seal;
static double Upt_disc_o_seal;
static double Upt_disc_i_seal;
static double Upt_fishp_o_seal;
static double Upt_fishp_i_seal;
static double Upt_fishm_o_seal;
static double Upt_fishm_i_seal;
static double Upt_fishd_o_seal;
static double Upt_fishd_i_seal;

static double Upt_bird_o_seal;
static double Upt_bird_i_seal;

static double Upt_total_seal_o;
static double Upt_total_seal_i;


//  static double Upt_corpse_s1_ceta;   // NOT USED IN THIS CODE
//  static double Upt_corpse_s2_ceta;   // NOT USED IN THIS CODE
//  static double Upt_corpse_s3_ceta;   // NOT USED IN THIS CODE
//  static double Upt_corpse_d1_ceta;   // NOT USED IN THIS CODE
//  static double Upt_corpse_d2_ceta;   // NOT USED IN THIS CODE
//  static double Upt_corpse_d3_ceta;   // NOT USED IN THIS CODE
//  static double Upt_corpse_i_ceta;    // NOT USED IN THIS CODE
//  static double Upt_corpse_o_ceta;    // NOT USED IN THIS CODE
static double Upt_herb_o_ceta;
static double Upt_herb_i_ceta;
static double Upt_carn_o_ceta;
static double Upt_carn_i_ceta;
static double Upt_benths_o_ceta;
static double Upt_benths_i_ceta;
static double Upt_benthc_o_ceta;
static double Upt_benthc_i_ceta;
static double Upt_disc_o_ceta;
static double Upt_disc_i_ceta;
static double Upt_fishp_o_ceta;
static double Upt_fishp_i_ceta;
static double Upt_fishm_o_ceta;
static double Upt_fishm_i_ceta;
static double Upt_fishd_o_ceta;
static double Upt_fishd_i_ceta;

static double Upt_bird_o_ceta;
static double Upt_bird_i_ceta;
static double Upt_seal_o_ceta;
static double Upt_seal_i_ceta;

static double Upt_total_ceta_o;
static double Upt_total_ceta_i;

static double Exude_kelp_i;

static double Excr_herb_so;
static double Excr_herb_si;
static double Excr_herb_d;
static double Excr_carn_so;
static double Excr_carn_si;
static double Excr_carn_d;
static double Excr_benthslar_so;
static double Excr_benthslar_si;
static double Excr_benthslar_d;
static double Excr_benthclar_so;
static double Excr_benthclar_si;
static double Excr_benthclar_d;

static double Excr_benths_i;
static double Excr_benths_s0;
static double Excr_benths_s1;
static double Excr_benths_s2;
static double Excr_benths_s3;
static double Excr_benthc_i;
static double Excr_benthc_s0;
static double Excr_benthc_s1;
static double Excr_benthc_s2;
static double Excr_benthc_s3;
static double Excr_benths_o;
static double Excr_benths_d0;
static double Excr_benths_d1;
static double Excr_benths_d2;
static double Excr_benths_d3;
static double Excr_benthc_o;
static double Excr_benthc_d0;
static double Excr_benthc_d1;
static double Excr_benthc_d2;
static double Excr_benthc_d3;
static double Excr_fishplar_so;
static double Excr_fishplar_si;
static double Excr_fishplar_d;
static double Excr_fishdlar_so;
static double Excr_fishdlar_si;
static double Excr_fishdlar_d;
static double Excr_fishp_so;
static double Excr_fishp_si;
static double Excr_fishp_d;
static double Excr_fishm_so;
static double Excr_fishm_si;
static double Excr_fishm_d;
static double Excr_fishd_so;
static double Excr_fishd_si;
static double Excr_fishd_d;

static double Excr_bird_so;
static double Excr_bird_si;
static double Excr_bird_d;


static double Excr_seal_so;
static double Excr_seal_si;
static double Excr_seal_d;


static double Excr_ceta_so;
static double Excr_ceta_si;
static double Excr_ceta_d;

static double Defec_herb_so;
static double Defec_herb_si;
static double Defec_herb_d;
static double Defec_carn_so;
static double Defec_carn_si;
static double Defec_carn_d;
static double Defec_benthslar_so;
static double Defec_benthslar_si;
static double Defec_benthslar_d;
static double Defec_benthclar_so;
static double Defec_benthclar_si;
static double Defec_benthclar_d;
static double Defec_benths_s0;
static double Defec_benths_s1;
static double Defec_benths_s2;
static double Defec_benths_s3;
static double Defec_benths_d0;
static double Defec_benths_d1;
static double Defec_benths_d2;
static double Defec_benths_d3;
static double Defec_benthc_s0;
static double Defec_benthc_s1;
static double Defec_benthc_s2;
static double Defec_benthc_s3;
static double Defec_benthc_d0;
static double Defec_benthc_d1;
static double Defec_benthc_d2;
static double Defec_benthc_d3;
static double Defec_fishplar_so;
static double Defec_fishplar_si;
static double Defec_fishplar_d;
static double Defec_fishdlar_so;
static double Defec_fishdlar_si;
static double Defec_fishdlar_d;
static double Defec_fishp_so;
static double Defec_fishp_si;
static double Defec_fishp_d;
static double Defec_fishm_so;
static double Defec_fishm_si;
static double Defec_fishm_d;
static double Defec_fishd_so;
static double Defec_fishd_si;
static double Defec_fishd_d;

static double Defec_bird_so;
static double Defec_bird_si;
static double Defec_bird_d;

static double Defec_seal_so;
static double Defec_seal_si;
static double Defec_seal_d;

static double Defec_ceta_so;
static double Defec_ceta_si;
static double Defec_ceta_d;

static double Assim_herb_o;
static double Assim_herb_i;
static double Assim_carn_o;
static double Assim_carn_i;
static double Assim_benthslar_o;
static double Assim_benthslar_i;
static double Assim_benthclar_o;
static double Assim_benthclar_i;
static double Assim_benths_o;
static double Assim_benths_i;
static double Assim_benthc_o;
static double Assim_benthc_i;
static double Assim_fishplar_o;
static double Assim_fishplar_i;
static double Assim_fishp_o;
static double Assim_fishp_i;
static double Assim_fishm_o;
static double Assim_fishm_i;
static double Assim_fishdlar_o;
static double Assim_fishdlar_i;
static double Assim_fishd_o;
static double Assim_fishd_i;

static double Assim_bird_o;
static double Assim_bird_i;

static double Assim_seal_o;
static double Assim_seal_i;

static double Assim_ceta_o;
static double Assim_ceta_i;

static double Vmix_detritus;
static double Vmix_ammonia;
static double Vmix_nitrate;
static double Vmix_phyt;
static double detr_settle_s_b;
static double detr_settle_s_d;
static double detr_settle_d;
static double bioturb_daily_s1;
static double bioturb_daily_s2;
static double bioturb_daily_s3;
static double bioturb_daily_d1;
static double bioturb_daily_d2;
static double bioturb_daily_d3;
static double porewater_disturb_s1;
static double porewater_disturb_s2;
static double porewater_disturb_s3;
static double porewater_disturb_d1;
static double porewater_disturb_d2;
static double porewater_disturb_d3;
static double sediment_resuspend_s1;
static double sediment_resuspend_s2;
static double sediment_resuspend_s3;
static double sediment_resuspend_d1;
static double sediment_resuspend_d2;
static double sediment_resuspend_d3;
static double s_w_amm_flx_s1;
static double s_w_amm_flx_s2;
static double s_w_amm_flx_s3;
static double s_w_nit_flx_s1;
static double s_w_nit_flx_s2;
static double s_w_nit_flx_s3;
static double s_w_amm_flx_d1;
static double s_w_amm_flx_d2;
static double s_w_amm_flx_d3;
static double s_w_nit_flx_d1;
static double s_w_nit_flx_d2;
static double s_w_nit_flx_d3;
static double s_w_amm_disturb_flx_s1;
static double s_w_amm_disturb_flx_s2;
static double s_w_amm_disturb_flx_s3;
static double s_w_nit_disturb_flx_s1;
static double s_w_nit_disturb_flx_s2;
static double s_w_nit_disturb_flx_s3;
static double s_w_amm_disturb_flx_d1;
static double s_w_amm_disturb_flx_d2;
static double s_w_amm_disturb_flx_d3;
static double s_w_nit_disturb_flx_d1;
static double s_w_nit_disturb_flx_d2;
static double s_w_nit_disturb_flx_d3;
static double s_w_det_resuspend_flx_s1;
static double s_w_det_resuspend_flx_s2;
static double s_w_det_resuspend_flx_s3;
static double s_w_det_resuspend_flx_d1;
static double s_w_det_resuspend_flx_d2;
static double s_w_det_resuspend_flx_d3;
static double p_disc_s0;
static double p_disc_s1;
static double p_disc_s2;
static double p_disc_s3;
static double p_disc_d0;
static double p_disc_d1;
static double p_disc_d2;
static double p_disc_d3;
static double OceanIN_sodetritus;
static double OceanIN_soammonia;
static double OceanIN_sonitrate;
static double OceanIN_sophyt;
static double OceanIN_ddetritus;
static double OceanIN_dammonia;
static double OceanIN_dnitrate;
static double OceanIN_dphyt;
static double OceanIN_sidetritus;
static double OceanIN_siammonia;
static double OceanIN_sinitrate;
static double OceanIN_siphyt;
static double OceanOUT_sodetritus;
static double OceanOUT_soammonia;
static double OceanOUT_sonitrate;
static double OceanOUT_sophyt;
static double OceanOUT_ddetritus;
static double OceanOUT_dammonia;
static double OceanOUT_dnitrate;
static double OceanOUT_dphyt;
static double OceanOUT_sidetritus;
static double OceanOUT_siammonia;
static double OceanOUT_sinitrate;
static double OceanOUT_siphyt;
static double InshoreIN_sdetritus;
static double InshoreIN_sammonia;
static double InshoreIN_snitrate;
static double InshoreIN_sphyt;
static double InshoreIN_benthslar;
static double InshoreIN_benthclar;
static double InshoreIN_herb;
static double InshoreIN_carn;
static double InshoreIN_fishplar;
static double InshoreIN_fishdlar;
static double InshoreIN_fishp;
static double InshoreIN_fishd;
static double InshoreIN_fishm;

static double InshoreIN_bird;
static double InshoreIN_seal;
static double InshoreIN_ceta;

static double InshoreOUT_sdetritus;
static double InshoreOUT_sammonia;
static double InshoreOUT_snitrate;
static double InshoreOUT_sphyt;
static double InshoreOUT_benthslar;
static double InshoreOUT_benthclar;
static double InshoreOUT_herb;
static double InshoreOUT_carn;
static double InshoreOUT_fishplar;
static double InshoreOUT_fishdlar;
static double InshoreOUT_fishp;
static double InshoreOUT_fishd;
static double InshoreOUT_fishm;

static double InshoreOUT_bird;
static double InshoreOUT_seal;
static double InshoreOUT_ceta;

static double Upwelling_det;
static double Upwelling_amm;
static double Upwelling_nit;
static double Upwelling_phyt;

static double Riv_amm_IN;
static double Riv_nit_IN;
static double Riv_det_IN;
static double Atm_amm_IN_o;
static double Atm_nit_IN_o;
static double Atm_amm_IN_i;
static double Atm_nit_IN_i;
static double Flx_pfish_disc_s0;
static double Flx_pfish_disc_s1;
static double Flx_pfish_disc_s2;
static double Flx_pfish_disc_s3;
static double Flx_pfish_disc_d0;
static double Flx_pfish_disc_d1;
static double Flx_pfish_disc_d2;
static double Flx_pfish_disc_d3;
static double Flx_dfish_disc_s0;
static double Flx_dfish_disc_s1;
static double Flx_dfish_disc_s2;
static double Flx_dfish_disc_s3;
static double Flx_dfish_disc_d0;
static double Flx_dfish_disc_d1;
static double Flx_dfish_disc_d2;
static double Flx_dfish_disc_d3;
static double Flx_sbfish_disc_s0;
static double Flx_sbfish_disc_s1;
static double Flx_sbfish_disc_s2;
static double Flx_sbfish_disc_s3;
static double Flx_sbfish_disc_d0;
static double Flx_sbfish_disc_d1;
static double Flx_sbfish_disc_d2;
static double Flx_sbfish_disc_d3;
static double Flx_cbfish_disc_s0;
static double Flx_cbfish_disc_s1;
static double Flx_cbfish_disc_s2;
static double Flx_cbfish_disc_s3;
static double Flx_cbfish_disc_d0;
static double Flx_cbfish_disc_d1;
static double Flx_cbfish_disc_d2;
static double Flx_cbfish_disc_d3;
static double Flx_czfish_disc_s0;
static double Flx_czfish_disc_s1;
static double Flx_czfish_disc_s2;
static double Flx_czfish_disc_s3;
static double Flx_czfish_disc_d0;
static double Flx_czfish_disc_d1;
static double Flx_czfish_disc_d2;
static double Flx_czfish_disc_d3;
static double Flx_mfish_disc_s0;
static double Flx_mfish_disc_s1;
static double Flx_mfish_disc_s2;
static double Flx_mfish_disc_s3;
static double Flx_mfish_disc_d0;
static double Flx_mfish_disc_d1;
static double Flx_mfish_disc_d2;
static double Flx_mfish_disc_d3;
static double Flx_tot_disc_i;
static double Flx_tot_disc_o;

static double Flx_pfish_offal_s0;
static double Flx_pfish_offal_s1;
static double Flx_pfish_offal_s2;
static double Flx_pfish_offal_s3;
static double Flx_pfish_offal_d0;
static double Flx_pfish_offal_d1;
static double Flx_pfish_offal_d2;
static double Flx_pfish_offal_d3;
static double Flx_dfish_offal_s0;
static double Flx_dfish_offal_s1;
static double Flx_dfish_offal_s2;
static double Flx_dfish_offal_s3;
static double Flx_dfish_offal_d0;
static double Flx_dfish_offal_d1;
static double Flx_dfish_offal_d2;
static double Flx_dfish_offal_d3;
static double Flx_sbfish_offal_s0;
static double Flx_sbfish_offal_s1;
static double Flx_sbfish_offal_s2;
static double Flx_sbfish_offal_s3;
static double Flx_sbfish_offal_d0;
static double Flx_sbfish_offal_d1;
static double Flx_sbfish_offal_d2;
static double Flx_sbfish_offal_d3;
static double Flx_cbfish_offal_s0;
static double Flx_cbfish_offal_s1;
static double Flx_cbfish_offal_s2;
static double Flx_cbfish_offal_s3;
static double Flx_cbfish_offal_d0;
static double Flx_cbfish_offal_d1;
static double Flx_cbfish_offal_d2;
static double Flx_cbfish_offal_d3;
static double Flx_czfish_offal_s0;
static double Flx_czfish_offal_s1;
static double Flx_czfish_offal_s2;
static double Flx_czfish_offal_s3;
static double Flx_czfish_offal_d0;
static double Flx_czfish_offal_d1;
static double Flx_czfish_offal_d2;
static double Flx_czfish_offal_d3;
static double Flx_mfish_offal_s0;
static double Flx_mfish_offal_s1;
static double Flx_mfish_offal_s2;
static double Flx_mfish_offal_s3;
static double Flx_mfish_offal_d0;
static double Flx_mfish_offal_d1;
static double Flx_mfish_offal_d2;
static double Flx_mfish_offal_d3;

static double Flx_bird_offal_s0;
static double Flx_bird_offal_s1;
static double Flx_bird_offal_s2;
static double Flx_bird_offal_s3;
static double Flx_bird_offal_d0;
static double Flx_bird_offal_d1;
static double Flx_bird_offal_d2;
static double Flx_bird_offal_d3;

static double Flx_seal_offal_s0;
static double Flx_seal_offal_s1;
static double Flx_seal_offal_s2;
static double Flx_seal_offal_s3;
static double Flx_seal_offal_d0;
static double Flx_seal_offal_d1;
static double Flx_seal_offal_d2;
static double Flx_seal_offal_d3;

static double Flx_ceta_offal_s0;
static double Flx_ceta_offal_s1;
static double Flx_ceta_offal_s2;
static double Flx_ceta_offal_s3;
static double Flx_ceta_offal_d0;
static double Flx_ceta_offal_d1;
static double Flx_ceta_offal_d2;
static double Flx_ceta_offal_d3;

static double Flx_tot_offal_i;
static double Flx_tot_offal_o;

static double jan_fishd_o;
static double jan_fishd_i;
static double jan_fishd;
static double Ddiscard;
static double Didiscard;
static double Dodiscard;
static double DFp_NQ;
static double DFp_undersizeNQ;
static double DFp_undersizeQ;
static double DFp_discardNQ;
static double DFip_discardNQ;
static double DFop_discardNQ;
static double DFp_discardQ;
static double DFip_discardQ;
static double DFop_discardQ;
static double D_i_weighting;
static double D_o_weighting;
static double DFp_undersizeALL;
static double Fdidaily_USC;
static double Fdodaily_USC;
static double food_gradient_pfish;
static double food_gradient_mfish;
static double food_gradient_dfish;

static double food_gradient_bird;
static double food_gradient_seal;
static double food_gradient_ceta;

static double HTLmetabolism_so;
static double HTLmetabolism_d;
static double HTLmetabolism_so_d;
static double HTLmetabolism_si;
static double s1_stick_reflect;
static double s2_stick_reflect;
static double s3_stick_reflect;
static double d1_stick_reflect;
static double d2_stick_reflect;
static double d3_stick_reflect;

static double kelp_i_slope;
static double kelp_Ucsc;
static double kelp_Unsc;

static double CZ_edible_o;
static double CZ_edible_i;

//___________________________________________________________________________________________________
//___________________________________________________________________________________________________
//___________________________________________________________________________________________________
//___________________________________________________________________________________________________
//___________________________________________________________________________________________________
//___________________________________________________________________________________________________
//___________________________________________________________________________________________________
//___________________________________________________________________________________________________


/// ################################################## ///
///            THE ECOLOGICAL MODEL                    ///
/// ################################################## ///


//model passed to solver via derivsc function.

void derivsc (int *neq,
	     double *t, 
	     double *y, 
	     double *ydot, 
	     double *yout, 
	     int *ip)
{


// ------------------------------------------------------------------------------------------
// Calculations that only need to be done once at the start of the run and never again can go in here
// Also, each 1st January grab values of any model state variables that are needed for setting
// any dynamic parameters, such as demersal fish quota/non-quota fractions and undersize fractions


//  DETECT 1st JANUARY EACH YEAR.....

     static int last_year;
     int this_year;

        // start of run at time = 0.0, perform one-off initialisation
	if(*t < DBL_EPSILON) {						 // Test whether *t is zero
		last_year = -1;	

             // one off compute the volume of each water column layer at the start of the first year only

             volume_so = thik_so*(1-shallowprop);
             volume_si = thik_si*shallowprop;
             volume_d  = thik_d*(1-shallowprop);

             // one-off SET A FLAG TO DICTATE WHETHER DETRITUS STICKS TO THE SEABED OR IS REFLECTED BACK INTO THE WATER COLUMN
             // IF rock_hab = 0 THEN THAT MEANS ITS ROCK AND STUFF IS REFLECTED
             // IF rock_hab = 1 THEN THAT MEANS ITS SEDIMENT AND STUFF STICKS
             // stick_reflect = 1 means that stuff reflects
             // stick_reflect = 0 means that stuff sticks
	     //           s0_stick_reflect  = 1;   // sediment is reflected from shallow rock and kelp habitat - this term not needed as 'reflection' is hard coded for rock
	     //           d0_stick_reflect  = 1;   // sediment is reflected from prescribed deep rock habitat - this term not needed as 'reflection' is hard coded for rock

             if (rock_s1 < 0.5) {
               s1_stick_reflect = 1;
             }
             else {
               s1_stick_reflect = 0;
             }

             if (rock_s2 < 0.5) {
               s2_stick_reflect = 1;
             }
             else {
               s2_stick_reflect = 0;
             }

             if (rock_s3 < 0.5) {
               s3_stick_reflect = 1;
             }
             else {
               s3_stick_reflect = 0;
             }

             if (rock_d1 < 0.5) {
               d1_stick_reflect = 1;
             }
             else {
               d1_stick_reflect = 0;
             }

             if (rock_d2 < 0.5) {
               d2_stick_reflect = 1;
             }
             else {
               d2_stick_reflect = 0;
             }

             if (rock_d3 < 0.5) {
               d3_stick_reflect = 1;
             }
             else {
               d3_stick_reflect = 0;
             }

             // one off scale the density dependent process parameters to the volume of area of the layer or zone to which they will be applied

             /* ______ Scale the denisity dependent mortality coefficients to the volume or area of each layer ___ */

             // Kelp 
             wave_kelp_i      = xwave_kelp*(1/area_s0);
             exudeC_kelpt_i    = exudeC_kelpt*(1/area_s0);
             selfshade_kelp_i = selfshade_kelp*(1/area_s0);

             // Phytoplankton
             xs_o= xst  * (volume_so+volume_si)/volume_so;
             //   xs_i= xst  * (volume_so+volume_si)/volume_si;
             xs_i= xdt  * (volume_so+volume_si)/volume_si;
             xd = xdt;

             // Omnivzoo
             xherb_o = xherb * (volume_so+volume_si+volume_d)/(volume_so + volume_d);
             xherb_i = xherb * (volume_so+volume_si+volume_d)/(volume_si);

             // Carnzoo
             xcarn_o = xcarn * (volume_so+volume_si+volume_d)/(volume_so + volume_d);
             xcarn_i = xcarn * (volume_so+volume_si+volume_d)/(volume_si);

             // F/d feeding benthos larvae
             xbenthslar_o = xbenthslar  * (volume_so+volume_si+volume_d)/(volume_so + volume_d);
             xbenthslar_i = xbenthslar * (volume_so+volume_si+volume_d)/(volume_si);

             // C/s feeding benthos larvae  
             xbenthclar_o = xbenthclar  * (volume_so+volume_si+volume_d)/(volume_so + volume_d);
             xbenthclar_i = xbenthclar * (volume_so+volume_si+volume_d)/(volume_si);

             // F/d benthos
             xbenths_o = xbenths * 1/(1-shallowprop);
             xbenths_i = xbenths * 1/shallowprop;

             // C/s benthos
             xbenthc_o = xbenthc * 1/(1-shallowprop);
             xbenthc_i = xbenthc * 1/shallowprop;

             // Pelagic fish larvae
             xpfishlar_o = xpfishlar * (volume_so+volume_si+volume_d)/(volume_so + volume_d);
             xpfishlar_i = xpfishlar * (volume_so+volume_si+volume_d)/(volume_si);

             // Demersal fish larvae
             xdfishlar_o = xdfishlar * (volume_so+volume_si+volume_d)/(volume_so + volume_d);
             xdfishlar_i = xdfishlar * (volume_so+volume_si+volume_d)/(volume_si);

             // Pelagic fish 
             xpfish_o = xpfish * (volume_so+volume_si+volume_d)/(volume_so + volume_d);
             xpfish_i = xpfish * (volume_so+volume_si+volume_d)/(volume_si);

             // Demersal fish 
             xdfish_o = xdfish * 1/(1-shallowprop);
             xdfish_i = xdfish * 1/shallowprop;

             // Migratory fish
             xmfish_o = xmfish * (volume_so+volume_si+volume_d)/(volume_so + volume_d);
             xmfish_i = xmfish * (volume_so+volume_si+volume_d)/(volume_si);

             // Birds
             xbird_o = xbird * 1/(1-shallowprop);
             xbird_i = xbird * 1/shallowprop;

             // Seals
             xseal_o = xseal * 1/(1-shallowprop);
             xseal_i = xseal * 1/shallowprop;

             // Cetaceans
             xceta_o = xceta * 1/(1-shallowprop);
             xceta_i = xceta * 1/shallowprop;


             // ----------------------------------------------------
             // Sensitivity of N and C uptake inhibition terms for kelp
             kelp_i_slope = (1/(NCmax_kelp-NCmin_kelp));

        }

// end of first year only calculations


	this_year = ((int)(*t / 360));

        // .......................................................................


        // now, for the first AND all subsequent 1st Januaries...


	if (this_year > last_year) {


        // grab the start-of-each-year value of demersal fish biomass...
        jan_fishd_o = y[47];
        jan_fishd_i = y[66];
        jan_fishd = (jan_fishd_o + jan_fishd_i)/dfdp;
        // Here the modelled whole region demersal fish biomass is converted to the equivalent value of the
        // survey abundance on which the empirical relationships for pNQ and undersize fractions are based
        // dfdp is the proportionality relationship between model nitrogen mass and the survey index of regional biomass

        // Now use this start-of-year value of DF to set the non-quota fraction in the catch and the
        // undersize fractions
        DFp_NQ          = DFpropNQ_scale * exp(-DFpropNQ_coeff*jan_fishd);
        DFp_undersizeNQ = DFpropNQ_undersizescale * exp(-DFpropNQ_undersizecoeff*jan_fishd);
        DFp_undersizeQ  = DFpropQ_undersizescale * exp(-DFpropQ_undersizecoeff*jan_fishd);

        if(external_Ddiscard>0){
        D_i_weighting = external_Didiscard/external_Ddiscard;
        D_o_weighting = external_Dodiscard/external_Ddiscard;
        }
        else {
        D_i_weighting = 1;
        D_o_weighting = 1;
        }


        // ---------------------------------------------------------------------------------------
        // ---------------------------------------------------------------------------------------

        // CHECK THE VALUE OF DFsize_switch TO SEE WHETHER THE DF HARVEST RATIO NEEDS TO BE ATTENUATED TO MIMIC
        // IMPROVED SELECTIVITY SO AS TO NOT CATCH ANY UNDERSIZE QUOTA OR NON-QUOTA DEMERSAL FISH
        // DFsize_switch = 0 means proceed as with the externally provided harvest ratios for demersal fish
        // DFsize_switch = 1 means reduce the externally provided DF harvest ratio by an amount equivalent to the undersize fractin in the catch
        //                 and then reset the undersize fraction=0 so that discards will also be zero


        // Assume that improved selectivity enables the exclusion of ALL undersize quota and ALL non-quota stuff from the catches
        //   if (DFsize_switch>0.5) {
        //   DFp_undersizeALL = (DFp_undersizeQ * (1-DFp_NQ)) + (DFp_undersizeNQ * (DFp_NQ)) ;
        // then the DF harvest ratio gets reduced to (1-DFp_undersizeALL)*external value
        //   Fdidaily_USC = Fdidaily * (1-DFp_undersizeALL) ;
        //   Fdodaily_USC = Fdodaily * (1-DFp_undersizeALL) ;
        //   DFp_undersizeNQ = 0 ;
        //   DFp_undersizeQ = 0 ;
        //   }


        // Alternative might be to...
        // Assume that only the lesser of DFp_undersizeQ and DFp_undersizeNQ represents the selectivity saving to be applied to the entire DF community
        // Then there remains a residual undersize fraction (DFp_undersizeNQ-DFp_undersizeQ) or -1 * this, which is still applicable to either the Q or NQ part
        // IF DFsize_switch = 1
        //.....................................................
	if (DFsize_switch>0.5 && DFsize_switch<1.5) {
   
        DFp_undersizeALL = twomin(DFp_undersizeQ, DFp_undersizeNQ) ;
        // then the DF harvest ratio gets reduced to (1-DFp_undersizeALL)*external value
        Fdidaily_USC = Fdidaily * (1-DFp_undersizeALL) ;
        Fdodaily_USC = Fdodaily * (1-DFp_undersizeALL) ;

        if (DFp_undersizeQ < DFp_undersizeNQ) {
        DFp_undersizeNQ = (DFp_undersizeNQ - DFp_undersizeQ) ;
        DFp_undersizeQ = 0 ;
        }

        if (DFp_undersizeQ > DFp_undersizeNQ) {
        DFp_undersizeNQ = 0 ;
        DFp_undersizeQ = (DFp_undersizeQ - DFp_undersizeNQ) ;
        }

        if (DFp_undersizeQ == DFp_undersizeNQ) {
        DFp_undersizeNQ = 0 ;
        DFp_undersizeQ =  0 ;
        }

        }
        //.....................................................


        // use default with usual catches of undersize fish
        // IF DFsize_switch = 0
        //.....................................................
        if (DFsize_switch<0.5) {
        Fdidaily_USC = Fdidaily ;
        Fdodaily_USC = Fdodaily ;
        }
        //.....................................................

        // ---------------------------------------------------------------------------------------
        // ---------------------------------------------------------------------------------------


        // Now use these quota/non-quota and undersize fractions to decide the discarding rates

        // If the switch DFdiscard_switch=0 then.....
        // Assume that only the internally generated undersize quota and non-quota fractions are discarded,
        // overriding the overall demersal fish discard rate provided from the fleet model
        if (DFdiscard_switch<0.5) {
        Ddiscard = twomin(1,(((DFp_undersizeNQ*DFp_NQ) + (DFp_undersizeQ*(1-DFp_NQ)))));
        Didiscard = twomin(1,(Ddiscard*D_i_weighting));
        Dodiscard = twomin(1,(Ddiscard*D_o_weighting));

        DFp_discardNQ = twomin(1,(DFp_undersizeNQ));
        DFip_discardNQ = twomin(1,(DFp_discardNQ * D_i_weighting));
        DFop_discardNQ = twomin(1,(DFp_discardNQ * D_o_weighting));
        DFp_discardQ  = twomin(1,(DFp_undersizeQ));
        DFip_discardQ  = twomin(1,(DFp_discardQ * D_i_weighting));
        DFop_discardQ  = twomin(1,(DFp_discardQ * D_o_weighting));
        }


        // OR............................... 


        // If the switch DFdiscard_switch=1 then.....
        // RECOMPUTE DFp_discardQ SO THAT THE OVERALL DISCARD RATE MATCHES THAT IMPOSED BY THE FLEET MODEL,
        // IMPLICITLY ASSUMING THAT THE DIFFERENCE IS ACCOUNTED FOR ENTIRELY BY THE DISCARDING RATE OF QUOTA LIMITED SPECIES
        // IF THIS IMPLIES NEGATIVE VALUES OF DFp_discardQ THEN ADJUST THE NON-QUOTA DISCARD RATE AS WELL
        // DFdiscard_switch = 1
        if (DFdiscard_switch>0.5 && DFdiscard_switch<1.5) {
        Ddiscard = external_Ddiscard;
        Didiscard = external_Didiscard;
        Dodiscard = external_Dodiscard;

        DFp_discardQ = ( (external_Ddiscard) - (DFp_undersizeNQ*DFp_NQ) ) / (1-DFp_NQ);
        DFp_discardNQ = DFp_undersizeNQ;
        if (DFp_discardQ<0) {
            DFp_discardQ=0;
            DFp_discardNQ= external_Ddiscard/(DFp_NQ);
        }
        if (DFp_discardQ>1) {
            DFp_discardQ=1;
            DFp_discardNQ= external_Ddiscard/(DFp_NQ);
        }
        if (DFp_discardNQ<0) {
            DFp_discardNQ=0;
        }
        if (DFp_discardNQ>1) {
            DFp_discardNQ=1;
        }

        DFip_discardNQ = twomin(1,(DFp_discardNQ * D_i_weighting));
        DFop_discardNQ = twomin(1,(DFp_discardNQ * D_o_weighting));

        DFip_discardQ  = twomin(1,(DFp_discardQ * D_i_weighting));
        DFop_discardQ  = twomin(1,(DFp_discardQ * D_o_weighting));


        // If the externally prescribed overall demersal discard rates are zero, then force both the quota and non-quota discard rates to be zero too

        if(external_Didiscard == 0) {
	     DFip_discardNQ = 0;
             DFip_discardQ  = 0;
        }

        if(external_Dodiscard == 0) {
             DFop_discardNQ = 0;
             DFop_discardQ  = 0;
	}

        }


        // OR............................... 


        // If the switch DFdiscard_switch=2 then.....
   //   Alternative verson to represent the EU LANDING OBLIGATION case

        // DFdiscard_switch = 2
        if (DFdiscard_switch>1.5 && DFdiscard_switch<2.5) {
         Ddiscard = twomin(1,(((DFp_undersizeNQ*DFp_NQ) )));
// total discard rate is just the non quota part

//      This bit as in the normal code
        DFp_discardNQ = twomin(1,(DFp_undersizeNQ));
        DFip_discardNQ = twomin(1,(DFp_discardNQ * D_i_weighting));
        DFop_discardNQ = twomin(1,(DFp_discardNQ * D_o_weighting));

//      But the quota limited discards are set to ZERO
        DFp_discardQ  = 0;
        DFip_discardQ  = twomin(1,(DFp_discardQ * D_i_weighting));
        DFop_discardQ  = twomin(1,(DFp_discardQ * D_o_weighting));

        }


        // OR............................... 


        // If the switch DFdiscard_switch=3 then.....
   //   All demersal fish discards are set to zero regardless of selectivity

        // DFdiscard_switch = 3
        if (DFdiscard_switch>2.5 && DFdiscard_switch<3.5) {
         Ddiscard = 0;

//      Non Quota discards are set to ZERO
        DFp_discardNQ = 0;
        DFip_discardNQ = twomin(1,(DFp_discardNQ * D_i_weighting));
        DFop_discardNQ = twomin(1,(DFp_discardNQ * D_o_weighting));

//      Quota limited discards are set to ZERO
        DFp_discardQ  = 0;
        DFip_discardQ  = twomin(1,(DFp_discardQ * D_i_weighting));
        DFop_discardQ  = twomin(1,(DFp_discardQ * D_o_weighting));

        }


        // ---------------------------------------------------------------------------------------
        // ---------------------------------------------------------------------------------------



//    For diagnostics....
//        Rprintf("time=%f last_year=%d, this_year=%d, jan_fishd=%f, DFp_NQ=%f, DFp_undersizeNQ=%f, DFp_undersizeQ=%f,Ddiscard=%f  -> \n", *t, last_year, this_year,jan_fishd, DFp_NQ, DFp_undersizeNQ, DFp_undersizeQ,Ddiscard);
//        Rprintf("this_year=%d, jan_fishd=%f, DFp_NQ=%f, DFp_undersizeNQ=%f, DFp_undersizeQ=%f,DFp_discardNQ=%f, DFp_discardQ=%f,Ddiscard=%f  -> \n", this_year,jan_fishd, DFp_NQ, DFp_undersizeNQ, DFp_undersizeQ,DFp_discardNQ, DFp_discardQ,Ddiscard);

        last_year = this_year;
        }

/* ______ End start-of-year-based calculations ______ */




//  REST OF THE CALCULATIONS ARE PERFORMED EVERY TIME INCREMENT


/* _____ EDIBLE BIOMASS OF CARNIVOROUS ZOOPLANKTON _____ */

 if(y[40]>0) {
         CZ_edible_o  = twomax(0,y[40]-(CZ_inedible_o/y[40])) ;
 }
 else {
         CZ_edible_o  = 0;
 } 

 if(y[61]>0) {
         CZ_edible_i  = twomax(0,y[61]-(CZ_inedible_i/y[61])) ;
 }
 else {
         CZ_edible_i  = 0;
 } 



/* _____dynamic vertical distributions of zooplankton and benthic larvae _____ */
 prop_herb_surfo=         ((udet_herbt*y[0])+(uphyt_herbt*y[37]))/((udet_herbt*y[0])+(udet_herbt*y[1])+(uphyt_herbt*y[37])+(uphyt_herbt*y[38]));
 prop_herb_deep=(1-prop_herb_surfo);

 prop_benthslar_surfo= ((udet_benthslart*y[0])+(uphyt_benthslart*y[37]))/((udet_benthslart*y[0])+(udet_benthslart*y[1])+(uphyt_benthslart*y[37])+(uphyt_benthslart*y[38]));
 prop_benthslar_deep=(1-prop_benthslar_surfo);

 prop_benthclar_surfo= ((udet_benthclart*y[0])+(uphyt_benthclart*y[37]))/((udet_benthclart*y[0])+(udet_benthclart*y[1])+(uphyt_benthclart*y[37])+(uphyt_benthclart*y[38]));
 prop_benthclar_deep=(1-prop_benthclar_surfo);

 prop_fishplar_surfo=((uherb_fishplart*prop_herb_surfo*y[39])+(ubenthslar_fishplart*prop_benthslar_surfo*y[41])+(ubenthclar_fishplart*prop_benthclar_surfo*y[43]))/((uherb_fishplart*y[39])+(ubenthslar_fishplart*y[41])+(ubenthclar_fishplart*y[43]));
 prop_fishplar_deep=(1-prop_fishplar_surfo);

 prop_fishdlar_surfo=((uherb_fishdlart*prop_herb_surfo*y[39])+(ubenthslar_fishdlart*prop_benthslar_surfo*y[41])+(ubenthclar_fishdlart*prop_benthclar_surfo*y[43]))/((uherb_fishdlart*y[39])+(ubenthslar_fishdlart*y[41])+(ubenthclar_fishdlart*y[43]));
 prop_fishdlar_deep=(1-prop_fishdlar_surfo);

 prop_carn_surfo=((uherb_carnt*prop_herb_surfo*y[39])+(ubenthslar_carnt*prop_benthslar_surfo*y[41])+(ubenthclar_carnt*prop_benthclar_surfo*y[43])+(ufishplar_carnt*prop_fishplar_surfo*y[46])+(ufishdlar_carnt*prop_fishdlar_surfo*y[48]))/((uherb_carnt*y[39])+(ubenthslar_carnt*y[41])+(ubenthclar_carnt*y[43])+(ufishplar_carnt*y[46])+(ufishdlar_carnt*y[48]));
 prop_carn_deep=(1-prop_carn_surfo);


// Rprintf("prop_herb_surf_o=%f\n", prop_herb_surfo);
// Rprintf("prop_herb_deep=%f\n", prop_herb_deep);

// Rprintf("prop_benthslar_surfo=%f\n", prop_benthslar_surfo);
// Rprintf("prop_benthslar_deep=%f\n", prop_benthslar_deep);

// Rprintf("prop_benthclar_surfo=%f\n", prop_benthclar_surfo);
// Rprintf("prop_benthclar_deep=%f\n", prop_benthclar_deep);

// Rprintf("prop_carn_surf_o=%f\n", prop_carn_surfo);
// Rprintf("prop_carn_deep=%f\n", prop_carn_deep);

// Rprintf("prop_fishplar_surfo=%f\n", prop_fishplar_surfo);
// Rprintf("prop_fishplar_deep=%f\n", prop_fishplar_deep);

// Rprintf("prop_fishdlar_surfo=%f\n", prop_fishdlar_surfo);
// Rprintf("prop_fishdlar_deep=%f\n", prop_fishdlar_deep);

/* __________ Food concentration gradients for pelagic and migratory fish and birds&mammals, relative to their own gradients___________*/

// if log-gradients are >0 the food is higher offshore relative to predator so predator needs to shift offshore
// if log-gradients are <0 then food is higher inshore relative to predator so predator needs to shift inshore

food_gradient_pfish =  log( ( ((uherb_fishpt * y[39] + ucarn_fishpt * CZ_edible_o
                                         +ufishplar_fishpt * y[46] + ufishdlar_fishpt * y[48]
                                         +ubenthslar_fishpt * y[41] + ubenthclar_fishpt * y[43])/(1-shallowprop))
                                      / ((uherb_fishpt * y[60] + ucarn_fishpt * CZ_edible_i
                                         +ufishplar_fishpt * y[62] + ufishdlar_fishpt * y[63]
                                         +ubenthslar_fishpt * y[55] + ubenthclar_fishpt * y[56])/(shallowprop))
                                      )  / ( (y[45]/(1-shallowprop))/(y[64]/shallowprop) ) );

food_gradient_mfish =  log( ( ((uherb_fishmt * y[39] + ucarn_fishmt * CZ_edible_o
                                         +ufishplar_fishmt * y[46] + ufishdlar_fishmt * y[48]
                                         +ubenthslar_fishmt * y[41] + ubenthclar_fishmt * y[43])/(1-shallowprop))
                                      / ((uherb_fishmt * y[60] + ucarn_fishmt * CZ_edible_i
                                         +ufishplar_fishmt * y[62] + ufishdlar_fishmt * y[63]
                                         +ubenthslar_fishmt * y[55] + ubenthclar_fishmt * y[56])/(shallowprop))
                                      )  / ( (y[49]/(1-shallowprop))/(y[65]/shallowprop) ) );

food_gradient_bird =    log( ( ((ucarn_bird * CZ_edible_o
                                         +ubenths_bird * y[42] + ubenthc_bird * y[44]
                                         +udisc_bird * y[14] + ucorp_bird * (y[73]+y[18]+y[19]+y[20])
                                         +ufishp_bird * y[45] + ufishd_bird * y[47]  + ufishm_bird * y[49])  /(1-shallowprop))
                                      / ((ucarn_bird * CZ_edible_i
                                         +ubenths_bird * y[57] + ubenthc_bird * y[58]
                                         +udisc_bird * y[59] + ucorp_bird * (y[72]+y[15]+y[16]+y[17])
                                         +ufishp_bird * y[64] + ufishd_bird * y[66] + ufishm_bird * y[65])/(shallowprop))
                                      )  / ( (y[50]/(1-shallowprop))/(y[67]/shallowprop) ) );

food_gradient_seal =    log( ( ((ucarn_seal * CZ_edible_o
                                         +ubenths_seal * y[42] + ubenthc_seal * y[44]
                                         +udisc_seal * y[14] + ucorp_seal * (y[73]+y[18]+y[19]+y[20]) + ubird_seal*y[50]
                                         +ufishp_seal * y[45] + ufishd_seal * y[47]  + ufishm_seal * y[49])  /(1-shallowprop))
                                      / ((ucarn_seal * CZ_edible_i
                                         +ubenths_seal * y[57] + ubenthc_seal * y[58]
                                         +udisc_seal * y[59] + ucorp_seal * (y[72]+y[15]+y[16]+y[17]) + ubird_seal*y[67]
                                         +ufishp_seal * y[64] + ufishd_seal * y[66] + ufishm_seal * y[65])/(shallowprop))
                                      )  / ( (y[68]/(1-shallowprop))/(y[69]/shallowprop) ) );


food_gradient_ceta =    log( ( ((uherb_ceta * y[39] + ucarn_ceta * CZ_edible_o
                                         +ubenths_ceta * y[42] + ubenthc_ceta * y[44]
                                         +udisc_ceta * y[14] + ubird_ceta*y[50] + useal_ceta*y[68]
                                         +ufishp_ceta * y[45] + ufishd_ceta * y[47]  + ufishm_ceta * y[49])  /(1-shallowprop))
                                      / ((uherb_ceta * y[60] + ucarn_ceta * CZ_edible_i
                                         +ubenths_ceta * y[57] + ubenthc_ceta * y[58]
                                         +udisc_ceta * y[59] + ubird_ceta*y[67] + useal_ceta*y[69]
                                         +ufishp_ceta * y[64] + ufishd_ceta * y[66] + ufishm_ceta * y[65])/(shallowprop))
                                      )  / ( (y[70]/(1-shallowprop))/(y[71]/shallowprop) ) );



food_gradient_dfish =  log( ( (( ucarn_fishdt * CZ_edible_o
                                         +ufishplar_fishdt * y[46] + ufishdlar_fishdt * y[48]
                                         + ufishp_fishdt * y[45]
                                         + ufishm_fishdt * y[49]
                                         + ufishd_fishdt * y[47]
                                         + ubenths_fishdt * y[42] + ubenthc_fishdt * y[44]
                                         + udisc_fishdt * y[14] + ucorp_fishdt * (y[73]+y[18]+y[19]+y[20])
                                         )/(1-shallowprop))

                                      / ((ucarn_fishdt * CZ_edible_i
                                         +ufishplar_fishdt * y[62] + ufishdlar_fishdt * y[63]
                                         + ufishp_fishdt * y[64]      
                                         + ufishm_fishdt * y[65]      
                                         + ufishd_fishdt * y[66]      
                                         + ubenths_fishdt * y[57] + ubenthc_fishdt * y[58]
                                         + udisc_fishdt * y[59] + ucorp_fishdt * (y[72]+y[15]+y[16]+y[17])
                                         )/(shallowprop))
                                      )  / ( (y[47]/(1-shallowprop))/(y[66]/shallowprop) ) );


/* _____Apply the appropriate q10 and surface temperature to the uptake and metabolic parameters_____ */


/* Calculate some  weighted average temperatures  */


/* Whole domain average temperature */
// driverhdtemp = ((driversotemp*volume_so)+(driversitemp*volume_si)+(driverdtemp*volume_d))/(volume_so+volume_si+volume_d); // NOT USED IN THIS CODE

/* Water column average temperature in the offshore region */
driverwcotemp = ((driversotemp*volume_so)+(driverdtemp*volume_d))/(volume_so+volume_d);

/* Zooplankton offshore vertical distribution weighted average temperature */
driverzodtemp = ((driversotemp*prop_herb_surfo)+(driverdtemp*prop_herb_deep))/(prop_herb_surfo+prop_herb_deep);

/* Offshore benthslarvae distribution weighted average temperature */
driverbslotemp = ((driversotemp*prop_benthslar_surfo)+(driverdtemp*prop_benthslar_deep))/(prop_benthslar_surfo+prop_benthslar_deep);

/* Offshore benthslarvae distribution weighted average temperature */
driverbclotemp = ((driversotemp*prop_benthclar_surfo)+(driverdtemp*prop_benthclar_deep))/(prop_benthslar_surfo+prop_benthslar_deep);



/* _____Autotrophic uptake parameters at offshore surface temperatures_____       cccccccccccccccccc*/
 uNIT_phyt_o=	  (exp((((driversotemp -qtenr)*log(qtena))/10)+log(uNIT_phytt)));
 uAMM_phyt_o=     (exp((((driversotemp -qtenr)*log(qtena))/10)+log(uAMM_phytt)));


/* _____Autotrophic uptake and kelp excretion parameters at inshore surface temperatures_____       cccccccccccccccccc*/
 uC_kelp_i  =     (exp((((driversitemp -qtenr)*log(qtena))/10)+log(umaxC_kelpt)));
 exC_kelp_i =     (exp((((driversitemp -qtenr)*log(qtenm))/10)+log(exudeC_kelpt_i))); 

 uNIT_kelp_i=     (exp((((driversitemp -qtenr)*log(qtena))/10)+log(uNIT_kelpt))); 
 uAMM_kelp_i=     (exp((((driversitemp -qtenr)*log(qtena))/10)+log(uAMM_kelpt))); 

 uNIT_phyt_i=	  (exp((((driversitemp -qtenr)*log(qtena))/10)+log(uNIT_phytt)));
 uAMM_phyt_i=     (exp((((driversitemp -qtenr)*log(qtena))/10)+log(uAMM_phytt)));
 

/* _____ Zooplankton uptake parameters at offshore vertical distribution  average temperatures */

 ubenthslar_o_herb=(exp((((driverzodtemp-qtenr)*log(qtenh))/10)+log(ubenthslar_herbt)));
 ubenthclar_o_herb=(exp((((driverzodtemp-qtenr)*log(qtenh))/10)+log(ubenthclar_herbt)));

 eH_o =                (exp((((driverzodtemp-qtenr)*log(qtenm))/10)+log(eHt)));

/* _____ Zooplankton uptake parameters at in-situ temperatures */

 udet_so_herb=      (exp((((driversotemp-qtenr)*log(qtenh))/10)+log(udet_herbt)));
 udet_si_herb=      (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(udet_herbt)));
 udet_d_herb=       (exp((((driverdtemp -qtenr)*log(qtenh))/10)+log(udet_herbt)));

 uphyt_so_herb=      (exp((((driversotemp-qtenr)*log(qtenh))/10)+log(uphyt_herbt)));
 uphyt_si_herb=      (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(uphyt_herbt)));
 uphyt_d_herb=       (exp((((driverdtemp -qtenr)*log(qtenh))/10)+log(uphyt_herbt)));

 ubenthslar_i_herb=(exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthslar_herbt)));
 ubenthclar_i_herb=(exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthclar_herbt)));

 eH_i =                (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(eHt)));



/* _____Offshore benthos larvae metabolic parameters at offshore benthos larvae distribution weighted averaged temperatures_____ */

 eBslar_o=(exp((((driverbslotemp-qtenr)*log(qtenm))/10)+log(eBslart)));
 eBclar_o=(exp((((driverbclotemp-qtenr)*log(qtenm))/10)+log(eBclart)));

/* _____ Offshore benthic larvae uptake parameters at in-situ temperatures */

 udet_so_benthslar=  (exp((((driversotemp-qtenr)*log(qtenh))/10)+log(udet_benthslart)));
 udet_d_benthslar=   (exp((((driverdtemp -qtenr)*log(qtenh))/10)+log(udet_benthslart)));

 uphyt_so_benthslar= (exp((((driversotemp-qtenr)*log(qtenh))/10)+log(uphyt_benthslart)));
 uphyt_d_benthslar=  (exp((((driverdtemp -qtenr)*log(qtenh))/10)+log(uphyt_benthslart)));

 udet_so_benthclar=  (exp((((driversotemp-qtenr)*log(qtenh))/10)+log(udet_benthclart)));
 udet_d_benthclar=   (exp((((driverdtemp -qtenr)*log(qtenh))/10)+log(udet_benthclart)));

 uphyt_so_benthclar= (exp((((driversotemp-qtenr)*log(qtenh))/10)+log(uphyt_benthclart)));
 uphyt_d_benthclar=  (exp((((driverdtemp -qtenr)*log(qtenh))/10)+log(uphyt_benthclart)));


/* ______ Inshore benthic larvae uptake and metabolic parameters ____________________*/

 eBslar_i=(exp((((driversitemp-qtenr)*log(qtenm))/10)+log(eBslart)));
 eBclar_i=(exp((((driversitemp-qtenr)*log(qtenm))/10)+log(eBclart)));

 udet_si_benthslar=  (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(udet_benthslart )));
 uphyt_si_benthslar= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(uphyt_benthslart)));

 udet_si_benthclar=  (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(udet_benthclart )));
 uphyt_si_benthclar= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(uphyt_benthclart)));



/* ______________ Inshore susp/dep feeding benthos  uptake and metabolic parameters a shallow inshore temperatures ____*/

 uphyt_si_benths_i=      (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(uphyt_benthst)));
 udet_si_benths_i=       (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(udet_benthst )));
 used_si_benths_i=       (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(used_benthst )));

 eBs_i=                  (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(eBst)));

/* ______________ Offshore susp/dep feeding benthos  uptake and metabolic parameters a deep temperatures ____*/

 uphyt_d_benths_o=      (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(uphyt_benthst)));
 udet_d_benths_o=       (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(udet_benthst )));
 used_d_benths_o=       (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(used_benthst )));

 eBs_o=                 (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(eBst)));

/* ______________ Inshore carn/scav feeding benthos  uptake and metabolic parameters a shallow inshore temperatures ____*/

 ukelpdebris_i_benthc_i=     (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ukelpdebris_benthct)));
 ukelp_i_benthc_i=     (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ukelp_benthct)));

 ubenths_i_benthc_i=     (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenths_benthct)));
 ucorp_i_benthc_i=       (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ucorp_benthct  )));

 eBc_i=                (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(eBct)));


/* ______________ Offshore carn/scav feeding benthos  uptake and metabolic parameters a deep temperatures ____*/

 ubenths_o_benthc_o=     (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ubenths_benthct)));
 ucorp_o_benthc_o=       (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ucorp_benthct)));

 eBc_o=                (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(eBct)));


/* ______________ Carnivorous zooplankton uptake and metabolic parameters at domain averaged temperatures ___*/

 uherb_o_carn=      (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(uherb_carnt)));
 uherb_i_carn=      (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(uherb_carnt)));

 ufishplar_o_carn=  (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ufishplar_carnt)));
 ufishplar_i_carn=  (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishplar_carnt)));

 ufishdlar_o_carn=  (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ufishdlar_carnt)));
 ufishdlar_i_carn=  (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishdlar_carnt)));

 ubenthslar_i_carn= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthslar_carnt)));
 ubenthclar_i_carn= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthclar_carnt)));

 ubenthslar_o_carn= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthslar_carnt)));
 ubenthclar_o_carn= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthclar_carnt)));

 eC_o=                (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(eCt)));
 eC_i=                (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(eCt)));



/* ______________ Pelagic fish larvae uptake and metabolic parameters at domain averaged temperatures ___*/

 uherb_o_fishplar=  (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(uherb_fishplart)));
 uherb_i_fishplar=  (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(uherb_fishplart)));

 ubenthslar_i_fishplar= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthslar_fishplart)));
 ubenthclar_i_fishplar= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthclar_fishplart)));

 ubenthslar_o_fishplar= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthslar_fishplart)));
 ubenthclar_o_fishplar= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthclar_fishplart)));

 eFplar_o=            (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(eFplart)));
 eFplar_i=            (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(eFplart)));


/* ______________ Demersal fish larvae uptake and metabolic parameters at domain averaged temperatures ___*/

 uherb_o_fishdlar=  (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(uherb_fishdlart)));
 uherb_i_fishdlar=  (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(uherb_fishdlart)));

 ubenthslar_i_fishdlar= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthslar_fishdlart)));
 ubenthclar_i_fishdlar= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthclar_fishdlart)));

 ubenthslar_o_fishdlar= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthslar_fishdlart)));
 ubenthclar_o_fishdlar= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthclar_fishdlart)));

 eFdlar_o=            (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(eFdlart)));
 eFdlar_i=            (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(eFdlart)));


/* ______________ Pelagic fish uptake and metabolic parameters at domain averaged temperatures ___*/

 uherb_o_fishp=     (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(uherb_fishpt)));
 uherb_i_fishp=     (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(uherb_fishpt)));

 ucarn_o_fishp=     (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ucarn_fishpt)));
 ucarn_i_fishp=     (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ucarn_fishpt)));

 ufishdlar_o_fishp= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ufishdlar_fishpt)));
 ufishdlar_i_fishp= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishdlar_fishpt)));

 ufishplar_o_fishp= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ufishplar_fishpt)));
 ufishplar_i_fishp= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishplar_fishpt)));

 ubenthslar_i_fishp= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthslar_fishpt)));
 ubenthclar_i_fishp= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthclar_fishpt)));

 ubenthslar_o_fishp= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthslar_fishpt)));
 ubenthclar_o_fishp= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthclar_fishpt)));

 eFp_o=               (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(eFpt)));
 eFp_i=               (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(eFpt)));



/* ______________ Migratory fish uptake and metabolic parameters at domain averaged temperatures ___*/

 uherb_o_fishm=     (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(uherb_fishmt)));
 uherb_i_fishm=     (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(uherb_fishmt)));

 ucarn_o_fishm=     (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ucarn_fishmt)));
 ucarn_i_fishm=     (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ucarn_fishmt)));

 ufishdlar_o_fishm= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ufishdlar_fishmt)));
 ufishdlar_i_fishm= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishdlar_fishmt)));

 ufishplar_o_fishm= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ufishplar_fishmt)));
 ufishplar_i_fishm= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishplar_fishmt)));

 ubenthslar_i_fishm= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthslar_fishmt)));
 ubenthclar_i_fishm= (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthclar_fishmt)));

 ubenthslar_o_fishm= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthslar_fishmt)));
 ubenthclar_o_fishm= (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(ubenthclar_fishmt)));

 eFm_o=               (exp((((driverwcotemp-qtenr)*log(qtenh))/10)+log(eFmt)));
 eFm_i=               (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(eFmt)));


/* ______________ Demersal fish uptake and metabolic parameters at bottom averaged temperatures ___*/

 ucarn_o_fishd=         (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ucarn_fishdt)));
 ucarn_i_fishd=         (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ucarn_fishdt)));

 ufishplar_o_fishd=     (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ufishplar_fishdt)));
 ufishplar_i_fishd=     (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishplar_fishdt)));

 ufishdlar_o_fishd=     (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ufishdlar_fishdt)));
 ufishdlar_i_fishd=     (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishdlar_fishdt)));

 ufishp_o_fishd=        (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ufishp_fishdt)));
 ufishp_i_fishd=        (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishp_fishdt)));

 ufishm_o_fishd=        (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ufishm_fishdt)));
 ufishm_i_fishd=        (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishm_fishdt)));

 ufishd_o_fishd=        (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ufishd_fishdt)));
 ufishd_i_fishd=        (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ufishd_fishdt)));

 ubenths_i_fishd=       (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenths_fishdt)));
 ubenthc_i_fishd=       (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ubenthc_fishdt)));

 ubenths_o_fishd=       (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ubenths_fishdt)));
 ubenthc_o_fishd=       (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ubenthc_fishdt)));

 udisc_i_fishd=         (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(udisc_fishdt)));
 ucorp_i_fishd=         (exp((((driversitemp-qtenr)*log(qtenh))/10)+log(ucorp_fishdt)));

 udisc_o_fishd=         (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(udisc_fishdt)));
 ucorp_o_fishd=         (exp((((driverdtemp-qtenr)*log(qtenh))/10)+log(ucorp_fishdt)));

 eFd_o=               (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(eFdt)));
 eFd_i=               (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(eFdt)));



/* ______________ Bird/mammal metabolic parameters at surface averaged temperatures ___*/

ebird_o=             (exp((((driversotemp-qtenr)*log(qtenh)*(-1))/10)+log(ebirdt)));
ebird_i=             (exp((((driversitemp-qtenr)*log(qtenh)*(-1))/10)+log(ebirdt)));

// NEW -------------------------------------
eseal_o=             (exp((((driversotemp-qtenr)*log(qtenh)*(-1))/10)+log(esealt)));
eseal_i=             (exp((((driversitemp-qtenr)*log(qtenh)*(-1))/10)+log(esealt)));

// NEW -------------------------------------
eceta_o=             (exp((((driversotemp-qtenr)*log(qtenh)*(-1))/10)+log(ecetat)));
eceta_i=             (exp((((driversitemp-qtenr)*log(qtenh)*(-1))/10)+log(ecetat)));

// - NOTE THAT FOR BIRDS AND MAMMALS THE TEMPERATURE RESPONSE IS AN INVERSE Q10 EFFECT

/* ______________ no temperature correction needed here as max uptake rates assumed independent of temperature______*/
ubenths_i_bird = ubenths_bird;
ubenthc_i_bird = ubenthc_bird;
ubenths_o_bird = ubenths_bird;
ubenthc_o_bird = ubenthc_bird;

udisc_i_bird = udisc_bird;
ucorp_i_bird = ucorp_bird;
//  uherb_i_bird = uherb_bird;
ucarn_i_bird = ucarn_bird;
ufishp_i_bird = ufishp_bird;
ufishm_i_bird = ufishm_bird;
ufishd_i_bird = ufishd_bird;

udisc_o_bird = udisc_bird;
ucorp_o_bird = ucorp_bird;
//  uherb_o_bird = uherb_bird;
ucarn_o_bird = ucarn_bird;
ufishp_o_bird = ufishp_bird;
ufishm_o_bird = ufishm_bird;
ufishd_o_bird = ufishd_bird;

// -------

ubenths_i_seal = ubenths_seal;
ubenthc_i_seal = ubenthc_seal;
ubenths_o_seal = ubenths_seal;
ubenthc_o_seal = ubenthc_seal;

udisc_i_seal = udisc_seal;
ucorp_i_seal = ucorp_seal;
ucarn_i_seal = ucarn_seal;
ufishp_i_seal = ufishp_seal;
ufishm_i_seal = ufishm_seal;
ufishd_i_seal = ufishd_seal;
ubird_i_seal = ubird_seal;

udisc_o_seal = udisc_seal;
ucorp_o_seal = ucorp_seal;
ucarn_o_seal = ucarn_seal;
ufishp_o_seal = ufishp_seal;
ufishm_o_seal = ufishm_seal;
ufishd_o_seal = ufishd_seal;
ubird_o_seal = ubird_seal;


// -------

ubenths_i_ceta = ubenths_ceta;
ubenthc_i_ceta = ubenthc_ceta;
ubenths_o_ceta = ubenths_ceta;
ubenthc_o_ceta = ubenthc_ceta;

udisc_i_ceta = udisc_ceta;
//  ucorp_i_ceta = ucorp_ceta;
uherb_i_ceta = uherb_ceta;
ucarn_i_ceta = ucarn_ceta;
ufishp_i_ceta = ufishp_ceta;
ufishm_i_ceta = ufishm_ceta;
ufishd_i_ceta = ufishd_ceta;
ubird_i_ceta = ubird_ceta;
useal_i_ceta = useal_ceta;


udisc_o_ceta = udisc_ceta;
//  ucorp_o_ceta = ucorp_ceta;
uherb_o_ceta = uherb_ceta;
ucarn_o_ceta = ucarn_ceta;
ufishp_o_ceta = ufishp_ceta;
ufishm_o_ceta = ufishm_ceta;
ufishd_o_ceta = ufishd_ceta;
ubird_o_ceta = ubird_ceta;
useal_o_ceta = useal_ceta;


/* _____Biogeochemical parameters at surface temperature_____ */

 m_si=               (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(mt)));
 n_si=               (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(ndt)));
 d_si=               (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(ddt)));
// NOTE - nitrification and denitrification rates in the inshore zone as per the deep zone offshore

 m_so=               (exp((((driversotemp-qtenr)*log(qtenm))/10)+log(mt)));
 n_so=               (exp((((driversotemp-qtenr)*log(qtenm))/10)+log(nst)));
 d_so=               (exp((((driversotemp-qtenr)*log(qtenm))/10)+log(dst)));


 if (rock_s1>0.5) {
         msed_s1=            ( (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(msedt))) ) + msens*log10(sed_ref_Kxw/sed_wat_dif_s1)  ;
	 nsed_s1=            ( (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(nsedt))) ) + nsens*log10(sed_ref_Kxw/sed_wat_dif_s1)  ;
	 dsed_s1=            ( (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(dsedt))) ) + dsens*log10(sed_ref_Kxw/sed_wat_dif_s1)  ;
 }
 else {
         msed_s1=0;
         nsed_s1=0;
	 dsed_s1=0;
 }
 
 
  if (rock_s2>0.5) {
 	 msed_s2=            ( (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(msedt))) ) + msens*log10(sed_ref_Kxw/sed_wat_dif_s2)  ;
	 nsed_s2=            ( (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(nsedt))) ) + nsens*log10(sed_ref_Kxw/sed_wat_dif_s2)  ;
	 dsed_s2=            ( (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(dsedt))) ) + dsens*log10(sed_ref_Kxw/sed_wat_dif_s2)  ;
  }
  else {
         msed_s2=0;
         nsed_s2=0;
	 dsed_s2=0;
 }
    
  if (rock_s3>0.5) {
	 msed_s3=            ( (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(msedt))) ) + msens*log10(sed_ref_Kxw/sed_wat_dif_s3)  ;
	 nsed_s3=            ( (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(nsedt))) ) + nsens*log10(sed_ref_Kxw/sed_wat_dif_s3)  ;
	 dsed_s3=            ( (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(dsedt))) ) + dsens*log10(sed_ref_Kxw/sed_wat_dif_s3)  ;
  }
  else {
         msed_s3=0;
         nsed_s3=0;
	 dsed_s3=0;
 }

 
 corp_det_i=        (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(corp_det)));

 kelpdebris_det_i = (exp((((driversitemp-qtenr)*log(qtenm))/10)+log(kelpdebris_det)));



/* _____Metabolic and biogeochemical parameters at deep temperature_____*/
 m_d=               (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(mt)));
 n_d=               (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(ndt)));
 d_d=               (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(ddt)));

 
  if (rock_d1>0.5) {
	 msed_d1=            ( (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(msedt))) ) + msens*log10(sed_ref_Kxw/sed_wat_dif_d1)  ;
	 nsed_d1=            ( (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(nsedt))) ) + nsens*log10(sed_ref_Kxw/sed_wat_dif_d1)  ;
	 dsed_d1=            ( (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(dsedt))) ) + dsens*log10(sed_ref_Kxw/sed_wat_dif_d1)  ;
  }
  else {
         msed_d1=0;
         nsed_d1=0;
	 dsed_d1=0;
 }
   
    
   if (rock_d2>0.5) {
	 msed_d2=            ( (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(msedt))) ) + msens*log10(sed_ref_Kxw/sed_wat_dif_d2)  ;
	 nsed_d2=            ( (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(nsedt))) ) + nsens*log10(sed_ref_Kxw/sed_wat_dif_d2)  ;
	 dsed_d2=            ( (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(dsedt))) ) + dsens*log10(sed_ref_Kxw/sed_wat_dif_d2)  ;
  }
  else {
         msed_d2=0;
         nsed_d2=0;
	 dsed_d2=0;
 }


   if (rock_d3>0.5) {
	 msed_d3=            ( (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(msedt))) ) + msens*log10(sed_ref_Kxw/sed_wat_dif_d3)  ;
	 nsed_d3=            ( (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(nsedt))) ) + nsens*log10(sed_ref_Kxw/sed_wat_dif_d3)  ;
	 dsed_d3=            ( (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(dsedt))) ) + dsens*log10(sed_ref_Kxw/sed_wat_dif_d3)  ;
  }
  else {
         msed_d3=0;
         nsed_d3=0;
	 dsed_d3=0;
 }

 
 
 corp_det_o=         (exp((((driverdtemp-qtenr)*log(qtenm))/10)+log(corp_det)));



/* _____ Irradiance and light attenuation______  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */

 kvertattn_o =  lightSPM_intercept + lightSPM_slope * exp(driverlogespm_o) ;    // Parameterised relationship between light vertical attenuation coefficient and SPM
 kvertattn_i =  lightSPM_intercept + lightSPM_slope * exp(driverlogespm_i) ;    // Parameterised relationship between light vertical attenuation coefficient and SPM


 phyt_propsl_o =        (((1/kvertattn_o)*exp(-kvertattn_o*0)) - ((1/kvertattn_o)*exp(-kvertattn_o*thik_so)))/(thik_so);
 phyt_propsl_i =        (((1/kvertattn_i)*exp(-kvertattn_i*0)) - ((1/kvertattn_i)*exp(-kvertattn_i*(thik_si*inshore_phyt_depth_prop))))/(thik_si*inshore_phyt_depth_prop);
 kelp_propsl_i =        (((1/kvertattn_i)*exp(-kvertattn_i*0)) - ((1/kvertattn_i)*exp(-kvertattn_i*(thik_si*inshore_kelp_depth_prop))))/(thik_si*inshore_kelp_depth_prop);
 phyt_S_layer_light_o = phyt_propsl_o*driversslight;
 phyt_S_layer_light_i = phyt_propsl_i*driversslight;
 kelp_S_layer_light_i = kelp_propsl_i*driversslight;


/* _____Now start calcualating the various flux terms_____ */

/* _____Uptake rates_____ CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC*/


// Kelp
// _____________

kelp_Ucsc =  kelp_i_slope * ((y[75]/y[74]) - NCmin_kelp);          // Attenuation factor for Carbon uptake
if(kelp_Ucsc<0) {
   kelp_Ucsc=0;
}
if(kelp_Ucsc>1) {
   kelp_Ucsc=1;
}


kelp_Unsc = kelp_i_slope * (NCmax_kelp - (y[75]/y[74]));         // Attenuation factor for nitrogen uptake
if(kelp_Unsc<0) {
   kelp_Unsc=0;
}
if(kelp_Unsc>1) {
   kelp_Unsc=1;
}

Upt_C_kelp_i   = kelp_Ucsc * (f4(y[74],kelp_S_layer_light_i,uC_kelp_i,Lmaxup_kelp,selfshade_kelp_i) );
Upt_samm_kelp_i= kelp_Unsc * (f1(y[52],y[74],uAMM_kelp_i,(hsAMM_kelp*volume_si)) );         
Upt_snit_kelp_i= kelp_Unsc * (f1(y[53],y[74],uNIT_kelp_i,(hsNIT_kelp*volume_si)) );          



// Phytoplankton
// _____________

 Upt_samm_sphyt_o=         (f2(y[21],y[37],phyt_S_layer_light_o,uAMM_phyt_o,(hsAMM_phyt*volume_so),Lmaxup_phyt));
 Upt_snit_sphyt_o=         (f2(y[29],y[37],phyt_S_layer_light_o,uNIT_phyt_o,(hsNIT_phyt*volume_so),Lmaxup_phyt));

 Upt_samm_sphyt_i=         (f2(y[52],y[54],phyt_S_layer_light_i,uAMM_phyt_i,(hsAMM_phyt*volume_si),Lmaxup_phyt));
 Upt_snit_sphyt_i=         (f2(y[53],y[54],phyt_S_layer_light_i,uNIT_phyt_i,(hsNIT_phyt*volume_si),Lmaxup_phyt));


// Zooplankton offshore
// _____________

 Upt_detritus_so_herb=     (f1((y[0]),(y[39]*prop_herb_surfo),udet_so_herb,(hsdet_herb*(volume_so))));
 Upt_detritus_d_herb=     (f1(y[1],(y[39]*prop_herb_deep),udet_d_herb,(hsdet_herb*volume_d)));

 Upt_phyt_so_herb=         (f1((y[37]),(y[39]*prop_herb_surfo),uphyt_so_herb,(hsphyt_herb*(volume_so))));
 Upt_phyt_d_herb=         (f1(y[38],(y[39]*prop_herb_deep),uphyt_d_herb,(hsphyt_herb*volume_d)));

 Upt_benthslar_o_herb=     (f1((y[41]),(y[39]*(prop_herb_surfo+prop_herb_deep)),ubenthslar_o_herb,(hsbenthslar_herb*(volume_so+volume_d))));
 Upt_benthclar_o_herb=     (f1((y[43]),(y[39]*(prop_herb_surfo+prop_herb_deep)),ubenthclar_o_herb,(hsbenthclar_herb*(volume_so+volume_d))));

// Sum over all prey
      Upt_total_herb_o = Upt_detritus_so_herb + Upt_detritus_d_herb
                      +Upt_phyt_so_herb + Upt_phyt_d_herb
                      +Upt_benthslar_o_herb + Upt_benthclar_o_herb;


// Zooplankton inshore
// _____________

 Upt_detritus_si_herb=     (f1((y[51]),(y[60]),udet_si_herb,(hsdet_herb*(volume_si))));

 Upt_phyt_si_herb=         (f1((y[54]),(y[60]),uphyt_si_herb,(hsphyt_herb*(volume_si))));

 Upt_benthslar_i_herb=     (f1((y[55]),(y[60]),ubenthslar_i_herb,(hsbenthslar_herb*(volume_si))));
 Upt_benthclar_i_herb=     (f1((y[56]),(y[60]),ubenthclar_i_herb,(hsbenthclar_herb*(volume_si))));

// Sum over all prey
      Upt_total_herb_i = Upt_detritus_si_herb +
                      +Upt_phyt_si_herb 
                      +Upt_benthslar_i_herb + Upt_benthclar_i_herb ;




// Larvae of offshore susp/dep feeding benthos
// _____________

 Upt_detritus_so_benthslar=(f1(y[0],(y[41]*prop_benthslar_surfo),udet_so_benthslar,(hsdet_benthslar*volume_so)));
 Upt_detritus_d_benthslar=(f1(y[1],(y[41]*prop_benthslar_deep),udet_d_benthslar,(hsdet_benthslar*volume_d)));

 Upt_phyt_so_benthslar=    (f1(y[37],(y[41]*prop_benthslar_surfo),uphyt_so_benthslar,(hsphyt_benthslar*volume_so)));
 Upt_phyt_d_benthslar=    (f1(y[38],(y[41]*prop_benthslar_deep),uphyt_d_benthslar,(hsphyt_benthslar*volume_d)));

// Sum over all prey
      Upt_total_benthslar_o = Upt_detritus_so_benthslar + Upt_detritus_d_benthslar
                             +Upt_phyt_so_benthslar + Upt_phyt_d_benthslar ;



// Larvae of offshore carn/scav feeding benthos
// _____________

 Upt_detritus_so_benthclar=(f1(y[0],(y[43]*prop_benthclar_surfo),udet_so_benthclar,(hsdet_benthclar*volume_so)));
 Upt_detritus_d_benthclar=(f1(y[1],(y[43]*prop_benthclar_deep),udet_d_benthclar,(hsdet_benthclar*volume_d)));

 Upt_phyt_so_benthclar=    (f1(y[37],(y[43]*prop_benthclar_surfo),uphyt_so_benthclar,(hsphyt_benthclar*volume_so)));
 Upt_phyt_d_benthclar=    (f1(y[38],(y[43]*prop_benthclar_deep),uphyt_d_benthclar,(hsphyt_benthclar*volume_d)));

// Sum over all prey
      Upt_total_benthclar_o = Upt_detritus_so_benthclar + Upt_detritus_d_benthclar
                             +Upt_phyt_so_benthclar + Upt_phyt_d_benthclar ;


// Larvae of inshore susp/dep feeding benthos
// _____________

 Upt_detritus_si_benthslar=(f1(y[51],(y[55]),udet_si_benthslar,(hsdet_benthslar*volume_si)));
 Upt_phyt_si_benthslar=    (f1(y[54],(y[55]),uphyt_si_benthslar,(hsphyt_benthslar*volume_si)));

// Sum over all prey
      Upt_total_benthslar_i = Upt_detritus_si_benthslar + Upt_phyt_si_benthslar ;


// Larvae of inshore carn/scav feeding benthos
// _____________

 Upt_detritus_si_benthclar=(f1(y[51],(y[56]),udet_si_benthclar,(hsdet_benthclar*volume_si)));
 Upt_phyt_si_benthclar=    (f1(y[54],(y[56]),uphyt_si_benthclar,(hsphyt_benthclar*volume_si)));

// Sum over all prey
      Upt_total_benthclar_i = Upt_detritus_si_benthclar + Upt_phyt_si_benthclar ;




// Offshore Zooplankton carnivores
// _____________

 Upt_herb_o_carn=          (f1(y[39],y[40],uherb_o_carn,(hsherb_carn*(volume_so+volume_d))));

 Upt_fishplar_o_carn=      (f1(y[46],y[40],ufishplar_o_carn,(hsfishplar_carn*(volume_so+volume_d))));

 Upt_fishdlar_o_carn=      (f1(y[48],y[40],ufishdlar_o_carn,(hsfishdlar_carn*(volume_so+volume_d))));

 Upt_benthslar_o_carn=     (f1((y[41]),y[40],ubenthslar_o_carn,(hsbenthslar_carn*(volume_so+volume_d))));
 Upt_benthclar_o_carn=     (f1((y[43]),y[40],ubenthclar_o_carn,(hsbenthclar_carn*(volume_so+volume_d))));


// Sum over all prey
      Upt_total_carn_o = Upt_herb_o_carn + Upt_fishplar_o_carn + Upt_fishdlar_o_carn 
                      +Upt_benthslar_o_carn + Upt_benthclar_o_carn ;


// Inshore Zooplankton carnivores
// _____________

 Upt_herb_i_carn=          (f1(y[60],y[61],uherb_i_carn,(hsherb_carn*(volume_si))));

 Upt_fishplar_i_carn=      (f1(y[62],y[61],ufishplar_i_carn,(hsfishplar_carn*(volume_si))));

 Upt_fishdlar_i_carn=      (f1(y[63],y[61],ufishdlar_i_carn,(hsfishdlar_carn*(volume_si))));

 Upt_benthslar_i_carn=     (f1((y[55]),y[61],ubenthslar_i_carn,(hsbenthslar_carn*(volume_si))));
 Upt_benthclar_i_carn=     (f1((y[56]),y[61],ubenthclar_i_carn,(hsbenthclar_carn*(volume_si))));

// Sum over all prey
      Upt_total_carn_i = Upt_herb_i_carn  + Upt_fishplar_i_carn + Upt_fishdlar_i_carn
                      +Upt_benthslar_i_carn + Upt_benthclar_i_carn  ;


 
// Inshore filter/dep feeding benthos
// _____________


 Upt_detritus_si_benths_i=   (f1((y[51]*(thik_b/thik_si)),y[57],udet_si_benths_i,hsdet_benths*shallowprop));
 Upt_phyt_si_benths_i=       (f1((y[54]*(thik_b/thik_si)),y[57],uphyt_si_benths_i,hsphyt_benths*shallowprop));

if(area_s0>0) {
	 Upt_total_benths_s0       = (area_s0/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
  }
  else {
	 Upt_total_benths_s0       =0;
  }

// Calculate uptake of labile and refractory sediment detritus combined
	 Upt_xTdetritus_s1_benths_i= (f1(y[2]+y[8],(y[57]*area_s1/shallowprop),used_si_benths_i,hssed_benths*area_s1));
	 Upt_xTdetritus_s2_benths_i= (f1(y[3]+y[9],(y[57]*area_s2/shallowprop),used_si_benths_i,hssed_benths*area_s2));
	 Upt_xTdetritus_s3_benths_i= (f1(y[4]+y[10],(y[57]*area_s3/shallowprop),used_si_benths_i,hssed_benths*area_s3));

if(area_s1>0 && rock_s1>0.5 && (y[2]+y[8])>0) {
// Separate out the uptake of just the refractory component
	 Upt_xRdetritus_s1_benths_i= Upt_xTdetritus_s1_benths_i*y[8]/(y[2]+y[8]) ;
// Separate out the uptake of just the labile component
	 Upt_xdetritus_s1_benths_i= Upt_xTdetritus_s1_benths_i - Upt_xRdetritus_s1_benths_i ;
// Here the total assimilatable uptake includes both labile and refractory sediment detritus                                        
//	 Upt_total_benths_s1 = Upt_xTdetritus_s1_benths_i + (area_s1/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
// Here the total digestible uptake includes the labile material and the digestible fraction of refractory detritus
	 Upt_total_benths_s1 = Upt_xdetritus_s1_benths_i + qs_p3*Upt_xRdetritus_s1_benths_i + (area_s1/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
  }
  else {
         Upt_xRdetritus_s1_benths_i=0;
	 Upt_xdetritus_s1_benths_i =0;
	 Upt_total_benths_s1       =(area_s1/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
  }

if(area_s2>0 && rock_s2>0.5 && (y[3]+y[9])>0) {
// Separate out the uptake of just the refractory component
	 Upt_xRdetritus_s2_benths_i= Upt_xTdetritus_s2_benths_i*y[9]/(y[3]+y[9]) ;
// Separate out the uptake of just the labile component
	 Upt_xdetritus_s2_benths_i= Upt_xTdetritus_s2_benths_i - Upt_xRdetritus_s2_benths_i ;
// Here the total assimilatable uptake includes both labile and refractory sediment detritus                                        
//	 Upt_total_benths_s2 = Upt_xTdetritus_s2_benths_i + (area_s2/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
// Here the total digestible uptake includes the labile material and the digestible fraction of refractory detritus
	 Upt_total_benths_s2 = Upt_xdetritus_s2_benths_i + qs_p3*Upt_xRdetritus_s2_benths_i + (area_s2/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
  }
  else {
         Upt_xRdetritus_s2_benths_i=0;
	 Upt_xdetritus_s2_benths_i =0;
	 Upt_total_benths_s2       =(area_s2/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
  }

if(area_s3>0 && rock_s3>0.5 && (y[4]+y[10])>0) {
// Separate out the uptake of just the refractory component
	 Upt_xRdetritus_s3_benths_i= Upt_xTdetritus_s3_benths_i*y[10]/(y[4]+y[10]) ;
// Separate out the uptake of just the labile component
	 Upt_xdetritus_s3_benths_i= Upt_xTdetritus_s3_benths_i - Upt_xRdetritus_s3_benths_i ;
// Here the total assimilatable uptake includes both labile and refractory sediment detritus                                        
//	 Upt_total_benths_s3 = Upt_xTdetritus_s3_benths_i + (area_s3/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
// Here the total digestible uptake includes the labile material and the digestible fraction of refractory detritus
	 Upt_total_benths_s3 = Upt_xdetritus_s3_benths_i + qs_p3*Upt_xRdetritus_s3_benths_i + (area_s3/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
  }
  else {
         Upt_xRdetritus_s3_benths_i=0;
	 Upt_xdetritus_s3_benths_i =0;
	 Upt_total_benths_s3       =(area_s3/(shallowprop))*(Upt_detritus_si_benths_i+Upt_phyt_si_benths_i);
  }
 
	 Upt_total_benths_i=       (Upt_total_benths_s0+Upt_total_benths_s1+Upt_total_benths_s2+Upt_total_benths_s3);



// Offshore filter/dep feeding benthos
// _____________


 Upt_detritus_d_benths_o=   (f1((y[1]*(thik_b/thik_d)),y[42],udet_d_benths_o,hsdet_benths*(1-shallowprop)));
 Upt_phyt_d_benths_o=       (f1((y[38]*(thik_b/thik_d)),y[42],uphyt_d_benths_o,hsphyt_benths*(1-shallowprop)));

if(area_d0>0) {
	 Upt_total_benths_d0       = (area_d0/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
  }
  else {
	 Upt_total_benths_d0       =0;
  }


// Calculate uptake of labile and refractory sediment detritus combined
	 Upt_xTdetritus_d1_benths_o= (f1(y[5]+y[11],(y[42]*area_d1/(1-shallowprop)),used_d_benths_o,hssed_benths*area_d1));
	 Upt_xTdetritus_d2_benths_o= (f1(y[6]+y[12],(y[42]*area_d2/(1-shallowprop)),used_d_benths_o,hssed_benths*area_d2));
	 Upt_xTdetritus_d3_benths_o= (f1(y[7]+y[13],(y[42]*area_d3/(1-shallowprop)),used_d_benths_o,hssed_benths*area_d3));

if(area_d1>0 && rock_d1>0.5 && (y[5]+y[11])>0) {
// Separate out the uptake of just the refractory component
	 Upt_xRdetritus_d1_benths_o= Upt_xTdetritus_d1_benths_o*y[11]/(y[5]+y[11]) ;
// Separate out the uptake of just the labile component
	 Upt_xdetritus_d1_benths_o= Upt_xTdetritus_d1_benths_o - Upt_xRdetritus_d1_benths_o ;
// Here the total assimilatable uptake includes both labile and refractory sediment detritus                                        
//	 Upt_total_benths_d1 = Upt_xTdetritus_d1_benths_o + (area_d1/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
// Here the total digestible uptake includes the labile material and the digestible fraction of refractory detritus
	 Upt_total_benths_d1 = Upt_xdetritus_d1_benths_o + qs_p3*Upt_xRdetritus_d1_benths_o + (area_d1/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
  }
  else {
         Upt_xRdetritus_d1_benths_o=0;
	 Upt_xdetritus_d1_benths_o =0;
	 Upt_total_benths_d1       =(area_d1/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
  }

if(area_d2>0 && rock_d2>0.5 && (y[6]+y[12])>0) {
// Separate out the uptake of just the refractory component
	 Upt_xRdetritus_d2_benths_o= Upt_xTdetritus_d2_benths_o*y[12]/(y[6]+y[12]) ;
// Separate out the uptake of just the labile component
	 Upt_xdetritus_d2_benths_o= Upt_xTdetritus_d2_benths_o - Upt_xRdetritus_d2_benths_o ;
// Here the total assimilatable uptake includes both labile and refractory sediment detritus                                        
//	 Upt_total_benths_d2 = Upt_xTdetritus_d2_benths_o + (area_d2/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
// Here the total digestible uptake includes the labile material and the digestible fraction of refractory detritus
	 Upt_total_benths_d2 = Upt_xdetritus_d2_benths_o + qs_p3*Upt_xRdetritus_d2_benths_o + (area_d2/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
  }
  else {
         Upt_xRdetritus_d2_benths_o=0;
	 Upt_xdetritus_d2_benths_o =0;
	 Upt_total_benths_d2       =(area_d2/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
  }

if(area_d3>0 && rock_d3>0.5 && (y[7]+y[13])>0) {
// Separate out the uptake of just the refractory component
	 Upt_xRdetritus_d3_benths_o= Upt_xTdetritus_d3_benths_o*y[13]/(y[7]+y[13]) ;
// Separate out the uptake of just the labile component
	 Upt_xdetritus_d3_benths_o= Upt_xTdetritus_d3_benths_o - Upt_xRdetritus_d3_benths_o ;
// Here the total assimilatable uptake includes both labile and refractory sediment detritus                                        
//	 Upt_total_benths_d3 = Upt_xTdetritus_d3_benths_o + (area_d3/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
// Here the total digestible uptake includes the labile material and the digestible fraction of refractory detritus
	 Upt_total_benths_d3 = Upt_xdetritus_d3_benths_o + qs_p3*Upt_xRdetritus_d3_benths_o + (area_d3/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
  }
  else {
         Upt_xRdetritus_d3_benths_o=0;
	 Upt_xdetritus_d3_benths_o =0;
	 Upt_total_benths_d3       =(area_d3/(1-shallowprop))*(Upt_detritus_d_benths_o+Upt_phyt_d_benths_o);
  }

	 Upt_total_benths_o=       (Upt_total_benths_d0+Upt_total_benths_d1+Upt_total_benths_d2+Upt_total_benths_d3);



// Inshore carn/scav feeding benthos
// _____________

	 Upt_corpse_s0_benthc_i=    (f1(y[72],(y[58]*area_s0/(shallowprop)),ucorp_i_benthc_i,hscorp_benthc*area_s0));
	 Upt_corpse_s1_benthc_i=    (f1(y[15],(y[58]*area_s1/(shallowprop)),ucorp_i_benthc_i,hscorp_benthc*area_s1));
	 Upt_corpse_s2_benthc_i=    (f1(y[16],(y[58]*area_s2/(shallowprop)),ucorp_i_benthc_i,hscorp_benthc*area_s2));
	 Upt_corpse_s3_benthc_i=    (f1(y[17],(y[58]*area_s3/(shallowprop)),ucorp_i_benthc_i,hscorp_benthc*area_s3));

         Upt_benths_i_benthc_i=     (f1(y[57],y[58],ubenths_i_benthc_i,hsbenths_benthc*shallowprop));

         Upt_kelp_s0_benthc_i =     (f1(y[75],(y[58]*area_s0/(shallowprop)),ukelp_i_benthc_i,hskelp_benthc*area_s0));
         Upt_kelpdebris_s0_benthc_i=(f1(y[76],(y[58]*area_s0/(shallowprop)),ukelpdebris_i_benthc_i,hskelpdebris_benthc*area_s0));

	 Upt_total_benthc_s0=     ((Upt_total_benths_s0/Upt_total_benths_i)*Upt_benths_i_benthc_i)+Upt_corpse_s0_benthc_i+Upt_kelp_s0_benthc_i+Upt_kelpdebris_s0_benthc_i;
	 Upt_total_benthc_s1=     ((Upt_total_benths_s1/Upt_total_benths_i)*Upt_benths_i_benthc_i)+Upt_corpse_s1_benthc_i;
	 Upt_total_benthc_s2=     ((Upt_total_benths_s2/Upt_total_benths_i)*Upt_benths_i_benthc_i)+Upt_corpse_s2_benthc_i;
	 Upt_total_benthc_s3=     ((Upt_total_benths_s3/Upt_total_benths_i)*Upt_benths_i_benthc_i)+Upt_corpse_s3_benthc_i;

 Upt_total_benthc_i=       (Upt_total_benthc_s0+Upt_total_benthc_s1+Upt_total_benthc_s2+Upt_total_benthc_s3 );


// Offshore carn/scav feeding benthos
// _____________

	 Upt_corpse_d0_benthc_o=    (f1(y[73],(y[44]*area_d0/(1-shallowprop)),ucorp_o_benthc_o,hscorp_benthc*area_d0));
	 Upt_corpse_d1_benthc_o=    (f1(y[18],(y[44]*area_d1/(1-shallowprop)),ucorp_o_benthc_o,hscorp_benthc*area_d1));
	 Upt_corpse_d2_benthc_o=    (f1(y[19],(y[44]*area_d2/(1-shallowprop)),ucorp_o_benthc_o,hscorp_benthc*area_d2));
	 Upt_corpse_d3_benthc_o=    (f1(y[20],(y[44]*area_d3/(1-shallowprop)),ucorp_o_benthc_o,hscorp_benthc*area_d3));

         Upt_benths_o_benthc_o=      (f1(y[42],y[44],ubenths_o_benthc_o,hsbenths_benthc*(1-shallowprop)));

	 Upt_total_benthc_d0=     ((Upt_total_benths_d0/Upt_total_benths_o)*Upt_benths_o_benthc_o)+Upt_corpse_d0_benthc_o;
	 Upt_total_benthc_d1=     ((Upt_total_benths_d1/Upt_total_benths_o)*Upt_benths_o_benthc_o)+Upt_corpse_d1_benthc_o;
	 Upt_total_benthc_d2=     ((Upt_total_benths_d2/Upt_total_benths_o)*Upt_benths_o_benthc_o)+Upt_corpse_d2_benthc_o;
	 Upt_total_benthc_d3=     ((Upt_total_benths_d3/Upt_total_benths_o)*Upt_benths_o_benthc_o)+Upt_corpse_d3_benthc_o;

 Upt_total_benthc_o=       (Upt_total_benthc_d0+Upt_total_benthc_d1+Upt_total_benthc_d2+Upt_total_benthc_d3 );



// Offshore Larvae of pelagic fish
// _____________

 Upt_herb_o_fishplar=      (f1(y[39],y[46],uherb_o_fishplar,(hsherb_fishplar*(volume_so+volume_d))));

 Upt_benthslar_o_fishplar= (f1(y[41],y[46],ubenthslar_o_fishplar,(hsbenthslar_fishplar*(volume_so+volume_d))));
 Upt_benthclar_o_fishplar= (f1(y[43],y[46],ubenthclar_o_fishplar,(hsbenthclar_fishplar*(volume_so+volume_d))));

// Sum over all prey
      Upt_total_fishplar_o = Upt_herb_o_fishplar 
                          +Upt_benthslar_o_fishplar + Upt_benthclar_o_fishplar ;


// Inshore Larvae of pelagic fish
// ____________

 Upt_herb_i_fishplar=      (f1(y[60],y[62],uherb_i_fishplar,(hsherb_fishplar*(volume_si))));

 Upt_benthslar_i_fishplar= (f1(y[55],y[62],ubenthslar_i_fishplar,(hsbenthslar_fishplar*(volume_si))));
 Upt_benthclar_i_fishplar= (f1(y[56],y[62],ubenthclar_i_fishplar,(hsbenthclar_fishplar*(volume_si))));

// Sum over all prey
      Upt_total_fishplar_i = Upt_herb_i_fishplar
                          +Upt_benthslar_i_fishplar + Upt_benthclar_i_fishplar  ;


// Offshore Larvae of demersal fish
// _____________


 Upt_herb_o_fishdlar=     (f1(y[39],y[48],uherb_o_fishdlar,(hsherb_fishdlar*(volume_so+volume_d))));

 Upt_benthslar_o_fishdlar= (f1(y[41],y[48],ubenthslar_o_fishdlar,(hsbenthslar_fishdlar*(volume_so+volume_d))));
 Upt_benthclar_o_fishdlar= (f1(y[43],y[48],ubenthclar_o_fishdlar,(hsbenthclar_fishdlar*(volume_so+volume_d))));
 
// Sum over all prey
      Upt_total_fishdlar_o = Upt_herb_o_fishdlar 
                          +Upt_benthslar_o_fishdlar + Upt_benthclar_o_fishdlar ;


// Inshore Larvae of demersal fish
// _____________


 Upt_herb_i_fishdlar=     (f1(y[60],y[63],uherb_i_fishdlar,(hsherb_fishdlar*(volume_si))));

 Upt_benthslar_i_fishdlar= (f1(y[55],y[63],ubenthslar_i_fishdlar,(hsbenthslar_fishdlar*(volume_si))));
 Upt_benthclar_i_fishdlar= (f1(y[56],y[63],ubenthclar_i_fishdlar,(hsbenthclar_fishdlar*(volume_si))));

// Sum over all prey
      Upt_total_fishdlar_i = Upt_herb_i_fishdlar
                          +Upt_benthslar_i_fishdlar + Upt_benthclar_i_fishdlar  ;



// Offshore Pelagic fish
// _____________

 Upt_herb_o_fishp=         (f1(y[39],y[45],uherb_o_fishp,(hsherb_fishp*(volume_so+volume_d))));

 Upt_carn_o_fishp=         (f1(CZ_edible_o,y[45],ucarn_o_fishp,(hscarn_fishp*(volume_so+volume_d))));

 Upt_fishplar_o_fishp=     (f1(y[46],y[45],ufishplar_o_fishp,(hsfishplar_fishp*(volume_so+volume_d))));

 Upt_fishdlar_o_fishp=     (f1(y[48],y[45],ufishdlar_o_fishp,(hsfishdlar_fishp*(volume_so+volume_d))));

 Upt_benthslar_o_fishp=    (f1(y[41],y[45],ubenthslar_o_fishp,(hsbenthslar_fishp*(volume_so+volume_d))));
 Upt_benthclar_o_fishp=    (f1(y[43],y[45],ubenthclar_o_fishp,(hsbenthclar_fishp*(volume_so+volume_d))));

// Sum over all prey
      Upt_total_fishp_o = Upt_herb_o_fishp + Upt_carn_o_fishp + Upt_fishplar_o_fishp + Upt_fishdlar_o_fishp
                          +Upt_benthslar_o_fishp + Upt_benthclar_o_fishp ;


// Inshore Pelagic fish
// _____________

 Upt_herb_i_fishp=         (f1(y[60],y[64],uherb_i_fishp,(hsherb_fishp*(volume_si))));

 Upt_carn_i_fishp=         (f1(CZ_edible_i,y[64],ucarn_i_fishp,(hscarn_fishp*(volume_si))));

 Upt_fishplar_i_fishp=     (f1(y[62],y[64],ufishplar_i_fishp,(hsfishplar_fishp*(volume_si))));

 Upt_fishdlar_i_fishp=     (f1(y[63],y[64],ufishdlar_i_fishp,(hsfishdlar_fishp*(volume_si))));

 Upt_benthslar_i_fishp= (f1(y[55],y[64],ubenthslar_i_fishp,(hsbenthslar_fishp*(volume_si))));
 Upt_benthclar_i_fishp= (f1(y[56],y[64],ubenthclar_i_fishp,(hsbenthclar_fishp*(volume_si))));

// Sum over all prey
      Upt_total_fishp_i = Upt_herb_i_fishp + Upt_carn_i_fishp + Upt_fishplar_i_fishp + Upt_fishdlar_i_fishp
                          +Upt_benthslar_i_fishp + Upt_benthclar_i_fishp  ;



// Offshore Migratory fish
// _____________

 Upt_herb_o_fishm=         (f1(y[39],y[49],uherb_o_fishm,(hsherb_fishm*(volume_so+volume_d))));

 Upt_carn_o_fishm=         (f1(CZ_edible_o,y[49],ucarn_o_fishm,(hscarn_fishm*(volume_so+volume_d))));

 Upt_fishplar_o_fishm=     (f1(y[46],y[49],ufishplar_o_fishm,(hsfishplar_fishm*(volume_so+volume_d))));

 Upt_fishdlar_o_fishm=     (f1(y[48],y[49],ufishdlar_o_fishm,(hsfishdlar_fishm*(volume_so+volume_d))));

 Upt_benthslar_o_fishm=    (f1(y[41],y[49],ubenthslar_o_fishm,(hsbenthslar_fishm*(volume_so+volume_d))));
 Upt_benthclar_o_fishm=    (f1(y[43],y[49],ubenthclar_o_fishm,(hsbenthclar_fishm*(volume_so+volume_d))));

// Sum over all prey
      Upt_total_fishm_o = Upt_herb_o_fishm +  Upt_carn_o_fishm + Upt_fishplar_o_fishm + Upt_fishdlar_o_fishm
                          +Upt_benthslar_o_fishm + Upt_benthclar_o_fishm ;


// Inshore Migratory fish
// _____________

 Upt_herb_i_fishm=         (f1(y[60],y[65],uherb_i_fishm,(hsherb_fishm*(volume_si))));

 Upt_carn_i_fishm=         (f1(CZ_edible_i,y[65],ucarn_i_fishm,(hscarn_fishm*(volume_si))));

 Upt_fishplar_i_fishm=     (f1(y[62],y[65],ufishplar_i_fishm,(hsfishplar_fishm*(volume_si))));

 Upt_fishdlar_i_fishm=     (f1(y[63],y[65],ufishdlar_i_fishm,(hsfishdlar_fishm*(volume_si))));

 Upt_benthslar_i_fishm= (f1(y[55],y[65],ubenthslar_i_fishm,(hsbenthslar_fishm*(volume_si))));
 Upt_benthclar_i_fishm= (f1(y[56],y[65],ubenthclar_i_fishm,(hsbenthclar_fishm*(volume_si))));

// Sum over all prey
      Upt_total_fishm_i = Upt_herb_i_fishm +  Upt_carn_i_fishm + Upt_fishplar_i_fishm + Upt_fishdlar_i_fishm
                          +Upt_benthslar_i_fishm + Upt_benthclar_i_fishm  ;



// Offshore Demersal fish
// _____________

	 Upt_corpse_d0_fishd=     (f1(y[73],(y[47]*area_d0/(1-shallowprop)),ucorp_o_fishd,(hscorp_fishd*(volume_so+volume_d)*(area_d0/(1-shallowprop)))));
	 Upt_corpse_d1_fishd=     (f1(y[18],(y[47]*area_d1/(1-shallowprop)),ucorp_o_fishd,(hscorp_fishd*(volume_so+volume_d)*(area_d1/(1-shallowprop)))));
	 Upt_corpse_d2_fishd=     (f1(y[19],(y[47]*area_d2/(1-shallowprop)),ucorp_o_fishd,(hscorp_fishd*(volume_so+volume_d)*(area_d2/(1-shallowprop)))));
	 Upt_corpse_d3_fishd=     (f1(y[20],(y[47]*area_d3/(1-shallowprop)),ucorp_o_fishd,(hscorp_fishd*(volume_so+volume_d)*(area_d3/(1-shallowprop)))));

// sum up over all sediment types
	 Upt_corpse_o_fishd=       (Upt_corpse_d0_fishd+Upt_corpse_d1_fishd+Upt_corpse_d2_fishd+Upt_corpse_d3_fishd );

 Upt_disc_o_fishd=         (f1(y[14],y[47],udisc_o_fishd,hsdisc_fishd*(volume_so+volume_d)));

 Upt_benths_o_fishd=       (f1(y[42],y[47],ubenths_o_fishd,(hsbenths_fishd*(volume_so+volume_d))));

 Upt_benthc_o_fishd=       (f1(y[44],y[47],ubenthc_o_fishd,(hsbenthc_fishd*(volume_so+volume_d))));

 Upt_carn_o_fishd=         (f1(CZ_edible_o,y[47],ucarn_o_fishd,(hscarn_fishd*(volume_so+volume_d))));

 Upt_fishplar_o_fishd=     (f1(y[46],y[47],ufishplar_o_fishd,(hsfishplar_fishd*(volume_so+volume_d))));

 Upt_fishdlar_o_fishd=     (f1(y[48],y[47],ufishdlar_o_fishd,(hsfishdlar_fishd*(volume_so+volume_d))));

 Upt_fishp_o_fishd=        (f1(y[45],y[47],ufishp_o_fishd,(hsfishp_fishd*(volume_so+volume_d))));

 Upt_fishm_o_fishd=        (f1(y[49],y[47],ufishm_o_fishd,(hsfishm_fishd*(volume_so+volume_d))));

 Upt_fishd_o_fishd=        (f1(y[47],y[47],ufishd_o_fishd,(hsfishd_fishd*(volume_so+volume_d))));

// Sum over all prey
      Upt_total_fishd_o = Upt_corpse_o_fishd + Upt_carn_o_fishd + Upt_disc_o_fishd + Upt_fishplar_o_fishd + Upt_fishdlar_o_fishd
                          +Upt_fishp_o_fishd + Upt_fishm_o_fishd + Upt_fishd_o_fishd
                          +Upt_benths_o_fishd + Upt_benthc_o_fishd ;


// Inshore Demersal fish
// _____________


	 Upt_corpse_s0_fishd=     (f1(y[72],(y[66]*area_s0/shallowprop),ucorp_i_fishd,(hscorp_fishd*(volume_si*(area_s0/shallowprop)))));
	 Upt_corpse_s1_fishd=     (f1(y[15],(y[66]*area_s1/shallowprop),ucorp_i_fishd,(hscorp_fishd*(volume_si*(area_s1/shallowprop)))));
	 Upt_corpse_s2_fishd=     (f1(y[16],(y[66]*area_s2/shallowprop),ucorp_i_fishd,(hscorp_fishd*(volume_si*(area_s2/shallowprop)))));
	 Upt_corpse_s3_fishd=     (f1(y[17],(y[66]*area_s3/shallowprop),ucorp_i_fishd,(hscorp_fishd*(volume_si*(area_s3/shallowprop)))));

// sum up over all sediment types
	 Upt_corpse_i_fishd=       (Upt_corpse_s0_fishd+Upt_corpse_s1_fishd+Upt_corpse_s2_fishd+Upt_corpse_s3_fishd );

 Upt_disc_i_fishd=         (f1(y[59],y[66],udisc_i_fishd,hsdisc_fishd*(volume_si)));

 Upt_benths_i_fishd=       (f1(y[57],y[66],ubenths_i_fishd,(hsbenths_fishd*(volume_si))));

 Upt_benthc_i_fishd=       (f1(y[58],y[66],ubenthc_i_fishd,(hsbenthc_fishd*(volume_si))));

 Upt_carn_i_fishd=         (f1(CZ_edible_i,y[66],ucarn_i_fishd,(hscarn_fishd*(volume_si))));

 Upt_fishplar_i_fishd=     (f1(y[62],y[66],ufishplar_i_fishd,(hsfishplar_fishd*(volume_si))));

 Upt_fishdlar_i_fishd=     (f1(y[63],y[66],ufishdlar_i_fishd,(hsfishdlar_fishd*(volume_si))));

 Upt_fishp_i_fishd=        (f1(y[64],y[66],ufishp_i_fishd,(hsfishp_fishd*(volume_si))));

 Upt_fishm_i_fishd=        (f1(y[65],y[66],ufishm_i_fishd,(hsfishm_fishd*(volume_si))));

 Upt_fishd_i_fishd=        (f1(y[66],y[66],ufishd_i_fishd,(hsfishd_fishd*(volume_si))));


// Sum over all prey - PREVIOUS VERSION (10) O FTHE CODE HAD AN ERROR HERE - WAS OVERWRITING Upt_total_fishd_O. Upt_total_fishd_i WAS NOT BEING CALCULATED AT ALL
      Upt_total_fishd_i = Upt_corpse_i_fishd + Upt_carn_i_fishd + Upt_disc_i_fishd + Upt_fishplar_i_fishd + Upt_fishdlar_i_fishd
                          +Upt_fishp_i_fishd + Upt_fishm_i_fishd + Upt_fishd_i_fishd
                          +Upt_benths_i_fishd + Upt_benthc_i_fishd ;





// Offshore Birds
// _____________


	 Upt_corpse_d0_bird=      (f3(y[73],(y[50]*area_d0/(1-shallowprop)),ucorp_o_bird,(hscorp_bird*(volume_so+volume_d)*area_d0/(1-shallowprop)),bdapar_bird));
	 Upt_corpse_d1_bird=      (f3(y[18],(y[50]*area_d1/(1-shallowprop)),ucorp_o_bird,(hscorp_bird*(volume_so+volume_d)*area_d1/(1-shallowprop)),bdapar_bird));
	 Upt_corpse_d2_bird=      (f3(y[19],(y[50]*area_d2/(1-shallowprop)),ucorp_o_bird,(hscorp_bird*(volume_so+volume_d)*area_d2/(1-shallowprop)),bdapar_bird));
	 Upt_corpse_d3_bird=      (f3(y[20],(y[50]*area_d3/(1-shallowprop)),ucorp_o_bird,(hscorp_bird*(volume_so+volume_d)*area_d3/(1-shallowprop)),bdapar_bird));

// sum up over all sediment types
	 Upt_corpse_o_bird=        (Upt_corpse_d0_bird+Upt_corpse_d1_bird+Upt_corpse_d2_bird+Upt_corpse_d3_bird );

 Upt_benths_o_bird=        (f3(y[42],(y[50]),ubenths_o_bird,(hsbenths_bird*(volume_so+volume_d)),bdapar_bird));
 Upt_benthc_o_bird=        (f3(y[44],(y[50]),ubenthc_o_bird,(hsbenthc_bird*(volume_so+volume_d)),bdapar_bird));
 Upt_disc_o_bird=          (f3(y[14],(y[50]),udisc_o_bird,(hsdisc_bird*(volume_so+volume_d)),bdapar_bird));

// Upt_herb_o_bird=          (f3(y[39],(y[50]),uherb_o_bird,(hsherb_bird*(volume_so+volume_d)),bdapar_bird));

 Upt_carn_o_bird=          (f3(CZ_edible_o,(y[50]),ucarn_o_bird,(hscarn_bird*(volume_so+volume_d)),bdapar_bird));

 Upt_fishp_o_bird=         (f3(y[45],(y[50]),ufishp_o_bird,(hsfishp_bird*(volume_so+volume_d)),bdapar_bird));

 Upt_fishd_o_bird=         (f3(y[47],(y[50]),ufishd_o_bird,(hsfishd_bird*(volume_so+volume_d)),bdapar_bird));

 Upt_fishm_o_bird=         (f3(y[49],(y[50]),ufishm_o_bird,(hsfishm_bird*(volume_so+volume_d)),bdapar_bird));

// Sum over all prey
      Upt_total_bird_o = Upt_carn_o_bird + Upt_fishp_o_bird + Upt_fishm_o_bird + Upt_fishd_o_bird
                      +Upt_disc_o_bird +Upt_corpse_o_bird 
                      +Upt_benths_o_bird + Upt_benthc_o_bird;


// Inshore Birds
// _____________

	 Upt_corpse_s0_bird=      (f3(y[72],(y[67]*area_s0/shallowprop),ucorp_i_bird,(hscorp_bird*(volume_si*area_s0/shallowprop)),bdapar_bird));
	 Upt_corpse_s1_bird=      (f3(y[15],(y[67]*area_s1/shallowprop),ucorp_i_bird,(hscorp_bird*(volume_si*area_s1/shallowprop)),bdapar_bird));
	 Upt_corpse_s2_bird=      (f3(y[16],(y[67]*area_s2/shallowprop),ucorp_i_bird,(hscorp_bird*(volume_si*area_s2/shallowprop)),bdapar_bird));
	 Upt_corpse_s3_bird=      (f3(y[17],(y[67]*area_s3/shallowprop),ucorp_i_bird,(hscorp_bird*(volume_si*area_s3/shallowprop)),bdapar_bird));

// sum up over all sediment types
	 Upt_corpse_i_bird=        (Upt_corpse_s0_bird+Upt_corpse_s1_bird+Upt_corpse_s2_bird+Upt_corpse_s3_bird );

 Upt_benths_i_bird=        (f3(y[57],(y[67]),ubenths_i_bird,(hsbenths_bird*(volume_si)),bdapar_bird));
 Upt_benthc_i_bird=        (f3(y[58],(y[67]),ubenthc_i_bird,(hsbenthc_bird*(volume_si)),bdapar_bird));
 Upt_disc_i_bird=          (f3(y[59],(y[67]),udisc_i_bird,(hsdisc_bird*(volume_si)),bdapar_bird));

//  Upt_herb_i_bird=          (f3(y[60],(y[67]),uherb_i_bird,(hsherb_bird*(volume_si)),bdapar_bird));

 Upt_carn_i_bird=          (f3(CZ_edible_i,(y[67]),ucarn_i_bird,(hscarn_bird*(volume_si)),bdapar_bird));

 Upt_fishp_i_bird=         (f3(y[64],(y[67]),ufishp_i_bird,(hsfishp_bird*(volume_si)),bdapar_bird));

 Upt_fishd_i_bird=         (f3(y[66],(y[67]),ufishd_i_bird,(hsfishd_bird*(volume_si)),bdapar_bird));

 Upt_fishm_i_bird=         (f3(y[65],(y[67]),ufishm_i_bird,(hsfishm_bird*(volume_si)),bdapar_bird));

// Sum over all prey
      Upt_total_bird_i = Upt_carn_i_bird + Upt_fishp_i_bird + Upt_fishm_i_bird + Upt_fishd_i_bird
                      +Upt_corpse_i_bird + + Upt_disc_i_bird 
                      +Upt_benths_i_bird + Upt_benthc_i_bird ;



// Offshore seals
// _____________


	 Upt_corpse_d0_seal=      (f3(y[73],(y[68]*area_d0/(1-shallowprop)),ucorp_o_seal,(hscorp_seal*(volume_so+volume_d)*area_d0/(1-shallowprop)),bdapar_seal));
	 Upt_corpse_d1_seal=      (f3(y[18],(y[68]*area_d1/(1-shallowprop)),ucorp_o_seal,(hscorp_seal*(volume_so+volume_d)*area_d1/(1-shallowprop)),bdapar_seal));
	 Upt_corpse_d2_seal=      (f3(y[19],(y[68]*area_d2/(1-shallowprop)),ucorp_o_seal,(hscorp_seal*(volume_so+volume_d)*area_d2/(1-shallowprop)),bdapar_seal));
	 Upt_corpse_d3_seal=      (f3(y[20],(y[68]*area_d3/(1-shallowprop)),ucorp_o_seal,(hscorp_seal*(volume_so+volume_d)*area_d3/(1-shallowprop)),bdapar_seal));

// sum up over all sediment types
	 Upt_corpse_o_seal=        (Upt_corpse_d0_seal+Upt_corpse_d1_seal+Upt_corpse_d2_seal+Upt_corpse_d3_seal );

 Upt_benths_o_seal=        (f3(y[42],(y[68]),ubenths_o_seal,(hsbenths_seal*(volume_so+volume_d)),bdapar_seal));
 Upt_benthc_o_seal=        (f3(y[44],(y[68]),ubenthc_o_seal,(hsbenthc_seal*(volume_so+volume_d)),bdapar_seal));
 Upt_disc_o_seal=          (f3(y[14],(y[68]),udisc_o_seal,(hsdisc_seal*(volume_so+volume_d)),bdapar_seal));

// Upt_herb_o_seal=          (f3(y[39],(y[68]),uherb_o_seal,(hsherb_seal*(volume_so+volume_d)),bdapar_seal));

 Upt_carn_o_seal=          (f3(CZ_edible_o,(y[68]),ucarn_o_seal,(hscarn_seal*(volume_so+volume_d)),bdapar_seal));

 Upt_fishp_o_seal=         (f3(y[45],(y[68]),ufishp_o_seal,(hsfishp_seal*(volume_so+volume_d)),bdapar_seal));

 Upt_fishd_o_seal=         (f3(y[47],(y[68]),ufishd_o_seal,(hsfishd_seal*(volume_so+volume_d)),bdapar_seal));

 Upt_fishm_o_seal=         (f3(y[49],(y[68]),ufishm_o_seal,(hsfishm_seal*(volume_so+volume_d)),bdapar_seal));

 Upt_bird_o_seal=         (f3(y[50],(y[68]),ubird_o_seal,(hsbird_seal*(volume_so+volume_d)),bdapar_seal));

// Sum over all prey
      Upt_total_seal_o = Upt_carn_o_seal + Upt_fishp_o_seal + Upt_fishm_o_seal + Upt_fishd_o_seal
                      +Upt_disc_o_seal +Upt_corpse_o_seal + Upt_bird_o_seal
                      +Upt_benths_o_seal + Upt_benthc_o_seal;




// Inshore Seals
// _____________

	 Upt_corpse_s0_seal=      (f3(y[72],(y[69]*area_s0/shallowprop),ucorp_i_seal,(hscorp_seal*(volume_si*area_s0/shallowprop)),bdapar_seal));
	 Upt_corpse_s1_seal=      (f3(y[15],(y[69]*area_s1/shallowprop),ucorp_i_seal,(hscorp_seal*(volume_si*area_s1/shallowprop)),bdapar_seal));
	 Upt_corpse_s2_seal=      (f3(y[16],(y[69]*area_s2/shallowprop),ucorp_i_seal,(hscorp_seal*(volume_si*area_s2/shallowprop)),bdapar_seal));
	 Upt_corpse_s3_seal=      (f3(y[17],(y[69]*area_s3/shallowprop),ucorp_i_seal,(hscorp_seal*(volume_si*area_s3/shallowprop)),bdapar_seal));

// sum up over all sediment types
	 Upt_corpse_i_seal=        (Upt_corpse_s0_seal+Upt_corpse_s1_seal+Upt_corpse_s2_seal+Upt_corpse_s3_seal );

 Upt_benths_i_seal=        (f3(y[57],(y[69]),ubenths_i_seal,(hsbenths_seal*(volume_si)),bdapar_seal));
 Upt_benthc_i_seal=        (f3(y[58],(y[69]),ubenthc_i_seal,(hsbenthc_seal*(volume_si)),bdapar_seal));
 Upt_disc_i_seal=          (f3(y[59],(y[69]),udisc_i_seal,(hsdisc_seal*(volume_si)),bdapar_seal));

// Upt_herb_i_seal=          (f3(y[60],(y[69]),uherb_i_seal,(hsherb_seal*(volume_si)),bdapar_seal));

 Upt_carn_i_seal=          (f3(CZ_edible_i,(y[69]),ucarn_i_seal,(hscarn_seal*(volume_si)),bdapar_seal));

 Upt_fishp_i_seal=         (f3(y[64],(y[69]),ufishp_i_seal,(hsfishp_seal*(volume_si)),bdapar_seal));

 Upt_fishd_i_seal=         (f3(y[66],(y[69]),ufishd_i_seal,(hsfishd_seal*(volume_si)),bdapar_seal));

 Upt_fishm_i_seal=         (f3(y[65],(y[69]),ufishm_i_seal,(hsfishm_seal*(volume_si)),bdapar_seal));

 Upt_bird_i_seal=         (f3(y[67],(y[69]),ubird_i_seal,(hsbird_seal*(volume_si)),bdapar_seal));

// Sum over all prey
      Upt_total_seal_i = Upt_carn_i_seal + Upt_fishp_i_seal + Upt_fishm_i_seal + Upt_fishd_i_seal
                      +Upt_corpse_i_seal + + Upt_disc_i_seal + Upt_bird_i_seal
                      +Upt_benths_i_seal + Upt_benthc_i_seal ;



// Offshore Cetaceans
// _____________


//	 Upt_corpse_d1_ceta=      (f3(y[18],(y[70]*area_d1/(1-shallowprop)),ucorp_o_ceta,(hscorp_ceta*(volume_so+volume_d)*area_d1/(1-shallowprop)),bdapar_ceta));
//	 Upt_corpse_d2_ceta=      (f3(y[19],(y[70]*area_d2/(1-shallowprop)),ucorp_o_ceta,(hscorp_ceta*(volume_so+volume_d)*area_d2/(1-shallowprop)),bdapar_ceta));
//	 Upt_corpse_d3_ceta=      (f3(y[20],(y[70]*area_d3/(1-shallowprop)),ucorp_o_ceta,(hscorp_ceta*(volume_so+volume_d)*area_d3/(1-shallowprop)),bdapar_ceta));

// sum up over all sediment types
//	 Upt_corpse_o_ceta=        (Upt_corpse_d1_ceta+Upt_corpse_d2_ceta+Upt_corpse_d3_ceta );

 Upt_benths_o_ceta=        (f3(y[42],(y[70]),ubenths_o_ceta,(hsbenths_ceta*(volume_so+volume_d)),bdapar_ceta));
 Upt_benthc_o_ceta=        (f3(y[44],(y[70]),ubenthc_o_ceta,(hsbenthc_ceta*(volume_so+volume_d)),bdapar_ceta));
 Upt_disc_o_ceta=          (f3(y[14],(y[70]),udisc_o_ceta,(hsdisc_ceta*(volume_so+volume_d)),bdapar_ceta));

 Upt_herb_o_ceta=          (f3(y[39],(y[70]),uherb_o_ceta,(hsherb_ceta*(volume_so+volume_d)),bdapar_ceta));

 Upt_carn_o_ceta=          (f3(CZ_edible_o,(y[70]),ucarn_o_ceta,(hscarn_ceta*(volume_so+volume_d)),bdapar_ceta));

 Upt_fishp_o_ceta=         (f3(y[45],(y[70]),ufishp_o_ceta,(hsfishp_ceta*(volume_so+volume_d)),bdapar_ceta));

 Upt_fishd_o_ceta=         (f3(y[47],(y[70]),ufishd_o_ceta,(hsfishd_ceta*(volume_so+volume_d)),bdapar_ceta));

 Upt_fishm_o_ceta=         (f3(y[49],(y[70]),ufishm_o_ceta,(hsfishm_ceta*(volume_so+volume_d)),bdapar_ceta));

 Upt_bird_o_ceta=         (f3(y[50],(y[70]),ubird_o_ceta,(hsbird_ceta*(volume_so+volume_d)),bdapar_ceta));

 Upt_seal_o_ceta=         (f3(y[68],(y[70]),useal_o_ceta,(hsseal_ceta*(volume_so+volume_d)),bdapar_ceta));

// Sum over all prey
      Upt_total_ceta_o = Upt_herb_o_ceta  
                      +Upt_carn_o_ceta + Upt_fishp_o_ceta + Upt_fishm_o_ceta + Upt_fishd_o_ceta
                      +Upt_disc_o_ceta + Upt_bird_o_ceta  + Upt_seal_o_ceta
                      +Upt_benths_o_ceta + Upt_benthc_o_ceta;


// Inshore Cetaceans
// _____________

//	 Upt_corpse_s1_ceta=      (f3(y[15],(y[71]*area_s1/shallowprop),ucorp_i_ceta,(hscorp_ceta*(volume_si*area_s1/shallowprop)),bdapar_ceta));
//	 Upt_corpse_s2_ceta=      (f3(y[16],(y[71]*area_s2/shallowprop),ucorp_i_ceta,(hscorp_ceta*(volume_si*area_s2/shallowprop)),bdapar_ceta));
//	 Upt_corpse_s3_ceta=      (f3(y[17],(y[71]*area_s3/shallowprop),ucorp_i_ceta,(hscorp_ceta*(volume_si*area_s3/shallowprop)),bdapar_ceta));

// sum up over all sediment types
//	 Upt_corpse_i_ceta=        (Upt_corpse_s1_ceta+Upt_corpse_s2_ceta+Upt_corpse_s3_ceta );

 Upt_benths_i_ceta=        (f3(y[57],(y[71]),ubenths_i_ceta,(hsbenths_ceta*(volume_si)),bdapar_ceta));
 Upt_benthc_i_ceta=        (f3(y[58],(y[71]),ubenthc_i_ceta,(hsbenthc_ceta*(volume_si)),bdapar_ceta));
 Upt_disc_i_ceta=          (f3(y[59],(y[71]),udisc_i_ceta,(hsdisc_ceta*(volume_si)),bdapar_ceta));

 Upt_herb_i_ceta=          (f3(y[60],(y[71]),uherb_i_ceta,(hsherb_ceta*(volume_si)),bdapar_ceta));

 Upt_carn_i_ceta=          (f3(CZ_edible_i,(y[71]),ucarn_i_ceta,(hscarn_ceta*(volume_si)),bdapar_ceta));

 Upt_fishp_i_ceta=         (f3(y[64],(y[71]),ufishp_i_ceta,(hsfishp_ceta*(volume_si)),bdapar_ceta));

 Upt_fishd_i_ceta=         (f3(y[66],(y[71]),ufishd_i_ceta,(hsfishd_ceta*(volume_si)),bdapar_ceta));

 Upt_fishm_i_ceta=         (f3(y[65],(y[71]),ufishm_i_ceta,(hsfishm_ceta*(volume_si)),bdapar_ceta));

 Upt_bird_i_ceta=         (f3(y[67],(y[71]),ubird_i_ceta,(hsbird_ceta*(volume_si)),bdapar_ceta));

 Upt_seal_i_ceta=         (f3(y[69],(y[71]),useal_i_ceta,(hsseal_ceta*(volume_si)),bdapar_ceta));

// Sum over all prey
      Upt_total_ceta_i = Upt_herb_i_ceta 
                      +Upt_carn_i_ceta + Upt_fishp_i_ceta + Upt_fishm_i_ceta + Upt_fishd_i_ceta
                      +Upt_disc_i_ceta  + Upt_bird_i_ceta + Upt_seal_i_ceta
                      +Upt_benths_i_ceta + Upt_benthc_i_ceta ;





/* _____ Carbohydrate excretion by kelp_______ */

Exude_kelp_i = exC_kelp_i * y[74] * y[74];



/* _____Ammonia Excretion by each animal consumer group_____ */


 Excr_herb_so=      (1-aH)*0.5*(Upt_total_herb_o)*prop_herb_surfo;
 Excr_herb_d=       (1-aH)*0.5*(Upt_total_herb_o)*prop_herb_deep;
 Excr_herb_si=      (1-aH)*0.5*(Upt_total_herb_i);

 Excr_carn_so=     (1-aC)*0.5*(Upt_total_carn_o)*prop_carn_surfo;
 Excr_carn_d=      (1-aC)*0.5*(Upt_total_carn_o)*prop_carn_deep;
 Excr_carn_si=     (1-aC)*0.5*(Upt_total_carn_i);

 Excr_benthslar_so=      (1-aBslar)*0.5*(Upt_total_benthslar_o)*prop_benthslar_surfo;
 Excr_benthslar_d=       (1-aBslar)*0.5*(Upt_total_benthslar_o)*prop_benthslar_deep;
 Excr_benthslar_si=      (1-aBslar)*0.5*(Upt_total_benthslar_i);

 Excr_benthclar_so=      (1-aBclar)*0.5*(Upt_total_benthclar_o)*prop_benthclar_surfo;
 Excr_benthclar_d=       (1-aBclar)*0.5*(Upt_total_benthclar_o)*prop_benthclar_deep;
 Excr_benthclar_si=      (1-aBclar)*0.5*(Upt_total_benthclar_i);


 Excr_benths_i=    ((1-aBs)*0.5*(Upt_total_benths_i));
          Excr_benths_s0 = ((1-aBs)*0.5*(Upt_total_benths_s0));
          Excr_benths_s1 = ((1-aBs)*0.5*(Upt_total_benths_s1));
          Excr_benths_s2 = ((1-aBs)*0.5*(Upt_total_benths_s2));
          Excr_benths_s3 = ((1-aBs)*0.5*(Upt_total_benths_s3));

 Excr_benthc_i=    ((1-aBc)*0.5*(Upt_total_benthc_i));
          Excr_benthc_s0 = ((1-aBs)*0.5*(Upt_total_benthc_s0));
          Excr_benthc_s1 = ((1-aBs)*0.5*(Upt_total_benthc_s1));
          Excr_benthc_s2 = ((1-aBs)*0.5*(Upt_total_benthc_s2));
          Excr_benthc_s3 = ((1-aBs)*0.5*(Upt_total_benthc_s3));

 Excr_benths_o=    ((1-aBs)*0.5*(Upt_total_benths_o));
          Excr_benths_d0 = ((1-aBs)*0.5*(Upt_total_benths_d0));
          Excr_benths_d1 = ((1-aBs)*0.5*(Upt_total_benths_d1));
          Excr_benths_d2 = ((1-aBs)*0.5*(Upt_total_benths_d2));
          Excr_benths_d3 = ((1-aBs)*0.5*(Upt_total_benths_d3));

 Excr_benthc_o=    ((1-aBc)*0.5*(Upt_total_benthc_o));
          Excr_benthc_d0 = ((1-aBs)*0.5*(Upt_total_benthc_d0));
          Excr_benthc_d1 = ((1-aBs)*0.5*(Upt_total_benthc_d1));
          Excr_benthc_d2 = ((1-aBs)*0.5*(Upt_total_benthc_d2));
          Excr_benthc_d3 = ((1-aBs)*0.5*(Upt_total_benthc_d3));


 Excr_fishplar_so=     (1-aFplar)*0.5*(Upt_total_fishplar_o)*prop_fishplar_surfo;
 Excr_fishplar_d=      (1-aFplar)*0.5*(Upt_total_fishplar_o)*prop_fishplar_deep;
 Excr_fishplar_si=     (1-aFplar)*0.5*(Upt_total_fishplar_i);

 Excr_fishdlar_so=     (1-aFdlar)*0.5*(Upt_total_fishdlar_o)*prop_fishdlar_surfo;
 Excr_fishdlar_d=      (1-aFdlar)*0.5*(Upt_total_fishdlar_o)*prop_fishdlar_deep;
 Excr_fishdlar_si=     (1-aFdlar)*0.5*(Upt_total_fishdlar_i);

 Excr_fishp_so=     (1-aFp)*0.5*(Upt_total_fishp_o)*volume_so/(volume_d+volume_so);
 Excr_fishp_d=      (1-aFp)*0.5*(Upt_total_fishp_o)*volume_d/(volume_d+volume_so);
 Excr_fishp_si=     (1-aFp)*0.5*(Upt_total_fishp_i);


 Excr_fishm_so=     (1-aFm)*0.5*(Upt_total_fishm_o)*volume_so/(volume_d+volume_so);
 Excr_fishm_d=      (1-aFm)*0.5*(Upt_total_fishm_o)*volume_d/(volume_d+volume_so);
 Excr_fishm_si=     (1-aFm)*0.5*(Upt_total_fishm_i);

 Excr_fishd_so=  0;
 Excr_fishd_d=  (1-aFd)*0.5*Upt_total_fishd_o;
 Excr_fishd_si= (1-aFd)*0.5*Upt_total_fishd_i;

 Excr_bird_so=  (1-abird)*0.5*Upt_total_bird_o;
 Excr_bird_d=   0;
 Excr_bird_si= (1-abird)*0.5*Upt_total_bird_i;



// NEW <----------------------------
 Excr_seal_so=  (1-aseal)*0.5*Upt_total_seal_o;
 Excr_seal_d=   0;
 Excr_seal_si= (1-aseal)*0.5*Upt_total_seal_i;


// NEW <----------------------------
 Excr_ceta_so=  (1-aceta)*0.5*Upt_total_ceta_o;
 Excr_ceta_d=   0;
 Excr_ceta_si= (1-aceta)*0.5*Upt_total_ceta_i;


/* _____Defecation rate of each animal consumer group_____ */

Defec_herb_so = Excr_herb_so ;
Defec_herb_si = Excr_herb_si ;
Defec_herb_d  = Excr_herb_d ;

Defec_carn_so = Excr_carn_so ;
Defec_carn_si = Excr_carn_si ;
Defec_carn_d  = Excr_carn_d ;

Defec_fishplar_so = Excr_fishplar_so ;
Defec_fishplar_si = Excr_fishplar_si ;
Defec_fishplar_d  = Excr_fishplar_d ;

Defec_fishdlar_so = Excr_fishdlar_so ;
Defec_fishdlar_si = Excr_fishdlar_si ;
Defec_fishdlar_d  = Excr_fishdlar_d ;

Defec_benthslar_so = Excr_benthslar_so ;
Defec_benthslar_si = Excr_benthslar_si ;
Defec_benthslar_d  = Excr_benthslar_d ;

Defec_benthclar_so = Excr_benthclar_so ;
Defec_benthclar_si = Excr_benthclar_si ;
Defec_benthclar_d  = Excr_benthclar_d ;

// Here it is assumed that the ingested but indigestible refractory material is voided back as refractory and not altered - so the boundary source terms is only the digestible fraction of refractory material //
    Defec_benths_s0 = Excr_benths_s0 ; 
    Defec_benths_s1 = Excr_benths_s1 ; 
    Defec_benths_s2 = Excr_benths_s2 ; 
    Defec_benths_s3 = Excr_benths_s3 ; 


    Defec_benths_d0 = Excr_benths_d0 ; 
    Defec_benths_d1 = Excr_benths_d1 ; 
    Defec_benths_d2 = Excr_benths_d2 ; 
    Defec_benths_d3 = Excr_benths_d3 ; 

// Here it is assumed that all of the ingested but indigestible refractory uptake is voided as labile material - so it is reactivated (priming) and this becomes a souce into the food web //
//Defec_benths_i = Excr_benths_i + (1-qs_p3)*(Upt_xRdetritus_s1_benths_i + Upt_xRdetritus_s2_benths_i + Upt_xRdetritus_s3_benths_i) ;
//    Defec_benths_s1 = Excr_benths_s1 + (1-qs_p3)*Upt_xRdetritus_s1_benths_i ;
//    Defec_benths_s2 = Excr_benths_s2 + (1-qs_p3)*Upt_xRdetritus_s2_benths_i ;
//    Defec_benths_s3 = Excr_benths_s3 + (1-qs_p3)*Upt_xRdetritus_s3_benths_i ;
//Defec_benths_o = Excr_benths_o + (1-qs_p3)*(Upt_xRdetritus_d1_benths_o + Upt_xRdetritus_d2_benths_o + Upt_xRdetritus_d3_benths_o) ;
//    Defec_benths_d1 = Excr_benths_d1 + (1-qs_p3)*Upt_xRdetritus_d1_benths_o ;
//    Defec_benths_d2 = Excr_benths_d2 + (1-qs_p3)*Upt_xRdetritus_d2_benths_o ;
//    Defec_benths_d3 = Excr_benths_d3 + (1-qs_p3)*Upt_xRdetritus_d3_benths_o ;


    Defec_benthc_s0 = Excr_benthc_s0 ;
    Defec_benthc_s1 = Excr_benthc_s1 ;
    Defec_benthc_s2 = Excr_benthc_s2 ;
    Defec_benthc_s3 = Excr_benthc_s3 ;

    Defec_benthc_d0 = Excr_benthc_d0 ;
    Defec_benthc_d1 = Excr_benthc_d1 ;
    Defec_benthc_d2 = Excr_benthc_d2 ;
    Defec_benthc_d3 = Excr_benthc_d3 ;

Defec_fishp_so = Excr_fishp_so ;
Defec_fishp_si = Excr_fishp_si ;
Defec_fishp_d  = Excr_fishp_d ;

Defec_fishm_so = Excr_fishm_so ;
Defec_fishm_si = Excr_fishm_si ;
Defec_fishm_d  = Excr_fishm_d ;

Defec_fishd_so = Excr_fishd_so ;
Defec_fishd_si = Excr_fishd_si ;
Defec_fishd_d  = Excr_fishd_d ;

Defec_bird_so = Excr_bird_so ;
Defec_bird_si = Excr_bird_si ;
Defec_bird_d  = Excr_bird_d ;


Defec_seal_so = Excr_seal_so ;
Defec_seal_si = Excr_seal_si ;
Defec_seal_d  = Excr_seal_d ;

Defec_ceta_so = Excr_ceta_so ;
Defec_ceta_si = Excr_ceta_si ;
Defec_ceta_d  = Excr_ceta_d ;





/* _____Assimilation of each animal consumer group_____ */
 Assim_herb_o= aH * Upt_total_herb_o ;
 Assim_herb_i= aH * Upt_total_herb_i ;
// Mass balance check - should  = 0
// Rprintf("herb_o=%f\n", Upt_total_herb_o - Assim_herb_o - Defec_herb_so - Defec_herb_d - Excr_herb_so - Excr_herb_d);
// Rprintf("herb_i=%f\n", Upt_total_herb_i - Assim_herb_i - Defec_herb_si - Excr_herb_si);

 Assim_carn_o= aC * Upt_total_carn_o ;
 Assim_carn_i= aC * Upt_total_carn_i ;
// Mass balance check - should  = 0
// Rprintf("carn_o=%f\n", Upt_total_carn_o - Assim_carn_o - Defec_carn_so - Defec_carn_d - Excr_carn_so - Excr_carn_d);
// Rprintf("carn_i=%f\n", Upt_total_carn_i - Assim_carn_i - Defec_carn_si - Excr_carn_si);

 Assim_benthslar_o= aBslar*Upt_total_benthslar_o;
 Assim_benthslar_i= aBslar*Upt_total_benthslar_i;
// Mass balance check - should  = 0
// Rprintf("benthslar_o=%f\n", Upt_total_benthslar_o - Assim_benthslar_o - Defec_benthslar_so - Defec_benthslar_d - Excr_benthslar_so - Excr_benthslar_d);
// Rprintf("benthslar_i=%f\n", Upt_total_benthslar_i - Assim_benthslar_i - Defec_benthslar_si - Excr_benthslar_si);

 Assim_benthclar_o= aBclar*Upt_total_benthclar_o;
 Assim_benthclar_i= aBclar*Upt_total_benthclar_i;
// Mass balance check - should  = 0
// Rprintf("benthclar_o=%f\n", Upt_total_benthclar_o - Assim_benthclar_o - Defec_benthclar_so - Defec_benthclar_d - Excr_benthclar_so - Excr_benthclar_d);
// Rprintf("benthclar_i=%f\n", Upt_total_benthclar_i - Assim_benthclar_i - Defec_benthclar_si - Excr_benthclar_si);

 Assim_benths_o=    aBs*Upt_total_benths_o;
 Assim_benths_i=    aBs*Upt_total_benths_i;
// Mass balance check - should  = 0
// Rprintf("benths_o=%f\n", Upt_total_benths_o - Assim_benths_o - Defec_benths_d0 - Defec_benths_d1 - Defec_benths_d2 - Defec_benths_d3 - Excr_benths_o);
// Rprintf("benths_i=%f\n", Upt_total_benths_i - Assim_benths_i - Defec_benths_s10- Defec_benths_s1 - Defec_benths_s2 - Defec_benths_s3 - Excr_benths_i);

 Assim_benthc_o=    aBc*Upt_total_benthc_o;
 Assim_benthc_i=    aBc*Upt_total_benthc_i;
// Mass balance check - should  = 0
// Rprintf("benthc_o=%f\n", Upt_total_benthc_o - Assim_benthc_o - Defec_benthc_d0- Defec_benthc_d1 - Defec_benthc_d2 - Defec_benthc_d3 - Excr_benthc_o);
// Rprintf("benthc_i=%f\n", Upt_total_benthc_i - Assim_benthc_i - Defec_benthc_s0- Defec_benthc_s1 - Defec_benthc_s2 - Defec_benthc_s3 - Excr_benthc_i);

 Assim_fishplar_o=    aFplar*Upt_total_fishplar_o;
 Assim_fishplar_i=    aFplar*Upt_total_fishplar_i;
// Mass balance check - should  = 0
// Rprintf("fishplar_o=%f\n", Upt_total_fishplar_o - Assim_fishplar_o - Defec_fishplar_so - Defec_fishplar_d - Excr_fishplar_so - Excr_fishplar_d);
// Rprintf("fishplar_i=%f\n", Upt_total_fishplar_i - Assim_fishplar_i - Defec_fishplar_si - Excr_fishplar_si);

 Assim_fishdlar_o=    aFdlar*Upt_total_fishdlar_o;
 Assim_fishdlar_i=    aFdlar*Upt_total_fishdlar_i;
// Mass balance check - should  = 0
// Rprintf("fishdlar_o=%f\n", Upt_total_fishdlar_o - Assim_fishdlar_o - Defec_fishdlar_so - Defec_fishdlar_d - Excr_fishdlar_so - Excr_fishdlar_d);
// Rprintf("fishdlar_i=%f\n", Upt_total_fishdlar_i - Assim_fishdlar_i - Defec_fishdlar_si - Excr_fishdlar_si);

 Assim_fishp_o=       aFp*Upt_total_fishp_o;
 Assim_fishp_i=       aFp*Upt_total_fishp_i;
// Mass balance check - should  = 0
// Rprintf("fishp_o=%f\n", Upt_total_fishp_o - Assim_fishp_o - Defec_fishp_so - Defec_fishp_d - Excr_fishp_so - Excr_fishp_d);
// Rprintf("fishp_i=%f\n", Upt_total_fishp_i - Assim_fishp_i - Defec_fishp_si - Excr_fishp_si);

 Assim_fishm_o=       aFm*Upt_total_fishm_o;
 Assim_fishm_i=       aFm*Upt_total_fishm_i;
// Mass balance check - should  = 0
// Rprintf("fishm_o=%f\n", Upt_total_fishm_o - Assim_fishm_o - Defec_fishm_so - Defec_fishm_d - Excr_fishm_so - Excr_fishm_d);
// Rprintf("fishm_i=%f\n", Upt_total_fishm_i - Assim_fishm_i - Defec_fishm_si - Excr_fishm_si);

 Assim_fishd_o=       aFd*Upt_total_fishd_o;
 Assim_fishd_i=       aFd*Upt_total_fishd_i;
// Mass balance check - should  = 0
// Rprintf("fishd_o=%f\n", Upt_total_fishd_o - Assim_fishd_o - Defec_fishd_so - Defec_fishd_d - Excr_fishd_so - Excr_fishd_d);
// Rprintf("fishd_i=%f\n", Upt_total_fishd_i - Assim_fishd_i - Defec_fishd_si - Excr_fishd_si);

 Assim_bird_o=        abird*Upt_total_bird_o;
 Assim_bird_i=        abird*Upt_total_bird_i;
// Mass balance check - should  = 0
// Rprintf("bird_o=%f\n", Upt_total_bird_o - Assim_bird_o - Defec_bird_so - Defec_bird_d - Excr_bird_so - Excr_bird_d);
// Rprintf("bird_i=%f\n", Upt_total_bird_i - Assim_bird_i - Defec_bird_si - Excr_bird_si);
// Rprintf("bird_o=%f\n", Upt_total_bird_o );

 Assim_seal_o=        aseal*Upt_total_seal_o;
 Assim_seal_i=        aseal*Upt_total_seal_i;
// Mass balance check - should  = 0
// Rprintf("seal_o=%f\n", Upt_total_seal_o - Assim_seal_o - Defec_seal_so - Defec_seal_d - Excr_seal_so - Excr_seal_d);
// Rprintf("seal_i=%f\n", Upt_total_seal_i - Assim_seal_i - Defec_seal_si - Excr_seal_si);
// Rprintf("seal_o=%f\n", Upt_total_seal_o);

 Assim_ceta_o=        aceta*Upt_total_ceta_o;
 Assim_ceta_i=        aceta*Upt_total_ceta_i;
// Mass balance check - should  = 0
// Rprintf("ceta_o=%f\n", Upt_total_ceta_o - Assim_ceta_o - Defec_ceta_so - Defec_ceta_d - Excr_ceta_so - Excr_ceta_d);
// Rprintf("ceta_i=%f\n", Upt_total_ceta_i - Assim_ceta_i - Defec_ceta_si - Excr_ceta_si);



/* _____Metabolic losses to ammonia of the combined animal consumers_____ */

// Offshore animals which only excrete to the surface layer
HTLmetabolism_so =   ( ebird_o * y[50] )
                   + ( eseal_o * y[68] )
                   + ( eceta_o * y[70] );

// Offshore animals which only excrete to the deep layer
HTLmetabolism_d =    ( eFd_o * y[47] ) 
                   + ( eBs_o * y[42] )
                   + ( eBc_o * y[44] ) ;

// Offshore animals which  excete to both surface and deep layers
HTLmetabolism_so_d =   ( eH_o * y[39] )
                     + ( eC_o * y[40] )
                     + ( eBslar_o * y[41])
                     + ( eBclar_o * y[43])
                     + ( eFp_o * y[45] ) 
                     + ( eFplar_o * y[46] ) 
                     + ( eFdlar_o * y[48] ) 
                     + ( eFm_o * y[49] ) ;

// Inshore animals which all excrete to the same layer
HTLmetabolism_si =     ( eH_i * y[60] )
                     + ( eC_i * y[61] )
                     + ( eFplar_i * y[62] )
                     + ( eFdlar_i * y[63] )
                     + ( eFp_i * y[64] )
                     + ( eFm_i * y[65] )
                     + ( eFd_i * y[66] ) 
                     + (eBslar_i * y[55])
                     + (eBclar_i * y[56])
                     + ( eBs_i * y[57] )
                     + ( eBc_i * y[58] )
                     + ( ebird_i * y[67] ) 
                     + ( eseal_i * y[69] )
                     + ( eceta_i * y[71] ) ;


/* ______________________________________________________________________________________ */


/* ______Fluxes from fish and shellfish groups to discards over each of the sediment types ____*/
// Need to do this by sediment type because the corpses are explicitly associated with each sediment //
// Discards of birds and mammals do not need ot be included here because they go direct to sediment corpses //
// Discards of kelp do not need ot be included here because they go direct to kelp_debris //

	 Flx_pfish_disc_s0 = pfish_D_p_s0 * Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * Pidiscard;
	 Flx_pfish_disc_s1 = pfish_D_p_s1 * Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * Pidiscard;
	 Flx_pfish_disc_s2 = pfish_D_p_s2 * Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * Pidiscard;
	 Flx_pfish_disc_s3 = pfish_D_p_s3 * Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * Pidiscard;

	 Flx_pfish_disc_d0 = pfish_D_p_d0 * Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * Podiscard;
	 Flx_pfish_disc_d1 = pfish_D_p_d1 * Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * Podiscard;
	 Flx_pfish_disc_d2 = pfish_D_p_d2 * Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * Podiscard;
	 Flx_pfish_disc_d3 = pfish_D_p_d3 * Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * Podiscard;

	 Flx_dfish_disc_s0 = dfish_D_p_s0 * Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * Didiscard;
	 Flx_dfish_disc_s1 = dfish_D_p_s1 * Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * Didiscard;
	 Flx_dfish_disc_s2 = dfish_D_p_s2 * Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * Didiscard;
	 Flx_dfish_disc_s3 = dfish_D_p_s3 * Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * Didiscard;

	 Flx_dfish_disc_d0 = dfish_D_p_d0 * Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * Dodiscard;
	 Flx_dfish_disc_d1 = dfish_D_p_d1 * Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * Dodiscard;
	 Flx_dfish_disc_d2 = dfish_D_p_d2 * Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * Dodiscard;
	 Flx_dfish_disc_d3 = dfish_D_p_d3 * Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * Dodiscard;

	 Flx_mfish_disc_s0 = mfish_D_p_s0 * Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * Midiscard;
	 Flx_mfish_disc_s1 = mfish_D_p_s1 * Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * Midiscard;
	 Flx_mfish_disc_s2 = mfish_D_p_s2 * Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * Midiscard;
	 Flx_mfish_disc_s3 = mfish_D_p_s3 * Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * Midiscard;

	 Flx_mfish_disc_d0 = mfish_D_p_d0 * Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * Modiscard;
	 Flx_mfish_disc_d1 = mfish_D_p_d1 * Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * Modiscard;
	 Flx_mfish_disc_d2 = mfish_D_p_d2 * Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * Modiscard;
	 Flx_mfish_disc_d3 = mfish_D_p_d3 * Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * Modiscard;

	 Flx_sbfish_disc_s0 = sbfish_D_p_s0 * Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * Bsidiscard;
	 Flx_sbfish_disc_s1 = sbfish_D_p_s1 * Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * Bsidiscard;
	 Flx_sbfish_disc_s2 = sbfish_D_p_s2 * Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * Bsidiscard;
	 Flx_sbfish_disc_s3 = sbfish_D_p_s3 * Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * Bsidiscard;

	 Flx_sbfish_disc_d0 = sbfish_D_p_d0 * Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * Bsodiscard;
	 Flx_sbfish_disc_d1 = sbfish_D_p_d1 * Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * Bsodiscard;
	 Flx_sbfish_disc_d2 = sbfish_D_p_d2 * Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * Bsodiscard;
	 Flx_sbfish_disc_d3 = sbfish_D_p_d3 * Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * Bsodiscard;

	 Flx_cbfish_disc_s0 = cbfish_D_p_s0 * Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * Bcidiscard;
	 Flx_cbfish_disc_s1 = cbfish_D_p_s1 * Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * Bcidiscard;
	 Flx_cbfish_disc_s2 = cbfish_D_p_s2 * Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * Bcidiscard;
	 Flx_cbfish_disc_s3 = cbfish_D_p_s3 * Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * Bcidiscard;

	 Flx_cbfish_disc_d0 = cbfish_D_p_d0 * Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * Bcodiscard;
	 Flx_cbfish_disc_d1 = cbfish_D_p_d1 * Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * Bcodiscard;
	 Flx_cbfish_disc_d2 = cbfish_D_p_d2 * Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * Bcodiscard;
	 Flx_cbfish_disc_d3 = cbfish_D_p_d3 * Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * Bcodiscard;

	 Flx_czfish_disc_s0 = czfish_D_p_s0 * Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * Zcidiscard;
	 Flx_czfish_disc_s1 = czfish_D_p_s1 * Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * Zcidiscard;
	 Flx_czfish_disc_s2 = czfish_D_p_s2 * Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * Zcidiscard;
	 Flx_czfish_disc_s3 = czfish_D_p_s3 * Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * Zcidiscard;

	 Flx_czfish_disc_d0 = czfish_D_p_d0 * Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * Zcodiscard;
	 Flx_czfish_disc_d1 = czfish_D_p_d1 * Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * Zcodiscard;
	 Flx_czfish_disc_d2 = czfish_D_p_d2 * Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * Zcodiscard;
	 Flx_czfish_disc_d3 = czfish_D_p_d3 * Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * Zcodiscard;


 Flx_tot_disc_i =  Flx_pfish_disc_s0 + Flx_pfish_disc_s1 + Flx_pfish_disc_s2 + Flx_pfish_disc_s3
                  +Flx_dfish_disc_s0 + Flx_dfish_disc_s1 + Flx_dfish_disc_s2 + Flx_dfish_disc_s3
                  +Flx_mfish_disc_s0 + Flx_mfish_disc_s1 + Flx_mfish_disc_s2 + Flx_mfish_disc_s3
                  +Flx_sbfish_disc_s0 + Flx_sbfish_disc_s1 + Flx_sbfish_disc_s2 + Flx_sbfish_disc_s3
                  +Flx_cbfish_disc_s0 + Flx_cbfish_disc_s1 + Flx_cbfish_disc_s2 + Flx_cbfish_disc_s3
                  +Flx_czfish_disc_s0 + Flx_czfish_disc_s1 + Flx_czfish_disc_s2 + Flx_czfish_disc_s3;


 Flx_tot_disc_o =  Flx_pfish_disc_d0 + Flx_pfish_disc_d1 + Flx_pfish_disc_d2 + Flx_pfish_disc_d3
                  +Flx_dfish_disc_d0 + Flx_dfish_disc_d1 + Flx_dfish_disc_d2 + Flx_dfish_disc_d3
                  +Flx_mfish_disc_d0 + Flx_mfish_disc_d1 + Flx_mfish_disc_d2 + Flx_mfish_disc_d3
                  +Flx_sbfish_disc_d0 + Flx_sbfish_disc_d1 + Flx_sbfish_disc_d2 + Flx_sbfish_disc_d3
                  +Flx_cbfish_disc_d0 + Flx_cbfish_disc_d1 + Flx_cbfish_disc_d2 + Flx_cbfish_disc_d3
                  +Flx_czfish_disc_d0 + Flx_czfish_disc_d1 + Flx_czfish_disc_d2 + Flx_czfish_disc_d3;




/* ______Fluxes from fish and shellfish groups to offal over each of the sediment types ____*/
// Need to do this by sediment type because the corpses are explicitly associated with each sediment //
// Offal from processing of birds and mammals IS included here because its more like discard material than sediment corpses //
// Offal from kelp processing is NOT included here because it goes direct to kelp_debris //

	 Flx_pfish_offal_s0 = pfish_G_p_s0 * Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * (1-Pidiscard) * Pigutting * offal_prop_live_weight;
	 Flx_pfish_offal_s1 = pfish_G_p_s1 * Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * (1-Pidiscard) * Pigutting * offal_prop_live_weight;
	 Flx_pfish_offal_s2 = pfish_G_p_s2 * Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * (1-Pidiscard) * Pigutting * offal_prop_live_weight;
	 Flx_pfish_offal_s3 = pfish_G_p_s3 * Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * (1-Pidiscard) * Pigutting * offal_prop_live_weight;

	 Flx_pfish_offal_d0 = pfish_G_p_d0 * Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * (1-Podiscard) * Pogutting * offal_prop_live_weight;
	 Flx_pfish_offal_d1 = pfish_G_p_d1 * Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * (1-Podiscard) * Pogutting * offal_prop_live_weight;
	 Flx_pfish_offal_d2 = pfish_G_p_d2 * Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * (1-Podiscard) * Pogutting * offal_prop_live_weight;
	 Flx_pfish_offal_d3 = pfish_G_p_d3 * Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * (1-Podiscard) * Pogutting * offal_prop_live_weight;

	 Flx_dfish_offal_s0 = dfish_G_p_s0 * Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (1-Didiscard) * Digutting * offal_prop_live_weight;
	 Flx_dfish_offal_s1 = dfish_G_p_s1 * Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (1-Didiscard) * Digutting * offal_prop_live_weight;
	 Flx_dfish_offal_s2 = dfish_G_p_s2 * Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (1-Didiscard) * Digutting * offal_prop_live_weight;
	 Flx_dfish_offal_s3 = dfish_G_p_s3 * Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (1-Didiscard) * Digutting * offal_prop_live_weight;

	 Flx_dfish_offal_d0 = dfish_G_p_d0 * Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (1-Dodiscard) * Dogutting * offal_prop_live_weight;
	 Flx_dfish_offal_d1 = dfish_G_p_d1 * Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (1-Dodiscard) * Dogutting * offal_prop_live_weight;
	 Flx_dfish_offal_d2 = dfish_G_p_d2 * Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (1-Dodiscard) * Dogutting * offal_prop_live_weight;
	 Flx_dfish_offal_d3 = dfish_G_p_d3 * Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (1-Dodiscard) * Dogutting * offal_prop_live_weight;

	 Flx_mfish_offal_s0 = mfish_G_p_s0 * Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * (1-Midiscard) * Migutting * offal_prop_live_weight;
	 Flx_mfish_offal_s1 = mfish_G_p_s1 * Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * (1-Midiscard) * Migutting * offal_prop_live_weight;
	 Flx_mfish_offal_s2 = mfish_G_p_s2 * Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * (1-Midiscard) * Migutting * offal_prop_live_weight;
	 Flx_mfish_offal_s3 = mfish_G_p_s3 * Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * (1-Midiscard) * Migutting * offal_prop_live_weight;

	 Flx_mfish_offal_d0 = mfish_G_p_d0 * Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * (1-Modiscard) * Mogutting * offal_prop_live_weight;
	 Flx_mfish_offal_d1 = mfish_G_p_d1 * Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * (1-Modiscard) * Mogutting * offal_prop_live_weight;
	 Flx_mfish_offal_d2 = mfish_G_p_d2 * Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * (1-Modiscard) * Mogutting * offal_prop_live_weight;
	 Flx_mfish_offal_d3 = mfish_G_p_d3 * Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * (1-Modiscard) * Mogutting * offal_prop_live_weight;

	 Flx_sbfish_offal_s0 = sbfish_G_p_s0 * Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * (1-Bsidiscard) * Bsigutting * offal_prop_live_weight;
	 Flx_sbfish_offal_s1 = sbfish_G_p_s1 * Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * (1-Bsidiscard) * Bsigutting * offal_prop_live_weight;
	 Flx_sbfish_offal_s2 = sbfish_G_p_s2 * Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * (1-Bsidiscard) * Bsigutting * offal_prop_live_weight;
	 Flx_sbfish_offal_s3 = sbfish_G_p_s3 * Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * (1-Bsidiscard) * Bsigutting * offal_prop_live_weight;

	 Flx_sbfish_offal_d0 = sbfish_G_p_d0 * Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * (1-Bsodiscard) * Bsogutting * offal_prop_live_weight;
	 Flx_sbfish_offal_d1 = sbfish_G_p_d1 * Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * (1-Bsodiscard) * Bsogutting * offal_prop_live_weight;
	 Flx_sbfish_offal_d2 = sbfish_G_p_d2 * Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * (1-Bsodiscard) * Bsogutting * offal_prop_live_weight;
	 Flx_sbfish_offal_d3 = sbfish_G_p_d3 * Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * (1-Bsodiscard) * Bsogutting * offal_prop_live_weight;

	 Flx_cbfish_offal_s0 = cbfish_G_p_s0 * Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * (1-Bcidiscard) * Bcigutting * offal_prop_live_weight;
	 Flx_cbfish_offal_s1 = cbfish_G_p_s1 * Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * (1-Bcidiscard) * Bcigutting * offal_prop_live_weight;
	 Flx_cbfish_offal_s2 = cbfish_G_p_s2 * Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * (1-Bcidiscard) * Bcigutting * offal_prop_live_weight;
	 Flx_cbfish_offal_s3 = cbfish_G_p_s3 * Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * (1-Bcidiscard) * Bcigutting * offal_prop_live_weight;

	 Flx_cbfish_offal_d0 = cbfish_G_p_d0 * Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * (1-Bcodiscard) * Bcogutting * offal_prop_live_weight;
	 Flx_cbfish_offal_d1 = cbfish_G_p_d1 * Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * (1-Bcodiscard) * Bcogutting * offal_prop_live_weight;
	 Flx_cbfish_offal_d2 = cbfish_G_p_d2 * Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * (1-Bcodiscard) * Bcogutting * offal_prop_live_weight;
	 Flx_cbfish_offal_d3 = cbfish_G_p_d3 * Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * (1-Bcodiscard) * Bcogutting * offal_prop_live_weight;

	 Flx_czfish_offal_s0 = czfish_G_p_s0 * Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * (1-Zcidiscard) * Zcigutting * offal_prop_live_weight;
	 Flx_czfish_offal_s1 = czfish_G_p_s1 * Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * (1-Zcidiscard) * Zcigutting * offal_prop_live_weight;
	 Flx_czfish_offal_s2 = czfish_G_p_s2 * Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * (1-Zcidiscard) * Zcigutting * offal_prop_live_weight;
	 Flx_czfish_offal_s3 = czfish_G_p_s3 * Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * (1-Zcidiscard) * Zcigutting * offal_prop_live_weight;

	 Flx_czfish_offal_d0 = czfish_G_p_d0 * Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * (1-Zcodiscard) * Zcogutting * offal_prop_live_weight;
	 Flx_czfish_offal_d1 = czfish_G_p_d1 * Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * (1-Zcodiscard) * Zcogutting * offal_prop_live_weight;
	 Flx_czfish_offal_d2 = czfish_G_p_d2 * Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * (1-Zcodiscard) * Zcogutting * offal_prop_live_weight;
	 Flx_czfish_offal_d3 = czfish_G_p_d3 * Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * (1-Zcodiscard) * Zcogutting * offal_prop_live_weight;



	 Flx_bird_offal_s0 = bird_G_p_s0 * Fbdidaily * (twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) * (1-BDidiscard) * BDigutting * offal_prop_live_weight;
	 Flx_bird_offal_s1 = bird_G_p_s1 * Fbdidaily * (twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) * (1-BDidiscard) * BDigutting * offal_prop_live_weight;
	 Flx_bird_offal_s2 = bird_G_p_s2 * Fbdidaily * (twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) * (1-BDidiscard) * BDigutting * offal_prop_live_weight;
	 Flx_bird_offal_s3 = bird_G_p_s3 * Fbdidaily * (twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) * (1-BDidiscard) * BDigutting * offal_prop_live_weight;

	 Flx_bird_offal_d0 = bird_G_p_d0 * Fczodaily * (twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) * (1-BDodiscard) * BDogutting * offal_prop_live_weight;
	 Flx_bird_offal_d1 = bird_G_p_d1 * Fczodaily * (twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) * (1-BDodiscard) * BDogutting * offal_prop_live_weight;
	 Flx_bird_offal_d2 = bird_G_p_d2 * Fczodaily * (twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) * (1-BDodiscard) * BDogutting * offal_prop_live_weight;
	 Flx_bird_offal_d3 = bird_G_p_d3 * Fczodaily * (twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) * (1-BDodiscard) * BDogutting * offal_prop_live_weight;



	 Flx_seal_offal_s0 = seal_G_p_s0 * Fslidaily * (twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) * (1-SLidiscard) * SLigutting * offal_prop_live_weight;
	 Flx_seal_offal_s1 = seal_G_p_s1 * Fslidaily * (twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) * (1-SLidiscard) * SLigutting * offal_prop_live_weight;
	 Flx_seal_offal_s2 = seal_G_p_s2 * Fslidaily * (twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) * (1-SLidiscard) * SLigutting * offal_prop_live_weight;
	 Flx_seal_offal_s3 = seal_G_p_s3 * Fslidaily * (twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) * (1-SLidiscard) * SLigutting * offal_prop_live_weight;

	 Flx_seal_offal_d0 = seal_G_p_d0 * Fslodaily * (twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) * (1-SLodiscard) * SLogutting * offal_prop_live_weight;
	 Flx_seal_offal_d1 = seal_G_p_d1 * Fslodaily * (twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) * (1-SLodiscard) * SLogutting * offal_prop_live_weight;
	 Flx_seal_offal_d2 = seal_G_p_d2 * Fslodaily * (twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) * (1-SLodiscard) * SLogutting * offal_prop_live_weight;
	 Flx_seal_offal_d3 = seal_G_p_d3 * Fslodaily * (twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) * (1-SLodiscard) * SLogutting * offal_prop_live_weight;


	 Flx_ceta_offal_s0 = ceta_G_p_s0 * Fctidaily * (twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) * (1-CTidiscard) * CTigutting * offal_prop_live_weight;
	 Flx_ceta_offal_s1 = ceta_G_p_s1 * Fctidaily * (twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) * (1-CTidiscard) * CTigutting * offal_prop_live_weight;
	 Flx_ceta_offal_s2 = ceta_G_p_s2 * Fctidaily * (twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) * (1-CTidiscard) * CTigutting * offal_prop_live_weight;
	 Flx_ceta_offal_s3 = ceta_G_p_s3 * Fctidaily * (twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) * (1-CTidiscard) * CTigutting * offal_prop_live_weight;

	 Flx_ceta_offal_d0 = ceta_G_p_d0 * Fctodaily * (twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) * (1-CTodiscard) * CTogutting * offal_prop_live_weight;
	 Flx_ceta_offal_d1 = ceta_G_p_d1 * Fctodaily * (twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) * (1-CTodiscard) * CTogutting * offal_prop_live_weight;
	 Flx_ceta_offal_d2 = ceta_G_p_d2 * Fctodaily * (twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) * (1-CTodiscard) * CTogutting * offal_prop_live_weight;
	 Flx_ceta_offal_d3 = ceta_G_p_d3 * Fctodaily * (twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) * (1-CTodiscard) * CTogutting * offal_prop_live_weight;




 Flx_tot_offal_i =  Flx_pfish_offal_s0 + Flx_pfish_offal_s1 + Flx_pfish_offal_s2 + Flx_pfish_offal_s3
                  +Flx_dfish_offal_s0 + Flx_dfish_offal_s1 + Flx_dfish_offal_s2 + Flx_dfish_offal_s3
                  +Flx_mfish_offal_s0 + Flx_mfish_offal_s1 + Flx_mfish_offal_s2 + Flx_mfish_offal_s3
                  +Flx_sbfish_offal_s0 + Flx_sbfish_offal_s1 + Flx_sbfish_offal_s2 + Flx_sbfish_offal_s3
                  +Flx_cbfish_offal_s0 + Flx_cbfish_offal_s1 + Flx_cbfish_offal_s2 + Flx_cbfish_offal_s3
                  +Flx_czfish_offal_s0 + Flx_czfish_offal_s1 + Flx_czfish_offal_s2 + Flx_czfish_offal_s3
                  +Flx_bird_offal_s0 + Flx_bird_offal_s1 + Flx_bird_offal_s2 + Flx_bird_offal_s3
                  +Flx_seal_offal_s0 + Flx_seal_offal_s1 + Flx_seal_offal_s2 + Flx_seal_offal_s3
                  +Flx_ceta_offal_s0 + Flx_ceta_offal_s1 + Flx_ceta_offal_s2 + Flx_ceta_offal_s3;


 Flx_tot_offal_o =  Flx_pfish_offal_d0 + Flx_pfish_offal_d1 + Flx_pfish_offal_d2 + Flx_pfish_offal_d3
                  +Flx_dfish_offal_d0 + Flx_dfish_offal_d1 + Flx_dfish_offal_d2 + Flx_dfish_offal_d3
                  +Flx_mfish_offal_d0 + Flx_mfish_offal_d1 + Flx_mfish_offal_d2 + Flx_mfish_offal_d3
                  +Flx_sbfish_offal_d0 + Flx_sbfish_offal_d1 + Flx_sbfish_offal_d2 + Flx_sbfish_offal_d3
                  +Flx_cbfish_offal_d0 + Flx_cbfish_offal_d1 + Flx_cbfish_offal_d2 + Flx_cbfish_offal_d3
                  +Flx_czfish_offal_d0 + Flx_czfish_offal_d1 + Flx_czfish_offal_d2 + Flx_czfish_offal_d3
                  +Flx_bird_offal_d0 + Flx_bird_offal_d1 + Flx_bird_offal_d2 + Flx_bird_offal_d3
                  +Flx_seal_offal_d0 + Flx_seal_offal_d1 + Flx_seal_offal_d2 + Flx_seal_offal_d3
                  +Flx_ceta_offal_d0 + Flx_ceta_offal_d1 + Flx_ceta_offal_d2 + Flx_ceta_offal_d3;

 // kelp offal goes to kelpdebris  not to discards, but offal from processing birds seals and cetaceans does go to discards
 // unlike bird seal and cetacean bycatch discards




 if((Flx_tot_disc_i + Flx_tot_offal_i) >0) {
	  p_disc_s0 = (Flx_pfish_offal_s0 + Flx_dfish_offal_s0 + Flx_mfish_offal_s0 + Flx_sbfish_offal_s0 + Flx_cbfish_offal_s0 + Flx_czfish_offal_s0 + Flx_bird_offal_s0 + Flx_seal_offal_s0 + Flx_ceta_offal_s0
                       + Flx_pfish_disc_s0 + Flx_dfish_disc_s0 + Flx_mfish_disc_s0 + Flx_sbfish_disc_s0 + Flx_cbfish_disc_s0 + Flx_czfish_disc_s0)/(Flx_tot_offal_i+Flx_tot_disc_i);


	  p_disc_s1 = (Flx_pfish_offal_s1 + Flx_dfish_offal_s1 + Flx_mfish_offal_s1 + Flx_sbfish_offal_s1 + Flx_cbfish_offal_s1 + Flx_czfish_offal_s1 + Flx_bird_offal_s1 + Flx_seal_offal_s1 + Flx_ceta_offal_s1
                       + Flx_pfish_disc_s1 + Flx_dfish_disc_s1 + Flx_mfish_disc_s1 + Flx_sbfish_disc_s1 + Flx_cbfish_disc_s1 + Flx_czfish_disc_s1)/(Flx_tot_offal_i+Flx_tot_disc_i);


	  p_disc_s2 = (Flx_pfish_offal_s2 + Flx_dfish_offal_s2 + Flx_mfish_offal_s2 + Flx_sbfish_offal_s2 + Flx_cbfish_offal_s2 + Flx_czfish_offal_s2 + Flx_bird_offal_s2 + Flx_seal_offal_s2 + Flx_ceta_offal_s2
                       + Flx_pfish_disc_s2 + Flx_dfish_disc_s2 + Flx_mfish_disc_s2 + Flx_sbfish_disc_s2 + Flx_cbfish_disc_s2 + Flx_czfish_disc_s2)/(Flx_tot_offal_i+Flx_tot_disc_i);


	  p_disc_s3 = (Flx_pfish_offal_s3 + Flx_dfish_offal_s3 + Flx_mfish_offal_s3 + Flx_sbfish_offal_s3 + Flx_cbfish_offal_s3 + Flx_czfish_offal_s3 + Flx_bird_offal_s3 + Flx_seal_offal_s3 + Flx_ceta_offal_s3
                       + Flx_pfish_disc_s3 + Flx_dfish_disc_s3 + Flx_mfish_disc_s3 + Flx_sbfish_disc_s3 + Flx_cbfish_disc_s3 + Flx_czfish_disc_s3)/(Flx_tot_offal_i+Flx_tot_disc_i);


 }
 else {
         p_disc_s0 = 0;
         p_disc_s1 = 0;
         p_disc_s2 = 0;
         p_disc_s3 = 0;
}

 if((Flx_tot_disc_o  + Flx_tot_offal_o) >0) {
	  p_disc_d0 = (Flx_pfish_offal_d0 + Flx_dfish_offal_d0 + Flx_mfish_offal_d0 + Flx_sbfish_offal_d0 + Flx_cbfish_offal_d0 + Flx_czfish_offal_d0 + Flx_bird_offal_d0 + Flx_seal_offal_d0 + Flx_ceta_offal_d0
                       + Flx_pfish_disc_d0 + Flx_dfish_disc_d0 + Flx_mfish_disc_d0 + Flx_sbfish_disc_d0 + Flx_cbfish_disc_d0 + Flx_czfish_disc_d0)/(Flx_tot_offal_o+Flx_tot_disc_o);


	  p_disc_d1 = (Flx_pfish_offal_d1 + Flx_dfish_offal_d1 + Flx_mfish_offal_d1 + Flx_sbfish_offal_d1 + Flx_cbfish_offal_d1 + Flx_czfish_offal_d1 + Flx_bird_offal_d1 + Flx_seal_offal_d1 + Flx_ceta_offal_d1
                       + Flx_pfish_disc_d1 + Flx_dfish_disc_d1 + Flx_mfish_disc_d1 + Flx_sbfish_disc_d1 + Flx_cbfish_disc_d1 + Flx_czfish_disc_d1)/(Flx_tot_offal_o+Flx_tot_disc_o);


	  p_disc_d2 = (Flx_pfish_offal_d2 + Flx_dfish_offal_d2 + Flx_mfish_offal_d2 + Flx_sbfish_offal_d2 + Flx_cbfish_offal_d2 + Flx_czfish_offal_d2 + Flx_bird_offal_d2 + Flx_seal_offal_d2 + Flx_ceta_offal_d2
                       + Flx_pfish_disc_d2 + Flx_dfish_disc_d2 + Flx_mfish_disc_d2 + Flx_sbfish_disc_d2 + Flx_cbfish_disc_d2 + Flx_czfish_disc_d2)/(Flx_tot_offal_o+Flx_tot_disc_o);


	  p_disc_d3 = (Flx_pfish_offal_d3 + Flx_dfish_offal_d3 + Flx_mfish_offal_d3 + Flx_sbfish_offal_d3 + Flx_cbfish_offal_d3 + Flx_czfish_offal_d3 + Flx_bird_offal_d3 + Flx_seal_offal_d3 + Flx_ceta_offal_d3
                       + Flx_pfish_disc_d3 + Flx_dfish_disc_d3 + Flx_mfish_disc_d3 + Flx_sbfish_disc_d3 + Flx_cbfish_disc_d3 + Flx_czfish_disc_d3)/(Flx_tot_offal_o+Flx_tot_disc_o);

 }
 else {
         p_disc_d0 = 0;
         p_disc_d1 = 0;
         p_disc_d2 = 0;
         p_disc_d3 = 0;
 }




/* ______________________________________________________________________________________ */

/* _____Background sediment water diffusion fluxes of dissolved material in undistrubed sediment_____ */
/* ______ excavation of sediment by deposit/susp feeding benthos - releases nutrient but does not suspend detritus ______*/
// Modified here to cover the fluxes between EACH OF THE 6 SEDIMENT TYPES and the surface and deep layers - CRITICAL BIT //
// Remember that Upt_xdetritus_xx_benths_x is the uptake of labile and refratory detritus combined

 if(area_s1>0 && rock_s1>0.5) {
	s_w_amm_flx_s1=            area_s1*(sed_wat_dif_s1*60*60*24*((y[23]/(area_s1*porosity_s1*thik_x_s1))-(y[52]/volume_si)));
	s_w_nit_flx_s1=            area_s1*(sed_wat_dif_s1*60*60*24*((y[31]/(area_s1*porosity_s1*thik_x_s1))-(y[53]/volume_si)));
        bioturb_daily_s1 = (Upt_xTdetritus_s1_benths_i/(y[2]+y[8]));
 }
 else {
        s_w_amm_flx_s1=0;
        s_w_nit_flx_s1=0;
        bioturb_daily_s1 = 0;
 }

/* .................... */


 if(area_s2>0 && rock_s2>0.5) {
	s_w_amm_flx_s2=            area_s2*(sed_wat_dif_s2*60*60*24*((y[24]/(area_s2*porosity_s2*thik_x_s2))-(y[52]/volume_si)));
	s_w_nit_flx_s2=            area_s2*(sed_wat_dif_s2*60*60*24*((y[32]/(area_s2*porosity_s2*thik_x_s2))-(y[53]/volume_si)));
        bioturb_daily_s2 = (Upt_xTdetritus_s2_benths_i/(y[3]+y[9]));
 }
 else {
        s_w_amm_flx_s2=0;
        s_w_nit_flx_s2=0;
        bioturb_daily_s2 = 0;
 }

/* .................... */

 if(area_s3>0 && rock_s3>0.5) {
	s_w_amm_flx_s3=            area_s3*(sed_wat_dif_s3*60*60*24*((y[25]/(area_s3*porosity_s3*thik_x_s3))-(y[52]/volume_si)));
	s_w_nit_flx_s3=            area_s3*(sed_wat_dif_s3*60*60*24*((y[33]/(area_s3*porosity_s3*thik_x_s3))-(y[53]/volume_si)));
        bioturb_daily_s3 = (Upt_xTdetritus_s3_benths_i/(y[4]+y[10]));
 }
 else {
        s_w_amm_flx_s3=0;
        s_w_nit_flx_s3=0;
        bioturb_daily_s3 = 0;
 }

/* .................... */


 if(area_d1>0 && rock_d1>0.5) {
	s_w_amm_flx_d1=            area_d1*(sed_wat_dif_d1*60*60*24*((y[26]/(area_d1*porosity_d1*thik_x_d1))-(y[22]/(volume_d))));
	s_w_nit_flx_d1=            area_d1*(sed_wat_dif_d1*60*60*24*((y[34]/(area_d1*porosity_d1*thik_x_d1))-(y[30]/(volume_d))));
        bioturb_daily_d1 = (Upt_xTdetritus_d1_benths_o/(y[5]+y[11]));
 }
 else {
        s_w_amm_flx_d1=0;
        s_w_nit_flx_d1=0;
        bioturb_daily_d1 = 0;
 }

/* .................... */


 if(area_d2>0 && rock_d2>0.5) {
	s_w_amm_flx_d2=            area_d2*(sed_wat_dif_d2*60*60*24*((y[27]/(area_d2*porosity_d2*thik_x_d2))-(y[22]/(volume_d))));
	s_w_nit_flx_d2=            area_d2*(sed_wat_dif_d2*60*60*24*((y[35]/(area_d2*porosity_d2*thik_x_d2))-(y[30]/(volume_d))));
        bioturb_daily_d2 = (Upt_xTdetritus_d2_benths_o/(y[6]+y[12]));
 }
 else {
        s_w_amm_flx_d2=0;
        s_w_nit_flx_d2=0;
        bioturb_daily_d2 = 0;
 }

/* .................... */


 if(area_d3>0 && rock_d3>0.5) {
	s_w_amm_flx_d3=            area_d3*(sed_wat_dif_d3*60*60*24*((y[28]/(area_d3*porosity_d3*thik_x_d3))-(y[22]/(volume_d))));
	s_w_nit_flx_d3=            area_d3*(sed_wat_dif_d3*60*60*24*((y[36]/(area_d3*porosity_d3*thik_x_d3))-(y[30]/(volume_d))));
        bioturb_daily_d3 = (Upt_xTdetritus_d3_benths_o/(y[7]+y[13]));
 }
 else {
        s_w_amm_flx_d3=0;
        s_w_nit_flx_d3=0;
        bioturb_daily_d3 = 0;
 }

/* .................... */




// Some print out to screen for diagnostics
// Rprintf("surface dif=%f\n", (log(sed_wat_dif_s1)) );
// Rprintf("deep dif=%f\n", (log(sed_wat_dif_d1)) );

// Some print out to screen for diagnostics
// Rprintf("surface nitrate flux=%f\n", (s_w_nit_flx_s1) );
// Rprintf("deep nitrate flux=%f\n", (s_w_nit_flx_d1) );

// Some print out to screen for diagnostics
// Rprintf("surface propn detritus uptake=%f\n", (bioturb_daily_s1) );
// Rprintf("deep propn detritus uptake=%f\n", (bioturb_daily_d1) );


/* _____ calculate the proportion of sediment area distrurbed per day due to natural processes, ploughing and bioturbation ___*/
/* _____ in the case that a habitat is designated as rock then the relevenat depth term is alreday set to zero ____*/


 inshore_plough_daily =  plough_daily_s1 * area_s1
                        + plough_daily_s2 * area_s2
                        + plough_daily_s3 * area_s3;


 offshore_plough_daily =  plough_daily_d1 * area_d1
                        + plough_daily_d2 * area_d2
                        + plough_daily_d3 * area_d3 ;
// These terms are the total plough rate in each depth zone. they are needed only to disaggregated the damage mortality flux from benthos to corpses in each
// depth zone into the individual sediment tiles. In the event that there is no sesabed ploughing in any of the tiles in a zone, so that the plough_daily terms
// are zero, the code would generate a divide by zero error in the balance equation we can catch is by resetting the plough_daily terms to any positive value
// in the event that they are zero. 
 if(inshore_plough_daily == 0) {
    inshore_plough_daily=1;
 }
 if(offshore_plough_daily == 0) {
    offshore_plough_daily=1;
 }



 porewater_disturb_s1 = twomin(1,(erosion_depth_s1*driver_s1_erosion + plough_depth_s1*plough_daily_s1 + bioturb_depth_s1*bioturb_daily_s1));
 porewater_disturb_s2 = twomin(1,(erosion_depth_s2*driver_s2_erosion + plough_depth_s2*plough_daily_s2 + bioturb_depth_s2*bioturb_daily_s2));
 porewater_disturb_s3 = twomin(1,(erosion_depth_s3*driver_s2_erosion + plough_depth_s3*plough_daily_s3 + bioturb_depth_s3*bioturb_daily_s3));
 porewater_disturb_d1 = twomin(1,(erosion_depth_d1*driver_d1_erosion + plough_depth_d1*plough_daily_d1 + bioturb_depth_d1*bioturb_daily_d1));
 porewater_disturb_d2 = twomin(1,(erosion_depth_d2*driver_d2_erosion + plough_depth_d2*plough_daily_d2 + bioturb_depth_d2*bioturb_daily_d2));
 porewater_disturb_d3 = twomin(1,(erosion_depth_d3*driver_d3_erosion + plough_depth_d3*plough_daily_d3 + bioturb_depth_d3*bioturb_daily_d3));

// Some print out to screen for diagnostics
// Rprintf("surface propn disturbed=%f\n", (porewater_disturb_s) );
// Rprintf("deep propn disturbed=%f\n", (porewater_disturb_d) );

 sediment_resuspend_s1 = twomin(1,(erosion_depth_s1*driver_s1_erosion + plough_depth_s1*plough_daily_s1 ));
 sediment_resuspend_s2 = twomin(1,(erosion_depth_s2*driver_s2_erosion + plough_depth_s2*plough_daily_s2 ));
 sediment_resuspend_s3 = twomin(1,(erosion_depth_s3*driver_s3_erosion + plough_depth_s3*plough_daily_s3 ));
 
 sediment_resuspend_d1 = twomin(1,(erosion_depth_d1*driver_d1_erosion + plough_depth_d1*plough_daily_d1 ));
 sediment_resuspend_d2 = twomin(1,(erosion_depth_d2*driver_d2_erosion + plough_depth_d2*plough_daily_d2 ));
 sediment_resuspend_d3 = twomin(1,(erosion_depth_d3*driver_d3_erosion + plough_depth_d3*plough_daily_d3 ));


/* _____sediment water exchange fluxes of dissolved material due to disturbance_____ */
// positive values = flux from sediment to the water

/* _____resuspension of organic matter from the sediment into the water column due to natural physical disturbance and ploughing_____ */
// positive values = flux from sediment to the water


 if(area_s1>0 && rock_s1>0.5) {
 s_w_amm_disturb_flx_s1=   area_s1*( (porewater_disturb_s1*y[23]) - ((y[52]/volume_si)*porewater_disturb_s1*porosity_s1*thik_x_s1) );
 s_w_nit_disturb_flx_s1=   area_s1*( (porewater_disturb_s1*y[31]) - ((y[53]/volume_si)*porewater_disturb_s1*porosity_s1*thik_x_s1) );
 s_w_det_resuspend_flx_s1=   area_s1*( (sediment_resuspend_s1*y[2]) );
 }
 else {
 s_w_amm_disturb_flx_s1=0;
 s_w_nit_disturb_flx_s1=0;
 s_w_det_resuspend_flx_s1=0;
 }

 if(area_s2>0 && rock_s2>0.5) {
 s_w_amm_disturb_flx_s2=   area_s2*( (porewater_disturb_s2*y[24]) - ((y[52]/volume_si)*porewater_disturb_s2*porosity_s2*thik_x_s2) );
 s_w_nit_disturb_flx_s2=   area_s2*( (porewater_disturb_s2*y[32]) - ((y[53]/volume_si)*porewater_disturb_s2*porosity_s2*thik_x_s2) );
 s_w_det_resuspend_flx_s2=   area_s2*( (sediment_resuspend_s2*y[3]) );
 }
 else {
 s_w_amm_disturb_flx_s2=0;
 s_w_nit_disturb_flx_s2=0;
 s_w_det_resuspend_flx_s2=0;
 }

 if(area_s3>0 && rock_s3>0.5) {
 s_w_amm_disturb_flx_s3=   area_s3*( (porewater_disturb_s3*y[25]) - ((y[52]/volume_si)*porewater_disturb_s3*porosity_s3*thik_x_s3) );
 s_w_nit_disturb_flx_s3=   area_s3*( (porewater_disturb_s3*y[33]) - ((y[53]/volume_si)*porewater_disturb_s3*porosity_s3*thik_x_s3) );
 s_w_det_resuspend_flx_s3=   area_s3*( (sediment_resuspend_s3*y[4]) );
 }
 else {
 s_w_amm_disturb_flx_s3=0;
 s_w_nit_disturb_flx_s3=0;
 s_w_det_resuspend_flx_s3=0;
 }

 if(area_d1>0 && rock_d1>0.5) {
 s_w_amm_disturb_flx_d1=   area_d1*( (porewater_disturb_d1*y[26]) - ((y[22]/volume_d)*porewater_disturb_d1*porosity_d1*thik_x_d1) );
 s_w_nit_disturb_flx_d1=   area_d1*( (porewater_disturb_d1*y[34]) - ((y[30]/volume_d)*porewater_disturb_d1*porosity_d1*thik_x_d1) ); 
 s_w_det_resuspend_flx_d1=   area_d1*( (sediment_resuspend_d1*y[5]) );
 }
 else {
 s_w_amm_disturb_flx_d1=0;
 s_w_nit_disturb_flx_d1=0;
 s_w_det_resuspend_flx_d1=0;
 }

 if(area_d2>0 && rock_d2>0.5) {
 s_w_amm_disturb_flx_d2=   area_d2*( (porewater_disturb_d2*y[27]) - ((y[22]/volume_d)*porewater_disturb_d2*porosity_d2*thik_x_d2) );
 s_w_nit_disturb_flx_d2=   area_d2*( (porewater_disturb_d2*y[35]) - ((y[30]/volume_d)*porewater_disturb_d2*porosity_d2*thik_x_d2) ); 
 s_w_det_resuspend_flx_d2=   area_d2*( (sediment_resuspend_d2*y[6]) );
 }
 else {
 s_w_amm_disturb_flx_d2=0;
 s_w_nit_disturb_flx_d2=0;
 s_w_det_resuspend_flx_d2=0;
 }

 if(area_d3>0 && rock_d3>0.5) {
 s_w_amm_disturb_flx_d3=   area_d3*( (porewater_disturb_d3*y[28]) - ((y[22]/volume_d)*porewater_disturb_d3*porosity_d3*thik_x_d3) );
 s_w_nit_disturb_flx_d3=   area_d3*( (porewater_disturb_d3*y[36]) - ((y[30]/volume_d)*porewater_disturb_d3*porosity_d3*thik_x_d3) ); 
 s_w_det_resuspend_flx_d3=   area_d3*( (sediment_resuspend_d3*y[7]) );
 }
 else {
 s_w_amm_disturb_flx_d3=0;
 s_w_nit_disturb_flx_d3=0;
 s_w_det_resuspend_flx_d3=0;
 }


/* .................... */




/* _____Verical mixing fluxes_____ */
// Offshore surface to deep is a negative term.

 Vmix_detritus=          (driverv_dif*60*60*24*((y[1]/volume_d)-(y[0]/volume_so)));
 Vmix_ammonia=           (driverv_dif*60*60*24*((y[22]/volume_d)-(y[21]/volume_so)));
 Vmix_nitrate=           (driverv_dif*60*60*24*((y[30]/volume_d)-(y[29]/volume_so)));
 Vmix_phyt=              (driverv_dif*60*60*24*((y[38]/volume_d)-(y[37]/volume_so)));


/* _____Upwelling fluxes_____*/
 Upwelling_det=          ((y[1]/volume_d)*drivers_upwell);
 Upwelling_amm=          ((y[22]/volume_d)*drivers_upwell);
 Upwelling_nit=          ((y[30]/volume_d)*drivers_upwell);
 Upwelling_phyt=         ((y[38]/volume_d)*drivers_upwell);



/* _____Settling flux  of DEEP suspended detritus onto the seabed_____ */
 detr_settle_d=         dsink_d*y[1] * (nonrock_d/(1-shallowprop));

/* _____Settling flux  of SHALLOW suspended detritus into the deep layer and onto the seabed_____ */
 detr_settle_s_d=            (dsink_s*y[0]);
// detr_settle_s_b=            (nonrock_s/shallowprop)*(dsink_s*y[51]);
 detr_settle_s_b=            (nonrock_s/shallowprop)*(dsink_d*y[51]);
// NOTE - deep settling rate applied in the inshore zone




/* _____Horizontal fluxes_____ */


/* _____Ocean boundary influxes to surface offshore layer_____ */
 OceanIN_sodetritus=      ((driverboundso_det)*driverso_inflow);
 OceanIN_soammonia=       ((driverboundso_amm)*driverso_inflow);
 OceanIN_sonitrate=       ((driverboundso_nit)*driverso_inflow);
 OceanIN_sophyt=          ((driverboundso_phyt)*driverso_inflow);


/* _____Ocean boundary influxes to deep offshore layer_____ */
 OceanIN_ddetritus=      ((driverboundd_det)*driverd_inflow);
 OceanIN_dammonia=       ((driverboundd_amm)*driverd_inflow);
 OceanIN_dnitrate=       ((driverboundd_nit)*driverd_inflow);
 OceanIN_dphyt=          ((driverboundd_phyt)*driverd_inflow);

/* _____Ocean boundary influxes to surface inshore layer_____ */
 OceanIN_sidetritus=      ((driverboundsi_det)*driversi_inflow);
 OceanIN_siammonia=       ((driverboundsi_amm)*driversi_inflow);
 OceanIN_sinitrate=       ((driverboundsi_nit)*driversi_inflow);
 OceanIN_siphyt=          ((driverboundsi_phyt)*driversi_inflow);



/*_____Ocean boundary outfluxes from surface offshore layer_____ */
/*_____Assumes that the river inflow to the inshore is pushed out o fthe model domain entirely via the offshore zone_____*/
 OceanOUT_sodetritus=     ((y[0]/volume_so)*(driverso_outflow + driverriver));
 OceanOUT_soammonia=      ((y[21]/volume_so)*(driverso_outflow + driverriver));
 OceanOUT_sonitrate=      ((y[29]/volume_so)*(driverso_outflow + driverriver));
 OceanOUT_sophyt=         ((y[37]/volume_so)*(driverso_outflow + driverriver));

/*_____Ocean boundary outfluxes from deep offshore layer_____ */
 OceanOUT_ddetritus=     ((y[1]/volume_d)*driverd_outflow);
 OceanOUT_dammonia=      ((y[22]/volume_d)*driverd_outflow);
 OceanOUT_dnitrate=      ((y[30]/volume_d)*driverd_outflow);
 OceanOUT_dphyt=         ((y[38]/volume_d)*driverd_outflow);

/*_____Ocean boundary outfluxes from surface inshore layer_____ */
 OceanOUT_sidetritus=     ((y[51]/volume_si)*(driversi_outflow));
 OceanOUT_siammonia=      ((y[52]/volume_si)*(driversi_outflow));
 OceanOUT_sinitrate=      ((y[53]/volume_si)*(driversi_outflow));
 OceanOUT_siphyt=         ((y[54]/volume_si)*(driversi_outflow));


/* _____Fluxes from offshore surface to inshore_____ */
 InshoreIN_sdetritus=      ((y[0]/volume_so)*driverso_si_flow);
 InshoreIN_sammonia=       ((y[21]/volume_so)*driverso_si_flow);
 InshoreIN_snitrate=       ((y[29]/volume_so)*driverso_si_flow);
 InshoreIN_sphyt=          ((y[37]/volume_so)*driverso_si_flow);
 InshoreIN_benthslar=      ((y[41]*prop_benthslar_surfo/volume_so)*driverso_si_flow);
 InshoreIN_benthclar=      ((y[43]*prop_benthclar_surfo/volume_so)*driverso_si_flow);
 InshoreIN_herb=           ((y[39]*prop_herb_surfo/volume_so)*driverso_si_flow);
 InshoreIN_carn=           ((y[40]*prop_carn_surfo/volume_so)*driverso_si_flow);
 InshoreIN_fishplar=       ((y[46]*prop_fishplar_surfo/volume_so)*driverso_si_flow);
 InshoreIN_fishdlar=       ((y[48]*prop_fishdlar_surfo/volume_so)*driverso_si_flow);

/* _____Fluxes from inshore to offshore surface_____ */
/*_____Assumes that the river inflow to the inshore is pushed out o fthe model domain entirely via the offshore zone_____*/
 InshoreOUT_sdetritus=     ((y[51]/volume_si)*(driversi_so_flow + driverriver));
 InshoreOUT_sammonia=      ((y[52]/volume_si)*(driversi_so_flow + driverriver));
 InshoreOUT_snitrate=      ((y[53]/volume_si)*(driversi_so_flow + driverriver));
 InshoreOUT_sphyt=         ((y[54]/volume_si)*(driversi_so_flow + driverriver));
 InshoreOUT_benthslar=     ((y[55]/volume_si)*(driversi_so_flow + driverriver));
 InshoreOUT_benthclar=     ((y[56]/volume_si)*(driversi_so_flow + driverriver));
 InshoreOUT_herb=          ((y[60]/volume_si)*(driversi_so_flow + driverriver));
 InshoreOUT_carn=          ((y[61]/volume_si)*(driversi_so_flow + driverriver));
 InshoreOUT_fishplar=      ((y[62]/volume_si)*(driversi_so_flow + driverriver));
 InshoreOUT_fishdlar=      ((y[63]/volume_si)*(driversi_so_flow + driverriver));


/* _____River influxes_____*/
 Riv_amm_IN=             ((driverboundriv_amm)*driverriver);
 Riv_nit_IN=             ((driverboundriv_nit)*driverriver);
 Riv_det_IN=             ((driverboundriv_det)*driverriver);


/* _____Atmosphere influxes_____*/
 Atm_amm_IN_i=             (driveratm_amm_i)*shallowprop;
 Atm_nit_IN_i=             (driveratm_nit_i)*shallowprop;

 Atm_amm_IN_o=             (driveratm_amm_o)*(1-shallowprop);
 Atm_nit_IN_o=             (driveratm_nit_o)*(1-shallowprop);




/* ____ ACTIVE MIGRATION OF FISH and BIRDS&MAMMALS INTO THE INSHORE ALONG FOOD CONCENTRATION GRADIENTS ____ */


if (food_gradient_pfish  < 0 ) {
    InshoreIN_fishp = (y[45]/(1-shallowprop)) * pfish_migcoef * pow( food_gradient_pfish , 2) ;
}
else {
    InshoreIN_fishp = 0 ;
}


if (food_gradient_dfish  < 0 ) {
    InshoreIN_fishd = (y[47]/(1-shallowprop)) * dfish_migcoef * pow( food_gradient_dfish , 2) ;
}
else {
    InshoreIN_fishd = 0 ;
}


if (food_gradient_mfish  < 0 ) {
    InshoreIN_fishm = (y[49]/(1-shallowprop)) * mfish_migcoef * pow( food_gradient_mfish , 2) ;
}
else {
    InshoreIN_fishm = 0 ;
}


if (food_gradient_bird  < 0 ) {
    InshoreIN_bird =  (y[50]/(1-shallowprop)) * bird_migcoef *  pow( food_gradient_bird  , 2) ;
}
else {
    InshoreIN_bird = 0 ;
}


// NEW <--------------------------------------------------
if (food_gradient_seal  < 0 ) {
    InshoreIN_seal =  (y[68]/(1-shallowprop)) * seal_migcoef *  pow( food_gradient_seal  , 2) ;
}
else {
    InshoreIN_seal = 0 ;
}


// NEW <--------------------------------------------------
if (food_gradient_ceta  < 0 ) {
    InshoreIN_ceta =  (y[70]/(1-shallowprop)) * ceta_migcoef *  pow( food_gradient_ceta  , 2) ;
}
else {
    InshoreIN_ceta = 0 ;
}





/* ____ ACTIVE MIGRATION OF FISH OUT OF THE INSHORE ALONG FOOD CONCENTRATION GRADIENTS ____ */


if (food_gradient_pfish  > 0 ) {
    InshoreOUT_fishp = (y[64]/(shallowprop)) * pfish_migcoef * pow( food_gradient_pfish , 2) ;
}
else {
    InshoreOUT_fishp = 0 ;
}



if (food_gradient_dfish  > 0 ) {
    InshoreOUT_fishd = (y[66]/(shallowprop)) * dfish_migcoef * pow( food_gradient_dfish , 2) ;
}
else {
    InshoreOUT_fishd = 0 ;
}


if (food_gradient_mfish  > 0 ) {
    InshoreOUT_fishm = (y[65]/(shallowprop)) * mfish_migcoef * pow( food_gradient_mfish , 2) ;
}
else {
    InshoreOUT_fishm = 0 ;
}


if (food_gradient_bird > 0 ) {
    InshoreOUT_bird =  (y[67]/(shallowprop)) * bird_migcoef *  pow( food_gradient_bird  , 2) ;
}
else {
    InshoreOUT_bird = 0 ;
}


if (food_gradient_seal > 0 ) {
    InshoreOUT_seal =  (y[69]/(shallowprop)) * seal_migcoef *  pow( food_gradient_seal  , 2) ;
}
else {
    InshoreOUT_seal = 0 ;
}

if (food_gradient_ceta > 0 ) {
    InshoreOUT_ceta =  (y[71]/(shallowprop)) * ceta_migcoef *  pow( food_gradient_ceta  , 2) ;
}
else {
    InshoreOUT_ceta = 0 ;
}


/* _____State variable balance equations_____ */

    // ddetritus_so
    ydot[0]= ( xs_o * y[37] * y[37] )
             + Defec_herb_so
             + Defec_carn_so
	     + Defec_fishplar_so
	     + Defec_fishp_so
	     + Defec_fishdlar_so
	     + Defec_fishd_so  
	     + Defec_fishm_so
	     + Defec_bird_so
	     + Defec_seal_so
	     + Defec_ceta_so
             + Defec_benthslar_so
             + Defec_benthclar_so
	     - ( m_so * y[0] )  
	     - detr_settle_s_d  
	     + Vmix_detritus  
             - InshoreIN_sdetritus
             + InshoreOUT_sdetritus
	     + OceanIN_sodetritus  
	     - OceanOUT_sodetritus 
	     + Upwelling_det  
	     - Upt_detritus_so_herb
             - Upt_detritus_so_benthslar
	     - Upt_detritus_so_benthclar ;

    // ddetritus_si
    ydot[51]= ( xs_i * y[54] * y[54] )
             + Defec_herb_si
             + Defec_carn_si
	     + Defec_fishplar_si  
	     + Defec_fishp_si  
	     + Defec_fishdlar_si  
	     + Defec_fishd_si  
	     + Defec_fishm_si  
	     + Defec_bird_si  
	     + Defec_seal_si  
	     + Defec_ceta_si  
	     + Defec_benthslar_si  
	     + Defec_benthclar_si  
	     - ( m_si * y[51] )  
	     - detr_settle_s_b  
	     + Riv_det_IN  
	     - Upt_detritus_si_herb
	     - Upt_detritus_si_benths_i  
             - Upt_detritus_si_benthslar
	     - Upt_detritus_si_benthclar
             + InshoreIN_sdetritus
             - InshoreOUT_sdetritus
             + OceanIN_sidetritus
             - OceanOUT_sidetritus
             + s_w_det_resuspend_flx_s1
             + s_w_det_resuspend_flx_s2
             + s_w_det_resuspend_flx_s3
             + ( (1-qs_p1) * corp_det_i * y[72] )
             + ( (1-qs_p1) * corp_det_i * y[15] ) * s1_stick_reflect
             + ( (1-qs_p1) * corp_det_i * y[16] ) * s2_stick_reflect
             + ( (1-qs_p1) * corp_det_i * y[17] ) * s3_stick_reflect
             + ( (1-qs_p1) * kelpdebris_det_i * y[76] )
             +( Defec_benths_s0 )
             +( Defec_benths_s1 ) * s1_stick_reflect
             +( Defec_benths_s2 ) * s2_stick_reflect
             +( Defec_benths_s3 ) * s3_stick_reflect
             +( Defec_benthc_s0 )
             +( Defec_benthc_s1 ) * s1_stick_reflect
             +( Defec_benthc_s2 ) * s2_stick_reflect
             +( Defec_benthc_s3 ) * s3_stick_reflect
             + ( xherb_i * ( y[60] * y[60] ) )
             + ( xcarn_i * ( y[61] * y[61] ) )
             + ( xpfishlar_i * ( y[62] * y[62] * y[62] ) )
             + ( xdfishlar_i * ( y[63] * y[63] * y[63] ) )
	     + (xbenthslar_i * ( y[55] * y[55] * y[55] ) )
	     + (xbenthclar_i * ( y[56] * y[56] * y[56] ) ) ;




    // ddetritus_d
    ydot[1]=   Defec_herb_d
	     + Defec_carn_d  
             + Defec_benthslar_d
             + Defec_benthclar_d
	     + Defec_fishplar_d  
	     + Defec_fishp_d  
	     + Defec_fishdlar_d  
	     + Defec_fishd_d  
	     + Defec_fishm_d  
	     + Defec_bird_d
	     + Defec_seal_d
	     + Defec_ceta_d
	     - Upt_detritus_d_benthslar
	     - Upt_detritus_d_benthclar
	     + ( xd * y[38] * y[38] ) 
	     + detr_settle_s_d
	     - detr_settle_d 
	     - Upt_detritus_d_herb 
	     - Upt_detritus_d_benths_o 
	     - ( m_d * y[1] )  
	     - Vmix_detritus  
	     + OceanIN_ddetritus 
	     - Upwelling_det  
	     - OceanOUT_ddetritus
             + s_w_det_resuspend_flx_d1
             + s_w_det_resuspend_flx_d2
             + s_w_det_resuspend_flx_d3
             + ( (1-qs_p1) * corp_det_o * y[73] )
             + ( (1-qs_p1) * corp_det_o * y[18] ) * d1_stick_reflect
             + ( (1-qs_p1) * corp_det_o * y[19] ) * d2_stick_reflect
             + ( (1-qs_p1) * corp_det_o * y[20] ) * d3_stick_reflect
             +( Defec_benths_d0 )
             +( Defec_benths_d1 ) * d1_stick_reflect
             +( Defec_benths_d2 ) * d2_stick_reflect
             +( Defec_benths_d3 ) * d3_stick_reflect
             +( Defec_benthc_d0 )
             +( Defec_benthc_d1 ) * d1_stick_reflect
             +( Defec_benthc_d2 ) * d2_stick_reflect
             +( Defec_benthc_d3 ) * d3_stick_reflect
             + ( xcarn_o * ( y[40] * y[40] ) )
             + ( xherb_o * ( y[39] * y[39] ) )
             + ( xpfishlar_o * ( y[46] * y[46] * y[46] ) )
             + ( xdfishlar_o * ( y[48] * y[48] * y[48] ) )
	     + (xbenthslar_o * ( y[41] * y[41] * y[41] ) )
	     + (xbenthclar_o * ( y[43] * y[43] * y[43] ) ) ;

/* ____________________________________________________________________________________ */


// Sediment detritus ammonia and nitrate updates (not corpses because they stick to rock)


// Area s1
   if(area_s1>0 && rock_s1>0.5) {

    // dx_detritus_s1
   ydot[2]=   (( Defec_benths_s1 + Defec_benthc_s1 + ( (1-qs_p1) * corp_det_i * y[15] )) )
              + (area_s1/nonrock_s)*detr_settle_s_b 
              - (Upt_xdetritus_s1_benths_i )
              - ( msed_s1 * y[2] )
              - s_w_det_resuspend_flx_s1 ;

    // dxR_detritus_s1
     ydot[8]=0;    

    // dx_ammonia_s1
    ydot[23] = ( (1-qs_p1) * msed_s1 * y[2] )
              + (msed_s1 * qs_p2 * y[8])
              - ( nsed_s1 * y[23] )
              - s_w_amm_flx_s1 
              - s_w_amm_disturb_flx_s1 ;

    // dx_nitrate_s1
    ydot[31] =  ( nsed_s1 * y[23] )
              - ( dsed_s1 * y[31] )
              - s_w_nit_flx_s1
              - s_w_nit_disturb_flx_s1 ;

   }
   else {
   ydot[2]=0;
   ydot[8]=0;
   ydot[23]=0;
   ydot[31]=0;
   }

// .................................


// Area s2
   if(area_s2>0 && rock_s2>0.5) {

    // dx_detritus_s2
   ydot[3]=   (( Defec_benths_s2 + Defec_benthc_s2 + ( (1-qs_p1) * corp_det_i * y[16] )) )
              + (area_s2/nonrock_s)*detr_settle_s_b 
              - (Upt_xdetritus_s2_benths_i )
              - ( msed_s2 * y[3] )
              - s_w_det_resuspend_flx_s2 ;

    // dxR_detritus_s2
    ydot[9]=0;

    // dx_ammonia_s2
    ydot[24] = ( (1-qs_p1) * msed_s2 * y[3] )
              + (msed_s2 * qs_p2 * y[9])
              - ( nsed_s2 * y[24] )
              - s_w_amm_flx_s2 
              - s_w_amm_disturb_flx_s2 ;

    // dx_nitrate_s2
    ydot[32] =  ( nsed_s2 * y[24] )
              - ( dsed_s2 * y[32] )
              - s_w_nit_flx_s2
              - s_w_nit_disturb_flx_s2 ;

   }
   else {
   ydot[3]=0;
   ydot[9]=0;
   ydot[23]=0;
   ydot[32]=0;
   }


// .................................


// Area s3
   if(area_s3>0 && rock_s3>0.5) {

    // dx_detritus_s3
   ydot[4]=   ((  Defec_benths_s3 + Defec_benthc_s3 + ( (1-qs_p1) * corp_det_i * y[17] )) )
              + (area_s3/nonrock_s)*detr_settle_s_b 
              - (Upt_xdetritus_s3_benths_i )
              - ( msed_s3 * y[4] )
              - s_w_det_resuspend_flx_s3  ;

    // dxR_detritus_s3
    ydot[10]=0;

    // dx_ammonia_s3
    ydot[25] = ( (1-qs_p1) * msed_s3 * y[4] )
              + (msed_s3 * qs_p2 * y[10])
              - ( nsed_s3 * y[25] )
              - s_w_amm_flx_s3 
              - s_w_amm_disturb_flx_s3 ;

    // dx_nitrate_s3
    ydot[33] =  ( nsed_s3 * y[25] )
              - ( dsed_s3 * y[33] )
              - s_w_nit_flx_s3
              - s_w_nit_disturb_flx_s3 ;

   }
   else {
   ydot[4]=0;
   ydot[10]=0;
   ydot[25]=0;
   ydot[33]=0;
   }

// .................................

// Area d1
   if(area_d1>0 && rock_d1>0.5) {

    // dx_detritus_d1
    ydot[5]=  (( Defec_benths_d1 + Defec_benthc_d1 + ( (1-qs_p1) * corp_det_o * y[18] )) )
              + (area_d1/(nonrock_d))*detr_settle_d
              - (Upt_xdetritus_d1_benths_o )
              - ( msed_d1 * y[5] )
              - s_w_det_resuspend_flx_d1 ;

    // dxR_detritus_d1
    ydot[11]=0;

    // dx_ammonia_d1
    ydot[26] = ( (1-qs_p1) * msed_d1 * y[5] )
              + (msed_d1 * qs_p2 * y[11])
              - ( nsed_d1 * y[26] )
              - s_w_amm_flx_d1
              - s_w_amm_disturb_flx_d1 ;

    // dx_nitrate_d1
    ydot[34] =  ( nsed_d1 * y[26] )
              - ( dsed_d1 * y[34] )
              - s_w_nit_flx_d1
              - s_w_nit_disturb_flx_d1 ;

   }
   else {
   ydot[5]=0;
   ydot[11]=0;
   ydot[26]=0;
   ydot[34]=0;
   }

// .................................

// Area d2
   if(area_d2>0 && rock_d2>0.5) {

    // dx_detritus_d2
    ydot[6]=  (( Defec_benths_d2 + Defec_benthc_d2 + ( (1-qs_p1) * corp_det_o * y[19] )) )
              + (area_d2/(nonrock_d))*detr_settle_d 
              - (Upt_xdetritus_d2_benths_o )
              - ( msed_d2 * y[6] )
              - s_w_det_resuspend_flx_d2  ;

    // dxR_detritus_d2
    ydot[12]=0;

    // dx_ammonia_d2
    ydot[27] = ( (1-qs_p1) * msed_d2 * y[6] )
              + (msed_d2 * qs_p2 * y[12])
              - ( nsed_d2 * y[27] )
              - s_w_amm_flx_d2
              - s_w_amm_disturb_flx_d2 ;

    // dx_nitrate_d2
    ydot[35] =  ( nsed_d2 * y[27] )
              - ( dsed_d2 * y[35] )
              - s_w_nit_flx_d2
              - s_w_nit_disturb_flx_d2 ;

   }
   else {
   ydot[6]=0;
   ydot[12]=0;
   ydot[27]=0;
   ydot[35]=0;
   }

// .................................

// Area d3
   if(area_d3>0 && rock_d3>0.5) {

    // dx_detritus_d3
    ydot[7]=  (( Defec_benths_d3 + Defec_benthc_d3 + ( (1-qs_p1) * corp_det_o * y[20] )) ) 
              + (area_d3/(nonrock_d))*detr_settle_d 
              - (Upt_xdetritus_d3_benths_o )
              - ( msed_d3 * y[7] )
              - s_w_det_resuspend_flx_d3  ;

    // dxR_detritus_d3
    ydot[13]=0;

    // dx_ammonia_d3
    ydot[28] = ( (1-qs_p1) * msed_d3 * y[7] )
              + (msed_d3 * qs_p2 * y[13])
              - ( nsed_d3 * y[28] )
              - s_w_amm_flx_d3
              - s_w_amm_disturb_flx_d3 ;

    // dx_nitrate_d3
    ydot[36] =  ( nsed_d3 * y[28] )
              - ( dsed_d3 * y[36] )
              - s_w_nit_flx_d3
              - s_w_nit_disturb_flx_d3 ;

   }
   else {
   ydot[7]=0;
   ydot[13]=0;
   ydot[28]=0;
   ydot[36]=0;
   }


/* ____________________________________________________________________________________ */


    // ddiscard_o
    ydot[14]=   (Flx_tot_disc_o + Flx_tot_offal_o) 
              - ( disc_corp * y[14] ) 
              - Upt_disc_o_fishd 
              - Upt_disc_o_bird 
              - Upt_disc_o_seal 
              - Upt_disc_o_ceta ;

    // ddiscard_i
    ydot[59]=   (Flx_tot_disc_i + Flx_tot_offal_i) 
              - ( disc_corp * y[59] ) 
              - Upt_disc_i_fishd 
              - Upt_disc_i_bird 
              - Upt_disc_i_seal 
              - Upt_disc_i_ceta ;


//___________________________________________________________________

//  Processing of kelp debris

    if(area_s0>0) {
    // dkelpdebris
    ydot[76] =  + ((driver_S_wave)*wave_kelp_i*y[74]) * (y[75])
                - Upt_kelpdebris_s0_benthc_i
                - kelpdebris_det_i * y[76]
                - (driver_S_wave)*wave_beach_kelpdebris*y[76] 
                + ( Fkpidaily*(twomax(0,(max_exploitable_f_KP*(y[75]-protect_KP_i)))) )  * KPigutting * kelp_G_p_s0 * offal_prop_live_weight 
                + ( Fkpidaily*(twomax(0,(max_exploitable_f_KP*(y[75]-protect_KP_i)))) )  * KPidiscard * kelp_D_p_s0 ;
   }
   else {
   ydot[76]=0;
   }





/* ____________________________________________________________________________________ */

    // dcorpse_s0
    if(area_s0>0) {
    ydot[72] =  p_disc_s0*( disc_corp * y[59] )
              + (plough_daily_s0*area_s0/inshore_plough_daily)  * ( ( bensdamage_i * y[57] ) + ( bencdamage_i * y[58] ) )
              + (area_s0/shallowprop)*( xbenths_i*(y[57]*y[57])) 
              + (area_s0/shallowprop)*( xbenthc_i*(y[58]*y[58])) 
              + (area_s0/shallowprop)*( xpfish_i * ( y[64] * y[64] ) ) 
              + (area_s0/shallowprop)*( xdfish_i * ( y[66] * y[66] ) ) 
              + (area_s0/shallowprop)*( xmfish_i * ( y[65] * y[65] ) ) 
              + (area_s0/shallowprop)*( xbird_i * ( y[67] * y[67] ) )
              + (area_s0/shallowprop)*( xseal_i * ( y[69] * y[69] ) )
              + (area_s0/shallowprop)*( xceta_i * ( y[71] * y[71] ) )
              + ( Fbdidaily*(twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) ) * BDidiscard * bird_D_p_s0
              + ( Fslidaily*(twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) ) * SLidiscard * seal_D_p_s0
              + ( Fctidaily*(twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) ) * CTidiscard * ceta_D_p_s0
              - ( corp_det_i * y[72] ) 
              - Upt_corpse_s0_benthc_i 
              - Upt_corpse_s0_fishd 
              - Upt_corpse_s0_bird 
              - Upt_corpse_s0_seal ;
//              - Upt_corpse_s0_ceta ;
   }
   else {
   ydot[72]=0;
   }



    // dcorpse_s1   - TO BE MOFIFIESD  y[] and then decide what to do about discrad rates
    if(area_s1>0) {
    ydot[15] =  p_disc_s1*( disc_corp * y[59] )
              + (plough_daily_s1*area_s1/inshore_plough_daily)  * ( ( bensdamage_i * y[57] ) + ( bencdamage_i * y[58] ) )
              + (area_s1/shallowprop)*( xbenths_i*(y[57]*y[57])) 
              + (area_s1/shallowprop)*( xbenthc_i*(y[58]*y[58])) 
              + (area_s1/shallowprop)*( xpfish_i * ( y[64] * y[64] ) ) 
              + (area_s1/shallowprop)*( xdfish_i * ( y[66] * y[66] ) ) 
              + (area_s1/shallowprop)*( xmfish_i * ( y[65] * y[65] ) ) 
              + (area_s1/shallowprop)*( xbird_i * ( y[67] * y[67] ) )
              + (area_s1/shallowprop)*( xseal_i * ( y[69] * y[69] ) )
              + (area_s1/shallowprop)*( xceta_i * ( y[71] * y[71] ) )
              + ( Fbdidaily*(twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) ) * BDidiscard * bird_D_p_s1
              + ( Fslidaily*(twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) ) * SLidiscard * seal_D_p_s1
              + ( Fctidaily*(twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) ) * CTidiscard * ceta_D_p_s1
              - ( corp_det_i * y[15] ) 
              - Upt_corpse_s1_benthc_i 
              - Upt_corpse_s1_fishd 
              - Upt_corpse_s1_bird 
              - Upt_corpse_s1_seal ;
//              - Upt_corpse_s1_ceta ;
   }
   else {
   ydot[15]=0;
   }

    // dcorpse_s2   neet o modify y[]
    if(area_s2>0) {
    ydot[16] =  p_disc_s2*( disc_corp * y[59] )
              + (plough_daily_s2*area_s2/inshore_plough_daily)  * ( ( bensdamage_i * y[57] ) + ( bencdamage_i * y[58] ) )
              + (area_s2/shallowprop)*( xbenths_i*(y[57]*y[57])) 
              + (area_s2/shallowprop)*( xbenthc_i*(y[58]*y[58])) 
              + (area_s2/shallowprop)*( xpfish_i * ( y[64] * y[64] ) ) 
              + (area_s2/shallowprop)*( xdfish_i * ( y[66] * y[66] ) ) 
              + (area_s2/shallowprop)*( xmfish_i * ( y[65] * y[65] ) ) 
              + (area_s2/shallowprop)*( xbird_i * ( y[67] * y[67] ) ) 
              + (area_s2/shallowprop)*( xseal_i * ( y[69] * y[69] ) ) 
              + (area_s2/shallowprop)*( xceta_i * ( y[71] * y[71] ) ) 
              + ( Fbdidaily*(twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) ) * BDidiscard * bird_D_p_s2
              + ( Fslidaily*(twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) ) * SLidiscard * seal_D_p_s2
              + ( Fctidaily*(twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) ) * CTidiscard * ceta_D_p_s2
              - ( corp_det_i * y[16] ) 
              - Upt_corpse_s2_benthc_i 
              - Upt_corpse_s2_fishd 
              - Upt_corpse_s2_bird 
              - Upt_corpse_s2_seal ; 
//              - Upt_corpse_s2_ceta ;
   }
   else {
   ydot[16]=0;
   }

    // dcorpse_s3  - need to modify y[] and discard rates
    if(area_s3>0) {
    ydot[17] =  p_disc_s3*( disc_corp * y[59] )
              + (plough_daily_s3*area_s3/inshore_plough_daily)  * ( ( bensdamage_i * y[57] ) + ( bencdamage_i * y[58] ) )
              + (area_s3/shallowprop)*( xbenths_i*(y[57]*y[57])) 
              + (area_s3/shallowprop)*( xbenthc_i*(y[58]*y[58])) 
              + (area_s3/shallowprop)*( xpfish_i * ( y[64] * y[64] ) ) 
              + (area_s3/shallowprop)*( xdfish_i * ( y[66] * y[66] ) ) 
              + (area_s3/shallowprop)*( xmfish_i * ( y[65] * y[65] ) ) 
              + (area_s3/shallowprop)*( xbird_i * ( y[67] * y[67] ) ) 
              + (area_s3/shallowprop)*( xseal_i * ( y[69] * y[69] ) ) 
              + (area_s3/shallowprop)*( xceta_i * ( y[71] * y[71] ) ) 
              + ( Fbdidaily*(twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) ) * BDidiscard * bird_D_p_s3
              + ( Fslidaily*(twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) ) * SLidiscard * seal_D_p_s3
              + ( Fctidaily*(twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) ) * CTidiscard * ceta_D_p_s3
              - ( corp_det_i * y[17] ) 
              - Upt_corpse_s3_benthc_i 
              - Upt_corpse_s3_fishd 
              - Upt_corpse_s3_bird 
              - Upt_corpse_s3_seal ; 
//              - Upt_corpse_s3_ceta ;
   }
   else {
   ydot[17]=0;
   }


    // dcorpse_d0
    if(area_d0>0) {
    ydot[73] =  p_disc_d0*( disc_corp * y[14] ) 
              + (plough_daily_d0*area_d0/offshore_plough_daily)  * ( ( bensdamage_o * y[42] ) + ( bencdamage_o * y[44] ) )
              + (area_d0/(1-shallowprop))*( xbenths_o*(y[42]*y[42])) 
              + (area_d0/(1-shallowprop))*( xbenthc_o*(y[44]*y[44])) 
              + (area_d0/(1-shallowprop))*( xpfish_o * ( y[45] * y[45] ) ) 
              + (area_d0/(1-shallowprop))*( xdfish_o * ( y[47] * y[47] ) ) 
              + (area_d0/(1-shallowprop))*( xmfish_o * ( y[49] * y[49] ) ) 
              + (area_d0/(1-shallowprop))*( xbird_o * ( y[50] * y[50] ) ) 
              + (area_d0/(1-shallowprop))*( xseal_o * ( y[68] * y[68] ) ) 
              + (area_d0/(1-shallowprop))*( xceta_o * ( y[70] * y[70] ) ) 
              + ( Fbdodaily*(twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) ) * BDodiscard * bird_D_p_d0
              + ( Fslodaily*(twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) ) * SLodiscard * seal_D_p_d0
              + ( Fctodaily*(twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) ) * CTodiscard * ceta_D_p_d0
              - ( corp_det_o * y[73] ) 
              - Upt_corpse_d0_benthc_o 
              - Upt_corpse_d0_fishd 
              - Upt_corpse_d0_bird 
              - Upt_corpse_d0_seal ; 
//              - Upt_corpse_d0_ceta ;
   }
   else {
   ydot[73]=0;
   }



    // dcorpse_d1 - need to modify y[] and discards
    if(area_d1>0) {
    ydot[18] =  p_disc_d1*( disc_corp * y[14] ) 
              + (plough_daily_d1*area_d1/offshore_plough_daily)  * ( ( bensdamage_o * y[42] ) + ( bencdamage_o * y[44] ) )
              + (area_d1/(1-shallowprop))*( xbenths_o*(y[42]*y[42])) 
              + (area_d1/(1-shallowprop))*( xbenthc_o*(y[44]*y[44])) 
              + (area_d1/(1-shallowprop))*( xpfish_o * ( y[45] * y[45] ) ) 
              + (area_d1/(1-shallowprop))*( xdfish_o * ( y[47] * y[47] ) ) 
              + (area_d1/(1-shallowprop))*( xmfish_o * ( y[49] * y[49] ) ) 
              + (area_d1/(1-shallowprop))*( xbird_o * ( y[50] * y[50] ) ) 
              + (area_d1/(1-shallowprop))*( xseal_o * ( y[68] * y[68] ) ) 
              + (area_d1/(1-shallowprop))*( xceta_o * ( y[70] * y[70] ) ) 
              + ( Fbdodaily*(twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) ) * BDodiscard * bird_D_p_d1
              + ( Fslodaily*(twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) ) * SLodiscard * seal_D_p_d1
              + ( Fctodaily*(twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) ) * CTodiscard * ceta_D_p_d1
              - ( corp_det_o * y[18] ) 
              - Upt_corpse_d1_benthc_o 
              - Upt_corpse_d1_fishd 
              - Upt_corpse_d1_bird 
              - Upt_corpse_d1_seal ; 
//              - Upt_corpse_d1_ceta ;
   }
   else {
   ydot[18]=0;
   }

    // dcorpse_d2 - need to modify y[] annd diiscards
    if(area_d2>0) {
    ydot[19] =  p_disc_d2*( disc_corp * y[14] ) 
              + (plough_daily_d2*area_d2/offshore_plough_daily)  * ( ( bensdamage_o * y[42] ) + ( bencdamage_o * y[44] ) )
              + (area_d2/(1-shallowprop))*( xbenths_o*(y[42]*y[42])) 
              + (area_d2/(1-shallowprop))*( xbenthc_o*(y[44]*y[44])) 
              + (area_d2/(1-shallowprop))*( xpfish_o * ( y[45] * y[45] ) ) 
              + (area_d2/(1-shallowprop))*( xdfish_o * ( y[47] * y[47] ) ) 
              + (area_d2/(1-shallowprop))*( xmfish_o * ( y[49] * y[49] ) ) 
              + (area_d2/(1-shallowprop))*( xbird_o * ( y[50] * y[50] ) ) 
              + (area_d2/(1-shallowprop))*( xseal_o * ( y[68] * y[68] ) ) 
              + (area_d2/(1-shallowprop))*( xceta_o * ( y[70] * y[70] ) ) 
              + ( Fbdodaily*(twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) ) * BDodiscard * bird_D_p_d2
              + ( Fslodaily*(twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) ) * SLodiscard * seal_D_p_d2
              + ( Fctodaily*(twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) ) * CTodiscard * ceta_D_p_d2
              - ( corp_det_o * y[19] ) 
              - Upt_corpse_d2_benthc_o 
              - Upt_corpse_d2_fishd 
              - Upt_corpse_d2_bird 
              - Upt_corpse_d2_seal ; 
//              - Upt_corpse_d2_ceta ;
   }
   else {
   ydot[19]=0;
   }

    // dcorpse_d3 - need to modify y[] and discards
    if(area_d3>0) {
    ydot[20] =  p_disc_d3*( disc_corp * y[14] ) 
              + (plough_daily_d3*area_d3/offshore_plough_daily)  * ( ( bensdamage_o * y[42] ) + ( bencdamage_o * y[44] ) )
              + (area_d3/(1-shallowprop))*( xbenths_o*(y[42]*y[42])) 
              + (area_d3/(1-shallowprop))*( xbenthc_o*(y[44]*y[44])) 
              + (area_d3/(1-shallowprop))*( xpfish_o * ( y[45] * y[45] ) ) 
              + (area_d3/(1-shallowprop))*( xdfish_o * ( y[47] * y[47] ) ) 
              + (area_d3/(1-shallowprop))*( xmfish_o * ( y[49] * y[49] ) ) 
              + (area_d3/(1-shallowprop))*( xbird_o * ( y[50] * y[50] ) ) 
              + (area_d3/(1-shallowprop))*( xseal_o * ( y[68] * y[68] ) ) 
              + (area_d3/(1-shallowprop))*( xceta_o * ( y[70] * y[70] ) ) 
              + ( Fbdodaily*(twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) ) * BDodiscard * bird_D_p_d3
              + ( Fslodaily*(twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) ) * SLodiscard * seal_D_p_d3
              + ( Fctodaily*(twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) ) * CTodiscard * ceta_D_p_d3
              - ( corp_det_o * y[20] ) 
              - Upt_corpse_d3_benthc_o 
              - Upt_corpse_d3_fishd 
              - Upt_corpse_d3_bird
              - Upt_corpse_d3_seal ;
//              - Upt_corpse_d3_ceta ;
   }
   else {
   ydot[20]=0;
   }



    //________________________________________________________

    // dammonia_so
    ydot[21] =  Excr_herb_so
              + Excr_carn_so 
	      + Excr_benthslar_so
	      + Excr_benthclar_so
              + Excr_fishplar_so 
              + Excr_fishp_so 
              + Excr_fishdlar_so
              + Excr_fishd_so 
              + Excr_fishm_so 
              + Excr_bird_so 
              + Excr_seal_so 
              + Excr_ceta_so 
              + HTLmetabolism_so
              + ( volume_so / ( volume_d + volume_so ) ) * HTLmetabolism_so_d
              - ( n_so * y[21] ) 
              + ( m_so * y[0] ) 
              - Upt_samm_sphyt_o 
              + Vmix_ammonia 
              + Upwelling_amm 
              - InshoreIN_sammonia
              + InshoreOUT_sammonia
              + OceanIN_soammonia 
              - OceanOUT_soammonia
              + Atm_amm_IN_o  ;



    // dammonia_si
    ydot[52] =  Excr_herb_si
              + Excr_carn_si 
	      + Excr_benthslar_si
	      + Excr_benthclar_si 
              + Excr_benths_i 
              + Excr_benthc_i 
              + Excr_fishplar_si 
              + Excr_fishp_si 
              + Excr_fishdlar_si 
	      + Excr_fishd_si 
              + Excr_fishm_si 
              + Excr_bird_si 
              + Excr_seal_si 
              + Excr_ceta_si 
              + HTLmetabolism_si
              - ( n_si * y[52] ) 
              + ( m_si * y[51] ) 
              - Upt_samm_sphyt_i 
              + OceanIN_siammonia 
              - OceanOUT_siammonia
              + InshoreIN_sammonia
              - InshoreOUT_sammonia
              + Riv_amm_IN 
              + Atm_amm_IN_i 
              + s_w_amm_flx_s1 + s_w_amm_flx_s2 + s_w_amm_flx_s3
              + s_w_amm_disturb_flx_s1 + s_w_amm_disturb_flx_s2 + s_w_amm_disturb_flx_s3 ;




    // dd_ammonia
    ydot[22] =  Excr_herb_d 
              + Excr_carn_d 
	      + Excr_benthslar_d
	      + Excr_benthclar_d
              + Excr_benths_o 
              + Excr_benthc_o
              + Excr_fishplar_d 
              + Excr_fishp_d 
              + Excr_fishdlar_d 
              + Excr_fishd_d 
              + Excr_fishm_d 
              + Excr_bird_d 
              + Excr_seal_d 
              + Excr_ceta_d 
              + HTLmetabolism_d
              + ( volume_d / ( volume_d + volume_so ) ) * HTLmetabolism_so_d
              - ( n_d * y[22] ) 
              + ( m_d * y[1] ) 
              - Vmix_ammonia 
              - Upwelling_amm 
              + OceanIN_dammonia 
              - OceanOUT_dammonia
              + s_w_amm_flx_d1 + s_w_amm_flx_d2 + s_w_amm_flx_d3 
              + s_w_amm_disturb_flx_d1 + s_w_amm_disturb_flx_d2 + s_w_amm_disturb_flx_d3  ;

//_________________________________________________________________

	      
    // dnitrate_so
    ydot[29] =  ( n_so * y[21] )
              - ( d_so * y[29] )
              - Upt_snit_sphyt_o
              + Vmix_nitrate
              + Upwelling_nit
              - InshoreIN_snitrate
              + InshoreOUT_snitrate
              + OceanIN_sonitrate
              - OceanOUT_sonitrate
              + Atm_nit_IN_o ;
	      
    // dnitrate_si
    ydot[53] =  ( n_si * y[52] )
              - ( d_si * y[53] )
              - Upt_snit_sphyt_i
              + InshoreIN_snitrate
              - InshoreOUT_snitrate
              + OceanIN_sinitrate
              - OceanOUT_sinitrate
              + Riv_nit_IN 
              + Atm_nit_IN_i 
              + s_w_nit_flx_s1 + s_w_nit_flx_s2 + s_w_nit_flx_s3 
              + s_w_nit_disturb_flx_s1 + s_w_nit_disturb_flx_s2 + s_w_nit_disturb_flx_s3 ;
	      

    // dnitrate_d
    ydot[30] =  ( n_d * y[22] )
              - ( d_d * y[30] )
              - Vmix_nitrate 
              - Upwelling_nit
              + OceanIN_dnitrate
              - OceanOUT_dnitrate
              + s_w_nit_flx_d1 + s_w_nit_flx_d2 + s_w_nit_flx_d3 
              + s_w_nit_disturb_flx_d1 + s_w_nit_disturb_flx_d2 + s_w_nit_disturb_flx_d3  ;
	      
// _________________________________________________________________


    // dkelpC
    if(area_s0>0) {
    ydot[74] =   Upt_C_kelp_i
               - Upt_kelp_s0_benthc_i * (y[74]/y[75])
               - (driver_S_wave*wave_kelp_i*y[74]) * y[74]
               - Exude_kelp_i   
               - ( Fkpidaily*(twomax(0,(max_exploitable_f_KP*(y[75]-protect_KP_i)))) )  * (y[74]/y[75]) ;
   }
   else {
   ydot[74]=0;
   }
// uptake by benthos is in nitrogen units so * CN ratio to get into carbon units

    // dkelpN
    if(area_s0>0) {
    ydot[75] =   Upt_samm_kelp_i
               + Upt_snit_kelp_i
               - Upt_kelp_s0_benthc_i
               - ((driver_S_wave)*wave_kelp_i*y[74]) * (y[75])  
               - ( Fkpidaily*(twomax(0,(max_exploitable_f_KP*(y[75]-protect_KP_i)))) ) ;
   }
   else {
   ydot[75]=0;
   }


//_________________________________________________________________

    // dphyt_so
    ydot[37] = Upt_samm_sphyt_o
              + Upt_snit_sphyt_o
              - ( xs_o * y[37] * y[37]  )
              - Upt_phyt_so_herb
	      - Upt_phyt_so_benthslar
	      - Upt_phyt_so_benthclar
              + Vmix_phyt
              - InshoreIN_sphyt
              + InshoreOUT_sphyt
              + OceanIN_sophyt
              + Upwelling_phyt
              - OceanOUT_sophyt ;

    // dphyt_si
    ydot[54] = Upt_samm_sphyt_i
              + Upt_snit_sphyt_i
              - ( xs_i * y[54] * y[54]  )
              - Upt_phyt_si_herb
	      - Upt_phyt_si_benthslar
	      - Upt_phyt_si_benthclar
              - Upt_phyt_si_benths_i
              + InshoreIN_sphyt
              - InshoreOUT_sphyt
              + OceanIN_siphyt
              - OceanOUT_siphyt ;


// Some print out to screen for diagnostics
// Rprintf("nitrate=%f\n", y[53] );
// Rprintf("ammonia=%f\n", y[52] );
// Rprintf("siphyt=%f\n",  y[54] );
// Rprintf("uptamm=%f\n",  Upt_samm_sphyt_i );
// Rprintf("uptnit=%f\n",  Upt_snit_sphyt_i );
// Rprintf("upt_p_herb=%f\n",  Upt_phyt_si_herb );
// Rprintf("upt_p_bslar=%f\n",  Upt_phyt_si_benthslar );
// Rprintf("upt_p_bclar=%f\n",  Upt_phyt_si_benthclar );
// Rprintf("upt_p_bs=%f\n",  Upt_phyt_si_benths_i );
// Rprintf("dd_loss=%f\n",  xs_i * y[54] * y[54] );
// Rprintf("OFF-IN=%f\n",  InshoreIN_sphyt );
// Rprintf("oceanIN=%f\n",  OceanIN_siphyt );
// Rprintf("IN-OFF=%f\n",  InshoreOUT_sphyt );
// Rprintf("oceanOUT=%f\n",  OceanOUT_siphyt );
// Rprintf("oceanOUT=%f\n",  "++++++++++++++++++" );



	      
    // dphyt_d
    ydot[38] = -Upt_phyt_d_herb
	      - Upt_phyt_d_benthslar
	      - Upt_phyt_d_benthclar
              - Upt_phyt_d_benths_o
              - ( xd * y[38] * y[38] )
              - Vmix_phyt
              + OceanIN_dphyt
              - Upwelling_phyt
              - OceanOUT_dphyt ;
	      
    // dherb_o
    ydot[39] = Assim_herb_o
              - ( eH_o * y[39] )
              - ( xherb_o * ( y[39] * y[39] ) )
              - InshoreIN_herb
              + InshoreOUT_herb
              - Upt_herb_o_carn
              - Upt_herb_o_fishplar
              - Upt_herb_o_fishdlar
              - Upt_herb_o_fishp
              - Upt_herb_o_fishm
//              - Upt_herb_o_bird
              - Upt_herb_o_ceta;

    // dherb_i
    ydot[60] = Assim_herb_i
              - ( eH_i * y[60] )
              - ( xherb_i * ( y[60] * y[60] ) )
              + InshoreIN_herb
              - InshoreOUT_herb
              - Upt_herb_i_carn
              - Upt_herb_i_fishplar
              - Upt_herb_i_fishdlar
              - Upt_herb_i_fishp
              - Upt_herb_i_fishm
//              - Upt_herb_i_bird
              - Upt_herb_i_ceta;


    // dcarn_o
    ydot[40] = Assim_carn_o
              - ( eC_o * y[40] )
              - InshoreIN_carn
              + InshoreOUT_carn
              - ( xcarn_o * ( y[40] * y[40] ) )
              - ( Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) )
              - Upt_carn_o_fishp
              - Upt_carn_o_fishd
              - Upt_carn_o_fishm
              - Upt_carn_o_bird 
              - Upt_carn_o_seal
              - Upt_carn_o_ceta ;

    // dcarn_i
    ydot[61] = Assim_carn_i
              - ( eC_i * y[61] )
              + InshoreIN_carn
              - InshoreOUT_carn
              - ( xcarn_i * ( y[61] * y[61] ) )
              - ( Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) )
              - Upt_carn_i_fishp
              - Upt_carn_i_fishd
              - Upt_carn_i_fishm
              - Upt_carn_i_bird 
              - Upt_carn_i_seal
              - Upt_carn_i_ceta ;




    //dbenthslar_o
    ydot[41]=Assim_benthslar_o
        	-(eBslar_o*y[41])
		-Upt_benthslar_o_herb
			-Upt_benthslar_o_carn
			-Upt_benthslar_o_fishp
			-Upt_benthslar_o_fishplar
			-Upt_benthslar_o_fishdlar
			-Upt_benthslar_o_fishm
		-(xbenthslar_o*(y[41]*y[41]*y[41]))
              - InshoreIN_benthslar
              + InshoreOUT_benthslar
		+(driverbs_sp*y[42]*BS_fec)
		-(driverbs_rec*y[41]);

    //dbenthslar_i
    ydot[55]=Assim_benthslar_i
        	-(eBslar_i*y[55])
		-Upt_benthslar_i_herb
			-Upt_benthslar_i_carn
			-Upt_benthslar_i_fishp
			-Upt_benthslar_i_fishplar
			-Upt_benthslar_i_fishdlar
			-Upt_benthslar_i_fishm
		-(xbenthslar_i*(y[55]*y[55]*y[55]))
              + InshoreIN_benthslar
              - InshoreOUT_benthslar
		+(driverbs_sp*y[57]*BS_fec)
		-(driverbs_rec*y[55]);

	      
    // dbenths_o
    ydot[42] = Assim_benths_o
              - ( eBs_o * y[42] )
              - ( xbenths_o * ( y[42] * y[42] ) )
              - Upt_benths_o_fishd
              - Upt_benths_o_benthc_o
              - Upt_benths_o_bird
              - Upt_benths_o_seal
              - Upt_benths_o_ceta
	      - (driverbs_sp*y[42]*BS_fec)
	      + (driverbs_rec*y[41])
              - ( Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) )
              - ( bensdamage_o * y[42] ) ;

    // dbenths_i
    ydot[57] = Assim_benths_i
              - ( eBs_i * y[57] )
              - ( xbenths_i * ( y[57] * y[57] ) )
              - Upt_benths_i_fishd
              - Upt_benths_i_benthc_i
              - Upt_benths_i_bird
              - Upt_benths_i_seal
              - Upt_benths_i_ceta
	      - (driverbs_sp*y[57]*BS_fec)
	      + (driverbs_rec*y[55])
              - ( Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) )
              - ( bensdamage_i * y[57] ) ;


    //dbenthclar_o
    ydot[43]=Assim_benthclar_o
		-(eBclar_o*y[43])
		-Upt_benthclar_o_herb
			-Upt_benthclar_o_carn
			-Upt_benthclar_o_fishp
			-Upt_benthclar_o_fishplar
			-Upt_benthclar_o_fishdlar
			-Upt_benthclar_o_fishm
		-(xbenthclar_o*(y[43]*y[43]*y[43]))
              - InshoreIN_benthclar
              + InshoreOUT_benthclar
		+(driverbc_sp*y[44]*BC_fec)
		-(driverbc_rec*y[43]);

    //dbenthclar_i
    ydot[56]=Assim_benthclar_i
		-(eBclar_i*y[56])
		-Upt_benthclar_i_herb
			-Upt_benthclar_i_carn
			-Upt_benthclar_i_fishp
			-Upt_benthclar_i_fishplar
			-Upt_benthclar_i_fishdlar
			-Upt_benthclar_i_fishm
		-(xbenthclar_i*(y[56]*y[56]*y[56]))
              + InshoreIN_benthclar
              - InshoreOUT_benthclar
		+(driverbc_sp*y[58]*BC_fec)
		-(driverbc_rec*y[56]);

	      
    // dbenthc_o
    ydot[44] =  Assim_benthc_o
              - ( eBc_o * y[44] )
              - ( xbenthc_o * ( y[44] * y[44] ) )
              - Upt_benthc_o_fishd
              - Upt_benthc_o_bird
              - Upt_benthc_o_seal
              - Upt_benthc_o_ceta
	      - (driverbc_sp*y[44]*BC_fec)
	      + (driverbc_rec*y[43])
              - ( Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) )
              - ( bencdamage_o * y[44] ) ;

    // dbenthc_i
    ydot[58] =  Assim_benthc_i
              - ( eBc_i * y[58] )
              - ( xbenthc_i * ( y[58] * y[58] ) )
              - Upt_benthc_i_fishd
              - Upt_benthc_i_bird
              - Upt_benthc_i_seal
              - Upt_benthc_i_ceta
	      - (driverbc_sp*y[58]*BC_fec)
	      + (driverbc_rec*y[56])
              - ( Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) )
              - ( bencdamage_i * y[58] ) ;



	      
    // dfishp_o
    ydot[45] = Assim_fishp_o
              - ( eFp_o * y[45] )
              - InshoreIN_fishp
              + InshoreOUT_fishp
              - Upt_fishp_o_fishd
              - Upt_fishp_o_bird
              - Upt_fishp_o_seal
              - Upt_fishp_o_ceta
              - ( xpfish_o * ( y[45] * y[45] ) )
              - ( driverpfish_sp * y[45] *PF_fec)
              + ( driverpfish_rec * y[46] )
              - ( Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) );
	      
    // dfishp_i
    ydot[64] = Assim_fishp_i
              - ( eFp_i * y[64] )
              + InshoreIN_fishp
              - InshoreOUT_fishp
              - Upt_fishp_i_fishd
              - Upt_fishp_i_bird
              - Upt_fishp_i_seal
              - Upt_fishp_i_ceta
              - ( xpfish_i * ( y[64] * y[64] ) )
              - ( driverpfish_sp * y[64] *PF_fec )
              + ( driverpfish_rec * y[62] )
              - ( Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) );
	      


    // dfishplar_o
    ydot[46] = Assim_fishplar_o
              - ( eFplar_o * y[46] )
              - InshoreIN_fishplar
              + InshoreOUT_fishplar
              - Upt_fishplar_o_fishp
              - Upt_fishplar_o_fishd
              - Upt_fishplar_o_carn
              - Upt_fishplar_o_fishm
              - ( xpfishlar_o * ( y[46] * y[46] * y[46] ) )
              + ( driverpfish_sp * y[45] *PF_fec )
              - ( driverpfish_rec * y[46] ) ;


    // dfishplar_i
    ydot[62] = Assim_fishplar_i
              - ( eFplar_i * y[62] )
              + InshoreIN_fishplar
              - InshoreOUT_fishplar
              - Upt_fishplar_i_fishp
              - Upt_fishplar_i_fishd
              - Upt_fishplar_i_carn
              - Upt_fishplar_i_fishm
              - ( xpfishlar_i * ( y[62] * y[62] * y[62] ) )
              + ( driverpfish_sp * y[64] *PF_fec )
              - ( driverpfish_rec * y[62] ) ;



	      
    // dfishd_o
    ydot[47] = Assim_fishd_o
              - ( eFd_o * y[47] )
              - Upt_fishd_o_fishd
              - Upt_fishd_o_bird
              - Upt_fishd_o_seal
              - Upt_fishd_o_ceta
              - ( xdfish_o * ( y[47] * y[47] ) )
              - InshoreIN_fishd
              + InshoreOUT_fishd
              - ( driverdfish_sp * y[47] *DF_fec )
              + ( driverdfish_rec * y[48] )
              - ( Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) ) ;
	      
    // dfishd_i
    ydot[66] = Assim_fishd_i
              - ( eFd_i * y[66] )
              - Upt_fishd_i_fishd
              - Upt_fishd_i_bird
              - Upt_fishd_i_seal
              - Upt_fishd_i_ceta
              - ( xdfish_i * ( y[66] * y[66] ) )
              + InshoreIN_fishd
              - InshoreOUT_fishd
              - ( driverdfish_sp * y[66] *DF_fec )
              + ( driverdfish_rec * y[63] )
              - ( Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) ) ;

// Some print out to screen for diagnostics
// Rprintf("assim_fishd_o=%f\n", Assim_fishd_o );
// Rprintf("assim_fishd_i=%f\n", Assim_fishd_i );



    // dfishdlar_o
    ydot[48] = Assim_fishdlar_o
              - ( eFdlar_o * y[48] )
              - InshoreIN_fishdlar
              + InshoreOUT_fishdlar
              - Upt_fishdlar_o_fishp
              - Upt_fishdlar_o_fishd
              - Upt_fishdlar_o_carn
              - Upt_fishdlar_o_fishm
              - ( xdfishlar_o * ( y[48] * y[48] * y[48] ) )
              + ( driverdfish_sp * y[47] *DF_fec )
              - ( driverdfish_rec * y[48] );

    // dfishdlar_i
    ydot[63] = Assim_fishdlar_i
              - ( eFdlar_i * y[63] )
              + InshoreIN_fishdlar
              - InshoreOUT_fishdlar
              - Upt_fishdlar_i_fishp
              - Upt_fishdlar_i_fishd
              - Upt_fishdlar_i_carn
              - Upt_fishdlar_i_fishm
              - ( xdfishlar_i * ( y[63] * y[63] * y[63] ) )
              + ( driverdfish_sp * y[66] *DF_fec )
              - ( driverdfish_rec * y[63] );



    // dfishm_o
    ydot[49] = Assim_fishm_o
              - ( eFm_o * y[49] )
              - InshoreIN_fishm
              + InshoreOUT_fishm
              - Upt_fishm_o_fishd
              - Upt_fishm_o_bird
              - Upt_fishm_o_seal
              - Upt_fishm_o_ceta
              - ( xmfish_o * ( y[49] * y[49] ) )
              - ( drivermfish_em * y[49] )
              + ( drivermfish_em * y[65] )
              + ( drivermfish_im )
              - ( Fmodaily*(twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) ) ;
    // All the external imigration goes into the offshore zone
    // Movememnt inshore-offshore is just a diffusion process
    // Emigration flux from inshore goes into the offshore zone, then out of the model


    // dfishm_i
    ydot[65] = Assim_fishm_i
              - ( eFm_i * y[65] )
              + InshoreIN_fishm
              - InshoreOUT_fishm
              - Upt_fishm_i_fishd
              - Upt_fishm_i_bird
              - Upt_fishm_i_seal
              - Upt_fishm_i_ceta
              - ( xmfish_i * ( y[65] * y[65] ) )
              - ( drivermfish_em * y[65] )
              - ( Fmidaily*(twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) ) ;



// Some print out to screen for diagnostics
// Rprintf("assim_fishm_o=%f\n", Assim_fishm_o );
// Rprintf("assim_fishm_i=%f\n", Assim_fishm_i );
// Rprintf("InshoreIN_fishm=%f\n", InshoreIN_fishm );
// Rprintf("InshoreOUT_fishm=%f\n",  InshoreOUT_fishm );

	      

    // dbird_o
    ydot[50] = Assim_bird_o
              - InshoreIN_bird
              + InshoreOUT_bird
              - Upt_bird_o_seal
              - Upt_bird_o_ceta
              - ( ebird_o * y[50] )
              - ( xbird_o * ( y[50] * y[50] ) )
              - ( Fbdodaily*(twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) ) ;

    // dbird_i
    ydot[67] = Assim_bird_i
              + InshoreIN_bird
              - InshoreOUT_bird
              - Upt_bird_i_seal
              - Upt_bird_i_ceta
              - ( ebird_i * y[67] )
              - ( xbird_i * ( y[67] * y[67] ) )
              - ( Fbdidaily*(twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) ) ;
	      

    // dseal_o
    ydot[68] = Assim_seal_o
              - InshoreIN_seal
              + InshoreOUT_seal
              - Upt_seal_o_ceta
              - ( eseal_o * y[68] )
              - ( xseal_o * ( y[68] * y[68] ) )
              - ( Fslodaily*(twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) ) ;


    // dseal_i
    ydot[69] = Assim_seal_i
              + InshoreIN_seal
              - InshoreOUT_seal
              - Upt_seal_i_ceta
              - ( eseal_i * y[69] )
              - ( xseal_i * ( y[69] * y[69] ) )
              - ( Fslidaily*(twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) ) ;
	      

    // dceta_o
    ydot[70] = Assim_ceta_o
              - InshoreIN_ceta
              + InshoreOUT_ceta
              - ( eceta_o * y[70] )
              - ( xceta_o * ( y[70] * y[70] ) )
              - ( Fctodaily*(twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) ) ;

    // dceta_i
    ydot[71] = Assim_ceta_i
              + InshoreIN_ceta
              - InshoreOUT_ceta
              - ( eceta_i * y[71] )
              - ( xceta_i * ( y[71] * y[71] ) )
              - ( Fctidaily*(twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) ) ;
	      




//    For diagnostics....
//        Rprintf("time=%f, Inshore_bird=%f, Offshore_bird=%f, food_gradient_bird=%f ,InshoreIN_bird=%f, InshoreOUT_bird=%f  -> \n", *t, y[67]/shallowprop, y[50]/(1-shallowprop), food_gradient_bird, InshoreIN_bird, InshoreOUT_bird);


// =========================================================================== 
// =========================================================================== 


/* _____Integrations for the derived variables_____ */


// __________ FIRST OF THESE BECOMES 77 (after 76 which is kelp debris)

// SOME BIOLOGICAL PRODUCTION RATES SEPARATELY FOR INSHORE AND OFFSHORE

    // Net primary production - offshore
    //dnetpprod_o
    ydot[77] = Upt_samm_sphyt_o
	      + Upt_snit_sphyt_o
	      - ( xs_o * y[37] * y[37] );

    // Net primary production - inshore
    //dnetpprod_i
    ydot[78] = Upt_samm_sphyt_i
	      + Upt_snit_sphyt_i
              - ( xs_i * y[54] * y[54] ) ;

    // Offshore net nitrate consumption_production (NNCP) in the surface + deep water column (biological consumption - production)
    //dNNCP_o        
    ydot[79] = - ( n_so * y[21] + n_d * y[22] ) 
               + ( d_so * y[29] + d_d * y[30] ) 
               + Upt_snit_sphyt_o ;

    // Inshore net nitrate consumption_production (NNCP) in the water column (biological consumption - production)
    //dNNCP_i        
    ydot[80] = - ( n_si * y[52] )
               + ( d_si * y[53] )
               + Upt_snit_sphyt_i ;

    // DIN uptake ie gross primary production - offshore
    //dphytgrossprod_o             
    ydot[81] = Upt_samm_sphyt_o + Upt_snit_sphyt_o ;

    // DIN uptake ie gross primary production - inshore
    //dphytgrossprod_i             
    ydot[82] = Upt_samm_sphyt_i + Upt_snit_sphyt_i;


    // kelp carbon uptake
    //dkelpCprod_i
    ydot[83] = Upt_C_kelp_i;

    // kelp carbon exutate
    //dkelpCexud_i
    ydot[84] = Exude_kelp_i;

    // kelp nitrogen uptake
    //dkelpNprod_i
    ydot[85] = Upt_samm_kelp_i+Upt_snit_kelp_i;





    // Omnivorous zooplankton gross production - offshore
    //dherbgrossprod_o
    ydot[86] = Assim_herb_o ;

    // Omnivorous zooplankton gross production - inshore
    //dherbgrossprod_i
    ydot[87] = Assim_herb_i ;

    // Carniv zooplankton gross production - offshore
    //dcarngrossprod_o              
    ydot[88] = Assim_carn_o ;

    // Carniv zooplankton gross production - inshore
    //dcarngrossprod_i              
    ydot[89] = Assim_carn_i;


    // Pelfish larvae gross production - offshore
    //dpfishlargrossprod_o          
    ydot[90] = Assim_fishplar_o  ;

    // Pelfish larvae gross production - inshore
    //dpfishlargrossprod_i          
    ydot[91] = Assim_fishplar_i ;


    // Demfish larvae gross production - offshore
    //ddfishlargrossprod_o          
    ydot[92] = Assim_fishdlar_o  ;

    // Demfish larvae gross production - inshore
    //ddfishlargrossprod_i          
    ydot[93] =  Assim_fishdlar_i ;

    // Pelfish gross production - offshore
    //dpfishgrossprod_o             
    ydot[94] = Assim_fishp_o ;

    // Pelfish gross production - inshore
    //dpfishgrossprod_i             
    ydot[95] = Assim_fishp_i ;

    // Migfish gross production - offshore
    //dmfishgrossprod_o 
    ydot[96] = Assim_fishm_o ;

    // Migfish gross production - inshore
    //dmfishgrossprod_i 
    ydot[97] = Assim_fishm_i ;

    // Demfish gross production - offshore
    //ddfishgrossprod_o             
    ydot[98] = Assim_fishd_o  ;

    // Demfish gross production - inshore
    //ddfishgrossprod_i             
    ydot[99] = Assim_fishd_i ;

    // Benths larvae gross production - offshore
    //dbenthslargrossprod_o
    ydot[100]=  Assim_benthslar_o;

    // Benths larvae gross production - inshore
    //dbenthslargrossprod_i
    ydot[101]=Assim_benthslar_i ;

    // Benthc larvae gross production - offshore
    //dbenthclargrossprod_o
    ydot[102]=Assim_benthclar_o;

    // Benthc larvae gross production - inshore
    //dbenthclargrossprod_i
    ydot[103]=Assim_benthclar_i ;

    // Benths gross production - offshore
    //dbenthsgrossprod_o  
    ydot[104] = Assim_benths_o ;

    // Benths gross production - inshore
    //dbenthsgrossprod_i  
    ydot[105] = Assim_benths_i ;

    // Benthc gross production - offshore
    //dbenthcgrossprod_o           
    ydot[106] = Assim_benthc_o ;

    // Benthc gross production - inshore
    //dbenthcgrossprod_i            
    ydot[107] = Assim_benthc_i ;

    // Bird gross production - offshore
    //dbirdgrossprod_o              
    ydot[108] = Assim_bird_o ;

    // Bird gross production - inshore
    //dbirdgrossprod_i              
    ydot[109] = Assim_bird_i ;


    // Seal gross production - offshore
    //dsealgrossprod_o              
    ydot[110] = Assim_seal_o ;

    // Seal gross production - inshore
    //dsealgrossprod_i              
    ydot[111] = Assim_seal_i ;

    // Cetacean gross production - offshore
    //dcetagrossprod_o              
    ydot[112] = Assim_ceta_o ;

    // Cetacean gross production - inshore
    //dcetagrossprod_i              
    ydot[113] = Assim_ceta_i ;


    //dwcdenitrif_o            
    ydot[114] = ( d_so * y[29] ) + ( d_d * y[30] ) ;

    //dwcdenitrif_i            
    ydot[115] = d_si * y[53]  ;

    //dseddenitrif_o           
    ydot[116] =  (dsed_d1 * y[34])  + (dsed_d2 * y[35])  + (dsed_d3 * y[36])  ;

    //dseddenitrif_i           
    ydot[117] = (dsed_s1 * y[31]) + (dsed_s2 * y[32]) + (dsed_s3 * y[33])   ;


// .......................................... 

// WHOLE DOMAIN - ALL THE FLUXES NEEDED TO CREATE THE FULL FLOW MATRIX
// .......................................... 


    //dfluxsedamm_wcamm
    ydot[118] =     s_w_amm_flx_s1 + s_w_amm_flx_s2 + s_w_amm_flx_s3 + s_w_amm_flx_d1 + s_w_amm_flx_d2 + s_w_amm_flx_d3
               + s_w_amm_disturb_flx_s1 + s_w_amm_disturb_flx_s2 + s_w_amm_disturb_flx_s3 + s_w_amm_disturb_flx_d1 + s_w_amm_disturb_flx_d2 + s_w_amm_disturb_flx_d3 ;


    //dfluxwcdet_wcamm
    ydot[119] = ( m_so * y[0] ) + ( m_si * y[51] )+ ( m_d * y[1] ) ;


    //dfluxherb_wcamm
    ydot[120] =   Excr_herb_so
               + Excr_herb_si
               + Excr_herb_d
               + ( eH_o * y[39] ) 
               + ( eH_i * y[60] ) ;

    //dfluxcarn_wcamm
    ydot[121] =     Excr_carn_so
               + Excr_carn_si
               + Excr_carn_d
               + ( eC_o * y[40] ) 
               + ( eC_i * y[61] ) ;

    //dfluxpfishlar_wcamm
    ydot[122]  =    Excr_fishplar_so
               + Excr_fishplar_si
               + Excr_fishplar_d
               + ( eFplar_o * y[46] ) 
               + ( eFplar_i * y[62] ) ;


    //dfluxdfishlar_wcamm
    ydot[123]  =    Excr_fishdlar_so
               + Excr_fishdlar_si
               + Excr_fishdlar_d
               + ( eFdlar_o * y[48] ) 
               + ( eFdlar_i * y[63] ) ;

    //dfluxpfish_wcamm
    ydot[124] =    Excr_fishp_so
               + Excr_fishp_si
               + Excr_fishp_d
               + ( eFp_o * y[45] ) 
               + ( eFp_i * y[64] ) ;


    //dfluxmfish_wcamm
    ydot[125]  =    Excr_fishm_so
               + Excr_fishm_si
               + Excr_fishm_d
               + ( eFm_o * y[49] )
               + ( eFm_i * y[65] ) ;


    //dfluxdfish_wcamm
    ydot[126]  =    Excr_fishd_so
               + Excr_fishd_si
               + Excr_fishd_d
               + ( eFd_o * y[47] )
               + ( eFd_i * y[66] ) ;

    //dfluxbenthslar_wcamm
    ydot[127]  = 	 Excr_benthslar_so
	       + Excr_benthslar_si
	       + Excr_benthslar_d
	       + (eBslar_o*y[41])
	       + (eBslar_i*y[55]) ;


    //dfluxbenthclar_wcamm
    ydot[128]  =    Excr_benthclar_so
	       + Excr_benthclar_si
	       + Excr_benthclar_d
	       + (eBclar_o*y[43])
	       + (eBclar_i*y[56]) ;


    //dfluxbenths_wcamm            - benthos excretion goes straight to the water column not to porewater
    ydot[129] =     Excr_benths_i
               + Excr_benths_o
               + ( eBs_o * y[42] )
               + ( eBs_i * y[57] ) ;

    //dfluxbenthc_wcamm            - benthos excretion goes straight to the water column not to porewater
    ydot[130] =     Excr_benthc_i
               + Excr_benthc_o
               + ( eBc_o * y[44] ) 
               + ( eBc_i * y[58] ) ;


    //dfluxbird_wcamm
    ydot[131]  =    Excr_bird_so
               + Excr_bird_si
               + Excr_bird_d
               + ( ebird_o * y[50] )
               + ( ebird_i * y[67] ) ;

    //dfluxseal_wcamm 
    ydot[132]  =    Excr_seal_so
               + Excr_seal_si
               + Excr_seal_d
               + ( eseal_o * y[68] )
               + ( eseal_i * y[69] ) ;


    //dfluxceta_wcamm  
    ydot[133]  =    Excr_ceta_so
               + Excr_ceta_si
               + Excr_ceta_d
               + ( eceta_o * y[70] )
               + ( eceta_i * y[71] ) ;




// .......................................... 



    //dfluxxdet_sedamm
    ydot[134]  =  (msed_s1 * y[2]) + (msed_s2 * y[3]) + (msed_s3 * y[4]) 
             + (msed_d1 * y[5]) + (msed_d2 * y[6]) + (msed_d3 * y[7]) ;



    //dfluxxRdet_sedamm
    ydot[135]  =   (msed_s1 * qs_p2 * y[8]) + (msed_s2 * qs_p2 * y[9]) + (msed_s3 * qs_p2 * y[10])
              + (msed_d1 * qs_p2 * y[11]) + (msed_d2 * qs_p2 * y[12]) + (msed_d3 * qs_p2 * y[13]);


// .......................................... 

    //dfluxwcamm_wcnit
    ydot[136] = ( n_so * y[21] ) + ( n_si * y[52] )+ ( n_d * y[22] ) ;


    //dfluxsednit_wcnit
    ydot[137] = s_w_nit_flx_s1  + s_w_nit_flx_s2  + s_w_nit_flx_s3 + s_w_nit_flx_d1 + s_w_nit_flx_d2 + s_w_nit_flx_d3
               + s_w_nit_disturb_flx_s1 + s_w_nit_disturb_flx_s2 + s_w_nit_disturb_flx_s3 + s_w_nit_disturb_flx_d1 + s_w_nit_disturb_flx_d2 + s_w_nit_disturb_flx_d3 ;

// .......................................... 

    //dfluxsedamm_sednit             
    ydot[138]= (nsed_s1 * y[23]) +  (nsed_s2 * y[24]) + (nsed_s3 * y[25])  + (nsed_d1 * y[26]) + (nsed_d2 * y[27]) + (nsed_d3 * y[28])  ;

    
// .......................................... 


    //dfluxxdet_wcdet
    ydot[139]  =  s_w_det_resuspend_flx_s1
             + s_w_det_resuspend_flx_s2
             + s_w_det_resuspend_flx_s3
             + s_w_det_resuspend_flx_d1
             + s_w_det_resuspend_flx_d2
             + s_w_det_resuspend_flx_d3 ;



    // kelp kelpdebris to suspended detritus
    //dfluxkelpdebris_wcdet
    ydot[140] = ( (1-qs_p1) * kelpdebris_det_i * y[76] );



    //dfluxcorp_wcdet   - only over rocky seabed habitat
    ydot[141]  =    ( (1-qs_p1) * corp_det_i * y[72] )
               + ( (1-qs_p1) * corp_det_i * y[15] ) * s1_stick_reflect
               + ( (1-qs_p1) * corp_det_i * y[16] ) * s2_stick_reflect
               + ( (1-qs_p1) * corp_det_i * y[17] ) * s3_stick_reflect
               + ( (1-qs_p1) * corp_det_o * y[73] )
               + ( (1-qs_p1) * corp_det_o * y[18] ) * d1_stick_reflect
               + ( (1-qs_p1) * corp_det_o * y[19] ) * d2_stick_reflect
               + ( (1-qs_p1) * corp_det_o * y[20] ) * d3_stick_reflect ;


    //dfluxphyt_wcdet
    ydot[142]  =    ( xs_o * y[37] * y[37]  )
               + ( xs_i * y[54] * y[54]  )
               + ( xd * y[38] * y[38] ) ;


    //dfluxherb_wcdet
    ydot[143]  =  Defec_herb_so
               + Defec_herb_si
               + Defec_herb_d
               + ( xherb_o * ( y[39] * y[39] ) )
               + ( xherb_i * ( y[60] * y[60] ) ) ;

    //dfluxcarn_wcdet
    ydot[144] =     Defec_carn_so
               + Defec_carn_si
               + Defec_carn_d
               + ( xcarn_o * ( y[40] * y[40] ) )
               + ( xcarn_i * ( y[61] * y[61] ) ) ;

    //dfluxpfishlar_wcdet
    ydot[145]  =    Defec_fishplar_so
               + Defec_fishplar_si
               + Defec_fishplar_d
               + ( xpfishlar_o * ( y[46] * y[46] * y[46] ) )
               + ( xpfishlar_i * ( y[62] * y[62] * y[62] ) ) ;

    //dfluxdfishlar_wcdet
    ydot[146]  =    Defec_fishdlar_so
               + Defec_fishdlar_si
               + Defec_fishdlar_d
               + ( xdfishlar_o * ( y[48] * y[48] * y[48] ) )
               + ( xdfishlar_i * ( y[63] * y[63] * y[63] ) ) ;

    //dfluxpfish_wcdet
    ydot[147]  =    Defec_fishp_so
               + Defec_fishp_si
               + Defec_fishp_d ;

    //dfluxmfish_wcdet
    ydot[148]  =    Defec_fishm_so
               + Defec_fishm_si
               + Defec_fishm_d ;
  
    //dfluxdfish_wcdet
    ydot[149]  =    Defec_fishd_so
               + Defec_fishd_si
               + Defec_fishd_d ;

    //dfluxbenthslar_wcdet
    ydot[150]  =    Defec_benthslar_so
	       + Defec_benthslar_si
	       + Defec_benthslar_d
	       +(xbenthslar_o*(y[41]*y[41]*y[41]))
	       +(xbenthslar_i*(y[55]*y[55]*y[55])) ;

    //dfluxbenthclar_wcdet
    ydot[151] =   Defec_benthclar_so
	       + Defec_benthclar_si
	       + Defec_benthclar_d
	       +(xbenthclar_o*(y[43]*y[43]*y[43]))
	       +(xbenthclar_i*(y[56]*y[56]*y[56])) ;

    //dfluxbenths_wcdet - only over rocky habitats 
    ydot[152]  =   ( Defec_benths_s0 )
               +( Defec_benths_s1 ) * s1_stick_reflect
               +( Defec_benths_s2 ) * s2_stick_reflect
               +( Defec_benths_s3 ) * s3_stick_reflect
               +( Defec_benths_d0 )
               +( Defec_benths_d1 ) * d1_stick_reflect
               +( Defec_benths_d2 ) * d2_stick_reflect
               +( Defec_benths_d3 ) * d3_stick_reflect ;

    //dfluxbenthc_wcdet - only over rocky habitats 
    ydot[153]  =   ( Defec_benthc_s0 )
               +( Defec_benthc_s1 ) * s1_stick_reflect
               +( Defec_benthc_s2 ) * s2_stick_reflect
               +( Defec_benthc_s3 ) * s3_stick_reflect
               +( Defec_benthc_d0 )
               +( Defec_benthc_d1 ) * d1_stick_reflect
               +( Defec_benthc_d2 ) * d2_stick_reflect
               +( Defec_benthc_d3 ) * d3_stick_reflect  ;
  
    //dfluxbird_wcdet
    ydot[154]  =    Defec_bird_so
               + Defec_bird_si
               + Defec_bird_d ;

// NEW
    //dfluxseal_wcdet
    ydot[155]  =    Defec_seal_so
               + Defec_seal_si
               + Defec_seal_d ;

// NEW
    //dfluxceta_wcdet
    ydot[156]  =    Defec_ceta_so
               + Defec_ceta_si
               + Defec_ceta_d ;



// .......................................... 


    //dfluxwcdet_xdet
    ydot[157]  =    detr_settle_d
               + detr_settle_s_b ;


    //dfluxcorp_xdet   - only over muddy and sandy habitats
    ydot[158]  =    ( (1-qs_p1) * corp_det_i * y[15] ) * (1 - s1_stick_reflect)
               + ( (1-qs_p1) * corp_det_i * y[16] ) * (1 - s2_stick_reflect)
               + ( (1-qs_p1) * corp_det_i * y[17] ) * (1 - s3_stick_reflect)
               + ( (1-qs_p1) * corp_det_o * y[18] ) * (1 - d1_stick_reflect)
               + ( (1-qs_p1) * corp_det_o * y[19] ) * (1 - d2_stick_reflect)
               + ( (1-qs_p1) * corp_det_o * y[20] ) * (1 - d3_stick_reflect) ;


    //dfluxbenths_xdet   - only over muddy and sandy habitats
    ydot[159]  =   ( Defec_benths_s1 ) * (1 - s1_stick_reflect)
               +( Defec_benths_s2 ) * (1 - s2_stick_reflect)
               +( Defec_benths_s3 ) * (1 - s3_stick_reflect)
               +( Defec_benths_d1 ) * (1 - d1_stick_reflect)
               +( Defec_benths_d2 ) * (1 - d2_stick_reflect)
               +( Defec_benths_d3 ) * (1 - d3_stick_reflect) ;

   //dfluxbenthc_xdet   - only over muddy and sandy habitats
    ydot[160]  =   ( Defec_benthc_s1 ) * (1 - s1_stick_reflect)
               +( Defec_benthc_s2 ) * (1 - s2_stick_reflect)
               +( Defec_benthc_s3 ) * (1 - s3_stick_reflect)
               +( Defec_benthc_d1 ) * (1 - d1_stick_reflect)
               +( Defec_benthc_d2 ) * (1 - d2_stick_reflect)
               +( Defec_benthc_d3 ) * (1 - d3_stick_reflect) ;
 
// .......................................... 

   //dfluxxdet_xRdet
   ydot[161]   =   (msed_s1 * (qs_p1 * y[2]) )
              + (msed_s2 * (qs_p1 * y[3]) )
              + (msed_s3 * (qs_p1 * y[4]) )
              + (msed_d1 * (qs_p1 * y[5]) )
              + (msed_d2 * (qs_p1 * y[6]) )
              + (msed_d3 * (qs_p1 * y[7]) ) ;


     //dfluxkelpdebris_xRdet
     ydot[162] = ( (qs_p1) * kelpdebris_det_i * y[76] );



    //dfluxcorp_xRdet
    ydot[163]  =    qs_p1 * ( corp_det_i * y[72] )
               + qs_p1 * ( corp_det_i * y[15] )
               + qs_p1 * ( corp_det_i * y[16] )
               + qs_p1 * ( corp_det_i * y[17] )
               + qs_p1 * ( corp_det_o * y[73] )
               + qs_p1 * ( corp_det_o * y[18] )
               + qs_p1 * ( corp_det_o * y[19] )
               + qs_p1 * ( corp_det_o * y[20] ) ;



// .......................................... 


    // flux kelp to debris due to wave action
    //dfluxkelp_kelpdebris
    ydot[164] = ((driver_S_wave)*wave_kelp_i*y[74]) * (y[75]);


    //dfluxdisc_corp
    ydot[165] = disc_corp * y[14] + disc_corp * y[59] ;


    //dflux_pfish_corp
    ydot[166]   =  xpfish_o * ( y[45] * y[45] )
               + xpfish_i * ( y[64] * y[64] ) ;


    //dflux_mfish_corp
    ydot[167]   = xmfish_o * ( y[49] * y[49] )
               + xmfish_i * ( y[65] * y[65] ) ;



    //dflux_dfish_corp
    ydot[168]   = xdfish_o * ( y[47] * y[47] )
               + xdfish_i * ( y[66] * y[66] ) ;



    //dflux_benths_corp
    ydot[169]   = xbenths_o * ( y[42] * y[42] )
               + xbenths_i * ( y[57] * y[57] )
               + ( bensdamage_i * y[57] ) + ( bensdamage_o * y[42] ) ;

    //dflux_benthc_corp
    ydot[170]   = xbenthc_o * ( y[44] * y[44] )
               + xbenthc_i * ( y[58] * y[58] )
               + ( bencdamage_i * y[58] ) + ( bencdamage_o * y[44] ) ;

    //dflux_bird_corp
    ydot[171]   = xbird_o * ( y[50] * y[50] ) 
             + xbird_i * ( y[67] * y[67] ) 
             + Fbdodaily * (twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) * ( BDodiscard )  
             + Fbdidaily * (twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) * ( BDidiscard );  

    //dflux_seal_corp
    ydot[172]   = xseal_o * ( y[68] * y[68] ) 
             + xseal_i * ( y[69] * y[69] ) 
             + Fslodaily * (twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) * ( SLodiscard )  
             + Fslidaily * (twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) * ( SLidiscard );  


    //dflux_ceta_corp
    ydot[173]   = xceta_o * ( y[70] * y[70] ) 
             + xceta_i * ( y[71] * y[71] ) 
             + Fctodaily * (twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) * ( CTodiscard )  
             + Fctidaily * (twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) * ( CTidiscard );  


// .......................................... 


    // kelp ammonia uptake
    //dfluxwcamm_kelp
    ydot[174] = Upt_samm_kelp_i;

    // kelp nitrate uptake
    //dfluxwcnit_kelp
    ydot[175] = Upt_snit_kelp_i;



// .......................................... 


    //Ammonia uptake by offshore phytoplankton
    //dfluxwcamm_phyt_o
    ydot[176] = Upt_samm_sphyt_o ;

    //Ammonia uptake by inshore phytoplankton
    //dfluxwcamm_phyt_i
    ydot[177] = Upt_samm_sphyt_i ;



    //Nitrate uptake by offshore phytoplankton
    //dfluxwcnit_phyt_o              
    ydot[178] = Upt_snit_sphyt_o ;

    //Nitrate uptake by inshore phytoplankton
    //dfluxwcnit_phyt_i              
    ydot[179] = Upt_snit_sphyt_i ;

// .......................................... 

    //dfluxwcdet_herb
    ydot[180] = Upt_detritus_so_herb + Upt_detritus_si_herb + Upt_detritus_d_herb ;

    //dfluxphyt_herb
    ydot[181] = Upt_phyt_so_herb + Upt_phyt_si_herb + Upt_phyt_d_herb ;

    //dfluxbenthslar_herb
    ydot[182]= Upt_benthslar_o_herb + Upt_benthslar_i_herb;

    //dfluxbenthclar_herb
    ydot[183]=Upt_benthclar_o_herb + Upt_benthclar_i_herb;
    
// .......................................... 
    
    //dfluxherb_carn         
    ydot[184] = Upt_herb_o_carn + Upt_herb_i_carn ;

    //dfluxpfishlar_carn     
    ydot[185] = Upt_fishplar_o_carn + Upt_fishplar_i_carn;

    //dfluxdfishlar_carn     
    ydot[186] = Upt_fishdlar_o_carn + Upt_fishdlar_i_carn;

    //dfluxbenslar_carn
    ydot[187]= Upt_benthslar_o_carn + Upt_benthslar_i_carn;

    //dfluxbenclar_carn
    ydot[188]= Upt_benthclar_o_carn + Upt_benthclar_i_carn;

// .......................................... 

    //dfluxherb_pfishlar     
    ydot[189] = Upt_herb_o_fishplar + Upt_herb_i_fishplar;
    
    //dfluxbenslar_pfishlar
    ydot[190]= Upt_benthslar_o_fishplar + Upt_benthslar_i_fishplar;

    //dfluxbenclar_pfishlar
    ydot[191]= Upt_benthclar_o_fishplar + Upt_benthclar_i_fishplar;

// .......................................... 

    //dfluxherb_dfishlar     
    ydot[192] = Upt_herb_o_fishdlar + Upt_herb_i_fishdlar ;

    //dfluxbenslar_dfishlar
    ydot[193]=Upt_benthslar_o_fishdlar + Upt_benthslar_i_fishdlar;

    //dfluxbenclar_dfishlar
    ydot[194]= Upt_benthclar_o_fishdlar + Upt_benthclar_i_fishdlar;

// .......................................... 

    //dfluxherb_pfish        
    ydot[195] = Upt_herb_o_fishp + Upt_herb_i_fishp ;

    //dfluxcarn_pfish        
    ydot[196] = Upt_carn_o_fishp + Upt_carn_i_fishp;

    //dfluxpfishlar_pfish    
    ydot[197] = Upt_fishplar_o_fishp + Upt_fishplar_i_fishp;

    //dfluxdfishlar_pfish   
    ydot[198] = Upt_fishdlar_o_fishp + Upt_fishdlar_i_fishp ;

    //dfluxbenslar_pfish
    ydot[199]= Upt_benthslar_o_fishp + Upt_benthslar_i_fishp;

    //dfluxbenclar_pfish
    ydot[200]= Upt_benthclar_o_fishp + Upt_benthclar_i_fishp;

// .......................................... 
    
    //dfluxherb_mfish
    ydot[201] = Upt_herb_o_fishm + Upt_herb_i_fishm;

    //dfluxcarn_mfish
    ydot[202] = Upt_carn_o_fishm + Upt_carn_i_fishm;

    //dfluxpfishlar_mfish
    ydot[203] = Upt_fishplar_o_fishm + Upt_fishplar_i_fishm;

    //dfluxdfishlar_mfish
    ydot[204] = Upt_fishdlar_o_fishm + Upt_fishdlar_i_fishm;

    //dfluxbenthslar_mfish
    ydot[205] = Upt_benthslar_o_fishm + Upt_benthslar_i_fishm;

    //dfluxbenthclar_mfish
    ydot[206] = Upt_benthclar_o_fishm + Upt_benthclar_i_fishm;


// .......................................... 


    //dfluxcorp_dfish        
    ydot[207] =  Upt_corpse_o_fishd + Upt_corpse_i_fishd;

    //dfluxdisc_dfish        
    ydot[208] = Upt_disc_o_fishd + Upt_disc_i_fishd ;

    //dfluxcarn_dfish        
    ydot[209] = Upt_carn_o_fishd + Upt_carn_i_fishd;

    //dfluxpfishlar_dfish    
    ydot[210] = Upt_fishplar_o_fishd + Upt_fishplar_i_fishd ;

    //dfluxdfishlar_dfish    
    ydot[211] = Upt_fishdlar_o_fishd + Upt_fishdlar_i_fishd;

    //dfluxpfish_dfish       
    ydot[212] = Upt_fishp_o_fishd + Upt_fishp_i_fishd;

    //dfluxmfish_dfish
    ydot[213] = Upt_fishm_o_fishd + Upt_fishm_i_fishd;       

    //dfluxdfish_dfish       
    ydot[214] = Upt_fishd_o_fishd + Upt_fishd_i_fishd;

    //dfluxbens_dfish        
    ydot[215] = Upt_benths_o_fishd + Upt_benths_i_fishd ;

    //dfluxbenc_dfish 
    ydot[216] = Upt_benthc_o_fishd + Upt_benthc_i_fishd ;



// .......................................... 

    //dfluxwcdet_benthslar
    ydot[217] = Upt_detritus_so_benthslar + Upt_detritus_si_benthslar + Upt_detritus_d_benthslar ;


    //dfluxphyt_benthslar
    ydot[218] = Upt_phyt_so_benthslar + Upt_phyt_si_benthslar + Upt_phyt_d_benthslar ;


// .......................................... 

    //dfluxwcdet_benthclar
    ydot[219] = Upt_detritus_so_benthclar + Upt_detritus_si_benthclar + Upt_detritus_d_benthclar ;


    //dfluxphyt_benthclar
    ydot[220] = Upt_phyt_so_benthclar + Upt_phyt_si_benthclar + Upt_phyt_d_benthclar ;


// .......................................... 


    //dfluxwcdet_benths
    ydot[221] = Upt_detritus_d_benths_o + Upt_detritus_si_benths_i ;

    //dfluxxdet_benths
    ydot[222] =   Upt_xdetritus_s1_benths_i
                + Upt_xdetritus_s2_benths_i
                + Upt_xdetritus_s3_benths_i
                + Upt_xdetritus_d1_benths_o
                + Upt_xdetritus_d2_benths_o
                + Upt_xdetritus_d3_benths_o ;

    //dfluxxRdet_benths
    ydot[223] =   qs_p3*Upt_xRdetritus_s1_benths_i
                + qs_p3*Upt_xRdetritus_s2_benths_i
                + qs_p3*Upt_xRdetritus_s3_benths_i
                + qs_p3*Upt_xRdetritus_d1_benths_o
                + qs_p3*Upt_xRdetritus_d2_benths_o
                + qs_p3*Upt_xRdetritus_d3_benths_o ;

    //dfluxphyt_benths
    ydot[224] = Upt_phyt_d_benths_o + Upt_phyt_si_benths_i ;


// .......................................... 


    //dfluxkelp_benthc
    ydot[225] = Upt_kelp_s0_benthc_i;


    //dfluxkelpdebris_benthc
    ydot[226] = Upt_kelpdebris_s0_benthc_i;



    //dfluxcorp_benthc       
    ydot[227] =  Upt_corpse_s0_benthc_i
               + Upt_corpse_s1_benthc_i
               + Upt_corpse_s2_benthc_i
               + Upt_corpse_s3_benthc_i
               + Upt_corpse_d0_benthc_o
               + Upt_corpse_d1_benthc_o
               + Upt_corpse_d2_benthc_o
               + Upt_corpse_d3_benthc_o ;

    //dfluxbens_benthc         
    ydot[228] = Upt_benths_o_benthc_o + Upt_benths_i_benthc_i ;


// .......................................... 

    //dfluxcorp_bird         
    ydot[229] =  Upt_corpse_o_bird + Upt_corpse_i_bird; 

    //dfluxdisc_bird         
    ydot[230] = Upt_disc_o_bird + Upt_disc_i_bird ;

//          //dfluxherb_bird
//          ydot[xxx] = Upt_herb_o_bird + Upt_herb_i_bird ;

    //dfluxcarn_bird
    ydot[231] = Upt_carn_o_bird + Upt_carn_i_bird ;

    //dfluxpfish_bird        
    ydot[232] = Upt_fishp_o_bird + Upt_fishp_i_bird ;

    //dfluxmfish_bird        
    ydot[233] = Upt_fishm_o_bird + Upt_fishm_i_bird ;

    //dfluxdfish_bird        
    ydot[234] = Upt_fishd_o_bird + Upt_fishd_i_bird;

    //dfluxbenths_bird        
    ydot[235] = Upt_benths_o_bird + Upt_benths_i_bird;

    //dfluxbenthc_bird        
    ydot[236] = Upt_benthc_o_bird + Upt_benthc_i_bird;


// .......................................... 

    //dfluxcorp_seal         
    ydot[237] =  Upt_corpse_o_seal + Upt_corpse_i_seal; 

    //dfluxdisc_seal         
    ydot[238] = Upt_disc_o_seal + Upt_disc_i_seal ;

    //dfluxcarn_seal
    ydot[239] = Upt_carn_o_seal + Upt_carn_i_seal ;

    //dfluxpfish_seal        
    ydot[240] = Upt_fishp_o_seal + Upt_fishp_i_seal ;

    //dfluxmfish_seal        
    ydot[241] = Upt_fishm_o_seal + Upt_fishm_i_seal ;

    //dfluxdfish_seal        
    ydot[242] = Upt_fishd_o_seal + Upt_fishd_i_seal;

    //dfluxbenths_seal        
    ydot[243] = Upt_benths_o_seal + Upt_benths_i_seal;

    //dfluxbenthc_seal        
    ydot[244] = Upt_benthc_o_seal + Upt_benthc_i_seal;

    //dfluxbird_seal        
    ydot[245] = Upt_bird_o_seal + Upt_bird_i_seal;   //

// .......................................... 

//       //dfluxcorp_ceta         
//       ydot[xxx] =  Upt_corpse_o_ceta + Upt_corpse_i_ceta; 

    //dfluxdisc_ceta         
    ydot[246] = Upt_disc_o_ceta + Upt_disc_i_ceta ;

    //dfluxherb_ceta
    ydot[247] = Upt_herb_o_ceta + Upt_herb_i_ceta ;

    //dfluxcarn_ceta
    ydot[248] = Upt_carn_o_ceta + Upt_carn_i_ceta ;

    //dfluxpfish_ceta        
    ydot[249] = Upt_fishp_o_ceta + Upt_fishp_i_ceta ;

    //dfluxmfish_ceta        
    ydot[250] = Upt_fishm_o_ceta + Upt_fishm_i_ceta ;

    //dfluxdfish_ceta        
    ydot[251] = Upt_fishd_o_ceta + Upt_fishd_i_ceta;

    //dfluxbenths_ceta        
    ydot[252] = Upt_benths_o_ceta + Upt_benths_i_ceta;

    //dfluxbenthc_ceta        
    ydot[253] = Upt_benthc_o_ceta + Upt_benthc_i_ceta;

    //dfluxbird_ceta        
    ydot[254] = Upt_bird_o_ceta + Upt_bird_i_ceta;   //

    //dfluxseal_ceta        
    ydot[255] = Upt_seal_o_ceta + Upt_seal_i_ceta;   //



// ============================================================== 

// SPAWNING AND RECRUITMENT FLUXES

    //dBs_spawn
    ydot[256]=(driverbs_sp*y[42] *BS_fec) + (driverbs_sp*y[57] *BS_fec);

    //dBs_recruit
    ydot[257]=(driverbs_rec*y[41]) + (driverbs_rec*y[55] );

    //dBc_spawn
    ydot[258]=(driverbc_sp*y[44] *BC_fec) + (driverbc_sp*y[58] *BC_fec);

    //dBc_recruit
    ydot[259]=(driverbc_rec*y[43]) + (driverbc_rec*y[56]);

    //dPfish_spawn           
    ydot[260] = (driverpfish_sp * y[45] *PF_fec) + (driverpfish_sp * y[64]*PF_fec) ;

    //dPfish_recruit         
    ydot[261] = driverpfish_rec * y[46] + driverpfish_rec * y[62];

    //dDfish_spawn           
    ydot[262] = (driverdfish_sp * y[47]*DF_fec) + (driverdfish_sp * y[66]*DF_fec);

    //dDfish_recruit         
    ydot[263] = driverdfish_rec * y[48] + driverdfish_rec * y[63] ;

// ============================================================== 


    //dfluxwcnit_Ngas           
    ydot[264] = ( d_so * y[29] ) + ( d_d * y[30] ) + (d_si * y[53]) ;

    //dfluxsednit_Ngas           
    ydot[265] =  (dsed_d1 * y[34])  + (dsed_d2 * y[35])  + (dsed_d3 * y[36])
               + (dsed_s1 * y[31]) + (dsed_s2 * y[32]) + (dsed_s3 * y[33])   ;



// ============================================================== 



// ADVECTION MIXING AND MIGRATION FLUXES



// Kelpdebris export to beaches
    //dfluxkelpdebris_beachexport
    ydot[266] = (driver_S_wave)*wave_beach_kelpdebris*y[76];
 


// External ocean DIN outflow from offshore zone
    //dfluxAMMoutflow_o       
    ydot[267] = OceanOUT_soammonia 
              + OceanOUT_dammonia ;

    //dfluxNIToutflow_o       
    ydot[268] = OceanOUT_sonitrate 
	      + OceanOUT_dnitrate ;


// External ocean DIN outflow from inshore zone
    //dfluxAMMoutflow_i        
    ydot[269] = OceanOUT_siammonia ;

    //dfluxNIToutflow_i        
    ydot[270] = OceanOUT_sinitrate ;


// External ocean PON outflow from offshore zone
    //dfluxPHYToutflow_o        
    ydot[271] = OceanOUT_sophyt 
	      + OceanOUT_dphyt  ;

    //dfluxDEToutflow_o        
    ydot[272] = OceanOUT_sodetritus 
	      + OceanOUT_ddetritus ;


// External ocean PON outflow from inshore zone
    //dfluxPHYToutflow_i        
    ydot[273] = OceanOUT_siphyt ;

    //dfluxDEToutflow_i        
    ydot[274] = OceanOUT_sidetritus ;


// External emigration of migratory fish
    //dmfish_emigration 
    ydot[275] = drivermfish_em * y[49] ;




//  --------------------------------------------------------




// Implicit boundary flux of nitrogen within the sediment for the OFFSHORE ONLY
// The defecation from the deposit feeding benthos assumes that all of the labile sed det is assimilatable plus the qs_p3 fraction of refractory                               //
// The indigestible fraction of refractory uptake is voided as refractory material - so only the digestible fraction is a net souce into the food web //
// Positive values of this flux indicate net inflow to the model and vice versa
    //dfluxsedboundary_o
    //ydot[276] =   (msed_d1 * ( ( (qs_p2 * y[11]) -  (qs_p1 * y[5]) ) )) +  qs_p3*Upt_xRdetritus_d1_benths_o
    //            + (msed_d2 * ( ( (qs_p2 * y[12]) -  (qs_p1 * y[6]) ) )) +  qs_p3*Upt_xRdetritus_d2_benths_o
    //            + (msed_d3 * ( ( (qs_p2 * y[13]) -  (qs_p1 * y[7]) ) )) +  qs_p3*Upt_xRdetritus_d3_benths_o
    //            - qs_p1 * ( corp_det_o * y[73] )
    //            - qs_p1 * ( corp_det_o * y[18] )
    //            - qs_p1 * ( corp_det_o * y[19] )
    //            - qs_p1 * ( corp_det_o * y[20] ) ;


     // Or just set this as the difference between all the other influxes and outfluxes
     ydot[276]   = (OceanOUT_soammonia 
              + OceanOUT_dammonia 
	      + OceanOUT_sonitrate 
	      + OceanOUT_dnitrate
              + OceanOUT_sophyt 
	      + OceanOUT_dphyt 
	      + OceanOUT_sodetritus 
	      + OceanOUT_ddetritus
              + ( d_so * y[29] ) + ( d_d * y[30] )
              + (dsed_d1 * y[34])  + (dsed_d2 * y[35])  + (dsed_d3 * y[36])
              + drivermfish_em * y[49]
              + Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * ( 1 - Podiscard ) * ( 1 - (Pogutting * offal_prop_live_weight))
              + Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (( 1 - DFp_NQ ) * (1 - DFop_discardQ)) * ( 1 - (Dogutting * offal_prop_live_weight))
              + Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (( DFp_NQ ) * (1 - DFop_discardNQ)) * ( 1 - (Dogutting * offal_prop_live_weight))    
              + Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * ( 1 - Modiscard ) * ( 1 - (Mogutting * offal_prop_live_weight))
              + Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * ( 1 - Bsodiscard ) * ( 1 - (Bsogutting * offal_prop_live_weight))
              + Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * ( 1 - Bcodiscard ) * ( 1 - (Bcogutting * offal_prop_live_weight))
              + Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * ( 1 - Zcodiscard ) * ( 1 - (Zcogutting * offal_prop_live_weight))
              + Fbdodaily * (twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) * ( 1 - BDodiscard ) * ( 1 - (BDogutting * offal_prop_live_weight))
              + Fslodaily * (twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) * ( 1 - SLodiscard ) * ( 1 - (SLogutting * offal_prop_live_weight)) 
              + Fctodaily * (twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) * ( 1 - CTodiscard ) * ( 1 - (CTogutting * offal_prop_live_weight))  )
          // plus all the passive outflows from offshore to inshore
              + (InshoreIN_sdetritus
              + InshoreIN_sammonia
              + InshoreIN_snitrate
              + InshoreIN_sphyt
              + InshoreIN_benthslar
              + InshoreIN_benthclar
              + InshoreIN_herb
              + InshoreIN_carn
              + InshoreIN_fishplar
              + InshoreIN_fishdlar)
          // plus all the active migrations from offshore to inshore
              + (InshoreIN_fishp
              + InshoreIN_fishd
              + InshoreIN_fishm
              + InshoreIN_bird
              + InshoreIN_seal
              + InshoreIN_ceta)
          // minus all the external inflows
              - (OceanIN_soammonia 
              + OceanIN_dammonia 
	      + OceanIN_sonitrate 
	      + OceanIN_dnitrate
              + OceanIN_sophyt 
              + OceanIN_dphyt 
	      + OceanIN_sodetritus 
	      + OceanIN_ddetritus
              + Atm_amm_IN_o + Atm_nit_IN_o 
              + drivermfish_im )
          // minus all the passive inflows from inshore to offshore
              - (InshoreOUT_sdetritus
              + InshoreOUT_sammonia
              + InshoreOUT_snitrate
              + InshoreOUT_sphyt
              + InshoreOUT_benthslar
              + InshoreOUT_benthclar
              + InshoreOUT_herb
              + InshoreOUT_carn
              + InshoreOUT_fishplar
              + InshoreOUT_fishdlar)
          // minus all the active migrations from inshore to offshore
              - (InshoreOUT_fishp
              + InshoreOUT_fishd
              + InshoreOUT_fishm
              + InshoreOUT_bird
              + InshoreOUT_seal
              + InshoreOUT_ceta
              + ( drivermfish_em * y[65] ) );
           // so if inflows are greater than outflows then this implies a net sink of N in the sediment
 


// Implicit boundary flux of nitrogen within the sediment for the INSHORE ONLY
// The defecation from the deposit feeding benthos assumes that all of the labile sed det is assimilatable plus the qs_p3 fraction of refractory                               //
// The indigestible fraction of refractory uptake is voided as refractory material - so only the digestible fraction is a net souce into the food web //
// Positive values of this flux indicate net inflow to the model and vice versa
    //dfluxsedboundary_i
    //ydot[277] =   (msed_s1 * ( ( (qs_p2 * y[8])  -  (qs_p1 * y[2]) ) )) +  qs_p3*Upt_xRdetritus_s1_benths_i
    //            + (msed_s2 * ( ( (qs_p2 * y[9])  -  (qs_p1 * y[3]) ) )) +  qs_p3*Upt_xRdetritus_s2_benths_i
    //            + (msed_s3 * ( ( (qs_p2 * y[10]) -  (qs_p1 * y[4]) ) )) +  qs_p3*Upt_xRdetritus_s3_benths_i
    //            - qs_p1 * ( kelpdebris_det_i * y[76] )
    //            - qs_p1 * ( corp_det_i * y[72] )
    //            - qs_p1 * ( corp_det_i * y[15] )
    //            - qs_p1 * ( corp_det_i * y[16] )
    //            - qs_p1 * ( corp_det_i * y[17] ) ;

     // Or just set this as the difference between all the other influxes and outfluxes
     ydot[277]   = (OceanOUT_siammonia 
	      + OceanOUT_sinitrate 
              + OceanOUT_siphyt
	      + OceanOUT_sidetritus 
              + ( d_si * y[53] )
              + (dsed_s1 * y[31]) + (dsed_s2 * y[32]) + (dsed_s3 * y[33])
              + (driver_S_wave)*wave_beach_kelpdebris*y[76] 
              + Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * ( 1 - Pidiscard ) * ( 1 - (Pigutting * offal_prop_live_weight)) 
              + Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (( 1 - DFp_NQ ) * (1 - DFip_discardQ)) * ( 1 - (Digutting * offal_prop_live_weight))  
              + Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (( DFp_NQ ) * (1 - DFip_discardNQ)) * ( 1 - (Digutting * offal_prop_live_weight))      
              + Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * ( 1 - Midiscard ) * ( 1 - (Migutting * offal_prop_live_weight))  
              + Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * ( 1 - Bsidiscard ) * ( 1 - (Bsigutting * offal_prop_live_weight))  
              + Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * ( 1 - Bcidiscard ) * ( 1 - (Bcigutting * offal_prop_live_weight)) 
              + Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * ( 1 - Zcidiscard ) * ( 1 - (Zcigutting * offal_prop_live_weight))  
              + Fbdidaily * (twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) * ( 1 - BDidiscard ) * ( 1 - (BDigutting * offal_prop_live_weight)) 
              + Fslidaily * (twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) * ( 1 - SLidiscard ) * ( 1 - (SLigutting * offal_prop_live_weight)) 
              + Fctidaily * (twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) * ( 1 - CTidiscard ) * ( 1 - (CTigutting * offal_prop_live_weight)) 
              + Fkpidaily * (twomax(0,(max_exploitable_f_KP*(y[75]-protect_KP_i)))) * ( 1 - KPidiscard ) * ( 1 - (KPigutting * offal_prop_live_weight))    )
          // plus all the passive outflows from inshore to offshore
              + (InshoreOUT_sdetritus
              + InshoreOUT_sammonia
              + InshoreOUT_snitrate
              + InshoreOUT_sphyt
              + InshoreOUT_benthslar
              + InshoreOUT_benthclar
              + InshoreOUT_herb
              + InshoreOUT_carn
              + InshoreOUT_fishplar
              + InshoreOUT_fishdlar)
          // plus all the active migrations from inshore to offshore
              + (InshoreOUT_fishp
              + InshoreOUT_fishd
              + InshoreOUT_fishm
              + InshoreOUT_bird 
              + InshoreOUT_seal 
              + InshoreOUT_ceta 
              + ( drivermfish_em * y[65] ) )
          // minus all the inflows
              - (OceanIN_siammonia 
	      + OceanIN_sinitrate 
	      + OceanIN_siphyt 
	      + OceanIN_sidetritus 
              + Atm_amm_IN_i + Atm_nit_IN_i
              + Riv_amm_IN + Riv_nit_IN + Riv_det_IN )
          // minus all the passive inflows from offshore to inshore
              - (InshoreIN_sdetritus
              + InshoreIN_sammonia
              + InshoreIN_snitrate
              + InshoreIN_sphyt
              + InshoreIN_benthslar
              + InshoreIN_benthclar
              + InshoreIN_herb
              + InshoreIN_carn
              + InshoreIN_fishplar
              + InshoreIN_fishdlar)
          // minus all the active migrations from offshore to inshore
              - (InshoreIN_fishp
              + InshoreIN_fishd
              + InshoreIN_fishm
              + InshoreIN_bird
              + InshoreIN_seal
              + InshoreIN_ceta) ;
           // so if inflows are greater than outflows then this implies a net sink of N in the sediment


//  --------------------------------------------------------


// External ocean DIN inflow to offshore zone
    //dfluxAMMinflow_o         
    ydot[278] = OceanIN_soammonia 
              + OceanIN_dammonia;

    //dfluxNITinflow_o         
    ydot[279] = OceanIN_sonitrate 
	      + OceanIN_dnitrate ;

// External ocean DIN inflow to inshore zone
    //dfluxAMMinflow_i         
    ydot[280] = OceanIN_siammonia;

    //dfluxNITinflow_i         
    ydot[281] = OceanIN_sinitrate ;


// External ocean PON inflow to offshore zone
    //dfluxPHYTinflow_o        
    ydot[282] = OceanIN_sophyt 
              + OceanIN_dphyt  ;

    //dfluxDETinflow_o        
    ydot[283] = OceanIN_sodetritus 
	      + OceanIN_ddetritus ;


// External ocean PON inflow to inshore zone
    //dfluxPHYTinflow_i        
    ydot[284] = OceanIN_siphyt  ;

    //dfluxDETinflow_i        
    ydot[285] = OceanIN_sidetritus ;


// External imigration of migratory fish
    //dmfish_imigration 
    ydot[286] = drivermfish_im  ;



//  --------------------------------------------------------



// Atmospheric DIN input to the offshore zone
    //datmosAMMinput_o         
    ydot[287] = Atm_amm_IN_o  ;

    //datmosNITinput_o         
    ydot[288] =  Atm_nit_IN_o  ;

// Atmospheric DIN input to the inshore zone
    //datmosAMMinput_i         
    ydot[289] = Atm_amm_IN_i  ;

    //datmosNITinput_i         
    ydot[290] =  Atm_nit_IN_i ;

// River DIN input to the inshore zone
    //drivAMMinflow          
    ydot[291] = Riv_amm_IN  ;

    //drivNITinflow          
    ydot[292] =  Riv_nit_IN ;

// River PON input to the inshore zone
    //drivPARTinflow         
    ydot[293] = Riv_det_IN ;


//  --------------------------------------------------------



// Passive DIN flux from inshore to offshore
     //dDINflux_i_o
     ydot[294]   = (InshoreOUT_sammonia + InshoreOUT_snitrate);

// Passive DIN flux from offshore to inshore
     //dDINflux_o_i
     ydot[295]   = (InshoreIN_sammonia + InshoreIN_snitrate);

// Passive particulate flux from inshore to offshore
     //dPARTflux_i_o
     ydot[296]   = (InshoreOUT_sdetritus
              + InshoreOUT_sphyt
              + InshoreOUT_benthslar
              + InshoreOUT_benthclar
              + InshoreOUT_herb
              + InshoreOUT_carn
              + InshoreOUT_fishplar
              + InshoreOUT_fishdlar) ;

// Passive particulate flux from offshore to inshore
     //dPARTflux_o_i
     ydot[297]   = (InshoreIN_sdetritus
              + InshoreIN_sphyt
              + InshoreIN_benthslar
              + InshoreIN_benthclar
              + InshoreIN_herb
              + InshoreIN_carn
              + InshoreIN_fishplar
              + InshoreIN_fishdlar) ;

//  ------------------


// Active migration flux of planktivorous fish from inshore to offshore
     //dactivemigpelfish_i_o
     ydot[298]   = (InshoreOUT_fishp);

// Active migration flux of migratory fish from inshore to offshore
     //dactivemigmigfish_i_o
     ydot[299]   = (InshoreOUT_fishm)
              + ( drivermfish_em * y[65] )  ;
// Combination of the food-driven migrations and the seasonal offshore movement of migratory fish

// Active migration flux of demersal fish from inshore to offshore
     //dactivemigdemfish_i_o
     ydot[300]   = (InshoreOUT_fishd);

// Active migration flux of birds fish from inshore to offshore
     //dactivemigbird_i_o
     ydot[301]   = (InshoreOUT_bird);

// Active migration flux of seals fish from inshore to offshore
     //dactivemigseal_i_o
     ydot[302]   = (InshoreOUT_seal);

// Active migration flux of cetaceans fish from inshore to offshore
     //dactivemigceta_i_o
     ydot[303]   = (InshoreOUT_ceta);


//  --------------------


// Active migration flux of planktivorous fish from offshore to inshore
     //dactivemigpelfish_o_i
     ydot[304]   = (InshoreIN_fishp);

// Active migration flux of migratory fish from offshore to inshore
     //dactivemigmigfish_o_i
     ydot[305]   = (InshoreIN_fishm);

// Active migration flux of demersal fish from offshore to inshore
     //dactivemigdemfish_o_i
     ydot[306]   = (InshoreIN_fishd);

// Active migration flux of birds fish from offshore to inshore
     //dactivemigbird_o_i
     ydot[307]   = (InshoreIN_bird);

// Active migration flux of seals fish from offshore to inshore
     //dactivemigseal_o_i
     ydot[308]   = (InshoreIN_seal);

// Active migration flux of cetaceans fish from offshore to inshore
     //dactivemigceta_o_i
     ydot[309]   = (InshoreIN_ceta);


// Vertical flux of nitrate in the offshore zone
    //dvertnitflux           
    ydot[310] = Vmix_nitrate + Upwelling_nit ;
                

// Horizontal flux of nitrate in the upper layers 
    //dhoriznitflux        
    ydot[311] = Riv_nit_IN
               + Atm_nit_IN_o + Atm_nit_IN_i
               + OceanIN_sonitrate
               + OceanIN_sinitrate
               - OceanOUT_sonitrate
               - OceanOUT_sinitrate ;


// ============================================================== 

// LANDINGS LIVE WEIGHT AND DISCARD FLUXES
// But remember that discards of birds&mammals go to corpses NOT the discard pool


// Offshore landings live weight
    //dlandp_o                
    ydot[312] = Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * ( 1 - Podiscard ) ;
    //dlandd_quota_o                
    ydot[313] = Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (( 1 - DFp_NQ ) * (1 - DFop_discardQ)) ;
    //dlandd_nonquota_o                
    ydot[314] = Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (( DFp_NQ ) * (1 - DFop_discardNQ))     ;
    //dlandm_o 
    ydot[315] = Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * ( 1 - Modiscard ) ;
    //dlandsb_o               
    ydot[316] = Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * ( 1 - Bsodiscard ) ;
    //dlandsb_o               
    ydot[317] = Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * ( 1 - Bcodiscard ) ;
    //dlandcz_o               
    ydot[318] = Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * ( 1 - Zcodiscard ) ;
    //dlandbd_o               
    ydot[319] = Fbdodaily * (twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) * ( 1 - BDodiscard ) ;
    //dlandsl_o               
    ydot[320] = Fslodaily * (twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) * ( 1 - SLodiscard ) ;
    //dlandct_o               
    ydot[321] = Fctodaily * (twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) * ( 1 - CTodiscard ) ;


// Offshore discards
    //ddiscpel_o               
    ydot[322] = Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * ( Podiscard ) ;
    //ddiscdem_quota_o              
    ydot[323] = Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (( 1 - DFp_NQ ) * (DFop_discardQ))    ;
    //ddiscdem_nonquota_o               
    ydot[324] = Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (( DFp_NQ ) * (DFop_discardNQ))      ;
    //ddiscmig_o
    ydot[325] = Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * ( Modiscard ) ;
    //ddiscsb_o               
    ydot[326] =  Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * ( Bsodiscard )   ;
    //ddisccb_o               
    ydot[327] =  Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * ( Bcodiscard )   ;
    //ddisccz_o               
    ydot[328] =  Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * ( Zcodiscard )   ;
    //ddiscbd_o               
    ydot[329] =  Fbdodaily * (twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) * ( BDodiscard )   ;
    //ddiscsl_o               
    ydot[330] =  Fslodaily * (twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) * ( SLodiscard )   ;
    //ddiscct_o               
    ydot[331] =  Fctodaily * (twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) * ( CTodiscard )   ;


// Inshore landings live weight
    //dlandp_i                
    ydot[332] = Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * ( 1 - Pidiscard ) ;
    //dlandd_quota_i                
    ydot[333] = Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (( 1 - DFp_NQ ) * (1 - DFip_discardQ)) ;
    //dlandd_nonquota_i                
    ydot[334] = Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (( DFp_NQ ) * (1 - DFip_discardNQ))     ;
    //dlandm_i 
    ydot[335] = Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * ( 1 - Midiscard ) ;
    //dlandsb_i               
    ydot[336] = Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * ( 1 - Bsidiscard ) ;
    //dlandcb_i               
    ydot[337] = Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * ( 1 - Bcidiscard ) ;
    //dlandcz_i               
    ydot[338] = Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * ( 1 - Zcidiscard ) ;
    //dlandbd_i               
    ydot[339] = Fbdidaily * (twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) * ( 1 - BDidiscard ) ;
    //dlandsl_i               
    ydot[340] = Fslidaily * (twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) * ( 1 - SLidiscard ) ;
    //dlandct_i               
    ydot[341] = Fctidaily * (twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) * ( 1 - CTidiscard ) ;
    //dlandkp_i               
    ydot[342] = Fkpidaily * (twomax(0,(max_exploitable_f_KP*(y[75]-protect_KP_i)))) * ( 1 - KPidiscard ) ;



// Inshore discards
    //ddiscpel_i               
    ydot[343] = Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * ( Pidiscard ) ;
    //ddiscdem_quota_i              
    ydot[344] = Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (( 1 - DFp_NQ ) * (DFip_discardQ))    ;
    //ddiscdem_nonquota_i               
    ydot[345] = Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (( DFp_NQ ) * (DFip_discardNQ))      ;
    //ddiscmig_i
    ydot[346] = Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * ( Midiscard ) ;
    //ddiscsb_i               
    ydot[347] =  Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * ( Bsidiscard )   ;
    //ddisccb_i              
    ydot[348] =  Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * ( Bcidiscard )   ;
    //ddisccz_i               
    ydot[349] =  Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * ( Zcidiscard )   ;
    //ddiscbd_i               
    ydot[350] =  Fbdidaily * (twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) * ( BDidiscard )   ;
    //ddiscsl_i               
    ydot[351] =  Fslidaily * (twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) * ( SLidiscard )   ;
    //ddiscct_i               
    ydot[352] =  Fctidaily * (twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) * ( CTidiscard )   ;
    //ddisckp_i               
    ydot[353] =  Fkpidaily * (twomax(0,(max_exploitable_f_KP*(y[75]-protect_KP_i)))) * ( KPidiscard )   ;



// FLUXES TO OFFAL - REMEMBER THAT OFFAL FROM PROCESSING  AT SEA OF FISH AND INVERTEBRATED GOES TO THE DISCARD POOL,
// OFFAL FROM PROCESSING  AT SEA OF BIRDS, SEALS AND CETACEANS ALSO GOES TO DISCARDS NOT CORPSES
// WHILE OFFAL FROM PROCESSING AT SEA OF KELP GOES TO KELP DEBRIS


// Offshore offal
    //doffalpel_o               
    ydot[354] = Fpodaily * (twomax(0,(max_exploitable_f_PF*(y[45]-protect_PF_o)))) * (1-Podiscard) * ( Pogutting )  * offal_prop_live_weight;
    //doffaldem_quota_o              
    ydot[355] = Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (( 1 - DFp_NQ ) * (1-DFop_discardQ) * (Dogutting))  * offal_prop_live_weight   ;
    //doffaldem_nonquota_o               
    ydot[356] = Fdodaily_USC * (twomax(0,(max_exploitable_f_DF*(y[47]-protect_DF_o)))) * (( DFp_NQ ) * (1-DFop_discardNQ) * (Dogutting))  * offal_prop_live_weight     ;
    //doffalmig_o
    ydot[357] = Fmodaily * (twomax(0,(max_exploitable_f_MF*(y[49]-protect_MF_o)))) * (1-Modiscard) * ( Mogutting )  * offal_prop_live_weight;
    //doffalsb_o               
    ydot[358] =  Fsbodaily * (twomax(0,(max_exploitable_f_SB*(y[42]-protect_SB_o)))) * (1-Bsodiscard) * ( Bsogutting )  * offal_prop_live_weight  ;
    //doffalcb_o               
    ydot[359] =  Fcbodaily * (twomax(0,(max_exploitable_f_CB*(y[44]-protect_CB_o)))) * (1-Bcodiscard) * ( Bcogutting )  * offal_prop_live_weight  ;
    //doffalcz_o               
    ydot[360] =  Fczodaily * (twomax(0,(max_exploitable_f_CZ*(y[40]-protect_CZ_o)))) * (1-Zcodiscard) * ( Zcogutting )  * offal_prop_live_weight  ;
    //doffalbd_o               
    ydot[361] =  Fbdodaily * (twomax(0,(max_exploitable_f_BD*(y[50]-protect_BD_o)))) * (1-BDodiscard) * ( BDogutting )  * offal_prop_live_weight  ;
    //doffalsl_o               
    ydot[362] =  Fslodaily * (twomax(0,(max_exploitable_f_SL*(y[68]-protect_SL_o)))) * (1-SLodiscard) * ( SLogutting )  * offal_prop_live_weight  ;
    //doffalct_o               
    ydot[363] =  Fctodaily * (twomax(0,(max_exploitable_f_CT*(y[70]-protect_CT_o)))) * (1-CTodiscard) * ( CTogutting )  * offal_prop_live_weight  ;


// Inshore offal
    //doffalpel_i               
    ydot[364] = Fpidaily * (twomax(0,(max_exploitable_f_PF*(y[64]-protect_PF_i)))) * (1-Pidiscard) * ( Pigutting )  * offal_prop_live_weight;
    //doffaldem_quota_i              
    ydot[365] = Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (( 1 - DFp_NQ ) * (1-DFip_discardQ) * (Digutting))  * offal_prop_live_weight   ;
    //doffaldem_nonquota_i               
    ydot[366] = Fdidaily_USC * (twomax(0,(max_exploitable_f_DF*(y[66]-protect_DF_i)))) * (( DFp_NQ ) * (1-DFip_discardNQ) * (Digutting))   * offal_prop_live_weight    ;
    //doffalmig_i
    ydot[367] = Fmidaily * (twomax(0,(max_exploitable_f_MF*(y[65]-protect_MF_i)))) * (1-Midiscard) * ( Migutting )  * offal_prop_live_weight;
    //doffalsb_i               
    ydot[368] =  Fsbidaily * (twomax(0,(max_exploitable_f_SB*(y[57]-protect_SB_i)))) * (1-Bsidiscard) * ( Bsigutting )  * offal_prop_live_weight  ;
    //doffalcb_i              
    ydot[369] =  Fcbidaily * (twomax(0,(max_exploitable_f_CB*(y[58]-protect_CB_i)))) * (1-Bcidiscard) * ( Bcigutting )  * offal_prop_live_weight  ;
    //doffalcz_i               
    ydot[370] =  Fczidaily * (twomax(0,(max_exploitable_f_CZ*(y[61]-protect_CZ_i)))) * (1-Zcidiscard) * ( Zcigutting )  * offal_prop_live_weight  ;
    //doffalbd_i               
    ydot[371] =  Fbdidaily * (twomax(0,(max_exploitable_f_BD*(y[67]-protect_BD_i)))) * (1-BDidiscard) * ( BDigutting )   * offal_prop_live_weight ;
    //doffalsl_i               
    ydot[372] =  Fslidaily * (twomax(0,(max_exploitable_f_SL*(y[69]-protect_SL_i)))) * (1-SLidiscard) * ( SLigutting )  * offal_prop_live_weight  ;
    //doffalct_i               
    ydot[373] =  Fctidaily * (twomax(0,(max_exploitable_f_CT*(y[71]-protect_CT_i)))) * (1-CTidiscard) * ( CTigutting )  * offal_prop_live_weight  ;
    //doffalkp_i               
    ydot[374] =  Fkpidaily * (twomax(0,(max_exploitable_f_KP*(y[75]-protect_KP_i)))) * (1-KPidiscard) * ( KPigutting )  * offal_prop_live_weight  ;


    // Omnivorous zooplankton net production - offshore
    //dherbnetprod_o
    ydot[375] = Assim_herb_o - ( eH_o * y[39] );

    // Omnivorous zooplankton net production - inshore
    //dherbnetprod_i
    ydot[376] = Assim_herb_i - ( eH_i * y[60] );

    // Carniv zooplankton net production - offshore
    //dcarnnetprod_o              
    ydot[377] = Assim_carn_o - ( eC_o * y[40] );

    // Carniv zooplankton net production - inshore
    //dcarnnetprod_i              
    ydot[378] = Assim_carn_i - ( eC_i * y[61] );


    // Pelfish larvae net production - offshore
    //dpfishlarnetprod_o          
    ydot[379] = Assim_fishplar_o - ( eFplar_o * y[46] ) ;

    // Pelfish larvae net production - inshore
    //dpfishlarnetprod_i          
    ydot[380] = Assim_fishplar_i - ( eFplar_i * y[62] );


    // Demfish larvae net production - offshore
    //ddfishlarnetprod_o          
    ydot[381] = Assim_fishdlar_o - ( eFdlar_o * y[48] ) ;

    // Demfish larvae net production - inshore
    //ddfishlarnetprod_i          
    ydot[382] =  Assim_fishdlar_i - ( eFdlar_i * y[63] );

    // Pelfish net production - offshore
    //dpfishnetprod_o             
    ydot[383] = Assim_fishp_o - ( eFp_o * y[45] ) ;

    // Pelfish net production - inshore
    //dpfishnetprod_i             
    ydot[384] = Assim_fishp_i - ( eFp_i * y[64] );

    // Migfish net production - offshore
    //dmfishnetprod_o 
    ydot[385] = Assim_fishm_o  - ( eFm_o * y[49] );

    // Migfish net production - inshore
    //dmfishnetprod_i 
    ydot[386] = Assim_fishm_i - ( eFm_i * y[65] );

    // Demfish net production - offshore
    //ddfishnetprod_o             
    ydot[387] = Assim_fishd_o  - ( eFd_o * y[47] );

    // Demfish net production - inshore
    //ddfishnetprod_i             
    ydot[388] = Assim_fishd_i - ( eFd_i * y[66] );

    // Benths larvae net production - offshore
    //dbenthslarnetprod_o
    ydot[389]=  Assim_benthslar_o -(eBslar_o*y[41]);

    // Benths larvae net production - inshore
    //dbenthslarnetprod_i
    ydot[390]=Assim_benthslar_i -(eBslar_i*y[55]) ;

    // Benthc larvae net production - offshore
    //dbenthclarnetprod_o
    ydot[391]=Assim_benthclar_o  -(eBclar_o*y[43]);

    // Benthc larvae net production - inshore
    //dbenthclarnetprod_i
    ydot[392]=Assim_benthclar_i -(eBclar_i*y[56]);

    // Benths net production - offshore
    //dbenthsnetprod_o  
    ydot[393] = Assim_benths_o - ( eBs_o * y[42] );

    // Benths net production - inshore
    //dbenthsnetprod_i  
    ydot[394] = Assim_benths_i - ( eBs_i * y[57] );

    // Benthc net production - offshore
    //dbenthcnetprod_o           
    ydot[395] = Assim_benthc_o - ( eBc_o * y[44] );

    // Benthc net production - inshore
    //dbenthcnetprod_i            
    ydot[396] = Assim_benthc_i - ( eBc_i * y[58] );

    // Bird net production - offshore
    //dbirdnetprod_o              
    ydot[397] = Assim_bird_o - ( ebird_o * y[50] );

    // Bird net production - inshore
    //dbirdnetprod_i              
    ydot[398] = Assim_bird_i - ( ebird_i * y[67] );


    // Seal net production - offshore
    //dsealnetprod_o              
    ydot[399] = Assim_seal_o - ( eseal_o * y[68] );

    // Seal net production - inshore
    //dsealnetprod_i              
    ydot[400] = Assim_seal_i - ( eseal_i * y[69] );

    // Cetacean net production - offshore
    //dcetanetprod_o              
    ydot[401] = Assim_ceta_o - ( eceta_o * y[70] );

    // Cetacean net production - inshore
    //dcetanetprod_i              
    ydot[402] = Assim_ceta_i - ( eceta_i * y[71] );

 }

/* END file StrathE2E2_ecology_model_function.c */
