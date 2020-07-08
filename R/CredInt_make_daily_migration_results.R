#
# CredInt_make_daily_migration_results.R
#
#' Save current set of fitted parameters to file
#'
#' @param model model object
#' @param lastyearstore inshore annual flux data
#' @param csv.output (TRUE or FALSE, default=TRUE) Set to FALSE to disable writing of CSV output files - useful for testing
#'
#' @importFrom graphics abline
#'
#' @noRd
#
# ------------------------------------------------------------------------------

CredInt_make_daily_migration_results <- function(model, lastyearstore,csv.output) {

	pkg.env$csv.output <- csv.output	# controls writing of CSV files

	resultsdir <- elt(model, "setup", "resultsdir")
	identifier <- elt(model, "setup", "model.ident")

	credpath <- makepath(resultsdir, CREDINT_DIR)

	#THIS VERSION PROCESSES THE FLUX DATA SAVED IN THE CUMULATIVE DAILY OUTPUT STORE
	#
	#THE FLUX DATA ARE CUMULATIVE SO FIRST THING TO DO IS MAKE A NEW DATAFRAME CONTAINING
	#THE DAILY INCREMENTS.


lastyearstore$dummy<-1


# names(lastyearstore)
#  [1] "time"                       "detritus_so"                "detritus_d"                
#  [4] "x_detritus_s1"              "x_detritus_s2"              "x_detritus_s3"             
#  [7] "x_detritus_d1"              "x_detritus_d2"              "x_detritus_d3"             
# [10] "xR_detritus_s1"             "xR_detritus_s2"             "xR_detritus_s3"            
# [13] "xR_detritus_d1"             "xR_detritus_d2"             "xR_detritus_d3"            
# [16] "discard_o"                  "corpse_s1"                  "corpse_s2"                 
# [19] "corpse_s3"                  "corpse_d1"                  "corpse_d2"                 
# [22] "corpse_d3"                  "ammonia_so"                 "ammonia_d"                 
# [25] "x_ammonia_s1"               "x_ammonia_s2"               "x_ammonia_s3"              
# [28] "x_ammonia_d1"               "x_ammonia_d2"               "x_ammonia_d3"              
# [31] "nitrate_so"                 "nitrate_d"                  "x_nitrate_s1"              
# [34] "x_nitrate_s2"               "x_nitrate_s3"               "x_nitrate_d1"              
# [37] "x_nitrate_d2"               "x_nitrate_d3"               "phyt_so"                   
# [40] "phyt_d"                     "herb_o"                     "carn_o"                    
# [43] "benthslar_o"                "benths_o"                   "benthclar_o"               
# [46] "benthc_o"                   "fishp_o"                    "fishplar_o"                
# [49] "fishd_o"                    "fishdlar_o"                 "fishm_o"                   
# [52] "bird_o"                     "detritus_si"                "ammonia_si"                
# [55] "nitrate_si"                 "phyt_si"                    "benthslar_i"               
# [58] "benthclar_i"                "benths_i"                   "benthc_i"                  
# [61] "discard_i"                  "herb_i"                     "carn_i"                    
# [64] "fishplar_i"                 "fishdlar_i"                 "fishp_i"                   
# [67] "fishm_i"                    "fishd_i"                    "bird_i"                    
# [70] "seal_o"                     "seal_i"                     "ceta_o"                    
# [73] "ceta_i"                     "corpse_s0"                  "corpse_d0"                 
# [76] "kelpC"                      "kelpN"                      "kelpdebris"                
# [79] "netpprod_o"                 "netpprod_i"                 "PNP_o"                     
# [82] "PNP_i"                      "phytgrossprod_o"            "phytgrossprod_i"           
# [85] "kelpCprod_i"                "kelpCexud_i"                "kelpNprod_i"               
# [88] "herbgrossprod_o"            "herbgrossprod_i"            "carngrossprod_o"           
# [91] "carngrossprod_i"            "pfishlargrossprod_o"        "pfishlargrossprod_i"       
# [94] "dfishlargrossprod_o"        "dfishlargrossprod_i"        "pfishgrossprod_o"          
# [97] "pfishgrossprod_i"           "mfishgrossprod_o"           "mfishgrossprod_i"          
#[100] "dfishgrossprod_o"           "dfishgrossprod_i"           "benthslargrossprod_o"      
#[103] "benthslargrossprod_i"       "benthclargrossprod_o"       "benthclargrossprod_i"      
#[106] "benthsgrossprod_o"          "benthsgrossprod_i"          "benthcgrossprod_o"         
#[109] "benthcgrossprod_i"          "birdgrossprod_o"            "birdgrossprod_i"           
#[112] "sealgrossprod_o"            "sealgrossprod_i"            "cetagrossprod_o"           
#[115] "cetagrossprod_i"            "wcdenitrif_o"               "wcdenitrif_i"              
#[118] "seddenitrif_o"              "seddenitrif_i"              "fluxsedamm_wcamm"          
#[121] "fluxwcdet_wcamm"            "fluxherb_wcamm"             "fluxcarn_wcamm"            
#[124] "fluxpfishlar_wcamm"         "fluxdfishlar_wcamm"         "fluxpfish_wcamm"           
#[127] "fluxmfish_wcamm"            "fluxdfish_wcamm"            "fluxbenthslar_wcamm"       
#[130] "fluxbenthclar_wcamm"        "fluxbenths_wcamm"           "fluxbenthc_wcamm"          
#[133] "fluxbird_wcamm"             "fluxseal_wcamm"             "fluxceta_wcamm"            
#[136] "fluxxdet_sedamm"            "fluxxRdet_sedamm"           "fluxwcamm_wcnit"           
#[139] "fluxsednit_wcnit"           "fluxsedamm_sednit"          "fluxxdet_wcdet"            
#[142] "fluxkelpdebris_wcdet"       "fluxcorp_wcdet"             "fluxphyt_wcdet"            
#[145] "fluxherb_wcdet"             "fluxcarn_wcdet"             "fluxpfishlar_wcdet"        
#[148] "fluxdfishlar_wcdet"         "fluxpfish_wcdet"            "fluxmfish_wcdet"           
#[151] "fluxdfish_wcdet"            "fluxbenthslar_wcdet"        "fluxbenthclar_wcdet"       
#[154] "fluxbenths_wcdet"           "fluxbenthc_wcdet"           "fluxbird_wcdet"            
#[157] "fluxseal_wcdet"             "fluxceta_wcdet"             "fluxwcdet_xdet"            
#[160] "fluxcorp_xdet"              "fluxbenths_xdet"            "fluxbenthc_xdet"           
#[163] "fluxxdet_xRdet"             "fluxkelpdebris_xRdet"       "fluxcorp_xRdet"            
#[166] "fluxkelp_kelpdebris"        "fluxdisc_corp"              "fluxpfish_corp"            
#[169] "fluxmfish_corp"             "fluxdfish_corp"             "fluxbenths_corp"           
#[172] "fluxbenthc_corp"            "fluxbird_corp"              "fluxseal_corp"             
#[175] "fluxceta_corp"              "fluxwcamm_kelp"             "fluxwcnit_kelp"            
#[178] "fluxwcamm_phyt_o"           "fluxwcamm_phyt_i"           "fluxwcnit_phyt_o"          
#[181] "fluxwcnit_phyt_i"           "fluxwcdet_herb"             "fluxphyt_herb"             
#[184] "fluxbenthslar_herb"         "fluxbenthclar_herb"         "fluxherb_carn"             
#[187] "fluxpfishlar_carn"          "fluxdfishlar_carn"          "fluxbenthslar_carn"        
#[190] "fluxbenthclar_carn"         "fluxherb_pfishlar"          "fluxbenthslar_pfishlar"    
#[193] "fluxbenthclar_pfishlar"     "fluxherb_dfishlar"          "fluxbenthslar_dfishlar"    
#[196] "fluxbenthclar_dfishlar"     "fluxherb_pfish"             "fluxcarn_pfish"            
#[199] "fluxpfishlar_pfish"         "fluxdfishlar_pfish"         "fluxbenthslar_pfish"       
#[202] "fluxbenthclar_pfish"        "fluxherb_mfish"             "fluxcarn_mfish"            
#[205] "fluxpfishlar_mfish"         "fluxdfishlar_mfish"         "fluxbenthslar_mfish"       
#[208] "fluxbenthclar_mfish"        "fluxcorp_dfish"             "fluxdisc_dfish"            
#[211] "fluxcarn_dfish"             "fluxpfishlar_dfish"         "fluxdfishlar_dfish"        
#[214] "fluxpfish_dfish"            "fluxmfish_dfish"            "fluxdfish_dfish"           
#[217] "fluxbenths_dfish"           "fluxbenthc_dfish"           "fluxwcdet_benthslar"       
#[220] "fluxphyt_benthslar"         "fluxwcdet_benthclar"        "fluxphyt_benthclar"        
#[223] "fluxwcdet_benths"           "fluxxdet_benths"            "fluxxRdet_benths"          
#[226] "fluxphyt_benths"            "fluxkelp_benthc"            "fluxkelpdebris_benthc"     
#[229] "fluxcorp_benthc"            "fluxbenths_benthc"          "fluxcorp_bird"             
#[232] "fluxdisc_bird"              "fluxcarn_bird"              "fluxpfish_bird"            
#[235] "fluxmfish_bird"             "fluxdfish_bird"             "fluxbenths_bird"           
#[238] "fluxbenthc_bird"            "fluxcorp_seal"              "fluxdisc_seal"             
#[241] "fluxcarn_seal"              "fluxpfish_seal"             "fluxmfish_seal"            
#[244] "fluxdfish_seal"             "fluxbenths_seal"            "fluxbenthc_seal"           
#[247] "fluxbird_seal"              "fluxdisc_ceta"              "fluxherb_ceta"             
#[250] "fluxcarn_ceta"              "fluxpfish_ceta"             "fluxmfish_ceta"            
#[253] "fluxdfish_ceta"             "fluxbenths_ceta"            "fluxbenthc_ceta"           
#[256] "fluxbird_ceta"              "fluxseal_ceta"              "Bs_spawn"                  
#[259] "Bs_recruit"                 "Bc_spawn"                   "Bc_recruit"                
#[262] "Pfish_spawn"                "Pfish_recruit"              "Dfish_spawn"               
#[265] "Dfish_recruit"              "fluxwcnit_Ngas"             "fluxsednit_Ngas"           
#[268] "fluxkelpdebris_beachexport" "fluxAMMoutflow_o"           "fluxNIToutflow_o"          
#[271] "fluxAMMoutflow_i"           "fluxNIToutflow_i"           "fluxPHYToutflow_o"         
#[274] "fluxDEToutflow_o"           "fluxPHYToutflow_i"          "fluxDEToutflow_i"          
#[277] "mfish_emigration"           "fluxsedboundary_o"          "fluxsedboundary_i"         
#[280] "fluxAMMinflow_o"            "fluxNITinflow_o"            "fluxAMMinflow_i"           
#[283] "fluxNITinflow_i"            "fluxPHYTinflow_o"           "fluxDETinflow_o"           
#[286] "fluxPHYTinflow_i"           "fluxDETinflow_i"            "mfish_imigration"          
#[289] "atmosAMMinput_o"            "atmosNITinput_o"            "atmosAMMinput_i"           
#[292] "atmosNITinput_i"            "rivAMMinflow"               "rivNITinflow"              
#[295] "rivPARTinflow"              "DINflux_i_o"                "DINflux_o_i"               
#[298] "PARTflux_i_o"               "PARTflux_o_i"               "activemigpelfish_i_o"      
#[301] "activemigmigfish_i_o"       "activemigdemfish_i_o"       "activemigbird_i_o"         
#[304] "activemigseal_i_o"          "activemigceta_i_o"          "activemigpelfish_o_i"      
#[307] "activemigmigfish_o_i"       "activemigdemfish_o_i"       "activemigbird_o_i"         
#[310] "activemigseal_o_i"          "activemigceta_o_i"          "vertnitflux"               
#[313] "horiznitflux"               "landp_o"                    "landd_quota_o"             
#[316] "landd_nonquota_o"           "landm_o"                    "landsb_o"                  
#[319] "landcb_o"                   "landcz_o"                   "landbd_o"                  
#[322] "landsl_o"                   "landct_o"                   "discpel_o"                 
#[325] "discdem_quota_o"            "discdem_nonquota_o"         "discmig_o"                 
#[328] "discsb_o"                   "disccb_o"                   "disccz_o"                  
#[331] "discbd_o"                   "discsl_o"                   "discct_o"                  
#[334] "landp_i"                    "landd_quota_i"              "landd_nonquota_i"          
#[337] "landm_i"                    "landsb_i"                   "landcb_i"                  
#[340] "landcz_i"                   "landbd_i"                   "landsl_i"                  
#[343] "landct_i"                   "landkp_i"                   "discpel_i"                 
#[346] "discdem_quota_i"            "discdem_nonquota_i"         "discmig_i"                 
#[349] "discsb_i"                   "disccb_i"                   "disccz_i"                  
#[352] "discbd_i"                   "discsl_i"                   "discct_i"                  
#[355] "disckp_i"                   "offalpel_o"                 "offaldem_quota_o"          
#[358] "offaldem_nonquota_o"        "offalmig_o"                 "offalsb_o"                 
#[361] "offalcb_o"                  "offalcz_o"                  "offalbd_o"                 
#[364] "offalsl_o"                  "offalct_o"                  "offalpel_i"                
#[367] "offaldem_quota_i"           "offaldem_nonquota_i"        "offalmig_i"                
#[370] "offalsb_i"                  "offalcb_i"                  "offalcz_i"                 
#[373] "offalbd_i"                  "offalsl_i"                  "offalct_i"                 
#[376] "offalkp_i"                  "herbnetprod_o"              "herbnetprod_i"             
#[379] "carnnetprod_o"              "carnnetprod_i"              "pfishlarnetprod_o"         
#[382] "pfishlarnetprod_i"          "dfishlarnetprod_o"          "dfishlarnetprod_i"         
#[385] "pfishnetprod_o"             "pfishnetprod_i"             "mfishnetprod_o"            
#[388] "mfishnetprod_i"             "dfishnetprod_o"             "dfishnetprod_i"            
#[391] "benthslarnetprod_o"         "benthslarnetprod_i"         "benthclarnetprod_o"        
#[394] "benthclarnetprod_i"         "benthsnetprod_o"            "benthsnetprod_i"           
#[397] "benthcnetprod_o"            "benthcnetprod_i"            "birdnetprod_o"             
#[400] "birdnetprod_i"              "sealnetprod_o"              "sealnetprod_i"             
#[403] "cetanetprod_o"              "cetanetprod_i"              "totalN"                    
#[406] "totalN_o"                   "totalN_i"                   "x_detritus"                
#[409] "x_detritus_o"               "x_detritus_i"               "corpse"                    
#[412] "corpse_o"                   "corpse_i"                   "x_ammonia"                 
#[415] "x_ammonia_o"                "x_ammonia_i"                "x_nitrate"                 
#[418] "x_nitrate_o"                "x_nitrate_i"                "s_detritus"                
#[421] "s_ammonia"                  "s_nitrate"                  "s_phyt"                    
#[424] "benthslar"                  "benthclar"                  "benths"                    
#[427] "benthc"                     "discard"                    "herb"                      
#[430] "carn"                       "fishp"                      "fishd"                     
#[433] "fishm"                      "bird"                       "seal"                      
#[436] "ceta"                       "fishplar"                   "fishdlar"                  
#[439] "PNP"                        "netpprod"                   "fluxwcamm_phyt"            
#[442] "fluxwcnit_phyt"             "phytgrossprod"              "herbgrossprod"             
#[445] "carngrossprod"              "pfishlargrossprod"          "dfishlargrossprod"         
#[448] "pfishgrossprod"             "mfishgrossprod"             "dfishgrossprod"            
#[451] "benthslargrossprod"         "benthclargrossprod"         "benthsgrossprod"           
#[454] "benthcgrossprod"            "birdgrossprod"              "sealgrossprod"             
#[457] "cetagrossprod"              "herbnetprod"                "carnnetprod"               
#[460] "pfishlarnetprod"            "dfishlarnetprod"            "pfishnetprod"              
#[463] "mfishnetprod"               "dfishnetprod"               "benthslarnetprod"          
#[466] "benthclarnetprod"           "benthsnetprod"              "benthcnetprod"             
#[469] "birdnetprod"                "sealnetprod"                "cetanetprod"               
#[472] "wcdenitrif"                 "seddenitrif"                "fluxsedboundary"           
#[475] "DIN_NET_flux_o_i"           "PART_NET_flux_o_i"          "NET_activemigpelfish_o_i"  
#[478] "NET_activemigmigfish_o_i"   "NET_activemigdemfish_o_i"   "NET_activemigbird_o_i"     
#[481] "NET_activemigseal_o_i"      "NET_activemigceta_o_i"      "NET_mfish_ext_o"           
#[484] "fluxDINinflow"              "fluxDINoutflow"             "fluxPARTinflow"            
#[487] "fluxPARToutflow"            "atmosDINinput"              "rivDINinflow"              
#[490] "landp"                      "landd"                      "landd_o"                   
#[493] "landd_i"                    "landd_quota"                "landd_nonquota"            
#[496] "landm"                      "landsb"                     "landcb"                    
#[499] "landcz"                     "landbd"                     "landsl"                    
#[502] "landct"                     "discpel"                    "discdem"                   
#[505] "discdem_o"                  "discdem_i"                  "discdem_quota"             
#[508] "discdem_nonquota"           "discmig"                    "discsb"                    
#[511] "disccb"                     "disccz"                     "discbd"                    
#[514] "discsl"                     "discct"                     "offalpel"                  
#[517] "offaldem"                   "offaldem_o"                 "offaldem_i"                
#[520] "offaldem_quota"             "offaldem_nonquota"          "offalmig"                  
#[523] "offalsb"                    "offalcb"                    "offalcz"                   
#[526] "offalbd"                    "offalsl"                    "offalct"                   
#[529] "shallowprop"                "depth_si"                   "depth_so"                  
#[532] "depth_d"                    "area_s0"                    "area_s1"                   
#[535] "area_s2"                    "area_s3"                    "area_d0"                   
#[538] "area_d1"                    "area_d2"                    "area_d3"                   
#[541] "depth_s1"                   "depth_s2"                   "depth_s3"                  
#[544] "depth_d1"                   "depth_d2"                   "depth_d3"                  
#[547] "poros_s1"                   "poros_s2"                   "poros_s3"                  
#[550] "poros_d1"                   "poros_d2"                   "poros_d3"                  
#[553] "iteration"                  "likelihood"                 "dummy"                     


creds<-c(0.005,0.25,0.5,0.75,0.995) # This can be any lenth of vector in any order. All values must be between 0 and 1

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	Build_Cred_Timeseries_Fluxes<-function(VtoPid,areaid,thikid,io,creds,varname){
		#io = 0 means offshore
		#io = 1 means inshore
		#io = 2 means whole domain

		rownamesvec<-rep(NA,(length(creds)+1))
		for(kkk in 1:(length(creds)+1)){
			if(kkk==1) rownamesvec[kkk]<-paste(varname,"-maxlik",sep="")
			if(kkk>1)  rownamesvec[kkk]<-paste(varname,"-",creds[kkk-1],sep="")
		}

		Resultdata<-data.frame(rep(NA,(length(creds)+1)))
		for(kkk in 1:360){
			Resultdata[,kkk+1]<-(rep(NA,(length(creds)+1)))
		}
		names(Resultdata)<-seq(0,360)
		row.names(Resultdata)<-rownamesvec

		template<-data.frame(newlastyearstore$time)
		names(template)<-"time"
		if(io==2) template$VtoP<-newlastyearstore[,VtoPid]
		if(io==1) template$VtoP<-newlastyearstore[,VtoPid]/(newlastyearstore[,areaid]*newlastyearstore[,thikid])
		if(io==0) template$VtoP<-newlastyearstore[,VtoPid]/((1-newlastyearstore[,areaid])*newlastyearstore[,thikid])
		template$iteration<-newlastyearstore$iteration
		template$likelihood<-newlastyearstore$likelihood

		for(ii in 0:360){
			Resultdata[1,ii+1]<-template$VtoP[which(template$time==ii&template$iteration==1)]
			modresvector<-template$VtoP[which(template$time==ii)]
			likevector<-template$likelihood[which(template$time==ii)]	
			#message("\n","Processing ",varname," data")
			credvals<-GetCredInt(modresvector,likevector,creds,var=varname,plotgraph=FALSE)
			Resultdata[2:(length(credvals)+1),ii+1]<-credvals
		}
		return(Resultdata)
	}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# CONVERT CUMULATIVE FLUX COLUMNS INTO DAILY FLUXES

#Find the number of runs that were stored


n_datasets<-nrow(lastyearstore)/361
newlastyearstore<-data.frame(lastyearstore$time)
names(newlastyearstore)<-"time"
newlastyearstore$dummy<-1
newlastyearstore$iteration<-lastyearstore$iteration
newlastyearstore$likelihood<-lastyearstore$likelihood
newlastyearstore$shallowprop<-lastyearstore$shallowprop
tempset<-rep(0,361)

#################################################
col2proc<-477 # Pelagic fish migrations
v2proc<-lastyearstore[,col2proc]
newcol<-(ncol(newlastyearstore)+1)
newnames<-c(names(newlastyearstore),names(lastyearstore)[col2proc])
newlastyearstore[,newcol]<-0
names(newlastyearstore)<-newnames
for(jjjj in 1:n_datasets){
tempset[1:360]<-v2proc[((jjjj-1)*361+1+(1)):(jjjj*361)]
tempset[361] <-v2proc[(jjjj*361)] + v2proc[((jjjj-1)*361+1+(1))] - v2proc[((jjjj-1)*361+1)]
newlastyearstore[((jjjj-1)*361+1):(jjjj*361),newcol] <- tempset - v2proc[((jjjj-1)*361+1):(jjjj*361)]
}


col2proc<-478 # Migratory fish migrations
v2proc<-lastyearstore[,col2proc]
newcol<-(ncol(newlastyearstore)+1)
newnames<-c(names(newlastyearstore),names(lastyearstore)[col2proc])
newlastyearstore[,newcol]<-0
names(newlastyearstore)<-newnames
for(jjjj in 1:n_datasets){
tempset[1:360]<-v2proc[((jjjj-1)*361+1+(1)):(jjjj*361)]
tempset[361] <-v2proc[(jjjj*361)] + v2proc[((jjjj-1)*361+1+(1))] - v2proc[((jjjj-1)*361+1)]
newlastyearstore[((jjjj-1)*361+1):(jjjj*361),newcol] <- tempset - v2proc[((jjjj-1)*361+1):(jjjj*361)]
}


col2proc<-479 # Demersal fish migrations
v2proc<-lastyearstore[,col2proc]
newcol<-(ncol(newlastyearstore)+1)
newnames<-c(names(newlastyearstore),names(lastyearstore)[col2proc])
newlastyearstore[,newcol]<-0
names(newlastyearstore)<-newnames
for(jjjj in 1:n_datasets){
tempset[1:360]<-v2proc[((jjjj-1)*361+1+(1)):(jjjj*361)]
tempset[361] <-v2proc[(jjjj*361)] + v2proc[((jjjj-1)*361+1+(1))] - v2proc[((jjjj-1)*361+1)]
newlastyearstore[((jjjj-1)*361+1):(jjjj*361),newcol] <- tempset - v2proc[((jjjj-1)*361+1):(jjjj*361)]
}


col2proc<-480 # Bird migrations
v2proc<-lastyearstore[,col2proc]
newcol<-(ncol(newlastyearstore)+1)
newnames<-c(names(newlastyearstore),names(lastyearstore)[col2proc])
newlastyearstore[,newcol]<-0
names(newlastyearstore)<-newnames
for(jjjj in 1:n_datasets){
tempset[1:360]<-v2proc[((jjjj-1)*361+1+(1)):(jjjj*361)]
tempset[361] <-v2proc[(jjjj*361)] + v2proc[((jjjj-1)*361+1+(1))] - v2proc[((jjjj-1)*361+1)]
newlastyearstore[((jjjj-1)*361+1):(jjjj*361),newcol] <- tempset - v2proc[((jjjj-1)*361+1):(jjjj*361)]
}

col2proc<-481 # Seal migrations
v2proc<-lastyearstore[,col2proc]
newcol<-(ncol(newlastyearstore)+1)
newnames<-c(names(newlastyearstore),names(lastyearstore)[col2proc])
newlastyearstore[,newcol]<-0
names(newlastyearstore)<-newnames
for(jjjj in 1:n_datasets){
tempset[1:360]<-v2proc[((jjjj-1)*361+1+(1)):(jjjj*361)]
tempset[361] <-v2proc[(jjjj*361)] + v2proc[((jjjj-1)*361+1+(1))] - v2proc[((jjjj-1)*361+1)]
newlastyearstore[((jjjj-1)*361+1):(jjjj*361),newcol] <- tempset - v2proc[((jjjj-1)*361+1):(jjjj*361)]
}

col2proc<-482 # Cetacean migrations
v2proc<-lastyearstore[,col2proc]
newcol<-(ncol(newlastyearstore)+1)
newnames<-c(names(newlastyearstore),names(lastyearstore)[col2proc])
newlastyearstore[,newcol]<-0
names(newlastyearstore)<-newnames
for(jjjj in 1:n_datasets){
tempset[1:360]<-v2proc[((jjjj-1)*361+1+(1)):(jjjj*361)]
tempset[361] <-v2proc[(jjjj*361)] + v2proc[((jjjj-1)*361+1+(1))] - v2proc[((jjjj-1)*361+1)]
newlastyearstore[((jjjj-1)*361+1):(jjjj*361),newcol] <- tempset - v2proc[((jjjj-1)*361+1):(jjjj*361)]
}

################################################################



newlastyearstore$NET_activemigpelfish_o_i_propbiom <- newlastyearstore$NET_activemigpelfish_o_i/(lastyearstore$fishp_o+lastyearstore$fishp_i)
newlastyearstore$NET_activemigmigfish_o_i_propbiom <- newlastyearstore$NET_activemigmigfish_o_i/(lastyearstore$fishm_o+lastyearstore$fishm_i)
newlastyearstore$NET_activemigdemfish_o_i_propbiom <- newlastyearstore$NET_activemigdemfish_o_i/(lastyearstore$fishd_o+lastyearstore$fishd_i)
newlastyearstore$NET_activemigbird_o_i_propbiom <- newlastyearstore$NET_activemigbird_o_i/(lastyearstore$bird_o+lastyearstore$bird_i)
newlastyearstore$NET_activemigseal_o_i_propbiom <- newlastyearstore$NET_activemigseal_o_i/(lastyearstore$seal_o+lastyearstore$seal_i)
newlastyearstore$NET_activemigceta_o_i_propbiom <- newlastyearstore$NET_activemigceta_o_i/(lastyearstore$ceta_o+lastyearstore$ceta_i)

#################################################




#Pelagic fish migrations
VtoPid<-which(names(newlastyearstore)=="NET_activemigpelfish_o_i")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Planktivorous_fish_offshore_inshore_net_migration"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-Result_data

#Pelagic fish migrations as proportion of whole domain biomass
VtoPid<-which(names(newlastyearstore)=="NET_activemigpelfish_o_i_propbiom")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Planktivorous_fish_off_in_net_migration_propbiom"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)



#migratory fish migrations
VtoPid<-which(names(newlastyearstore)=="NET_activemigmigfish_o_i")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Migratory_fish_offshore_inshore_net_migration"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)

#migratory fish migrations as proportion of whole domain biomass
VtoPid<-which(names(newlastyearstore)=="NET_activemigmigfish_o_i_propbiom")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Migratory_fish_off_in_net_migration_propbiom"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)



#Demersal fish migrations
VtoPid<-which(names(newlastyearstore)=="NET_activemigdemfish_o_i")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Demersal_fish_offshore_inshore_net_migration"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)

#Demersal fish migrations as proportion of whole domain biomass
VtoPid<-which(names(newlastyearstore)=="NET_activemigdemfish_o_i_propbiom")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Demersal_fish_off_in_net_migration_propbiom"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)



#Bird migrations
VtoPid<-which(names(newlastyearstore)=="NET_activemigbird_o_i")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Birds_offshore_inshore_net_migration"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)

#Bird migrations as proportion of whole domain biomass
VtoPid<-which(names(newlastyearstore)=="NET_activemigbird_o_i_propbiom")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Birds_off_in_net_migration_propbiom"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)



#Seals migrations
VtoPid<-which(names(newlastyearstore)=="NET_activemigseal_o_i")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Pinnipeds_offshore_inshore_net_migration"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)

#Seals migrations as proportion of whole domain biomass
VtoPid<-which(names(newlastyearstore)=="NET_activemigseal_o_i_propbiom")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Pinnipeds_off_in_net_migration_propbiom"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)


#Cetaceans migrations
VtoPid<-which(names(newlastyearstore)=="NET_activemigceta_o_i")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Cetaceans_offshore_inshore_net_migration"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)

#Cetaceans migrations as proportion of whole domain biomass
VtoPid<-which(names(newlastyearstore)=="NET_activemigceta_o_i_propbiom")
areaid<-which(names(lastyearstore)=="dummy")
thikid<-which(names(lastyearstore)=="dummy")
io<-2  #whole domain
varname<-"Cetaceans_off_in_net_migration_propbiom"
Result_data<-Build_Cred_Timeseries_Fluxes(VtoPid,areaid,thikid,io,creds,varname)
Result_data_store<-rbind(Result_data_store,Result_data)


# goes into resultsdir/CredInt/datafile-ident.csv
csvfile <- csvname(credpath, "CredInt_processed_daily_migrations", identifier)
writecsv(Result_data_store, csvfile)

Result_data_store

}

