#
# CredInt_make_aamass_results.R
#
#' Save current set of fitted parameters to file
#'
#' @param model model object
#' @param inshoreaamassstore inshore annual flux data
#' @param offshoreaamassstore offshore annual flux data
#' @param wholeaamassstore whole annual flux data
#' @param csv.output (TRUE or FALSE, default=TRUE) Set to FALSE to disable writing of CSV output files - useful for testing
#'
#' @importFrom graphics abline bxp
#'
#' @noRd
#
# ------------------------------------------------------------------------------

CredInt_make_aamass_results <- function(model, inshoreaamassstore, offshoreaamassstore, wholeaamassstore,csv.output) {

	pkg.env$csv.output <- csv.output	# controls writing of CSV files

	setup		<- elt(model, "setup")	
	resultsdir	<- elt(setup, "resultsdir")
	identifier	<- elt(setup, "model.ident")

	credpath <- makepath(resultsdir, CREDINT_DIR)



	# names(offshoreaamassstore)
	# [1] "iteration"                                "likelihood"                              
	# [3] "Surface_layer_detritus"                   "Deep_layer_detritus"                     
	# [5] "Sediment_labile_plus_refractory_detritus" "Sediment_refractory_detritus"            
	# [7] "Fishery_discards"                         "Corpses"                                 
	# [9] "Macrophyte_debris"                              "Surface_layer_ammonia"                   
	#[11] "Deep_layer_ammonia"                       "Sediment_porewater_ammonia"              
	#[13] "Surface_layer_nitrate"                    "Deep_layer_nitrate"                      
	#[15] "Sediment_porewater_nitrate"               "Macrophyte_nitrogen"                           
	#[17] "Surface_layer_phytoplankton"              "Deep_layer_phytoplankton"                
	#[19] "Omnivorous_zooplankton"                   "Carnivorous_zooplankton"                 
	#[21] "Benthos_susp/dep_feeders_larvae"          "Benthos_susp/dep_feeders"                
	#[23] "Benthos_carn/scav_feeders_larvae"         "Benthos_carn/scav_feeders"               
	#[25] "Planktivorous_fish_larvae"                "Planktivorous_fish"                      
	#[27] "Migratory_fish"                           "Demersal_fish_larvae"                    
	#[29] "Demersal_fish"                            "Bird"                                    
	#[31] "Pinnipeds"                                    "Cetaceans"                              
	#[33] "Total_nitrogen_mass"                      "Area_proportion_of_inshore_zone"         
	#[35] "Thickness_of_inshore_surface_layer"       "Thickness_of_offshore_surface_layer"     
	#[37] "Thickness_of_offshore_deep_layer"         "Area_proportion_inshore_rock"            
	#[39] "Area_proportion_inshore_sediment_s1"      "Area_proportion_inshore_sediment_s2"     
	#[41] "Area_proportion_inshore_sediment_s3"      "Area_proportion_offshore_rock"           
	#[43] "Area_proportion_offshore_sediment_d1"     "Area_proportion_offshore_sediment_d2"    
	#[45] "Area_proportion_offshore_sediment_d3"     "Thickness_of_inshore_sediment_layer_s1"  
	#[47] "Thickness_of_inshore_sediment_layer_s2"   "Thickness_of_inshore_sediment_layer_s3"  
	#[49] "Thickness_of_offshore_sediment_layer_d1"  "Thickness_of_offshore_sediment_layer_d2" 
	#[51] "Thickness_of_offshore_sediment_layer_d3"  "Porosity_of_inshore_sediment_layer_s1"   
	#[53] "Porosity_of_inshore_sediment_layer_s2"    "Porosity_of_inshore_sediment_layer_s3"   
	#[55] "Porosity_of_offshore_sediment_layer_d1"   "Porosity_of_offshore_sediment_layer_d2"  
	#[57] "Porosity_of_offshore_sediment_layer_d3"  



	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#Just need the values of the constant xR detritus to subtract from the total x-detritus figures here
	xRdetritus_i <- inshoreaamassstore$Sediment_refractory_detritus[1]
	xRdetritus_o <- offshoreaamassstore$Sediment_refractory_detritus[1]
	xRdetritus_w <- wholeaamassstore$Sediment_refractory_detritus[1]

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	xLdetritus_i <- inshoreaamassstore$Sediment_labile_plus_refractory_detritus-xRdetritus_i
	xLdetritus_o <- offshoreaamassstore$Sediment_labile_plus_refractory_detritus-xRdetritus_o
	xLdetritus_w <- wholeaamassstore$Sediment_labile_plus_refractory_detritus-xRdetritus_w

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	total_labile_detritus_i <- xLdetritus_i + inshoreaamassstore$Surface_layer_detritus + inshoreaamassstore$Corpses + inshoreaamassstore$Fishery_discards + inshoreaamassstore$Macrophyte_debris
	total_labile_detritus_o <- xLdetritus_o + offshoreaamassstore$Surface_layer_detritus + offshoreaamassstore$Deep_layer_detritus + offshoreaamassstore$Corpses + offshoreaamassstore$Fishery_discards
	total_labile_detritus_w <- xLdetritus_w + wholeaamassstore$Surface_layer_detritus + wholeaamassstore$Deep_layer_detritus + wholeaamassstore$Corpses + wholeaamassstore$Fishery_discards + wholeaamassstore$Macrophyte_debris


	offshoreaamassstore$total_labile_detritus_bd<-total_labile_detritus_o/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$kelp_bd<-(offshoreaamassstore$Macrophyte_nitrogen)/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$phyt_bd<-(offshoreaamassstore$Surface_layer_phytoplankton+offshoreaamassstore$Deep_layer_phytoplankton)/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)

	#offshoreaamassstore$benth_susp_bd<-(offshoreaamassstore$Benthos_susp/dep_feeders+offshoreaamassstore$Benthos_susp/dep_feeders_larvae)/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$benth_susp_bd<-(offshoreaamassstore[,21]+offshoreaamassstore[,22])/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)

	offshoreaamassstore$herb_bd<-offshoreaamassstore$Omnivorous_zooplankton/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)

	#offshoreaamassstore$benth_carn_bd<-(offshoreaamassstore$Benthos_carn/scav_feeders+offshoreaamassstore$Benthos_carn/scav_feeders_larvae)/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$benth_carn_bd<-(offshoreaamassstore[,23]+offshoreaamassstore[,24])/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)

	offshoreaamassstore$carn_bd<-offshoreaamassstore$Carnivorous_zooplankton/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$plankfish_bd<-(offshoreaamassstore$Planktivorous_fish+offshoreaamassstore$Planktivorous_fish_larvae)/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$migfish_bd<-offshoreaamassstore$Migratory_fish/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$demfish_bd<-(offshoreaamassstore$Demersal_fish+offshoreaamassstore$Demersal_fish_larvae)/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$bird_bd<-offshoreaamassstore$Bird/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$seal_bd<-offshoreaamassstore$Pinnipeds/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)
	offshoreaamassstore$ceta_bd<-offshoreaamassstore$Cetaceans/(1-offshoreaamassstore$Area_proportion_of_inshore_zone)


	inshoreaamassstore$total_labile_detritus_bd<-total_labile_detritus_i/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$kelp_bd<-(inshoreaamassstore$Macrophyte_nitrogen)/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$phyt_bd<-(inshoreaamassstore$Surface_layer_phytoplankton)/(inshoreaamassstore$Area_proportion_of_inshore_zone)

	#inshoreaamassstore$benth_susp_bd<-(inshoreaamassstore$Benthos_susp/dep_feeders+inshoreaamassstore$Benthos_susp/dep_feeders_larvae)/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$benth_susp_bd<-(inshoreaamassstore[,21]+inshoreaamassstore[,22])/(inshoreaamassstore$Area_proportion_of_inshore_zone)

	inshoreaamassstore$herb_bd<-inshoreaamassstore$Omnivorous_zooplankton/(inshoreaamassstore$Area_proportion_of_inshore_zone)

	#inshoreaamassstore$benth_carn_bd<-(inshoreaamassstore$Benthos_carn/scav_feeders+inshoreaamassstore$Benthos_carn/scav_feeders_larvae)/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$benth_carn_bd<-(inshoreaamassstore[,23]+inshoreaamassstore[,24])/(inshoreaamassstore$Area_proportion_of_inshore_zone)

	inshoreaamassstore$carn_bd<-inshoreaamassstore$Carnivorous_zooplankton/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$plankfish_bd<-(inshoreaamassstore$Planktivorous_fish+inshoreaamassstore$Planktivorous_fish_larvae)/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$migfish_bd<-inshoreaamassstore$Migratory_fish/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$demfish_bd<-(inshoreaamassstore$Demersal_fish+inshoreaamassstore$Demersal_fish_larvae)/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$bird_bd<-inshoreaamassstore$Bird/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$seal_bd<-inshoreaamassstore$Pinnipeds/(inshoreaamassstore$Area_proportion_of_inshore_zone)
	inshoreaamassstore$ceta_bd<-inshoreaamassstore$Cetaceans/(inshoreaamassstore$Area_proportion_of_inshore_zone)


	wholeaamassstore$total_labile_detritus_bd<-total_labile_detritus_w
	wholeaamassstore$kelp_bd<-(wholeaamassstore$Macrophyte_nitrogen)
	wholeaamassstore$phyt_bd<-(wholeaamassstore$Surface_layer_phytoplankton+wholeaamassstore$Deep_layer_phytoplankton)

	#wholeaamassstore$benth_susp_bd<-(wholeaamassstore$Benthos_susp/dep_feeders+wholeaamassstore$Benthos_susp/dep_feeders_larvae)
	wholeaamassstore$benth_susp_bd<-(wholeaamassstore[,21]+wholeaamassstore[,22])

	wholeaamassstore$herb_bd<-wholeaamassstore$Omnivorous_zooplankton

	#wholeaamassstore$benth_carn_bd<-(wholeaamassstore$Benthos_carn/scav_feeders+wholeaamassstore$Benthos_carn/scav_feeders_larvae)
	wholeaamassstore$benth_carn_bd<-(wholeaamassstore[,23]+wholeaamassstore[,24])

	wholeaamassstore$carn_bd<-wholeaamassstore$Carnivorous_zooplankton
	wholeaamassstore$plankfish_bd<-(wholeaamassstore$Planktivorous_fish+wholeaamassstore$Planktivorous_fish_larvae)
	wholeaamassstore$migfish_bd<-wholeaamassstore$Migratory_fish
	wholeaamassstore$demfish_bd<-(wholeaamassstore$Demersal_fish+wholeaamassstore$Demersal_fish_larvae)
	wholeaamassstore$bird_bd<-wholeaamassstore$Bird
	wholeaamassstore$seal_bd<-wholeaamassstore$Pinnipeds
	wholeaamassstore$ceta_bd<-wholeaamassstore$Cetaceans


	Nind<-13

	cols2use<-58:(58-1+Nind)

	creds<-c(0.005,0.25,0.5,0.75,0.995)  # This can be any lenth of vector in any order. All values must be between 0 and 1


	O_results<-data.frame(rep(0,(length(creds)+1)))

	rownames(O_results)<-c("maxlik","lowlimit","lowquart","median","uppquart","upplimit")

	for(jj in 2:Nind){
		O_results[,jj]<-rep(0,(length(creds)+1))
	}
	colnames(O_results)<-names(offshoreaamassstore[cols2use])

	I_results<-O_results
	W_results<-O_results
	O_results[1,]<-offshoreaamassstore[1,cols2use]
	I_results[1,]<-inshoreaamassstore[1,cols2use]
	W_results[1,]<-wholeaamassstore[1,cols2use]

	for(jj in 1:Nind){

		if(jj==1 || jj>2){             # Exception here for kelp where offshore values are all zero
			Result<-offshoreaamassstore[,cols2use[jj]]

			Likelihood<-offshoreaamassstore$likelihood
			#message("\n","Processing offshore ",colnames(O_results)[jj]," data              ")
			O_results[2:(length(creds)+1),jj]<-GetCredInt(Result,Likelihood,creds,var=colnames(O_results)[jj],plotgraph=FALSE)
		}


		Result<-inshoreaamassstore[,cols2use[jj]]

		Likelihood<-inshoreaamassstore$likelihood
		#message("\n","Processing inshore ",colnames(I_results)[jj]," data              ")
		I_results[2:(length(creds)+1),jj]<-GetCredInt(Result,Likelihood,creds,var=colnames(I_results)[jj],plotgraph=FALSE)


		Result<-wholeaamassstore[,cols2use[jj]]

		Likelihood<-wholeaamassstore$likelihood
		#message("\n","Processing whole domain ",colnames(W_results)[jj]," data              ")
		W_results[2:(length(creds)+1),jj]<-GetCredInt(Result,Likelihood,creds,var=colnames(W_results)[jj],plotgraph=FALSE)
	}

	O_results
	I_results
	W_results

	writecsv(O_results, csvname(credpath, "CredInt_processed_AAMresults_offshore", identifier), row.names=TRUE)
	writecsv(I_results, csvname(credpath, "CredInt_processed_AAMresults_inshore", identifier), row.names=TRUE)
	writecsv(W_results, csvname(credpath, "CredInt_processed_AAMresults_whole", identifier), row.names=TRUE)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        output_data<-list(offshore = O_results,
			  inshore  = I_results,
			  whole    = W_results)

	output_data
}


