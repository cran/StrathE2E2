#
# read_physical_parameters.R
#
#' set up the physical configuration of the model
#'
#' Data required are the vertical thicknesses of the surface, deep and sediment layer,
#' sediment porosity, and the diffusion coefficients for exchange between the sediment and water
#' The parameters are read from the "physical_parameters.csv" file.
#'
#' @param model.path path to model
#'
#' @return default physical parameters
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_physical_parameters <- function(model.path) {

	PCdata <- get.model.file(model.path, PARAMETERS_DIR, file.pattern=PHYSICAL_PARAMETERS)

	#Set water column layer thicknesses (metres), benthic boundary layer thickness,
	so_depth	<- PCdata[1,1]		# offshore surface layer thickness
	d_depth		<- PCdata[2,1]		# offshore deep layer thickness
	si_depth	<- PCdata[3,1]		# inshore shallow layer thickness
	bx_depth	<- PCdata[4,1]		# bottom boundary layer thickness for benthos feeding

	# Here, specify the proportion of total seabed area accounted for by each sediment habitat type
	# ---- THESE PROPORTIONS MUST SUM TO 1 ----
	# Sediment areas as proportion of total area
	# This set of values defines a North Sea model
	x_area_s0	<- PCdata[5,1]		# shallow kelp habitat proportion of region by area
	x_area_s1	<- PCdata[6,1]		# shallow cohesive sediment proportion of region by area
	x_area_s2	<- PCdata[7,1]		# shallow sandy sediment proportion of region by area
	x_area_s3	<- PCdata[8,1]		# shallow coarse proportion of region by area
	x_area_d0	<- PCdata[9,1]		# deep cohesive sediment proportion of region by area
	x_area_d1	<- PCdata[10,1]		# deep cohesive sediment proportion of region by area
	x_area_d2	<- PCdata[11,1]		# deep sandy sediment proportion of region by area
	x_area_d3	<- PCdata[12,1]		# deep coarse sediment proportion of region by area

	#Proportion of seabed area in contact with the surface water column layer
	x_shallowprop	<- x_area_s0 + x_area_s1 + x_area_s2 + x_area_s3

	habitat_areas	<- c(x_area_s0,x_area_s1,x_area_s2,x_area_s3,x_area_d0,x_area_d1,x_area_d2,x_area_d3)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	xvolume_si<-si_depth*x_shallowprop
	xvolume_so<-so_depth*(1-x_shallowprop)
	xd_volume<-d_depth*(1-x_shallowprop)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#Median grain size of sediments (mm)  #<------------------------------------------------ provide values here if using option 1
	#To specify bedrock set the griansize=0. This will trigger 0's for porosity, permeability and sediment layer thickness
	grainsize_s1<-PCdata[13,1]          # shallow sediment 1 median grain size
	grainsize_s2<-PCdata[14,1]          # shallow sediment 2 median grain size
	grainsize_s3<-PCdata[15,1]          # shallow sediment 3 median grain size
	grainsize_d1<-PCdata[16,1]           # deep sediment 1 median grain size
	grainsize_d2<-PCdata[17,1]           # deep sediment 2 median grain size
	grainsize_d3<-PCdata[18,1]          # deep sediment 3 median grain size


	#The rates of mineralisation, nitrification and denitrification in the sediment are linked to
	#permeabiity in the model. The parameter list requires values for these rates at a reference grain size
	#(ie. reference permeability). Give here the grain size of the reference sediment for which these rate
	#parameters are quoted. The parameter list will also include a sensitivity paramater for how the geochemical
	#rates chnage with log-hydraulic conductivity
	ref_grain_size<-PCdata[19,1]   # <------------------------------- ref grain size in mm


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#Parameters for relationships to impute porosity, permeability and TN% from gran size
	#-----------------------------------------------------------------------------------

	#Parameters for relationship between porosity and grain size (mm) derived from literature data
	gspor_p1<-PCdata[20,1]
	gspor_p2<-PCdata[21,1]
	gspor_p3<-PCdata[22,1]
	gspor_p4<-PCdata[23,1]

	#Parameters for relationship between permeability (m-2) and grain size (mm) derived from Serpetti thesis data
	gsperm_p1<-PCdata[24,1]
	gsperm_p2<-PCdata[25,1]

	MUDpc_p1<-PCdata[26,1]
	MUDpc_p2<-PCdata[27,1]

	TONpc_p1<-PCdata[28,1]
	TONpc_p2<-PCdata[29,1]
	TONpc_p3<-PCdata[30,1]  # shallow water TON vs global estimate
	TONpc_p4<-PCdata[31,1]  # deep water TON vs global estimate
	TON_p_refractory<-PCdata[32,1]  # proportion of measured total sediment nitrogen assumed to be refractory


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#Set the sediment sediment porosity
	#Either:
	#  0) let the code calculate porosity
	#or
	#  1) provide the porosity of each sediment
	# Set the switch here to 0 or 1
	calc_poros_properties<-PCdata[33,1]       #<-------------------------


	if(calc_poros_properties==0){

		if(grainsize_s1>0){
  			x_poros_s1<- 10^( gspor_p3 + gspor_p4 * (1/ (1+exp(-(log10(grainsize_s1)-gspor_p1)/gspor_p2))) )
		} else {
  			x_poros_s1<-0
		}

		if(grainsize_s2>0){
  			x_poros_s2<- 10^( gspor_p3 + gspor_p4 * (1/ (1+exp(-(log10(grainsize_s2)-gspor_p1)/gspor_p2))) )
		} else {
  			x_poros_s2<-0
		}

		if(grainsize_s3>0){
  			x_poros_s3<- 10^( gspor_p3 + gspor_p4 * (1/ (1+exp(-(log10(grainsize_s3)-gspor_p1)/gspor_p2))) )
		} else {
  			x_poros_s3<-0
		}

		if(grainsize_d1>0){
  			x_poros_d1<- 10^( gspor_p3 + gspor_p4 * (1/ (1+exp(-(log10(grainsize_d1)-gspor_p1)/gspor_p2))) )
		} else {
  			x_poros_d1<-0
		}

		if(grainsize_d2>0){
  			x_poros_d2<- 10^( gspor_p3 + gspor_p4 * (1/ (1+exp(-(log10(grainsize_d2)-gspor_p1)/gspor_p2))) )
		} else {
  			x_poros_d2<-0
		}

		if(grainsize_d3>0){
  			x_poros_d3<- 10^( gspor_p3 + gspor_p4 * (1/ (1+exp(-(log10(grainsize_d3)-gspor_p1)/gspor_p2))) )
		} else {
  			x_poros_d3<-0
		}

	}

	if(calc_poros_properties==1){

		#Pre-determined porosities of shallow and deep sediments  #<-- provide values here if using option 1
		x_poros_s1<- PCdata[34,1]
		x_poros_s2<- PCdata[35,1]
		x_poros_s3<- PCdata[36,1]
		x_poros_d1<- PCdata[37,1]
		x_poros_d2<- PCdata[38,1]
		x_poros_d3<- PCdata[39,1]

	}


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#Set the sediment permeability
	#Either:
	#  0) let the code calculate permeability
	#or
	#  1) provide the permeability of each sediment
	# Set the switch here to 0 or 1
	calc_permeab_properties<-PCdata[40,1]       #<-------------------------

	if(calc_permeab_properties==0){

		permcal_gs_s1<-grainsize_s1
		permcal_gs_s2<-grainsize_s2
		permcal_gs_s3<-grainsize_s3
		permcal_gs_d1<-grainsize_d1
		permcal_gs_d2<-grainsize_d2
		permcal_gs_d3<-grainsize_d3

		permcal_gs_lo <-0.11
		permcal_gs_hi <-0.5
		if(permcal_gs_s1<permcal_gs_lo) permcal_gs_s1<-permcal_gs_lo
		if(permcal_gs_s2<permcal_gs_lo) permcal_gs_s2<-permcal_gs_lo
		if(permcal_gs_s3<permcal_gs_lo) permcal_gs_s3<-permcal_gs_lo
		if(permcal_gs_d1<permcal_gs_lo) permcal_gs_d1<-permcal_gs_lo
		if(permcal_gs_d2<permcal_gs_lo) permcal_gs_d2<-permcal_gs_lo
		if(permcal_gs_d3<permcal_gs_lo) permcal_gs_d3<-permcal_gs_lo
 
		if(permcal_gs_s1<permcal_gs_hi) permcal_gs_s1<-permcal_gs_hi
		if(permcal_gs_s2<permcal_gs_hi) permcal_gs_s2<-permcal_gs_hi
		if(permcal_gs_s3<permcal_gs_hi) permcal_gs_s3<-permcal_gs_hi
		if(permcal_gs_d1<permcal_gs_hi) permcal_gs_d1<-permcal_gs_hi
		if(permcal_gs_d2<permcal_gs_hi) permcal_gs_d2<-permcal_gs_hi
		if(permcal_gs_d3<permcal_gs_hi) permcal_gs_d3<-permcal_gs_hi

		permab_s1<- (10^gsperm_p1)*(grainsize_s1^gsperm_p2)
		permab_s2<- (10^gsperm_p1)*(grainsize_s2^gsperm_p2)
		permab_s3<- (10^gsperm_p1)*(grainsize_s3^gsperm_p2)
		permab_d1<- (10^gsperm_p1)*(grainsize_d1^gsperm_p2)
		permab_d2<- (10^gsperm_p1)*(grainsize_d2^gsperm_p2)
		permab_d3<- (10^gsperm_p1)*(grainsize_d3^gsperm_p2)

	}


	if(calc_permeab_properties==1){

		#Average permeabilities (m-2) of shallow and deep sediments  #<-------------------------- provide values here if using option 2
		permab_s1<- PCdata[41,1]
		permab_s2<- PCdata[42,1]
		permab_s3<- PCdata[43,1]
		permab_d1<- PCdata[44,1]
		permab_d2<- PCdata[45,1]
		permab_d3<- PCdata[46,1]

	}



	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#Set the sediment Total nitrogen%
	#Either:
	#  0) let the code calculate TN%
	#or
	#  1) provide the TN% of each sediment
	# Set the switch here to 0 or 1
	calc_TN_properties<-PCdata[47,1]       #<-------------------------


	if(calc_TN_properties==0){


		if(grainsize_s1>0){
			MUD <- 10^(MUDpc_p1) * (grainsize_s1 ^ (MUDpc_p2))
			if(MUD>100) {MUD<-100}
			TONpc_s1 <- 10^(TONpc_p1) * (MUD ^ (TONpc_p2))
		} else {
  			TONpc_s1<-0
		}

		if(grainsize_s2>0){
			MUD <- 10^(MUDpc_p1) * (grainsize_s2 ^ (MUDpc_p2))
			if(MUD>100) {MUD<-100}
			TONpc_s2 <- 10^(TONpc_p1) * (MUD ^ (TONpc_p2))
		} else {
  			TONpc_s2<-0
		}

		if(grainsize_s3>0){
			MUD <- 10^(MUDpc_p1) * (grainsize_s3 ^ (MUDpc_p2))
			if(MUD>100) {MUD<-100}
			TONpc_s3 <- 10^(TONpc_p1) * (MUD ^ (TONpc_p2))
		} else {
  			TONpc_s3<-0
		}

		#===================================================
		# Apply refractory proportion and shallow water scaling to sediment TON content  ----------------------- >
		RONpc_s1 <- TON_p_refractory*(TONpc_s1 * TONpc_p3)
		RONpc_s2 <- TON_p_refractory*(TONpc_s2 * TONpc_p3)
		RONpc_s3 <- TON_p_refractory*(TONpc_s3 * TONpc_p3)
		#===================================================

		if(grainsize_d1>0){
			MUD <- 10^(MUDpc_p1) * (grainsize_d1 ^ (MUDpc_p2))
			if(MUD>100) {MUD<-100}
			TONpc_d1 <- 10^(TONpc_p1) * (MUD ^ (TONpc_p2))
		} else {
	  		TONpc_d1<-0
		}

		if(grainsize_d2>0){
			MUD <- 10^(MUDpc_p1) * (grainsize_d2 ^ (MUDpc_p2))
			if(MUD>100) {MUD<-100}
			TONpc_d2 <- 10^(TONpc_p1) * (MUD ^ (TONpc_p2))
		} else {
  			TONpc_d2<-0
		}

		if(grainsize_d3>0){
			MUD <- 10^(MUDpc_p1) * (grainsize_d3 ^ (MUDpc_p2))
			if(MUD>100) {MUD<-100}
			TONpc_d3 <- 10^(TONpc_p1) * (MUD ^ (TONpc_p2))
		} else {
	  		TONpc_d3<-0
		}

		#===================================================
		# Apply refractory proportion and deep water scaling to sediment TON content ----------------------- >
		RONpc_d1 <- TON_p_refractory*(TONpc_d1 * TONpc_p4)
		RONpc_d2 <- TON_p_refractory*(TONpc_d2 * TONpc_p4)
		RONpc_d3 <- TON_p_refractory*(TONpc_d3 * TONpc_p4)
		#===================================================

	}

	if(calc_TN_properties==1){

		#Average Refractory N (%DW) of shallow and deep sediments  #<-------------------------- provide values here if using option 2
		RONpc_s1<- PCdata[48,1] * TON_p_refractory
		RONpc_s2<- PCdata[49,1] * TON_p_refractory
		RONpc_s3<- PCdata[50,1] * TON_p_refractory
		RONpc_d1<- PCdata[51,1] * TON_p_refractory
		RONpc_d2<- PCdata[52,1] * TON_p_refractory
		RONpc_d3<- PCdata[53,1] * TON_p_refractory

	}


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	# Extract the proportion of seabed in each layer which is not rock
	# set flag for rock (0) or not rock (1)

	if (x_poros_s1==0) {
  		x_rock_s1=0
	} else {
  		x_rock_s1=1
	}

	if (x_poros_s2==0) {
  		x_rock_s2=0
	} else {
  		x_rock_s2=1
	}


	if (x_poros_s3==0) {
  		x_rock_s3=0
	} else {
  		x_rock_s3=1
	}


	if (x_poros_d1==0) {
  		x_rock_d1=0
	} else {
  		x_rock_d1=1
	}


	if (x_poros_d2==0) {
  		x_rock_d2=0
	} else {
  		x_rock_d2=1
	}


	if (x_poros_d3==0) {
  		x_rock_d3=0
	} else {
  		x_rock_d3=1
	}

	x_nonrock_s=(x_area_s1*x_rock_s1 + x_area_s2*x_rock_s2 + x_area_s3*x_rock_s3);
	x_nonrock_d=(x_area_d1*x_rock_d1 + x_area_d2*x_rock_d2 + x_area_d3*x_rock_d3);
	#THIS SUMS UP ALL THE PERMEABLE SEDIMENT AREAS AND ACCOUNTS FOR THE EVENTUALITY THAT ONE OR MORE SEDIMENT
	#TYPES HAS BEEN CLASSIFIED AS ROCK IN THE SETUP IN ADDITION TO THE DEFINED INSHORE AND ROFFSHORE ROCK HABITATS

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#Convert permeabilities to hydraulic conductivity (m/2)

	if(permab_s1>0){
  		Kxw_s1 <-( permab_s1 * 6800400 )
	} else {
  		Kxw_s1<-0
	}

	if(permab_s2>0){
		  Kxw_s2 <-( permab_s2 * 6800400 )
	} else {
  		Kxw_s2<-0
	}

	if(permab_s3>0){
  		Kxw_s3 <-( permab_s3 * 6800400 )
	} else {
  		Kxw_s3<-0
	}

	if(permab_d1>0){
  		Kxw_d1 <-( permab_d1 * 6800400 )
	} else {
  		Kxw_d1<-0
	}

	if(permab_d2>0){
  		Kxw_d2 <-( permab_d2 * 6800400 )
	} else {
  		Kxw_d2<-0
	}

	if(permab_d3>0){
  		Kxw_d3 <-( permab_d3 * 6800400 )
	} else {
  		Kxw_d3<-0
	}

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#Values for permeability and hydraulic conductivity at the reference grain size for geochemical parameters

	ref_permab<- (10^gsperm_p1)*(ref_grain_size^gsperm_p2)
	ref_Kxw <- ( ref_permab * 6800400 )

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



	#Switch for whether to calculate the sediment layer thickness internally from the hydraulic condictivity
	#or whether to let the user define thsi thickness themselves
	#So, for muddy impermeable sediments the effective layer thickness is smaller than for 
	#coarse permeable sediments. There is a minimum layer thickness of 0.1m
	#Either:
	#  0) let the code calculate layer thickness
	#or
	#  1) provide the layer thickness of each sediment
	# Set the switch here to 0 or 1
	#(0=calculated_internally_(RECOMMENDED) / 1=defined_here_as_values_(AT_YOUR_PERIL))

	calc_sed_thickness<-PCdata[54,1]   #<----------------



	if(calc_sed_thickness==0){

		Kxw_limit<-(10^(-4.5))       #<----------------<<<<<<<<<<<<<<<< for OLD NORTH SEA
		#Kxw_limit<-(10^(-4.8))        #<----------------<<<<<<<<<<<<<<<< for new runs using ERSEM outputs and Roberts sediment maps data

		if(Kxw_s1>Kxw_limit) Kxw_s1<-Kxw_limit
		if(Kxw_s1>0) x_depth_s1<-max(0.1,(0.1)+0.5*(log10(Kxw_s1)+6.5))
		if(Kxw_s1==0) x_depth_s1<-0

		if(Kxw_s2>Kxw_limit) Kxw_s2<-Kxw_limit
		if(Kxw_s2>0) x_depth_s2<-max(0.1,(0.1)+0.5*(log10(Kxw_s2)+6.5))
		if(Kxw_s2==0) x_depth_s2<-0

		if(Kxw_s3>Kxw_limit) Kxw_s3<-Kxw_limit
		if(Kxw_s3>0) x_depth_s3<-max(0.1,(0.1)+0.5*(log10(Kxw_s3)+6.5))
		if(Kxw_s3==0) x_depth_s3<-0


		if(Kxw_d1>Kxw_limit) Kxw_d1<-Kxw_limit
		if(Kxw_d1>0) x_depth_d1<-max(0.1,(0.1)+0.5*(log10(Kxw_d1)+6.5))
		if(Kxw_d1==0) x_depth_d1<-0

		if(Kxw_d2>Kxw_limit) Kxw_d2<-Kxw_limit
		if(Kxw_d2>0) x_depth_d2<-max(0.1,(0.1)+0.5*(log10(Kxw_d2)+6.5))
		if(Kxw_d2==0) x_depth_d2<-0

		if(Kxw_d3>Kxw_limit) Kxw_d3<-Kxw_limit
		if(Kxw_d3>0) x_depth_d3<-max(0.1,(0.1)+0.5*(log10(Kxw_d3)+6.5))
		if(Kxw_d3==0) x_depth_d3<-0

	}

	if(calc_sed_thickness==1){

		# You can override these calculated sediment thickness parameter values by un-commenting any of the lines below
		# and entering your own preferred values manually - BUT BE VERY CAREFUL - YOU CAN PRECIPITATE VERY LONG RUN TIMES DUE TO
		# THE ADAPTIVE TIME STEPPING AT HIGH PERMEABILITIES IF THE LAYER THICKNESS IS TOO SMALL
       
		# sediment layer thickness - typical value 0.1m (for muds), 1m for gravels
 		x_depth_s1<-PCdata[55,1]
 		x_depth_s2<-PCdata[56,1]
 		x_depth_s3<-PCdata[57,1]
 		x_depth_d1<-PCdata[58,1]
 		x_depth_d2<-PCdata[59,1]
 		x_depth_d3<-PCdata[60,1]
	
	}

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




	#Parameters setting the proportion of the thickness of the active sediment layer which is disturbed by
	#natural erosion or which is subject to bio-irrigation
	#-----------------------------------------------------

	fauna_thickness <-PCdata[61,1]  # assumes that fauna live in the top 5cm of sediment
	scour_thickness <-PCdata[62,1]  # assumes that erosion scours the top 1cm of sediment


	if(x_depth_s1>0){
		xbioturb_depth_s1 <- fauna_thickness/x_depth_s1
	} else {
		xbioturb_depth_s1 <- 0
	}

	if(x_depth_s2>0){
		xbioturb_depth_s2 <- fauna_thickness/x_depth_s2
	} else {
		xbioturb_depth_s2 <- 0
	}

	if(x_depth_s3>0){
		xbioturb_depth_s3 <- fauna_thickness/x_depth_s3
	} else {
		xbioturb_depth_s3 <- 0
	}

	if(x_depth_d1>0){
		xbioturb_depth_d1 <- fauna_thickness/x_depth_d1
	} else {
		xbioturb_depth_d1 <- 0
	}

	if(x_depth_d2>0){
		xbioturb_depth_d2 <- fauna_thickness/x_depth_d2
	} else {
		xbioturb_depth_d2 <- 0
	}

	if(x_depth_d3>0){
		xbioturb_depth_d3 <- fauna_thickness/x_depth_d3
	} else {
		xbioturb_depth_d3 <- 0
	}


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	if(x_depth_s1>0){
		xerosion_depth_s1 <- scour_thickness/x_depth_s1
	} else {
		xerosion_depth_s1 <- 0
	}

	if(x_depth_s2>0){
		xerosion_depth_s2 <- scour_thickness/x_depth_s2
	} else {
		xerosion_depth_s2 <- 0
	}

	if(x_depth_s3>0){
		xerosion_depth_s3 <- scour_thickness/x_depth_s3
	} else {
		xerosion_depth_s3 <- 0
	}

	if(x_depth_d1>0){
		xerosion_depth_d1 <- scour_thickness/x_depth_d1
	} else {
		xerosion_depth_d1 <- 0
	}

	if(x_depth_d2>0){
		xerosion_depth_d2 <- scour_thickness/x_depth_d2
	} else {
		xerosion_depth_d2 <- 0
	}

	if(x_depth_d3>0){
		xerosion_depth_d3 <- scour_thickness/x_depth_d3
	} else {
		xerosion_depth_d3 <- 0
	}


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#Convert RON% values for each sediment type into the mass of fixed refractory organic N per habitat
	#convert gN to mMN
	#convert sediemnt dry weight into sediment dry volume using dry mineral matter density
	#convert dry sedimet volume into wet volume using porosity
	#convert mMN/m3 wet volume into mMN per habitat using layer area and thickness

	x_xR_detritus_s1<- (RONpc_s1 * 1000 * 1000 * 2650 * x_area_s1 * x_depth_s1 * (1-x_poros_s1)) / (14 * 100)
	x_xR_detritus_s2<- (RONpc_s2 * 1000 * 1000 * 2650 * x_area_s2 * x_depth_s2 * (1-x_poros_s2)) / (14 * 100)
	x_xR_detritus_s3<- (RONpc_s3 * 1000 * 1000 * 2650 * x_area_s3 * x_depth_s3 * (1-x_poros_s3)) / (14 * 100)

	x_xR_detritus_d1<- (RONpc_d1 * 1000 * 1000 * 2650 * x_area_d1 * x_depth_d1 * (1-x_poros_d1)) / (14 * 100)
	x_xR_detritus_d2<- (RONpc_d2 * 1000 * 1000 * 2650 * x_area_d2 * x_depth_d2 * (1-x_poros_d2)) / (14 * 100)
	x_xR_detritus_d3<- (RONpc_d3 * 1000 * 1000 * 2650 * x_area_d3 * x_depth_d3 * (1-x_poros_d3)) / (14 * 100)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#Parameters for estimating light attenuation coefficient (base e - Beers law) from SPM
	#These are just passed directly to the model parameter vector

	xlightSPM_intercept<-PCdata[63,1]  # intercept of the liner relationship between y=light attenuation and x=SPM
	xlightSPM_slope    <-PCdata[64,1]  # slope of the liner relationship between y=light attenuation and x=SPM

	xinshore_phyt_prop_depth <-PCdata[65,1]  # proportion of inshore depth layer occupied by phytoplankton

	xinshore_kelp_prop_depth <-PCdata[66,1]  # proportion of inshore depth layer occupied by kelp   #####################


	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	physical.parms <- list(
		so_depth		= so_depth,
		d_depth			= d_depth,
		si_depth		= si_depth,
		bx_depth		= bx_depth,

		x_depth_s1		= x_depth_s1,
		x_depth_s2		= x_depth_s2,
		x_depth_s3		= x_depth_s3,
		x_depth_d1		= x_depth_d1,
		x_depth_d2		= x_depth_d2,
		x_depth_d3		= x_depth_d3,

		x_area_s0		= x_area_s0,
		x_area_s1		= x_area_s1,
		x_area_s2		= x_area_s2,
		x_area_s3		= x_area_s3,
		x_area_d0		= x_area_d0,
		x_area_d1		= x_area_d1,
		x_area_d2		= x_area_d2,
		x_area_d3		= x_area_d3,

		x_rock_s1		= x_rock_s1,
		x_rock_s2		= x_rock_s2,
		x_rock_s3		= x_rock_s3,
		x_rock_d1		= x_rock_d1,
		x_rock_d2		= x_rock_d2,
		x_rock_d3		= x_rock_d3,

		x_nonrock_s		= x_nonrock_s,
		x_nonrock_d		= x_nonrock_d,

		x_poros_s1		= x_poros_s1,
		x_poros_s2		= x_poros_s2,
		x_poros_s3		= x_poros_s3,
		x_poros_d1		= x_poros_d1,
		x_poros_d2		= x_poros_d2,
		x_poros_d3		= x_poros_d3,

		Kxw_s1			= Kxw_s1,
		Kxw_s2			= Kxw_s2,
		Kxw_s3			= Kxw_s3,
		Kxw_d1			= Kxw_d1,
		Kxw_d2			= Kxw_d2,
		Kxw_d3			= Kxw_d3,
		
		xbioturb_depth_s1	= xbioturb_depth_s1,
		xbioturb_depth_s2	= xbioturb_depth_s2,
		xbioturb_depth_s3	= xbioturb_depth_s3,
		xbioturb_depth_d1	= xbioturb_depth_d1,
		xbioturb_depth_d2	= xbioturb_depth_d2,
		xbioturb_depth_d3	= xbioturb_depth_d3,

		xerosion_depth_s1	= xerosion_depth_s1,
		xerosion_depth_s2	= xerosion_depth_s2,
		xerosion_depth_s3	= xerosion_depth_s3,
		xerosion_depth_d1	= xerosion_depth_d1,
		xerosion_depth_d2	= xerosion_depth_d2,
		xerosion_depth_d3	= xerosion_depth_d3,

		xlightSPM_intercept	= xlightSPM_intercept,
		xlightSPM_slope		= xlightSPM_slope,

		xinshore_phyt_prop_depth= xinshore_phyt_prop_depth,
		xinshore_kelp_prop_depth= xinshore_kelp_prop_depth,

		x_xR_detritus_s1	= x_xR_detritus_s1,
		x_xR_detritus_s2	= x_xR_detritus_s2,
		x_xR_detritus_s3	= x_xR_detritus_s3,

		x_xR_detritus_d1	= x_xR_detritus_d1,
		x_xR_detritus_d2	= x_xR_detritus_d2,
		x_xR_detritus_d3	= x_xR_detritus_d3,

		ref_Kxw			= ref_Kxw,

		x_shallowprop		= x_shallowprop,

		habitat_areas		= habitat_areas
	)
}

