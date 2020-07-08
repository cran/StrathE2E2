#
# read_fishing_fleet_model.R
#
#' read fishing fleet model configuration
#'
#' @param model.path path to users model folder, otherwise read package model
#' @param physical.parameters physical model parameters
#'
#' @return model object
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_fishing_fleet_model <- function(model.path, physical.parameters) {

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Subroutine to configure the fishing fleet model
	# set the rates and parameters for a set of 12 fishing gears
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#THIS VERSION ALSO DEALS WITH THE BYCATCH OF BIRDS AND MAMMALS

	FGAdata	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FISHING_ACTIVITY_PARAMETERS)
	FGPdata	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FISHING_POWER_PARAMETERS)
	FGDdata	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FISHING_DISCARD_PARAMETERS)
	FGSdata	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FISHING_DISTRIBUTION_PARAMETERS)
	FGOdata	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FISHING_FLEET_PARAMETERS)
	FGGdata	<- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FISHING_PROCESSING_PARAMETERS)
	FGMdata <- get.model.file(model.path, PARAMETERS_DIR, file.pattern=FISHING_ACTIVITY_SCALING_VALUES)
	FGHdata <- get.model.file(model.path, PARAMETERS_DIR, file.pattern=HARVEST_RATIO_SCALING_VALUES)

	gear_mult <- as.vector(FGMdata[,3])
	HRscale_vector_multiplier<-as.vector(FGHdata[,2])


#JUST FOR TESTING...
#PFratemult<-1
#DFratemult<-1
#MFratemult<-1
#BCratemult<-1
#BSratemult<-1
#CZratemult<-1
#KPratemult<-1
#nyears<-3

gear_labels <- FGAdata[,1]
gear_codes <- FGAdata[,2]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First, provide the regional average ANNUAL fishing activity density - ie regional seconds fished / regional area (m2) / year

#ACTIVITY Values in seconds per m2 per day
gear_activity<-(rep(0,12))
gear_activity[] <- FGAdata[,3]

#Gear RELATIVE POWER values (=catch/activity kg/sec per model resource group during a calibdation data period)
#INCLUDE the commercial by-catch in here
#Units - mMN/sec activity 
gear_group_rel_power<-data.frame(rep(0,12))
names(gear_group_rel_power)<-"pelagic"
gear_group_rel_power$pelagic <- FGPdata[,3]
gear_group_rel_power$demersal<- FGPdata[,4]
gear_group_rel_power$migratory<- FGPdata[,5]
gear_group_rel_power$filtben<- FGPdata[,6]
gear_group_rel_power$carnben<- FGPdata[,7]
gear_group_rel_power$carnzoo<- FGPdata[,8]
gear_group_rel_power$bird<- FGPdata[,9]
gear_group_rel_power$seal<- FGPdata[,10]
gear_group_rel_power$ceta<- FGPdata[,11]
gear_group_rel_power$kelp<- FGPdata[,12]

#------------------------------------------------------------------------

#In the real world, Quota limited and non-quota demersal species are treated differently in terms of discarding.
#In the model we apportion the catch between quota and nonquota biomass using density dependent parameters
#derived from analysis of observed data.

#We assume that the ratio of quota/non-quota demersal species caught is the same for all gears

#Proportion of demersal catch non-quota = a * exp(-b*(1st January demersal stock biomass)))

DFpropNQscale<-FGOdata[1,1]    # derived from analysis of data from the North Sea
DFpropNQcoeff<-FGOdata[2,1]    # derived from analysis of data from the North Sea

# To disable the distinction between quota limited and non-quota species set DFpropNQscale=0. Then all demersal biomass
# will be treated as quota limited

#------------------------------------------------------------------------

#Proportion of demersal catch which is undersize fish and/or of no commercial value is not a constnat fraction,
#but an exponential function of demersal biomass, to mimic the change in length composition expressed by the LFI. 
#Proportion of catch which is undersize = a * exp(-b*demersal stock biomass))

#For NON-QUOTA limited species
DFpropNQundersizescale<- FGOdata[3,1]               #derived from analysis of North Sea data
DFpropNQundersizecoeff<- FGOdata[4,1]               #derived from analysis of North Sea data

#For QUOTA limited species
DFpropQundersizescale<- FGOdata[5,1]                #derived from analysis of North Sea data
DFpropQundersizecoeff<- FGOdata[6,1]               #derived from analysis of North Sea data


#------------------------------------------------------------------------


#ACTUAL GEAR DISCARD RATES HERE as a 12 x 9 matrix like the power data

gear_group_discard<-data.frame(rep(0,12))
names(gear_group_discard)<-"pelagic"

gear_group_discard$pelagic<-FGDdata[,3]

gear_group_discard$demersal<-FGDdata[,4]
gear_group_discard$migratory<-FGDdata[,5]
gear_group_discard$filtben<-FGDdata[,6]
gear_group_discard$carnben<-FGDdata[,7]
gear_group_discard$carnzoo<-FGDdata[,8]
gear_group_discard$bird<-FGDdata[,9]
gear_group_discard$seal<-FGDdata[,10]
gear_group_discard$ceta<-FGDdata[,11]
gear_group_discard$kelp<-FGDdata[,12]

# FOR DEMERSAL FISH ONLY, WE CAN 
# a) ASSUME THAT DISCARDED FISH CORRESPOND TO THE UNDERSIZE AND/OR NON-COMMERCIAL FRACTIONS DERIVED
#    FROM THE INTERNAL DENSITY DEPENDENT FUNCTIONS IN THE MODEL. IN THIS CASE THE PROPORTIONAL DISTRIBUTION
#    OF DISCARDS ACROSS GEARS IN THE ABOVE DATA IS PRESERVED, BUT OVERALL DISCARD RATE IS NOT.
#
# OR
#
# b) USE THE OVERALL DISCARD RATE PRESCRIBED BY THE ABOVE DATA, RETAINING THE SPLIT BETWEEN QUOTA AND NON-QUOTA
#    LIMITED SPECIES DERIVED FROM THE INTERNAL DENSITY DEPENDENT FUNCTIONS


# ENTER A SWITCH VALUE HERE (COMMENT OUT ONE OF THESE LINES AS APPROPRIATE)...........
    DFsize_SWITCH <-FGOdata[7,1]   # FOR USING THE INTERNALLY GENERATED undersize fraction and provided DF harvest rate
#    DFdiscard_SWITCH <-1   # FOR resetting the internally derived undersize fraction and attenuating the harvest rate accordingly

# ENTER A SWITCH VALUE HERE (COMMENT OUT ONE OF THESE LINES AS APPROPRIATE)...........
    DFdiscard_SWITCH <-FGOdata[8,1]   # FOR USING THE INTERNALLY GENERATED OVERALL DISCARD FRACTION
#    DFdiscard_SWITCH <-1   # FOR OVERRIDING THE INTERNALLY DERIVED DISCARD RATE WITH THE DATA PROVIDED HERE


#------------------------------------------------------------------------

#GEAR GROUP GUTTING AT SEA RATES HERE as a 12 x 10 matrix like the power data

gear_group_gutting<-data.frame(rep(0,12))
names(gear_group_gutting)<-"pelagic"

gear_group_gutting$pelagic<-FGGdata[,3]

gear_group_gutting$demersal<-FGGdata[,4]
gear_group_gutting$migratory<-FGGdata[,5]
gear_group_gutting$filtben<-FGGdata[,6]
gear_group_gutting$carnben<-FGGdata[,7]
gear_group_gutting$carnzoo<-FGGdata[,8]
gear_group_gutting$bird<-FGGdata[,9]
gear_group_gutting$seal<-FGGdata[,10]
gear_group_gutting$ceta<-FGGdata[,11]
gear_group_gutting$kelp<-FGGdata[,12]



#------------------------------------------------------------------------




#SEABED AREA PLOUGHING POWER in m2/sec
#These get multipled by the activity to produce an area proportion which is ploughed by each gear (d-1)
#and then redistributed across seabed habitats 
gear_ploughing_rate<-FGAdata[,4]

#------------------------------------------------------------------------

#SEABED PLOUGHING DEPTH - as a proportion of the thickness of the active sediment layer

plough_thickness <- FGOdata[9,1]  # Assumes that all sediment types are ploughed to a depth by fishing gears
                                  # except fo rthe kelp forst habitat which is solid rock

	x_depth_s1 <- elt(physical.parameters, "x_depth_s1")
	x_depth_s2 <- elt(physical.parameters, "x_depth_s2")
	x_depth_s3 <- elt(physical.parameters, "x_depth_s3")
	x_depth_d1 <- elt(physical.parameters, "x_depth_d1")
	x_depth_d2 <- elt(physical.parameters, "x_depth_d2")
	x_depth_d3 <- elt(physical.parameters, "x_depth_d3")

	plough_depth_s0 <- 0
	plough_depth_s1 <- ifelse(x_depth_s1>0, plough_thickness/x_depth_s1, 0)
	plough_depth_s2 <- ifelse(x_depth_s2>0, plough_thickness/x_depth_s2, 0)
	plough_depth_s3 <- ifelse(x_depth_s3>0, plough_thickness/x_depth_s3, 0)

	plough_depth_d0 <- 0
	plough_depth_d1 <- ifelse(x_depth_d1>0, plough_thickness/x_depth_d1, 0)
	plough_depth_d2 <- ifelse(x_depth_d2>0, plough_thickness/x_depth_d2, 0)
	plough_depth_d3 <- ifelse(x_depth_d3>0, plough_thickness/x_depth_d3, 0)

	plough_depth_vector <- c(
		plough_depth_s0,
		plough_depth_s1,
		plough_depth_s2,
		plough_depth_s3,
		plough_depth_d0,
		plough_depth_d1,
		plough_depth_d2,
		plough_depth_d3
	)

#------------------------------------------------------------------------

#Damage mortality inflicted on benthos....................

#values to be entered are the mortality rate inflicted per trawl pass per year
#Typical values seem to be between 0.1 and 0.6. A value of 0.2 seems sensible

#100% of this mortality goes to corpses

BSmort_gear<-FGOdata[10,1]   # damage mortality on benthos filter/deposit feeders

BCmort_gear<-FGOdata[11,1]   # damage mortality on benthos carniovores


#------------------------------------------------------------------------

#Spatial distribution of activity by each gear
#ACtivity of each gear needs to be distributed across the 7 habitat types....

# Spatial distribution of fishing in relation to sediment types
# kelp + 3 sediment types in both shallow and deep waters

gear_habitat_activity<-data.frame(rep(0,12))
names(gear_habitat_activity)<-"s0"

gear_habitat_activity$s0<-FGSdata[,3]
gear_habitat_activity$s1<-FGSdata[,4]
gear_habitat_activity$s2<-FGSdata[,5]
gear_habitat_activity$s3<-FGSdata[,6]
gear_habitat_activity$d0<-FGSdata[,7]
gear_habitat_activity$d1<-FGSdata[,8]
gear_habitat_activity$d2<-FGSdata[,9]
gear_habitat_activity$d3<-FGSdata[,10]

#THIS IS JUST FOR CHECKING THAT THE ROWS SUM TO 1
#sum(gear_habitat_activity[1,])
#sum(gear_habitat_activity[2,])
#sum(gear_habitat_activity[3,])
#sum(gear_habitat_activity[4,])
#sum(gear_habitat_activity[5,])
#sum(gear_habitat_activity[6,])
#sum(gear_habitat_activity[7,])
#sum(gear_habitat_activity[8,])
#sum(gear_habitat_activity[9,])
#sum(gear_habitat_activity[10,])
#sum(gear_habitat_activity[11,])
#sum(gear_habitat_activity[12,])

#if(sum(rowSums(gear_habitat_activity))<12) TERMINATE

#------------------------------------------------------------------------

#Scaling coefficient between regional average daily effort density and daily harvest ratio
# These need to be calculated separately - there is an R programme for doing this
  xPFeffort_HRscale    <- FGOdata[12,1]
  xDFeffort_HRscale    <- FGOdata[13,1]
  xMFeffort_HRscale    <- FGOdata[14,1]

  xSBeffort_HRscale    <- FGOdata[15,1]
  xCBeffort_HRscale    <- FGOdata[16,1]

  xCZeffort_HRscale    <- FGOdata[17,1]
  xBDeffort_HRscale    <- FGOdata[18,1]
  xSLeffort_HRscale    <- FGOdata[19,1]
  xCTeffort_HRscale    <- FGOdata[20,1]
  xKPeffort_HRscale    <- FGOdata[21,1]

HRscale_vector<-c(PF_HR_scale=xPFeffort_HRscale,
                  DF_HR_scale=xDFeffort_HRscale,
                  MF_HR_scale=xMFeffort_HRscale,
                  SB_HR_scale=xSBeffort_HRscale,
                  CB_HR_Scale=xCBeffort_HRscale,
                  CZ_HR_Scale=xCZeffort_HRscale,
                  BD_HR_Scale=xBDeffort_HRscale,
                  SL_HR_Scale=xSLeffort_HRscale,
                  CT_HR_Scale=xCTeffort_HRscale,
                  KP_HR_Scale=xKPeffort_HRscale)


#------------------------------------------------------------------------

#Offal weight as a proportion of live weight for fish which are gutted or processed at sea and offal discarded

  offal_prop_live_weight <- FGOdata[22,1]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Now we need to flatten the power and activity distribution matrices so the data can be strung together as a a vector

power_vector<-as.numeric(gear_group_rel_power[1,])
for(ii in 2:12){
power_vector<-c(power_vector,as.numeric(gear_group_rel_power[ii,]))
}

quota_nonquota_parms_vector<-as.numeric(c(DFpropNQscale,DFpropNQcoeff,DFpropNQundersizescale,DFpropNQundersizecoeff,DFpropQundersizescale,DFpropQundersizecoeff))


discard_vector<-as.numeric(gear_group_discard[1,])
for(ii in 2:12){
discard_vector<-c(discard_vector,as.numeric(gear_group_discard[ii,]))
}

gutting_at_sea_vector<-as.numeric(gear_group_gutting[1,])
for(ii in 2:12){
gutting_at_sea_vector<-c(gutting_at_sea_vector,as.numeric(gear_group_gutting[ii,]))
}

activity_distribution_vector<-as.numeric(gear_habitat_activity[1,])
for(ii in 2:12){
activity_distribution_vector<-c(activity_distribution_vector,as.numeric(gear_habitat_activity[ii,]))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	fleet.model <- list(
		gear_labels			= gear_labels,
		gear_codes			= gear_codes,
		gear_activity			= gear_activity,
		gear_group_rel_power		= gear_group_rel_power,
		gear_group_discard		= gear_group_discard,
		gear_group_gutting		= gear_group_gutting,
		gear_ploughing_rate		= gear_ploughing_rate,
		gear_habitat_activity		= gear_habitat_activity,
		HRscale_vector			= HRscale_vector,
		HRscale_vector_multiplier	= HRscale_vector_multiplier,
		offal_prop_live_weight		= offal_prop_live_weight,
		gear_mult			= gear_mult,
		quota_nonquota_parms_vector	= quota_nonquota_parms_vector,
		DFsize_SWITCH			= DFsize_SWITCH,
		DFdiscard_SWITCH		= DFdiscard_SWITCH,
		plough_thickness		= plough_thickness,
		plough_depth_vector		= plough_depth_vector,
		BSmort_gear			= BSmort_gear,
		BCmort_gear			= BCmort_gear
	)

	fleet.model
}

