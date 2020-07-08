#
# fishing_fleet_model.R
#
#' defines the fishing fleet model
#'
#' returns fishing fleet model
#'
#' @param model model object
#' @param nyears number of years to run
#'
#' @return fishing fleet model
#'
#' @noRd
#
# ------------------------------------------------------------------------------

fishing_fleet_model <- function(model, nyears) {

	# Unpack:
	data				<- elt(model, "data")
	initial.state			<- elt(data, "initial.state")
	fleet.model			<- elt(data, "fleet.model")
	physical.parameters		<- elt(data, "physical.parameters")

	habitat_areas			<- elt(physical.parameters, "habitat_areas")

	gear_activity			<- elt(fleet.model, "gear_activity")
	gear_group_rel_power		<- elt(fleet.model, "gear_group_rel_power")
	gear_group_discard		<- elt(fleet.model, "gear_group_discard")
	gear_group_gutting		<- elt(fleet.model, "gear_group_gutting")
	gear_ploughing_rate		<- elt(fleet.model, "gear_ploughing_rate")
	gear_habitat_activity		<- elt(fleet.model, "gear_habitat_activity")
	HRscale_vector			<- elt(fleet.model, "HRscale_vector")
	HRscale_vector_multiplier	<- elt(fleet.model, "HRscale_vector_multiplier")
	gear_mult			<- elt(fleet.model, "gear_mult")
	quota_nonquota_parms_vector	<- elt(fleet.model, "quota_nonquota_parms_vector")
	DFsize_SWITCH			<- elt(fleet.model, "DFsize_SWITCH")
	DFdiscard_SWITCH		<- elt(fleet.model, "DFdiscard_SWITCH")
	plough_depth_vector		<- elt(fleet.model, "plough_depth_vector")
	BSmort_gear			<- elt(fleet.model, "BSmort_gear")
	BCmort_gear			<- elt(fleet.model, "BCmort_gear")
	offal_prop_live_weight		<- elt(fleet.model, "offal_prop_live_weight")

	Last_January			<- 0
	RESETS				<- 0
	xstart				<- initial.state
	out				<- 0

	HRscale_vector			<- HRscale_vector*HRscale_vector_multiplier

## ------- Fishing fleet parameters -------- ##


#INPUTS REQUIRED FROM THE FLEET CONFIGURATION PROGRAMME
  #gear_activity
  #gear_group_rel_power
  #gear_group_discard
  #gear_group_gutting
  #gear_ploughing_rate
  #gear_habitat_activity
  #HRscale_vector
  #gear_mult
  #quota_nonquota_parms_vector
  #plough_depth_vector
  #BSmort_gear, BCmort_gear
  #offal_prop_live_weight

#INPUT REQUIRED FROM THE PHYSICAL CONFIGURATION PROGRAMME
  #habitat_areas

#INPUT REQUIRED FROM THE MAIN MODEL MANAGEMENT SCRIPT
  #RESETS, Last_January, xstart, out

#OUTPUTS FROM THE FLEET MODEL
  #Vector of parameters needed fof the ecology model
  #matrix of the proportional distribution of group effort across gears
  #matrix of, for each group, the ratio of gear discard rate to the effort-weighted overall discard rate

# ------------------------------
# ------------------------------
# ------------------------------

#THIS BIT IS A PLACE-HOLDER FOR FUTURE DEVELOPMENT INVOLVING MANAGEMENT DECISION RULES
#Grab the 1 January value of any state variables eg. dfish from either the start conditions or the last 1 january row of the model output to date
#Illustrated here with the value of dfish

if(RESETS==0) {
    current_dfish<-xstart["fishd"]
} else {
    current_dfish<-out$fishd[which(out[,1]==Last_January)]
}   

# ------------------------------



 ################################################## ///
 #           THE FISHING FLEET MODEL                 ///
 ################################################## ///


#Apply the group fishing multiplers - - these are activity scaling factors used to generate scenarios of fishing activity
# The scaling factors are applied across groups of gears which mainly target the same resource groups in the model

gear_activity_mult<-gear_activity * gear_mult
#Activity density - units s/m2/d

#-------------------------------------------------------------

#Divide the activity density between inshore and offshore subregions
#To preserve the activity density units of s/m2/d in each subregion we need to divide by the areas
#4 habitats inshore to account for kelp forests

inshore_gear_activity_mult  <- (gear_activity_mult*rowSums(gear_habitat_activity[,1:4]))/sum(habitat_areas[1:4])
offshore_gear_activity_mult <- (gear_activity_mult*rowSums(gear_habitat_activity[,5:8]))/sum(habitat_areas[5:8])
#gear_habitat_activity has rows = gears columns = habitats, cells = proportion of activity by each gear in habitat, rowsums=1
#Units still s/m2/d

#-------------------------------------------------------------

#Derive the effort density for each gear ( = activity density * power)
#Units of group rel power = mMN/sec; rows = gears columns = groups.

gear_group_effort <- gear_activity_mult*gear_group_rel_power
# columns are : pelagic,  demersal, migratory, filtben,  carnben,  carnzoo, bird, seal, ceta, kelp; rows are gears
# comparison between gears (down the columns) is meaningful. Comparison between groups (along rows) is not
group_effort<-colSums(gear_group_effort)
#Units of effort density = mMN/m2/d (for stock abundance during a calibration period when harvest ratio is known)


#Repeat for inshore and offshore
inshore_gear_group_effort <- inshore_gear_activity_mult*gear_group_rel_power
# columns are : pelagic,  demersal, migratory, filtben,  carnben,  carnzoo, bird,  seal,  ceta, kelp
inshore_group_effort<-colSums(inshore_gear_group_effort)
#Units of effort density = mMN/m2/d

offshore_gear_group_effort <- offshore_gear_activity_mult*gear_group_rel_power
# columns are : pelagic,  demersal, migratory, filtben,  carnben,  carnzoo, bird,  seal,  ceta, kelp (though kelp not meaningful offshore)
offshore_group_effort<-colSums(offshore_gear_group_effort)
#Units of effort density = mMN/m2/d

#-------------------------------------------------------------

#Derive the proportional distribution of effort density for each group across gears
#needed for disaggregating the simulated total landings and discards after the model run
#rows = gears, columns = groups, colsums = 1

gear_group_prop_effort<-gear_group_effort
for(jj in 1:ncol(gear_group_effort)){
	if(group_effort[jj]>0){
	gear_group_prop_effort[,jj]<-gear_group_prop_effort[,jj]/group_effort[jj]
	} else {
	gear_group_prop_effort[,jj]<-0}
}

#Repeat for inshore and offshore
inshore_gear_group_prop_effort<-inshore_gear_group_effort
for(jj in 1:ncol(inshore_gear_group_effort)){
	if(inshore_group_effort[jj]>0){
	inshore_gear_group_prop_effort[,jj]<-inshore_gear_group_prop_effort[,jj]/inshore_group_effort[jj]
	} else {
	inshore_gear_group_prop_effort[,jj]<-0}
}

offshore_gear_group_prop_effort<-offshore_gear_group_effort
for(jj in 1:ncol(offshore_gear_group_effort)){
	if(offshore_group_effort[jj]>0){
	offshore_gear_group_prop_effort[,jj]<-offshore_gear_group_prop_effort[,jj]/offshore_group_effort[jj]
	} else {
	offshore_gear_group_prop_effort[,jj]<-0}
}

#-------------------------------------------------------------

#Derive the harvest ratios on each resource group ( = total effort * HRscaling)
Fx_daily<-group_effort * HRscale_vector

#Repeat for inshore and offshore
inshore_Fx_daily <- inshore_group_effort * HRscale_vector
offshore_Fx_daily<- offshore_group_effort * HRscale_vector

#-------------------------------------------------------------

#Calculate the gear effort-weighted overall discard rate of each resource group

gear_group_effort_times_discard<-gear_group_effort * gear_group_discard
gear_group_effortdiscard<-0*gear_group_effort_times_discard
for(jj in 1:ncol(gear_group_effort_times_discard)){
	if(group_effort[jj]>0){
	gear_group_effortdiscard[,jj]<-gear_group_effort_times_discard[,jj]/group_effort[jj]
	} else {
	gear_group_effortdiscard[,jj]<-0}
}
group_discard<-colSums(gear_group_effortdiscard)

#Repeat for inshore and offshore
inshore_gear_group_effort_times_discard<-inshore_gear_group_effort * gear_group_discard
inshore_gear_group_effortdiscard<-0*inshore_gear_group_effort_times_discard
for(jj in 1:ncol(inshore_gear_group_effort_times_discard)){
	if(inshore_group_effort[jj]>0){
	inshore_gear_group_effortdiscard[,jj]<-inshore_gear_group_effort_times_discard[,jj]/inshore_group_effort[jj]
	} else {
	inshore_gear_group_effortdiscard[,jj]<-0}
}
inshore_group_discard<-colSums(inshore_gear_group_effortdiscard)

offshore_gear_group_effort_times_discard<-offshore_gear_group_effort * gear_group_discard
offshore_gear_group_effortdiscard<-0*offshore_gear_group_effort_times_discard
for(jj in 1:ncol(offshore_gear_group_effort_times_discard)){
	if(offshore_group_effort[jj]>0){
	offshore_gear_group_effortdiscard[,jj]<-offshore_gear_group_effort_times_discard[,jj]/offshore_group_effort[jj]
	} else {
	offshore_gear_group_effortdiscard[,jj]<-0}
}
offshore_group_discard<-colSums(offshore_gear_group_effortdiscard)



#-------------------------------------------------------------


#Calculate the gear effort-weighted overall gutting-at-sea rate of each resource group

gear_group_effort_times_gutting<-gear_group_effort * gear_group_gutting
gear_group_effortgutting<-0*gear_group_effort_times_gutting
for(jj in 1:ncol(gear_group_effort_times_gutting)){
	if(group_effort[jj]>0){
	gear_group_effortgutting[,jj]<-gear_group_effort_times_gutting[,jj]/group_effort[jj]
	} else {
	gear_group_effortgutting[,jj]<-0}
}
group_gutting<-colSums(gear_group_effortgutting)

#Repeat for inshore and offshore
inshore_gear_group_effort_times_gutting<-inshore_gear_group_effort * gear_group_gutting
inshore_gear_group_effortgutting<-0*inshore_gear_group_effort_times_gutting
for(jj in 1:ncol(inshore_gear_group_effort_times_gutting)){
	if(inshore_group_effort[jj]>0){
	inshore_gear_group_effortgutting[,jj]<-inshore_gear_group_effort_times_gutting[,jj]/inshore_group_effort[jj]
	} else {
	inshore_gear_group_effortgutting[,jj]<-0}
}
inshore_group_gutting<-colSums(inshore_gear_group_effortgutting)

offshore_gear_group_effort_times_gutting<-offshore_gear_group_effort * gear_group_gutting
offshore_gear_group_effortgutting<-0*offshore_gear_group_effort_times_gutting
for(jj in 1:ncol(offshore_gear_group_effort_times_gutting)){
	if(offshore_group_effort[jj]>0){
	offshore_gear_group_effortgutting[,jj]<-offshore_gear_group_effort_times_gutting[,jj]/offshore_group_effort[jj]
	} else {
	offshore_gear_group_effortgutting[,jj]<-0}
}
offshore_group_gutting<-colSums(offshore_gear_group_effortgutting)



#-------------------------------------------------------------


#Calculate the proportional distribution of discard quantities across gears for each resource group
#Columns sum to 1 in the matrix gear_group_prop_discard

effort_time_discard_colsum <- colSums(gear_group_effort_times_discard)
gear_group_prop_discard<-0*gear_group_effort_times_discard
for(jj in 1:ncol(gear_group_prop_discard)){
	if(effort_time_discard_colsum[jj]>0){
	gear_group_prop_discard[,jj]<-gear_group_effort_times_discard[,jj]/effort_time_discard_colsum[jj]
	} else {
	gear_group_prop_discard[,jj]<-0}
}

#Just a bit of plotting for a reality check - compare propn distribution of effort and discards
#cc<-10
#plot(seq(1,12),gear_group_prop_discard[,cc],type="l",ylim=c(0,1))
#lines(rep(1:12),gear_group_prop_effort[,cc],type="p")

#Repeat for inshore and offshore
inshore_effort_time_discard_colsum <- colSums(inshore_gear_group_effort_times_discard)
inshore_gear_group_prop_discard<-0*inshore_gear_group_effort_times_discard
for(jj in 1:ncol(inshore_gear_group_prop_discard)){
	if(inshore_effort_time_discard_colsum[jj]>0){
	inshore_gear_group_prop_discard[,jj]<-inshore_gear_group_effort_times_discard[,jj]/inshore_effort_time_discard_colsum[jj]
	} else {
	inshore_gear_group_prop_discard[,jj]<-0}
}

offshore_effort_time_discard_colsum <- colSums(offshore_gear_group_effort_times_discard)
offshore_gear_group_prop_discard<-0*offshore_gear_group_effort_times_discard
for(jj in 1:ncol(offshore_gear_group_prop_discard)){
	if(offshore_effort_time_discard_colsum[jj]>0){
	offshore_gear_group_prop_discard[,jj]<-offshore_gear_group_effort_times_discard[,jj]/offshore_effort_time_discard_colsum[jj]
	} else {
	offshore_gear_group_prop_discard[,jj]<-0}
}

#-------------------------------------------------------------

#Generate matrices of the ratio of gear-discard rate to the regional effort weighted all-gear discard rate of each group

gear_to_region_discard_rate_ratio  <-  gear_group_discard
   for(jj in 1:(ncol(gear_group_discard))) {
   	gear_to_region_discard_rate_ratio[,jj]<-0
   }
   for(jj in 1:ncol(gear_group_discard)){
      if(group_discard[jj]>0){
      gear_to_region_discard_rate_ratio[,jj] <- gear_group_discard[,jj] / group_discard[jj] 
      } else {
      gear_to_region_discard_rate_ratio[,jj]<-0
      }
      }


#Inshore
inshore_gear_to_region_discard_rate_ratio  <-  gear_group_discard
   for(jj in 1:(ncol(gear_group_discard))) {
   	inshore_gear_to_region_discard_rate_ratio[,jj]<-0
   }
   for(jj in 1:ncol(gear_group_discard)){
      if(inshore_group_discard[jj]>0){
      inshore_gear_to_region_discard_rate_ratio[,jj] <- gear_group_discard[,jj] / inshore_group_discard[jj] 
      } else {
      inshore_gear_to_region_discard_rate_ratio[,jj]<-0
      }
      }


#Offshore
offshore_gear_to_region_discard_rate_ratio  <-  gear_group_discard
   for(jj in 1:(ncol(gear_group_discard))) {
   	offshore_gear_to_region_discard_rate_ratio[,jj]<-0
   }
   for(jj in 1:ncol(gear_group_discard)){
      if(offshore_group_discard[jj]>0){
      offshore_gear_to_region_discard_rate_ratio[,jj] <- gear_group_discard[,jj] / offshore_group_discard[jj] 
      } else {
      offshore_gear_to_region_discard_rate_ratio[,jj]<-0
      }
      }



#-------------------------------------------------------------


   #For each group get the proportion of effort in each habitat
   group_effort_prop_habitat<-data.frame(rep(0,ncol(gear_habitat_activity)))
   for(jj in 2:(ncol(gear_group_effort))) {
   	group_effort_prop_habitat[,jj]<-0
   }
   names(group_effort_prop_habitat)<-names(gear_group_effort)
   rownames(group_effort_prop_habitat)<-names(gear_habitat_activity)
   for(jj in 1:ncol(gear_group_effort)){
   	if(group_effort[jj]>0){
   	group_effort_prop_habitat[,jj]<-(colSums(gear_group_effort[,jj]*gear_habitat_activity)) /   group_effort[jj]
   	} else {
   	group_effort_prop_habitat[,jj]<-0}
   }   

  #Extract separate matrices for inshore (4 rows) and offshore (4 rows), in which the columns still sum to 1 - ie its
  #the proportionm distribution of effort across habitats WITHIN inshore and offshore zones

  inshore_group_effort_prop_habitat  <- (group_effort_prop_habitat[1:4,])
  offshore_group_effort_prop_habitat <- (group_effort_prop_habitat[5:8,])
  for(jj in 1:4){
  inshore_group_effort_prop_habitat[jj,]  <- inshore_group_effort_prop_habitat[jj,]/(colSums(group_effort_prop_habitat[1:4,]))
  }
  for(jj in 1:4){
  offshore_group_effort_prop_habitat[jj,]  <- offshore_group_effort_prop_habitat[jj,]/(colSums(group_effort_prop_habitat[5:8,]))
  }




#-------------------------------------------------------------

#For each group get the proportion of total quantity discarded over each habitat
#In the matrix group_discard_prop_habitat rows=habitats columns = groups, so colsums = 1
group_discard_prop_habitat<-data.frame(rep(0,ncol(gear_habitat_activity)))
for(jj in 2:(ncol(gear_group_effort))) {
	group_discard_prop_habitat[,jj]<-0
}
names(group_discard_prop_habitat)<-names(gear_group_effort)
rownames(group_discard_prop_habitat)<-names(gear_habitat_activity)
for(jj in 1:ncol(gear_group_effort)){
	if(group_effort[jj] & group_discard[jj] >0){
	group_discard_prop_habitat[,jj]<-(colSums(gear_group_effortdiscard[,jj]*gear_habitat_activity)) /   group_discard[jj]
	} else {
	group_discard_prop_habitat[,jj]<-0}
}   

  #Extract separate matrices for inshore (4 row) and offshore (4 row), in which the columns still sum to 1 - ie its
  #the proportional distribution of discard quantity across habitats WITHIN inshore and offshore zones

inshore_group_discard_prop_habitat<-data.frame(rep(0,ncol(gear_habitat_activity[,1:4])))
for(jj in 2:(ncol(inshore_gear_group_effort))) {
	inshore_group_discard_prop_habitat[,jj]<-0
}
names(inshore_group_discard_prop_habitat)<-names(inshore_gear_group_effort)
rownames(inshore_group_discard_prop_habitat)<-names(gear_habitat_activity[1:4])
for(jj in 1:ncol(inshore_gear_group_effort)){
	if(inshore_group_effort[jj] & inshore_group_discard[jj] >0){
	inshore_group_discard_prop_habitat[,jj]<-(colSums(inshore_gear_group_effortdiscard[,jj]*gear_habitat_activity[,1:4]/rowSums(gear_habitat_activity[,1:4],na.rm=TRUE),na.rm=TRUE)) /   inshore_group_discard[jj]
	} else {
	inshore_group_discard_prop_habitat[,jj]<-0}
}   

offshore_group_discard_prop_habitat<-data.frame(rep(0,ncol(gear_habitat_activity[,5:8])))
for(jj in 2:(ncol(offshore_gear_group_effort))) {
	offshore_group_discard_prop_habitat[,jj]<-0
}
names(offshore_group_discard_prop_habitat)<-names(offshore_gear_group_effort)
rownames(offshore_group_discard_prop_habitat)<-names(gear_habitat_activity[5:8])
for(jj in 1:ncol(offshore_gear_group_effort)){
	if(offshore_group_effort[jj] & offshore_group_discard[jj] >0){
	offshore_group_discard_prop_habitat[,jj]<-(colSums(offshore_gear_group_effortdiscard[,jj]*gear_habitat_activity[,5:8]/rowSums(gear_habitat_activity[,5:8],na.rm=TRUE),na.rm=TRUE)) /   offshore_group_discard[jj]
	} else {
	offshore_group_discard_prop_habitat[,jj]<-0}
}   

#-------------------------------------------------------------



#For each group get the proportion of total quantity of offal produced over each habitat
#In the matrix group_gutting_prop_habitat rows=habitats columns = groups, so colsums = 1
group_gutting_prop_habitat<-data.frame(rep(0,ncol(gear_habitat_activity)))
for(jj in 2:(ncol(gear_group_effort))) {
	group_gutting_prop_habitat[,jj]<-0
}
names(group_gutting_prop_habitat)<-names(gear_group_effort)
rownames(group_gutting_prop_habitat)<-names(gear_habitat_activity)
for(jj in 1:ncol(gear_group_effort)){
	if(group_effort[jj] & group_gutting[jj] >0){
	group_gutting_prop_habitat[,jj]<-(colSums(gear_group_effortgutting[,jj]*gear_habitat_activity)) /   group_gutting[jj]
	} else {
	group_gutting_prop_habitat[,jj]<-0}
}   

  #Extract separate matrices for inshore (4 row) and offshore (4 row), in which the columns still sum to 1 - ie its
  #the proportional distribution of gutting quantity across habitats WITHIN inshore and offshore zones

inshore_group_gutting_prop_habitat<-data.frame(rep(0,ncol(gear_habitat_activity[,1:4])))
for(jj in 2:(ncol(inshore_gear_group_effort))) {
	inshore_group_gutting_prop_habitat[,jj]<-0
}
names(inshore_group_gutting_prop_habitat)<-names(inshore_gear_group_effort)
rownames(inshore_group_gutting_prop_habitat)<-names(gear_habitat_activity[1:4])
for(jj in 1:ncol(inshore_gear_group_effort)){
	if(inshore_group_effort[jj] & inshore_group_gutting[jj] >0){
	inshore_group_gutting_prop_habitat[,jj]<-(colSums(inshore_gear_group_effortgutting[,jj]*gear_habitat_activity[,1:4]/rowSums(gear_habitat_activity[,1:4],na.rm=TRUE),na.rm=TRUE)) /   inshore_group_gutting[jj]
	} else {
	inshore_group_gutting_prop_habitat[,jj]<-0}
}   

offshore_group_gutting_prop_habitat<-data.frame(rep(0,ncol(gear_habitat_activity[,5:8])))
for(jj in 2:(ncol(offshore_gear_group_effort))) {
	offshore_group_gutting_prop_habitat[,jj]<-0
}
names(offshore_group_gutting_prop_habitat)<-names(offshore_gear_group_effort)
rownames(offshore_group_gutting_prop_habitat)<-names(gear_habitat_activity[5:8])
for(jj in 1:ncol(offshore_gear_group_effort)){
	if(offshore_group_effort[jj] & offshore_group_gutting[jj] >0){
	offshore_group_gutting_prop_habitat[,jj]<-(colSums(offshore_gear_group_effortgutting[,jj]*gear_habitat_activity[,5:8]/rowSums(gear_habitat_activity[,5:8],na.rm=TRUE),na.rm=TRUE)) /   offshore_group_gutting[jj]
	} else {
	offshore_group_gutting_prop_habitat[,jj]<-0}
}   

#-------------------------------------------------------------







#Calculate the daily ploughing rate in each habitat (proportion of habitat area per day)
#To preserve the activity density units of s/m2/d in each subregion we need to divide by the areas
plough_daily_habitat<-rep(0,ncol(gear_habitat_activity))
for(jj in 1:ncol(gear_habitat_activity)){
	if(habitat_areas[jj]>0){
	plough_daily_habitat[jj]<-(sum(gear_activity_mult * gear_habitat_activity[,jj] * gear_ploughing_rate)) /   habitat_areas[jj]
	} else {
	plough_daily_habitat[jj]<-0}
}   

#-------------------------------------------------------------

# Calculate the mortality of benthos due to damage by passage of the trawl gear
# This is given by the area-weighted mortality per trawl pass x the proportion of total seabed area ploughed per day
bensdamage <- BSmort_gear * sum(plough_daily_habitat * habitat_areas)
bencdamage <- BCmort_gear * sum(plough_daily_habitat * habitat_areas)

#Inshore and offshore
inshore_bensdamage  <- BSmort_gear * (sum(plough_daily_habitat[1:4] * habitat_areas[1:4]))/sum(habitat_areas[1:4])
offshore_bensdamage <- BSmort_gear * (sum(plough_daily_habitat[5:8] * habitat_areas[5:8]))/sum(habitat_areas[5:8])

inshore_bencdamage  <- BCmort_gear * (sum(plough_daily_habitat[1:4] * habitat_areas[1:4]))/sum(habitat_areas[1:4])
offshore_bencdamage <- BCmort_gear * (sum(plough_daily_habitat[5:8] * habitat_areas[5:8]))/sum(habitat_areas[5:8])


#-------------------------------------------------------------



################################


#STUFF THAT NEEDS TO BE OUTPUT FROM THE ROUTINE AND PASSED TO THE ECOLOGY MODEL

#NEED TO COMPILE COMPOSITE VECTORS OF THE INSHORE AND OFFSHORE HARVEST RATES FOR FISH AND BENTHOS

#Compile a vector of daily harvest ratios - inshore and offshore rates for each group except birtds&mammals
Fx_daily_out<-c(inshore_Fx_daily["pelagic"],offshore_Fx_daily["pelagic"],
                inshore_Fx_daily["demersal"],offshore_Fx_daily["demersal"],
                inshore_Fx_daily["migratory"],offshore_Fx_daily["migratory"],
                inshore_Fx_daily["filtben"],offshore_Fx_daily["filtben"],
                inshore_Fx_daily["carnben"],offshore_Fx_daily["carnben"],
                inshore_Fx_daily["carnzoo"],offshore_Fx_daily["carnzoo"],
                inshore_Fx_daily["bird"],offshore_Fx_daily["bird"],
                inshore_Fx_daily["seal"],offshore_Fx_daily["seal"],
                inshore_Fx_daily["ceta"],offshore_Fx_daily["ceta"],
                inshore_Fx_daily["kelp"],offshore_Fx_daily["kelp"])

#Compile a vector of discard rates - inshore and offshore rates for all group except birtds&mammals, plus whole region for demersal
group_discard_out<-c(inshore_group_discard["pelagic"],offshore_group_discard["pelagic"],
                     inshore_group_discard["demersal"],offshore_group_discard["demersal"],group_discard["demersal"],
                     inshore_group_discard["migratory"],offshore_group_discard["migratory"],
                     inshore_group_discard["filtben"],offshore_group_discard["filtben"],
                     inshore_group_discard["carnben"],offshore_group_discard["carnben"],
                     inshore_group_discard["carnzoo"],offshore_group_discard["carnzoo"],
                     inshore_group_discard["bird"],offshore_group_discard["bird"],
                     inshore_group_discard["seal"],offshore_group_discard["seal"],
                     inshore_group_discard["ceta"],offshore_group_discard["ceta"],
                     inshore_group_discard["kelp"],offshore_group_discard["kelp"])

#Compile a vector of gutting rates - inshore and offshore rates for all group except birtds&mammals, plus whole region for demersal
group_gutting_out<-c(inshore_group_gutting["pelagic"],offshore_group_gutting["pelagic"],
                     inshore_group_gutting["demersal"],offshore_group_gutting["demersal"],
                     inshore_group_gutting["migratory"],offshore_group_gutting["migratory"],
                     inshore_group_gutting["filtben"],offshore_group_gutting["filtben"],
                     inshore_group_gutting["carnben"],offshore_group_gutting["carnben"],
                     inshore_group_gutting["carnzoo"],offshore_group_gutting["carnzoo"],
                     inshore_group_gutting["bird"],offshore_group_gutting["bird"],
                     inshore_group_gutting["seal"],offshore_group_gutting["seal"],
                     inshore_group_gutting["ceta"],offshore_group_gutting["ceta"],
                     inshore_group_gutting["kelp"],offshore_group_gutting["kelp"])


#Compile a vector of proportions of discard quantity over each habitat WITHIN the inshore and offshore subregions
#except for birds and mamals where its the proportions across all habitats

discard_map<- c(

inshore_group_discard_prop_habitat[,1],  # pelagic fish distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,1], # pelagic fish distribution of discard quantity across 4 offshore habitats

inshore_group_discard_prop_habitat[,2],  # demersal fish distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,2], # demersal fish distribution of discard quantity across 4 offshore habitats

inshore_group_discard_prop_habitat[,3],  # migratory fish distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,3], # migratory fish distribution of discard quantity across 4 offshore habitats

inshore_group_discard_prop_habitat[,4],  # inshire filtben distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,4], # offshore filtben distribution of discard quantity across 4 offshore habitats

inshore_group_discard_prop_habitat[,5],  # inshore carnben distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,5], # offshore carnben distribution of discard quantity across 4 offshore habitats

inshore_group_discard_prop_habitat[,6],  # carnzoo distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,6], # carnzoo distribution of discard quantity across 4 offshore habitats

inshore_group_discard_prop_habitat[,7],  # bird distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,7],  # bird distribution of discard quantity across 4 offshore habitats

inshore_group_discard_prop_habitat[,8],  # seal distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,8],  # seal distribution of discard quantity across 4 offshore habitats

inshore_group_discard_prop_habitat[,9],  # ceta distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,9],  # ceta distribution of discard quantity across 4 offshore habitats

inshore_group_discard_prop_habitat[,10],  # kelp distribution of discard quantity across 4 inshore habitats
offshore_group_discard_prop_habitat[,10])  # kelp distribution of discard quantity across 4 offshore habitats



#Compile a vector of proportions of discard quantity over each habitat WITHIN the inshore and offshore subregions
#except for birds and mamals where its the proportions across all habitats

gutting_map<- c(

inshore_group_gutting_prop_habitat[,1],  # pelagic fish distribution of offal quantity across 4 inshore habitats
offshore_group_gutting_prop_habitat[,1], # pelagic fish distribution of offal quantity across 4 offshore habitats

inshore_group_gutting_prop_habitat[,2],  # demersal fish distribution of offal quantity across 4 inshore habitats
offshore_group_gutting_prop_habitat[,2], # demersal fish distribution of offal quantity across 4 offshore habitats

inshore_group_gutting_prop_habitat[,3],  # migratory fish distribution of offal quantity across 4 inshore habitats
offshore_group_gutting_prop_habitat[,3], # migratory fish distribution of offal quantity across 4 offshore habitats

inshore_group_gutting_prop_habitat[,4],  # inshire filtben distribution of offal quantity across 4 inshore habitats
offshore_group_gutting_prop_habitat[,4], # offshore filtben distribution of offal quantity across 4 offshore habitats

inshore_group_gutting_prop_habitat[,5],  # inshore carnben distribution of offal quantity across 4 inshore habitats
offshore_group_gutting_prop_habitat[,5], # offshore carnben distribution of offal quantity across 4 offshore habitats

inshore_group_gutting_prop_habitat[,6],  # carnzoo distribution of gutting offal across 4 inshore habitats
offshore_group_gutting_prop_habitat[,6], # carnzoo distribution of gutting offal across 4 offshore habitats

inshore_group_gutting_prop_habitat[,7],  # bird distribution of gutting offal across 4 inshore habitats
offshore_group_gutting_prop_habitat[,7],  # bird distribution of gutting offal across 4 offshore habitats

inshore_group_gutting_prop_habitat[,8],  # seal distribution of gutting offal across 4 inshore habitats
offshore_group_gutting_prop_habitat[,8],  # seal distribution of gutting offal across 4 offshore habitats

inshore_group_gutting_prop_habitat[,9],  # ceta distribution of gutting offal across 4 inshore habitats
offshore_group_gutting_prop_habitat[,9],  # ceta distribution of gutting offal across 4 offshore habitats

inshore_group_gutting_prop_habitat[,10],  # kelp distribution of gutting offal across 4 inshore habitats
offshore_group_gutting_prop_habitat[,10])  # kelp distribution of gutting offal across 4 offshore habitats






		#.........................

		#Compile all the above elements into a single vector that will be piped into the ecology model (6 more elements than the orignal version)

		fleet_vector<-c(

			Fx_daily_out,

			quota_nonquota_parms_vector,

			group_discard_out,

			group_gutting_out,

			DFsize_SWITCH,

			DFdiscard_SWITCH,

			discard_map,

			gutting_map,

			plough_daily_habitat,

			inshore_bensdamage,
			offshore_bensdamage,
			inshore_bencdamage,
			offshore_bencdamage,

			offal_prop_live_weight,

			plough_depth_vector
		)


		names(fleet_vector)<-c(
			"F_inshore_pelagic","F_offshore_pelagic",
                       "F_inshore_demersal","F_offshore_demersal",
                       "F_inshore_migratory","F_offshore_migratory",
                       "F_inshore_filtben","F_offshore_filtben",
                       "F_inshore_carnben","F_offshore_carnben",
                       "F_inshore_carnzoo","F_offshore_carnzoo",
                       "F_inshore_bird","F_offshore_bird",
                       "F_inshore_seal","F_offshore_seal",
                       "F_inshore_ceta","F_offshore_ceta",
                       "F_inshore_kelp","F_offshore_kelp",

                       "QnQ_coef","QnQ_exp","NQus_coef","NQus_exp","QLus_coef","QLus_exp",

                       "D_inshore_pelagic","D_offshore_pelagic",
                       "D_inshore_demersal","D_offshore_demersal","D_demersal",
                       "D_inshore_migratory","D_offshore_migratory",
                       "D_inshore_filtben","D_offshore_filtben",
                       "D_inshore_carnben","D_offshore_carnben",
                       "D_inshore_carnzoo","D_offshore_carnzoo",
                       "D_inshore_bird","D_offshore_bird",
                       "D_inshore_seal","D_offshore_seal",
                       "D_inshore_ceta","D_offshore_ceta",
                       "D_inshore_kelp","D_offshore_kelp",

                       "G_inshore_pelagic","G_offshore_pelagic",
                       "G_inshore_demersal","G_offshore_demersal",
                       "G_inshore_migratory","G_offshore_migratory",
                       "G_inshore_filtben","G_offshore_filtben",
                       "G_inshore_carnben","G_offshore_carnben",
                       "G_inshore_carnzoo","G_offshore_carnzoo",
                       "G_inshore_bird","G_offshore_bird",
                       "G_inshore_seal","G_offshore_seal",
                       "G_inshore_ceta","G_offshore_ceta",
                       "G_inshore_kelp","G_offshore_kelp",

                       "Undersizeswitch",
                       "Discardswitch",

                        "PelpD_S0","PelpD_S1","PelpD_S2","PelpD_S3","PelpD_D0","PelpD_D1","PelpD_D2","PelpD_D3",
                        "DempD_S0","DempD_S1","DempD_S2","DempD_S3","DempD_D0","DempD_D1","DempD_D2","DempD_D3",
                        "MigpD_S0","MigpD_S1","MigpD_S2","MigpD_S3","MigpD_D0","MigpD_D1","MigpD_D2","MigpD_D3",
                        "IBfpD_S0","IBfpD_S1","IBfpD_S2","IBfpD_S3","OBfpD_D0","OBfpD_D1","OBfpD_D2","OBfpD_D3",
                        "IBcpD_S0","IBcpD_S1","IBcpD_S2","IBcpD_S3","OBcpD_D0","OBcpD_D1","OBcpD_D2","OBcpD_D3",
                        "CzopD_S0","CzopD_S1","CzopD_S2","CzopD_S3","CzopD_D0","CzopD_D1","CzopD_D2","CzopD_D3",
                        "BirpD_S0","BirpD_S1","BirpD_S2","BirpD_S3","BirpD_D0","BirpD_D1","BirpD_D2","BirpD_D3",
                        "SeapD_S0","SeapD_S1","SeapD_S2","SeapD_S3","SeapD_D0","SeapD_D1","SeapD_D2","SeapD_D3",
                        "CetpD_S0","CetpD_S1","CetpD_S2","CetpD_S3","CetpD_D0","CetpD_D1","CetpD_D2","CetpD_D3",
                        "KelpD_S0","KelpD_S1","KelpD_S2","KelpD_S3","KelpD_D0","KelpD_D1","KelpD_D2","KelpD_D3",

                        "PelpG_S0","PelpG_S1","PelpG_S2","PelpG_S3","PelpG_D0","PelpG_D1","PelpG_D2","PelpG_D3",
                        "DempG_S0","DempG_S1","DempG_S2","DempG_S3","DempG_D0","DempG_D1","DempG_D2","DempG_D3",
                        "MigpG_S0","MigpG_S1","MigpG_S2","MigpG_S3","MigpG_D0","MigpG_D1","MigpG_D2","MigpG_D3",
                        "IBfpG_S0","IBfpG_S1","IBfpG_S2","IBfpG_S3","OBfpG_D0","OBfpG_D1","OBfpG_D2","OBfpG_D3",
                        "IBcpG_S0","IBcpG_S1","IBcpG_S2","IBcpG_S3","OBcpG_D0","OBcpG_D1","OBcpG_D2","OBcpG_D3",
                        "CzopG_S0","CzopG_S1","CzopG_S2","CzopG_S3","CzopG_D0","CzopG_D1","CzopG_D2","CzopG_D3",
                        "BirpG_S0","BirpG_S1","BirpG_S2","BirpG_S3","BirpG_D0","BirpG_D1","BirpG_D2","BirpG_D3",
                        "SeapG_S0","SeapG_S1","SeapG_S2","SeapG_S3","SeapG_D0","SeapG_D1","SeapG_D2","SeapG_D3",
                        "CetpG_S0","CetpG_S1","CetpG_S2","CetpG_S3","CetpG_D0","CetpG_D1","CetpG_D2","CetpG_D3",
                        "KelpG_S0","KelpG_S1","KelpG_S2","KelpG_S3","KelpG_D0","KelpG_D1","KelpG_D2","KelpG_D3",

                        "ploughdaily_S0","ploughdaily_S1","ploughdaily_S2","ploughdaily_S3","ploughdaily_D0","ploughdaily_D1","ploughdaily_D2","ploughdaily_D3",

                        "inshore_bensdamage","offshore_bensdamage",
                        "inshore_bencdamage","offshore_bencdamage",

                        "offal_prop_live_weight",

                        "ploughdepth_S0","ploughdepth_S1","ploughdepth_S2","ploughdepth_S3","ploughdepth_D0","ploughdepth_D1","ploughdepth_D2","ploughdepth_D3"
		)


#STUFF THAT NEEDS TO BE SAVED FOR POST-PROCESSING OF THE ECOLOGY MODEL OUTPUT

	fleet_model_output <- list(
		fleet_vector					= fleet_vector,
		offshore_gear_group_prop_effort			= offshore_gear_group_prop_effort,
		inshore_gear_group_prop_effort			= inshore_gear_group_prop_effort,
		offshore_gear_to_region_discard_rate_ratio	= offshore_gear_to_region_discard_rate_ratio,
		inshore_gear_to_region_discard_rate_ratio	= inshore_gear_to_region_discard_rate_ratio
	)

	gear_group_props <- store_fleet_model_output(fleet_model_output, nyears)

	# concat the lists:
	fishing_fleet_model_output <- c(
		fleet_model_output,
		gear_group_props
	)

	fishing_fleet_model_output
}


