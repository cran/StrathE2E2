#
# disaggregate_landings_discards_by_gear.R
#
#' disaggregate group annual landings and discards by gear
#'
#' returns a model object with run and data slots
#'
#' @param fleet.model.output fleet model output
#' @param annual.landings annual landings
#'
#' @return model output with gear disaggregation
#'
#' @noRd
#
# ------------------------------------------------------------------------------

disaggregate_landings_discards_by_gear <- function(fleet.model.output, annual.landings) {

	# Previously compiled time series of annual group landimgs and discards in the dataframe : annual_group_land_disc
	#names(annual_group_land_disc)
	# [1] "year"     "PFland"   "DFQland"  "DFNQland" "MFland"   "SBland"  
	# [7] "CBland"  "CZland" "BMland" "PFdisc"   "DFQdisc"  "DFNQdisc" "MFdisc"   "SBdisc"  
	# [14] "CBdisc"  "CZdisc"   "BMdisc"

	# Previously compiled time series of ... for each group.. proportion of effort and proportion of discards
	# due to each gear, in the dataframe: gear_group_proportions

	# So, the new data frame containing ... for each group... the quantities of landings and discards for each gear
	# will have the same number of rows but 24 more columns as : gear_group_proportions
	# this is because DF will need to be split into quota and non quota

	ngears<-12
	ngroups<-10 # this is the number of groups in gear_group_proportions

	offshore_gear_group_props	<- elt(fleet.model.output, "offshore_gear_group_props")
	inshore_gear_group_props	<- elt(fleet.model.output, "inshore_gear_group_props")

	offshore_annual_group_land_disc	<- elt(annual.landings, "offshore_annual_group_land_disc")
	inshore_annual_group_land_disc	<- elt(annual.landings, "inshore_annual_group_land_disc")

	#First we need to unpack the gear_group_proportions list saved from the fleet model...
	offshore_gear_group_proportions <- data.frame(offshore_gear_group_props)
	inshore_gear_group_proportions  <- data.frame(inshore_gear_group_props)

	offshore_annual_group_gear_land_disc <- offshore_gear_group_proportions

	offshore_annual_group_gear_land_disc[,2:ncol(offshore_gear_group_proportions)] <- 0

	#Add enough extra columns to accommodate the splitting of DF into DFQ and DFNQ
	for(rr in 1:(ngears*2)) {
		offshore_annual_group_gear_land_disc[,(((ngears*ngroups*2)+1)+rr)]<-0
	}

	#Now rename all the columns..
	names(offshore_annual_group_gear_land_disc)<-c("year","PF_1_L","PF_2_L","PF_3_L","PF_4_L","PF_5_L","PF_6_L",
                                        "PF_7_L","PF_8_L","PF_9_L","PF_10_L","PF_11_L","PF_12_L",			

                                        "DFQ_1_L","DFQ_2_L","DFQ_3_L","DFQ_4_L","DFQ_5_L","DFQ_6_L",
                                        "DFQ_7_L","DFQ_8_L","DFQ_9_L","DFQ_10_L","DFQ_11_L","DFQ_12_L",
                                        "DFNQ_1_L","DFNQ_2_L","DFNQ_3_L","DFNQ_4_L","DFNQ_5_L","DFNQ_6_L",
                                        "DFNQ_7_L","DFNQ_8_L","DFNQ_9_L","DFNQ_10_L","DFNQ_11_L","DFNQ_12_L",

                                        "MF_1_L","MF_2_L","MF_3_L","MF_4_L","MF_5_L","MF_6_L",
                                        "MF_7_L","MF_8_L","MF_9_L","MF_10_L","MF_11_L","MF_12_L",
                                        "SB_1_L","SB_2_L","SB_3_L","SB_4_L","SB_5_L","SB_6_L",
                                        "SB_7_L","SB_8_L","SB_9_L","SB_10_L","SB_11_L","SB_12_L",
                                        "CB_1_L","CB_2_L","CB_3_L","CB_4_L","CB_5_L","CB_6_L",
                                        "CB_7_L","CB_8_L","CB_9_L","CB_10_L","CB_11_L","CB_12_L",

                                        "CZ_1_L","CZ_2_L","CZ_3_L","CZ_4_L","CZ_5_L","CZ_6_L",
                                        "CZ_7_L","CZ_8_L","CZ_9_L","CZ_10_L","CZ_11_L","CZ_12_L",

                                        "BD_1_L","BD_2_L","BD_3_L","BD_4_L","BD_5_L","BD_6_L",
                                        "BD_7_L","BD_8_L","BD_9_L","BD_10_L","BD_11_L","BD_12_L",

                                        "SL_1_L","SL_2_L","SL_3_L","SL_4_L","SL_5_L","SL_6_L",
                                        "SL_7_L","SL_8_L","SL_9_L","SL_10_L","SL_11_L","SL_12_L",

                                        "CT_1_L","CT_2_L","CT_3_L","CT_4_L","CT_5_L","CT_6_L",
                                        "CT_7_L","CT_8_L","CT_9_L","CT_10_L","CT_11_L","CT_12_L",

                                        "KP_1_L","KP_2_L","KP_3_L","KP_4_L","KP_5_L","KP_6_L",
                                        "KP_7_L","KP_8_L","KP_9_L","KP_10_L","KP_11_L","KP_12_L",

                                        "PF_1_D","PF_2_D","PF_3_D","PF_4_D","PF_5_D","PF_6_D",
                                        "PF_7_D","PF_8_D","PF_9_D","PF_10_D","PF_11_D","PF_12_D",

                                        "DFQ_1_D","DFQ_2_D","DFQ_3_D","DFQ_4_D","DFQ_5_D","DFQ_6_D",
                                        "DFQ_7_D","DFQ_8_D","DFQ_9_D","DFQ_10_D","DFQ_11_D","DFQ_12_D",
                                        "DFNQ_1_D","DFNQ_2_D","DFNQ_3_D","DFNQ_4_D","DFNQ_5_D","DFNQ_6_D",
                                        "DFNQ_7_D","DFNQ_8_D","DFNQ_9_D","DFNQ_10_D","DFNQ_11_D","DFNQ_12_D",

                                        "MF_1_D","MF_2_D","MF_3_D","MF_4_D","MF_5_D","MF_6_D",
                                        "MF_7_D","MF_8_D","MF_9_D","MF_10_D","MF_11_D","MF_12_D",
                                        "SB_1_D","SB_2_D","SB_3_D","SB_4_D","SB_5_D","SB_6_D",
                                        "SB_7_D","SB_8_D","SB_9_D","SB_10_D","SB_11_D","SB_12_D",
                                        "CB_1_D","CB_2_D","CB_3_D","CB_4_D","CB_5_D","CB_6_D",
                                        "CB_7_D","CB_8_D","CB_9_D","CB_10_D","CB_11_D","CB_12_D",

                                        "CZ_1_D","CZ_2_D","CZ_3_D","CZ_4_D","CZ_5_D","CZ_6_D",
                                        "CZ_7_D","CZ_8_D","CZ_9_D","CZ_10_D","CZ_11_D","CZ_12_D",

                                        "BD_1_D","BD_2_D","BD_3_D","BD_4_D","BD_5_D","BD_6_D",
                                        "BD_7_D","BD_8_D","BD_9_D","BD_10_D","BD_11_D","BD_12_D",

                                        "SL_1_D","SL_2_D","SL_3_D","SL_4_D","SL_5_D","SL_6_D",
                                        "SL_7_D","SL_8_D","SL_9_D","SL_10_D","SL_11_D","SL_12_D",

                                        "CT_1_D","CT_2_D","CT_3_D","CT_4_D","CT_5_D","CT_6_D",
                                        "CT_7_D","CT_8_D","CT_9_D","CT_10_D","CT_11_D","CT_12_D",

                                        "KP_1_D","KP_2_D","KP_3_D","KP_4_D","KP_5_D","KP_6_D",
                                        "KP_7_D","KP_8_D","KP_9_D","KP_10_D","KP_11_D","KP_12_D")

	#copy the offshore dataframe into the inshore zone
	inshore_annual_group_gear_land_disc <- offshore_annual_group_gear_land_disc

	#Loop through the ngroups+1 classes of catch
	#extra one because DF and split into Q and NQ

	for(dsa in 1:(ngroups+1)) {

		olcol1<-1+1+(dsa-1)*ngears
		olcol2<-olcol1+ngears-1
		olcol<-seq(olcol1,olcol2)

		odcol1<-((ngears*ngroups)+ngears+1)+1+(dsa-1)*ngears
		odcol2<-odcol1+ngears-1
		odcol<-seq(odcol1,odcol2)

		#olcol and odcol are vectors of column numbers in the OUTPUT matrix
		#where the data for landings and discards of each group are up be placed
		#so including DFQ and DFNQ


		#gcol is the column in the INPUT data on proportions of group effort
		#per gear, and proportions of discard quantity per gear coming
		#from the fleet model - where DF are not split into DFQ and DFNQ

		#so if dsa=3 (ie DFNQ) then gcol=2 (DF proportions of effort etc)

		gcol<-dsa
		if(dsa>2){gcol<-(dsa-1)}

		ecol1<-1+1+(gcol-1)*ngears
		ecol2<-ecol1+ngears-1
		ecol<-seq(ecol1,ecol2)

		dcol1<-((ngears*ngroups)+1)+1+(gcol-1)*ngears
		dcol2<-dcol1+ngears-1
		dcol<-seq(dcol1,dcol2)

		#ecol and dcol are vectors of column numbers in the INPUT matrix
		#of effort and discard proportions per gear which need to be applied
		#to the quantity of landings and discards per group coming out of
		#the ecology model


		lqcol<-1+dsa
		dqcol<-(ngroups+2)+dsa

		#lqcol is the column number in the ecology model output corresponding to landings of the group in question (including DNQ and DFNQ)
		#dqcol is the column number in the ecology model output corresponding to discards of the group in question (including DNQ and DFNQ)


		olcol
		ecol
		lqcol

		odcol
		dcol
		dqcol


		#c(olcol1,olcol2,odcol1,odcol2,ecol1,ecol2,dcol1,dcol2,dqcol,lqcol)

		#sum(offshore_gear_group_proportions[100,ecol])
		#sum(offshore_gear_group_proportions[100,dcol])



		for(fgh in 1:ngears){

			#Calculate the all-gears discard rate from the ecomodel output - so resolves Q and NQ DF

			AGo_discr <- offshore_annual_group_land_disc[,dqcol]/(offshore_annual_group_land_disc[,lqcol]+offshore_annual_group_land_disc[,dqcol])
			AGo_discr[which(is.na(AGo_discr))] <-0
			AGi_discr <- inshore_annual_group_land_disc[,dqcol]/(inshore_annual_group_land_disc[,lqcol]+inshore_annual_group_land_disc[,dqcol])
			AGi_discr[which(is.na(AGi_discr))] <-0

			#Now calculate the actual discard rate per gear
			Go_discr <- offshore_gear_group_proportions[,(dcol[fgh])] * AGo_discr
			Gi_discr <- inshore_gear_group_proportions[,(dcol[fgh])] * AGi_discr

			#If any of the discard rates exceed 1, reset to 1
			Go_discr[which(Go_discr>1)]<-1
			Gi_discr[which(Gi_discr>1)]<-1

			#Now disaggregate the catch in proportion to effort
			Go_catch<-(offshore_annual_group_land_disc[,lqcol]+offshore_annual_group_land_disc[,dqcol])*offshore_gear_group_proportions[,(ecol[fgh])]
			Gi_catch<-(inshore_annual_group_land_disc[,lqcol]+inshore_annual_group_land_disc[,dqcol])*inshore_gear_group_proportions[,(ecol[fgh])]

			#Then output the landings by gear...
			offshore_annual_group_gear_land_disc[,(olcol[fgh])]<-Go_catch * (1-Go_discr)
			inshore_annual_group_gear_land_disc[,(olcol[fgh])]<-Gi_catch * (1-Gi_discr)

			#And the discards by gear
			offshore_annual_group_gear_land_disc[,(odcol[fgh])]<-Go_catch * (Go_discr)
			inshore_annual_group_gear_land_disc[,(odcol[fgh])]<-Gi_catch * (Gi_discr)

		}
	}

	landings_by_gear <- list(
		offshore_annual_group_gear_land_disc	= offshore_annual_group_gear_land_disc,
		inshore_annual_group_gear_land_disc	= inshore_annual_group_gear_land_disc
	)
}

