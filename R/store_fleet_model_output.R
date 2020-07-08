#
# store_fleet_model_output.R
#
#' grab the 'gear_group_prop_effort' and 'gear_group_prob_discard' matrices, reorganise and store them
#' for use after the main model run to derive the landings and discards from each gear, from inshore and offshore zones
#'
#' returns fishing fleet model output suitably reorganised
#'
#' @param fleet_model_output output from fleet model
#' @param years_to_fill makes data fill the run length 
#'
#' @return reorganised fleet model output
#'
#' @noRd
#
# ------------------------------------------------------------------------------

store_fleet_model_output <- function(fleet_model_output, years_to_fill) {

	ngears<-12
	ngroups<-10

	#set up inshore and offshore dataframes of length 'nyears' and columns 2*ngroups*ngears + 1 = 241
	inshore_gear_group_props <- data.frame(seq(1,years_to_fill))

	for(hj in 2:(2*ngroups*ngears + 1)){
		inshore_gear_group_props[,hj]<-0
	}

	names(inshore_gear_group_props)<-c("year","PF_1_E","PF_2_E","PF_3_E","PF_4_E","PF_5_E","PF_6_E",
                                        "PF_7_E","PF_8_E","PF_9_E","PF_10_E","PF_11_E","PF_12_E",
                                        "DF_1_E","DF_2_E","DF_3_E","DF_4_E","DF_5_E","DF_6_E",
                                        "DF_7_E","DF_8_E","DF_9_E","DF_10_E","DF_11_E","DF_12_E",
                                        "MF_1_E","MF_2_E","MF_3_E","MF_4_E","MF_5_E","MF_6_E",
                                        "MF_7_E","MF_8_E","MF_9_E","MF_10_E","MF_11_E","MF_12_E",
                                        "SB_1_E","SB_2_E","SB_3_E","SB_4_E","SB_5_E","SB_6_E",
                                        "SB_7_E","SB_8_E","SB_9_E","SB_10_E","SB_11_E","SB_12_E",
                                        "CB_1_E","CB_2_E","CB_3_E","CB_4_E","CB_5_E","CB_6_E",
                                        "CB_7_E","CB_8_E","CB_9_E","CB_10_E","CB_11_E","CB_12_E",
                                        "CZ_1_E","CZ_2_E","CZ_3_E","CZ_4_E","CZ_5_E","CZ_6_E",
                                        "CZ_7_E","CZ_8_E","CZ_9_E","CZ_10_E","CZ_11_E","CZ_12_E",
                                        "BD_1_E","BD_2_E","BD_3_E","BD_4_E","BD_5_E","BD_6_E",
                                        "BD_7_E","BD_8_E","BD_9_E","BD_10_E","BD_11_E","BD_12_E",
                                        "SL_1_E","SL_2_E","SL_3_E","SL_4_E","SL_5_E","SL_6_E",
                                        "SL_7_E","SL_8_E","SL_9_E","SL_10_E","SL_11_E","SL_12_E",
                                        "CT_1_E","CT_2_E","CT_3_E","CT_4_E","CT_5_E","CT_6_E",
                                        "CT_7_E","CT_8_E","CT_9_E","CT_10_E","CT_11_E","CT_12_E",
                                        "KP_1_E","KP_2_E","KP_3_E","KP_4_E","KP_5_E","KP_6_E",
                                        "KP_7_E","KP_8_E","KP_9_E","KP_10_E","KP_11_E","KP_12_E",

                                        "PF_1_D","PF_2_D","PF_3_D","PF_4_D","PF_5_D","PF_6_D",
                                        "PF_7_D","PF_8_D","PF_9_D","PF_10_D","PF_11_D","PF_12_D",
                                        "DF_1_D","DF_2_D","DF_3_D","DF_4_D","DF_5_D","DF_6_D",
                                        "DF_7_D","DF_8_D","DF_9_D","DF_10_D","DF_11_D","DF_12_D",
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

	offshore_gear_group_props <- inshore_gear_group_props

	offshore_gear_group_prop_effort_matrix<-(as.data.frame(fleet_model_output[2]))
	offshore_gear_group_prop_discard_matrix<-(as.data.frame(fleet_model_output[4]))

	inshore_gear_group_prop_effort_matrix<-(as.data.frame(fleet_model_output[3]))
	inshore_gear_group_prop_discard_matrix<-(as.data.frame(fleet_model_output[5]))

	for(hj in 1:ngroups){
		offshore_gear_group_props[1,((2+(hj-1)*ngears):((ngears+1)+(hj-1)*ngears))] <- offshore_gear_group_prop_effort_matrix[,hj]
		offshore_gear_group_props[1,(( ((ngears*ngroups)+2)+(hj-1)*ngears):( ((ngears*ngroups)+2+ngears-1) +(hj-1)*ngears))] <- offshore_gear_group_prop_discard_matrix[,hj]

		inshore_gear_group_props[1,((2+(hj-1)*ngears):((ngears+1)+(hj-1)*ngears))] <- inshore_gear_group_prop_effort_matrix[,hj]
		inshore_gear_group_props[1,(( ((ngears*ngroups)+2)+(hj-1)*ngears):( ((ngears*ngroups)+2+ngears-1) +(hj-1)*ngears))] <- inshore_gear_group_prop_discard_matrix[,hj]
	}

	if(years_to_fill>1){
		offshore_gear_group_props[2:years_to_fill,2:((2*(ngears*ngroups))+1) ]<-offshore_gear_group_props[1,2:((2*(ngears*ngroups))+1)]
		inshore_gear_group_props[2:years_to_fill,2:((2*(ngears*ngroups))+1) ]<-inshore_gear_group_props[1,2:((2*(ngears*ngroups))+1)]
	}

	gear_groups <- list(
		offshore_gear_group_props	= offshore_gear_group_props,
		inshore_gear_group_props	= inshore_gear_group_props
	)
}

