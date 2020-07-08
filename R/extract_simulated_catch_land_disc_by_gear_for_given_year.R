#
# extract_simulated_catch_land_disc_by_gear_for_given_year.R
#
#' extract simulated catch
#'
#' saves to various files
#'
#' @param model model object
#' @param landings.by.gear annual catch by gear
#' @param ytp year to plot
#'
#' @noRd
#
# ------------------------------------------------------------------------------

extract_simulated_catch_land_disc_by_gear_for_given_year <- function(model, landings.by.gear, ytp=-1) {

	data		<- elt(model, "data")
	fleet.model	<- elt(data, "fleet.model")
	gear_labels	<- elt(fleet.model, "gear_labels")
    
	setup		<- elt(model, "setup")
	identifier	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

	offshore_annual_group_gear_land_disc	<- elt(landings.by.gear, "offshore_annual_group_gear_land_disc")
	inshore_annual_group_gear_land_disc	<- elt(landings.by.gear, "inshore_annual_group_gear_land_disc")
   
#showall("offshore_annual_group_gear_land_disc", offshore_annual_group_gear_land_disc)
#showall("inshore_annual_group_gear_land_disc", inshore_annual_group_gear_land_disc)
#stop("ss")

#Extracts a year of modelled landingfs and disccards disaggregated by gears

#Expects imnput dataframe  annual_group_gear_land_disc

#Produces matrices catchmat and the transpose catchmat_t, landmat and landmat_t, discmat and discmat_t


ngears<-12
ngroups<-10

#Vector of group names - NOTE 1 longer than ngroups as demersal fish split into quota and non-quota
group_labels<-c(
"Planktivorous fish",
"Quota-limited demersal fish",
"Non-quota demersal fish",
"Migratory fish",
"Susp/deposit feeding benthos",
"Carn/scavenge feeding benthos",
"Pelagic invertebrates",
"Birds",
"Pinnipeds",
"Cetaceans",
"Macrophytes")


#First the offshore data

annual_group_gear_land_disc<-offshore_annual_group_gear_land_disc

#Select a year to plot
if (ytp == -1) {
	ytp <- nrow(annual_group_gear_land_disc)
} else {
	if (ytp < 1 || ytp > nrow(annual_group_gear_land_disc)) {
		message("ytp out of range, defaulting to last year of run")
		ytp <- nrow(annual_group_gear_land_disc)
	}
}

#First need to set up matrices to hold the data for plotting

catchmat<-array(dim=c((ngroups+1),12))

rownames(catchmat)<-group_labels
#c(
#"Pelagic fish",
#"Quota-limited demersal fish",
#"Non-quota demersal fish",
#"Migratory fish",
#"Susp/deposit feeding benthos",
#"Carn/scavenge feeding benthos",
#"Pelagic invertebrates",
#"Birds,
#"Seals",
#"Cetaceans",
#"Kelp")

colnames(catchmat)<-gear_labels

landmat<-catchmat
discmat<-catchmat


((ngears*ngroups)+ngears+1)

for(dsa in 1:(ngroups+1)) {

olcol1<-1+1+(dsa-1)*ngears
olcol2<-olcol1+ngears-1
olcol<-seq(olcol1,olcol2)

odcol1<-((ngears*ngroups)+ngears+1)+1+(dsa-1)*ngears
odcol2<-odcol1+ngears-1
odcol<-seq(odcol1,odcol2)

landmat[dsa,1:ngears]<-as.numeric(annual_group_gear_land_disc[ytp,olcol])

discmat[dsa,1:ngears]<-as.numeric(annual_group_gear_land_disc[ytp,odcol])


}

catchmat<-landmat+discmat

#......................

#Make transposes of these matrices

catchmat_t<-t(catchmat)
discmat_t <-t(discmat)
landmat_t <-t(landmat)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

offshore_catchmat<-catchmat
offshore_discmat <-discmat
offshore_landmat <-landmat

offshore_catchmat_t<-catchmat_t
offshore_discmat_t <-discmat_t
offshore_landmat_t <-landmat_t

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#Second the inshore data

annual_group_gear_land_disc<-inshore_annual_group_gear_land_disc



#First need to set up matrices to hold the data for plotting

catchmat<-array(dim=c((ngroups+1),12))

rownames(catchmat)<-group_labels
#c(
#"Pelagic fish",
#"Quota-limited demersal fish",
#"Non-quota demersal fish",
#"Migratory fish",
#"Susp/deposit feeding benthos",
#"Carn/scavenge feeding benthos",
#"Pelagic invertebrates",
#"Birds",
#"Seals",
#"Cetaceans",
#"Kelp")

colnames(catchmat)<-gear_labels

landmat<-catchmat
discmat<-catchmat


((ngears*ngroups)+ngears+1)

for(dsa in 1:(ngroups+1)) {

olcol1<-1+1+(dsa-1)*ngears
olcol2<-olcol1+ngears-1
olcol<-seq(olcol1,olcol2)

odcol1<-((ngears*ngroups)+ngears+1)+1+(dsa-1)*ngears
odcol2<-odcol1+ngears-1
odcol<-seq(odcol1,odcol2)

landmat[dsa,1:ngears]<-as.numeric(annual_group_gear_land_disc[ytp,olcol])

discmat[dsa,1:ngears]<-as.numeric(annual_group_gear_land_disc[ytp,odcol])


}

catchmat<-landmat+discmat

#......................

#Make transposes of these matrices

catchmat_t<-t(catchmat)
discmat_t <-t(discmat)
landmat_t <-t(landmat)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

inshore_catchmat<-catchmat
inshore_discmat <-discmat
inshore_landmat <-landmat

inshore_catchmat_t<-catchmat_t
inshore_discmat_t <-discmat_t
inshore_landmat_t <-landmat_t


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#OUTPUT THE TRANSPOSED MATRICES TO FILES FOR STORAGE
filename = csvname(resultsdir, "OFFSHORE_catchcomposition_by_gear", identifier)
writecsv(offshore_catchmat, filename)

filename = csvname(resultsdir, "OFFSHORE_landingcomposition_by_gear", identifier)
writecsv(offshore_landmat, filename)

filename = csvname(resultsdir, "OFFSHORE_discardcomposition_by_gear", identifier)
writecsv(offshore_discmat, filename)


filename = csvname(resultsdir, "INSHORE_catchcomposition_by_gear", identifier)
writecsv(inshore_catchmat, filename)

filename = csvname(resultsdir, "INSHORE_landingcomposition_by_gear", identifier)
writecsv(inshore_landmat, filename)

filename = csvname(resultsdir, "INSHORE_discardcomposition_by_gear", identifier)
writecsv(inshore_discmat, filename)

#-------------------------------------------------------------------------------------------------------

	catch_land_disc <- list(
		inshore_catchmat	= inshore_catchmat,
		inshore_discmat		= inshore_discmat,
		inshore_landmat		= inshore_landmat,
		offshore_catchmat	= offshore_catchmat,
		offshore_landmat	= offshore_landmat,
		offshore_discmat	= offshore_discmat
	)
}

