#
# plot_gear_guild_offal_rates.R
#
#' Plot the distribution of offal generation rates across guilds.
#'
#' Plots a colour-scaled matrix of the distribiution of offal generation rates for each guild in the ecology model, due to each gear.
#'
#' The function plots a matrix of the values of offal generation rate (i.e. the proportion by weight of total catch which is returned to the sea as viscera after processing) for each guild (columns) by each gear (rows). 
#' Cells in the matrix are colour-shaded to indicate offal generation rate (range 0 - max; colour gradient: white = 0, purple = high) on a linear scale.
#'
#' The offal generation rate is a quantity derived from three input variables to the fishing fleet model:
#' \itemize{
#'   \item Matrix of the discard rate (proportion of catch weight discarded without being processed) for each guild and gear; (D); (input file '/Param/fishing_discards*.csv')
#'   \item Matrix of the processing-at-sea rate (proportion of retained catch which is processed) for each guild and gear; (P); (input file '/Param/fishing_processing_*.csv')
#'   \item Proportion by weight of viscera in the retained catch (assumed constant across all guilds); (V); (value in input file '/Param/fishing_fleet_*.csv')
#' }
#'
#' The offal generation rate is then given by (1 - D).P.V
#'
#' It is possible (and allowable) for the discard rate and processing-at-sea rate of a gear-guild combination to be set as positive numbers in the relevant input files, but nevertheless the
#' the catching power settings in the corresponding 'fishing_power' file to be set to zero - in other words the given gear does not actually catch the given guild. In such cases this
#' function resets the discard rate and processing-at-sea rate to NA for plotting purposes.
#'
#' Note that the visualization of offal rate generated by this function is based on the csv input data to the fleet model. 
#' Hence, it does not reflect any effects on discard rate arising from discarding scenarios configured in the fleet model input 
#' since these are dynamic and generated during run-time within the ecology model.
#'
#' @param model R-list object defining the model configuration compiled by the e2e_read() function
#'
#' @return Graphical display in a new graphics window and a list object comprising the plotted data and axis labels.
#'
#' @importFrom graphics grconvertX grconvertY image
#' @importFrom grDevices colorRampPalette
#'
#' @noRd
#
# ------------------------------------------------------------------------------

plot_gear_guild_offal_rates <- function(model) {

start_par = par()$mfrow
on.exit(par(mfrow = start_par))

data                <- elt(model, "data")
fleet.model<-elt(data, "fleet.model")

gear_labels <- elt(fleet.model,"gear_labels")
gear_codes <- elt(fleet.model,"gear_codes")
FGDdata <- elt(fleet.model,"gear_group_discard")
FGGdata <- elt(fleet.model,"gear_group_gutting")
power  <-  elt(fleet.model,"gear_group_rel_power")
viscera <- elt(fleet.model,"offal_prop_live_weight")

gutrate<-(1-FGDdata)*FGGdata*viscera

PSPACE<-(as.matrix(gutrate))
rownames(PSPACE)<-gear_labels
colnames(PSPACE)<-colnames(FGDdata)

group_labels_l <-c("Planktiv.fish ",
                     "Demersal fish ",
                     "Migratory fish ",
                     "Susp/dep.benthos ",
                     "Carn/scav.benthos ",
                     "Carniv.zoo ",
                     "Birds ",
                     "Pinnipeds ",
		     "Cetaceans ",
		     "Macrophytes ")

for(j in 1:ncol(PSPACE)){
for(k in 1:nrow(PSPACE)){
if(PSPACE[k,j]==0) PSPACE[k,j]<-NA
if(power[k,j]==0) PSPACE[k,j]<-NA  # if the gear does not catch the guild at all then set discard rate to NA
}
}

gear_labels_l <- gear_labels[12:1]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plotmat<-PSPACE
#Rows are gears columns are groups - gear1 group 1 in top left
#So we need to invert the matrix top to bottom
plotmat_i<-(plotmat)
plotmat_i[,]<-0
for(i in 1:nrow(plotmat_i)){
plotmat_i[i,]<-plotmat[(nrow(plotmat_i)-(i-1)),]
}
rownames(plotmat_i)<-rownames(plotmat)[seq(12,1,by=-1)]

#Now we need to transpose the plotmat_i matrix to get bottom-left to top-right
plotmat_it<-t(plotmat_i)

#plotmat_it<-log(plotmat_it) # log transform the data  # No need to log transform the offal generation rates


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(mfrow=c(1,1))
par(mar=c(8,16,2,1))
    nsamples <- 100
    gridmax<-max(plotmat_it,na.rm=TRUE)
    colMap <- colorRampPalette(c("white","purple" ))(nsamples)
    image(seq(1,10,by=1),seq(1,12,by=1),(plotmat_it[,1:12]),col=colMap,xaxt="n",yaxt="n",ann=FALSE)
    axis(1, labels = FALSE,tck=0)
    axis(side=2,at=seq(1,12),labels=gear_labels_l,las=1,cex.axis=0.9)
    text(1:10, par("usr")[1] - 0, srt = 45, adj = 1,labels = group_labels_l, xpd = TRUE,cex=1)
    mtext("Offal generation rate",cex=1.2,side=3,line=0.5)
    abline(v=seq(0.5,9.5,by=1))
    abline(h=seq(1.5,12.5,by=1))
    legend(grconvertX(0.6, "ndc", "user"), grconvertY(0.09, "ndc", "user"),
    c(0,((floor((gridmax/2)*100))/100),((floor((gridmax)*100))/100)),
    fill = colMap[c(1, (nsamples/2), nsamples)], ncol=3, bty="n", xpd = NA)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

retdata<-list(data=plotmat_it,
	      labels=list(gears=gear_labels_l,guilds=group_labels_l))

return(retdata)

}
