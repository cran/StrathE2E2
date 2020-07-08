#
# plot_seabed_abrasion_area_ratios.R
#
#' Plot the spatial distribution of seabed abrasion rate by fishing gears
#'
#' Plots a colour-scaled matrix of the distribiution of seabed abrasion area ratio across habitats.
#'
#' The function plots a matrix of seabed habitats vs fishing gears with each cell colour-shaded to indicate abrasion area ratio (log(e) scaled proportion of habitat area abraded per day; white = 0, purple = high).
#'
#' The spatial distribution of seabed abrasion in the model is defined by three input data sets:
#' 
#' \itemize{
#'   \item Vector of whole domain activity density of each fishing gear (s/d per m2 of whole model domain)
#'   \item Matrix of the proportional distribution of whole domain activity density of each gear (rows) across seabed habitats (columns)
#'   \item Vector of seabed abrasion rate of each fishing gear (m2/s)
#' }
#'
#' The activity density of each gear in each habitat is then obtained by multiplying the whole domain activity density into the proportional
#' distribution matrix, and dividing by the area-proportions of habitats in the domain. The units of habitat-specific activity density
#' are then (s/d/m2).
#'
#' The product of activity density per gear and habitat and gear abrasion rate is then the abrasion area ratio, or abrasion rate (m2/m2/d)
#'
#' The vector of area-proportions of each habitat in the model domain is part of the moldel configuration parameter set.
#' 
#' The calculation take account of any activity multipliers set in the csv inputs to be applied to the activity density.
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

plot_seabed_abrasion_area_ratios <- function(model) {

start_par = par()$mfrow
on.exit(par(mfrow = start_par))

data                <- elt(model, "data")
physical.parameters <- elt(data,"physical.parameters")
habareas       <- elt(physical.parameters,"habitat_areas")

fleet.model<-elt(data, "fleet.model")
gear_labels <- elt(fleet.model,"gear_labels")
gear_codes <- elt(fleet.model,"gear_codes")
ACTdens <- elt(fleet.model,"gear_activity")
FGSdata <- elt(fleet.model,"gear_habitat_activity")
FGSdata <- elt(fleet.model,"gear_habitat_activity")
gear_mult <- elt(fleet.model,"gear_mult")
gear_ploughing_rate<- elt(fleet.model,"gear_ploughing_rate")

ACTdens<-ACTdens*gear_mult   # Usually gear_mult will be 1.0 for all gears but could be otherwise

#Get the proportional spatial distribution data
PSPACE<-(as.matrix(FGSdata))
rownames(PSPACE)<-gear_labels
colnames(PSPACE)<-c("S0","S1","S2","S3","D0","D1","D2","D3")

habitat_labels_l <-c("Inshore rock ",
                     "Inshore fine ",
                     "Inshore medium ",
                     "Inshore coarse ",
                     "Offshore rock ",
                     "Offshore fine ",
                     "Offshore medium ",
                     "Offshore coarse ")

#Make a matrix of the activity density by habitat
ACTSPACE<-PSPACE
ACTSPACE[,]<-NA

for(j in 1:ncol(PSPACE)){
if(habareas[j]>0) ACTSPACE[,j]<-PSPACE[,j]*ACTdens*gear_ploughing_rate/habareas[j]  #activity density * ploughing rate m2/m2/d of habitat
qq<-which(ACTSPACE[,j]==0)
ACTSPACE[qq,j]<-NA
}

#Reset zeros to MA
for(j in 1:ncol(ACTSPACE)){
for(k in 1:nrow(ACTSPACE)){
if(is.na(ACTSPACE[k,j])==FALSE){
	if(ACTSPACE[k,j]==0) ACTSPACE[k,j]<-NA
}
}
}

gear_labels_l <- gear_labels[12:1]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plotmat<-ACTSPACE
#Rows are gears columns are gears - gear1 group 1 in top left
#So we need to invert the matrix top to bottom
plotmat_i<-(plotmat)
plotmat_i[,]<-0
for(i in 1:nrow(plotmat_i)){
plotmat_i[i,]<-plotmat[(nrow(plotmat_i)-(i-1)),]
}
rownames(plotmat_i)<-rownames(plotmat)[seq(12,1,by=-1)]

#Now we need to transpose the plotmat_i matrix to get bottom-left to top-right
plotmat_it<-t(plotmat_i)

plotmat_it<-log(plotmat_it) # log transform the data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

par(mfrow=c(1,1))
par(mar=c(8,16,2,1))
    nsamples <- 20
    gridmax<-max(plotmat_it,na.rm=TRUE)
    gridmin<-min(plotmat_it,na.rm=TRUE)
    colMap <- colorRampPalette(c("white","purple" ))(nsamples)    #normal colour scale
    image(seq(1,8,by=1),seq(1,12,by=1),(plotmat_it[,1:12]),col=colMap,xaxt="n",yaxt="n",ann=FALSE)
    axis(1, labels = FALSE,tck=0)
    axis(side=2,at=seq(1,12),labels=gear_labels_l,las=1,cex.axis=0.9)
    text(1:8, par("usr")[1] - 0, srt = 45, adj = 1,labels = habitat_labels_l, xpd = TRUE,cex=1)
    mtext(bquote("Seabed abrasion rate (log"[e] ~ ".d"^-1 ~ ")"),cex=1.2,side=3,line=0.5)
    abline(v=seq(0.5,7.5,by=1))
    abline(h=seq(1.5,12.5,by=1))
    legend(grconvertX(0.35, "ndc", "user"), grconvertY(0.09, "ndc", "user"),
    c( "no abrasion",
      ((floor((gridmin+((gridmax-gridmin)/2))*100))/100),
      ((floor((gridmax)*100))/100) ),
    fill = colMap[c(1, (nsamples/2), nsamples)], ncol=3, bty="n", xpd = NA)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


retdata<-list(data=plotmat_it,
	      labels=list(gears=gear_labels_l,habitats=habitat_labels_l))

return(retdata)

}
