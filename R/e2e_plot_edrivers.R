#
# e2e_plot_edrivers.R
#
#' Plot climatological year of environmental driving data.
#'
#' Multi-panel time series plots of climatological annual cycles of driving data as provided in the input csv files.
#'
#' The function plots a multi-panel page of time series plots of monthly values of the environmental driving data for the model.
#'
#' Units for the plotted variables are as follows:
#'
#' \itemize{
#'   \item Sea surface irradiance: uE/m2/d
#'   \item Suspended particulate matter: g/m3
#'   \item Temperature: deg-C
#'   \item Vertical diffusivity gradient: m/d (derived from the vertical diffusivity (m2/s) and mixing length scale (m))
#'   \item External inflows: m3 per m2 sea surface of model domain (derived from proportion input per layer volume, layer thicknesses and areas)
#'   \item River discharge: m3 per m2 sea surface of model domain (derived from proportion input to inshore volume, and inshore layer thickness and area)
#'   \item Inshore significant wave height: m
#'   \item Proportion of seabed disturbed: /d (aggregated over the three sediment classes in each zone)
#'   \item External boundary nitrate concentration: mMN/m3
#'   \item External boundary ammonia concentration: mMN/m3
#'   \item External boundary phytoplankton concentration: mMN/m3
#'   \item External boundary detritus concentration: mMN/m3
#'   \item River nitrate concentration: mMN/m3
#'   \item River ammonia concentration: mMN/m3
#'   \item Atmospheric nitrate deposition flux: mMN/m2/d
#'   \item Atmospheric ammonia deposition flux: mMN/m2/d
#' }
#'
#' @param model R-list object defining the model configuration compiled by the e2e_read() function.
#'
#' @return Graphical display in a new graphics window. Does not return any data object since the data plotted are all available as input csv files.
#'
#' @seealso \code{\link{e2e_read}}, \code{\link{e2e_run}}, \code{\link{e2e_plot_fdrivers}}
#'
#' @export
#'
#' @examples
#' # Load the 2003-2013 version of the North Sea model supplied with the package:
#'     model <- e2e_read("North_Sea", "2003-2013")
#' # Plot the annual cyles of driving data
#'     e2e_plot_edrivers(model)
#'
#' # Direct the graphics output to a pdf file ... 
#' # or jpeg("plot.jpg"), png("plot.png")
#'     pdf(file.path(tempdir(), "plot.pdf"),width=8,height=6)
#'     e2e_plot_edrivers(model)
#'     dev.off()
#'
# ---------------------------------------------------------------------
# |                                                                   |
# | Authors: Mike Heath, Ian Thurlbeck                                |
# | Department of Mathematics and Statistics                          |
# | University of Strathclyde, Glasgow                                |
# |                                                                   |
# | Date of this version: May 2020                                    |
# |                                                                   |
# ---------------------------------------------------------------------

e2e_plot_edrivers <- function(model) {

   oo <- options()
   on.exit(options(oo))

start_par = par()$mfrow
on.exit(par(mfrow = start_par))

data                <- elt(model, "data")
physics.drivers     <- elt(data,"physics.drivers")
chemistry.drivers   <- elt(data,"chemistry.drivers")
physical.parameters <- elt(data,"physical.parameters")
habitat_areas       <- elt(physical.parameters,"habitat_areas")
so_depth            <- elt(physical.parameters,"so_depth")
d_depth             <- elt(physical.parameters,"d_depth")
si_depth            <- elt(physical.parameters,"si_depth")

shallowprop         <- sum(habitat_areas[1:4])

#What's available

#physics.drivers
# [1] "month"              "sslight"            "so_logespm"        
# [4] "si_logespm"         "so_temp"            "d_temp"            
# [7] "si_temp"            "rivervol"           "logkvert"          
#[10] "mixlscale"          "upwelling"          "so_inflow"         
#[13] "d_inflow"           "si_inflow"          "si_outflow"        
#[16] "so_si_flow"         "s1_pdist"           "s2_pdist"          
#[19] "s3_pdist"           "d1_pdist"           "d2_pdist"          
#[22] "d3_pdist"           "Inshore_waveheight"

#chemistry.drivers
# [1] "month"           "so_nitrate"      "so_ammonia"      "so_phyt"        
# [5] "so_detritus"     "d_nitrate"       "d_ammonia"       "d_phyt"         
# [9] "d_detritus"      "si_nitrate"      "si_ammonia"      "si_phyt"        
#[13] "si_detritus"     "rivnitrate"      "rivammonia"      "rivdetritus"    
#[17] "so_atmnitrate"   "so_atmammonia"   "si_atmnitrate"   "si_atmammonia"  
#[21] "si_othernitrate" "si_otherammonia"
  

	par(mfrow=c(4,4))

        tsmonthplot1("Surface irradiance",physics.drivers$sslight) # Units uE/m2/d

        tsmonthplot2("Susp.partic. matter",exp(physics.drivers$so_logespm)/1000,exp(physics.drivers$si_logespm)/1000) # Units g/m3
	legend("topright",box.lty=0,bg="transparent",legend=c("Offshore","Inshore"),col=c("black","black"),pch=c(16,1),lty=c(1,2),pt.cex=c(1,1),cex=c(0.9,0.9))

        tsmonthplot3("Temperature",(physics.drivers$so_temp),(physics.drivers$si_temp),(physics.drivers$d_temp))
	legend("topright",box.lty=0,bg="transparent",legend=c("Surf.off","Inshore","Deep"),col=c("black","black","grey"),pch=c(16,1,16),lty=c(1,2,1),pt.cex=c(1,1),cex=c(0.9,0.9,0.9))

	vdif<-(  (10^(physics.drivers$logkvert))/(physics.drivers$mixlscale*(so_depth+d_depth)))*(60*60*24) # Units m/d
        tsmonthplot1("Diffusivity gradient",vdif)

	so_inflowvol<-physics.drivers$so_inflow * (so_depth * (1-shallowprop))
	d_inflowvol <- physics.drivers$d_inflow * (d_depth * (1-shallowprop))
	si_inflowvol<-physics.drivers$si_inflow * (si_depth * (shallowprop))
        tsmonthplot3("External Inflow",so_inflowvol,si_inflowvol,d_inflowvol)  # Units m3/m2 model domain
	legend("topright",box.lty=0,bg="transparent",legend=c("Surf.off","Inshore","Deep"),col=c("black","black","grey"),pch=c(16,1,16),lty=c(1,2,1),pt.cex=c(1,1),cex=c(0.9,0.9,0.9))

	rivinflow<- physics.drivers$rivervol * (si_depth * (shallowprop))
        tsmonthplot1("River discharge",rivinflow) # Units m3/m2 model domain

        tsmonthplot1("Wave height",physics.drivers$Inshore_waveheight) # Units m

	offshoreseddist <- (physics.drivers$d1_pdist * habitat_areas[6] +
			    physics.drivers$d2_pdist * habitat_areas[7] +
			    physics.drivers$d3_pdist * habitat_areas[8])/sum(habitat_areas[6:8])
	inshoreseddist  <- (physics.drivers$s1_pdist * habitat_areas[2] +
			    physics.drivers$s2_pdist * habitat_areas[3] +
			    physics.drivers$s3_pdist * habitat_areas[4])/sum(habitat_areas[2:4])
        tsmonthplot2("Sediment disturbance",offshoreseddist,inshoreseddist) # Units d-1
	legend("topright",box.lty=0,bg="transparent",legend=c("Offshore","Inshore"),col=c("black","black"),pch=c(16,1),lty=c(1,2),pt.cex=c(1,1),cex=c(0.9,0.9))

        tsmonthplot3("Boundary nitrate",chemistry.drivers$so_nitrate,chemistry.drivers$si_nitrate,chemistry.drivers$d_nitrate)  # Units mMN/m3
	legend("topright",box.lty=0,bg="transparent",legend=c("Surf.off","Inshore","Deep"),col=c("black","black","grey"),pch=c(16,1,16),lty=c(1,2,1),pt.cex=c(1,1),cex=c(0.9,0.9,0.9))

        tsmonthplot3("Boundary ammonia",chemistry.drivers$so_ammonia,chemistry.drivers$si_ammonia,chemistry.drivers$d_ammonia)  # Units mMN/m3
	legend("topright",box.lty=0,bg="transparent",legend=c("Surf.off","Inshore","Deep"),col=c("black","black","grey"),pch=c(16,1,16),lty=c(1,2,1),pt.cex=c(1,1),cex=c(0.9,0.9,0.9))

        tsmonthplot3("Boundary phytoplankton",chemistry.drivers$so_phyt,chemistry.drivers$si_phyt,chemistry.drivers$d_phyt)  # Units mMN/m3
	legend("topright",box.lty=0,bg="transparent",legend=c("Surf.off","Inshore","Deep"),col=c("black","black","grey"),pch=c(16,1,16),lty=c(1,2,1),pt.cex=c(1,1),cex=c(0.9,0.9,0.9))

        tsmonthplot3("Boundary detritus",chemistry.drivers$so_detritus,chemistry.drivers$si_detritus,chemistry.drivers$d_detritus)  # Units mMN/m3
	legend("topright",box.lty=0,bg="transparent",legend=c("Surf.off","Inshore","Deep"),col=c("black","black","grey"),pch=c(16,1,16),lty=c(1,2,1),pt.cex=c(1,1),cex=c(0.9,0.9,0.9))

        tsmonthplot1("River nitrate",chemistry.drivers$rivnitrate) # Units mMN/m3

        tsmonthplot1("River ammonia",chemistry.drivers$rivammonia) # Units mMN/m3

        tsmonthplot2("Atmospheric nitrate",chemistry.drivers$so_atmnitrate,chemistry.drivers$si_atmnitrate) # Units mMN/m2/d
	legend("topright",box.lty=0,bg="transparent",legend=c("Offshore","Inshore"),col=c("black","black"),pch=c(16,1),lty=c(1,2),pt.cex=c(1,1),cex=c(0.9,0.9))

        tsmonthplot2("Atmospheric ammonia",chemistry.drivers$so_atmammonia,chemistry.drivers$si_atmammonia) # Units mMN/m2/d
	legend("topright",box.lty=0,bg="transparent",legend=c("Offshore","Inshore"),col=c("black","black"),pch=c(16,1),lty=c(1,2),pt.cex=c(1,1),cex=c(0.9,0.9))


}


