#
# plot_final_year_trophic_data.R
#
#' Plots of credible value distributions mean trophic level and omnivory index generated generated by a StrathE2E run
#'
#' Create a two-panel plot showing the whole domain mean trophic level and omnivory index of each food web component over the final year of a simulation
#' generated by the e2e_run() function. The trophic indices originate from the NetIndices R- package.
#'
#' The plots shows a set of values for a) mean trophic level, and b) omnivory index derived by the NetIndices package
#' from the annual flux-matrix for the final year of each model run. 
#'
#' Within each panel the sumbols for each food web component are ordered by the mean trophic level.
#'
#' NOTE that the NetIndices package assigns detritus and dissolved nutrients as trophic level 1, so that the phytoplankton and macrophytes are assigned trophic level 2.
#'
#' @param model R-list object defining the model configuration compiled by the e2e_read() function
#' @param use.saved Logical. If TRUE use data from a prior user-defined run held as csv files data in the current results folder, (default=FALSE).
#' @param results R-list object output generated by the e2e_run(), (default=NULL).
#'
#' @return Graphical display in a new graphics window
#'
#' @importFrom graphics polygon
#' @importFrom graphics text
#'
#' @noRd
#
# ------------------------------------------------------------------------------

plot_final_year_trophic_data <- function(model, use.saved=FALSE, results=NULL) {

start_par = par()$mfrow
on.exit(par(mfrow = start_par))

if(use.saved==FALSE & is.list(results)==FALSE){
        stop("no source of data has been specified ! \n")
}

if(use.saved==TRUE & is.list(results)==TRUE){
        stop("use.saved is TRUE but a results list object has also been specified ! \n")
}

if(use.saved==FALSE & is.list(results)==TRUE){
	networkresults	<- elt(results, "final.year.outputs", "NetworkIndexResults")
}

if(use.saved==TRUE & is.list(results)==FALSE){
	resultsdir	<- elt(model, "setup", "resultsdir")
	model.ident	<- elt(model, "setup", "model.ident")
#	model.name 	<- elt(model, "setup", "model.name")
#	model.variant 	<- elt(model, "setup", "model.variant")
	networkfile	<- csvname(resultsdir, "Network_indices_output", model.ident)
	message("Reading network data from '", networkfile, "'")
	networkresults<- readcsv(networkfile, row.names=1)
}

#Rows 11-26 = mean trophic level
#Columns 37-52 = ominvory indices

#Code to assemble a dataframe of trophic index data, ordered by baseline trophic level

baseline_trophiclevel<-networkresults[11:26,1]

baseline_omnivory<-networkresults[37:52,1]

asc_trophiclevel<-order(baseline_trophiclevel)
trlevel_names<- c("Macrophytes",
                  "Phytoplankton",
                  "Omniv.zooplankton",
                  "Carn.zooplankton",
                  "Planktiv.fish larvae",
                  "Dem.fish larvae",
                  "Planktiv.fish",
                  "Mig.fish",
                  "Dem.fish",
                  "Susp/dep.benth larvae",
                  "Carn/scav.benth larvae",
                  "Susp/dep.benth",
                  "Carn/scav.benth",
                  "Birds",
                  "Pinnipeds",
                  "Cetaceans")
#trlevel_names[asc_trophiclevel]
#baseline_trophiclevel[asc_trophiclevel]

#...............
#function to get trophic level data 
get_trophicdata<-function(zz){

gotdata<-data.frame(tl=rep(NA,6),oz=rep(NA,6))

gotdata$tl<-networkresults[(10+zz),1]
gotdata$oz<-networkresults[(36+zz),1]

return(gotdata)
}
#...............


trophiclevel_table <-  data.frame(rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),
                                  rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6),
                                  rep(NA,6),rep(NA,6),rep(NA,6),rep(NA,6))
names(trophiclevel_table) <- trlevel_names[asc_trophiclevel]
#rownames(trophiclevel_table)<-rownames(networkresults)

omnivory_table<-trophiclevel_table

baseline_trophiclevel_table<-trophiclevel_table
baseline_omnivory_table<-omnivory_table
for(jjk in 1:16){
baseline_trophiclevel_table[,jjk]<-baseline_trophiclevel[asc_trophiclevel[jjk]]
baseline_omnivory_table[,jjk]<-baseline_omnivory[asc_trophiclevel[jjk]]
}

for(jjk in 1:16){

tloxdata<-get_trophicdata(asc_trophiclevel[jjk])
trophiclevel_table[,jjk]<-tloxdata$tl
omnivory_table[,jjk]<-tloxdata$oz

}


trlevel_main<-list(stats=trophiclevel_table[2:6,],n=rep(100,16),conf=NULL,out=numeric(length=0),names=names(trophiclevel_table))
trlevel_base<-list(stats=baseline_trophiclevel_table[2:6,],n=rep(100,16),conf=NULL,out=numeric(length=0),names=names(trophiclevel_table))

omniv_main<-list(stats=omnivory_table[2:6,],n=rep(100,16),conf=NULL,out=numeric(length=0),names=names(omnivory_table))
omniv_base<-list(stats=baseline_omnivory_table[2:6,],n=rep(100,16),conf=NULL,out=numeric(length=0),names=names(omnivory_table))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


par(mfrow=c(2,1))
par(mar=c(4,10,.5,1.75))
   bxp(trlevel_main,boxwex=0.75,at=seq(1,16),ylim=c(2,max(trophiclevel_table[2:6,],na.rm=T)),cex.axis=0.75,show.names=TRUE,horizontal=TRUE,las=1,boxcol="black",whiskcol="black",whisklty="solid",medcol="black",staplecol="black")
   boxplot(baseline_trophiclevel_table[2:6,],add=TRUE,boxwex=0.6,boxcol="black",whiskcol="black",whisklty="solid",medlty=0,staplecol="black",horizontal=TRUE,yaxt="n",xaxt="n")
   mtext("Mean trophic level",cex=1,side=1,line=2)

   bxp(omniv_main,boxwex=0.75,at=seq(1,16),ylim=c(0,max(omnivory_table[2:6,],na.rm=T)),cex.axis=0.75,show.names=TRUE,horizontal=TRUE,las=1,boxcol="black",whiskcol="black",whisklty="solid",medcol="black",staplecol="black")
   boxplot(baseline_omnivory_table[2:6,],add=TRUE,boxwex=0.6,boxcol="black",whiskcol="black",whisklty="solid",medlty=0,staplecol="black",horizontal=TRUE,yaxt="n",xaxt="n")
   mtext("Omnivory index",cex=1,side=1,line=2)


}

