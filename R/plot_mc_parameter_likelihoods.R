#
# plot_mc_parameter_likelihoods.R
#
#' Plots the credible distributions of ecology model parameters from a Monte Carlo simulation.
#'
#' Create a plot showing the credible distributions of ecology model parameters based on the results from the e2e_run_mc() function.
#' These distributions are formed from the input distributions to the Monte Carlo process, weighted by the likelihood of observed target data
#' on the state of the ecosystem given each combination of parameter values.
#'
#' Post-processed data from the e2e_run_mc() function are stored in the file
#' /results/Modelname/Variantname/CredInt/CredInt_processed_PARAMETERresults_data-*.csv, where * represents the model run identifier
#' (model.ident) text embedded in the R-list object created by the e2e_read() function.
#'
#' Optionally the function can read an example data set for one of the two North Sea model variants supplied with the package.
#'
#' Each parameter in the plot is scaled to its baseline value as (value - baseline)/baseline. Ideally, the baseline is the maximum likelihood model
#' developed by application of the optimization functions available in the package (e.g. e2e_optimize_eco()).
#' Each parameter is then represented by a box and whisker plot which shows the distribution of credible parameter values around zero, i.e.
#' around the baseline. The median of the credible values distribution for each parameter
#' is shown my a black tick-mark. The box spans the 50% credible interval (quartiles of the cumulative likelihood
#' of simulated values). Whisker lines span the 99% credible interval.
#'
#' The individual parameters are identified by numbers (rather than text names). These numnbers correspond to the column numnbers in
#' the file /results/Modelname/Variantname/CredInt/CredInt_processed_parameters-*.csv . Details of the parameters associated with each identification
#' number are available as a dataframe by using the function e2e_get_parmdoc().
#' 
#' The input distribution of parameter values to the Monte Carlo process is drawn from a random uniform distribution with limits specified in
#' the monte_carlo control file for the model setup (located in a sub-folder of the /Param/control folder). This distribution is shown by
#' a red box and whisker at the bottom of the plot. Given the random uniform input we expect the quartiles (limits of the box) to be symetrical
#' and located mid-way between zero and the upper and lower extremes. Vertical red lines show the expected limits of the quartiles boxes if 
#' model results were completely insensitive to individual parameter values.
#'
#' The extent to which individual parameter distributions deviate from the random uniform input is an indication of their sensitivity in the model. 
#' Parameters whose distributions are tightly confined around zero (the baseline value) are highly sensitive.
#'
#' For some parameters, in particular the preference parameters, their credible distributions may span a wider range than the inputs. This may seem
#' unexpected, but arises because within each feeding guild the preference parameters are not independent of each other. The preferences within each guild
#' must sum to 1. Hence, during the Monte Carlo process, after drawing new values of the preference values they are all rescaled to sum to 1, which may mean that
#' some of them will have been varied by more than the original coefficient of variation of the input random uniform. 
#'
#' For details of how the distribution of credible output values from StrathE2E are calculated see the help information for the e2e_run_mc() function.
#'
#' @param model R-list object defining the model configuration, compiled by the e2e_read() function
#' @param use.example Logical. If TRUE use pre-computed example data from the internal North Sea model rather than user-generated data (default=FALSE).
#'
#' @return Graphical display in a new graphics window.
#'
#' @noRd
#
# ------------------------------------------------------------------------------

plot_mc_parameter_likelihoods <- function(model, use.example=FALSE) {

start_par = par()$mfrow
on.exit(par(mfrow = start_par))

	resultsdir	<- elt(model, "setup", "resultsdir")
	model.ident	<- elt(model, "setup", "model.ident")
	model.path	<- elt(model, "setup", "model.path")
	model.name 	<- elt(model, "setup", "model.name")
	model.variant 	<- elt(model, "setup", "model.variant")

corefilename<-"CredInt_processed_parameters"
rawcorefilename<-"CredInt_cumulative_parameters"

if(use.example==TRUE){
	O_results <- get.example.results(model.name, model.variant, corefilename, CREDINT_DIR)
	Raw_results <- get.example.results(model.name, model.variant, rawcorefilename, CREDINT_DIR)
}

if(use.example==FALSE){
	credpath	<- makepath(resultsdir, CREDINT_DIR)
	credfile	<- csvname(credpath, corefilename, model.ident)
	rawcredfile	<- csvname(credpath, rawcorefilename, model.ident)
	message("Reading credible interval processed data from '", credfile, "'")

	O_results<- readcsv(credfile, row.names=1)
	Raw_results<- readcsv(rawcredfile)
}

	Nind<-ncol(O_results)

	annealing.parms <- read_SD_credint(model.path)	     # read the SD control file for Monte Carlo runs
        CVused<-annealing.parms$Prefsd                       # Pick up the CV for the preference vakues and assume all the others are the same

        N_iter<-nrow(Raw_results)                            # Extract the number of iterations used in the Monte Carlo run
        rm(Raw_results)                                      # discard the raw data

#Express the data as differences from the maxliklihood value

O_results_dif<-O_results
for(jj in 1:ncol(O_results)){
O_results_dif[1:6,jj]<-(O_results[1:6,jj]-O_results[1,jj])/O_results[1,jj]
}


#bxpstats for the prior....

   array2plot<- array(dim=c(5,1),quantile(runif(N_iter,-CVused,+CVused),probs=c(0.005,0.25,0.5,0.75,0.995)))
   colnames(array2plot)<-"prior"
   bxpdata_prior<-list(stats=array2plot,n=rep(N_iter,1),conf=NULL,out=numeric(length=0))


#bpxstats for the posteriors

   modeldata2plot<-(O_results_dif[2:6,1])
   for(jj in 2:Nind) { modeldata2plot<-c(modeldata2plot,(O_results_dif[2:6,jj]))}
   array2plot<- array(dim=c(5,Nind),modeldata2plot)
   colnames(array2plot)<-colnames(O_results_dif)
   bxpdata_O<-list(stats=array2plot,n=rep(N_iter,Nind),conf=NULL,out=numeric(length=0),names=colnames(O_results_dif))


   modeldata2plot<-rep(0,5)
   for(jj in 1:Nind) { modeldata2plot<-c(modeldata2plot,(O_results_dif[2:6,jj]))}
   array2plot<- array(dim=c(5,Nind+1),modeldata2plot)
   colnames(array2plot)<-c("prior",colnames(O_results_dif))
   bxpdata_O<-list(stats=array2plot,n=rep(N_iter,Nind+1),conf=NULL,out=numeric(length=0),names=c("prior",colnames(O_results_dif)))

   modeldata2plot<-quantile(runif(N_iter,-CVused,+CVused),probs=c(0.005,0.25,0.5,0.75,0.995))
   for(jj in 1:Nind) { modeldata2plot<-c(modeldata2plot,rep(0,5))}
   array2plot<- array(dim=c(5,Nind+1),modeldata2plot)
   colnames(array2plot)<-c("prior",colnames(O_results_dif))
   bxpdata_prior<-list(stats=array2plot,n=rep(N_iter,Nind+1),conf=NULL,out=numeric(length=0),names=c("prior",colnames(O_results_dif)))

par(mar=c(3,2,1,1))
   bxp(bxpdata_prior,boxwex=0.35,at=seq(0,Nind),horizontal=TRUE,show.names=FALSE,las=1,ylim=c(-0.2,0.2),boxcol="red",whiskcol="red",whisklty="solid",medcol="red",staplecol="red")
   bxp(bxpdata_O,add=TRUE,boxwex=0.35,at=seq(0,Nind),yaxt="n",xaxt="n",cex.axis=1.1,horizontal=TRUE,boxcol="black",whiskcol="black",whisklty="solid",medcol="black",staplecol="black")
axis(side=2,las=1,at=seq(0,Nind),cex.axis=0.3)
mtext("Parameters relative to baseline values",cex=1.1,side=1,line=2)
abline(v=0)
abline(v=-0.15/2,col="red")
abline(v=+0.15/2,col="red")


}

