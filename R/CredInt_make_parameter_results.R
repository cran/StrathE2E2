#
# CredInt_make_parameter_results.R
#
#' Save current set of fitted parameters to file
#'
#' @param model model object
#' @param parameterstore inshore annual flux data
#' @param csv.output (TRUE or FALSE, default=TRUE) Set to FALSE to disable writing of CSV output files - useful for testing
#'
#' @importFrom stats quantile sd rnorm
#'
#' @noRd
#
# ------------------------------------------------------------------------------

CredInt_make_parameter_results <- function(model, parameterstore,csv.output) {

	pkg.env$csv.output <- csv.output	# controls writing of CSV files

	resultsdir <- elt(model, "setup", "resultsdir")
	identifier <- elt(model, "setup", "model.ident")

	credpath <- makepath(resultsdir, CREDINT_DIR)


cols2use<-(2:172)


Nind<-length(cols2use)


creds<-c(0.005,0.25,0.5,0.75,0.995)  


O_results<-data.frame(rep(0,(length(creds)+1)))

rownames(O_results)<-c("maxlik","lowlimit","lowquart","median","uppquart","upplimit")

for(jj in 2:Nind){
O_results[,jj]<-rep(0,(length(creds)+1))
}
colnames(O_results)<-names(parameterstore[cols2use])

O_results[1,]<-parameterstore[1,cols2use]


for(jj in 1:Nind){

Result<-parameterstore[,cols2use[jj]]

Likelihood<-parameterstore$likelihood
#message("\n","Processing ",colnames(O_results)[jj]," parameter data                          ")
O_results[2:(length(creds)+1),jj]<-GetCredInt(Result,Likelihood,creds,var=colnames(O_results)[jj],plotgraph=FALSE)

}


csvfile <- csvname(credpath, "CredInt_processed_parameters", identifier)
writecsv(O_results, csvfile)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

O_results

}

