#
# CredInt_make_target_results.R
#
#' Save current set of fitted parameters to file
#'
#' @param model model object
#' @param optresultsstore inshore annual flux data
#' @param csv.output (TRUE or FALSE, default=TRUE) Set to FALSE to disable writing of CSV output files - useful for testing
#'
#' @noRd
#
# ------------------------------------------------------------------------------

CredInt_make_target_results <- function(model, optresultsstore,csv.output) {

	pkg.env$csv.output <- csv.output	# controls writing of CSV files

	resultsdir <- elt(model, "setup", "resultsdir")
	identifier <- elt(model, "setup", "model.ident")

	credpath <- makepath(resultsdir, CREDINT_DIR)


	Nind<-ncol(optresultsstore)-2

	creds<-c(0.005,0.25,0.5,0.75,0.995)  

	ouresults<-data.frame(rep(0,(length(creds)+1)))

	rownames(ouresults)<-c("maxlik","lowlimit","lowquart","median","uppquart","upplimit")

	for(jj in 2:Nind){
		ouresults[,jj]<-rep(0,(length(creds)+1))
	}
	colnames(ouresults)<-names(optresultsstore[3:(Nind+2)])
	ouresults[1,]<-optresultsstore[1,3:(Nind+2)]

	for(jj in 1:Nind){

		Result<-optresultsstore[,jj+2]

		Likelihood<-optresultsstore[,2]
		#message("\n","Processing ",colnames(ouresults)[jj]," target data                          ")
		ouresults[2:(length(creds)+1),jj]<-GetCredInt(Result,Likelihood,creds,var=colnames(ouresults)[jj],plotgraph=FALSE)
	}

	csvfile <- csvname(credpath, "CredInt_processed_targetresults", identifier)
	writecsv(ouresults, csvfile)

	ouresults

}

