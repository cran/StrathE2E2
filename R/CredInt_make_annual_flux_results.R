#
# CredInt_make_annual_flux_results.R
#
#' Save current set of fitted parameters to file
#'
#' @param model model object
#' @param inshoreannualfluxstore inshore annual flux data
#' @param offshoreannualfluxstore offshore annual flux data
#' @param wholeannualfluxstore whole annual flux data
#' @param csv.output (TRUE or FALSE, default=TRUE) Set to FALSE to disable writing of CSV output files - useful for testing
#'
#' @noRd
#
# ------------------------------------------------------------------------------

CredInt_make_annual_flux_results <- function(model, inshoreannualfluxstore, offshoreannualfluxstore, wholeannualfluxstore,csv.output) {

	pkg.env$csv.output <- csv.output	# controls writing of CSV files

	setup		<- elt(model, "setup")
	identifier	<- elt(setup, "model.ident")
	resultsdir	<- elt(setup, "resultsdir")

	credpath <- makepath(resultsdir, CREDINT_DIR)


	ProcessCredIntFile <- function(Optresults,runtime.plot=runtime.plot){

		Nind<-ncol(Optresults)-2

		#creds<-c(0.1,0.5,0.9)  # This can be any lenth of vector in anu order. All values must be between 0 and 1
		creds<-c(0.005,0.25,0.5,0.75,0.995)

		ouresults<-data.frame(rep(0,(length(creds)+1)))

		#rownames(ouresults)<-c("maxlik","credlow","median","credhigh")
		rownames(ouresults)<-c("maxlik","lowlimit","lowquart","median","uppquart","upplimit")

		for(jj in 2:Nind){
			ouresults[,jj]<-rep(0,(length(creds)+1))
		}
		colnames(ouresults)<-names(Optresults[3:(Nind+2)])
		ouresults[1,]<-Optresults[1,3:(Nind+2)]


		for(jj in 1:Nind){

			Result<-Optresults[,jj+2]

			Likelihood<-Optresults[,2]
			#message("Processing ",names(Optresults[3+(Nind-1)])," flux data                 ")
			ouresults[2:(length(creds)+1),jj]<-GetCredInt(Result,Likelihood,creds,var=names(Optresults[3+(Nind-1)]),plotgraph=FALSE)

		}

		return(ouresults)

	}

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	wholefluxresults<-ProcessCredIntFile(wholeannualfluxstore)
	writecsv(wholefluxresults, csvname(credpath, "CredInt_processed_annualflux_whole", identifier))

	offshorefluxresults<-ProcessCredIntFile(offshoreannualfluxstore)
	writecsv(offshorefluxresults, csvname(credpath, "CredInt_processed_annualflux_offshore", identifier))

	inshorefluxresults<-ProcessCredIntFile(inshoreannualfluxstore)
	writecsv(inshorefluxresults, csvname(credpath, "CredInt_processed_annualflux_inshore", identifier))

        output_data<-list(offshore = offshorefluxresults,
			  inshore  = inshorefluxresults,
			  whole    = wholefluxresults)

	output_data


}

