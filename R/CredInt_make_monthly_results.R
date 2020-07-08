#
# CredInt_make_monthly_results.R
#
#' Save current set of fitted parameters to file
#'
#' @param model model object
#' @param monthlystore inshore annual flux data
#' @param csv.output (TRUE or FALSE, default=TRUE) Set to FALSE to disable writing of CSV output files - useful for testing
#'
#' @noRd
#
# ------------------------------------------------------------------------------

CredInt_make_monthly_results <- function(model, monthlystore,csv.output) {

	pkg.env$csv.output <- csv.output	# controls writing of CSV files

	resultsdir <- elt(model, "setup", "resultsdir")
	identifier <- elt(model, "setup", "model.ident")

	credpath <- makepath(resultsdir, CREDINT_DIR)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




Nind<-ncol(monthlystore)-3

creds<-c(0.005,0.25,0.5,0.75,0.995)

rownamesvec<-rep(NA,Nind*(length(creds)+1))

for(jjj in 1:Nind){
for(kkk in 1:(length(creds)+1)){
if(kkk==1) rownamesvec[((jjj-1)*(length(creds)+1))+kkk]<-paste((names(monthlystore)[jjj]),"-maxlik",sep="")
if(kkk>1)  rownamesvec[((jjj-1)*(length(creds)+1))+kkk]<-paste((names(monthlystore)[jjj]),"-",creds[kkk-1],sep="")
}
}


#set up the output data frame
#Rows are Variablex x stats, columns are months - so the transpose of the observed data and the stored model outputs

ouresults<-data.frame(rep(0,(length(rownamesvec))))

rownames(ouresults)<-rownamesvec

for(jj in 2:12){
ouresults[,jj]<-rep(0,(length(rownamesvec)))
}
colnames(ouresults)<-seq(1,12)


#Fill in the maximum likelihood rows

for(jjj in 1:Nind){
maxlikrow<-((jjj-1)*(length(creds)+1))+1
credrows<- seq(   ((jjj-1)*(length(creds)+1))+2,((jjj-1)*(length(creds)+1))+(length(creds)+1)   )

ouresults[maxlikrow,]<-monthlystore[which(monthlystore$iteration==1),jjj]

for(kkk in 1:12){
modresvector<-monthlystore[which(monthlystore$month==kkk),jjj]
likevector<-monthlystore$likelihood[which(monthlystore$month==kkk)]
#message("\n","Processing ",names(monthlystore)[jjj]," monthly data                          ")
credvals<-GetCredInt(modresvector,likevector,creds,var=names(monthlystore)[jjj] ,plotgraph=FALSE)

ouresults[credrows,kkk]<-credvals

}

}


csvfile <- csvname(credpath, "CredInt_processed_monthly_mass", identifier)
writecsv(ouresults, csvfile)

ouresults

}

