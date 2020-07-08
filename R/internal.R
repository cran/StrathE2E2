#
# Internal functions - not for export
#
#' @importFrom graphics axis legend lines mtext par plot points boxplot
#' @importFrom utils read.csv write.table
#' @importFrom methods show
#' @importFrom utils askYesNo install.packages packageVersion installed.packages
#'
#' @keywords internal
#'
#' @noRd
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FILE_SEP			<- .Platform$file.sep

PACKAGE_NAME			<- "StrathE2E2"
PACKAGE_VERSION			<- packageVersion(PACKAGE_NAME)				# 3.1.0

MODEL_SETUP			<- "MODEL_SETUP.csv"	# located in the Model/Version/ directory
# MODEL_RESULTS_DIR		<- "results"		# located in the current directory
CREDINT_DIR			<- "CredInt"		# located within the results/model/variant folder

PARAMETERS_DIR			<- "Param"		# sub-directories of the Model/Version/ directory
DRIVING_DATA_DIR		<- "Driving"
TARGET_DATA_DIR			<- "Target"
SD_CONTROL_DIR			<- "Param/control"

# These are fixed filenames:
SD_CONTROL_FILE_ECOLOGY		<- "optimize_ecology.csv"
SD_CONTROL_FILE_FISHING		<- "optimize_fishing.csv"
SD_CONTROL_FILE_CREDINT		<- "monte_carlo.csv"
SD_CONTROL_FILE_SENSITIVITY	<- "sensitivity.csv"

# These are search patterns:
PHYSICAL_PARAMETERS		<- "physical_parameters"				# look for csv file containing these string patterns
PHYSICS_DRIVERS			<- "physics"
CHEMISTRY_DRIVERS		<- "chemistry"
INITIAL_STATE			<- "initial_values"
BIOLOGICAL_EVENTS		<- "event_timing"

FIXED_PARAMETERS_CONSUMER	<- "fixed_consumers"
FIXED_PARAMETERS_MISCELLANEOUS	<- "fixed_miscellaneous"

FITTED_PARAMETERS_PREFERENCE	<- "fitted_preference_matrix"
FITTED_PARAMETERS_UPTAKE_MORT	<- "fitted_uptake_mort_rates"
FITTED_PARAMETERS_MICROBIOLOGY	<- "fitted_microbiology_others"

FISHING_FLEET_PARAMETERS	<- "fishing_fleet"
FISHING_ACTIVITY_PARAMETERS	<- "fishing_activity"
FISHING_POWER_PARAMETERS	<- "fishing_power"
FISHING_DISCARD_PARAMETERS	<- "fishing_discards"
FISHING_PROCESSING_PARAMETERS	<- "fishing_processing"
FISHING_DISTRIBUTION_PARAMETERS	<- "fishing_distribution"
FISHING_ACTIVITY_SCALING_VALUES	<- "fishing_gear_multiplier"
FISHING_FLEET_GEAR_LINKAGES	<- "fishing_gear_linkages"				# Parameters

HARVEST_RATIO_SCALING_VALUES	<- "harvest_ratio_multiplier"				# Target_Data
KNOWN_HARVEST_RATIO_VALUES	<- "region_harvest"					# Target_Data
FISHING_FLEET_TARGET_HR		<- "zonal_harvest"					# Target_Data

ANNUAL_TARGET_DATA		<- "annual_observed"
MONTHLY_TARGET_DATA		<- "monthly_observed"
FOOD_WEB_FLOW_MATRIX		<- "food_web_flow_matrix"


# Data package:
SRAN_REPOSITORY			<- "https://marineresourcemodelling.gitlab.io/sran"
DATA_PACKAGE_NAME		<- "StrathE2E2examples"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pkg.env				<- new.env()
pkg.env$SETUPFILES		<- character()
pkg.env$quiet			<- TRUE
pkg.env$silent			<- FALSE
pkg.env$csv.output		<- FALSE


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


check.exists <- function(filename) {
	if (! file.exists(filename)) {
		mesg <- paste0("Error: could not find file '", filename, "' !\n")
		stop(mesg)
	}
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


isdefined <- function(var, val) {
	exists(var) && (get(var) == val)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

remove.dirsep <- function(path) {
        if(! is.null(path)) path <- trimws(path, "r", whitespace=FILE_SEP)   # remove trailing file sep only
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# build up a filename path
#
makepath <- function(...) {
	FILE_SEP_MULTI <- paste0(FILE_SEP, FILE_SEP, "*")		# //*

	components <- list(...)
	components <- trimws(components)				# remove whitespace from start/end of strings
	#components <- trimws(components,"r", whitespace=singleslash)	# remove trailing file sep only
	components <- remove.dirsep(components)				# remove trailing file sep only
	path <- paste(components, collapse=FILE_SEP)			# paste with dir separator, collapsing to a single string
	path <- gsub(FILE_SEP_MULTI, FILE_SEP, path)			# remove any duplicate file seps for "niceness"

	path
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# reads the setup csv which specifies the names for all the model input and output files
#
read.model.setup <- function(model.path) {
	setup <- readcsv(model.path, MODEL_SETUP, header=TRUE)		# DF
	#pkg.env$SETUPFILES <- as.character(levels(setup[[1]]))		# character vector
	pkg.env$SETUPFILES <- setup[[1]]				# character vector
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# fitted.pars <- get.model.file(MODEL_DIR, PARAMETER_DIR, file.pattern=FITTED_PARAMETERS)
# using the file patterns listed above
#
get.model.file <- function(..., file.pattern, header=TRUE) {

	matches <- grep(file.pattern, pkg.env$SETUPFILES, value=TRUE)

	if (length(matches) != 1) {
		message("Matches=",length(matches))
		showall("setupfiles",pkg.env$SETUPFILES)
		if (length(matches) == 0) {
			# no match!
			message("Error: could not find model file using pattern '", file.pattern, "' !")
		} else if (length(matches) > 1) {
			# more than 1 match!
			message("Error: matched more than one model file using pattern '", file.pattern, "' !")
			for (m in matches) {
				message(" matched:", m)
			}
		}
		stop("Cannot find requested model filename!")
	}

	# found it:
	filename <- matches[[1]]
	readcsv(..., filename, header=header)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# read CSV data from the path units
# by default first line of file is treated as a header line (header=TRUE)
#	readcsv(model.path, PARAMETERS_DIR, "fitted_parameters.csv")
# R read.csv() will interpret first column as row names if the 1st line of the file has 1 less column than the rest!
# We now standardise on completely regular CSV files (same number of columns in every row including header, if present)
# If we want the 1st column to be treated as row names, then we must explicity request it:
#	readcsv(model.path, PARAMETERS_DIR, "fitted_parameters_prefs.csv", row.names=1)
# of course header is TRUE by default.
#
readcsv <- function(..., header=TRUE, row.names=NULL) {

	filepath <- makepath(...)

	check.exists(filepath)

	file <- basename(filepath)
	if (! pkg.env$quiet) message(" Reading CSV file: ", file)
	data <- read.csv(filepath, header=header, row.names=row.names)

	data
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# writecsv(): write data to CSV file
# the dir is created if it doesn't already exist
# file will be overwritten if it already exists
# we use write.table() instead of write.csv() for more flexibility
# defaults are same as write.table(), col.names(header)=TRUE, row.names=TRUE
# assume a CSV file:
#	"names,"kelp","phyt"
#	"ammonia","0.11","0.22"
#	"nitrate","0.33","0.44"
# read it:
#	csv <- read.csv("test1.csv", row.names=1)
# To re-write this use:
#	writecsv(csv, "test1.csv")
# To omit the header, header=FALSE:
#	"ammonia","0.11","0.22"
#	"nitrate","0.33","0.44"
# To omit the row.names as well, header=FALSE, row.names=FALSE:
#	0.11, 0.22
#	0.33, 0.44
# Just the headers, row.names=FALSE:
#	"kelp","phyt"
#	0.11,0.22
#	0.33,0.44
#
writecsv <- function(data, filepath, header=TRUE, row.names=TRUE) {

	if (pkg.env$csv.output) {
		# CSV output enabled:
		dir <- dirname(filepath)
		file <- basename(filepath)

		create.folder(dir)

		# compatibility with write.table:
		col.names <- header
		if ((row.names == TRUE) && (col.names == TRUE)) {
			# writes an element into the header line for the row.names:
			col.names = NA
		}

		if (! pkg.env$quiet) message(" Writing CVS file: ", filepath)
		#message(" row.names=",row.names, " col.names=",col.names)
		write.table(data, file=filepath, row.names=row.names, col.names=col.names, sep=",")
	}
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Version of writecsv where control over whether to write or not is exerted locally rather then at package level. Forces csv output under all circumstances.
# This is helpful where eg a StrathE2E() function call nested within a function turns off csv.output when the higher level function is wanting to produce it

writecsv.local <- function(data, filepath, header=TRUE, row.names=TRUE) {

		# CSV output enabled:
		dir <- dirname(filepath)
		file <- basename(filepath)

		create.folder(dir)

		# compatibility with write.table:
		col.names <- header
		if ((row.names == TRUE) && (col.names == TRUE)) {
			# writes an element into the header line for the row.names:
			col.names = NA
		}

		if (! pkg.env$quiet) message(" Writing CVS file: ", filepath)
		#message(" row.names=",row.names, " col.names=",col.names)
		write.table(data, file=filepath, row.names=row.names, col.names=col.names, sep=",")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# simple wrapper around writecsv() to save a list with the element names as row names in 1st column, data in 2nd, with no header:
# 	"el1",33
#	"el2",44
#
writecsv.list <- function(data, filepath) {
	writecsv(data, filepath, row.names=TRUE, header=FALSE)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# create a csv filename:
#	output/dir/file-identifier.csv
# or:
#	output/dir/file.csv
# if identifier not set
#
csvname <- function(dir, file, identifier)
{
	name <- makepath(dir, file)			# dir/file

	if (nchar(identifier)) {
		# add "-ident" if set:
		name <- paste0(name, "-", identifier)	# dir/file-identifier
	}
	name <- paste0(name, ".csv")			# dir/file-identifier.csv (or dir/file.csv)

	name
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# wrapper around source() to produce consistent error messages
#
sourcefile <- function(filename) {
	check.exists(filename)

	source(filename)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# return the full path to the requested model
# either a system packaged model or a user supplied one
#
get.model.path <- function(model.name, user.path="") {

	if (!nchar(model.name)) {
		stop("Error: please supply a model name!")
	}

	# get path to model:
	if (is.null(user.path)) {
		# System model:
		model.path <- system.file("extdata/Models", model.name, package="StrathE2E2")

		if (model.path == "") {
			stop("Error: system model: '", model.name, "' does not exist! (use e2e_ls() to show system models)")
		}
	} else {
		# User model:
		if (! dir.exists(user.path)) {
			stop("Error: user model path: '", user.path, "' does not exist!")
		}

		model.path <- makepath(user.path, model.name)
		if (! dir.exists(model.path)) {
			stop("Error: user model: '", model.name, "' does not exist! (use e2e_ls(", user.path, ") to show user models)")
		}
	}

	model.path
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# return the full path to the requested model and variant
#
get.variant.path <- function(model.name, model.variant, user.path="") {

	model.path <- get.model.path(model.name, user.path)

	if (!nchar(model.variant)) {
		stop("Error: please supply a model variant!")
	}

	# get path to model variant:
	full.path <- makepath(model.path, model.variant)
	if (! dir.exists(full.path)) {
		message("Cannot find model: '", full.path, "' !")
		message("Cannot find model variant: '", model.variant, "' !")
		message("Looking in model path: '", model.path, "'")
		stop("Error: cannot find requested variant, stopping")
	}

	full.path
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# return the full path to the requested model documentation
#
#get.documentation.path <- function(model.name, model.documentation, user.path="") {
#
#	model.path <- get.model.path(model.name, user.path)
#
#	if (!nchar(model.documentation)) {
#		stop("Error: please supply a model documentation folder name!")
#	}
#
#	# get path to model documentation:
#	full.path <- makepath(model.path, model.documentation)
#	if (! dir.exists(full.path)) {
#		message("Cannot find model documentation: '", model.documentation, "' !")
#		message("Looking in model path: '", model.path, "'")
#		stop("Error: cannot find requested documentation, stopping")
#	}
#
#	full.path
#}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check folder exists and create if not present
#
create.folder <- function(folder) {
	if (! dir.exists(folder)) {
		if (! pkg.env$quiet) message(" Creating folder : ", folder)
		if (!dir.create(folder, recursive=TRUE, showWarnings=FALSE)) {
			stop("Error: could not create folder '", folder, "'\n", sep="")
		}
	}
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# given a set of list/data.frame element names, extract the value if present
# if the element does not exist then print out a warning and a call trace UNLESS the default value is
# set in which case return that
# In R if you access a non existant list element:
#	x <- list$notpresent
# then x will be NULL and you get no warning!
# Using this elt() function will generate warnings (and hence pick up typos, etc.)
#	x <- elt(list, "notpresent")			looks for list$notpresent, print warning and trace
#	x <- elt(list, "notpresent", default=10.0)	looks for list$notpresent, uses default to return 10.0
#	x <- elt(list, "exists")			looks for list$exists and returns it
#	x <- elt(list, "exists1", "exists2")		looks for list$exists1$exists2 and returns it
# 
elt <- function(data, ..., default="NOTSET") {
	elements <- list(...)
	ret <- data
	for (element in elements) {
		if (element %in% names(ret)) {
			ret <- ret[[element]]		# element exists
		} else if (default != "NOTSET") {
			ret <- default			# element does not exist, but caller has set a default
			break
		} else {
			message("Error: unknown list/data.frame element '", element, "'")
			message("Trace:")
			# print call list:
			calls <- sys.calls()
			for (c in 1:length(calls)) {
				message("\t", c, ": ", appendLF=FALSE)
				print(calls[[c]])
			}
			message("")
			ret <- NULL
			break
		}
	}

	ret
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# print out entire R object
#
showall <- function(title, v) {
	# save current settings:
	max <- options("max.print")
	dig <- options("digits")

	# print everything to high precision:
	options(digits=20)
	options(max.print=999999)
	message(title, ":")
	if (is.list(v)) message("Length=",length(v))
	message("Class: ", class(v))
	show(v)

	# restore original settings:
	options(max.print=max$max.print)
	options(digits=dig$digits)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


showallsort <-function(title, v) {
	showall(title, v[order(names(v))])
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# print out elements of a dataframe or list suitable for printing:
#
genshowall <- function(v, prefix="") {
	for (i in names(v)) {
		message("showall(\"", i, "\", ", prefix, i, ")")
	}
}


#Plot the final year of output


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


fyplot1<-function(tspmain,axtitle,tspvar1){
par(mar=c(3,3.8,2.5,0.4))
tsyears<-length(tspvar1)
tseq<-seq(0,360,by=1)
plmax<-1.5*max(max(tspvar1))
plmin<-0
plot(tseq,tspvar1,type="l",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE,main=tspmain)
axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
axis(side=2,las=1,cex.axis=0.9)
mtext("Days",cex=0.7,side=1,line=2)
mtext(axtitle,cex=0.7,side=2,line=2.8)
mtext(tspmain,cex=0.7,side=3,line=0.5)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


fyplot2<-function(tspmain,axtitle,tsptitle1,tsptitle2,tspvar1,tspvar2){
par(mar=c(3,3.8,2.5,0.4))
tsyears<-length(tspvar1)
tseq<-seq(0,360,by=1)
plmax<-max(max(tspvar1),max(tspvar2))
plmin<-0
plmax<-plmax+0.55*(plmax-plmin)
plot(tseq,tspvar1,type="l",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
lines(tseq,tspvar2,type="l",lty="dashed")
axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
axis(side=2,las=1,cex.axis=0.9)
mtext("Days",cex=0.7,side=1,line=2)
mtext(axtitle,cex=0.7,side=2,line=2.8)
mtext(tspmain,cex=0.7,side=3,line=0.5)
legend("topleft",c(tsptitle1,tsptitle2),box.lty=0,bg="transparent",col=c("black","black"),lty=c(1,2),cex=c(0.9,0.9),pt.cex=c(1,1))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


fyplot3<-function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tspvar1,tspvar2,tspvar3){
par(mar=c(3,3.8,2.5,0.4))
tsyears<-length(tspvar1)
tseq<-seq(0,360,by=1)
plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
plmin<-0
plmax<-plmax+1.15*(plmax-plmin)
plot(tseq,tspvar1,type="l",col="black",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
lines(tseq,tspvar2,type="l",col="black",lty="dashed")
lines(tseq,tspvar3,type="l",col="red",lty="dashed")
axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
axis(side=2,las=1,cex.axis=0.9)
mtext("Days",cex=0.7,side=1,line=2)
mtext(axtitle,cex=0.7,side=2,line=2.8)
mtext(tspmain,cex=0.7,side=3,line=0.5)
legend("topleft",c(tsptitle1,tsptitle2,tsptitle3),box.lty=0,bg="transparent",col=c("black","black","red"),lty=c(1,2,2),cex=c(0.9,0.9,0.9),pt.cex=c(1,1,1))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


fyplot4<-function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tsptitle4,tspvar1,tspvar2,tspvar3,tspvar4){
par(mar=c(3,3.8,2.5,0.4))
tsyears<-length(tspvar1)
tseq<-seq(0,360,by=1)
plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3),max(tspvar4))
plmin<-0
plmax<-plmax+1.45*(plmax-plmin)
plot(tseq,tspvar1,type="l",col="black",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
lines(tseq,tspvar2,type="l",col="black",lty="dashed")
lines(tseq,tspvar3,type="l",col="red")
lines(tseq,tspvar4,type="l",col="red",lty="dashed")
axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
axis(side=2,las=1,cex.axis=0.9)
mtext("Days",cex=0.7,side=1,line=2)
mtext(axtitle,cex=0.7,side=2,line=2.8)
mtext(tspmain,cex=0.7,side=3,line=0.5)
legend("topleft",c(tsptitle1,tsptitle2,tsptitle3,tsptitle4),box.lty=0,bg="transparent",col=c("black","black","red","red"),lty=c(1,2,1,2),cex=c(0.9,0.9,0.9,0.9),pt.cex=c(1,1,1,1))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


fyplot3_hab <- function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tspvar1,tspvar2,tspvar3) {
	par(mar=c(3,3.8,1.3,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,360,by=1)
	plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
	plmin<-0
	plmax<-plmax+(1.4*(plmax-plmin))
	plot(tseq,tspvar1,type="l",col="red",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",col="red",lty="dashed")
	lines(tseq,tspvar3,type="l",col="black",lty="dashed")
	axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Days",cex=0.7,side=1,line=2)
	mtext(axtitle,cex=0.7,side=2,line=2.8)
	mtext(tspmain,cex=0.7,side=3,line=0.2)
	legend("topleft",c(tsptitle1,tsptitle2,tsptitle3),box.lty=0,bg="transparent",col=c("red","red","black"),lty=c(1,2,2),pt.cex=c(1,1,1),cex=c(0.9,0.9,0.9))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tsplot1 <- function(tsptitle, tspvar1) {
	par(mar=c(3,3.8,0.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,(tsyears-1)/360,by=1/360)
	plmax<-max((tspvar1))
	plmin<-min((tspvar1))
	plmax<-plmax+(0.1*(plmax-plmin))
	plot(tseq,tspvar1,type="l",yaxt="n",ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	axis(side=1,las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Year",cex=0.7,side=1,line=2)
	mtext(tsptitle,cex=0.7,side=2,line=2.8)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tsplot2 <- function(tsptitle,tspvar1,tspvar2) {
	par(mar=c(3,3.8,0.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,(tsyears-1)/360,by=1/360)
	plmax<-max(max(tspvar1),max(tspvar2))
	plmin<-min(min(tspvar1),min(tspvar2))
	plmax<-plmax+(0.8*(plmax-plmin))
	plot(tseq,tspvar1,type="l",yaxt="n",ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",lty="dashed")
	axis(side=1,las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Year",cex=0.7,side=1,line=2)
	mtext(tsptitle,cex=0.7,side=2,line=2.8)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tsplot3 <- function(tsptitle,tspvar1,tspvar2,tspvar3) {
	par(mar=c(3,3.8,0.5,0.4))
	tsyears<-length(tspvar1)
	tseq<-seq(0,(tsyears-1)/360,by=1/360)
	plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
	plmin<-min(min(tspvar1),min(tspvar2),min(tspvar3))
	plmax<-plmax+(1.05*(plmax-plmin))
	plot(tseq,tspvar1,type="l",yaxt="n",ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	lines(tseq,tspvar2,type="l",lty="dashed")
	lines(tseq,tspvar3,type="l",lty="dashed",col="red")
	axis(side=1,las=1,cex.axis=0.9)
	axis(side=2,las=1,cex.axis=0.9)
	mtext("Year",cex=0.7,side=1,line=2)
	mtext(tsptitle,cex=0.7,side=2,line=2.8)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tsmonthplot1 <- function(tsptitle, tspvar1) {
	par(mar=c(3,4.1,0.5,0.4))
	tseq<-seq(0.5,11.5,by=1)
	plmax<-max((tspvar1))
#	plmin<-min((tspvar1))
	plmin<-0
	plmax<-plmax+(0.1*(plmax-plmin))
	plot(tseq,tspvar1,type="l",lty=1,yaxt="n",xlim=c(0,12),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	points(tseq,tspvar1,pch=16)
	axis(side=1,at=c(0,3,6,9,12),las=1,cex.axis=0.9,padj=-0.80)
	axis(side=2,las=1,cex.axis=0.9, hadj=0.82)
	mtext("Months",cex=0.7,side=1,line=1.5)
	mtext(tsptitle,cex=0.7,side=2,line=3.1)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tsmonthplot2 <- function(tsptitle,tspvar1,tspvar2) {
	par(mar=c(3,4.1,0.5,0.4))
	tseq<-seq(0.5,11.5,by=1)
	plmax<-max(max(tspvar1),max(tspvar2))
#	plmin<-min(min(tspvar1),min(tspvar2))
	plmin<-0
	plmax<-plmax+(0.5*(plmax-plmin))
	plot(tseq,tspvar1,type="l",lty=1,yaxt="n",xlim=c(0,12),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	points(tseq,tspvar1,pch=16)
	lines(tseq,tspvar2,lty="dashed")
	points(tseq,tspvar2,pch=1)
	axis(side=1,at=c(0,3,6,9,12),las=1,cex.axis=0.9,padj=-0.80)
	axis(side=2,las=1,cex.axis=0.9, hadj=0.82)
	mtext("Months",cex=0.7,side=1,line=1.5)
	mtext(tsptitle,cex=0.7,side=2,line=3.1)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tsmonthplot3 <- function(tsptitle,tspvar1,tspvar2,tspvar3) {
	par(mar=c(3,4.1,0.5,0.4))
	tseq<-seq(0.5,11.5,by=1)
	plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
#	plmin<-min(min(tspvar1),min(tspvar2),min(tspvar3))
	plmin<-0
	plmax<-plmax+(0.8*(plmax-plmin))
	plot(tseq,tspvar1,type="l",lty=1,yaxt="n",xlim=c(0,12),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
	points(tseq,tspvar1,pch=16)
	lines(tseq,tspvar2,lty="dashed")
	points(tseq,tspvar2,pch=1)
	lines(tseq,tspvar3,lty=1,col="grey")
	points(tseq,tspvar3,pch=16,col="grey")
	axis(side=1,at=c(0,3,6,9,12),las=1,cex.axis=0.9,padj=-0.80)
	axis(side=2,las=1,cex.axis=0.9, hadj=0.82)
	mtext("Months",cex=0.7,side=1,line=1.5)
	mtext(tsptitle,cex=0.7,side=2,line=3.1)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




StrathE2E.run <- function(build) {

	initial.state		<- elt(build, "initial.state")
	model.parameters	<- elt(build, "model.parameters")
	forcings		<- elt(build, "forcings")
	nyears			<- elt(build, "run", "nyears")
	times			<- elt(build, "run", "times")

	#showall("initial.state", initial.state)
	#showall("times", times)
	#showall("parms", model.parameters)
	#showall("forcings", forcings)

	if (! pkg.env$quiet) message("Running model for ", nyears, " years")

	output <- as.data.frame(
		ode(
			y		= unlist(initial.state),
			times		= times,
			func		= "derivsc",
			parms		= model.parameters,
			dllname		= "StrathE2E2",
			initforc	= "forcc",
			forcings	= forcings,
			initfunc	= "odec",
			fcontrol	= list(method="linear", rule=2, ties="ordered"),
			method		= "lsoda"
		)
	)
	#showall("output", output)

	output
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#---------------------------------
#Setup a vector of text elements to be included as a column of 
#descriptons in the csv output files for each of the final year annual mean
#max and minimum mass results for inshore offshore and wholedomain.
#These are used repetively in 3 different sections of source code 
#so they are configured just once here.
#
mass_results_descriptions<-c(
"Surface_layer_detritus",
"Deep_layer_detritus",
"Sediment_labile_plus_refractory_detritus",
"Sediment_refractory_detritus",
"Fishery_discards",
"Corpses",
"Macrophyte_debris",
"Surface_layer_ammonia",
"Deep_layer_ammonia",
"Sediment_porewater_ammonia",
"Surface_layer_nitrate",
"Deep_layer_nitrate",
"Sediment_porewater_nitrate",
"Macrophyte_nitrogen",
"Surface_layer_phytoplankton",
"Deep_layer_phytoplankton",
"Omnivorous_zooplankton",
"Carnivorous_zooplankton",
"Benthos_susp/dep_feeders_larvae",
"Benthos_susp/dep_feeders",
"Benthos_carn/scav_feeders_larvae",
"Benthos_carn/scav_feeders",
"Planktivorous_fish_larvae",
"Planktivorous_fish",
"Migratory_fish",
"Demersal_fish_larvae",
"Demersal_fish",
"Bird",
"Pinnipeds",
"Cetaceans",
"Total_nitrogen_mass",
"Area_proportion_of_inshore_zone",
"Thickness_of_inshore_surface_layer",
"Thickness_of_offshore_surface_layer",
"Thickness_of_offshore_deep_layer",
"Area_proportion_inshore_rock",
"Area_proportion_inshore_sediment_s1",
"Area_proportion_inshore_sediment_s2",
"Area_proportion_inshore_sediment_s3",
"Area_proportion_offshore_rock",
"Area_proportion_offshore_sediment_d1",
"Area_proportion_offshore_sediment_d2",
"Area_proportion_offshore_sediment_d3",
"Thickness_of_inshore_sediment_layer_s1",
"Thickness_of_inshore_sediment_layer_s2",
"Thickness_of_inshore_sediment_layer_s3",
"Thickness_of_offshore_sediment_layer_d1",
"Thickness_of_offshore_sediment_layer_d2",
"Thickness_of_offshore_sediment_layer_d3",
"Porosity_of_inshore_sediment_layer_s1",
"Porosity_of_inshore_sediment_layer_s2",
"Porosity_of_inshore_sediment_layer_s3",
"Porosity_of_offshore_sediment_layer_d1",
"Porosity_of_offshore_sediment_layer_d2",
"Porosity_of_offshore_sediment_layer_d3")
#---------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#---------------------------------
#Setup a vector of text elements to be included as a column of 
#descriptons in the csv output files for each of the final year annual
#flux results for inshore offshore and wholedomain.
#These are used repetively in 3 different sections of source code 
#so they are configured just once here.
#
annual_flux_descriptions<-c(
"DIN_inflow",
"DIN_outflow",
"Particulate_inflow",
"Particulate_outflow",
"Atmosphere_DIN_input",
"River_DIN_inflow",
"River_particulate_inflow",
"Summer_DIN_inflow",
"Summer_DIN_outflow",
"Summer_particulate_inflow",
"Summer_particulate_outflow",
"Summer_river_DIN_inflow",
"Summer_atmosphere_DIN_input",
"Vertical_nitrate_flux",
"Surface_horizontal_nitrate_flux",
"Net_import/export_flux_in_the_sediment",
"Beachcast_export_of_macrophyte_debris",
"DIN_Net_flux_offshore_to_inshore",
"Particulate_net_flux_offshore_to_inshore",

"Plank.fish_net_active_migration_offshore_to_inshore",
"Mig.fish_net_active_migration_offshore_to_inshore",
"Dem.fish_net_active_migration_offshore_to_inshore",
"Bird_net_active_migration_offshore_to_inshore",
"Pinniped_net_active_migration_offshore_to_inshore",
"Cetacean_net_active_migration_offshore_to_inshore",
"Mig.fish_net_migration_external_offshore",
"Mig.fish_annual_immigration",
"Mig.fish_annual_emigration",

"Phytoplankton_net_primary_production",
"Phytoplankton_new_production_(nitrate_drawdown)",
"Phytoplankton_new_production_(H&B)",
"Water_column_net_nitrate_consumption_production",
"Phytoplankton_nitrate_uptake",
"Phytoplankton_ammonia_uptake",
"Phytoplankton_fratio_(H&B/NetPP)",
"Phytoplankton_fratio_(traditional)",

"Macrophyte_nitrate_uptake",
"Macrophyte_ammonia_uptake",
"Macrophyte_gross_production",
"Phytoplankton_gross_production",
"Omniv.zooplankton_gross_production",
"Carniv.zooplankton_gross_production",
"Planktiv.fish_larvae_gross_production",
"Dem.fish_larvae_gross_production",
"Planktiv.fish_gross_production", 
"Mig.fish_gross_production", 
"Dem.fish_gross_production", 
"Benthos_susp/dep_larvae_gross_production", 
"Benthos_carn/scav_larvae_gross_production", 
"Benthos_susp/dep_gross_production",
"Benthos_carn/scav_gross_production",
"Bird_gross_production",
"Pinniped_gross_production",
"Cetacean_gross_production",

"Omniv.zooplankton_net_production",
"Carniv.zooplankton_net_production",
"Planktiv.fish_larvae_net_production",
"Dem.fish_larvae_net_production",
"Planktiv.fish_net_production", 
"Mig.fish_net_production", 
"Dem.fish_net_production", 
"Benthos_susp/dep_larvae_net_production", 
"Benthos_carn/scav_larvae_net_production", 
"Benthos_susp/dep_net_production",
"Benthos_carn/scav_net_production",
"Bird_net_production",
"Pinniped_net_production",
"Cetacean_net_production",

"Water_column_detritus_production",
"Sediment_detritus_production", 
"Corpse_production",
"Flux_of_detritus_from_water_to_sediment",
"Flux_of_discards_to_corpses",
"Pelagic_fauna_ammonia_production", 
"Benthic_fauna_ammonia_production",
"Water_column_detritus_mineralisation",
"Sediment_detritus_mineralisation",
"Water_column_nitrification",
"Sediment_nitrification", 
"Water_column_denitrification",
"Sediment_denitrification", 
"Sediment_to_water_ammonia_flux",
"Sediment_to_water_nitrate_flux",

"Flux_detritus_to_omniv.zooplankton",
"Flux_phytoplankton_to_omniv.zooplankton",
"Flux_benthoss/d.larvae_to_omniv.zooplankton",
"Flux_benthosc/s.larvae_to_omniv.zooplankton",

"Flux_omniv.zooplankton_to_carniv.zooplankton",
"Flux_plank.fish.larvae_to_carniv.zooplankton",
"Flux_dem.fish.larvae_to_carniv.zooplankton",
"Flux_benthoss/d.larvae_to_carniv.zooplankton",
"Flux_benthosc/s.larvae_to_carniv.zooplankton",

"Flux_omniv.zooplankton_to_plank.fish.larvae",
"Flux_benthoss/d.larvae_to_plank.fish.larvae",
"Flux_benthosc/s.larvae_to_plank.fish.larvae",

"Flux_omniv.zooplankton_to_dem.fish.larvae",
"Flux_benthoss/d.larvae_to_dem.fish.larvae",
"Flux_benthosc/s.larvae_to_dem.fish.larvae",

"Flux_omniv.zooplankton_to_plank.fish",
"Flux_carniv.zooplankton_to_plank.fish",
"Flux_plank.fish.larvae_to_plank.fish",
"Flux_dem.fish.larvae_to_plank.fish",
"Flux_benthoss/d.larvae_to_plank.fish",
"Flux_benthosc/s.larvae_to_plank.fish",

"Flux_omniv.zooplankton_to_mig.fish",
"Flux_carniv.zooplankton_to_mig.fish",
"Flux_plank.fish.larvae_to_mig.fish",
"Flux_dem.fish.larvae_to_mig.fish",
"Flux_benthoss/d.larvae_to_mig.fish",
"Flux_benthosc/s.larvae_to_mig.fish",

"Flux_corpses_to_dem.fish",
"Flux_discards_to_dem.fish",
"Flux_carniv.zooplankton_to_dem.fish",
"Flux_plank.fish.larvae_to_dem.fish",
"Flux_dem.fish.larvae_to_dem.fish",
"Flux_plank.fish_to_dem.fish",
"Flux_mig.fish_to_dem.fish",
"Flux_dem.fish_to_dem.fish",
"Flux_benthoss/d_to_dem.fish",
"Flux_benthosc/s_to_dem.fish",

"Flux_detritus_to_benthoss/d.larvae",
"Flux_phytoplankton_to_benthoss/d.larvae",

"Flux_detritus_to_benthosc/s.larvae",
"Flux_phytoplankton_to_benthosc/s.larvae",

"Flux_detritus_to_benthoss/d",
"Flux_sediment.detritus_to_benthoss/d",
"Flux_phytoplankton_to_benthoss/d",

"Flux_macrophyte.debris_to_benthosc/s",
"Flux_corpses_to_benthosc/s",
"Flux_macrophyte_to_benthosc/s",
"Flux_benthoss/d_to_benthosc/sc",

"Flux_corpses_to_birds", 
"Flux_discards_to_birds",
#"Flux_ominiv.zooplankton_to_birds",
"Flux_carniv.zooplankton_to_birds",
"Flux_plank.fish_to_birds",
"Flux_mig.fish_to_birds",
"Flux_dem.fish_to_birds",
"Flux_benthoss/d_to_birds",
"Flux_benthosc/s_to_birds",

"Flux_corpses_to_pinnipeds", 
"Flux_discards_to_pinnipeds",
"Flux_carniv.zooplankton_to_pinnipeds",
"Flux_plank.fish_to_pinnipeds",
"Flux_mig.fish_to_pinnipeds",
"Flux_dem.fish_to_pinnipeds",
"Flux_benthoss/d_to_pinnipeds",
"Flux_benthosc/s_to_pinnipeds",
"Flux_birds_to_pinnipeds",

"Flux_discards_to_cetaceans",
"Flux_ominiv.zooplankton_to_cetaceans",
"Flux_carniv.zooplankton_to_cetaceans",
"Flux_plank.fish_to_cetaceans",
"Flux_mig.fish_to_cetaceans",
"Flux_dem.fish_to_cetaceans",
"Flux_benthoss/d_to_cetaceans",
"Flux_benthosc/s_to_cetaceans",
"Flux_birds_to_cetaceans",
"Flux_pinnipeds_to_cetaceans",

"Net_production_of_all_secondary_and_higher_trophic_levels",
"Export_from_secondary_producers",
"Plank.fish_annual_spawning",
"Plank.fish_annual_recruitment", 
"Dem.fish_annual_spawning",
"Dem.fish_annual_recruitment", 
"Benthoss/d_annual_spawning",
"Benthoss/d_annual_recruitment", 
"Benthosc/s_annual_spawning",
"Benthosc/sc_annual_recruitment",

"Plank.fish_landings_live_weight",
"Mig.fish_landings_live_weight",
"Dem.fish_landings_live_weight",
"Dem.fish_quota_limited_landings_live_weight",
"Dem.fish_non.quota_landings_live_weight",
"Benthoss/d_landings_live_weight",
"Benthosc/s_landings_live_weight",
"Carniv.zooplankton_landings_live_weight",
"Bird_landings_live_weight",
"Pinniped_landings_live_weight",
"Cetacean_landings_live_weight",
"Macrophyte_landings_live_weight",

"Plank.fish_discards",
"Mig.fish_discards",
"Dem.fish_discards",
"Dem.fish_quota_limited_discards",
"Dem.fish_non.quota_discards",
"Benthoss/d_discards",
"Benthosc/s_discards",
"Carniv.zooplankton_discards",
"Bird_discards",
"Pinniped_discards",
"Cetacean_discards",
"Macrophyte_discards",

"Plank.fish_offal",
"Mig.fish_offal",
"Dem.fish_offal",
"Dem.fish_quota_limited_offal",
"Dem.fish_non.quota_offal",
"Benthoss/d_offal",
"Benthosc/s_offal",
"Carniv.zooplankton_offal",
"Bird_offal",
"Pinniped_offal",
"Cetacean_offal",
"Macrophyte_offal",

"Plank.fish_landings_processed_weight",
"Mig.fish_landings_processed_weight",
"Dem.fish_landings_processed_weight",
"Dem.fish_quota_limited_landings_processed_weight",
"Dem.fish_non.quota_landings_processed_weight",
"Benthoss/d_landings_processed_weight",
"Benthosc/s_landings_processed_weight",
"Carniv.zooplankton_landings_processed_weight",
"Bird_landings_processed_weight",
"Pinniped_landings_processed_weight",
"Cetacean_landings_processed_weight",
"Macrophyte_landings_processed_weight",

"Area_proportion_of_inshore_zone",
"Thickness_of_inshore_surface_layer",
"Thickness_of_offshore_surface_layer",
"Thickness_of_offshore_deep_layer",
"Area_proportion_inshore_rock",
"Area_proportion_inshore_sediment_s1",
"Area_proportion_inshore_sediment_s2",
"Area_proportion_inshore_sediment_s3",
"Area_proportion_offshore_rock",
"Area_proportion_offshore_sediment_d1",
"Area_proportion_offshore_sediment_d2",
"Area_proportion_offshore_sediment_d3",
"Thickness_of_inshore_sediment_layer_s1",
"Thickness_of_inshore_sediment_layer_s2",
"Thickness_of_inshore_sediment_layer_s3",
"Thickness_of_offshore_sediment_layer_d1",
"Thickness_of_offshore_sediment_layer_d2",
"Thickness_of_offshore_sediment_layer_d3",
"Porosity_of_inshore_sediment_layer_s1",
"Porosity_of_inshore_sediment_layer_s2",
"Porosity_of_inshore_sediment_layer_s3",
"Porosity_of_offshore_sediment_layer_d1",
"Porosity_of_offshore_sediment_layer_d2",
"Porosity_of_offshore_sediment_layer_d3")
#---------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Function to extract the last set of accepted gear multipler values from an annealing gear fitting run
# re-arrange in the same format as an input "fishing_gear_multiplier" file, append the current identifier
# to the file name and write to the model Param folder
#
extract_ACTmult_to_parmsfolder <- function(model, ACTmult_accepted,csv.output) {

	read.only	<- elt(model, "setup", "read.only")

	if (read.only & csv.output==TRUE) {
		message("Warning: model is read-only: accepted gear multiplier values have not been written to model parameters folder")
		return
	}

	model.ident	<- elt(model, "setup", "model.ident")
	model.path	<- elt(model, "setup", "model.path")

	fleet.model	<- elt(model, "data", "fleet.model")
	gearlabels	<- elt(fleet.model, "gear_labels")
	gearcodes	<- elt(fleet.model, "gear_codes")
	original_act	<- elt(fleet.model, "gear_activity")

	pardir		<- makepath(model.path, PARAMETERS_DIR)

	final_ACTmult <- as.numeric(ACTmult_accepted[(nrow(ACTmult_accepted)),1:12])

	#Check if any of the original input activity rates were zero, in which case the fitted multipliers are meaningless and should be reset to 1
	izero <- which(original_act==0)
	if(length(izero)>0){
		final_ACTmult[izero]<-1
	}

	output_ACTmult<-data.frame("Gear_name"=gearlabels,"Gear_code"=gearcodes,"Multiplier_to_be_applied_to_activity"=final_ACTmult)

	if(read.only==FALSE & csv.output==TRUE){
	csvfile <- csvname(pardir, "fishing_gear_multiplier", model.ident)
	writecsv(output_ACTmult, csvfile, row.names=FALSE)
	print(paste("Writing gear multiplier file :",csvfile))
	}

	output_ACTmult

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Function to extract the last set of accepted HR multipler values from an annealing HRscale fitting run
# re-arrange in the same format as an input "harvest_ratio_multiplier" file, append the current identifier
# to the file name and write to the model Param folder
#
extract_HRmult_to_parmsfolder <- function(model, HRscale_accepted,csv.output) {

	read.only	<- elt(model, "setup", "read.only")

	if (read.only & csv.output==TRUE) {
		message("Warning: model is read-only: accepted harvest ratio multiplier values have not been written to model parameters folder")
		return
	}

	model.ident	<- elt(model, "setup", "model.ident")
	model.path	<- elt(model, "setup", "model.path")

	pardir		<- makepath(model.path, PARAMETERS_DIR)

	grouplabels <- c(
		"Planktivorous_fish",
		"Demersal_fish",
		"Migratory_fish",
		"Benthos_susp-dep",
		"Benthos_carn-scav",
		"Zooplankton_carn",
		"Birds",
		"Pinnipeds",
		"Cetaceans",
		"Macrophytes"
	)

	final_HRmult <- as.numeric(HRscale_accepted[(nrow(HRscale_accepted)),(1:length(grouplabels))])

	output_HRmult<-data.frame("Group"=grouplabels,"Harvest_ratio_multiplier"=final_HRmult)

	if(read.only==FALSE & csv.output==TRUE){
		csvfile <- csvname(pardir, "harvest_ratio_multiplier", model.ident)
		writecsv(output_HRmult, csvfile, row.names=FALSE)
		print(paste("Writing harvest ratio muliplier file :",csvfile))
	}

	output_HRmult

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	GetCredInt<-function(Di,Li,Ci,var="",plotgraph=TRUE){

		#Function extracts the centiles of a vector of
		#data and their associated liklihoods

		Rset<-data.frame(R=Di)
		Rset$L<-Li

		ZRset<-Rset
		if(length((which(is.na(Rset$R)==FALSE))) > 0 ){
			ZRset<-Rset[(which(is.na(Rset$R)==FALSE)),] # Strip out any data = NA rows
		}

		ORset<-ZRset[order(ZRset$R),]

		ORset$cumL<-0
		ORset$cumPL<-0

		nonzerocheck<-sqrt( (sum(ORset$R,na.rm=TRUE))^2 )

		singularcheck <- length(unique(ORset$R)) # =1 if all values are the same

		ORset$cumL[1]<-ORset$L[1]
		for(jj in 2:nrow(ORset)){
			ORset$cumL[jj]<-ORset$cumL[jj-1]+ORset$L[jj]
		}
		ORset$cumPL<-ORset$cumL/sum(ORset$L)
		
		RZ<-rep(NA,length(Ci))

		suppressWarnings(
		if(is.na(sum(ORset$R))==FALSE & nonzerocheck>0 & singularcheck>1){
#		if(is.na(sum(ORset$R))==FALSE & nonzerocheck>0 ){
			if(plotgraph==TRUE){
				par(mfrow=c(1,1))
				plot(ORset$R,ORset$cumPL,type="l",ylim=c(0,1), main=var,xlab="Simulated values of variable",ylab="Propn. of cumulative likelihood values")
			}
			find_cred_int <- approxfun(ORset$cumPL,ORset$R,rule=2)
			RZ<-find_cred_int(Ci)
		}
		)

		if(is.na(sum(ORset$R))==TRUE){
			RZ<-rep(NA,length(Ci))
		}

		if(nonzerocheck == 0){
			RZ<-rep(0,length(Ci))
		}

		if(singularcheck == 1){
			RZ<-rep(ORset$R[1],length(Ci))
		}

		return(RZ)
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# hasExamples() is called wherever use.example=TRUE is used to ensure that the examples data package is installed and loaded
#
hasExamples <- function() {

	# couldn't get requireNamespace() to pass check()
	if (requireNamespace(DATA_PACKAGE_NAME, quietly = TRUE)) {
	#pkgs <- installed.packages()
	#if (DATA_PACKAGE_NAME %in% pkgs[, "Package"]) {
		# package is installed, check its version:
		DATA_PACKAGE_VERSION <- packageVersion(DATA_PACKAGE_NAME)
		if (DATA_PACKAGE_VERSION == PACKAGE_VERSION) {
			# everything correct
			return(TRUE)
		} else {
			# wrong version of data package installed:
			message("Error: version of installed '", DATA_PACKAGE_NAME, "' does not match that of StrathE2E2!")
			message(PACKAGE_NAME, ":", PACKAGE_VERSION)
			message(DATA_PACKAGE_NAME, ":", DATA_PACKAGE_VERSION)
			stop("Please remove package", DATA_PACKAGE_NAME, "and try again!")
		}
	}

	# data package isn't installed:
	message("Warning: the data package '", DATA_PACKAGE_NAME, "' is required when using example results!")

	installed <- FALSE
	if (interactive()) {
		# ask the user for permission to install:
		answer <- askYesNo("Do you want to install the required data package from the MarineResourceModelling repository ?")
		if (answer == TRUE) {
			install.packages(DATA_PACKAGE_NAME, repos=SRAN_REPOSITORY)
			if (requireNamespace(DATA_PACKAGE_NAME, quietly = TRUE)) {
			#pkgs <- installed.packages()
			#if (DATA_PACKAGE_NAME %in% pkgs[, "Package"]) {
				installed <- TRUE
			} else {
				message("\nError: could not install data package from repository:\n\t", SRAN_REPOSITORY)
				stop("Installation of", DATA_PACKAGE_NAME, "data package failed!")
			}
		}
	}

	# Either user said "no" to the install, or we are in a script without the package:
	if (! installed) {
		stop("Cannot continue without the data package")
	}
}

get.example.results <- function(model.name, model.variant, data.name, sub.dir=NULL) {

	message("Reading example results from StrathE2E2examples data package for the ",model.name," ",model.variant," model")

	# "1970-1999" -> "variant_1970_1999"
	valid.model.variant <- paste0("variant_", gsub("-", ".", model.variant))

	if (is.null(sub.dir)) {
		if (exists(data.name, where=StrathE2E2examples::example.results[[model.name]][[valid.model.variant]])) {
			results <- StrathE2E2examples::example.results[[model.name]][[valid.model.variant]][[data.name]]
		} else {
			stop("Cannot find example results '", data.name, "' in top level of StrathE2E2examples dataset")
		}
	} else {
		if (exists(data.name, where=StrathE2E2examples::example.results[[model.name]][[valid.model.variant]][[sub.dir]])) {
			results <- StrathE2E2examples::example.results[[model.name]][[valid.model.variant]][[sub.dir]][[data.name]]
		} else {
			stop("Cannot find example results '", data.name, "' in credint section of StrathE2E2examples dataset")
		}
	}

	results
}

