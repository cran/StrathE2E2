#
# e2e_copy.R
#
#' Make a copy of a named model/variant and documentation in a user defined workspace
#'
#' @param model.name	Name of model to copy.
#' @param model.variant	Name of model variant to copy.
#' @param source.path	Relative path from the current working directory to the source model to be copied. Setting source.path="" is valid. Default source.path=NULL, meaning read a North Sea model setup from the package folder extdata/Models.
#' @param dest.path	Relative path from the current working directory to a destination address in which to create a 'Models' folder if necessary, and then copy the model files. Setting dest.path="" is valid. Default dest.path=NULL in which case the Models folder will be created in a temporary directory.
#
#' @return A copy of an entire model/variant and the associated documentation folder in the designated workspace.
#'
#' @seealso \code{\link{e2e_ls}}, \code{\link{e2e_read}}
#'
#' @export
#'
#' @examples
#' # Copy the 2003-2013 version of the North Sea model supplied with the package into a
#' # temporary folder: 
#'     e2e_copy("North_Sea", "2003-2013")
#'
#' # Dummy example illustrating copy the 2003-2013 version of the North Sea model 
#' # supplied with the package into a user-defined folder (edit "Folder/Models to 
#' # your own relative path): 
#' #   e2e_copy("North_Sea", "2003-2013",dest.path="Folder/Models")
#'
#' # Dummy example illustrating copying a user model into a user workspace:
#' # Replace "Folder1/Models" and "Folder2/Models" with your own source.path and dest.path
#' # remembering that these are relative to the current working directory.
#' # e.g.... e2e_copy("Modelname", "Modelvariant",
#' #         source.path="Folder1/Models",
#' #         dest.path="Folder2/Models")
#'
#
# ---------------------------------------------------------------------
# |                                                                   |
# | Authors: Ian Thurlbeck                                            |
# | Department of Mathematics and Statistics                          |
# | University of Strathclyde, Glasgow                                |
# |                                                                   |
# | Date of this version: May 2020                                    |
# |                                                                   |
# ---------------------------------------------------------------------

e2e_copy <- function(model.name, model.variant, source.path=NULL, dest.path=NULL) {

   oo <- options()
   on.exit(options(oo))

        message("Current working directory is '",getwd(),"'\n")

	source.path <- remove.dirsep(source.path)	# remove any trailing '/'
	dest.path <- remove.dirsep(dest.path)	# remove any trailing '/'

        if( ! is.null(source.path)) {
             source.path<-makepath(getwd(),source.path)
	     if (! dir.exists(source.path)) {
		mesg <- paste0("Error: could not find the source path '", source.path, "' !\n")
	       stop(mesg)
             }
	}

        if( ! is.null(dest.path)) {
             dest.path<-makepath(getwd(),dest.path)
	     if (! dir.exists(dest.path)) {
		mesg <- paste0("Error: could not find the destination path '", dest.path, "' !\n")
	       stop(mesg)
             }
        }

        if( is.null(dest.path) ){
           dest.path<-tempdir()
	   message("No 'dest.path' specified so the copied model files") 
           message("will be directed to the temporary directory...")
           message("'",dest.path,"'")
           message("")
        }

        dest.path<-makepath(dest.path, "Models")

	src.path <- get.variant.path(model.name, model.variant, source.path)	# path to model variant folder, either system or user folder

	dst.path <- makepath(dest.path, model.name, model.variant)
	if (dir.exists(dst.path)) {
		stop("destination folder '", dst.path, "' already exists !\n", sep="")
	}

	# create top-level model folder:
	dst.path <- makepath(dest.path, model.name)		
	create.folder(dst.path)

	message(" Copying model: '", model.name, ", variant '", model.variant, "':")
	message("  from '", src.path, "'")
	message("  to   '", makepath(dst.path, model.variant), "'\n")
	file.copy(src.path, dst.path, recursive=TRUE)

#	doc.path <- makepath(dest.path, model.name, "Documentation")
#	if (dir.exists(doc.path)) {
#		message("documentation folder '", doc.path, "' already exists !")
#	} else {
#	src.doc.path <- get.documentation.path(model.name, model.documentation="Documentation", source.path)	# path to model documentation folder, either system or user folder
#       file.copy(src.doc.path, dst.path, recursive=TRUE)
#	}

	invisible(NULL)
}

