#
# e2e_ls.R
#
#' List the available models in a designated workspace.
#'
#' @param models.path Relative path from the current working directory to a folder containing a library of model configurations (typically "Folder/Models"). Setting models.path="" is valid. Default models.path=NULL, meaning read a North Sea model setup from the package folder extdata/Models.
#'
#' @return An on-screen list of available models in the designated folder.
#'
#' @seealso \code{\link{e2e_copy}}, \code{\link{e2e_read}}
#'
#' @export
#'
#' @examples
#' # List the models/variants supplied with the package:
#'     e2e_ls()
#'
#' # Dummy example to illustrate listing the models/variants in a user defined
#' # workspace. REPLACE "Folder1/Models" with your own models.path 
#' # remembering that this are relative to the current working directory.
#' #    e2e_ls("Folder/Models")
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

e2e_ls <- function(models.path=NULL) {

   oo <- options()
   on.exit(options(oo))

#        if( ! is.null(models.path) ) {
        message("Current working directory is '",getwd(),"'\n")
#        }

	models.path <- remove.dirsep(models.path)	# remove any trailing '/'

        models.r.path<-models.path   # Keep a record of the relative path

        if( ! is.null(models.path)) {
             models.path<-makepath(getwd(),models.path)
	     if (! dir.exists(models.path)) {
		mesg <- paste0("Error: could not find the model path '", models.path, "' !\n")
	       stop(mesg)
             }
	}

	if (is.null(models.path)) {
		# get path to internal model folder
		full.path <- system.file("extdata/Models", package="StrathE2E2")
	}
	else {
		full.path <- models.path
	}

	# build up list of models:
	models <- list()
	for (model.dir in list.files(full.path)) {
		model.path <- makepath(full.path, model.dir)				# path to model dir
		if (dir.exists(model.path)) {
			variants <- list()
			for (variant.dir in list.files(model.path)) {
				variant.path <- makepath(model.path, variant.dir)	# path to variant dir
				if (dir.exists(variant.path)) {
					# exists, but check for model setup file:
					setup.file <- makepath(variant.path, MODEL_SETUP)
					if (file.exists(setup.file)) {
						# model/variant/setup.csv exist, so treat this as a model
						variants <- c(variants, variant.dir)
					}
				}
			}
			if (length(variants)) {
				models[[model.dir]] <- variants
			}
		}
	}

	if (length(models)) {
		if (is.null(models.path)) {
			message("List of package models in system folder \"extdata/Models\", with examples of how to read them:\n")
		} else {
			message("List of user models in folder \"", models.r.path, "\" with examples of how to read them:\n")
		}
		for (model in names(models)) {
			message(" Model: \"", model, "\"")
			for (variant in models[[model]]) {

                               if(is.null(models.r.path)){
				message("  Variant: \"", variant, "\"\tmodel <- e2e_read(\"", model, "\", \"", variant, "\"",")")
                               } else {
				message("  Variant: \"", variant, "\"\tmodel <- e2e_read(\"", model, "\", \"", variant, "\"",", models.path=\"", models.r.path,"\")")
                               }
			}

	                message("")
		}
	}
	else {
		message("Error: could not find any models in path '", full.path, "' !")
	}

#	message("")
}

