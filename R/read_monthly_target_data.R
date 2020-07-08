#
# read_monthly_target_data.R
#
#' read monthly target data
#'
#' returns target data
#'
#' @param model.path path to current model/version folder
#'
#' @return target data
#'
#' @noRd
#
# ------------------------------------------------------------------------------

read_monthly_target_data <- function(model.path) {

	read.model.setup(model.path)

	monthlytargetdata <- get.model.file(model.path, TARGET_DATA_DIR, file.pattern=MONTHLY_TARGET_DATA)

	# Column 1 = Month
	# Column 2 = Variable
	# Column 3 = median
	# Column 4 = lower_centile
	# Column 5 = upper_centile
	# Column 6 = Units
	# Column 7 = low_cent_value
	# Column 8 = upp_cent_value
	# Column 9 = Comments

	monthlytargetdata
}

