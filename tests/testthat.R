
#
# A few long running tests make use of the testthat::skip_on_cran() mechanism so that they don't run when on CRAN
#
# This skip function relies on devtools package setting an environment variable NOT_CRAN, so if you want to run the tests
# locally you must either use devtools or set this variable explicitly yourself:
#
# cd StrathE2E2
# R
# > library(StrathE2E2)
# > Sys.setenv(NOT_CRAN="true")
# > testthat::test_dir("tests")
#

library(testthat)
library(StrathE2E2)

test_check("StrathE2E2")

