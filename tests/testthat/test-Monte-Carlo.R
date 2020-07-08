test_that("test that the Monte-Carlo function produces correct output structure", {

skip_on_cran()

#-----------------------------------------------------------------------------------------------------------------
#
# This test runs the Monte-Carlo procedure to determine
# whether the ouputs conform to the expected structure.
#
# The test uses the 1970-1999 North Sea model which is provide in the package, with the csv.output argument set to FALSE
#
# The returned object should be a list of 10 elements. The 4th and 6th element should have 3 sub-elements
#
# We test various attributes of the list and its contents to check that they are as expected.
#
#------------------------------------------------------------------------------------------------------------------


# Read the internal 1970-1999 North Sea model
model<-e2e_read(model.name="North_Sea",
                   model.variant="1970-1999",
                   model.ident="TEST")
nyears<-3
n_iter<-10
test_run <- e2e_run_mc(model,nyears=nyears,baseline.mode=TRUE,n_iter=n_iter,csv.output=FALSE,runtime.plot=FALSE)

#--------------

#Extract some attributes of the returned list object
n_1stlev <- nrow(summary(test_run))  # Number of 1st level objects in the list - should = 10
n_2ndlev1 <- nrow(summary(test_run$CI_annual_avmass))  # Number of parameter objects in the parameter data level of the list - should = 3
n_2ndlev2 <- nrow(summary(test_run$CI_annual_fluxes))  # Number of parameter objects in the parameter data level of the list - should = 3

#--------------

#Extract attributes of the first elements of the list
nr_parms<-nrow(test_run$parameter_record) # number of rows of data in the parameter history - should = n_iter

parms_lik <- test_run$parameter_record$likelihood  # Likelihoods for parameter set on each iteration - should all be < 1
nneg<-length(which(parms_lik>1))  # nneg should=0

#--------------

# Check that the credible interval outputs are ordered as expected in the output structure
# use one object as a test
pf_test<-test_run$CI_annual_avmass$whole[,1]
#First element of pf_test should be the maximum likelihood value
#Elements 2, 3, 4, 5, 6 should be lower limit, lower quartile, median, upper quartile, upper limit
#So, unless there are NAs,
#  [6] notlessthan [5] notlessthan [4] notlessthan [3] notlessthan [2]

t1<-0
t2<-0
t3<-0
t4<-0
if( (is.na(pf_test[6])==FALSE) & (is.na(pf_test[5])==FALSE) ){
	if(pf_test[6]<pf_test[5]) t1<-1
}
if( (is.na(pf_test[5])==FALSE) & (is.na(pf_test[4])==FALSE) ){
	if(pf_test[5]<pf_test[4]) t2<-1
}
if( (is.na(pf_test[4])==FALSE) & (is.na(pf_test[3])==FALSE) ){
	if(pf_test[4]<pf_test[3]) t3<-1
}
if( (is.na(pf_test[3])==FALSE) & (is.na(pf_test[2])==FALSE) ){
	if(pf_test[3]<pf_test[2]) t4<-1
}


#--------------


#Implement the testthat checks

expect_equal(n_1stlev, 10)
expect_equal(n_2ndlev1, 3)
expect_equal(n_2ndlev2, 3)
expect_equal(nr_parms, n_iter)
expect_equal(nneg,0)
expect_equal(t1,0)
expect_equal(t2,0)
expect_equal(t3,0)
expect_equal(t4,0)

})
