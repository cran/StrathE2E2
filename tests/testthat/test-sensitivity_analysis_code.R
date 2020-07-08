test_that("Test integrity of sensitivity analysis code", {

skip_on_cran()
skip_if_not_installed("StrathE2E2examples")


#WARNING THIS TEST WILL TAKE A LONG TIME TO RUN

model <- e2e_read("North_Sea", "1970-1999")
nyears<-1
n_traj<-2
sens_results <- e2e_run_sens(model, nyears=nyears, n_traj=n_traj, postprocess=FALSE, csv.output=FALSE, runtime.plot=FALSE)

#Each iteration involves a model run for a baseline parameter set plus a separate run of each of 453 parameters. So one trajectory should = 454 rows of output
#The output should contain 7 columns of data

ibaselines<-which(sens_results$parametername=="baseline")
itraj<-max(sens_results$trajectoryid)

	expect_equal(nrow(sens_results), (n_traj*454))
	expect_equal(ncol(sens_results), 7)
	expect_equal(length(ibaselines), n_traj)
	expect_equal(itraj, n_traj)
})

test_that("Test the integrity of the post-processing code for sensitivity analysis data", {

skip_on_cran()
skip_if_not_installed("StrathE2E2examples")


model <- e2e_read("North_Sea", "1970-1999")

sorted_sens_results<-e2e_process_sens_mc(model, selection="SENS",use.example=TRUE, csv.output=FALSE)

#Expect 453 rows of data in sorted_sens_results and 10 columns

#select the rows where EEmean is not NA (NA occurs when a parameter value is zero)
valid_sorted_sens_results<-sorted_sens_results[which(is.na(sorted_sens_results$EEmean)==FALSE),]

#If the sorting routine is working correctly then:
#First row of valid data should be the largest value of sqrt(EEmean^2)
#Last row of valid data should be the smallest value of sqrt(EEmean^2)
largest<-which(sqrt((valid_sorted_sens_results$EEmean)^2) == max(sqrt((valid_sorted_sens_results$EEmean)^2)))
smallest<-which(sqrt((valid_sorted_sens_results$EEmean)^2) == min(sqrt((valid_sorted_sens_results$EEmean)^2)))

	expect_equal(nrow(sorted_sens_results), 453)
	expect_equal(ncol(sorted_sens_results), 10)
	expect_equal(largest, 1)
	expect_equal(smallest, nrow(valid_sorted_sens_results))
	
})

