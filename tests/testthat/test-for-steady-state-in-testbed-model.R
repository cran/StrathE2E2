test_that("check that the testbed model achieves a steady state", {

#-----------------------------------------------------------------------------------------------------------------
#
# This test uses a 'Testbed' version of the model in which all of the external drivers are set to constant values.
# We expect the model outputs to converge to a steady state under these conditions. Then we take the masses of the
# food web variables from the steady state model output, and manually derive the uptake fluxes between them.
# These manually derived fluxes are then compared with the fluxes generated in the 'flux_matrix' outputs from the model.
# So the test is checking that the flux calculations in the C-code of the package are correctly evaluated.
#
# The Testbed model is stored in /StrathE2E2/tests/testdata/models/
#
# The model has been pre-run for >1000 years, and the end-state saved as initial conditions.
# So if the model is correctly coded and not leaking nutrient then it should be at steady state from time 0. In fact, the input csv file containing the initial conditions
# does not hold sufficient precision (number of decimal places) to ensure perfect steady state from the onset of a run.
# This is manifest as some tiny oscillations in the first year or so of a run before everything settles to its steady state.
# Hence we run the Testbed model for 20 years and then base the test on the final year of the run.
# 
# For convenience, the Testbed model uses the fitted parameters from the North Sea implementation of StrathE2E2.
# 
# In this test, select an 18 month period at the end of the run, and for each variable, test whether any point within this
# test period deviates from the mean by more than a given tolerance. 
#-----------------------------------------------------------------------------------------------------------------

#Run the testbed model - assuming R home dir is "/GitLab/StrathE2E2/tests/testthat"

nyears<-40  # number of years to run the model - the first 3 years should be ignored due to very tiny fluctuations arising from
#             lack of required precision capability in the csv files for setting initial conditions

model<-e2e_read(model.name="Testbed",
                  model.variant="Const",
                  model.ident="base",
                  models.path="../testdata/models")
results <- e2e_run(model,nyears=nyears,csv.output=FALSE)

ntimes<-nrow(results$output)
samp_period_end<-(ntimes-20)                      # end time for the 18 months sampling period
samp_period_start<-(samp_period_end-540)          # start time for the 18 months sampling period

#names(results$output[1:78]) # Lists the names of state varibales in the header

sampdata<- results$output[samp_period_start:samp_period_end,2:78]
sampmeans<-colSums(sampdata)/nrow(sampdata)

diffdata<-sampdata
devs<-sampmeans
devs[]<-0
for(jj in 1:77){
    diffdata[,jj]  <-  sqrt((sampdata[,jj]-sampmeans[jj])^2)
    devs[jj]<-max(diffdata[,jj])
}

non_stat<-length(which(devs[]>1e-7))

#non_stat

#Implement testthat check...

expect_equal(non_stat,0)

})

