% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e2e_run_mc.R
\name{e2e_run_mc}
\alias{e2e_run_mc}
\title{Run a Monte Carlo simulation with StrathE2E and derive centiles of credible values of model outputs.}
\usage{
e2e_run_mc(
  model,
  nyears = 50,
  baseline.mode = TRUE,
  use.example.baseparms = FALSE,
  baseparms.ident = "",
  begin.sample = 1,
  n_iter = 1000,
  csv.output = FALSE,
  runtime.plot = TRUE,
  postprocess = TRUE
)
}
\arguments{
\item{model}{R-list object defining the model configuration (baseline or scenario), compiled by the e2e_read() function.}

\item{nyears}{Number of years to run each instance of the model. Needs to be long enough to allow the model to attain a stationary state (default=50).}

\item{baseline.mode}{Logical. If TRUE then the simulation adopts a baseline mode. If FALSE then scenario mode (default=TRUE).}

\item{use.example.baseparms}{Logical. Value required only if baseline.mode=FALSE. If TRUE then the baseline parameters set is drawn from example data provided with the package. If FALSE then a user generated baseline parameter set is expected.}

\item{baseparms.ident}{Default = "". Value required if baseline.run=FALSE and use.example.baseparms=FALSE, in which case this is the model.ident string of the a user generated baseline parameter set. Remember to include the phrase within "" quotes.}

\item{begin.sample}{Default = 1. Value required if baseline.mode=FALSE. Value sets the first row in the file of baseline parameter data from which the sequence of parameters sets to be used in this run will be taken. Set a value > 1 if this run is part of a parallel set of runs.}

\item{n_iter}{Number of iterations of the model (default=1000). If baseline.mode=FALSE and the number of sets in the supplied baseline parameter file beyond the value of begin.sample is less than n_iter, then n_iter is reset to the remaining number available.}

\item{csv.output}{Logical. If TRUE then enable writing of csv output files (default=FALSE).}

\item{runtime.plot}{Logical. If FALSE then disable runtime plotting of the progress of the run - useful for testing (default=TRUE).}

\item{postprocess}{Logical. If FALSE then disable post-processing of the cumulative outputs, for example in parallel mode where multiple runs are going to be combined later (default=TRUE).}
}
\value{
Depends on argument settings. List object containing processed data if postprocess=TRUE, otherwise a NULL object; csv files of raw and if processed data if csv.output=TRUE; real-time graphical displays during the simulation if runtime.plot=TRUE
}
\description{
The Monte Carlo scheme provided with the StrathE2E2 package has two modes of operation. The first is referred to as
‘baseline mode’. This involves generating a list of ecology model parameter sets and, for each set, determining the
likelihood of observational data consistent with the environmental and fishery drivers. The model outputs
generated with each set are then weighted by the likelihood before deriving quantiles of their distribution.
The quantile ranges then represent credible intervals of the model outputs.
}
\details{
The second mode of operation is referred to as the ‘scenario mode’. In this case, the parameter sets and their
associated likelihoods from a baseline mode simulation, are used to generate model outputs for scenarios of
environmental or fishing drivers – e.g. increased activity by selected gears, or warmer sea temperatures.
These scenario outputs are then weighted by the baseline mode likelihoods before derivation of quantiles
and credible intervals.

In baseline mode, the Monte Carlo scheme generates an ensemble of model runs by sampling the ecology model parameters from uniform distibutions
centred on an initial (baseline) parameter set. Ideally, this should be the maximum likelihood parameter set arrived at by prior application the various 
optimization functions in the package to 'fit' the model to a suite of observational data on the state of ecosystem
(target data for fitting are in /Modelname/Variantname/Target/annual_observed_*.csv).
Each iteration in the Monte Carlo scheme then generates a unique time series of model outputs at daily intervals,
together with the overall likelihood of the obervational target data. From these data, the function then derives credible intervals of each output by
weighting the values from each parameter set by the associated likelihood.

Running a scenario mode simulation requires a baseline model to have been previously completed, in order to generate a list of parameter sets and associated
likelihoods. In scenario mode, model driving data (e.g. temperatures or fishing activities) are varied from the baseline configuration by editing
the model object generated by a e2e_read() function call, and the model is then run with the existing baseline model parameter sets, and the results
weighted by the baseline mode likelihoods in order to derive the credible intervals.

The choice of baseline vs scenario mode of simulations is determined in the function by a logical argument setting 'baseline.mode'.

In baseline mode, the coefficients of variation for jiggling the ecology parameter can be varied in real-time
during the run by editing the file "monte_carlo.csv" in the folder
/Param/control/ of the model version. However, it is recommended to leave the setting constant for the duration of a run.
A CV of 0.10 to 0.15 is recommended. If comparing the credible intervals for two different baseline models then it is important to use the same CV in both cases.

In scenario model, the parameter sets are pre-ordained by a prior baseline simulation, so the CV setting is ineffective.

On completion of all the iterations of the model, whether in baseline or scenarion mode, for each model output variable in turn, values from the individual runs and
their associated model likelihoods are assembled as a list of paired values. The list is then sorted by ascending values of the model variable,
and the cumulative likelihoods with increasing value of model variable is calculated. Values for the model variable at standard proportions
(0.005, 0.25, 0.5, 0.75, 0.995) of the maximum cumulative likelhood are then extracted by interpolation.
These represent the credible intervals of model output given uncertainty in the ecology model parameters.

Outputs from the e2e_run_mc() function depend on whether post-processing is selected. If not, then raw data are saved as csv files
(provided that the argument csv.output=TRUE), and the function also returns a dataframe of the parameter sets used in the run and their corresponding likelihoods. In scenario mode this is just a replica of the 
baseline mode data that have been used to run the simulation. If post-processing is selected, then 
both the raw and processed credible interval data are saved to csv files (provided that the argument csv.output=TRUE), and the function returns a list object which includes the parameter
dataframe plus all the processed credible interval data structures. csv files are saved in the folder /results/Modelname/Variantname/CredInt/ with an
identifier for the simulation (model.ident) created by the e2e_read() function. There are two types of output. First is simply an
accumulation of all the standard outputs from StrathE2E on a run-by-run basis. The second is a set of files containing the derived credible intervals.

The function displays a real-time graphic to show the progress of the simulation. During the StrathE2E-running phase of the
process an x-y graph is updated after each iteration (x-axis=iterations, y-axis=Likelihood of the target data), with a horizontal grey line showing the
baseline (maximum liklihood) result, and black symbols showing the likelihood for each iteration based on the parameter values sampled from the baseline.
The y-axis limits for the graph can be varied in real-time
during the run by editing the file "monte_carlo.csv" in the folder /Param/control/ of the model version.

During the post-processing phase, a message is displayed as each output variable is processed.

WARNING - the scheme can take a long time to run (~2 days with the default settings), and generate large output files (11 files total 3.2 Mb per iteration). 
Check the memory capacity of you machine before starting a long run. It can make sense to 
parallelize the process by splitting across different processors and merging the results afterwards. The package contains a function for merging cumulative output files from 
multiple runs and post-processing the combined data.

In baseline mode, parallel instances of the function can be implemented on different processors, all starting from the same
initial parameter set. During the subsequent merging process, the outputs from the first parameter set, of all but the first instance, are stripped away and discarded.

Impementing parallel runs in scenario mode requires a different approach. Here, the parameter sets are pre-ordained and it is important to avoid using duplicate sets in the simulations.
Hence, the function includes an argument 'begin.sample' to select a pointer in the baseline parameter set to begin sampling in any scenario mode run. For example, in the first instance, begin.sample=1, and the number
of iterations (n_iter) might be set to e.g. 200. For the second instance the pointer (begin.sample) would then be set to 201, and so on. If begin.sample > 1, the function runs the first baseline parameter set (sample 1)
first before proceeding to the pointer begin.sample and completing the requested number of iterations, so there will be one extra iteration in the output files. If the requested number of iterations 
means that sampling would over the end of available number of parameter sets
then the number of iterations is reduced accordingly.

Although the e2e_run_mc() function generates large raw data files (11 files total 3.2 Mb per iteration), the processed data are much smaller (13 files total ~4 Mb regardless of
the nunber of iterations).
}
\examples{
# The examples provided here are illustration of how to set up and run Monte Carlo
# analyses. Even though they are stripped-down minimalist examples they each still
# take 10-15 minutes to run.

# --------------------------------------------------------------------------

\donttest{
# A quick demonstration of baseline mode with postprocessing, but csv output disabled.
# Load the 1970-1999 version of the North Sea model supplied with the package
# Run the Monte Carlo process and generate some data but disable csv output.
#    model <- e2e_read("North_Sea", "1970-1999")
#    demo <- e2e_run_mc(model,nyears=2,baseline.mode=TRUE,n_iter=5,csv.output=FALSE)
# View the structure of the returned list:
#    str(demo,max.level=1)
# View the structure of a returned list element containing sub-sets:
#    str(demo$CI_annual_avmass,max.level=1)
# View the structure of a returned list element containing sub-sets:
#    str(demo$CI_annual_fluxes,max.level=1)
# View the top few rows on the whole domain data on annual averge mass:
#    head(demo$CI_annual_avmass$whole)
}

# --------------------------------------------------------------------------

# Dummy example to illustrate a more meaningful run. Output directed to 
# "../Folder/results" relative to the current working directory (REPLACE with your
# own results.path before running):
#    model <- e2e_read("North_Sea", "1970-1999",results.path="Folder/results")
#    mc_results <- e2e_run_mc(model,baseline.mode=TRUE,nyears=50,n_iter=1000,csv,output=TRUE)
# WARNING: This run will take about 48h to complete, much better to split up and spread
# across multiple processors !

# --------------------------------------------------------------------------

\donttest{
# A quick demonstration run in baseline mode, save the data to a temporary folder:
    basemodel <- e2e_read("North_Sea", "1970-1999",model.ident="mcbaseline")
    basedemo <- e2e_run_mc(basemodel,nyears=2,baseline.mode=TRUE,
                           n_iter=5,csv.output=TRUE)
}

# --------------------------------------------------------------------------
\donttest{
# Then a quick demonstration run in scenario mode using the saved baseline parameter data,
# from the previous example, and save to csv. It is assumed that the baseline parameter 
# data are in the temporary folder in which they were created, and that the same temporary
# folder is used for this example.
# First create an extreme fishing scenario - quadruple some gear activities, run for 10 years
    scenariomodel<-basemodel
    scenariomodel$setup$model.ident <- "mcscenario"
# Gear 1 (Pelagic trawls) activity rate rescaled to 4*baseline:
    scenariomodel$data$fleet.model$gear_mult[1] <- 4
# Gear 4 (Beam_Trawl_BT1+BT2) activity rate rescaled to 4*baseline:
    scenariomodel$data$fleet.model$gear_mult[4] <- 4 
    scendemo <- e2e_run_mc(scenariomodel,nyears=2,baseline.mode=FALSE, 
                           use.example.baseparms=FALSE, baseparms.ident="mcbaseline",
                           begin.sample=1, n_iter=5,csv.output=TRUE)

# Compare the results of the baseline and scenario:
    basemodel <- e2e_read("North_Sea", "1970-1999", model.ident="mcbaseline")
    scenariomodel <- e2e_read("North_Sea","1970-1999",model.ident="mcscenario")
    e2e_compare_runs_box(selection="ANNUAL", model1=basemodel, ci.data1=TRUE, use.saved1=TRUE,
                         model2=scenariomodel, ci.data2=TRUE, use.saved2=TRUE )
    dev.new()
    e2e_compare_runs_box(selection="MONTHLY", model1=basemodel, ci.data1=TRUE, use.saved1=TRUE,
                         model2=scenariomodel, ci.data2=TRUE, use.saved2=TRUE )
}

# --------------------------------------------------------------------------

\donttest{
# Quick demonstration of parallelizing the process in baseline mode, with output folder
# to a temporary folder. To explore further details of results.path="YourFolder"
# relative to the current working directory.
# Launch batch 1 (on processor 1):
    model1 <- e2e_read("North_Sea", "1970-1999",  model.ident="BATCH1")
    results1 <- e2e_run_mc(model1,nyears=2,baseline.mode=TRUE,
                           n_iter=5,csv.output=TRUE,postprocess=FALSE)                
# Launch batch 2 (on processor 2):
    model2 <- e2e_read("North_Sea", "1970-1999", model.ident="BATCH2")
    results2 <- e2e_run_mc(model2,nyears=2,baseline.mode=TRUE,
                           n_iter=6,csv.output=TRUE,postprocess=FALSE) 
# Note that these two runs return only raw data since postprocess=FALSE

# Note that Batch 2 requests 6 iterations, rather that 5 in Batch 1.
# The number of iterations do not have to be the same in each batch.
# However, the first in each batch has to use the initial parameter set from the model setup,
# as this is the parent for all the subsequent samples generated during the run.
# When we come to merge the data from separate batches, data from the first iteration are
# stripped off and discarded for all but the first batch as we do not want to include duplicate
# data in the combined files. Hence we choose 6 iterations here in Batch 2 to make the point,
# and we expect the combined data to include 10 iterations.
#
# Then, afterwards, merge the two raw results files with text-tags BATCH1 and BATCH2,
# and post process the combined file:
    model3 <- e2e_read("North_Sea", "1970-1999", model.ident="COMBINED")
    processed_data <- e2e_merge_sens_mc(model3, selection="MC",
                      ident.list<-c("BATCH1","BATCH2"), postprocess=TRUE, csv.output=TRUE)
# or...
    combined_data <- e2e_merge_sens_mc(model3, selection="MC",
                     ident.list<-c("BATCH1","BATCH2"), postprocess=FALSE, csv.output=TRUE)
    processed_data<-e2e_process_sens_mc(model3,selection="MC",use.example=FALSE,
                                        csv.output=TRUE)

# Plot the parameter likelihood distributions from the combined data
    e2e_plot_sens_mc(model3, selection="MC")
}

# --------------------------------------------------------------------------

\donttest{
# Example of parallelizing the process in scenario mode, using the baseline parameter
# sets 'COMBINED' from above (assuming that this is sitting in the same temporary folder
# in which it was created in the example above.
# The activity of all fishing gears is reduced to zero to create a no-fishing scenario.
# Run each batch for 10 years as a relatively quick demo - a real run would need to
# run for at least 40 year
# Launch batch 1 (in processor 1):
    model1s <- e2e_read("North_Sea", "1970-1999", model.ident="BATCH1_S")
# Activity rates of all 12 gears rescaled to 0*baseline:
    model1s$data$fleet.model$gear_mult[1:12] <- 0
    results1s <- e2e_run_mc(model1s,nyears=2,baseline.mode=FALSE, baseparms.ident="COMBINED",
                 begin.sample=1, n_iter=5,csv.output=TRUE,postprocess=FALSE)                
# Launch batch 2 (on processore 2):
    model2s <- e2e_read("North_Sea", "1970-1999", model.ident="BATCH2_S")
# Activity rates of all 12 gears rescaled to 0*baseline:
    model2s$data$fleet.model$gear_mult[1:12] <- 0
    results2s <- e2e_run_mc(model2s,nyears=2,baseline.mode=FALSE, baseparms.ident="COMBINED",
                 begin.sample=6, n_iter=5,csv.output=TRUE,postprocess=FALSE)
# Note that Batch 1 samples rows 1:5 of the baseline mode parameter set archive "COMBINED"
# (begin.sample=1, n_iter=5), so Batch 2 needs to start sampling at row 6 (begin.sample=6).
# The baseline archive contains 10 rows, so Batch 2 has the capacity to undertake up to 5
# sample iterations (rows 6:10). If we select more than 5 iterations (e.g. n_iter=8) then
# the code will automatically restrict to 5. Note that in fact, to be consistent with the 
# format of output files from the baseline mode, each scenario mode run where 
# 'begin.sample' > 1 will complete n_iter+1 iterations, by adding an initial run using the 
# parameter values from row 1 of the baseline parameter set - which is then stripped off 
# during merging. 
#
# Then, merge the two raw results files with text-tags BATCH1_S and BATCH2_S, and post
# process the combined file:
    model3s <- e2e_read("North_Sea", "1970-1999", model.ident="COMBINED_S")
    processed_datas <- e2e_merge_sens_mc(model3s, selection="MC",
                       ident.list<-c("BATCH1_S","BATCH2_S"), postprocess=TRUE, csv.output=TRUE)

# Finally plot comparisons of the baseline and scenario model runs:
    e2e_compare_runs_box(selection="ANNUAL", model1=model3, ci.data1=TRUE, use.saved1=TRUE,
                         model2=model3s, ci.data2=TRUE, use.saved2=TRUE )
    dev.new()
    e2e_compare_runs_box(selection="MONTHLY", model1=model3, ci.data1=TRUE, use.saved1=TRUE,
                         model2=model3s, ci.data2=TRUE, use.saved2=TRUE )
}

# --------------------------------------------------------------------------

}
\seealso{
\code{\link{e2e_read}}, \code{\link{e2e_merge_sens_mc}}, \code{\link{e2e_process_sens_mc}}, \code{\link{e2e_plot_sens_mc}}
}
