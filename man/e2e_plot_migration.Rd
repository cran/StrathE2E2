% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e2e_plot_migration.R
\name{e2e_plot_migration}
\alias{e2e_plot_migration}
\title{Plot daily data on migration fluxes by actively mobile guilds during the final year of a run, optionally with credible intervals.}
\usage{
e2e_plot_migration(
  model,
  ci.data = FALSE,
  use.saved = FALSE,
  use.example = FALSE,
  results = NULL
)
}
\arguments{
\item{model}{R-list object defining the baseline model configuration used to generate the data and compiled by the e2e_read() function.}

\item{ci.data}{Logical. If TRUE plot credible intervals around model results based on Monte Carlo simulation with the e2e_run_mc() function (default=FALSE).}

\item{use.saved}{Logical. If TRUE use data from a prior user-defined run held as csv files data in the current results folder (default=FALSE).}

\item{use.example}{Logical. If TRUE use pre-computed example data from the internal North Sea model rather than user-generated data (default=FALSE).}

\item{results}{R-list object of model output generated by the e2e_run() function. Only needed if ci.data=FALSE, use.saved=FALSE and use.example=FALSE. (Default=NULL).}
}
\value{
Graphical display in a new graphics window.
}
\description{
Generate a multi-panel set of one-year time series plots of the mass fluxes between inshore and offshore zones due to migration by actively mobile guilds in the 
ecology model: all three fish guilds, birds, pinnipeds and cetaceans. The default is to plot data from a single model run but if available, 
credible intervals of model output from a Monte Carlo analysis can be plotted instead.
}
\details{
Daily interval post-processed data from the Monte Carlo e2e_run_mc() function are stored in the file
../Modelname/Variantname/CredInt/CredInt_processed_daily_migrations-*.csv, where * represents the model run identifier
(model.ident) text embedded in the R-list object created by the e2e_read() function. The path to this file is relative to the value of results.path
as set by a prior e2e_read() function call.

Arguments determine the source of model data to be plotted. These can be outputs from a single model run with data held in memory as a list object or in a saved csv file, or from 
a Monte Carlo simulation (using the function e2e_run_mc()) to estimate credible intervals of model outputs. Generation of credible interval data is a long computing task, so 
data for the North Sea model provided with the package are available as example data sets.

Each panel of the plot shows a time-series of the net flux densities (mMN/d in the model domain as a whole, assumed to be 1 m2 sea surface area) of one
of the migratory guilds in the model (all three guilds of fish, birds, pinnipeds and cetaceans) between the inshore and offshore zones
of the model, over the final year of a run. These migration fluxes are the dynamic product of gradients in the ratio of food
concentration to predator concentration across the inshore-offshore boundary. Positive values of the net migration flux indicate net movement from the 
offshore to inshore zone. Negative values indicate net movement from inshore to offshore.

If plotting of credible intervals is selected, results from the maximum likelihood model are shown by a red line. The median of the credible values distribution
is shown my a solid black line. A grey-shaded area indicates the 50% credible interval (spanning quartiles of the cumulative likelihood
of simulated values). Black dashed lines span the 99% credible interval.

For details of how the distribution of credible output values from StrathE2E are calculated see the help information for the e2e_run_mc() function.
}
\examples{
# Load the 1970-1999 version of the North Sea model supplied with the package, 
# run, and generate a plot:
    model <- e2e_read("North_Sea", "1970-1999")
    results <- e2e_run(model, nyears=2,csv.output=FALSE)
    e2e_plot_migration(model, results=results)

# Direct the graphics output to a pdf file ... 
# or jpeg("plot.jpg"), png("plot.png")
    pdf(file.path(tempdir(), "plot.pdf"),width=8,height=6)
    e2e_plot_migration(model, results=results)
    dev.off()

# For the same model, plot the example data with credible intervals:
# This example requires the Strathe2E2examples supplementary data package.
if(require(StrathE2E2examples)){
    e2e_plot_migration(model, ci.data=TRUE, use.example=TRUE)
}

}
\seealso{
\code{\link{e2e_read}}, \code{\link{e2e_run}}, \code{\link{e2e_run_mc}}, \code{\link{e2e_plot_eco}}, \code{\link{e2e_plot_catch}}, \code{\link{e2e_plot_trophic}}, \code{\link{e2e_plot_biomass}}
}
