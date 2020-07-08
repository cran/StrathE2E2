
# To re-generate the tests, cd to this folder (tests/testdata/plots) and type:
#
#	Rscript --vanilla plotall.R
#
# at various points we set the RNG seed to make the results replicable
#

library(StrathE2E2)

NPLOTS <- 53
PLOT <- 1

set.seed(2020)
model <- e2e_read("North_Sea","1970-1999")
results <- e2e_run(model,nyear=5,csv.output=FALSE)

#
# e2e_plot_ts:
#
message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("ts_ecology_variables.png")
e2e_plot_ts(model, results, selection="CATCH")
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("ts_fishery_statistics.png")
e2e_plot_ts(model, results, selection="ECO")
q<-dev.off()


#
# e2e_optimize_eco and e2e_plot_opt_disgnostics:
#
set.seed(2020)
model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
model$data$physics.drivers$so_temp <- model$data$physics.drivers$so_temp+3   # add 3 degC to upper layer offshore temperatures
model$data$physics.drivers$si_temp <- model$data$physics.drivers$si_temp+3   # add 3 degC to inshore temperatures
model$data$physics.drivers$d_temp  <- model$data$physics.drivers$d_temp+3    # add 3 degC to lower layer offshore temperatures
test_run <- e2e_optimize_eco(model, nyears=5, n_iter=10, start_temperature=0.4,csv.output=FALSE, runtime.plot=FALSE)

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("opt_eco.png")
plot_data <- e2e_plot_opt_diagnostics(model,selection="ECO",results=test_run)
q<-dev.off()


#
# e2e_optimize_hr and e2e_plot_opt_disgnostics
#

set.seed(2020)
model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
# This model is already optimised to the observed ecosystem data supplied with the package
# Perturb the temperature driving to knock the model away from its maximum likelihood state relative to the target data:
model$data$physics.drivers$so_temp <- model$data$physics.drivers$so_temp+3   # add 3 degC to upper layer offshore temperatures
model$data$physics.drivers$si_temp <- model$data$physics.drivers$si_temp+3   # add 3 degC to inshore temperatures
model$data$physics.drivers$d_temp  <- model$data$physics.drivers$d_temp+3    # add 3 degC to lower layer offshore temperatures
test_run  <- e2e_optimize_hr(model, nyears=5, n_iter=10, start_temperature=0.4,csv.output=FALSE, runtime.plot=FALSE)

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("opt_hr.png")
plot_data <- e2e_plot_opt_diagnostics(model,selection="HR",results=test_run)
q<-dev.off()


#
# e2e_optimize_act (eco) and e2e_plot_opt_disgnostics
#

set.seed(2020)
model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
# This model is already optimised to the observed ecosystem data supplied with the package
# Perturb the temperature driving to knock the model away from its maximum likelihood state relative to the target data:
model$data$physics.drivers$so_temp <- model$data$physics.drivers$so_temp+3   # add 3 degC to upper layer offshore temperatures
model$data$physics.drivers$si_temp <- model$data$physics.drivers$si_temp+3   # add 3 degC to inshore temperatures
model$data$physics.drivers$d_temp  <- model$data$physics.drivers$d_temp+3    # add 3 degC to lower layer offshore temperatures
test_run  <- e2e_optimize_act(model, selection="ECO", n_iter=10, start_temperature=0.4, cooling=0.975, csv.output=FALSE, nyears=5, runtime.plot=FALSE)

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("opt_act_eco.png")
plot_data <- e2e_plot_opt_diagnostics(model,selection="ACT", fitted.to="ECO",results=test_run)
q<-dev.off()

############################################################################

#e2e_optimize_act (hr) and e2e_plot_opt_disgnostics

set.seed(2020)
model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
# Activity rates in this model are already optimised to the target harvest ratios supplied with the package but we woud not expect
# to recover these values in this short demonstration run
test_run  <- e2e_optimize_act(model, selection="HR", n_iter=100, start_temperature=1.0, cooling=0.985, csv.output=FALSE, n_traj=5, runtime.plot=FALSE)

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("opt_act_hr.png")
plot_data <- e2e_plot_opt_diagnostics(model,selection="ACT",fitted.to="HR",results=test_run)
q<-dev.off()


############################################################################

#
# e2e_plot_edrivers
#

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("edrivers.png")
model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
e2e_plot_edrivers(model)
q<-dev.off()

############################################################################

#
# e2e_plot_fdrivers
#

model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("fdrivers_activity.png")
plotted_data<-e2e_plot_fdrivers(model, selection="ACTIVITY")
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("fdrivers_abrasion.png")
plotted_data<-e2e_plot_fdrivers(model, selection="ABRASION")
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("fdrivers_harvestr.png")
plotted_data<-e2e_plot_fdrivers(model, selection="HARVESTR")
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("fdrivers_discards.png")
plotted_data<-e2e_plot_fdrivers(model, selection="DISCARDS")
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("fdrivers_offal.png")
plotted_data<-e2e_plot_fdrivers(model, selection="OFFAL")
q<-dev.off()


############################################################################

#e2e_plot_eco

set.seed(2020)
model <- e2e_read("North_Sea", "1970-1999")
results <- e2e_run(model, nyears=3,csv.output=FALSE)

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_nut_phyt.png")
e2e_plot_eco(model, selection="NUT_PHYT",results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_sediment.png")
e2e_plot_eco(model, selection="SEDIMENT",results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_zooplankton.png")
e2e_plot_eco(model, selection="ZOOPLANKTON",results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_fish.png")
e2e_plot_eco(model, selection="FISH",results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_benthos.png")
e2e_plot_eco(model, selection="BENTHOS",results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_predators.png")
e2e_plot_eco(model, selection="PREDATORS",results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_corp_disc.png")
e2e_plot_eco(model, selection="CORP_DISC",results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_macrophyte.png")
e2e_plot_eco(model, selection="MACROPHYTE",results=results)
q<-dev.off()

# +CI

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_nut_phyt_ci.png")
e2e_plot_eco(model, selection="NUT_PHYT",ci.data=TRUE,use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_sediment_ci.png")
e2e_plot_eco(model, selection="SEDIMENT",ci.data=TRUE,use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_zooplankton_ci.png")
e2e_plot_eco(model, selection="ZOOPLANKTON",ci.data=TRUE,use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_fish_ci.png")
e2e_plot_eco(model, selection="FISH",ci.data=TRUE,use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_benthos_ci.png")
e2e_plot_eco(model, selection="BENTHOS",ci.data=TRUE,use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_predators_ci.png")
e2e_plot_eco(model, selection="PREDATORS",ci.data=TRUE,use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_corp_disc_ci.png")
e2e_plot_eco(model, selection="CORP_DISC",ci.data=TRUE,use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("eco_macrophyte_ci.png")
e2e_plot_eco(model, selection="MACROPHYTE",ci.data=TRUE,use.example=TRUE)
q<-dev.off()

# some boxplots:

set.seed(2020)
model <- e2e_read("North_Sea", "1970-1999")
results <- e2e_run(model, nyears=10,csv.output=FALSE)

# annual:

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("annual_compare_obs.png")
e2e_compare_obs("ANNUAL", model, ci.data=FALSE, use.saved=FALSE, use.example=FALSE, results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("annual_compare_obs_ci.png")
e2e_compare_obs("ANNUAL", model, ci.data=TRUE, use.saved=FALSE, use.example=TRUE, results=NULL)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("annual_compare_runs.png")
m1 <- e2e_read("North_Sea", "1970-1999")
r1<-e2e_run(m1,nyears=3,csv.output=FALSE)
m2 <- e2e_read("North_Sea", "2003-2013")
r2<-e2e_run(m2,nyears=3,csv.output=FALSE)
e2e_compare_runs_box("ANNUAL",  model1=m1, ci.data1=TRUE, use.example1=TRUE, model2=m2, ci.data2=FALSE, results2=r2)
q<-dev.off()

# monthly:

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("monthly_compare_obs.png")
model <- e2e_read("North_Sea", "1970-1999")
results <- e2e_run(model,nyears=3,csv.output=FALSE)
e2e_compare_obs("MONTHLY", model, ci.data=FALSE, use.saved=FALSE, use.example=FALSE, results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("monthly_compare_obs_ci.png")
model <- e2e_read("North_Sea", "1970-1999")
e2e_compare_obs("MONTHLY", model, ci.data=TRUE, use.saved=FALSE, use.example=TRUE, results=NULL)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("monthly_compare_runs.png")
m1 <- e2e_read("North_Sea", "1970-1999")
r1<-e2e_run(m1,nyears=3,csv.output=FALSE)
m2 <- e2e_read("North_Sea", "2003-2013")
r2<-e2e_run(m2,nyears=3,csv.output=FALSE)
e2e_compare_runs_box("MONTHLY",  model1=m1, ci.data1=TRUE, use.example1=TRUE, model2=m2, ci.data2=FALSE, results2=r2)
q<-dev.off()


# and some more:

set.seed(2020)
model <- e2e_read("North_Sea", "1970-1999")
results <- e2e_run(model, nyears=3)

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_biomass.png")
e2e_plot_biomass(model, results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_biomass_saved.png")
e2e_plot_biomass(model, use.saved=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_biomass_ci.png")
e2e_plot_biomass(model, ci.data=TRUE, use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_catch_by_gear.png")
e2e_plot_catch(model, results, selection="BY_GEAR")
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_catch_by_guild.png")
e2e_plot_catch(model, results, selection="BY_GUILD")
q<-dev.off()

# migration:

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_migration.png")
# ci.data == FALSE & use.saved==FALSE & is.list(results)==TRUE
e2e_plot_migration(model, results=results)					# plot_final_year_migration_data(model=model, results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_migration_ci1.png")
# ci.data == TRUE & use.saved==FALSE & use.example==TRUE
e2e_plot_migration(model,ci.data=TRUE, use.saved=FALSE, use.example=TRUE)	# plot_final_year_migration_data_with_ci(model=model, use.example=use.example)
q<-dev.off()



message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_sens.png")
e2e_plot_sens_mc(model, selection="SENS", use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_mc.png")
e2e_plot_sens_mc(model, selection="MC", use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_trophic.png")
e2e_plot_trophic(model, results=results)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_trophic_ci.png")
e2e_plot_trophic(model, ci.data=TRUE, use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_ycurve_planktiv.png")
pf_yield_data<-e2e_plot_ycurve(model, selection="PLANKTIV", use.example=TRUE)
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_ycurve_demersal.png")
pf_yield_data<-e2e_plot_ycurve(model, selection="DEMERSAL", use.example=TRUE)
q<-dev.off()

# A quick demonstration run in baseline mode and save the data to csv files:
set.seed(2020)
basemodel <- e2e_read("North_Sea", "1970-1999",model.ident="mcbaseline")
basedemo <- e2e_run_mc(basemodel,nyears=2,baseline.mode=TRUE, n_iter=10,csv.output=TRUE, runtime.plot=FALSE)

# --------------------------------------------------------------------------------------
# Then a quick demonstration run in scenario mode using the saved baseline parameter data, and save to csv:
# First create an extreme fishing scenario - quadruple some gear activities, run for 10 year
scenariomodel<-basemodel
scenariomodel$setup$model.ident <- "mcscenario"
scenariomodel$data$fleet.model$gear_mult[1] <- 4 # Gear 1 (Pelagic trawls) activity rate rescaled to 4*baseline
scenariomodel$data$fleet.model$gear_mult[4] <- 4 # Gear 4 (Beam_Trawl_BT1+BT2) activity rate rescaled to 4*baseline
scendemo <- e2e_run_mc(scenariomodel,nyears=10,baseline.mode=FALSE, use.example.baseparms=FALSE, baseparms.ident="mcbaseline",begin.sample=1, n_iter=10,csv.output=TRUE, runtime.plot=FALSE)

# Compare the results of the baseline and scenario:
basemodel <- e2e_read("North_Sea", "1970-1999", model.ident="mcbaseline")
scenariomodel <- e2e_read("North_Sea", "1970-1999",model.ident="mcscenario")

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_compare_runs_annual.png")
e2e_compare_runs_box(selection="ANNUAL", model1=basemodel, ci.data1=TRUE, use.saved1=TRUE, model2=scenariomodel, ci.data2=TRUE, use.saved2=TRUE )
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_compare_runs_monthly.png")
e2e_compare_runs_box(selection="MONTHLY", model1=basemodel, ci.data1=TRUE, use.saved1=TRUE, model2=scenariomodel, ci.data2=TRUE, use.saved2=TRUE )
q<-dev.off()


set.seed(2020)
base_model   <- e2e_read("North_Sea", "1970-1999",model.ident="baseline")
base_results <- e2e_run(base_model,nyears=5)

# Create a scenario run from 1970-1999 baseline:
scen1_model   <- base_model      # Copies the baseline configuration into a new model object
scen1_model$setup$model.ident <- "scenario1"
scen1_model$data$fleet.model$gear_mult[4] <- 0.5 # Gear 4 (Beam_Trawl_BT1+BT2) activity rate rescaled to 0.5*baseline
scen1_results <- e2e_run(scen1_model,nyears=30)

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_compare_runs_amm.png")
#Compare the annual average mass scenario data with the baseline
mdiff_results1 <- e2e_compare_runs_bar(model1=NA, use.saved1=FALSE, results1=base_results,
                                       model2=NA, use.saved2=FALSE, results2=scen1_results,
                                       selection="AAM",
                                       log.pc="PC", zone="W",
                                       bpmin=(-40),bpmax=(+40),
                                       maintitle="Beam Trawl activity reduced by half")
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_compare_runs_catch.png")
#Create a second sceanario run
scen2_model   <- base_model     # Copies the baseline configuration into a new model object
scen2_model$setup$model.ident <- "scenario2"
scen2_model$data$fleet.model$gear_mult[1] <- 0.5 # Gear 1 (Pelagic_Trawl+Seine) activity rate rescaled to 0.5*baseline
scen2_results <- e2e_run(scen2_model,nyears=30)

#Compare the annual catches in the new scenario with the baseline previously saved in a csv files
mdiff_results2 <- e2e_compare_runs_bar(model1=base_model, use.saved1=TRUE, results1=NA,
                                        model2=NA,use.saved2=FALSE, results2=scen2_results,
                                        selection="CATCH",
                                        log.pc="LG", zone="W",
                                        bpmin=(-0.4),bpmax=(+0.6),
                                        maintitle="Pelagic Trawl/Seine activity reduced by half")
q<-dev.off()


# yields

model <- e2e_read("North_Sea", "1970-1999")
pfhr=c(0,0.5,0.75,1.0,1.25,1.5,2.0,2.5,3.0)

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_pf_yield.png")
pf_yield_data <- e2e_run_ycurve(model,selection="PLANKTIV",nyears=5,HRvector=pfhr,HRfixed=1,csv.output=FALSE)
data <- e2e_plot_ycurve(model,selection="PLANKTIV", results=pf_yield_data, title="Planktivorous yield with baseline demersal fishing")
q<-dev.off()

message("\n\n############ PLOT ", PLOT, " of ", NPLOTS, " ###################"); PLOT <- PLOT+1
png("plot_df_yield.png")
df_yield_data <- e2e_run_ycurve(model,selection="DEMERSAL",nyears=5,HRvector=pfhr,HRfixed=1,csv.output=FALSE)
data <- e2e_plot_ycurve(model,selection="DEMERSAL", results=df_yield_data, title="Demersal yield with baseline demersal fishing")
q<-dev.off()

unlink("results", recursive=TRUE)

