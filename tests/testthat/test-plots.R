test_that("plotting checks", {

        skip_on_cran()
        skip_if_not_installed("StrathE2E2examples")

	library(visualTest)

	tmpfile <- tempfile(fileext = ".png")

	set.seed(2020)
	model <- e2e_read("North_Sea","1970-1999")
	results <- e2e_run(model,nyear=5,csv.output=FALSE)

	png(tmpfile)
	e2e_plot_ts(model, results, selection="CATCH")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/ts_ecology_variables.png")))

	png(tmpfile)
	e2e_plot_ts(model, results, selection="ECO")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/ts_fishery_statistics.png")))

	set.seed(2020)
	model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
	model$data$physics.drivers$so_temp <- model$data$physics.drivers$so_temp+3   # add 3 degC to upper layer offshore temperatures
	model$data$physics.drivers$si_temp <- model$data$physics.drivers$si_temp+3   # add 3 degC to inshore temperatures
	model$data$physics.drivers$d_temp  <- model$data$physics.drivers$d_temp+3    # add 3 degC to lower layer offshore temperatures

	png(tmpfile)
	test_run <- e2e_optimize_eco(model, nyears=5, n_iter=10, start_temperature=0.4,csv.output=FALSE, runtime.plot=FALSE)
	plot_data <- e2e_plot_opt_diagnostics(model,selection="ECO",results=test_run)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/opt_eco.png")))

	set.seed(2020)
	model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
	# This model is already optimised to the observed ecosystem data supplied with the package
	# Perturb the temperature driving to knock the model away from its maximum likelihood state relative to the target data:
	model$data$physics.drivers$so_temp <- model$data$physics.drivers$so_temp+3   # add 3 degC to upper layer offshore temperatures
	model$data$physics.drivers$si_temp <- model$data$physics.drivers$si_temp+3   # add 3 degC to inshore temperatures
	model$data$physics.drivers$d_temp  <- model$data$physics.drivers$d_temp+3    # add 3 degC to lower layer offshore temperatures
	test_run  <- e2e_optimize_hr(model, nyears=5, n_iter=10, start_temperature=0.4,csv.output=FALSE, runtime.plot=FALSE)

	png(tmpfile)
	plot_data <- e2e_plot_opt_diagnostics(model,selection="HR",results=test_run)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/opt_hr.png")))

	set.seed(2020)
	model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
	# This model is already optimised to the observed ecosystem data supplied with the package
	# Perturb the temperature driving to knock the model away from its maximum likelihood state relative to the target data:
	model$data$physics.drivers$so_temp <- model$data$physics.drivers$so_temp+3   # add 3 degC to upper layer offshore temperatures
	model$data$physics.drivers$si_temp <- model$data$physics.drivers$si_temp+3   # add 3 degC to inshore temperatures
	model$data$physics.drivers$d_temp  <- model$data$physics.drivers$d_temp+3    # add 3 degC to lower layer offshore temperatures
	test_run  <- e2e_optimize_act(model, selection="ECO", n_iter=10, start_temperature=0.4, cooling=0.975, csv.output=FALSE, nyears=5, runtime.plot=FALSE)

	png(tmpfile)
	plot_data <- e2e_plot_opt_diagnostics(model,selection="ACT", fitted.to="ECO",results=test_run)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/opt_act_eco.png")))

	set.seed(2020)
	model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
	# Activity rates in this model are already optimised to the target harvest ratios supplied with the package but we woud not expect
	# to recover these values in this short demonstration run
	test_run  <- e2e_optimize_act(model, selection="HR", n_iter=100, start_temperature=1.0, cooling=0.985, csv.output=FALSE, n_traj=5, runtime.plot=FALSE)

	png(tmpfile)
	plot_data <- e2e_plot_opt_diagnostics(model,selection="ACT",fitted.to="HR",results=test_run)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/opt_act_hr.png")))

	png(tmpfile)
	model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")
	e2e_plot_edrivers(model)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/edrivers.png")))

	model<-e2e_read(model.name="North_Sea", model.variant="1970-1999", model.ident="test")

	png(tmpfile)
	plotted_data<-e2e_plot_fdrivers(model, selection="ACTIVITY")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/fdrivers_activity.png")))

	png(tmpfile)
	plotted_data<-e2e_plot_fdrivers(model, selection="ABRASION")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/fdrivers_abrasion.png")))

	png(tmpfile)
	plotted_data<-e2e_plot_fdrivers(model, selection="HARVESTR")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/fdrivers_harvestr.png")))

	png(tmpfile)
	plotted_data<-e2e_plot_fdrivers(model, selection="DISCARDS")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/fdrivers_discards.png")))

	png(tmpfile)
	plotted_data<-e2e_plot_fdrivers(model, selection="OFFAL")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/fdrivers_offal.png")))

	############################################################################

	#e2e_plot_eco

	set.seed(2020)
	model <- e2e_read("North_Sea", "1970-1999")
	results <- e2e_run(model, nyears=3,csv.output=FALSE)

	png(tmpfile)
	e2e_plot_eco(model, selection="NUT_PHYT",results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_nut_phyt.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="SEDIMENT",results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_sediment.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="ZOOPLANKTON",results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_zooplankton.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="FISH",results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_fish.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="BENTHOS",results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_benthos.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="PREDATORS",results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_predators.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="CORP_DISC",results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_corp_disc.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="MACROPHYTE",results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_macrophyte.png")))

	# +CI

	png(tmpfile)
	e2e_plot_eco(model, selection="NUT_PHYT",ci.data=TRUE,use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_nut_phyt_ci.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="SEDIMENT",ci.data=TRUE,use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_sediment_ci.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="ZOOPLANKTON",ci.data=TRUE,use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_zooplankton_ci.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="FISH",ci.data=TRUE,use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_fish_ci.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="BENTHOS",ci.data=TRUE,use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_benthos_ci.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="PREDATORS",ci.data=TRUE,use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_predators_ci.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="CORP_DISC",ci.data=TRUE,use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_corp_disc_ci.png")))

	png(tmpfile)
	e2e_plot_eco(model, selection="MACROPHYTE",ci.data=TRUE,use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/eco_macrophyte_ci.png")))

	# some boxplots:

	set.seed(2020)
	model <- e2e_read("North_Sea", "1970-1999")
	results <- e2e_run(model, nyears=10,csv.output=FALSE)

	# annual:

	png(tmpfile)
	e2e_compare_obs("ANNUAL", model, ci.data=FALSE, use.saved=FALSE, use.example=FALSE, results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/annual_compare_obs.png")))

	png(tmpfile)
	e2e_compare_obs("ANNUAL", model, ci.data=TRUE, use.saved=FALSE, use.example=TRUE, results=NULL)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/annual_compare_obs_ci.png")))

	png(tmpfile)
	m1 <- e2e_read("North_Sea", "1970-1999")
	r1<-e2e_run(m1,nyears=3,csv.output=FALSE)
	m2 <- e2e_read("North_Sea", "2003-2013")
	r2<-e2e_run(m2,nyears=3,csv.output=FALSE)
	e2e_compare_runs_box("ANNUAL",  model1=m1, ci.data1=TRUE, use.example1=TRUE, model2=m2, ci.data2=FALSE, results2=r2)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/annual_compare_runs.png")))

	# monthly:

	png(tmpfile)
	model <- e2e_read("North_Sea", "1970-1999")
	results <- e2e_run(model,nyears=3,csv.output=FALSE)
	e2e_compare_obs("MONTHLY", model, ci.data=FALSE, use.saved=FALSE, use.example=FALSE, results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/monthly_compare_obs.png")))

	png(tmpfile)
	model <- e2e_read("North_Sea", "1970-1999")
	e2e_compare_obs("MONTHLY", model, ci.data=TRUE, use.saved=FALSE, use.example=TRUE, results=NULL)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/monthly_compare_obs_ci.png")))

	png(tmpfile)
	m1 <- e2e_read("North_Sea", "1970-1999")
	r1<-e2e_run(m1,nyears=3,csv.output=FALSE)
	m2 <- e2e_read("North_Sea", "2003-2013")
	r2<-e2e_run(m2,nyears=3,csv.output=FALSE)
	e2e_compare_runs_box("MONTHLY",  model1=m1, ci.data1=TRUE, use.example1=TRUE, model2=m2, ci.data2=FALSE, results2=r2)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/monthly_compare_runs.png")))

	# and some more:

	set.seed(2020)
	model <- e2e_read("North_Sea", "1970-1999")
	results <- e2e_run(model, nyears=3)

	png(tmpfile)
	e2e_plot_biomass(model, results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_biomass.png")))

	png(tmpfile)
	e2e_plot_biomass(model, use.saved=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_biomass_saved.png")))

	png(tmpfile)
	e2e_plot_biomass(model, ci.data=TRUE, use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_biomass_ci.png")))

	png(tmpfile)
	e2e_plot_catch(model, results, selection="BY_GEAR")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_catch_by_gear.png")))

	png(tmpfile)
	e2e_plot_catch(model, results, selection="BY_GUILD")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_catch_by_guild.png")))

	png(tmpfile)
	e2e_plot_migration(model, results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_migration.png")))

	png(tmpfile)
	e2e_plot_migration(model,ci.data=TRUE, use.saved=FALSE, use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_migration_ci1.png")))

	png(tmpfile)
	e2e_plot_sens_mc(model, selection="SENS", use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_sens.png")))

	png(tmpfile)
	e2e_plot_sens_mc(model, selection="MC", use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_mc.png")))

	png(tmpfile)
	e2e_plot_trophic(model, results=results)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_trophic.png")))

	png(tmpfile)
	e2e_plot_trophic(model, ci.data=TRUE, use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_trophic_ci.png")))

	png(tmpfile)
	pf_yield_data<-e2e_plot_ycurve(model, selection="PLANKTIV", use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_ycurve_planktiv.png")))

	png(tmpfile)
	pf_yield_data<-e2e_plot_ycurve(model, selection="DEMERSAL", use.example=TRUE)
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_ycurve_demersal.png")))


	set.seed(2020)
	basemodel <- e2e_read("North_Sea", "1970-1999",model.ident="mcbaseline")
	basedemo <- e2e_run_mc(basemodel,nyears=2,baseline.mode=TRUE, n_iter=10,csv.output=TRUE, runtime.plot=FALSE)

	scenariomodel<-basemodel
	scenariomodel$setup$model.ident <- "mcscenario"
	scenariomodel$data$fleet.model$gear_mult[1] <- 4 # Gear 1 (Pelagic trawls) activity rate rescaled to 4*baseline
	scenariomodel$data$fleet.model$gear_mult[4] <- 4 # Gear 4 (Beam_Trawl_BT1+BT2) activity rate rescaled to 4*baseline
	scendemo <- e2e_run_mc(scenariomodel,nyears=10,baseline.mode=FALSE, use.example.baseparms=FALSE, baseparms.ident="mcbaseline",begin.sample=1, n_iter=10,csv.output=TRUE, runtime.plot=FALSE)

	# Compare the results of the baseline and scenario:
	basemodel <- e2e_read("North_Sea", "1970-1999", model.ident="mcbaseline")
	scenariomodel <- e2e_read("North_Sea", "1970-1999",model.ident="mcscenario")

	png(tmpfile)
	e2e_compare_runs_box(selection="ANNUAL", model1=basemodel, ci.data1=TRUE, use.saved1=TRUE, model2=scenariomodel, ci.data2=TRUE, use.saved2=TRUE )
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_compare_runs_annual.png")))

	png(tmpfile)
	e2e_compare_runs_box(selection="MONTHLY", model1=basemodel, ci.data1=TRUE, use.saved1=TRUE, model2=scenariomodel, ci.data2=TRUE, use.saved2=TRUE )
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_compare_runs_monthly.png")))


	#############################

	set.seed(2020)
	base_model   <- e2e_read("North_Sea", "1970-1999",model.ident="baseline")
	base_results <- e2e_run(base_model,nyears=5)

	# Create a scenario run from 1970-1999 baseline:
	scen1_model   <- base_model      # Copies the baseline configuration into a new model object
	scen1_model$setup$model.ident <- "scenario1"
	scen1_model$data$fleet.model$gear_mult[4] <- 0.5 # Gear 4 (Beam_Trawl_BT1+BT2) activity rate rescaled to 0.5*baseline
	scen1_results <- e2e_run(scen1_model,nyears=30)

	png(tmpfile)
	#Compare the annual average mass scenario data with the baseline
	mdiff_results1 <- e2e_compare_runs_bar(model1=NA, use.saved1=FALSE, results1=base_results,
                                       model2=NA, use.saved2=FALSE, results2=scen1_results,
                                       selection="AAM",
                                       log.pc="PC", zone="W",
                                       bpmin=(-40),bpmax=(+40),
                                       maintitle="Beam Trawl activity reduced by half")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_compare_runs_amm.png")))

	png(tmpfile)
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
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_compare_runs_catch.png")))

	# yields

	model <- e2e_read("North_Sea", "1970-1999")
	pfhr=c(0,0.5,0.75,1.0,1.25,1.5,2.0,2.5,3.0)

	png(tmpfile)
	pf_yield_data <- e2e_run_ycurve(model,selection="PLANKTIV",nyears=5,HRvector=pfhr,HRfixed=1,csv.output=FALSE)
	data <- e2e_plot_ycurve(model,selection="PLANKTIV", results=pf_yield_data, title="Planktivorous yield with baseline demersal fishing")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_pf_yield.png")))

	png(tmpfile)
	df_yield_data <- e2e_run_ycurve(model,selection="DEMERSAL",nyears=5,HRvector=pfhr,HRfixed=1,csv.output=FALSE)
	data <- e2e_plot_ycurve(model,selection="DEMERSAL", results=df_yield_data, title="Demersal yield with baseline demersal fishing")
	dev.off()
	expect_true(isSimilar(tmpfile, getFingerprint("../testdata/plots/plot_df_yield.png")))

	# remove the generated results folder:
	unlink("results", recursive=TRUE)

})


