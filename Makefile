CPT_REAL_DATA_DIR = data/cpt-data
CPT_SYNTHETIC_DATA_DIR = data/synthetic-cpt-data
CPT_DATA_DIR = $(CPT_SYNTHETIC_DATA_DIR)

$(shell mkdir -p figures intermediates logs)
$(shell mkdir -p intermediates/fits-map \
	intermediates/fits-map-p25 \
	intermediates/fits-map-p100 \
	intermediates/cv-fits \
	intermediates/cv-predictions \
	intermediates/cv-scores \
	intermediates/predicted-grid-2d \
	intermediates/predicted-slices-3d)
$(shell mkdir -p figures/predicted-2d-mcmc-png \
	figures/predicted-2d-comparison-png \
	figures/predicted-3d-map-png \
	figures/predicted-3d-mcmc-png \
	figures/predicted-3d-comparison-png \
	figures/simulated-conditional-3d-mcmc-png \
	figures/traceplot-png)

DATASET_NAMES_A_FIELD = A1 A2 A3
DATASET_NAMES_B_FIELD = B1 B2 B3
DATASET_NAMES_3D = $(DATASET_NAMES_A_FIELD) $(DATASET_NAMES_B_FIELD)
DATASET_NAMES =  $(DATASET_NAMES_3D) A2-T B2-T
MODELS = GeoWarp GW-NoWarp GW-CV GW-NoWarp-CV GW-Vert-CV GW-WN-CV Linear

MODELS_SUBSET = GeoWarp Linear Binned BCS
MODELS_FULL = $(MODELS_SUBSET) GW-NoWarp GW-CV GW-NoWarp-CV GW-Vert-CV GW-WN-CV

DATA_A_FIELD = $(addprefix $(CPT_DATA_DIR)/, $(addsuffix .csv, $(DATASET_NAMES_A_FIELD)))
DATA_B_FIELD = $(addprefix $(CPT_DATA_DIR)/, $(addsuffix .csv, $(DATASET_NAMES_B_FIELD)))
DATA_JAKSA = $(CPT_REAL_DATA_DIR)/Jaksa.csv

FITS_MAP = $(addprefix intermediates/fits-map/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES)))
FITS_MAP_P25 = $(addprefix intermediates/fits-map-p25/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES)))
FITS_MAP_P100 = $(addprefix intermediates/fits-map-p100/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES)))
FITS_MCMC = $(addprefix intermediates/fits-mcmc/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES)))

FITS_3D_MAP = $(addprefix intermediates/fits-map/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES_3D)))
FITS_3D_MAP_P25 = $(addprefix intermediates/fits-map-p25/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES_3D)))
FITS_3D_MAP_P100 = $(addprefix intermediates/fits-map-p100/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES_3D)))
FITS_3D_MCMC = $(addprefix intermediates/fits-mcmc/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES_3D)))

ALPHA_BETA_SAMPLES = $(addprefix intermediates/alpha-beta-samples/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES)))

CV_FITS = $(foreach dataset,$(DATASET_NAMES),$(foreach model,$(MODELS),intermediates/cv-fits/$(model)_$(dataset).qs))
CV_FITS_JAKSA = $(foreach model,$(MODELS),intermediates/cv-fits/$(model)_Jaksa.qs)

CV_PREDICTIONS = $(foreach dataset,$(DATASET_NAMES),$(foreach model,$(MODELS),intermediates/cv-predictions/$(model)_$(dataset).qs)) \
	$(foreach dataset,$(DATASET_NAMES),intermediates/cv-predictions/Binned_$(dataset).qs)

CV_SCORES_SUBSET = $(foreach dataset,$(DATASET_NAMES),$(foreach model,$(MODELS_SUBSET),intermediates/cv-scores/$(model)_$(dataset).qs))
CV_SCORES_FULL = $(foreach dataset,$(DATASET_NAMES),$(foreach model,$(MODELS_FULL),intermediates/cv-scores/$(model)_$(dataset).qs))

CV_PREDICTIONS_JAKSA = $(foreach model,$(MODELS),intermediates/cv-predictions/$(model)_Jaksa.qs) \
	intermediates/cv-predictions/Binned_Jaksa.qs
CV_SCORES_JAKSA = $(foreach model,$(MODELS_FULL),intermediates/cv-scores/$(model)_Jaksa.qs)

VERTICAL_PROFILES = intermediates/vertical-profiles.qs

PREDICTED_GRID_2D_MAP = $(addprefix intermediates/predicted-grid-2d-map/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES)))
PREDICTED_GRID_2D_MCMC = $(addprefix intermediates/predicted-grid-2d-mcmc/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES)))

PREDICTED_SLICES_3D_MAP = $(addprefix intermediates/predicted-slices-3d-map/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES_3D)))
PREDICTED_SLICES_3D_MCMC = $(addprefix intermediates/predicted-slices-3d-mcmc/GeoWarp_, $(addsuffix .qs, $(MCMC_DATASETS)))

PREDICTED_PLOT_MEAN_3D_MAP_PDF = $(addprefix figures/predicted-3d-map-pdf/GeoWarp_, $(addsuffix -mean.pdf, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_MEAN_3D_MCMC_PDF = $(addprefix figures/predicted-3d-mcmc-pdf/GeoWarp_, $(addsuffix -mean.pdf, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_SD_3D_MAP_PDF = $(addprefix figures/predicted-3d-map-pdf/GeoWarp_, $(addsuffix -sd.pdf, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_SD_3D_MCMC_PDF = $(addprefix figures/predicted-3d-mcmc-pdf/GeoWarp_, $(addsuffix -sd.pdf, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_MEAN_DIFF_3D_PDF = $(addprefix figures/predicted-3d-comparison-pdf/GeoWarp_, $(addsuffix -mean-diff.pdf, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_SD_RATIO_3D_PDF = $(addprefix figures/predicted-3d-comparison-pdf/GeoWarp_, $(addsuffix -sd-ratio.pdf, $(DATASET_NAMES_3D)))

PREDICTED_PLOT_MEAN_3D_MAP_PNG = $(addprefix figures/predicted-3d-map-png/GeoWarp_, $(addsuffix -mean.png, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_MEAN_3D_MCMC_PNG = $(addprefix figures/predicted-3d-mcmc-png/GeoWarp_, $(addsuffix -mean.png, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_SD_3D_MAP_PNG = $(addprefix figures/predicted-3d-map-png/GeoWarp_, $(addsuffix -sd.png, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_SD_3D_MCMC_PNG = $(addprefix figures/predicted-3d-mcmc-png/GeoWarp_, $(addsuffix -sd.png, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_MEAN_DIFF_3D_PNG = $(addprefix figures/predicted-3d-comparison-png/GeoWarp_, $(addsuffix -mean-diff.png, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_SD_RATIO_3D_PNG = $(addprefix figures/predicted-3d-comparison-png/GeoWarp_, $(addsuffix -sd-ratio.png, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_COMPARISON_ASSEMBLY_3D_PNG = $(addprefix figures/predicted-3d-comparison-assembly-png/GeoWarp_, $(addsuffix .png, $(DATASET_NAMES_3D)))


TRACEPLOT_PDF = $(addprefix figures/traceplot-pdf/GeoWarp_, $(addsuffix .pdf, $(DATASET_NAMES)))
TRACEPLOT_PNG = $(addprefix figures/traceplot-png/GeoWarp_, $(addsuffix .png, $(DATASET_NAMES)))

SIMULATED_CONDITIONAL_PLOT_3D_MCMC_PDF = $(addprefix figures/simulated-conditional-3d-mcmc-pdf/GeoWarp_, $(addsuffix .pdf, $(DATASET_NAMES_3D)))
SIMULATED_CONDITIONAL_PLOT_3D_MCMC_PNG = $(addprefix figures/simulated-conditional-3d-mcmc-png/GeoWarp_, $(addsuffix .png, $(DATASET_NAMES_3D)))
SIMULATED_CONDITIONAL_PLOT_SINGLE_MCMC = $(addprefix figures/simulated-conditional-single-3d-mcmc/GeoWarp_, $(addsuffix .pdf, $(DATASET_NAMES_3D)))

CV_PREDICTION_PLOTS = $(foreach dataset,$(DATASET_NAMES),$(foreach model,$(MODELS),figures/cv-predictions/$(model)_$(dataset).pdf))
CV_PREDICTION_PLOTS_JAKSA = $(foreach model,$(MODELS),figures/cv-predictions/$(model)_Jaksa.pdf)

SITE_SUMMARIES_3D = $(addprefix figures/site-summary/, $(addsuffix .pdf, $(DATASET_NAMES_3D)))
SITE_SUMMARIES_JAKSA = figures/site-summary/Jaksa.pdf

all: all_except_cv cv

all_except_cv: figures/dataset-cpts-A.pdf \
	figures/dataset-cpts-B.pdf \
	figures/dataset-cpts-Jaksa.pdf \
	figures/summary-A2.pdf \
	figures/map-cpts-A.pdf \
	figures/map-cpts-B.pdf \
	figures/map-cpts-Jaksa.pdf \
	$(SITE_SUMMARIES_3D) \
	$(SITE_SUMMARIES_JAKSA) \
	figures/residuals-plot.pdf \
	figures/vertical-profiles-mean.pdf \
	figures/vertical-profiles-full.pdf \
	figures/vertical-profiles-n-parents.pdf \
	figures/isodistance-horizontal.png \
	figures/isodistance-vertical-easting.png \
	figures/isodistance-vertical-northing.png \
	figures/isodistance-horizontal-n-parents.pdf \
	figures/isodistance-vertical-easting-n-parents.pdf \
	figures/isodistance-vertical-northing-n-parents.pdf \
	figures/parent-example.pdf \
	figures/nu-selection-summary.txt \
	$(TRACEPLOT_PNG) \
	figures/gamma-horizontal-posterior-B3.pdf \
	figures/predicted-2d-mcmc-png/GeoWarp_A2-T.png \
	figures/predicted-2d-mcmc-png/GeoWarp_B2-T.png \
	figures/predicted-2d-comparison-png/GeoWarp_A2-T.png \
	figures/predicted-2d-comparison-png/GeoWarp_B2-T.png \
	$(PREDICTED_PLOT_MEAN_3D_MAP_PNG) \
	$(PREDICTED_PLOT_MEAN_3D_MCMC_PNG) \
	$(PREDICTED_PLOT_SD_3D_MAP_PNG) \
	$(PREDICTED_PLOT_SD_3D_MCMC_PNG) \
	$(PREDICTED_PLOT_MEAN_DIFF_3D_PNG) \
	$(PREDICTED_PLOT_SD_RATIO_3D_PNG) \
	$(PREDICTED_PLOT_COMPARISON_ASSEMBLY_3D_PNG) \
	$(SIMULATED_CONDITIONAL_PLOT_3D_MCMC_PNG) \
	$(SIMULATED_CONDITIONAL_PLOT_SINGLE_MCMC)




cv: figures/cv-metrics-plot.pdf \
	figures/cv-metrics-table.tex \
	figures/cv-metrics-table-full.tex \
	figures/cv-metrics-table-jaksa.tex \
	figures/cv-metrics-table-bootstrap.tex \
	figures/cv-metrics-table-full-bootstrap.tex \
	figures/cv-metrics-table-jaksa-bootstrap.tex \
	$(CV_PREDICTION_PLOTS) \
	$(CV_PREDICTION_PLOTS_JAKSA)


# NOTE(mgnb): Prevents these files, which are governed by pattern rules, from
# being deleted if make fails
.SECONDARY: \
	$(FITS_MAP) \
	$(FITS_MAP_P25) \
	$(FITS_MAP_P100) \
	$(FITS_MCMC) \
	$(ALPHA_BETA_SAMPLES) \
	$(TRACEPLOT_PDF) \
	$(CV_FITS) \
	$(CV_FITS_JAKSA) \
	$(CV_PREDICTIONS) \
	$(CV_PREDICTIONS_JAKSA) \
	$(CV_SCORES_FULL) \
	$(CV_SCORES_JAKSA) \
	$(PREDICTED_GRID_2D_MAP) \
	$(PREDICTED_GRID_2D_MCMC) \
	$(PREDICTED_SLICES_3D_MAP) \
	$(PREDICTED_SLICES_3D_MCMC) \
	$(PREDICTED_PLOT_MEAN_3D_MAP_PDF) \
	$(PREDICTED_PLOT_MEAN_3D_MCMC_PDF) \
	$(PREDICTED_PLOT_SD_3D_MAP_PDF) \
	$(PREDICTED_PLOT_SD_3D_MCMC_PDF) \
	$(PREDICTED_PLOT_MEAN_DIFF_3D_PDF) \
	$(PREDICTED_PLOT_SD_RATIO_3D_PDF) \
	$(SIMULATED_CONDITIONAL_PLOT_3D_MCMC_PDF) \
	figures/predicted-2d-mcmc-pdf/GeoWarp_A2-T.pdf \
	figures/predicted-2d-mcmc-pdf/GeoWarp_B2-T.pdf \
	figures/predicted-2d-comparison-pdf/GeoWarp_A2-T.pdf \
	figures/predicted-2d-comparison-pdf/GeoWarp_B2-T.pdf \
	figures/isodistance-horizontal.pdf \
	figures/isodistance-vertical-easting.pdf \
	figures/isodistance-vertical-northing.pdf


%.png: %.pdf
	convert -density 300 $< $@

figures/predicted-2d-mcmc-png/%.png: figures/predicted-2d-mcmc-pdf/%.pdf
	convert -density 300 $< $@

figures/predicted-2d-comparison-png/%.png: figures/predicted-2d-comparison-pdf/%.pdf
	convert -density 300 $< $@

figures/predicted-3d-map-png/%.png: figures/predicted-3d-map-pdf/%.pdf
	convert -density 300 $< $@

figures/predicted-3d-mcmc-png/%.png: figures/predicted-3d-mcmc-pdf/%.pdf
	convert -density 300 $< $@

figures/predicted-3d-comparison-png/%.png: figures/predicted-3d-comparison-pdf/%.pdf
	convert -density 300 $< $@

figures/predicted-3d-comparison-assembly-png/%.png: figures/predicted-3d-comparison-assembly-pdf/%.pdf
	convert -density 300 $< $@

figures/traceplot-png/%.png: figures/traceplot-pdf/%.pdf
	convert -density 300 $< $@

figures/simulated-conditional-3d-mcmc-png/%.png: figures/simulated-conditional-3d-mcmc-pdf/%.pdf
	convert -density 300 $< $@

figures/site-summary/%.pdf: \
	scripts/site-summary.R
	Rscript $< \
		--input $(CPT_DATA_DIR)/$*.csv \
		--output $@

figures/parent-example.pdf: \
	scripts/parent-example.R
	Rscript $< \
		--output $@

# Isodistances

## MCMC

figures/isodistance-horizontal.pdf: \
	scripts/isodistance-horizontal.R \
	$(FITS_3D_MCMC) \
	$(FITS_3D_MAP)
	Rscript $< \
		--mcmc-fits $(FITS_3D_MCMC) \
		--map-fits $(FITS_3D_MAP) \
		--output $@

figures/isodistance-vertical-easting.pdf: \
	scripts/isodistance-vertical.R \
	$(FITS_3D_MCMC) \
	$(FITS_3D_MAP)
	Rscript $< \
		--mcmc-fits $(FITS_3D_MCMC) \
		--map-fits $(FITS_3D_MAP) \
		--horizontal-coordinate easting \
		--output $@

figures/isodistance-vertical-northing.pdf: \
	scripts/isodistance-vertical.R \
	$(FITS_3D_MCMC) \
	$(FITS_3D_MAP)
	Rscript $< \
		--mcmc-fits $(FITS_3D_MCMC) \
		--map-fits $(FITS_3D_MAP) \
		--horizontal-coordinate northing \
		--output $@

## Comparison between choices of n_parents

figures/isodistance-horizontal-n-parents.pdf: \
	scripts/isodistance-horizontal-n-parents.R \
	$(FITS_3D_MAP) \
	$(FITS_3D_MAP_P25) \
	$(FITS_3D_MAP_P100)
	Rscript $< \
		--fits $(FITS_3D_MAP) $(FITS_3D_MAP_P25) $(FITS_3D_MAP_P100) \
		--output $@

figures/isodistance-vertical-easting-n-parents.pdf: \
	scripts/isodistance-vertical-n-parents.R \
	$(FITS_3D_MAP) \
	$(FITS_3D_MAP_P25) \
	$(FITS_3D_MAP_P100)
	Rscript $< \
		--fits $(FITS_3D_MAP) $(FITS_3D_MAP_P25) $(FITS_3D_MAP_P100) \
		--horizontal-coordinate easting \
		--output $@

figures/isodistance-vertical-northing-n-parents.pdf: \
	scripts/isodistance-vertical-n-parents.R \
	$(FITS_3D_MAP) \
	$(FITS_3D_MAP_P25) \
	$(FITS_3D_MAP_P100)
	Rscript $< \
		--fits $(FITS_3D_MAP) $(FITS_3D_MAP_P25) $(FITS_3D_MAP_P100) \
		--horizontal-coordinate northing \
		--output $@

## Vertical profiles

figures/vertical-profiles-mean.pdf: \
	scripts/vertical-profiles-mean.R \
  	$(VERTICAL_PROFILES)
	Rscript $< \
		--vertical-profiles $(VERTICAL_PROFILES) \
		--output $@

figures/vertical-profiles-full.pdf: \
	scripts/vertical-profiles-full.R \
  	$(VERTICAL_PROFILES)
	Rscript $< \
		--vertical-profiles $(VERTICAL_PROFILES) \
		--output $@

figures/vertical-profiles-n-parents.pdf: \
	scripts/vertical-profiles-n-parents.R \
  	$(FITS_MAP) \
	$(FITS_MAP_P25) \
	$(FITS_MAP_P100)
	Rscript $< \
		--fits \
			$(FITS_MAP) \
			$(FITS_MAP_P25) \
			$(FITS_MAP_P100) \
		--output $@

figures/residuals-plot.pdf: \
	scripts/residuals-plot.R \
	$(FITS_MCMC) \
	$(ALPHA_BETA_SAMPLES)
	Rscript $< \
		--fits $(FITS_MCMC) \
		--alpha-beta-samples $(ALPHA_BETA_SAMPLES) \
		--output $@

# Simulations from the conditional distribution

figures/simulated-conditional-3d-mcmc-pdf/%.pdf: \
	scripts/simulated-conditional-plot-3d.R \
  	intermediates/predicted-slices-3d-mcmc/%.qs
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--data $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--simulated-slices intermediates/predicted-slices-3d-mcmc/$*.qs \
		--output $@

figures/simulated-conditional-single-3d-mcmc/%.pdf: \
	scripts/simulated-conditional-plot-single-3d.R \
  	intermediates/predicted-slices-3d-mcmc/%.qs
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--data $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--simulated-slices intermediates/predicted-slices-3d-mcmc/$*.qs \
		--output $@

# Prediction plots

## 2-D

figures/predicted-2d-mcmc-pdf/%.pdf: \
	scripts/predicted-plot-2d.R \
  	intermediates/predicted-grid-2d-mcmc/%.qs
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--data $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--predicted-grid intermediates/predicted-grid-2d-mcmc/$*.qs \
		--output $@

figures/predicted-2d-comparison-pdf/%.pdf: \
	scripts/predicted-plot-2d-comparison.R \
  	intermediates/predicted-grid-2d-map/%.qs \
  	intermediates/predicted-grid-2d-mcmc/%.qs
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--data $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--predicted-grid-map intermediates/predicted-grid-2d-map/$*.qs \
		--predicted-grid-mcmc intermediates/predicted-grid-2d-mcmc/$*.qs \
		--output $@

## 3-D

figures/predicted-3d-map-pdf/%-mean.pdf: \
	scripts/predicted-plot-3d.R \
	intermediates/fits-map/%.qs \
  	intermediates/predicted-slices-3d-map/%.qs
	Rscript $< \
		--fit intermediates/fits-map/$*.qs \
		--variable log_q_c_mean \
		--predicted-slices intermediates/predicted-slices-3d-map/$*.qs \
		--output $@

figures/predicted-3d-map-pdf/%-sd.pdf: \
	scripts/predicted-plot-3d.R \
	intermediates/fits-map/%.qs \
  	intermediates/predicted-slices-3d-map/%.qs
	Rscript $< \
		--fit intermediates/fits-map/$*.qs \
		--variable log_q_c_sd \
		--predicted-slices intermediates/predicted-slices-3d-map/$*.qs \
		--output $@

figures/predicted-3d-mcmc-pdf/%-mean.pdf: \
	scripts/predicted-plot-3d.R \
	intermediates/fits-mcmc/%.qs \
  	intermediates/predicted-slices-3d-mcmc/%.qs
	Rscript $< \
		--fit intermediates/fits-mcmc/$*.qs \
		--variable log_q_c_mean \
		--predicted-slices intermediates/predicted-slices-3d-mcmc/$*.qs \
		--output $@

figures/predicted-3d-mcmc-pdf/%-sd.pdf: \
	scripts/predicted-plot-3d.R \
	intermediates/fits-mcmc/%.qs \
  	intermediates/predicted-slices-3d-mcmc/%.qs
	Rscript $< \
		--fit intermediates/fits-mcmc/$*.qs \
		--variable log_q_c_sd \
		--predicted-slices intermediates/predicted-slices-3d-mcmc/$*.qs \
		--output $@

figures/predicted-3d-comparison-pdf/%-mean-diff.pdf: \
	scripts/predicted-plot-3d.R \
	intermediates/fits-map/%.qs \
  	intermediates/predicted-slices-3d-map/%.qs \
  	intermediates/predicted-slices-3d-mcmc/%.qs
	Rscript $< \
		--fit intermediates/fits-map/$*.qs \
		--variable log_q_c_mean_diff \
		--predicted-slices intermediates/predicted-slices-3d-mcmc/$*.qs \
		--predicted-slices2 intermediates/predicted-slices-3d-map/$*.qs \
		--output $@

figures/predicted-3d-comparison-pdf/%-sd-ratio.pdf: \
	scripts/predicted-plot-3d.R \
	intermediates/fits-map/%.qs \
  	intermediates/predicted-slices-3d-map/%.qs \
  	intermediates/predicted-slices-3d-mcmc/%.qs
	Rscript $< \
		--fit intermediates/fits-map/$*.qs \
		--variable log_q_c_sd_ratio \
		--predicted-slices intermediates/predicted-slices-3d-mcmc/$*.qs \
		--predicted-slices2 intermediates/predicted-slices-3d-map/$*.qs \
		--output $@

figures/predicted-3d-comparison-assembly-png/%.png: \
	scripts/predicted-plot-3d-comparison-assembly.R \
	figures/predicted-3d-map-png/%-mean.png \
	figures/predicted-3d-map-png/%-sd.png \
	figures/predicted-3d-mcmc-png/%-mean.png \
	figures/predicted-3d-mcmc-png/%-sd.png \
	figures/predicted-3d-comparison-png/%-mean-diff.png \
	figures/predicted-3d-comparison-png/%-sd-ratio.png
	Rscript $< \
		--map-mean figures/predicted-3d-map-png/$*-mean.png \
		--map-sd figures/predicted-3d-map-png/$*-sd.png \
		--mcmc-mean figures/predicted-3d-mcmc-png/$*-mean.png \
		--mcmc-sd figures/predicted-3d-mcmc-png/$*-sd.png \
		--mean-diff figures/predicted-3d-comparison-png/$*-mean-diff.png \
		--sd-ratio figures/predicted-3d-comparison-png/$*-sd-ratio.png \
		--output $@

# Other

figures/gamma-horizontal-posterior-%.pdf: \
	scripts/gamma-horizontal-posterior.R \
	intermediates/fits-map/GeoWarp_%.qs \
	intermediates/fits-mcmc/GeoWarp_%.qs
	Rscript $< \
		--map-fit intermediates/fits-map/GeoWarp_$*.qs \
		--mcmc-fit intermediates/fits-mcmc/GeoWarp_$*.qs \
		--output $@

figures/cv-metrics-plot.pdf: \
	scripts/cv-metrics-plot.R \
	intermediates/cv-fit-metrics.qs
	Rscript $< \
		--cv-metrics intermediates/cv-fit-metrics.qs \
		--datasets $(DATASET_NAMES) \
  		--output $@

figures/cv-metrics-table.tex: \
	scripts/cv-metrics-table.R \
	intermediates/cv-fit-metrics-subset.qs
	Rscript $< \
		--cv-fit-metrics intermediates/cv-fit-metrics-subset.qs \
		--output $@

figures/cv-metrics-table-full.tex: \
	scripts/cv-metrics-table.R \
	intermediates/cv-fit-metrics-full.qs
	Rscript $< \
		--cv-fit-metrics intermediates/cv-fit-metrics-full.qs \
		--dash-after GeoWarp GW-NoWarp GW-WN-CV \
		--output $@

figures/cv-metrics-table-jaksa.tex: \
	scripts/cv-metrics-table-jaksa.R \
	intermediates/cv-fit-metrics-jaksa.qs
	Rscript $< \
		--cv-fit-metrics intermediates/cv-fit-metrics-jaksa.qs \
		--output $@


figures/cv-metrics-table-bootstrap.tex: \
	scripts/cv-metrics-table.R \
	intermediates/cv-fit-metrics-subset-bootstrap.qs
	Rscript $< \
		--cv-fit-metrics intermediates/cv-fit-metrics-subset-bootstrap.qs \
		--output $@

figures/cv-metrics-table-full-bootstrap.tex: \
	scripts/cv-metrics-table.R \
	intermediates/cv-fit-metrics-full-bootstrap.qs
	Rscript $< \
		--cv-fit-metrics intermediates/cv-fit-metrics-full-bootstrap.qs \
		--dash-after GeoWarp GW-NoWarp GW-WN-CV \
		--output $@

figures/cv-metrics-table-jaksa-bootstrap.tex: \
	scripts/cv-metrics-table-jaksa.R \
	intermediates/cv-fit-metrics-jaksa-bootstrap.qs
	Rscript $< \
		--cv-fit-metrics intermediates/cv-fit-metrics-jaksa-bootstrap.qs \
		--output $@

figures/cv-predictions/%.pdf: \
	scripts/cv-prediction-plot.R \
	intermediates/cv-predictions/%.qs
	Rscript $< \
		--input intermediates/cv-predictions/$*.qs \
		--output $@

figures/summary-A2.pdf: \
	scripts/summary-A2.R \
	$(CPT_DATA_DIR)/A2.csv
	Rscript $< \
		--input $(CPT_DATA_DIR)/A2.csv \
		--output $@

figures/dataset-cpts-A.pdf: \
	scripts/dataset-cpts.R \
	$(DATA_A_FIELD) \
	$(CPT_DATA_DIR)/A2-T.csv
	Rscript $< \
  		--datasets \
  			$(DATA_A_FIELD) \
  			$(CPT_DATA_DIR)/A2-T.csv \
  		--output $@

figures/dataset-cpts-B.pdf: \
	scripts/dataset-cpts.R \
	$(DATA_B_FIELD) \
	$(CPT_DATA_DIR)/B2-T.csv
	Rscript $< \
  		--datasets \
  			$(DATA_B_FIELD) \
  			$(CPT_DATA_DIR)/B2-T.csv \
  		--output $@

figures/dataset-cpts-Jaksa.pdf: \
	scripts/dataset-cpts.R \
	$(DATA_JAKSA)
	Rscript $< \
  		--datasets \
  			$(DATA_JAKSA) \
  		--output $@

figures/map-cpts-A.pdf: \
	scripts/map-cpts.R \
	$(DATA_A_FIELD)
	Rscript $< \
  		--datasets $(DATA_A_FIELD) \
  		--height 15 \
  		--output $@

figures/map-cpts-B.pdf: \
	scripts/map-cpts.R \
	$(DATA_B_FIELD)
	Rscript $< \
  		--datasets $(DATA_B_FIELD) \
  		--height 20 \
  		--output $@

figures/map-cpts-Jaksa.pdf: \
	scripts/map-cpts-jaksa.R \
	$(DATA_JAKSA)
	Rscript $< \
  		--input $(DATA_JAKSA) \
  		--output $@

figures/nu-selection-summary.txt: \
	scripts/nu-selection-summary.R \
	$(DATASETS) \
	intermediates/nu-selection-fits.qs
	Rscript $< \
		--datasets $(DATA_A_FIELD) $(DATA_B_FIELD) \
		--nu-selection-fits intermediates/nu-selection-fits.qs \
  		--output $@

figures/traceplot-pdf/%.pdf: \
	scripts/traceplot.R \
	intermediates/fits-mcmc/%.qs
	Rscript $< \
		--fit intermediates/fits-mcmc/$*.qs \
		--output $@

# Intermediates

intermediates/nu-selection-fits.qs: \
	scripts/nu-selection-fits.R \
	$(DATASETS)
	Rscript $< \
		--datasets $(DATA_A_FIELD) $(DATA_B_FIELD) \
		--output $@

intermediates/cv-fit-metrics-subset.qs: \
	scripts/cv-fit-metrics.R \
	$(CV_SCORES_SUBSET)
	Rscript $< \
		--cv-scores $(CV_SCORES_SUBSET) \
		--method t_test \
		--output $@

intermediates/cv-fit-metrics-full.qs: \
	scripts/cv-fit-metrics.R \
	$(CV_SCORES_FULL)
	Rscript $< \
		--cv-scores $(CV_SCORES_FULL) \
		--method t_test \
		--output $@

intermediates/cv-fit-metrics-jaksa.qs: \
	scripts/cv-fit-metrics.R \
	$(CV_SCORES_JAKSA)
	Rscript $< \
		--cv-scores $(CV_SCORES_JAKSA) \
		--method t_test \
		--output $@


intermediates/cv-fit-metrics-subset-bootstrap.qs: \
	scripts/cv-fit-metrics.R \
	$(CV_SCORES_SUBSET)
	Rscript $< \
		--cv-scores $(CV_SCORES_SUBSET) \
		--method blocked_bootstrap \
		--output $@

intermediates/cv-fit-metrics-full-bootstrap.qs: \
	scripts/cv-fit-metrics.R \
	$(CV_SCORES_FULL)
	Rscript $< \
		--cv-scores $(CV_SCORES_FULL) \
		--method blocked_bootstrap \
		--output $@

intermediates/cv-fit-metrics-jaksa-bootstrap.qs: \
	scripts/cv-fit-metrics.R \
	$(CV_SCORES_JAKSA)
	Rscript $< \
		--cv-scores $(CV_SCORES_JAKSA) \
		--method blocked_bootstrap \
		--output $@

$(VERTICAL_PROFILES): \
	scripts/vertical-profiles.R \
  	$(FITS_MAP) \
  	$(FITS_MCMC) \
  	$(ALPHA_BETA_SAMPLES)
	Rscript $< \
		--map-fits $(FITS_MAP) \
		--mcmc-fits $(FITS_MCMC) \
		--alpha-beta-samples $(ALPHA_BETA_SAMPLES) \
		--output $@

# CV Scores

intermediates/cv-scores/BCS_%.qs: \
	scripts/cv-scores.R \
	data/bcs-cv-predictions/BCS_%.qs
	Rscript $< \
		--input data/bcs-cv-predictions/BCS_$*.qs \
		--output $@

intermediates/cv-scores/%.qs: \
	scripts/cv-scores.R \
	intermediates/cv-predictions/%.qs
	Rscript $< \
		--input intermediates/cv-predictions/$*.qs \
		--output $@

# Predictions

## CV

intermediates/cv-predictions/Binned_%.qs: \
	scripts/cv-predictions-binned.R
	Rscript $< \
		--input $(CPT_DATA_DIR)/$*.csv \
		--output $@

intermediates/cv-predictions/%.qs: \
	scripts/cv-predictions.R \
	intermediates/cv-fits/%.qs
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--input $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--fits intermediates/cv-fits/$*.qs \
		--output $@

## 2-D

intermediates/predicted-grid-2d-map/%.qs: \
	scripts/predicted-grid-2d-map.R \
	intermediates/fits-map/%.qs
	Rscript $< \
  		--fit intermediates/fits-map/$*.qs \
  		--output $@

intermediates/predicted-grid-2d-mcmc/%.qs: \
	scripts/predicted-grid-2d-mcmc.R \
	intermediates/fits-mcmc/%.qs \
	intermediates/alpha-beta-samples/%.qs
	Rscript $< \
  		--fit intermediates/fits-mcmc/$*.qs \
  		--alpha-beta-samples intermediates/alpha-beta-samples/$*.qs \
  		--output $@

## 3-D

intermediates/predicted-slices-3d-map/%.qs: \
	scripts/predicted-slices-3d-map.R \
	intermediates/fits-map/%.qs
	Rscript $< \
  		--fit intermediates/fits-map/$*.qs \
  		--output $@

intermediates/predicted-slices-3d-mcmc/%.qs: \
	scripts/predicted-slices-3d-mcmc.R \
	intermediates/fits-mcmc/%.qs \
	intermediates/alpha-beta-samples/%.qs
	Rscript $< \
  		--fit intermediates/fits-mcmc/$*.qs \
  		--alpha-beta-samples intermediates/alpha-beta-samples/$*.qs \
  		--output $@

# Fits

intermediates/cv-fits/%.qs: \
	scripts/cv-fit.R
	$(eval MODEL_NAME := $(word 1,$(subst _, ,$*)))
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--input $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--model $(MODEL_NAME) \
		--log-file-pattern logs/cv-fits/$*_{group}.txt \
		--output $@

intermediates/alpha-beta-samples/%.qs: \
	scripts/alpha-beta-samples.R \
	intermediates/fits-mcmc/%.qs
	Rscript $< \
		--fit intermediates/fits-mcmc/$*.qs \
		--output $@

intermediates/fits-mcmc/%.qs: \
	scripts/fit-mcmc.R
	$(eval MODEL_NAME := $(word 1,$(subst _, ,$*)))
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--input $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--model $(MODEL_NAME) \
		--log-file logs/fits-mcmc/$*.txt \
		--output $@

intermediates/fits-map-p25/%.qs: \
	scripts/fit-map.R
	$(eval MODEL_NAME := $(word 1,$(subst _, ,$*)))
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--input $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--model $(MODEL_NAME) \
		--log-file logs/fits-p25/$*.txt \
		--n-parents 25 \
		--output $@

intermediates/fits-map-p100/%.qs: \
	scripts/fit-map.R
	$(eval MODEL_NAME := $(word 1,$(subst _, ,$*)))
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--input $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--model $(MODEL_NAME) \
		--log-file logs/fits-p100/$*.txt \
		--n-parents 100 \
		--output $@

intermediates/fits-map/%.qs: \
	scripts/fit-map.R
	$(eval MODEL_NAME := $(word 1,$(subst _, ,$*)))
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--input $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--model $(MODEL_NAME) \
		--log-file logs/fits/$*.txt \
		--output $@
