CPT_DATA_DIR = data/synthetic-cpt-data

$(shell mkdir -p figures intermediates logs)
$(shell mkdir -p intermediates/fits \
	intermediates/cv-fits \
	intermediates/cv-predictions \
	intermediates/predicted-grid-2d \
	intermediates/predicted-slices-3d \
	intermediates/simulated-slices-3d \
	intermediates/predicted-single-3d)
$(shell mkdir -p figures/prediction-2d-png \
	figures/prediction-3d-png \
	figures/simulation-conditional-3d-png \
	figures/simulation-unconditional-3d-png)

DATASET_NAMES_A_FIELD = A1 A2 A3
DATASET_NAMES_B_FIELD = B1 B2 B3
DATASET_NAMES_3D = $(DATASET_NAMES_A_FIELD) $(DATASET_NAMES_B_FIELD)
DATASET_NAMES =  $(DATASET_NAMES_3D) A2-T B2-T
MODELS = GeoWarp GW-Vert-CV GW-CV Linear

DATA_A_FIELD = $(addprefix $(CPT_DATA_DIR)/, $(addsuffix .csv, $(DATASET_NAMES_A_FIELD)))
DATA_B_FIELD = $(addprefix $(CPT_DATA_DIR)/, $(addsuffix .csv, $(DATASET_NAMES_B_FIELD)))

FITS = $(addprefix intermediates/fits/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES)))
FITS_3D = $(addprefix intermediates/fits/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES_3D)))

PREDICTED_SLICES_3D = $(addprefix intermediates/predicted-slices-3d/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES_3D)))
SIMULATED_SLICES_3D = $(addprefix intermediates/simulated-slices-3d/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES_3D)))
PREDICTED_SINGLE_3D = $(addprefix intermediates/predicted-single-3d/GeoWarp_, $(addsuffix .qs, $(DATASET_NAMES_3D)))
CV_PREDICTIONS = $(foreach dataset,$(DATASET_NAMES),$(foreach model,$(MODELS),intermediates/cv-predictions/$(model)_$(dataset).qs))

PREDICTED_PLOT_MEAN_3D_PDF = $(addprefix figures/prediction-3d-pdf/GeoWarp_, $(addsuffix -mean.pdf, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_SD_3D_PDF = $(addprefix figures/prediction-3d-pdf/GeoWarp_, $(addsuffix -sd.pdf, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_MEAN_3D_PNG = $(addprefix figures/prediction-3d-png/GeoWarp_, $(addsuffix -mean.png, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_SD_3D_PNG = $(addprefix figures/prediction-3d-png/GeoWarp_, $(addsuffix -sd.png, $(DATASET_NAMES_3D)))
PREDICTED_PLOT_SINGLE_3D = $(addprefix figures/prediction-single-3d/GeoWarp_, $(addsuffix .pdf, $(DATASET_NAMES_3D)))

SIMULATED_CONDITIONAL_PLOT_3D_PDF = $(addprefix figures/simulation-conditional-3d-pdf/GeoWarp_, $(addsuffix .pdf, $(DATASET_NAMES_3D)))
SIMULATED_CONDITIONAL_PLOT_3D_PNG = $(addprefix figures/simulation-conditional-3d-png/GeoWarp_, $(addsuffix .png, $(DATASET_NAMES_3D)))
SIMULATED_CONDITIONAL_PLOT_SINGLE = $(addprefix figures/simulation-conditional-single-3d/GeoWarp_, $(addsuffix .pdf, $(DATASET_NAMES_3D)))

SIMULATED_UNCONDITIONAL_PLOT_3D_PDF = $(addprefix figures/simulation-unconditional-3d-pdf/GeoWarp_, $(addsuffix .pdf, $(DATASET_NAMES_3D)))
SIMULATED_UNCONDITIONAL_PLOT_3D_PNG = $(addprefix figures/simulation-unconditional-3d-png/GeoWarp_, $(addsuffix .png, $(DATASET_NAMES_3D)))
SIMULATED_UNCONDITIONAL_PLOT_SINGLE = $(addprefix figures/simulation-unconditional-single-3d/GeoWarp_, $(addsuffix .pdf, $(DATASET_NAMES_3D)))

CV_PREDICTION_PLOTS = $(foreach dataset,$(DATASET_NAMES),$(foreach model,$(MODELS),figures/cv-predictions/$(model)_$(dataset).pdf))

SITE_SUMMARIES_3D = $(addprefix figures/site-summary/, $(addsuffix .pdf, $(DATASET_NAMES_3D)))

all: all_except_cv \
	figures/cv-metrics-plot.pdf \
	figures/cv-metrics-table.tex \
	figures/cv-metrics-table-full.tex \
	$(CV_PREDICTION_PLOTS)

all_except_cv: figures/dataset-cpts-A.pdf \
	figures/dataset-cpts-B.pdf \
	figures/summary-A2.pdf \
	figures/map-cpts-A.pdf \
	figures/map-cpts-B.pdf \
	$(PREDICTED_PLOT_MEAN_3D_PNG) \
	$(PREDICTED_PLOT_SD_3D_PNG) \
	$(PREDICTED_PLOT_SINGLE_3D) \
	figures/prediction-2d-png/GeoWarp_A2-T.png \
	figures/prediction-2d-png/GeoWarp_B2-T.png \
	$(SIMULATED_CONDITIONAL_PLOT_3D_PNG) \
	$(SIMULATED_CONDITIONAL_PLOT_SINGLE) \
	$(SIMULATED_UNCONDITIONAL_PLOT_3D_PNG) \
	$(SIMULATED_UNCONDITIONAL_PLOT_SINGLE) \
	$(SITE_SUMMARIES_3D) \
	figures/vertical-profiles.pdf \
	figures/isodistance-horizontal.pdf \
	figures/isodistance-vertical.pdf \
	figures/parent-example.pdf \
	figures/nu-selection-summary.txt

# NOTE(mgnb): Prevents these files, which are governed by pattern rules, from
# being deleted if make fails
.SECONDARY: \
	$(FITS) \
	$(PREDICTED_SLICES_3D) \
	$(SIMULATED_SLICES_3D) \
	$(PREDICTED_SINGLE_3D) \
	$(PREDICTED_PLOT_MEAN_3D_PDF) \
	$(PREDICTED_PLOT_SD_3D_PDF) \
	figures/predicted-plot-A2-T.pdf \
	figures/predicted-plot-B2-T.pdf \
	$(SIMULATED_CONDITIONAL_PLOT_3D_PDF) \
	$(SIMULATED_UNCONDITIONAL_PLOT_3D_PDF)

%.png: %.pdf
	convert -density 300 $< $@

figures/prediction-2d-png/%.png: figures/prediction-2d-pdf/%.pdf
	convert -density 300 $< $@

figures/prediction-3d-png/%.png: figures/prediction-3d-pdf/%.pdf
	convert -density 300 $< $@

figures/simulation-conditional-3d-png/%.png: figures/simulation-conditional-3d-pdf/%.pdf
	convert -density 300 $< $@

figures/simulation-unconditional-3d-png/%.png: figures/simulation-unconditional-3d-pdf/%.pdf
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

figures/isodistance-horizontal.pdf: \
	scripts/isodistance-horizontal.R \
  	$(FITS_3D)
	Rscript $< \
		--fits $(FITS_3D) \
		--output $@

figures/isodistance-vertical.pdf: \
	scripts/isodistance-vertical.R \
  	$(FITS_3D)
	Rscript $< \
		--fits $(FITS_3D) \
		--output $@

figures/vertical-profiles.pdf: \
	scripts/vertical-profiles.R \
  	$(FITS)
	Rscript $< \
		--fits $(FITS) \
		--output $@

figures/simulation-conditional-3d-pdf/GeoWarp_%.pdf: \
	scripts/simulation-conditional-plot-3d.R \
	intermediates/fits/GeoWarp_%.qs \
  	intermediates/predicted-slices-3d/GeoWarp_%.qs
	Rscript $< \
		--fit intermediates/fits/GeoWarp_$*.qs \
		--simulated-slices intermediates/predicted-slices-3d/$*.qs \
		--output $@

figures/simulation-conditional-single-3d/GeoWarp_%.pdf: \
	scripts/simulation-conditional-plot-single-3d.R \
	intermediates/fits/GeoWarp_%.qs \
  	intermediates/predicted-slices-3d/GeoWarp_%.qs
	Rscript $< \
		--fit intermediates/fits/GeoWarp_$*.qs \
		--simulated-slices intermediates/predicted-slices-3d/$*.qs \
		--output $@

figures/simulation-unconditional-3d-pdf/GeoWarp_%.pdf: \
	scripts/simulation-unconditional-plot-3d.R \
  	intermediates/simulated-slices-3d/GeoWarp_%.qs
	Rscript $< \
		--simulated-slices intermediates/simulated-slices-3d/$*.qs \
		--output $@

figures/simulation-unconditional-single-3d/GeoWarp_%.pdf: \
	scripts/simulation-unconditional-plot-single-3d.R \
	intermediates/fits/GeoWarp_%.qs \
  	intermediates/simulated-slices-3d/GeoWarp_%.qs
	Rscript $< \
		--fit intermediates/fits/GeoWarp_$*.qs \
		--simulated-slices intermediates/simulated-slices-3d/$*.qs \
		--output $@

figures/prediction-2d-pdf/GeoWarp_%.pdf: \
	scripts/prediction-plot-2d.R \
  	intermediates/fits/GeoWarp_%.qs \
  	intermediates/predicted-grid-2d/GeoWarp_%.qs
	Rscript $< \
		--fit intermediates/fits/GeoWarp_$*.qs \
		--predicted-grid intermediates/predicted-grid-2d/GeoWarp_$*.qs \
		--output $@

figures/prediction-3d-pdf/GeoWarp_%-mean.pdf: \
	scripts/prediction-plot-3d.R \
	intermediates/fits/GeoWarp_%.qs \
  	intermediates/predicted-slices-3d/GeoWarp_%.qs
	Rscript $< \
		--fit intermediates/fits/GeoWarp_$*.qs \
		--variable log_q_c_mean \
		--predicted-slices intermediates/predicted-slices-3d/$*.qs \
		--output $@

figures/prediction-3d-pdf/GeoWarp_%-sd.pdf: \
	scripts/prediction-plot-3d.R \
	intermediates/fits/GeoWarp_%.qs \
  	intermediates/predicted-slices-3d/GeoWarp_%.qs
	Rscript $< \
		--fit intermediates/fits/GeoWarp_$*.qs \
		--variable log_q_c_sd \
		--predicted-slices intermediates/predicted-slices-3d/$*.qs \
		--output $@

figures/prediction-single-3d/GeoWarp_%.pdf: \
	scripts/prediction-plot-single-3d.R \
	intermediates/fits/GeoWarp_%.qs \
  	intermediates/predicted-single-3d/GeoWarp_%.qs
	Rscript $< \
		--fit intermediates/fits/GeoWarp_$*.qs \
		--predicted-single intermediates/predicted-single-3d/$*.qs \
		--output $@

figures/vertical-profile.pdf: \
	scripts/vertical-profile.R
	Rscript $< \
  		--output $@

figures/cv-metrics-plot.pdf: \
	scripts/cv-metrics-plot.R \
	intermediates/cv-metrics-all.qs
	Rscript $< \
		--cv-metrics intermediates/cv-metrics-all.qs \
  		--output $@

figures/cv-metrics-table.tex: \
	scripts/cv-metrics-table.R \
	intermediates/cv-metrics-all.qs
	Rscript $< \
		--cv-metrics intermediates/cv-metrics-all.qs \
	    --models \
			GeoWarp \
			Linear \
			Binned \
			BCS \
		--output $@

figures/cv-metrics-table-full.tex: \
	scripts/cv-metrics-table.R \
	intermediates/cv-metrics-all.qs
	Rscript $< \
		--cv-metrics intermediates/cv-metrics-all.qs \
		--models \
			GeoWarp \
			GW-CV \
			GW-Vert-CV \
			Linear \
			Binned \
			BCS \
		--output $@

figures/cv-predictions/%.pdf: \
	scripts/cv-prediction-plot.R \
	intermediates/cv-predictions/%.qs
	Rscript $< \
		--input intermediates/cv-predictions/$*.qs \
		--output $@

figures/summary-A2.pdf: \
	scripts/summary-A2.R
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

figures/nu-selection-summary.txt: \
	scripts/nu-selection-summary.R \
	$(DATASETS) \
	intermediates/nu-selection-fits.qs
	Rscript $< \
		--datasets $(DATA_A_FIELD) $(DATA_B_FIELD) \
		--nu-selection-fits intermediates/nu-selection-fits.qs \
  		--output $@

# Intermediates

intermediates/nu-selection-fits.qs: \
	scripts/nu-selection-fits.R \
	$(DATASETS)
	Rscript $< \
		--datasets $(DATA_A_FIELD) $(DATA_B_FIELD) \
		--output $@

intermediates/cv-metrics-all.qs: \
	scripts/cv-metrics-all.R \
	intermediates/cv-fit-metrics.qs \
	intermediates/binned-statistics-metrics.qs \
	data/cv-metrics-bcs.rds
	Rscript $< \
		--fit-metrics intermediates/cv-fit-metrics.qs \
		--binned-statistics-metrics intermediates/binned-statistics-metrics.qs \
		--bcs-metrics data/cv-metrics-bcs.rds \
		--output $@

intermediates/binned-statistics-metrics.qs: \
	scripts/binned-statistics-metrics.R
	Rscript $< \
		--data-directory $(CPT_DATA_DIR) \
		--output $@


intermediates/cv-fit-metrics.qs: \
	scripts/cv-fit-metrics.R \
	$(CV_PREDICTIONS)
	Rscript $< \
		--cv-predictions $(CV_PREDICTIONS) \
		--output $@

intermediates/simulated-slices-3d/GeoWarp_%.qs: \
	scripts/simulated-slices-3d.R \
	intermediates/fits/GeoWarp_%.qs
	Rscript $< \
  		--fit intermediates/fits/GeoWarp_$*.qs \
  		--output $@

intermediates/predicted-slices-3d/GeoWarp_%.qs: \
	scripts/predicted-slices-3d.R \
	intermediates/fits/GeoWarp_%.qs
	Rscript $< \
  		--fit intermediates/fits/GeoWarp_$*.qs \
  		--output $@

intermediates/predicted-single-3d/GeoWarp_%.qs: \
	scripts/predicted-single-3d.R \
	intermediates/fits/GeoWarp_%.qs
	Rscript $< \
  		--fit intermediates/fits/GeoWarp_$*.qs \
  		--output $@

intermediates/predicted-grid-2d/GeoWarp_%.qs: \
	scripts/predicted-grid-2d.R \
	intermediates/fits/GeoWarp_%.qs
	Rscript $< \
  		--fit intermediates/fits/GeoWarp_$*.qs \
  		--output $@

intermediates/cv-predictions/%.qs: \
	scripts/cv-predictions.R \
	intermediates/cv-fits/%.qs
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--input $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--fits intermediates/cv-fits/$*.qs \
		--output $@

## Fits

intermediates/cv-fits/%.qs: \
	scripts/cv-fit.R
	$(eval MODEL_NAME := $(word 1,$(subst _, ,$*)))
	$(eval DATASET_NAME := $(word 2,$(subst _, ,$*)))
	Rscript $< \
		--input $(CPT_DATA_DIR)/$(DATASET_NAME).csv \
		--model $(MODEL_NAME) \
		--log-file-pattern logs/cv-fits/$*_{group}.txt \
		--output $@

intermediates/fits/GeoWarp_%.qs: \
	scripts/fit.R \
	$(CPT_DATA_DIR)/%.csv
	Rscript $< \
		--input $(CPT_DATA_DIR)/$*.csv \
		--model GeoWarp \
		--log-file logs/fits/GeoWarp_$*.txt \
		--output $@
