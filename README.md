# GeoWarp: Warped spatial processes for inferring subsea sediment properties

The code applies the GeoWarp modelling framework to CPT data. The data included in this repository is synthetic, built to match the properties of the real data in the paper.

## Installation

You need a reasonably up to date R environment (the software was developed with R 4.2). Then, install the following dependencies:

```r
install.packages(c(
  'FNN', 'GpGp', 'argparse', 'colorspace', 'devtools', 'dplyr', 'ggplot2',
  'ggrepel', 'gtable', 'knitr', 'logger', 'matrixStats', 'patchwork', 'plot3D',
  'qs', 'readr', 'scales', 'scoringRules', 'sparseinv', 'stringr', 'withr'
))
```

Finally, install the `geowarp` R package:

```r
devtools::install('external/geowarp')
```

For the latter, you will need a working compiler toolchain.

## Running the code

The full workflow can be run from the command line using `make`.

```bash
GEOWARP_GROUP_CORES=4 GEOWARP_THREADS=2 make -j4 all
```

This will run the entire workflow, including model fitting, cross validation, and plotting. Figures will go into the `figures` directory. The variables `GEOWARP_GROUP_CORES` and `GEOWARP_THREADS` control the number of cores used in different parts of the code, while the `-j` flag to `make` controls the number of parallel processes used by make. The maximum number of cores and threads used will be the product of these variables, so they should be tailored to the number of cores on your machine.

Different steps of the workflow benefit from different values for these variables. In particular, the cross-validation code, which must run many fits to data, benefits from using fewer threads but more cores. You may therefore wish to run the steps that exclude cross-validation like so:

```bash
GEOWARP_THREADS=8 make -j4 all_except_cv
```

and later run the cross-validation study using

```bash
GEOWARP_GROUP_CORES=4 GEOWARP_THREADS=2 make -j4 cv
```

where again the above defaults should be changed to match your system.

Three environment variables can be set to control the workflow:
- `GEOWARP_LOG_LEVEL`: One of `trace`, `debug`, `info`, `warn`, or `error`. Sets the level of logging; the default is `info`, which is pretty quiet. The highest level is `trace`.
- `GEOWARP_THREADS`: Sets the number of threads used in numerical routines during model fitting and prediction.
- `GEOWARP_GROUP_CORES`: In the cross-validation study, for each model and site, we refit the model with each individual horizontal location withheld. This variable controls how many locations will be fit in parallel.

## Getting the real data

Please contact the study authors to discuss access to the real data, which are not included in this repository due to commercial confidentiality.
