# Overview

Each CSV file in this directory contains synthetic data simulated from the GeoWarp model at the same locations as the real data analysed in the paper.

# Data format

All files share the following fields:

- `name`: A unique name for the CPT; all rows with the same name will have the same horizontal location, and were measured at the same time.
- `short_name`: A shorter version of the above name.
- `depth`: The depth of the measurement in metres.
- `log_q_c`: The log of the `q_c` variable, which is the cone tip pressure measured in megapascals

The six files A1.csv through A3.csv and B1.csv through B3.csv contain the following horizontal coordinate variables:

- `easting`: The easting of the measurement in metres (relative to the minimum easting in the site).
- `northing`: The northing of the measurement in metres (relative to the minimum northing in the site).

The two files A2-T.csv and B2-T.csv, in which data lie along a transect, instead contain the following coordinate:

- `horizontal`: The distance along the transect of the measurement.
