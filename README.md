# npm-badges

![Signals Mostly Reliable](https://img.shields.io/badge/signals-mostly%20reliable-yellowgreen.svg)

Appendices, data sets, and R scripts for:

```
"Adding Sparkle to Social Coding: An Empirical Study of Repository Badges in the npm Ecosystem."
Asher Trockman, Shurui Zhou, Christian Kästner, and Bogdan Vasilescu.
In Proceedings of the International Conference on Software Engineering, ICSE, ACM (2018).

```

## R scripts

You can run the R scripts on the CSV files in the repository to estimate the different regression models we report on in the paper.

### Before you run

Make sure you have R installed. We tested this on R 3.3.2.

We require the following dependencies: `RMySQL, lme4, lmerTest, MuMIn, car, MASS, effects, effsize, sqldf, MASS, pROC, plyr, beanplot, RColorBrewer`.

It may be necessary to change the Pandoc environment variable in `make.R`: the required path can be found by running `Sys.getenv("RSTUDIO_PANDOC")` in RStudio. If you are using OS X with an existing RStudio installation, this step is probably not neeeded.

### Run using make

You can run `make` from the root folder to generate `appendix.pdf`.

### Run from R

You can also run `make.R` directly from R by setting the working directory to this repository with `setwd('/path')` and running `source('make.R')`.

If you want to change something and rebuild the appendix, it is most efficient to run `source('make.R')` from an existing R session. The script will detect that the data has already been loaded into memory and processed, preventing this time-consuming step from happening again.

## Notes

Some data sets have been truncated to to meet GitHub's file size limit.
* `data/timeseries_freshness.csv`: reduced from 3,294,725 to 47,400 rows, since we only considered projects with non-zero freshness scores in our longitudinal analysis.
* `data/master_info.csv`: removed unnecessary columns such as license and author information, which were not used in our study.
