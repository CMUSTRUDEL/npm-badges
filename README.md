# npm-badges

Appendices, data sets, and R scripts for:

```
"Adding Sparkle to Social Coding: An Empirical Study of Repository Badges in the npm Ecosystem."
Asher Trockman, Shurui Zhou, Christian Kästner, and Bogdan Vasilescu.
In Proceedings of the International Conference on Software Engineering, ICSE, ACM (2018).

```

## R scripts

You can run the R scripts on the csv files in the repo to estimate the different regression 
models we report on in the paper.

### Before you run

Make sure you have R installed. We tested this on R <version>

Dependencies ...

### Run using make

You can run `make` from the root folder to generate `appendix.pdf`.

### Run from R

You can also run `make.R` directly from R. 
If you want to rebuild the appendix (e.g., after you change something) and keep your R session 
open, this is faster than running `make` from the terminal, since the data is already loaded
into memory.

