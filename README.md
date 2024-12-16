# BLP-with-R
A stylized BLP framework with R

## intro

This project originates from the EIO (3254) problem set at **SUFE**. Specifically, the task involves estimating a random coefficient demand model for the beer industry. The model is highly simplified, featuring only one linear parameter (price elasticity) and two nonlinear parameters (income demographics and structural error).

## repositories

- `demand_exercises.pdf`: problem set
- `WBER.xlsx`: A subsample of aggregated-level data from the beer industry
- `beer.rdata`: R-data, from `logit_main.r`
- `logit_main.r`: code for nested-logit
- `BLP_main.r`: code for random-coefficient model (with BLP contract mapping)
- `EIO_logit_BLP.pdf`: a report summary
- `out/`: output folder

## feature

Although the R-based BLP model package [BLPestimatoR](https://cran.r-project.org/web/packages/BLPestimatoR/vignettes/blp_intro.html) is available, building a BLP model from scratch through a tutorial remains meaningful from the perspective of extensibility.

The algorithm is highly efficient, fully leveraging **vectorized computation** and compatible with data.table for accelerated processing. Any practical application can be extended by simply adding parameter variables.

Anyway, it is just a course exercise after all.
