# Data and R code scripts to conduct a "Difference-in-Difference Analysis of Childhood Immunization Coverage Before and During the COVID-19 Pandemic"

Sooyoung Kim (sk9076@nyu.edu)

This repository contains the R code scripts and the data that was used in the manuscript __The Role of Universal Healthcare Coverage in Building Resilience Against Public Health Crises: A Case Study of the COVID-19 Pandemic and Childhood Immunization Coverage__ for reproducible results.

* Scripts are numbered in the order that should be run as some code chunks are dependent on the objects and functions that were created in the prior scripts. For example, if you do not source "0_functions.R" before running other scripts, they may generate error messages.
* The first script (1_merge and clean data.R) compiles data from different sources into one merged data file. This will be used througout the scripts. If you want to skip this process, you can run the code below to load the pre-cleaned/merged dataset in the data folder.

```{r, eval=F}
#dat_merged <- readRDS(here::here("data", "data_merged.rds"))
```
* Associated manuscript is currently under review. 