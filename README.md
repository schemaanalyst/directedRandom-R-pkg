# README #

This repository contains the R package for "Directed Random" research paper. This R packge was implmented using `devtools` developement framework. The content of this repository are the following:

* R functions that read the data sets, preform data manipulations, statistically analyze the results, and generates tables and plots of the results.
* Data sets of our experiment.
* Unit tests to mitigate the risk of our results.

## Installation Instructions

Please note that these instructions have been tested on an Ubuntu 16.04 LTS workstation running the following version of R:

```shell
R version 3.3.3 (2017-03-06) -- "Another Canoe"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)
```

You can type the next command in your R development environment if you want to want to install the package using `devtools`. This method is ideal if you plan to leverage, without modification, our existing functions and data sets in
your own work. 

```shell
devtools::install_github("schemaanalyst/directedRandom-R-pkg")
```

## Example Run
In your R environment use the following commands:

* Collecting the data sets into dataframes:

```shell
mutants <- directedRandomR::collect_mutanttiming()
analysis <- directedRandomR::collect_mutationanalysistime()
```
* Generate Tables (Latex or dataframes):

```shell
x1 <- directedRandomR::table_generator_timing(analysis, rtrn = "data", m = "mean")
x2 <- directedRandomR::table_generator_coverage(analysis, rtrn = "data", m = "mean")
x3 <- directedRandomR::table_generator_mutation_score(mutants, rtrn = "data", m = "mean")
x4 <- directedRandomR::table_generator_mutant_operators(mutants, rtrn = "data", m = "mean")
```

NOTE: The `rtrn` parameter is used to return either a dataframe (`rtrn = "data"`) or a latex table (`rtrn = "tex"`). The `m` parameter is used to print the table as `median` or `mean` results.


### R Scripts

* R/tables_generator.R: is used to generate latex tables in Directed Random paper, and it includes the use of statistical analyze.
* R/effecsize.R  used for effect size (A12) calculations.
* R/collect.R has two functions that collect the data from the data folder. mutanttiming and mutationanalysistiming.
* tests directory used tests all the tables generated.

Major functions used in the paper is:

* R/tables_generator.R `table_generator_timing(analysis, rtrn = "data", m = "median")`
* R/tables_generator.R `table_generator_coverage(analysis, rtrn = "data", m = "median")`
* R/tables_generator.R `table_generator_mutation_score(mutanttiming, rtrn = "data", m = "median")`
* R/tables_generator.R `table_generator_mutant_operators_fixed(mutanttiming, rtrn = "data", m = "median")`


## Developement

<!--
To use the data with our package you have to clone the **data** repo:

```shell
mkdir data
cd data
git clone https://github.com/schemaanalyst/directedRandom-data.git
cd directedRandom-data
```
NOTE: The data files include all data, plus RMarkDown, Sweave
-->
If you are interested in extending this package with new data sets and your own functions, then you can run the
following command to first clone this repository:

```shell
git clone https://github.com/schemaanalyst/directedRandom-R-pkg.git
```

In an R environment you can run each of the following commands to build and test our R packages using `devtools`:

```shell
devtools::document()
devtools::install()
devtools::load_all()
devtools::test()
```

### Test Results

Whilst running tests the following results will appear:

```shell
coverages-sample-size: ....
coverages-median-resutlts: ....
coverages-effect-size: ...
coverages-u-test: ...
mutant-operators-sample-size: ........
mutant-operators-mean-results: ............
mutant-operators-u-test-results: ...........
mutant-operators-effect-size-results: ...........
mutant-scores-mean-results: ....
mutant-scores-sample-size: ....
mutant-scores-u-test-results: ...
mutant-scores-effect-size-results: ...
testgenerationtiming-sample-size: ....
testgenerationtiming-median-results: ....
testgenerationtiming-u-test-results: ...
testgenerationtiming-effect-size-results: ...

DONE =========================================
```

## Generating Plots, RMarkDown, and Sweave

To build the RMarkDown and Sweave to review our results, you would need the `pandoc` and `LateX` installed in your current OS.

Also you would need the templates in the following repo:

```shell
git clone https://github.com/schemaanalyst/directedRandom-data
```

The following commands are done in OS terminal command, not in the R shell, and within the above cloned repo directory:

### Rmarkdown

```shell
Rscript -e "rmarkdown::render('RmarkDownFigures.Rmd')"
```

### Sweave (latex) for tables

```shell
R CMD Sweave tables.Rnw
```

Then

```shell
pdflatex tables.tex
```
