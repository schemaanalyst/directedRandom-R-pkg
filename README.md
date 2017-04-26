# README #

An R package for Directed Random

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

## Example Run
In your run the R environment and use the following commands:

* Collecting the data into a dataframe:

```shell
mutants <- directedRandomR::collect_mutanttiming()
analysis <- directedRandomR::collect_mutationanalysistime()
```
* Generate Tables (Latex or dataframes), use the `rtrn` parameter to change `rtrn = "data"` for data frame or `rtrn = "tex"` for latex table. And the `m` parameter to print the table using `median` or `mean`:

```shell
x1 <- directedRandomR::siginificant_timing(analysis, rtrn = "data", m = "mean")
x2 <- directedRandomR::siginificant_coverage(analysis, rtrn = "data", m = "mean")
x3 <- directedRandomR::siginificant_mutation_score(mutants, rtrn = "data", m = "mean")
x4 <- directedRandomR::siginificant_mutant_operators(mutants, rtrn = "data", m = "mean")
```


### What is this repository for? ###

* R/tables_generator.R: is used to generate latex tables in Directed Random paper.
* R/effecsize.R  used for effect size (A12) calculations.
* R/collect.R has two functions that collect the data from the data folder. mutanttiming and mutationanalysistiming.
* tests directory used tests all the tables generated.

Major functions used in the paper is:

* R/tables_generator.R `siginificant_timing(analysis, rtrn = "data", m = "median")`
* R/tables_generator.R `siginificant_coverage(analysis, rtrn = "data", m = "median")`
* R/tables_generator.R `siginificant_mutation_score(mutanttiming, rtrn = "data", m = "median")`
* R/tables_generator.R `siginificant_mutant_operators_fixed(mutanttiming, rtrn = "data", m = "median")`


## Tests

In an R environment you can run each of the following commands to build and test our R packages using `devtools`:

```shell
devtools::document()
devtools::install()
devtools::load_all()
devtools::test()
```

## Building Rmarkdown and Sweave (latex)

To build the RMarkDown and Sweave you would need the `pandoc` and `LateX` installed in your current OS.

The following commands are done in OS terminal command NOT in the R shell:

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
