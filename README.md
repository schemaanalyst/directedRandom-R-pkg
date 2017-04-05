# README #

An R package for Directed Random

## Installation Instructions

Please note that these instructions have been tested on an Ubuntu 16.04 LTS workstation running the following version of R:

```shell
R version 3.3.3 (2017-03-06) -- "Another Canoe"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)
```

You can type the next command in your R development environment if you want to want to install and then use this R
package. This method is ideal if you plan to leverage, without modification, our existing functions and data sets in
your own work. 

```shell
devtools::install_github("schemaanalyst/directedRandom-R-pkg")
```

To Use the data within the package and Knit the files you have to clone this repo

```shell
mkdir data
cd data
git clone https://github.com/schemaanalyst/directedRandom-R-pkg.git
cd directedRandom-R-pkg
```
NOTE: use the R shell after those then go to the commands in sction Example Run

If you are interested in extending this package with new data sets and your own functions, then you can run the
following command to first clone this repository:

```shell
git clone https://github.com/schemaanalyst/directedRandom-R-pkg.git
```

### What is this repository for? ###

* R/tables_generator.R is a file that have functions that generates the latex tables in Directed Random paper.
* R/effecsize.R is what is used in virtual mutation paper.
* R/collect.R has two functions that collect the data from the data folder. mutanttiming and mutationanalysistiming.
* tests directory tests all the tables generated.
* data directory (and this must be the working directory for R scripts to collect data)

Major functions used in the paper is:

* R/tables_generator.R `siginificant_timing(analysis, rtrn = "data")`
* R/tables_generator.R `siginificant_coverage(analysis, rtrn = "data")`
* R/tables_generator.R `siginificant_mutation_score(mutanttiming, rtrn = "data")`
* R/tables_generator.R `siginificant_mutant_operators_fixed(mutanttiming, rtrn = "data")`

## Example Run
All in the R shell

* Collect Data

```shell
mutants <- directedRandomR::collect_mutanttiming()
analysis <- directedRandomR::collect_mutationanalysistime()
```
* Generate Tables (Latex or dataframes) change `rtrn = "data"` for data frame or `rtrn = "tex"` for latex table

```shell
x1 <- directedRandomR::siginificant_timing(analysis, rtrn = "data")
x2 <- directedRandomR::siginificant_coverage(analysis, rtrn = "data")
x3 <- directedRandomR::siginificant_mutation_score(mutanttiming, rtrn = "data")
x4 <- directedRandomR::siginificant_mutant_operators_fixed(mutanttiming, rtrn = "data")
```

## Tests

In an R shell you can run each of the following commands to build and test the R packages using `devtools`:

```shell
devtools::document()
devtools::install()
devtools::load_all()
devtools::test()
```

## Building Rmarkdown and Sweave (latex)

All done in terminal command NOT in the R shell

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
