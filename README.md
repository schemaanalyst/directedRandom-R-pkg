# Directed Random R Language Package

This repository contains an R package for "Directed Random" research paper. The `devtools` development framework was used to implement this R package. The content of this repository are the following:

* R functions that read data sets, preform data manipulations, statistically analyse the results, and generate tables and plots.
* Data sets of our experiment.
* Unit tests to mitigate the risk of our results.

## Getting Started

The following instructions will get you a copy of the project up and running on your local R environment. This method is ideal if you plan to leverage, without modification, our existing functions and data sets.

### Prerequisites

Please note that these instructions have been tested on an Ubuntu 16.04 LTS workstation running the following version of R:

```shell
R version 3.3.3 (2017-03-06) -- "Another Canoe"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)
```

You would need `devtools` to install our package:

* [devtools](https://github.com/hadley/devtools) - Tools to make an R developer's life easier

### Installing

You can type the next command in your R development environment if you want to want to install this package:

```shell
devtools::install_github("schemaanalyst/directedRandom-R-pkg")
```

### Example

Within your R environment, use the following commands:

* Reading data sets into dataframes:

```shell
mutants <- directedRandomR::read_mutants()
analysis <- directedRandomR::read_analysis()
```

* Generating Tables (Latex or dataframes):

```shell
x1 <- directedRandomR::table_generator_timing(analysis, rtrn = "data", m = "mean")
x2 <- directedRandomR::table_generator_coverage(analysis, rtrn = "data", m = "mean")
x3 <- directedRandomR::table_generator_mutation_score(mutants, rtrn = "data", m = "mean")
x4 <- directedRandomR::table_generator_mutant_operators(mutants, rtrn = "data", m = "mean")
```

NOTE: The `rtrn` parameter is used to return either a dataframe (`rtrn = "data"`) or a latex table (`rtrn = "tex"`). The `m` parameter is used to print the table as `median` or `mean` results.

* For more information about our functions please type this:

```shell
library(directedRandomR)
?read_mutants
?read_analysis()
?table_generator_timing()
?table_generator_coverage()
?table_generator_mutation_score()
?table_generator_mutant_operators()
```

## Set-up for development

If you are interested in extending this package with new data sets and your own functions, then you can run the
following command to first clone this repository:

```shell
git clone https://github.com/schemaanalyst/directedRandom-R-pkg.git
```

Furthermore, in your R environment, you can run each of the following commands. This will help you to build, install, load, and test our R packages using `devtools`:

```shell
devtools::document()
devtools::install()
devtools::load_all()
devtools::test()
```

### Running the tests

Whilst running our tests, the following results will appear:

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
table-generator: ........
testgenerationtiming-sample-size: ....
testgenerationtiming-median-results: ....
testgenerationtiming-u-test-results: ...
testgenerationtiming-effect-size-results: ...

DONE =========================================
```

### Coding style tests

The conventions we have used are explained in the following URLs:

* [R Packages](http://r-pkgs.had.co.nz/) -  A site for “R packages” development. It was published with O’Reilly in April 2015.
* [README template](https://gist.github.com/PurpleBooth/109311bb0361f32d87a2).

## Built With

* [devtools](https://github.com/hadley/devtools) - Tools to make an R developer's life easier
* [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) - In-source documentation for R
* Check The DESCRIPTION file - To review all the dependencies


## Authors

* **Abdullah Alsharif** - *Initial work* - [aalshrif90](https://github.com/aalshrif90)
* **Gregory M. Kapfhammer** - *Quality Assurance* - [gkapfham](https://github.com/gkapfham)
* **Phil McMinn** - *Quality Assurance* - [philmcminn](https://github.com/philmcminn)
