library(dplyr)
#' FUNCTION: preform_averaging_of_samples
#'

#' average out all schemas for each data generators and DBMS for each run
#' @importFrom magrittr %>%
#' @export
preform_averaging_of_samples <- function(d) {
  dbmss <- as.vector(dplyr::distinct(d, dbms))[[1]]
  dgens <- as.vector(dplyr::distinct(d, datagenerator))[[1]]
  mean_results <- NULL
  for (seed in 1:30) {
    data <- d %>% dplyr::filter(randomseed == seed)
    for (db in dbmss) {
      for (gen in dgens) {
        results <- data %>% dplyr::filter(dbms == db) %>% dplyr::filter(datagenerator == gen)
        results$mutationscore = ((results$scorenumerator/results$scoredenominator) * 100)

        results <- results %>% dplyr::group_by(datagenerator, randomseed, dbms)  %>% dplyr::summarise(coverage = mean(coverage),testgenerationtime = mean(testgenerationtime), mutationscore = mean(mutationscore), evaluations = mean(evaluations))
        mean_results <- rbind(mean_results, results)

        #print(results)
      }
    }
  }

  #print(mean_results)
  return(mean_results)
}

#' FUNCTION: analyse_vargha_delaney_effect_size_specify
#'

#' Perform an effect size analysis of the timings for the AVM and Directed Random mutation analysis techniques.
#' @importFrom magrittr %>%
#' @export
analyse_vargha_delaney_effect_size_specify <- function(d, e) {
  # Mutation score
  #d$mutationscore = ((d$scorenumerator/d$scoredenominator) * 100)
  dbmss <- as.vector(d %>% dplyr::group_by(dbms)  %>% dplyr::distinct(dbms))[[1]]
  #dgens <- as.vector(dplyr::distinct(d, datagenerator))[[1]]

  mean_results <- NULL
  #for (seed in 1:30) {
    for (db in dbmss) {
        d_dbms <- d %>% dplyr::filter(dbms %in% c(db))

        avm <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("avsDefaults"))# %>% dplyr::filter(randomseed == seed)
        diR <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("directedRandom"))# %>% dplyr::filter(randomseed == seed)
        ran <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("random"))# %>% dplyr::filter(randomseed == seed)

        model <- e(diR$testgenerationtime, avm$testgenerationtime)
        results <-as.data.frame(model)
        namevector <- c("dbms")
        results[,namevector] <- db
        namevector <- c("vs")
        results[,namevector] <- "Directed Random vs AVM Test Generation Time"
        #namevector <- c("randomseed")
        #results[,namevector] <- seed
        mean_results <- rbind(mean_results, results)

        # model <- e(diR$testgenerationtime, ran$testgenerationtime)
        # results <-as.data.frame(model)
        # namevector <- c("dbms")
        # results[,namevector] <- db
        # namevector <- c("vs")
        # results[,namevector] <- "Directed Random vs Random Test Generation Time"
        # namevector <- c("randomseed")
        # results[,namevector] <- seed
        # mean_results <- rbind(mean_results, results)
    }
  #}

  return(mean_results)
}

#' FUNCTION: analyse_vargha_delaney_effect_size_specify
#'

#' Perform an effect size analysis of the timings for the AVM and Directed Random mutation analysis techniques.
#' @importFrom magrittr %>%
#' @export
analyse_vargha_delaney_effect_size_specify_no_time <- function(d, e) {
  # Mutation score
  d$mutationscore = ((d$scorenumerator/d$scoredenominator) * 100)
  dbmss <- as.vector(d %>% dplyr::group_by(dbms)  %>% dplyr::distinct(dbms))[[1]]
  #dgens <- as.vector(dplyr::distinct(d, datagenerator))[[1]]

  mean_results <- NULL
  #for (seed in 1:30) {
  for (db in dbmss) {
    d_dbms <- d %>% dplyr::filter(dbms %in% c(db))

    avm <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("avsDefaults"))# %>% dplyr::filter(randomseed == seed)
    diR <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("directedRandom"))# %>% dplyr::filter(randomseed == seed)
    ran <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("random"))# %>% dplyr::filter(randomseed == seed)

    model <- e(diR$mutationscore, avm$mutationscore)
    results <-as.data.frame(model)
    namevector <- c("dbms")
    results[,namevector] <- db
    namevector <- c("vs")
    results[,namevector] <- "Directed Random vs AVM Mutation Score"
    #namevector <- c("randomseed")
    #results[,namevector] <- seed
    mean_results <- rbind(mean_results, results)

    model <- e(diR$evaluations, avm$evaluations)
    results <-as.data.frame(model)
    namevector <- c("dbms")
    results[,namevector] <- db
    namevector <- c("vs")
    results[,namevector] <- "Directed Random vs AVM Number of Evaluations"
    #namevector <- c("randomseed")
    #results[,namevector] <- seed
    mean_results <- rbind(mean_results, results)

    model <- e(diR$coverage, avm$coverage)
    results <-as.data.frame(model)
    namevector <- c("dbms")
    results[,namevector] <- db
    namevector <- c("vs")
    results[,namevector] <- "Directed Random vs AVM Coverage"
    #namevector <- c("randomseed")
    #results[,namevector] <- seed
    mean_results <- rbind(mean_results, results)

    # model <- e(diR$testgenerationtime, ran$testgenerationtime)
    # results <-as.data.frame(model)
    # namevector <- c("dbms")
    # results[,namevector] <- db
    # namevector <- c("vs")
    # results[,namevector] <- "Directed Random vs Random Test Generation Time"
    # namevector <- c("randomseed")
    # results[,namevector] <- seed
    # mean_results <- rbind(mean_results, results)
  }
  #}

  return(mean_results)
}

#' FUNCTION: analyse_vargha_delaney_effect_size_thresholding
#'

#' Perform an effect size analysis of the timings for the AVM and DR mutation analysis techniques.
#' By default, specify that the calculation should use the effectsize_accurate function for calculating the statistic.
#' But, investigate the effect size with different levels of thresholding, to establish sensitivity.
#' @export

analyse_vargha_delaney_effect_size_thresholding <- function(d) {
  tobereturned <- NULL
  d <- preform_averaging_of_samples(d)
  for(threshold in seq(0, 1000, by=100)) {
    dt <-transform_execution_times_for_threshold(d, threshold)
    #print(threshold)
    a <- analyse_vargha_delaney_effect_size_specify(dt, effectsize_accurate)
    namevector <- c("threshold")
    a[,namevector] <- threshold
    tobereturned <- rbind(tobereturned, a)
  }

  return(tobereturned)
}

#' FUNCTION: analyse_vargha_delaney_effect_size
#'

#' Perform an effect size analysis of the timings for the AVM and DR mutation analysis techniques.
#' By default, specify that the calculation should use the effectsize_accurate function for calculating the statistic.
#' @export

analyse_vargha_delaney_effect_size <- function(d) {
  #d <- preform_averaging_of_samples(d)
  analyse_vargha_delaney_effect_size_specify_no_time(d, effectsize_accurate)
}



#' FUNCTION: analyse_wilcox_rank_sum_test
#'

#' Perform a statistical analysis of the timings for the AVM and directed random mutation analysis techniques. Right now, this method
#' simply returns textual output; it would be much better if it actually returned a single data frame.
#' @importFrom magrittr %>%
#' @export

analyse_wilcox_rank_sum_test <- function(d) {
  # Average All
  #d <- preform_averaging_of_samples(d)
  #a <- d %>% dplyr::group_by(dbms)  %>% dplyr::distinct(dbms)
  dbmss <- as.vector(d %>% dplyr::group_by(dbms)  %>% dplyr::distinct(dbms))[[1]]
  #ranseed <- as.vector(dplyr::distinct(d, randomseed))[[1]]

  #dgens <- as.vector(dplyr::distinct(d, datagenerator))[[1]]
  #print(dbmss)
  mean_results <- NULL
  #for (seed in 1:30) {
    for (db in dbmss) {
      #d_dbms <- d %>% dplyr::filter(dbms %in% c(db))
      # extract the relevant data values
      avm <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("avsDefaults"))# %>% dplyr::filter(randomseed == seed)
      diR <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("directedRandom"))# %>% dplyr::filter(randomseed == seed)
      ran <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("random"))# %>% dplyr::filter(randomseed == seed)

      # Test generation time
      avm_time <- preform_averaging_of_samples(d) %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("avsDefaults"))# %>% dplyr::filter(randomseed == seed)
      diR_time <- preform_averaging_of_samples(d) %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("directedRandom"))# %>% dplyr::filter(randomseed == seed)
      ran_time <- preform_averaging_of_samples(d) %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("random"))# %>% dplyr::filter(randomseed == seed)
      results <- analyse_perform_wilcox_rank_sum_test(avm_time, diR_time)
      namevector <- c("dbms")
      results[,namevector] <- db
      namevector <- c("vs")
      results[,namevector] <- "AVM vs Directed Random Test Generation Time"
      #namevector <- c("randomseed")
      #results[,namevector] <- seed
      mean_results <- rbind(mean_results, results)

      # results <- analyse_perform_wilcox_rank_sum_test(ran, diR)
      # namevector <- c("dbms")
      # results[,namevector] <- db
      # namevector <- c("vs")
      # results[,namevector] <- "Random vs Directed Random Test Generation Time"
      # #namevector <- c("randomseed")
      # #results[,namevector] <- seed
      # mean_results <- rbind(mean_results, results)
      #

      # Mutation score
      results <- analyse_perform_wilcox_rank_sum_test_mutationScore(avm, diR)
      namevector <- c("dbms")
      results[,namevector] <- db
      namevector <- c("vs")
      results[,namevector] <- "AVM vs Directed Random Mutation Score"
      #namevector <- c("randomseed")
      #results[,namevector] <- seed
      mean_results <- rbind(mean_results, results)

      # results <- analyse_perform_wilcox_rank_sum_test_mutationScore(ran, diR)
      # namevector <- c("dbms")
      # results[,namevector] <- db
      # namevector <- c("vs")
      # results[,namevector] <- "Random vs Directed Random Mutation Score"
      # #namevector <- c("randomseed")
      # #results[,namevector] <- seed
      # mean_results <- rbind(mean_results, results)

      # Evaluations
      results <- analyse_perform_wilcox_rank_sum_test_evaluations(avm, diR)
      namevector <- c("dbms")
      results[,namevector] <- db
      namevector <- c("vs")
      results[,namevector] <- "AVM vs Directed Random Number Of evaluations"
      #namevector <- c("randomseed")
      #results[,namevector] <- seed
      mean_results <- rbind(mean_results, results)

      # results <- analyse_perform_wilcox_rank_sum_test_evaluations(ran, diR)
      # namevector <- c("dbms")
      # results[,namevector] <- db
      # namevector <- c("vs")
      # results[,namevector] <- "Random vs Directed Random Number Of evaluations"
      # #namevector <- c("randomseed")
      # #results[,namevector] <- seed
      # mean_results <- rbind(mean_results, results)

      # Coverage
      results <- analyse_perform_wilcox_rank_sum_test_coverage(avm, diR)
      namevector <- c("dbms")
      results[,namevector] <- db
      namevector <- c("vs")
      results[,namevector] <- "AVM vs Directed Random Coverage"
      #namevector <- c("randomseed")
      #results[,namevector] <- seed
      mean_results <- rbind(mean_results, results)

      # results <- analyse_perform_wilcox_rank_sum_test_coverage(ran, diR)
      # namevector <- c("dbms")
      # results[,namevector] <- db
      # namevector <- c("vs")
      # results[,namevector] <- "Random vs Directed Random Coverage"
      # #namevector <- c("randomseed")
      # #results[,namevector] <- seed
      # mean_results <- rbind(mean_results, results)

    }
  #}
  return(mean_results)
}


#' FUNCTION: perform_wilcox_rank_sum_test
#'

#' Perform a single Wilcoxon rank-sum test for two input data sets.
#' @importFrom magrittr %>%
#' @export

analyse_perform_wilcox_rank_sum_test <- function(s,v) {
  # perform the statistical analysis and return a "tidy" version of the model
  model <- wilcox.test(v$testgenerationtime, s$testgenerationtime)
  tidy_model <- model %>% broom::tidy()
  return(tidy_model)
}


#' FUNCTION: analyse_perform_wilcox_rank_sum_test_mutationScore
#'

#' Perform a single Wilcoxon rank-sum test for two input data sets for mutation score
#' @importFrom magrittr %>%
#' @export

analyse_perform_wilcox_rank_sum_test_mutationScore <- function(s,v) {
  # perform the statistical analysis and return a "tidy" version of the model
  s$mutationscore = ((s$scorenumerator/s$scoredenominator) * 100)
  v$mutationscore = ((v$scorenumerator/v$scoredenominator) * 100)
  #a <- v %>% dplyr::summarise(mutationscore = mean(mutationscore))
  #b <- s %>% dplyr::summarise(mutationscore = mean(mutationscore))
  model <- wilcox.test(s$mutationscore, v$mutationscore)
  tidy_model <- model %>% broom::tidy()
  return(tidy_model)
}


#' FUNCTION: analyse_perform_wilcox_rank_sum_test_evaluations
#'

#' Perform a single Wilcoxon rank-sum test for two input data sets for evaluations
#' @importFrom magrittr %>%
#' @export

analyse_perform_wilcox_rank_sum_test_evaluations <- function(s,v) {
  # perform the statistical analysis and return a "tidy" version of the model
  model <- wilcox.test(s$evaluations, v$evaluations)
  tidy_model <- model %>% broom::tidy()
  return(tidy_model)
}

#' FUNCTION: analyse_perform_wilcox_rank_sum_test_coverage
#'

#' Perform a single Wilcoxon rank-sum test for two input data sets for coverage
#' @importFrom magrittr %>%
#' @export

analyse_perform_wilcox_rank_sum_test_coverage <- function(s,v) {
  # perform the statistical analysis and return a "tidy" version of the model
  model <- wilcox.test(s$coverage, v$coverage)
  tidy_model <- model %>% broom::tidy()
  return(tidy_model)
}


