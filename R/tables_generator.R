#' FUNCTION: generate_15_schmea_mutation_analysis
#'

#' gets a 15 schemas from the dataset of mutantionanalysistime file.
#' @importFrom magrittr %>%
#' @export
generate_15_schmea_mutation_analysis <- function(d) {
  library(kimisc)
  library(dplyr)
  set.seed(2)
  #browser()
  #cases <- distinct(d, casestudy)

  #a <- sample.rows(cases %>% filter(casestudy != "iTrust", casestudy != "Products"), 13)
  c <- c("ArtistTerm", "BookTown", "BrowserCookies", "Cloc", "DellStore", "Flights", "Inventory", "UnixUsage", "JWhoisServer", "MozillaExtensions", "NistXTS749", "RiskIt", "StudentResidence","Products", "iTrust")

  d_rest <- d %>% filter(casestudy %in% c)
  #d_itrust <- d %>% filter(casestudy == "iTrust")
  #d_products <- d %>% filter(casestudy == "Products")

  #d <- rbind(d_rest, d_products, d_itrust)
  d <- d_rest
  return(d)
}

#' FUNCTION: generate_15_schmea_mutanttiming
#'

#' gets a 15 schemas from the dataset of mutanttiming file.
#' @importFrom magrittr %>%
#' @export
generate_15_schmea_mutanttiming <- function(d) {
  library(kimisc)
  library(dplyr)
  set.seed(1)
  #browser()
  #cases <- distinct(d, schema)
  #a <- sample.rows(cases %>% filter(schema != "iTrust", schema != "Products"), 13)
  c <- c("ArtistTerm", "BookTown", "BrowserCookies", "Cloc", "DellStore", "Flights", "Inventory", "UnixUsage", "JWhoisServer", "MozillaExtensions", "NistXTS749", "RiskIt", "StudentResidence","Products", "iTrust")
  d_rest <- d %>% filter(schema %in% c)
  #d_itrust <- d %>% filter(schema == "iTrust")
  #d_products <- d %>% filter(schema == "Products")

  #d <- rbind(d_rest, d_products, d_itrust)
  d <- d_rest
  return(d)
}

#' FUNCTION: siginificant_coverage
#'

#' generates a latex table for coverage table with effect size and U test.
#' @importFrom magrittr %>%
#' @export
siginificant_coverage <- function(d, rtrn = "tex", m = "median") {
  library(dplyr)
  library(reshape2)
  library(xtable)
  d <- generate_15_schmea_mutation_analysis(d)
  #d1 <- directedRandomR::transform_execution_times_for_threshold(d, 1000)
  d <- d %>% arrange(casestudy)
  d1 <- d
  if (m == "mean") {
    d <- d %>% select(dbms, casestudy, datagenerator, coverage, randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% summarise(coverage = format(round((mean(coverage)), 1), nsmall = 1))
  } else {
    d <- d %>% select(dbms, casestudy, datagenerator, coverage, randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% summarise(coverage = format(round((median(coverage)), 1), nsmall = 1))
  }
  d <- dcast(d, casestudy ~ dbms + datagenerator)
  a1 <- d[1]
  d2 <- d[2:13]
  d <- d2[ , order(names(d2))]
  c <- d[1:4]
  c <- c[c(3,1,2,4)]
  a <- d[5:8]
  a <- a[c(3,1,2,4)]
  b <- d[9:12]
  b <- b[c(3,1,2,4)]
  a1$casestudy <- as.character(a1$casestudy)
  numberOfRows <- nrow(d)
  for (i in 1:numberOfRows) {
    schema <- a1[i,]
    dr <- d1 %>% filter(casestudy == schema, datagenerator == "directedRandom")
    avm <- d1 %>% filter(casestudy == schema, datagenerator == "avs")
    avmd <- d1 %>% filter(casestudy == schema, datagenerator == "avsDefaults")
    rand <- d1 %>% filter(casestudy == schema, datagenerator == "random")
    #a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))

    postgres_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$coverage, (avm %>% filter(dbms == "Postgres"))$coverage)$size
    postgres_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$coverage, (avmd %>% filter(dbms == "Postgres"))$coverage)$size
    postgres_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$coverage, (rand %>% filter(dbms == "Postgres"))$coverage)$size
    #postgres <- NULL

    drp <- d1 %>% filter(casestudy == schema, datagenerator == "directedRandom")
    avmp <- d1 %>% filter(casestudy == schema, datagenerator == "avs")
    avmdp <- d1 %>% filter(casestudy == schema, datagenerator == "avsDefaults")
    randp <- d1 %>% filter(casestudy == schema, datagenerator == "random")

    # p <- wilcox.test((avmp %>% filter(dbms == "Postgres"))$coverage, (drp %>% filter(dbms == "Postgres"))$coverage)$p.value <= 0.05
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,2]) > as.numeric(a[i,1])) {
    #     a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    #   } else {
    #     a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    #   }
    # }

    dr_coverage <- (drp %>% filter(dbms == "Postgres"))$coverage
    avmr_coverage <- (avmp %>% filter(dbms == "Postgres"))$coverage

    p1 <- wilcox.test(dr_coverage, avmr_coverage, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_coverage, avmr_coverage, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
      a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R YES ", a[i,2], sep = "")
      # print(oh)
      a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
    }

    if (postgres_avm == "large") {
      a[i,2] = paste("$^{\\ast\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "medium") {
      a[i,2] = paste("$^{\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "small") {
      a[i,2] = paste("$^{\\ast}$",a[i,2], sep = "")
    } else {

    }

    # p <- wilcox.test((avmdp %>% filter(dbms == "Postgres"))$coverage, (drp %>% filter(dbms == "Postgres"))$coverage)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,3]) > as.numeric(a[i,1])) {
    #     a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
    #   } else {
    #     a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
    #   }
    # }

    avmd_coverage <- (avmdp %>% filter(dbms == "Postgres"))$coverage

    p1 <- wilcox.test(dr_coverage, avmd_coverage, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_coverage, avmd_coverage, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
      a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R YES ", a[i,2], sep = "")
      # print(oh)
      a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
    }

    if (postgres_avmd == "large") {
      a[i,3] = paste("$^{\\ast\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "medium") {
      a[i,3] = paste("$^{\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "small") {
      a[i,3] = paste("$^{\\ast}$",a[i,3], sep = "")
    } else {

    }

    #check1 <- (randp %>% filter(dbms == "Postgres"))$mutationScore
    #check2 <- (drp %>% filter(dbms == "Postgres"))$mutationScore
    #browser(condition = (schema == "NistWeather"))
    # p <- wilcox.test((randp %>% filter(dbms == "Postgres"))$coverage, (drp %>% filter(dbms == "Postgres"))$coverage)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,4]) > as.numeric(a[i,1])) {
    #     a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
    #   } else {
    #     #browser()
    #     a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
    #   }
    # }

    rand_coverage <- (randp %>% filter(dbms == "Postgres"))$coverage

    p1 <- wilcox.test(dr_coverage, rand_coverage, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_coverage, rand_coverage, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
      a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R YES ", a[i,2], sep = "")
      # print(oh)
      a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
    }

    if (postgres_rand == "large") {
      a[i,4] = paste("$^{\\ast\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "medium") {
      a[i,4] = paste("$^{\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "small") {
      a[i,4] = paste("$^{\\ast}$",a[i,4], sep = "")
    } else {

    }
    # if (postgres_avm == "large" & postgres_rand == "large" & postgres_avmd == "large") {
    #   postgres <- "^{\\ast\\ast\\ast}"
    # } else if (postgres_avm == "medium" & postgres_rand == "medium" & postgres_avmd == "medium") {
    #   postgres <- "^{\\ast\\ast}"
    # } else {
    #   postgres <- "^{\\ast}"
    # }

    #b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("\\textbf{", b[i,], "}", sep = ""), as.numeric(b[i,]))
    sqlite_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$coverage, (avm %>% filter(dbms == "SQLite"))$coverage)$size
    sqlite_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$coverage, (avmd %>% filter(dbms == "SQLite"))$coverage)$size
    sqlite_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$coverage, (rand %>% filter(dbms == "SQLite"))$coverage)$size
    #sqlite <- NULL

    # p <- wilcox.test((avmp %>% filter(dbms == "SQLite"))$coverage, (drp %>% filter(dbms == "SQLite"))$coverage)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,2]) > as.numeric(b[i,1])) {
    #     b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
    #   } else {
    #     b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
    #   }
    # }

    dr_coverage <- (drp %>% filter(dbms == "SQLite"))$coverage
    avmr_coverage <- (avmp %>% filter(dbms == "SQLite"))$coverage

    p1 <- wilcox.test(dr_coverage, avmr_coverage, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_coverage, avmr_coverage, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
    } else {
    }


    if (sqlite_avm == "large") {
      b[i,2] = paste("$^{\\ast\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "medium") {
      b[i,2] = paste("$^{\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "small") {
      b[i,2] = paste("$^{\\ast}$",b[i,2], sep = "")
    } else {

    }

    # p <- wilcox.test((avmdp %>% filter(dbms == "SQLite"))$coverage, (drp %>% filter(dbms == "SQLite"))$coverage)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,3]) > as.numeric(b[i,1])) {
    #     b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
    #   } else {
    #     b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
    #   }
    # }
    avmd_coverage <- (avmdp %>% filter(dbms == "SQLite"))$coverage

    p1 <- wilcox.test(dr_coverage, avmd_coverage, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_coverage, avmd_coverage, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
    } else {
    }

    if (sqlite_avmd == "large") {
      b[i,3] = paste("$^{\\ast\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "medium") {
      b[i,3] = paste("$^{\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "small") {
      b[i,3] = paste("$^{\\ast}$",b[i,3], sep = "")
    } else {

    }

    # p <- wilcox.test((randp %>% filter(dbms == "SQLite"))$coverage, (drp %>% filter(dbms == "SQLite"))$coverage)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,4]) > as.numeric(b[i,1])) {
    #     b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
    #   } else {
    #     b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
    #   }
    # }

    rand_coverage <- (randp %>% filter(dbms == "SQLite"))$coverage

    p1 <- wilcox.test(dr_coverage, rand_coverage, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_coverage, rand_coverage, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
    } else {
    }

    if (sqlite_rand == "large") {
      b[i,4] = paste("$^{\\ast\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "medium") {
      b[i,4] = paste("$^{\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "small") {
      b[i,4] = paste("$^{\\ast}$",b[i,4], sep = "")
    } else {

    }

    # if (sqlite_avm == "large" & sqlite_rand == "large" & sqlite_avmd == "large") {
    #   sqlite <- "^{\\ast\\ast\\ast}"
    # } else if (sqlite_avm == "medium" & sqlite_rand == "medium" & sqlite_avmd == "medium") {
    #   sqlite <- "^{\\ast\\ast}"
    # } else {
    #   sqlite <- "^{\\ast}"
    # }

    hsql_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$coverage, (avm %>% filter(dbms == "HyperSQL"))$coverage)$size
    hsql_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$coverage, (avmd %>% filter(dbms == "HyperSQL"))$coverage)$size
    hsql_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$coverage, (rand %>% filter(dbms == "HyperSQL"))$coverage)$size
    #c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("\\textbf{", c[i,], "}", sep = ""), as.numeric(c[i,]))


    # p <- wilcox.test((avmp %>% filter(dbms == "HyperSQL"))$coverage, (drp %>% filter(dbms == "HyperSQL"))$coverage)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,2]) > as.numeric(c[i,1])) {
    #     c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
    #   } else {
    #     c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
    #   }
    # }

    dr_coverage <- (drp %>% filter(dbms == "HyperSQL"))$coverage
    avmr_coverage <- (avmp %>% filter(dbms == "HyperSQL"))$coverage

    p1 <- wilcox.test(dr_coverage, avmr_coverage, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_coverage, avmr_coverage, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
    } else {
    }

    if (hsql_avm == "large") {
      c[i,2] = paste("$^{\\ast\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "medium") {
      c[i,2] = paste("$^{\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "small") {
      c[i,2] = paste("$^{\\ast}$",c[i,2], sep = "")
    } else {

    }

    # p <- wilcox.test((avmdp %>% filter(dbms == "HyperSQL"))$coverage, (drp %>% filter(dbms == "HyperSQL"))$coverage)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,3]) > as.numeric(c[i,1])) {
    #     c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
    #   } else {
    #     c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
    #   }
    # }

    avmd_coverage <- (avmdp %>% filter(dbms == "HyperSQL"))$coverage

    p1 <- wilcox.test(dr_coverage, avmd_coverage, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_coverage, avmd_coverage, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
    } else {
    }

    if (hsql_avmd == "large") {
      c[i,3] = paste("$^{\\ast\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "medium") {
      c[i,3] = paste("$^{\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "small") {
      c[i,3] = paste("$^{\\ast}$",c[i,3], sep = "")
    } else {

    }

    # p <- wilcox.test((randp %>% filter(dbms == "HyperSQL"))$coverage, (drp %>% filter(dbms == "HyperSQL"))$coverage)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,4]) > as.numeric(c[i,1])) {
    #     c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
    #   } else {
    #     c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
    #   }
    # }

    rand_coverage <- (randp %>% filter(dbms == "HyperSQL"))$coverage

    p1 <- wilcox.test(dr_coverage, rand_coverage, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_coverage, rand_coverage, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
    } else {
    }

    if (hsql_rand == "large") {
      c[i,4] = paste("$^{\\ast\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "medium") {
      c[i,4] = paste("$^{\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "small") {
      c[i,4] = paste("$^{\\ast}$",c[i,4], sep = "")
    } else {

    }

    # hsql <- NULL
    # if (hsql_avm == "large" & hsql_rand == "large" & hsql_avmd == "large") {
    #   hsql <- "^{\\ast\\ast\\ast}"
    # } else if (hsql_avm == "medium" & hsql_rand == "medium" & hsql_avmd == "medium") {
    #   hsql <- "^{\\ast\\ast}"
    # } else {
    #   hsql <- "^{\\ast}"
    # }
    if (a1[i,] == "NistXTS749") {
      a1[i,] <- "NistXTSNine"
    }
    a1[i,] <- paste("\\", a1[i,], "ForTable", sep = "")

    #a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("$",postgres,"$","\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))
    #b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("$",sqlite,"$","\\textbf{", b[i,], "}", sep = ""), as.numeric(b[i,]))
    #c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("$",hsql,"$","\\textbf{", c[i,], "}", sep = ""), as.numeric(c[i,]))
  }
  a <- a[c(1,4,3,2)]
  b <- b[c(1,4,3,2)]
  c <- c[c(1,4,3,2)]
  # With HSQL
  #d <- cbind(a1,c,a,b)
  # Without HSQL
  d <- cbind(a1,a,b)
  #return(d)
  #return(print(xtable(d), include.rownames=FALSE ,sanitize.text.function = function(x){x}))
  if (rtrn == "tex") {
    return(print(xtable(d), include.rownames=FALSE ,sanitize.text.function = function(x){x}))
  } else {
    return(d)
  }
}

#' FUNCTION: siginificant_timing
#'

#' generates a latex table for test generation timing table with effect size and U test.
#' @importFrom magrittr %>%
#' @export
siginificant_timing <- function(d, rtrn = "tex", m = "median") {
  library(dplyr)
  library(reshape2)
  library(xtable)
  # d <- generate_15_schmea_mutation_analysis(d)
  d <- d %>% arrange(casestudy)
  d3 <- d
  #browser()
  d1 <- directedRandomR::transform_execution_times_for_threshold(d, 1000)
  d2 <- d
  if (m == "mean") {
    d <- d %>% select(dbms, casestudy, datagenerator, testgenerationtime, randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% summarise(testgenerationtime = format(round((mean(testgenerationtime) / 1000), 2), nsmall = 2))
  } else {
    d <- d %>% select(dbms, casestudy, datagenerator, testgenerationtime, randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% summarise(testgenerationtime = format(round((median(testgenerationtime) / 1000), 2), nsmall = 2))
  }
  d <- dcast(d, casestudy ~ dbms + datagenerator)
  a1 <- d[1]
  d2 <- d[2:13]
  d <- d2[ , order(names(d2))]
  c <- d[1:4]
  c <- c[c(3,1,2,4)]
  a <- d[5:8]
  a <- a[c(3,1,2,4)]
  b <- d[9:12]
  b <- b[c(3,1,2,4)]
  numberOfRows <- nrow(d)
  a1$casestudy <- as.character(a1$casestudy)
  for (i in 1:numberOfRows) {
    schema <- a1[i,]
    dr <- d1 %>% filter(casestudy == schema, datagenerator == "directedRandom")
    avm <- d1 %>% filter(casestudy == schema, datagenerator == "avs")
    avmd <- d1 %>% filter(casestudy == schema, datagenerator == "avsDefaults")
    rand <- d1 %>% filter(casestudy == schema, datagenerator == "random")
    #a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))

    postgres_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$testgenerationtime, (avm %>% filter(dbms == "Postgres"))$testgenerationtime)$size
    postgres_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$testgenerationtime, (avmd %>% filter(dbms == "Postgres"))$testgenerationtime)$size
    postgres_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$testgenerationtime, (rand %>% filter(dbms == "Postgres"))$testgenerationtime)$size
    #postgres <- NULL

    drp <- d3 %>% filter(casestudy == schema, datagenerator == "directedRandom")
    avmp <- d3 %>% filter(casestudy == schema, datagenerator == "avs")
    avmdp <- d3 %>% filter(casestudy == schema, datagenerator == "avsDefaults")
    randp <- d3 %>% filter(casestudy == schema, datagenerator == "random")

    # p <- wilcox.test((avmp %>% filter(dbms == "Postgres"))$testgenerationtime, (drp %>% filter(dbms == "Postgres"))$testgenerationtime)$p.value <= 0.05
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,2]) < as.numeric(a[i,1])) {
    #     a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    #   } else {
    #     a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    #   }
    # }

    dr_time <- (drp %>% filter(dbms == "Postgres"))$testgenerationtime
    avmr_time <- (avmp %>% filter(dbms == "Postgres"))$testgenerationtime

    p1 <- wilcox.test(dr_time, avmr_time, alternative = "greater")$p.value >= 0.01
    p2 <- wilcox.test(dr_time, avmr_time, alternative = "less")$p.value >= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
      a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R YES ", a[i,2], sep = "")
      # print(oh)
      a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
    }

    if (postgres_avm == "large") {
      a[i,2] = paste("$^{\\ast\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "medium") {
      a[i,2] = paste("$^{\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "small") {
      a[i,2] = paste("$^{\\ast}$",a[i,2], sep = "")
    } else {

    }
    # p <- wilcox.test((avmdp %>% filter(dbms == "Postgres"))$testgenerationtime, (drp %>% filter(dbms == "Postgres"))$testgenerationtime)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,3]) < as.numeric(a[i,1])) {
    #     a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
    #   } else {
    #     a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
    #   }
    # }

    avmd_time <- (avmdp %>% filter(dbms == "Postgres"))$testgenerationtime

    p1 <- wilcox.test(dr_time, avmd_time, alternative = "greater")$p.value >= 0.01
    p2 <- wilcox.test(dr_time, avmd_time, alternative = "less")$p.value >= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", a[i,1], " AVM-R NOT ", a[i,3], sep = "")
      # print(oh)
      a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R YES ", a[i,3], sep = "")
      # print(oh)
      a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R NOT ", a[i,3], sep = "")
      # print(oh)
    }

    if (postgres_avmd == "large") {
      a[i,3] = paste("$^{\\ast\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "medium") {
      a[i,3] = paste("$^{\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "small") {
      a[i,3] = paste("$^{\\ast}$",a[i,3], sep = "")
    } else {

    }

    #check1 <- (randp %>% filter(dbms == "Postgres"))$mutationScore
    #check2 <- (drp %>% filter(dbms == "Postgres"))$mutationScore
    #browser(condition = (schema == "NistWeather"))
    # p <- wilcox.test((randp %>% filter(dbms == "Postgres"))$testgenerationtime, (drp %>% filter(dbms == "Postgres"))$testgenerationtime)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,4]) < as.numeric(a[i,1])) {
    #     a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
    #   } else {
    #     #browser()
    #     a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
    #   }
    # }

    rand_time <- (randp %>% filter(dbms == "Postgres"))$testgenerationtime

    p1 <- wilcox.test(dr_time, rand_time, alternative = "greater")$p.value >= 0.01
    p2 <- wilcox.test(dr_time, rand_time, alternative = "less")$p.value >= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", a[i,1], " AVM-R NOT ", a[i,4], sep = "")
      # print(oh)
      a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R YES ", a[i,4], sep = "")
      # print(oh)
      a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", a[i,1], " AVM-R NOT ", a[i,4], sep = "")
      # print(oh)
    }

    if (postgres_rand == "large") {
      a[i,4] = paste("$^{\\ast\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "medium") {
      a[i,4] = paste("$^{\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "small") {
      a[i,4] = paste("$^{\\ast}$",a[i,4], sep = "")
    } else {

    }
    # if (postgres_avm == "large" & postgres_rand == "large" & postgres_avmd == "large") {
    #   postgres <- "^{\\ast\\ast\\ast}"
    # } else if (postgres_avm == "medium" & postgres_rand == "medium" & postgres_avmd == "medium") {
    #   postgres <- "^{\\ast\\ast}"
    # } else {
    #   postgres <- "^{\\ast}"
    # }

    #b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("\\textbf{", b[i,], "}", sep = ""), as.numeric(b[i,]))
    sqlite_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$testgenerationtime, (avm %>% filter(dbms == "SQLite"))$testgenerationtime)$size
    sqlite_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$testgenerationtime, (avmd %>% filter(dbms == "SQLite"))$testgenerationtime)$size
    sqlite_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$testgenerationtime, (rand %>% filter(dbms == "SQLite"))$testgenerationtime)$size
    #sqlite <- NULL

    # p <- wilcox.test((avmp %>% filter(dbms == "SQLite"))$testgenerationtime, (drp %>% filter(dbms == "SQLite"))$testgenerationtime)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,2]) < as.numeric(b[i,1])) {
    #     b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
    #   } else {
    #     b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
    #   }
    # }

    dr_time <- (drp %>% filter(dbms == "SQLite"))$testgenerationtime
    avmr_time <- (avmp %>% filter(dbms == "SQLite"))$testgenerationtime

    p1 <- wilcox.test(dr_time, avmr_time, alternative = "greater")$p.value >= 0.01
    p2 <- wilcox.test(dr_time, avmr_time, alternative = "less")$p.value >= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", b[i,1], " AVM-R NOT ", b[i,2], sep = "")
      # print(oh)
      b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", b[i,1], " AVM-R YES ", b[i,2], sep = "")
      # print(oh)
      b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", b[i,1], " AVM-R NOT ", b[i,2], sep = "")
      # print(oh)
    }

    if (sqlite_avm == "large") {
      b[i,2] = paste("$^{\\ast\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "medium") {
      b[i,2] = paste("$^{\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "small") {
      b[i,2] = paste("$^{\\ast}$",b[i,2], sep = "")
    } else {

    }

    # p <- wilcox.test((avmdp %>% filter(dbms == "SQLite"))$testgenerationtime, (drp %>% filter(dbms == "SQLite"))$testgenerationtime)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,3]) < as.numeric(b[i,1])) {
    #     b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
    #   } else {
    #     b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
    #   }
    # }

    avmd_time <- (avmdp %>% filter(dbms == "SQLite"))$testgenerationtime

    p1 <- wilcox.test(dr_time, avmd_time, alternative = "greater")$p.value >= 0.01
    p2 <- wilcox.test(dr_time, avmd_time, alternative = "less")$p.value >= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", b[i,1], " AVM-R NOT ", b[i,3], sep = "")
      # print(oh)
      b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", b[i,1], " AVM-R YES ", b[i,3], sep = "")
      # print(oh)
      b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", b[i,1], " AVM-R NOT ", b[i,3], sep = "")
      # print(oh)
    }

    if (sqlite_avmd == "large") {
      b[i,3] = paste("$^{\\ast\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "medium") {
      b[i,3] = paste("$^{\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "small") {
      b[i,3] = paste("$^{\\ast}$",b[i,3], sep = "")
    } else {

    }

    # p <- wilcox.test((randp %>% filter(dbms == "SQLite"))$testgenerationtime, (drp %>% filter(dbms == "SQLite"))$testgenerationtime)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,4]) < as.numeric(b[i,1])) {
    #     b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
    #   } else {
    #     b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
    #   }
    # }
    #
    rand_time <- (randp %>% filter(dbms == "SQLite"))$testgenerationtime

    p1 <- wilcox.test(dr_time, avmd_time, alternative = "greater")$p.value >= 0.01
    p2 <- wilcox.test(dr_time, avmd_time, alternative = "less")$p.value >= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", b[i,1], " AVM-R NOT ", b[i,4], sep = "")
      # print(oh)
      b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", b[i,1], " AVM-R YES ", b[i,4], sep = "")
      # print(oh)
      b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", b[i,1], " AVM-R NOT ", b[i,4], sep = "")
      # print(oh)
    }

    if (sqlite_rand == "large") {
      b[i,4] = paste("$^{\\ast\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "medium") {
      b[i,4] = paste("$^{\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "small") {
      b[i,4] = paste("$^{\\ast}$",b[i,4], sep = "")
    } else {

    }

    # if (sqlite_avm == "large" & sqlite_rand == "large" & sqlite_avmd == "large") {
    #   sqlite <- "^{\\ast\\ast\\ast}"
    # } else if (sqlite_avm == "medium" & sqlite_rand == "medium" & sqlite_avmd == "medium") {
    #   sqlite <- "^{\\ast\\ast}"
    # } else {
    #   sqlite <- "^{\\ast}"
    # }

    hsql_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$testgenerationtime, (avm %>% filter(dbms == "HyperSQL"))$testgenerationtime)$size
    hsql_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$testgenerationtime, (avmd %>% filter(dbms == "HyperSQL"))$testgenerationtime)$size
    hsql_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$testgenerationtime, (rand %>% filter(dbms == "HyperSQL"))$testgenerationtime)$size
    #c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("\\textbf{", c[i,], "}", sep = ""), as.numeric(c[i,]))


    # p <- wilcox.test((avmp %>% filter(dbms == "HyperSQL"))$testgenerationtime, (drp %>% filter(dbms == "HyperSQL"))$testgenerationtime)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,2]) < as.numeric(c[i,1])) {
    #     c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
    #   } else {
    #     c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
    #   }
    # }

    dr_time <- (drp %>% filter(dbms == "HyperSQL"))$testgenerationtime
    avmr_time <- (avmp %>% filter(dbms == "HyperSQL"))$testgenerationtime

    p1 <- wilcox.test(dr_time, avmr_time, alternative = "greater")$p.value >= 0.01
    p2 <- wilcox.test(dr_time, avmr_time, alternative = "less")$p.value >= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", c[i,1], " AVM-R NOT ", c[i,2], sep = "")
      # print(oh)
      c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", c[i,1], " AVM-R YES ", c[i,2], sep = "")
      # print(oh)
      c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", c[i,1], " AVM-R NOT ", c[i,2], sep = "")
      # print(oh)
    }

    if (hsql_avm == "large") {
      c[i,2] = paste("$^{\\ast\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "medium") {
      c[i,2] = paste("$^{\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "small") {
      c[i,2] = paste("$^{\\ast}$",c[i,2], sep = "")
    } else {

    }

    # p <- wilcox.test((avmdp %>% filter(dbms == "HyperSQL"))$testgenerationtime, (drp %>% filter(dbms == "HyperSQL"))$testgenerationtime)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,3]) < as.numeric(c[i,1])) {
    #     c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
    #   } else {
    #     c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
    #   }
    # }

    avmd_time <- (avmdp %>% filter(dbms == "HyperSQL"))$testgenerationtime

    p1 <- wilcox.test(dr_time, avmd_time, alternative = "greater")$p.value >= 0.01
    p2 <- wilcox.test(dr_time, avmd_time, alternative = "less")$p.value >= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", c[i,1], " AVM-R NOT ", c[i,2], sep = "")
      # print(oh)
      c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", c[i,1], " AVM-R YES ", c[i,2], sep = "")
      # print(oh)
      c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", c[i,1], " AVM-R NOT ", c[i,2], sep = "")
      # print(oh)
    }

    if (hsql_avmd == "large") {
      c[i,3] = paste("$^{\\ast\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "medium") {
      c[i,3] = paste("$^{\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "small") {
      c[i,3] = paste("$^{\\ast}$",c[i,3], sep = "")
    } else {

    }

    # p <- wilcox.test((randp %>% filter(dbms == "HyperSQL"))$testgenerationtime, (drp %>% filter(dbms == "HyperSQL"))$testgenerationtime)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,4]) < as.numeric(c[i,1])) {
    #     c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
    #   } else {
    #     c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
    #   }
    # }

    rand_time <- (randp %>% filter(dbms == "HyperSQL"))$testgenerationtime

    p1 <- wilcox.test(dr_time, rand_time, alternative = "greater")$p.value >= 0.01
    p2 <- wilcox.test(dr_time, rand_time, alternative = "less")$p.value >= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema, " DR YES ", c[i,1], " AVM-R NOT ", c[i,4], sep = "")
      # print(oh)
      c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", c[i,1], " AVM-R YES ", c[i,4], sep = "")
      # print(oh)
      c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema, " DR NOT ", c[i,1], " AVM-R NOT ", c[i,4], sep = "")
      # print(oh)
    }

    if (hsql_rand == "large") {
      c[i,4] = paste("$^{\\ast\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "medium") {
      c[i,4] = paste("$^{\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "small") {
      c[i,4] = paste("$^{\\ast}$",c[i,4], sep = "")
    } else {

    }

    # hsql <- NULL
    # if (hsql_avm == "large" & hsql_rand == "large" & hsql_avmd == "large") {
    #   hsql <- "^{\\ast\\ast\\ast}"
    # } else if (hsql_avm == "medium" & hsql_rand == "medium" & hsql_avmd == "medium") {
    #   hsql <- "^{\\ast\\ast}"
    # } else {
    #   hsql <- "^{\\ast}"
    # }
    if (a1[i,] == "NistXTS749") {
      a1[i,] <- "NistXTSNine"
    }
    a1[i,] <- paste("\\", a1[i,], "ForTable", sep = "")

    #a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("$",postgres,"$","\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))
    #b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("$",sqlite,"$","\\textbf{", b[i,], "}", sep = ""), as.numeric(b[i,]))
    #c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("$",hsql,"$","\\textbf{", c[i,], "}", sep = ""), as.numeric(c[i,]))
  }
  a <- a[c(1,4,3,2)]
  b <- b[c(1,4,3,2)]
  c <- c[c(1,4,3,2)]

  # With HSQL
  #d <- cbind(a1,c,a,b)
  # Without HSQL
  d <- cbind(a1,a,b)
  #return(d)
  if (rtrn == "tex") {
    return(print(xtable(d), include.rownames=FALSE ,sanitize.text.function = function(x){x}))
  } else {
    return(d)
  }
}

#' FUNCTION: siginificant_mutation_score
#'

#' generates a latex table for mutation score per schema table with effect size and U test.
#' @importFrom magrittr %>%
#' @export
siginificant_mutation_score <- function(d, rtrn = "tex", m = "median") {
  library(dplyr)
  library(reshape2)
  library(xtable)
  #d <- generate_15_schmea_mutanttiming(d)
  #d <- d %>% arrange(schema, identifier, operator)
  #browser()
  #d1 <- directedRandomR::transform_execution_times_for_threshold(d, 1000)
  #d1 <- d %>% select(dbms, casestudy, datagenerator, scorenumerator, scoredenominator,randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% mutate(mutationScore = scorenumerator/scoredenominator)#mutate(mutationScore = round((sum(scorenumerator)/sum(scoredenominator)) * 100, 2)) #summarise(scorenumerator = (sum(scorenumerator)), scoredenominator = (sum(scoredenominator)))
  #d <- d %>% select(dbms, casestudy, datagenerator, scorenumerator, scoredenominator,randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% summarise(mutationScore = round((sum(scorenumerator)/sum(scoredenominator)) * 100, 2))#mutate(mutationScore = sum(scorenumerator)/sum(scoredenominator)) #summarise(scorenumerator = (sum(scorenumerator)), scoredenominator = (sum(scoredenominator)))
  #d1 <- d %>% filter(type == "NORMAL") %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  #d <- d %>% group_by(schema, generator, dbms)  %>% filter(type == "NORMAL") %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = format(round((killed_mutants/total_mutants) * 100, 1), nsmall = 1))
  d <- ordering_mutants_per_schema(d)
  #d1 <- directedRandomR::transform_execution_times_for_threshold(d, 1000)
  #d1 <- d %>% select(dbms, casestudy, datagenerator, scorenumerator, scoredenominator,randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% mutate(mutationScore = scorenumerator/scoredenominator)#mutate(mutationScore = round((sum(scorenumerator)/sum(scoredenominator)) * 100, 2)) #summarise(scorenumerator = (sum(scorenumerator)), scoredenominator = (sum(scoredenominator)))
  #d <- d %>% select(dbms, casestudy, datagenerator, scorenumerator, scoredenominator,randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% summarise(mutationScore = round((sum(scorenumerator)/sum(scoredenominator)) * 100, 2))#mutate(mutationScore = sum(scorenumerator)/sum(scoredenominator)) #summarise(scorenumerator = (sum(scorenumerator)), scoredenominator = (sum(scoredenominator)))
  #d1 <- d %>% filter(type == "NORMAL") %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  d1 <- d
  if (m == "mean") {
    d <- d %>% group_by(schema, generator, dbms)  %>% summarise(mutationScore = format(round(mean(mutationScore), 1), nsmall = 1))
  } else {
    d <- d %>% group_by(schema, generator, dbms)  %>% summarise(mutationScore = format(round(median(mutationScore), 1), nsmall = 1))
  }
  #d <- dcast(d, casestudy ~ dbms + datagenerator)
  d <- dcast(d, schema ~ dbms + generator)
  a1 <- d[1]
  d2 <- d[2:13]
  d <- d2[ , order(names(d2))]
  c <- d[1:4]
  c <- c[c(3,1,2,4)]
  a <- d[5:8]
  a <- a[c(3,1,2,4)]
  b <- d[9:12]
  b <- b[c(3,1,2,4)]
  a1$schema <- as.character(a1$schema)
  numberOfRows <- nrow(d)
  for (i in 1:numberOfRows) {
    schema1 <- a1[i,]
    dr <- d1 %>% filter(schema == schema1, generator == "directedRandom")
    avm <- d1 %>% filter(schema == schema1, generator == "avs")
    avmd <- d1 %>% filter(schema == schema1, generator == "avsDefaults")
    rand <- d1 %>% filter(schema == schema1, generator == "random")
    #a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))

    postgres_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (avm %>% filter(dbms == "Postgres"))$mutationScore)$size
    postgres_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (avmd %>% filter(dbms == "Postgres"))$mutationScore)$size
    postgres_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (rand %>% filter(dbms == "Postgres"))$mutationScore)$size
    #postgres <- NULL
    #browser()
    drp <- d1 %>% filter(schema == schema1, generator == "directedRandom")
    avmp <- d1 %>% filter(schema == schema1, generator == "avs")
    avmdp <- d1 %>% filter(schema == schema1, generator == "avsDefaults")
    randp <- d1 %>% filter(schema == schema1, generator == "random")
    #p <- wilcox.test((avmp %>% filter(dbms == "Postgres"))$mutationScore, (drp %>% filter(dbms == "Postgres"))$mutationScore, alternative = "greater")$p.value <= 0.01

    dr_mutation <- (drp %>% filter(dbms == "Postgres"))$mutationScore
    avmr_mutation <- (avmp %>% filter(dbms == "Postgres"))$mutationScore

    p1 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema1, " DR YES ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
      a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-R YES ", a[i,2], sep = "")
      # print(oh)
      a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
    }


    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,2]) > as.numeric(a[i,1])) {
    #     a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    #   } else {
    #     a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    #   }
    # }

    if (postgres_avm == "large") {
      a[i,2] = paste("$^{\\ast\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "medium") {
      a[i,2] = paste("$^{\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "small") {
      a[i,2] = paste("$^{\\ast}$",a[i,2], sep = "")
    } else {
      a[i,2] = paste("$",a[i,2],"$", sep = "")
    }

    #p <- wilcox.test((avmdp %>% filter(dbms == "Postgres"))$mutationScore, (drp %>% filter(dbms == "Postgres"))$mutationScore)$p.value <= 0.05

    avmd_mutation <- (avmdp %>% filter(dbms == "Postgres"))$mutationScore

    p1 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema1, " DR YES ", a[i,1], " AVM-D NOT ", a[i,3], sep = "")
      # print(oh)
      a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-D YES ", a[i,3], sep = "")
      # print(oh)
      a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-D NOT ", a[i,3], sep = "")
      # print(oh)
    }
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,3]) > as.numeric(a[i,1])) {
    #     a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
    #   } else {
    #     a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
    #   }
    # }

    if (postgres_avmd == "large") {
      a[i,3] = paste("$^{\\ast\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "medium") {
      a[i,3] = paste("$^{\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "small") {
      a[i,3] = paste("$^{\\ast}$",a[i,3], sep = "")
    } else {

    }

    #check1 <- (randp %>% filter(dbms == "Postgres"))$mutationScore
    #check2 <- (drp %>% filter(dbms == "Postgres"))$mutationScore
    #browser(condition = (schema == "NistWeather"))
    #p <- wilcox.test((randp %>% filter(dbms == "Postgres"))$mutationScore, (drp %>% filter(dbms == "Postgres"))$mutationScore)$p.value <= 0.05

    rand_mutation <- (randp %>% filter(dbms == "Postgres"))$mutationScore

    p1 <- wilcox.test(dr_mutation, rand_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, rand_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema1, " DR YES ", a[i,1], " AVM-D NOT ", a[i,4], sep = "")
      # print(oh)
      a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-D YES ", a[i,4], sep = "")
      # print(oh)
      a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-D NOT ", a[i,4], sep = "")
      # print(oh)
    }

    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,4]) > as.numeric(a[i,1])) {
    #     a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
    #   } else {
    #     #browser()
    #     a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
    #   }
    # }

    if (postgres_rand == "large") {
      a[i,4] = paste("$^{\\ast\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "medium") {
      a[i,4] = paste("$^{\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "small") {
      a[i,4] = paste("$^{\\ast}$",a[i,4], sep = "")
    } else {

    }
    # if (postgres_avm == "large" & postgres_rand == "large" & postgres_avmd == "large") {
    #   postgres <- "^{\\ast\\ast\\ast}"
    # } else if (postgres_avm == "medium" & postgres_rand == "medium" & postgres_avmd == "medium") {
    #   postgres <- "^{\\ast\\ast}"
    # } else {
    #   postgres <- "^{\\ast}"
    # }

    #b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("\\textbf{", b[i,], "}", sep = ""), as.numeric(b[i,]))
    sqlite_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$mutationScore, (avm %>% filter(dbms == "SQLite"))$mutationScore)$size
    sqlite_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$mutationScore, (avmd %>% filter(dbms == "SQLite"))$mutationScore)$size
    sqlite_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$mutationScore, (rand %>% filter(dbms == "SQLite"))$mutationScore)$size
    #sqlite <- NULL

    #p <- wilcox.test((avmp %>% filter(dbms == "SQLite"))$mutationScore, (drp %>% filter(dbms == "SQLite"))$mutationScore)$p.value <= 0.05

    dr_mutation <- (drp %>% filter(dbms == "SQLite"))$mutationScore
    avmr_mutation <- (avmp %>% filter(dbms == "SQLite"))$mutationScore

    p1 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("SQLite Schema => ", schema1, " DR YES ", b[i,1], " AVM-D NOT ", b[i,2], sep = "")
      # print(oh)
      b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D YES ", b[i,2], sep = "")
      # print(oh)
      b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
    } else {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D NOT ", b[i,2], sep = "")
      # print(oh)
    }


    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,2]) > as.numeric(b[i,1])) {
    #     b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
    #   } else {
    #     b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
    #   }
    # }

    if (sqlite_avm == "large") {
      b[i,2] = paste("$^{\\ast\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "medium") {
      b[i,2] = paste("$^{\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "small") {
      b[i,2] = paste("$^{\\ast}$",b[i,2], sep = "")
    } else {

    }

    #p <- wilcox.test((avmdp %>% filter(dbms == "SQLite"))$mutationScore, (drp %>% filter(dbms == "SQLite"))$mutationScore)$p.value <= 0.05

    avmd_mutation <- (avmdp %>% filter(dbms == "SQLite"))$mutationScore
    #if (schema1 == "RiskIt") { browser() }

    p1 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("SQLite Schema => ", schema1, " DR YES ", b[i,1], " AVM-D NOT ", b[i,3], sep = "")
      # print(oh)
      b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D YES ", b[i,3], sep = "")
      # print(oh)
      b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
    } else {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D NOT ", b[i,3], sep = "")
      # print(oh)
    }

    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,3]) > as.numeric(b[i,1])) {
    #     b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
    #   } else {
    #     b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
    #   }
    # }

    if (sqlite_avmd == "large") {
      b[i,3] = paste("$^{\\ast\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "medium") {
      b[i,3] = paste("$^{\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "small") {
      b[i,3] = paste("$^{\\ast}$",b[i,3], sep = "")
    } else {

    }

    #p <- wilcox.test((randp %>% filter(dbms == "SQLite"))$mutationScore, (drp %>% filter(dbms == "SQLite"))$mutationScore)$p.value <= 0.05

    rand_mutation <- (randp %>% filter(dbms == "SQLite"))$mutationScore

    p1 <- wilcox.test(dr_mutation, rand_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, rand_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("SQLite Schema => ", schema1, " DR YES ", b[i,1], " AVM-D NOT ", b[i,3], sep = "")
      # print(oh)
      b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D YES ", b[i,3], sep = "")
      # print(oh)
      b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
    } else {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D NOT ", b[i,3], sep = "")
      # print(oh)
    }

    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,4]) > as.numeric(b[i,1])) {
    #     b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
    #   } else {
    #     b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
    #   }
    # }

    if (sqlite_rand == "large") {
      b[i,4] = paste("$^{\\ast\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "medium") {
      b[i,4] = paste("$^{\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "small") {
      b[i,4] = paste("$^{\\ast}$",b[i,4], sep = "")
    } else {

    }

    # if (sqlite_avm == "large" & sqlite_rand == "large" & sqlite_avmd == "large") {
    #   sqlite <- "^{\\ast\\ast\\ast}"
    # } else if (sqlite_avm == "medium" & sqlite_rand == "medium" & sqlite_avmd == "medium") {
    #   sqlite <- "^{\\ast\\ast}"
    # } else {
    #   sqlite <- "^{\\ast}"
    # }

    hsql_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$mutationScore, (avm %>% filter(dbms == "HyperSQL"))$mutationScore)$size
    hsql_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$mutationScore, (avmd %>% filter(dbms == "HyperSQL"))$mutationScore)$size
    hsql_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$mutationScore, (rand %>% filter(dbms == "HyperSQL"))$mutationScore)$size
    #c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("\\textbf{", c[i,], "}", sep = ""), as.numeric(c[i,]))


    #p <- wilcox.test((avmp %>% filter(dbms == "HyperSQL"))$mutationScore, (drp %>% filter(dbms == "HyperSQL"))$mutationScore)$p.value <= 0.05

    dr_mutation <- (drp %>% filter(dbms == "HyperSQL"))$mutationScore
    avmr_mutation <- (avmp %>% filter(dbms == "HyperSQL"))$mutationScore

    p1 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("HyperSQL Schema => ", schema1, " DR YES ", c[i,1], " AVM-D NOT ", c[i,2], sep = "")
      # print(oh)
      c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D YES ", c[i,2], sep = "")
      # print(oh)
      c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
    } else {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D NOT ", c[i,2], sep = "")
      # print(oh)
    }

    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,2]) > as.numeric(c[i,1])) {
    #     c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
    #   } else {
    #     c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
    #   }
    # }

    if (hsql_avm == "large") {
      c[i,2] = paste("$^{\\ast\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "medium") {
      c[i,2] = paste("$^{\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "small") {
      c[i,2] = paste("$^{\\ast}$",c[i,2], sep = "")
    } else {

    }

    #p <- wilcox.test((avmdp %>% filter(dbms == "HyperSQL"))$mutationScore, (drp %>% filter(dbms == "HyperSQL"))$mutationScore)$p.value <= 0.05

    avmd_mutation <- (avmdp %>% filter(dbms == "HyperSQL"))$mutationScore

    p1 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Schema => ", schema1, " DR YES ", c[i,1], " AVM-D NOT ", c[i,3], sep = "")
      # print(oh)
      c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D YES ", c[i,3], sep = "")
      # print(oh)
      c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
    } else {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D NOT ", c[i,3], sep = "")
      # print(oh)
    }

    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,3]) > as.numeric(c[i,1])) {
    #     c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
    #   } else {
    #     c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
    #   }
    # }

    if (hsql_avmd == "large") {
      c[i,3] = paste("$^{\\ast\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "medium") {
      c[i,3] = paste("$^{\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "small") {
      c[i,3] = paste("$^{\\ast}$",c[i,3], sep = "")
    } else {

    }

    #p <- wilcox.test((randp %>% filter(dbms == "HyperSQL"))$mutationScore, (drp %>% filter(dbms == "HyperSQL"))$mutationScore)$p.value <= 0.05

    rand_mutation <- (randp %>% filter(dbms == "HyperSQL"))$mutationScore

    p1 <- wilcox.test(dr_mutation, rand_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, rand_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Schema => ", schema1, " DR YES ", c[i,1], " AVM-D NOT ", c[i,4], sep = "")
      # print(oh)
      c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D YES ", c[i,4], sep = "")
      # print(oh)
      c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
    } else {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D NOT ", c[i,4], sep = "")
      # print(oh)
    }

    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,4]) > as.numeric(c[i,1])) {
    #     c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
    #   } else {
    #     c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
    #   }
    # }

    if (hsql_rand == "large") {
      c[i,4] = paste("$^{\\ast\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "medium") {
      c[i,4] = paste("$^{\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "small") {
      c[i,4] = paste("$^{\\ast}$",c[i,4], sep = "")
    } else {

    }

    # hsql <- NULL
    # if (hsql_avm == "large" & hsql_rand == "large" & hsql_avmd == "large") {
    #   hsql <- "^{\\ast\\ast\\ast}"
    # } else if (hsql_avm == "medium" & hsql_rand == "medium" & hsql_avmd == "medium") {
    #   hsql <- "^{\\ast\\ast}"
    # } else {
    #   hsql <- "^{\\ast}"
    # }
    #browser()
    if (a1[i,] == "NistXTS749") {
      a1[i,] <- "NistXTSNine"
    }
    a1[i,] <- paste("\\", a1[i,], "ForTable", sep = "")
    #a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("$",postgres,"$","\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))
    #b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("$",sqlite,"$","\\textbf{", b[i,], "}", sep = ""), as.numeric(b[i,]))
    #c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("$",hsql,"$","\\textbf{", c[i,], "}", sep = ""), as.numeric(c[i,]))
  }
  a <- a[c(1,4,3,2)]
  b <- b[c(1,4,3,2)]
  c <- c[c(1,4,3,2)]
  # With HSQL
  #d <- cbind(a1,c,a,b)
  # Without HSQL
  d <- cbind(a1,a,b)
  #return(d)
  #return(print(xtable(d), include.rownames=FALSE ,sanitize.text.function = function(x){x}))
  if (rtrn == "tex") {
    return(print(xtable(d), include.rownames=FALSE ,sanitize.text.function = function(x){x}))
  } else {
    return(d)
  }
}

#' FUNCTION: siginificant_mutant_operators
#'

#' generates a latex table for mutation operators table with effect size and U test.
#' @importFrom magrittr %>%
#' @export
siginificant_mutant_operators <- function(d) {
  library(dplyr)
  library(reshape2)
  # d <- generate_15_schmea_mutanttiming(d)
  d <- d %>% arrange(schema, identifier, operator)
  d1 <- d %>% filter(type == "NORMAL") %>% select(identifier, dbms, generator, killed, operator, schema) %>% group_by(identifier, dbms, generator, operator, schema) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  newdata <- d %>% group_by(generator, operator, dbms) %>% filter(type == "NORMAL") %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "false") + sum(killed == "true")))
  a <- newdata %>% group_by(dbms, generator, operator) %>% summarise(killed_mutants=mean(killed_mutants), total_mutants = mean(total_mutants)) %>% mutate(value = format(round((killed_mutants / total_mutants) * 100, 1), nsmall = 1))
  a <- a %>% select(dbms, generator, operator, value)
  d <- dcast(a,  operator ~ dbms + generator)

  a1 <- d[1]
  d2 <- d[2:13]
  d <- d2[ , order(names(d2))]
  c <- d[1:4]
  c <- c[c(3,1,2,4)]
  a <- d[5:8]
  a <- a[c(3,1,2,4)]
  b <- d[9:12]
  b <- b[c(3,1,2,4)]
  numberOfRows <- nrow(d)
  for (i in 1:numberOfRows) {
    selected_operator <- a1[i,]

    dr <- d1 %>% filter(operator == selected_operator, generator == "directedRandom")
    avm <- d1 %>% filter(operator == selected_operator, generator == "avs")
    avmd <- d1 %>% filter(operator == selected_operator, generator == "avsDefaults")
    rand <- d1 %>% filter(operator == selected_operator, generator == "random")
    #a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))

    postgres_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (avm %>% filter(dbms == "Postgres"))$mutationScore)$size
    postgres_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (avmd %>% filter(dbms == "Postgres"))$mutationScore)$size
    postgres_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (rand %>% filter(dbms == "Postgres"))$mutationScore)$size
    if (selected_operator == "CCRelationalExpressionOperatorE") {
      browser()
    }
    p <- wilcox.test((avm %>% filter(dbms == "Postgres"))$mutationScore, (dr %>% filter(dbms == "Postgres"))$mutationScore)$p.value <= 0.05
    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(a[i,2]) > as.numeric(a[i,1])) {
        a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
      } else {
        a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
      }
    }

    if (postgres_avm == "large") {
      a[i,2] = paste("$^{\\ast\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "medium") {
      a[i,2] = paste("$^{\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "small") {
      a[i,2] = paste("$^{\\ast}$",a[i,2], sep = "")
    } else {
    }

    p <- wilcox.test((avmd %>% filter(dbms == "Postgres"))$mutationScore, (dr %>% filter(dbms == "Postgres"))$mutationScore)$p.value <= 0.05

    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(a[i,3]) > as.numeric(a[i,1])) {
        a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
      } else {
        a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
      }
    }

    if (postgres_avmd == "large") {
      a[i,3] = paste("$^{\\ast\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "medium") {
      a[i,3] = paste("$^{\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "small") {
      a[i,3] = paste("$^{\\ast}$",a[i,3], sep = "")
    } else {

    }

    #check1 <- (randp %>% filter(dbms == "Postgres"))$mutationScore
    #check2 <- (drp %>% filter(dbms == "Postgres"))$mutationScore
    #browser(condition = (schema == "NistWeather"))
    p <- wilcox.test((rand %>% filter(dbms == "Postgres"))$mutationScore, (dr %>% filter(dbms == "Postgres"))$mutationScore)$p.value <= 0.05

    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(a[i,4]) > as.numeric(a[i,1])) {
        a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
      } else {
        #browser()
        a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
      }
    }

    if (postgres_rand == "large") {
      a[i,4] = paste("$^{\\ast\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "medium") {
      a[i,4] = paste("$^{\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "small") {
      a[i,4] = paste("$^{\\ast}$",a[i,4], sep = "")
    } else {

    }
    # if (postgres_avm == "large" & postgres_rand == "large" & postgres_avmd == "large") {
    #   postgres <- "^{\\ast\\ast\\ast}"
    # } else if (postgres_avm == "medium" & postgres_rand == "medium" & postgres_avmd == "medium") {
    #   postgres <- "^{\\ast\\ast}"
    # } else {
    #   postgres <- "^{\\ast}"
    # }

    #b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("\\textbf{", b[i,], "}", sep = ""), as.numeric(b[i,]))
    sqlite_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$mutationScore, (avm %>% filter(dbms == "SQLite"))$mutationScore)$size
    sqlite_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$mutationScore, (avmd %>% filter(dbms == "SQLite"))$mutationScore)$size
    sqlite_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$mutationScore, (rand %>% filter(dbms == "SQLite"))$mutationScore)$size
    #sqlite <- NULL

    p <- wilcox.test((avm %>% filter(dbms == "SQLite"))$mutationScore, (dr %>% filter(dbms == "SQLite"))$mutationScore)$p.value <= 0.05

    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(b[i,2]) > as.numeric(b[i,1])) {
        b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
      } else {
        b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
      }
    }

    if (sqlite_avm == "large") {
      b[i,2] = paste("$^{\\ast\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "medium") {
      b[i,2] = paste("$^{\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "small") {
      b[i,2] = paste("$^{\\ast}$",b[i,2], sep = "")
    } else {

    }

    p <- wilcox.test((avmd %>% filter(dbms == "SQLite"))$mutationScore, (dr %>% filter(dbms == "SQLite"))$mutationScore)$p.value <= 0.05

    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(b[i,3]) > as.numeric(b[i,1])) {
        b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
      } else {
        b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
      }
    }

    if (sqlite_avmd == "large") {
      b[i,3] = paste("$^{\\ast\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "medium") {
      b[i,3] = paste("$^{\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "small") {
      b[i,3] = paste("$^{\\ast}$",b[i,3], sep = "")
    } else {

    }

    p <- wilcox.test((rand %>% filter(dbms == "SQLite"))$mutationScore, (dr %>% filter(dbms == "SQLite"))$mutationScore)$p.value <= 0.05

    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(b[i,4]) > as.numeric(b[i,1])) {
        b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
      } else {
        b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
      }
    }

    if (sqlite_rand == "large") {
      b[i,4] = paste("$^{\\ast\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "medium") {
      b[i,4] = paste("$^{\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "small") {
      b[i,4] = paste("$^{\\ast}$",b[i,4], sep = "")
    } else {

    }

    # if (sqlite_avm == "large" & sqlite_rand == "large" & sqlite_avmd == "large") {
    #   sqlite <- "^{\\ast\\ast\\ast}"
    # } else if (sqlite_avm == "medium" & sqlite_rand == "medium" & sqlite_avmd == "medium") {
    #   sqlite <- "^{\\ast\\ast}"
    # } else {
    #   sqlite <- "^{\\ast}"
    # }

    hsql_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$mutationScore, (avm %>% filter(dbms == "HyperSQL"))$mutationScore)$size
    hsql_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$mutationScore, (avmd %>% filter(dbms == "HyperSQL"))$mutationScore)$size
    hsql_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$mutationScore, (rand %>% filter(dbms == "HyperSQL"))$mutationScore)$size
    #c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("\\textbf{", c[i,], "}", sep = ""), as.numeric(c[i,]))


    p <- wilcox.test((avm %>% filter(dbms == "HyperSQL"))$mutationScore, (dr %>% filter(dbms == "HyperSQL"))$mutationScore)$p.value <= 0.05

    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(c[i,2]) > as.numeric(c[i,1])) {
        c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
      } else {
        c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
      }
    }

    if (hsql_avm == "large") {
      c[i,2] = paste("$^{\\ast\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "medium") {
      c[i,2] = paste("$^{\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "small") {
      c[i,2] = paste("$^{\\ast}$",c[i,2], sep = "")
    } else {

    }

    p <- wilcox.test((avmd %>% filter(dbms == "HyperSQL"))$mutationScore, (dr %>% filter(dbms == "HyperSQL"))$mutationScore)$p.value <= 0.05

    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(c[i,3]) > as.numeric(c[i,1])) {
        c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
      } else {
        c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
      }
    }

    if (hsql_avmd == "large") {
      c[i,3] = paste("$^{\\ast\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "medium") {
      c[i,3] = paste("$^{\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "small") {
      c[i,3] = paste("$^{\\ast}$",c[i,3], sep = "")
    } else {

    }

    p <- wilcox.test((rand %>% filter(dbms == "HyperSQL"))$mutationScore, (dr %>% filter(dbms == "HyperSQL"))$mutationScore)$p.value <= 0.05

    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(c[i,4]) > as.numeric(c[i,1])) {
        c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
      } else {
        c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
      }
    }

    if (hsql_rand == "large") {
      c[i,4] = paste("$^{\\ast\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "medium") {
      c[i,4] = paste("$^{\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "small") {
      c[i,4] = paste("$^{\\ast}$",c[i,4], sep = "")
    } else {

    }




  }
  a <- a[c(1,3,2,4)]
  b <- b[c(1,3,2,4)]
  c <- c[c(1,3,2,4)]

  d <- cbind(a1,c,a,b)
  #return(d)
  return(print(xtable(d), include.rownames=FALSE ,sanitize.text.function = function(x){x}))
}

analyse_vargha_delaney_effect_size_specify_time_per_schema <- function(d, schema) {

  a1 <- d[1]
  a <- d[2:5]
  b <- d[6:9]
  c <- d[10:13]

  d <- d %>% dplyr::filter(casestudy == schema)
  dbmss <- as.vector(d %>% dplyr::group_by(dbms) %>% dplyr::distinct(dbms))[[1]]

  d <- d %>% dplyr::mutate(testgenerationtime = (testgenerationtime / 1000))

  mean_results <- NULL
  for (db in dbmss) {
    d_dbms <- d %>% dplyr::filter(dbms %in% c(db))

    avm <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("avs"))# %>% dplyr::filter(randomseed == seed)
    avmd <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("avsDefaults"))# %>% dplyr::filter(randomseed == seed)
    diR <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("directedRandom"))# %>% dplyr::filter(randomseed == seed)
    ran <- d %>% dplyr::filter(dbms %in% c(db)) %>% dplyr::filter(datagenerator %in% c("random"))# %>% dplyr::filter(randomseed == seed)

    model <- directedRandomR::effectsize_accurate(diR$testgenerationtime, avm$testgenerationtime)
    results <-as.data.frame(model)
    namevector <- c("dbms")
    results[,namevector] <- db
    namevector <- c("vs")
    results[,namevector] <- "Directed Random vs AVM Test Generation time"
    namevector <- c("control")
    results[,namevector] <- "AVM"
    #namevector <- c("randomseed")
    #results[,namevector] <- seed
    mean_results <- rbind(mean_results, results)

    model <- directedRandomR::effectsize_accurate(diR$testgenerationtime, avmd$testgenerationtime)
    results <-as.data.frame(model)
    namevector <- c("dbms")
    results[,namevector] <- db
    namevector <- c("vs")
    results[,namevector] <- "Directed Random vs AVMD Test Generation time"
    namevector <- c("control")
    results[,namevector] <- "AVMD"
    #namevector <- c("randomseed")
    #results[,namevector] <- seed
    mean_results <- rbind(mean_results, results)

    model <- directedRandomR::effectsize_accurate(diR$testgenerationtime, ran$testgenerationtime)
    results <-as.data.frame(model)
    namevector <- c("dbms")
    results[,namevector] <- db
    namevector <- c("vs")
    results[,namevector] <- "Directed Random vs Random Test Generation time"
    namevector <- c("control")
    results[,namevector] <- "Random"
    #namevector <- c("randomseed")
    #results[,namevector] <- seed
    mean_results <- rbind(mean_results, results)
  }

  return(mean_results)
}


#' FUNCTION: visual_grid_ten_schemas
#'

#' generates a plot grid of top 10 schemas
#' @importFrom magrittr %>%
#' @export
visual_grid_ten_schemas <- function(mutationanalysistiming) {
  library(dplyr)
  library(gridExtra)
  mutationanalysistiming %>% group_by(casestudy, datagenerator, dbms) %>% filter(datagenerator != "random", casestudy != "ArtistSimilarity", casestudy != "ArtistTerm", casestudy != "BankAccount", casestudy != "BookTown", casestudy != "Cloc",casestudy != "CoffeeOrders", casestudy != "DellStore", casestudy != "Employee", casestudy != "Examination", casestudy != "FrenchTowns", casestudy != "Inventory", casestudy != "Iso3166", casestudy != "IsoFlav_R2", casestudy != "JWhoisServer", casestudy != "MozillaExtensions", casestudy != "MozillaPermissions", casestudy != "NistDML181", casestudy != "NistDML183", casestudy != "NistWeather", casestudy != "NistXTS748", casestudy != "NistXTS749", casestudy != "Person", casestudy != "StackOverflow", casestudy != "StudentResidence", casestudy != "Usda", casestudy != "WordNet") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_wrap( ~ casestudy, scales = "free") +  labs(y = "Test Generation Time For all runs per Schema (In Sceconds)")

  a <- mutationanalysistiming %>% group_by(casestudy, datagenerator, dbms) %>% filter(datagenerator != "random", casestudy == "iTrust") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") +  labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  b <- mutationanalysistiming %>% group_by(casestudy, datagenerator, dbms) %>% filter(datagenerator != "random", casestudy == "BrowserCookies") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  c <- mutationanalysistiming %>% group_by(casestudy, datagenerator, dbms) %>% filter(datagenerator != "random", casestudy == "CustomerOrder") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  d <- mutationanalysistiming %>% group_by(casestudy, datagenerator, dbms) %>% filter(datagenerator != "random", casestudy == "Flights") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  e <- mutationanalysistiming %>% group_by(casestudy, datagenerator, dbms) %>% filter(datagenerator != "random", casestudy == "NistDML182") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  f <- mutationanalysistiming %>% group_by(casestudy, datagenerator, dbms) %>% filter(datagenerator != "random", casestudy == "Products") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  g <- mutationanalysistiming %>% group_by(casestudy, datagenerator, dbms) %>% filter(datagenerator != "random", casestudy == "RiskIt") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  h <- mutationanalysistiming %>% group_by(casestudy, datagenerator, dbms) %>% filter(datagenerator != "random", casestudy == "UnixUsage") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))

  grid.arrange(a, b, c, d, e, f, g, h, nrow =1, left="Test Generation Time For all runs per Schema (In Sceconds)", bottom = "Generator")

}

#' FUNCTION: siginificant_mutant_operators_fixed
#'

#' generates a latex table for mutation operators table with effect size and U test.
#' @importFrom magrittr %>%
#' @export
siginificant_mutant_operators_fixed <- function(d, rtrn = "tex", m = "median") {
  # BankAccount is first schema
  # ids <- a %>% filter(schema=='BankAccount') %>% select(identifier,dbms,schema,operator,type) %>% unique
  # ids$number=1:nrow(ids)
  # test
  # a %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test
  library(dplyr)
  library(reshape2)
  library(xtable)
  # d <- generate_15_schmea_mutanttiming(d)
  # #d <- d %>% arrange(schema, identifier, operator)
  # d1 <- d %>% filter(type == "NORMAL")
  # newdata <- d %>% group_by(generator, operator, dbms) %>% filter(type == "NORMAL") %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "false") + sum(killed == "true")))
  # a <- newdata %>% group_by(dbms, generator, operator) %>% summarise(killed_mutants=mean(killed_mutants), total_mutants = mean(total_mutants)) %>% mutate(value = format(round((killed_mutants / total_mutants) * 100, 2), nsmall = 2))
  # a <- a %>% select(dbms, generator, operator, value)
  # d <- dcast(a,  operator ~ dbms + generator)

  d <- ordering_mutants_per_operator(d)
  d1 <- d
  # browser()

  if (m == "mean") {
    a <- d %>% group_by(dbms, generator, operator) %>% summarise(value = format(mean(mutationScore), nsmall = 1))
  } else {
    a <- d %>% group_by(dbms, generator, operator) %>% summarise(value = format(median(mutationScore), nsmall = 1))
  }
  d <- dcast(a,  operator ~ dbms + generator)

  a1 <- d[1]
  d2 <- d[2:13]
  d <- d2[ , order(names(d2))]
  c <- d[1:4]
  c <- c[c(3,1,2,4)]
  a <- d[5:8]
  a <- a[c(3,1,2,4)]
  b <- d[9:12]
  b <- b[c(3,1,2,4)]
  a1$operator <- as.character(a1$operator)

  numberOfRows <- nrow(d)
  for (i in 1:numberOfRows) {
    selected_operator <- a1[i,]

    postgres_dr <- d1 %>% filter(operator == selected_operator, dbms == "Postgres", generator == "directedRandom")
    postgres_avm <- d1 %>% filter(operator == selected_operator, dbms == "Postgres", generator == "avs")
    postgres_avmd <- d1 %>% filter(operator == selected_operator, dbms == "Postgres", generator == "avsDefaults")
    postgres_rand <- d1 %>% filter(operator == selected_operator, dbms == "Postgres", generator == "random")
    sqlite_dr <- d1 %>% filter(operator == selected_operator, dbms == "SQLite", generator == "directedRandom")
    sqlite_avm <- d1 %>% filter(operator == selected_operator, dbms == "SQLite", generator == "avs")
    sqlite_avmd <- d1 %>% filter(operator == selected_operator, dbms == "SQLite", generator == "avsDefaults")
    sqlite_rand <- d1 %>% filter(operator == selected_operator, dbms == "SQLite", generator == "random")
    hsql_dr <- d1 %>% filter(operator == selected_operator, dbms == "HyperSQL", generator == "directedRandom")
    hsql_avm <- d1 %>% filter(operator == selected_operator, dbms == "HyperSQL", generator == "avs")
    hsql_avmd <- d1 %>% filter(operator == selected_operator, dbms == "HyperSQL", generator == "avsDefaults")
    hsql_rand <- d1 %>% filter(operator == selected_operator, dbms == "HyperSQL", generator == "random")

    postgres_avm_effectsize <- directedRandomR::effectsize_accurate(postgres_dr$mutationScore, postgres_avm$mutationScore)$size
    postgres_avmd_effectsize <- directedRandomR::effectsize_accurate(postgres_dr$mutationScore, postgres_avmd$mutationScore)$size
    postgres_rand_effectsize <- directedRandomR::effectsize_accurate(postgres_dr$mutationScore, postgres_rand$mutationScore)$size

    # postgres_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (avm %>% filter(dbms == "Postgres"))$mutationScore)$size
    # postgres_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (avmd %>% filter(dbms == "Postgres"))$mutationScore)$size
    # postgres_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (rand %>% filter(dbms == "Postgres"))$mutationScore)$size
    #if (selected_operator == "UCColumnE") { browser() }
    # p <- wilcox.test(postgres_avm$mutationScore, postgres_dr$mutationScore)$p.value <= 0.05
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,2]) > as.numeric(a[i,1])) {
    #     a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    #   } else {
    #     a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    #   }
    # }

    dr_mutation <- postgres_dr$mutationScore
    avmr_mutation <- postgres_avm$mutationScore

    p1 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema1, " DR YES ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
      a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-R YES ", a[i,2], sep = "")
      # print(oh)
      a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-R NOT ", a[i,2], sep = "")
      # print(oh)
    }

    if (postgres_avm_effectsize == "large") {
      a[i,2] = paste("$^{\\ast\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm_effectsize == "medium") {
      a[i,2] = paste("$^{\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm_effectsize == "small") {
      a[i,2] = paste("$^{\\ast}$",a[i,2], sep = "")
    } else {
    }

    # p <- wilcox.test(postgres_avmd$mutationScore, postgres_dr$mutationScore)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,3]) > as.numeric(a[i,1])) {
    #     a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
    #   } else {
    #     a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
    #   }
    # }

    avmd_mutation <- postgres_avmd$mutationScore

    p1 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "less")$p.value <= 0.01

    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema1, " DR YES ", a[i,1], " AVM-D NOT ", a[i,3], sep = "")
      # print(oh)
      a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-D YES ", a[i,3], sep = "")
      # print(oh)
      a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-D NOT ", a[i,3], sep = "")
      # print(oh)
    }

    if (postgres_avmd_effectsize == "large") {
      a[i,3] = paste("$^{\\ast\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd_effectsize == "medium") {
      a[i,3] = paste("$^{\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd_effectsize == "small") {
      a[i,3] = paste("$^{\\ast}$",a[i,3], sep = "")
    } else {

    }



    #check1 <- (randp %>% filter(dbms == "Postgres"))$mutationScore
    #check2 <- (drp %>% filter(dbms == "Postgres"))$mutationScore
    #browser(condition = (schema == "NistWeather"))
    # p <- wilcox.test(postgres_rand$mutationScore, postgres_dr$mutationScore)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(a[i,4]) > as.numeric(a[i,1])) {
    #     a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
    #   } else {
    #     #browser()
    #     a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
    #   }
    # }

    rand_mutation <- postgres_rand$mutationScore

    p1 <- wilcox.test(dr_mutation, rand_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, rand_mutation, alternative = "less")$p.value <= 0.01

    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Postgers Schema => ", schema1, " DR YES ", a[i,1], " AVM-D NOT ", a[i,4], sep = "")
      # print(oh)
      a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-D YES ", a[i,4], sep = "")
      # print(oh)
      a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
    } else {
      # oh = paste("Postgers Schema => ", schema1, " DR NOT ", a[i,1], " AVM-D NOT ", a[i,4], sep = "")
      # print(oh)
    }


    if (postgres_rand_effectsize == "large") {
      a[i,4] = paste("$^{\\ast\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand_effectsize == "medium") {
      a[i,4] = paste("$^{\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand_effectsize == "small") {
      a[i,4] = paste("$^{\\ast}$",a[i,4], sep = "")
    } else {

    }

    sqlite_avm_effectsize <- directedRandomR::effectsize_accurate(sqlite_dr$mutationScore, sqlite_avm$mutationScore)$size
    sqlite_avmd_effectsize <- directedRandomR::effectsize_accurate(sqlite_dr$mutationScore, sqlite_avmd$mutationScore)$size
    sqlite_rand_effectsize <- directedRandomR::effectsize_accurate(sqlite_dr$mutationScore, sqlite_rand$mutationScore)$size

    # p <- wilcox.test(sqlite_avm$mutationScore, sqlite_dr$mutationScore)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,2]) > as.numeric(b[i,1])) {
    #     b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
    #   } else {
    #     b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
    #   }
    # }

    dr_mutation <- sqlite_dr$mutationScore
    avmr_mutation <- sqlite_avm$mutationScore

    p1 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("SQLite Schema => ", schema1, " DR YES ", b[i,1], " AVM-D NOT ", b[i,2], sep = "")
      # print(oh)
      b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D YES ", b[i,2], sep = "")
      # print(oh)
      b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
    } else {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D NOT ", b[i,2], sep = "")
      # print(oh)
    }

    if (sqlite_avm_effectsize == "large") {
      b[i,2] = paste("$^{\\ast\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm_effectsize == "medium") {
      b[i,2] = paste("$^{\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm_effectsize == "small") {
      b[i,2] = paste("$^{\\ast}$",b[i,2], sep = "")
    } else {

    }

    # p <- wilcox.test(sqlite_avmd$mutationScore, sqlite_dr$mutationScore)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,3]) > as.numeric(b[i,1])) {
    #     b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
    #   } else {
    #     b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
    #   }
    # }

    avmd_mutation <- sqlite_avmd$mutationScore

    p1 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "less")$p.value <= 0.01

    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("SQLite Schema => ", schema1, " DR YES ", b[i,1], " AVM-D NOT ", b[i,3], sep = "")
      # print(oh)
      b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D YES ", b[i,3], sep = "")
      # print(oh)
      b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
    } else {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D NOT ", b[i,3], sep = "")
      # print(oh)
    }

    if (sqlite_avmd_effectsize == "large") {
      b[i,3] = paste("$^{\\ast\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd_effectsize == "medium") {
      b[i,3] = paste("$^{\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd_effectsize == "small") {
      b[i,3] = paste("$^{\\ast}$",b[i,3], sep = "")
    } else {

    }

    # p <- wilcox.test(sqlite_rand$mutationScore, sqlite_dr$mutationScore)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(b[i,4]) > as.numeric(b[i,1])) {
    #     b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
    #   } else {
    #     b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
    #   }
    # }

    rand_mutation <- sqlite_rand$mutationScore

    p1 <- wilcox.test(dr_mutation, rand_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, rand_mutation, alternative = "less")$p.value <= 0.01

    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("SQLite Schema => ", schema1, " DR YES ", b[i,1], " AVM-D NOT ", b[i,3], sep = "")
      # print(oh)
      b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D YES ", b[i,3], sep = "")
      # print(oh)
      b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
    } else {
      # oh = paste("SQLite Schema => ", schema1, " DR NOT ", b[i,1], " AVM-D NOT ", b[i,3], sep = "")
      # print(oh)
    }

    if (sqlite_rand_effectsize == "large") {
      b[i,4] = paste("$^{\\ast\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand_effectsize == "medium") {
      b[i,4] = paste("$^{\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand_effectsize == "small") {
      b[i,4] = paste("$^{\\ast}$",b[i,4], sep = "")
    } else {

    }


    hsql_avm_effectsize <- directedRandomR::effectsize_accurate(hsql_dr$mutationScore, hsql_avm$mutationScore)$size
    hsql_avmd_effectsize <- directedRandomR::effectsize_accurate(hsql_dr$mutationScore, hsql_avmd$mutationScore)$size
    hsql_rand_effectsize <- directedRandomR::effectsize_accurate(hsql_dr$mutationScore, hsql_rand$mutationScore)$size

    # p <- wilcox.test(hsql_avm$mutationScore, hsql_dr$mutationScore)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,2]) > as.numeric(c[i,1])) {
    #     c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
    #   } else {
    #     c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
    #   }
    # }

    dr_mutation <- hsql_dr$mutationScore
    avmr_mutation <- hsql_avm$mutationScore

    p1 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmr_mutation, alternative = "less")$p.value <= 0.01

    #if (isTRUE(p1) == FALSE & isTRUE(p2) == FALSE) {
    #  # oh = paste("Schema => ", schema1, " All False ", sep = "")
    #  # print(oh)
    #} else
    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("HyperSQL Schema => ", schema1, " DR YES ", c[i,1], " AVM-D NOT ", c[i,2], sep = "")
      # print(oh)
      c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D YES ", c[i,2], sep = "")
      # print(oh)
      c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
    } else {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D NOT ", c[i,2], sep = "")
      # print(oh)
    }

    if (hsql_avm_effectsize == "large") {
      c[i,2] = paste("$^{\\ast\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm_effectsize == "medium") {
      c[i,2] = paste("$^{\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm_effectsize == "small") {
      c[i,2] = paste("$^{\\ast}$",c[i,2], sep = "")
    } else {

    }

    # p <- wilcox.test(hsql_avmd$mutationScore, hsql_dr$mutationScore)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,3]) > as.numeric(c[i,1])) {
    #     c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
    #   } else {
    #     c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
    #   }
    # }

    avmd_mutation <- hsql_avmd$mutationScore
    p1 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, avmd_mutation, alternative = "less")$p.value <= 0.01

    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Schema => ", schema1, " DR YES ", c[i,1], " AVM-D NOT ", c[i,3], sep = "")
      # print(oh)
      c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D YES ", c[i,3], sep = "")
      # print(oh)
      c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
    } else {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D NOT ", c[i,3], sep = "")
      # print(oh)
    }

    if (hsql_avmd_effectsize == "large") {
      c[i,3] = paste("$^{\\ast\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd_effectsize == "medium") {
      c[i,3] = paste("$^{\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd_effectsize == "small") {
      c[i,3] = paste("$^{\\ast}$",c[i,3], sep = "")
    } else {

    }

    # p <- wilcox.test(hsql_rand$mutationScore, hsql_dr$mutationScore)$p.value <= 0.05
    #
    # if (isTRUE(p) == FALSE) {
    #
    # } else if (p) {
    #   if (as.numeric(c[i,4]) > as.numeric(c[i,1])) {
    #     c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
    #   } else {
    #     c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
    #   }
    # }

    rand_mutation <- hsql_rand$mutationScore

    p1 <- wilcox.test(dr_mutation, rand_mutation, alternative = "greater")$p.value <= 0.01
    p2 <- wilcox.test(dr_mutation, rand_mutation, alternative = "less")$p.value <= 0.01

    if (p1 == TRUE & p2 == FALSE) {
      # oh = paste("Schema => ", schema1, " DR YES ", c[i,1], " AVM-D NOT ", c[i,4], sep = "")
      # print(oh)
      c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
    } else if (p1 == FALSE & p2 == TRUE) {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D YES ", c[i,4], sep = "")
      # print(oh)
      c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
    } else {
      # oh = paste("Schema => ", schema1, " DR NOT ", c[i,1], " AVM-D NOT ", c[i,4], sep = "")
      # print(oh)
    }

    if (hsql_rand_effectsize == "large") {
      c[i,4] = paste("$^{\\ast\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand_effectsize == "medium") {
      c[i,4] = paste("$^{\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand_effectsize == "small") {
      c[i,4] = paste("$^{\\ast}$",c[i,4], sep = "")
    } else {

    }


    a1[i,] <- paste("\\", a1[i,], sep = "")


  }
  a <- a[c(1,4,3,2)]
  b <- b[c(1,4,3,2)]
  c <- c[c(1,4,3,2)]

  # With HSQL
  #d <- cbind(a1,c,a,b)
  # Without HSQL
  d <- cbind(a1,a,b)
  #return(d)
  #return(print(xtable(d), include.rownames=FALSE ,sanitize.text.function = function(x){x}))

  if (rtrn == "tex") {
    return(print(xtable(d), include.rownames=FALSE ,sanitize.text.function = function(x){x}))
  } else {
    return(d)
  }
}

#' FUNCTION: ordering_mutants_per_schema
#'

#' generates a a data frame ready with runs for mutant timing file per schema.
#' @importFrom magrittr %>%
#' @export
ordering_mutants_per_schema <- function(d) {
  library(dplyr)
  library(reshape2)
  # d <- generate_15_schmea_mutanttiming(d)
  d1 <- d %>% filter(type == "NORMAL")# %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

  dt <- NULL

  casestudy <- as.vector(dplyr::distinct(d1, schema))[[1]]
  dbs <- as.vector(distinct(d1, dbms))[[1]]
  for (case in casestudy) {
    schema1 <- case
    for (db in dbs) {
      #print(schema1)
      #print(db)
      #if (schema1 == "iTrust" & db == "SQLite") { browser() }
      filtered_data <- d1 %>% filter(schema == schema1, dbms == db) %>% group_by(identifier, dbms)
      first_schema <- filtered_data[1,3]

      test <- NULL

      #itrust_mutants <- d1 %>% filter(schema == "iTrust", schema == schema1, dbms == db)
      #filtered_data <- filtered_data %>% filter(dbms == "SQLite")
      ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "directedRandom") %>% select(identifier,dbms,schema) %>% unique
      ids$number=1:nrow(ids)
      filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test

      dr_minsitrust <- test %>% filter(generator == "directedRandom") %>% group_by(identifier, dbms, generator, number, schema) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      dr <- dr_minsitrust
      dr <- dr %>% group_by(number, generator, dbms, schema) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avs") %>% select(identifier,dbms,schema) %>% unique
      ids$number=1:nrow(ids)
      filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test

      avs_minsitrust <- test %>% filter(generator == "avs") %>% group_by(identifier, dbms, generator, number, schema) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      avm <- avs_minsitrust
      avm <- avm %>% group_by(number, generator, dbms, schema) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avsDefaults") %>% select(identifier,dbms,schema) %>% unique
      ids$number=1:nrow(ids)
      filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test


      avsd_minsitrust <- test %>% filter(generator == "avsDefaults") %>% group_by(identifier, dbms, generator, number, schema) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      avmd <- avsd_minsitrust
      avmd <- avmd %>% group_by(number, generator, dbms, schema) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "random") %>% select(identifier,dbms,schema) %>% unique
      ids$number=1:nrow(ids)
      filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test

      ran_minsitrust <- test %>% filter(generator == "random") %>% group_by(identifier, dbms, generator, number, schema) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      rand <- ran_minsitrust
      rand <- rand %>% group_by(number, generator, dbms, schema) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      if (db == "Postgres") {
        postgres_dr <- dr
        postgres_avm <- avm
        postgres_avmd <- avmd
        postgres_rand <- rand
      } else if (db == "SQLite") {
        sqlite_dr <- dr
        sqlite_avm <- avm
        sqlite_avmd <- avmd
        sqlite_rand <- rand
      } else if (db == "HyperSQL") {
        hsql_dr <- dr
        hsql_avm <- avm
        hsql_avmd <- avmd
        hsql_rand <- rand
      }

    }

    postgres_dr <- arrange(postgres_dr, number)
    postgres_avm <- arrange(postgres_avm, number)
    postgres_avmd <- arrange(postgres_avmd, number)
    postgres_rand <- arrange(postgres_rand, number)
    sqlite_dr <- arrange(sqlite_dr, number)
    sqlite_avm <- arrange(sqlite_avm, number)
    sqlite_avmd <- arrange(sqlite_avmd, number)
    sqlite_rand <- arrange(sqlite_rand, number)
    hsql_dr <- arrange(hsql_dr, number)
    hsql_avm <- arrange(hsql_avm, number)
    hsql_avmd <- arrange(hsql_avmd, number)
    hsql_rand <- arrange(hsql_rand, number)

    postgres <- rbind(postgres_dr, postgres_avm, postgres_avmd, postgres_rand)
    sqlite <- rbind(sqlite_dr, sqlite_avm, sqlite_avmd, sqlite_rand)
    hsql <- rbind(hsql_dr, hsql_avm, hsql_avmd, hsql_rand)

    dt <- rbind(dt, postgres, sqlite, hsql)
  }

  #browser()

  return(dt)

  #a <- dt %>% select(dbms, generator, schema, value)
  #d <- dcast(a, schema ~ dbms + generator)
}

#' FUNCTION: ordering_mutants_per_operator
#'

#' generates a a data frame ready with runs for mutant timing file per operator
#' @importFrom magrittr %>%
#' @export
ordering_mutants_per_operator <- function(d) {
  library(dplyr)
  library(reshape2)
  # d <- generate_15_schmea_mutanttiming(d)
  d1 <- d %>% filter(type == "NORMAL")# %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

  dt <- NULL

  dbs <- as.vector(distinct(d1, dbms))[[1]]
  operators <- as.vector(distinct(d1, operator))[[1]]
  for (selected_operator in operators) {
    for (db in dbs) {
      #print(selected_operator)
      #print(db)
      filtered_data <- d1 %>% filter(operator == selected_operator, schema != "iTrust", dbms == db) %>% group_by(identifier, dbms)
      first_schema <- filtered_data[1,3]

      test <- NULL

      #itrust_mutants <- d1 %>% filter(schema == "iTrust", operator == selected_operator, dbms == db)
      #filtered_data <- filtered_data %>% filter(dbms == "SQLite")

      ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "directedRandom") %>% select(identifier,dbms,schema,operator,type) %>% unique
      ids$number=1:nrow(ids)
      filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test

      dr_minsitrust <- test %>% filter(generator == "directedRandom") %>% group_by(identifier, dbms, generator, number, operator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      dr_itrust <- d1 %>% filter(schema == "iTrust", generator == "directedRandom", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator, operator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      if (nrow(dr_itrust) > 0) {
        dr_itrust$number=1:nrow(dr_itrust)
      }

      dr <- rbind(dr_minsitrust, dr_itrust)
      dr <- dr %>% group_by(number, generator, dbms, operator) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avs") %>% select(identifier,dbms,schema,operator,type) %>% unique
      ids$number=1:nrow(ids)
      filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test

      avs_minsitrust <- test %>% filter(generator == "avs") %>% group_by(identifier, dbms, generator, number, operator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      avs_itrust <- d1 %>% filter(schema == "iTrust", generator == "avs", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator, operator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      if (nrow(avs_itrust) > 0) {
        avs_itrust$number=1:nrow(avs_itrust)
      }
      avm <- rbind(avs_minsitrust, avs_itrust)
      avm <- avm %>% group_by(number, generator, dbms, operator) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avsDefaults") %>% select(identifier,dbms,schema,operator,type) %>% unique
      ids$number=1:nrow(ids)
      filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test


      avsd_minsitrust <- test %>% filter(generator == "avsDefaults") %>% group_by(identifier, dbms, generator, number, operator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      avsd_itrust <- d1 %>% filter(schema == "iTrust", generator == "avsDefaults", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator, operator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      if (nrow(avsd_itrust) > 0) {
        avsd_itrust$number=1:nrow(avsd_itrust)
      }
      avmd <- rbind(avsd_minsitrust, avsd_itrust)
      avmd <- avmd %>% group_by(number, generator, dbms, operator) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "random") %>% select(identifier,dbms,schema,operator,type) %>% unique
      ids$number=1:nrow(ids)
      filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test

      ran_minsitrust <- test %>% filter(generator == "random") %>% group_by(identifier, dbms, generator, number, operator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      ran_itrust <- d1 %>% filter(schema == "iTrust", generator == "random", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator, operator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false")))# %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
      if (nrow(ran_itrust) > 0) {
        ran_itrust$number=1:nrow(avsd_itrust)
      }

      rand <- rbind(ran_minsitrust, ran_itrust)
      rand <- rand %>% group_by(number, generator, dbms, operator) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

      if (db == "Postgres") {
        postgres_dr <- dr
        postgres_avm <- avm
        postgres_avmd <- avmd
        postgres_rand <- rand
      } else if (db == "SQLite") {
        sqlite_dr <- dr
        sqlite_avm <- avm
        sqlite_avmd <- avmd
        sqlite_rand <- rand
      } else if (db == "HyperSQL") {
        hsql_dr <- dr
        hsql_avm <- avm
        hsql_avmd <- avmd
        hsql_rand <- rand
      }

    }

    postgres_dr <- arrange(postgres_dr, number)
    postgres_avm <- arrange(postgres_avm, number)
    postgres_avmd <- arrange(postgres_avmd, number)
    postgres_rand <- arrange(postgres_rand, number)
    sqlite_dr <- arrange(sqlite_dr, number)
    sqlite_avm <- arrange(sqlite_avm, number)
    sqlite_avmd <- arrange(sqlite_avmd, number)
    sqlite_rand <- arrange(sqlite_rand, number)
    hsql_dr <- arrange(hsql_dr, number)
    hsql_avm <- arrange(hsql_avm, number)
    hsql_avmd <- arrange(hsql_avmd, number)
    hsql_rand <- arrange(hsql_rand, number)

    postgres <- rbind(postgres_dr, postgres_avm, postgres_avmd, postgres_rand)
    sqlite <- rbind(sqlite_dr, sqlite_avm, sqlite_avmd, sqlite_rand)
    hsql <- rbind(hsql_dr, hsql_avm, hsql_avmd, hsql_rand)

    dt <- rbind(dt, postgres, sqlite, hsql)
  }

  #browser()

  return(dt)
}
