

siginificant <- function(d) {
  library(dplyr)
  library(reshape2)
  library(xtable)
  d1 <- directedRandomR::transform_execution_times_for_threshold(d, 100)
  d <- d %>% select(dbms, casestudy, datagenerator, testgenerationtime, randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% summarise(testgenerationtime = round((mean(testgenerationtime) / 1000), 2))
  d <- dcast(d, casestudy ~ dbms + datagenerator)
  a1 <- d[1]
  a <- d[2:5]
  b <- d[6:9]
  c <- d[10:13]
  numberOfRows <- nrow(d)
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

    if (postgres_avm == "large") {
      a[i,2] = paste("$^{\\ast\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "medium") {
      a[i,2] = paste("$^{\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm == "small") {
      a[i,2] = paste("$^{\\ast}$",a[i,2], sep = "")
    } else {

    }

    p <- wilcox.test((avm %>% filter(dbms == "Postgres"))$testgenerationtime, (dr %>% filter(dbms == "Postgres"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    } else {
      a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    }

    p <- wilcox.test((avm %>% filter(dbms == "Postgres"))$testgenerationtime, (dr %>% filter(dbms == "Postgres"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
    } else {
      a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
    }

    if (postgres_avmd == "large") {
      a[i,3] = paste("$^{\\ast\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "medium") {
      a[i,3] = paste("$^{\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd == "small") {
      a[i,3] = paste("$^{\\ast}$",a[i,3], sep = "")
    } else {

    }

    p <- wilcox.test((avmd %>% filter(dbms == "Postgres"))$testgenerationtime, (dr %>% filter(dbms == "Postgres"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
    } else {
      a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
    }

    if (postgres_rand == "large") {
      a[i,4] = paste("$^{\\ast\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "medium") {
      a[i,4] = paste("$^{\\ast\\ast}$",a[i,4], sep = "")
    } else if (postgres_rand == "small") {
      a[i,4] = paste("$^{\\ast}$",a[i,4], sep = "")
    } else {

    }

    p <- wilcox.test((rand %>% filter(dbms == "Postgres"))$testgenerationtime, (dr %>% filter(dbms == "Postgres"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      a[i,4] = paste("\\textbf{",a[i,4],"}", sep = "")
    } else {
      a[i,4] = paste("\\textit{",a[i,4],"}", sep = "")
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

    if (sqlite_avm == "large") {
      b[i,2] = paste("$^{\\ast\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "medium") {
      b[i,2] = paste("$^{\\ast\\ast}$",b[i,2], sep = "")
    } else if (sqlite_avm == "small") {
      b[i,2] = paste("$^{\\ast}$",b[i,2], sep = "")
    } else {

    }

    p <- wilcox.test((avm %>% filter(dbms == "SQLite"))$testgenerationtime, (dr %>% filter(dbms == "SQLite"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      b[i,2] = paste("\\textbf{",b[i,2],"}", sep = "")
    } else {
      b[i,2] = paste("\\textit{",b[i,2],"}", sep = "")
    }

    if (sqlite_avmd == "large") {
      b[i,3] = paste("$^{\\ast\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "medium") {
      b[i,3] = paste("$^{\\ast\\ast}$",b[i,3], sep = "")
    } else if (sqlite_avmd == "small") {
      b[i,3] = paste("$^{\\ast}$",b[i,3], sep = "")
    } else {

    }

    p <- wilcox.test((avmd %>% filter(dbms == "SQLite"))$testgenerationtime, (dr %>% filter(dbms == "SQLite"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      b[i,3] = paste("\\textbf{",b[i,3],"}", sep = "")
    } else {
      b[i,3] = paste("\\textit{",b[i,3],"}", sep = "")
    }

    if (sqlite_rand == "large") {
      b[i,4] = paste("$^{\\ast\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "medium") {
      b[i,4] = paste("$^{\\ast\\ast}$",b[i,4], sep = "")
    } else if (sqlite_rand == "small") {
      b[i,4] = paste("$^{\\ast}$",b[i,4], sep = "")
    } else {

    }

    p <- wilcox.test((rand %>% filter(dbms == "SQLite"))$testgenerationtime, (dr %>% filter(dbms == "SQLite"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      b[i,4] = paste("\\textbf{",b[i,4],"}", sep = "")
    } else {
      b[i,4] = paste("\\textit{",b[i,4],"}", sep = "")
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

    if (hsql_avm == "large") {
      c[i,2] = paste("$^{\\ast\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "medium") {
      c[i,2] = paste("$^{\\ast\\ast}$",c[i,2], sep = "")
    } else if (hsql_avm == "small") {
      c[i,2] = paste("$^{\\ast}$",c[i,2], sep = "")
    } else {

    }

    p <- wilcox.test((avm %>% filter(dbms == "HyperSQL"))$testgenerationtime, (dr %>% filter(dbms == "HyperSQL"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      c[i,2] = paste("\\textbf{",c[i,2],"}", sep = "")
    } else {
      c[i,2] = paste("\\textit{",c[i,2],"}", sep = "")
    }

    if (hsql_avmd == "large") {
      c[i,3] = paste("$^{\\ast\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "medium") {
      c[i,3] = paste("$^{\\ast\\ast}$",c[i,3], sep = "")
    } else if (hsql_avmd == "small") {
      c[i,3] = paste("$^{\\ast}$",c[i,3], sep = "")
    } else {

    }


    p <- wilcox.test((avmd %>% filter(dbms == "HyperSQL"))$testgenerationtime, (dr %>% filter(dbms == "HyperSQL"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      c[i,3] = paste("\\textbf{",c[i,3],"}", sep = "")
    } else {
      c[i,3] = paste("\\textit{",c[i,3],"}", sep = "")
    }

    if (hsql_rand == "large") {
      c[i,4] = paste("$^{\\ast\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "medium") {
      c[i,4] = paste("$^{\\ast\\ast}$",c[i,4], sep = "")
    } else if (hsql_rand == "small") {
      c[i,4] = paste("$^{\\ast}$",c[i,4], sep = "")
    } else {

    }
    p <- wilcox.test((rand %>% filter(dbms == "HyperSQL"))$testgenerationtime, (dr %>% filter(dbms == "HyperSQL"))$testgenerationtime)$p.value <= 0.05

    if (p) {
      c[i,4] = paste("\\textbf{",c[i,4],"}", sep = "")
    } else {
      c[i,4] = paste("\\textit{",c[i,4],"}", sep = "")
    }
    # hsql <- NULL
    # if (hsql_avm == "large" & hsql_rand == "large" & hsql_avmd == "large") {
    #   hsql <- "^{\\ast\\ast\\ast}"
    # } else if (hsql_avm == "medium" & hsql_rand == "medium" & hsql_avmd == "medium") {
    #   hsql <- "^{\\ast\\ast}"
    # } else {
    #   hsql <- "^{\\ast}"
    # }

    #a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("$",postgres,"$","\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))
    #b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("$",sqlite,"$","\\textbf{", b[i,], "}", sep = ""), as.numeric(b[i,]))
    #c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("$",hsql,"$","\\textbf{", c[i,], "}", sep = ""), as.numeric(c[i,]))
  }
  d <- cbind(a1,a,b,c)
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
