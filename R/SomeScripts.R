

siginificant <- function(d) {
  library(dplyr)
  library(reshape2)
  library()
  d <- d %>% select(dbms, casestudy, datagenerator, testgenerationtime, randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% summarise(testgenerationtime = round((mean(testgenerationtime) / 1000), 2))
  d <- dcast(d, casestudy ~ dbms + datagenerator)
  a1 <- d[1]
  a <- d[2:5]
  b <- d[6:9]
  c <- d[10:13]
  numberOfRows <- nrow(d)
  for (i in 1:numberOfRows) {
    a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("\textbf{", a[i,], "}"), as.numeric(a[i,]))
    b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("\textbf{", b[i,], "}"), as.numeric(b[i,]))
    c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("\textbf{", c[i,], "}"), as.numeric(c[i,]))
  }
  d <- cbind(a1,a,b,c)
  return(d)
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
    #namevector <- c("randomseed")
    #results[,namevector] <- seed
    mean_results <- rbind(mean_results, results)

    model <- directedRandomR::effectsize_accurate(diR$testgenerationtime, avmd$testgenerationtime)
    results <-as.data.frame(model)
    namevector <- c("dbms")
    results[,namevector] <- db
    namevector <- c("vs")
    results[,namevector] <- "Directed Random vs AVMD Test Generation time"
    #namevector <- c("randomseed")
    #results[,namevector] <- seed
    mean_results <- rbind(mean_results, results)

    model <- directedRandomR::effectsize_accurate(diR$testgenerationtime, ran$testgenerationtime)
    results <-as.data.frame(model)
    namevector <- c("dbms")
    results[,namevector] <- db
    namevector <- c("vs")
    results[,namevector] <- "Directed Random vs Random Test Generation time"
    #namevector <- c("randomseed")
    #results[,namevector] <- seed
    mean_results <- rbind(mean_results, results)
  }

  return(mean_results)
}
