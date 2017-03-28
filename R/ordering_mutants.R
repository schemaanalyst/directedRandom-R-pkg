generate_15_schmea_mutanttiming <- function(d) {
  library(kimisc)
  library(dplyr)
  set.seed(1)
  #browser()
  cases <- distinct(d, schema)
  a <- sample.rows(cases %>% filter(schema != "iTrust", schema != "Products"), 13)
  d_rest <- d %>% filter(schema %in% a)
  d_itrust <- d %>% filter(schema == "iTrust")
  d_products <- d %>% filter(schema == "Products")

  d <- rbind(d_rest, d_products, d_itrust)

  return(d)
}


ordering_mutants_per_schema <- function(d) {
  library(dplyr)
  library(reshape2)
  d <- generate_15_schmea_mutanttiming(d)
  d1 <- d %>% filter(type == "NORMAL")# %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

  dt <- NULL

  casestudy <- as.vector(dplyr::distinct(d1, schema))[[1]]
  dbs <- as.vector(distinct(d1, dbms))[[1]]
  for (case in casestudy) {
    schema1 <- case
    for (db in dbs) {
      print(schema1)
      print(db)
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



ordering_mutants_per_operator <- function(d) {
  library(dplyr)
  library(reshape2)
  d <- generate_15_schmea_mutanttiming(d)
  d1 <- d %>% filter(type == "NORMAL")# %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

  dt <- NULL

  dbs <- as.vector(distinct(d1, dbms))[[1]]
  operators <- as.vector(distinct(d1, operator))[[1]]
  for (selected_operator in operators) {
    for (db in dbs) {
      print(selected_operator)
      print(db)
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


siginificant_mutation_score2 <- function(d) {
  library(dplyr)
  library(reshape2)
  library(xtable)
  #d <- generate_15_schmea_mutanttiming(d)
  #d <- d %>% arrange(schema, identifier, operator)

  d <- ordering_mutants_per_schema(d)

  #d1 <- directedRandomR::transform_execution_times_for_threshold(d, 1000)
  #d1 <- d %>% select(dbms, casestudy, datagenerator, scorenumerator, scoredenominator,randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% mutate(mutationScore = scorenumerator/scoredenominator)#mutate(mutationScore = round((sum(scorenumerator)/sum(scoredenominator)) * 100, 2)) #summarise(scorenumerator = (sum(scorenumerator)), scoredenominator = (sum(scoredenominator)))
  #d <- d %>% select(dbms, casestudy, datagenerator, scorenumerator, scoredenominator,randomseed) %>% group_by(dbms, casestudy, datagenerator) %>% summarise(mutationScore = round((sum(scorenumerator)/sum(scoredenominator)) * 100, 2))#mutate(mutationScore = sum(scorenumerator)/sum(scoredenominator)) #summarise(scorenumerator = (sum(scorenumerator)), scoredenominator = (sum(scoredenominator)))
  #d1 <- d %>% filter(type == "NORMAL") %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  d1 <- d
  d <- d %>% group_by(schema, generator, dbms)  %>% summarise(mutationScore = median(mutationScore))
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
  numberOfRows <- nrow(d)
  for (i in 1:numberOfRows) {
    browser()
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

    p <- wilcox.test((avmp %>% filter(dbms == "Postgres"))$mutationScore, (drp %>% filter(dbms == "Postgres"))$mutationScore)$p.value <= 0.05
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
      a[i,2] = paste("$",a[i,2],"$", sep = "")
    }
  }

  return(d)
}


siginificant_mutant_operators_fixed2 <- function(d) {
  # BankAccount is first schema
  # ids <- a %>% filter(schema=='BankAccount') %>% select(identifier,dbms,schema,operator,type) %>% unique
  # ids$number=1:nrow(ids)
  # test
  # a %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test
  library(dplyr)
  library(reshape2)
  library(xtable)
  #d <- generate_15_schmea_mutanttiming(d)
  #d <- d %>% arrange(schema, identifier, operator)
  #d1 <- d %>% filter(type == "NORMAL")

  d <- ordering_mutants_per_operator(d)
  d1 <- d
  browser()

  # newdata <- d %>% group_by(generator, operator, dbms) %>% filter(type == "NORMAL") %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "false") + sum(killed == "true")))
  a <- d %>% group_by(dbms, generator, operator) %>% summarise(value = format(median(mutationScore), nsmall = 2))
  # a <- a %>% select(dbms, generator, operator, value)
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

    # dr <- filtered_data %>% filter(generator == "directedRandom")
    # avm <- filtered_data %>% filter(generator == "avs")
    # avmd <- filtered_data %>% filter(generator == "avsDefaults")
    # rand <- filtered_data %>% filter(generator == "random")
    #a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))

    postgres_avm_effectsize <- directedRandomR::effectsize_accurate(postgres_dr$mutationScore, postgres_avm$mutationScore)$size
    postgres_avmd_effectsize <- directedRandomR::effectsize_accurate(postgres_dr$mutationScore, postgres_avmd$mutationScore)$size
    postgres_rand_effectsize <- directedRandomR::effectsize_accurate(postgres_dr$mutationScore, postgres_rand$mutationScore)$size

    # postgres_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (avm %>% filter(dbms == "Postgres"))$mutationScore)$size
    # postgres_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (avmd %>% filter(dbms == "Postgres"))$mutationScore)$size
    # postgres_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$mutationScore, (rand %>% filter(dbms == "Postgres"))$mutationScore)$size
    if (selected_operator == "UCColumnE") {
      browser()
    }
    p <- wilcox.test(postgres_avm$mutationScore, postgres_dr$mutationScore)$p.value <= 0.05
    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(a[i,2]) > as.numeric(a[i,1])) {
        a[i,2] = paste("\\textit{",a[i,2],"}", sep = "")
      } else {
        a[i,2] = paste("\\textbf{",a[i,2],"}", sep = "")
      }
    }

    if (postgres_avm_effectsize == "large") {
      a[i,2] = paste("$^{\\ast\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm_effectsize == "medium") {
      a[i,2] = paste("$^{\\ast\\ast}$",a[i,2], sep = "")
    } else if (postgres_avm_effectsize == "small") {
      a[i,2] = paste("$^{\\ast}$",a[i,2], sep = "")
    } else {
    }

    p <- wilcox.test(postgres_avmd$mutationScore, postgres_dr$mutationScore)$p.value <= 0.05

    if (isTRUE(p) == FALSE) {

    } else if (p) {
      if (as.numeric(a[i,3]) > as.numeric(a[i,1])) {
        a[i,3] = paste("\\textit{",a[i,3],"}", sep = "")
      } else {
        a[i,3] = paste("\\textbf{",a[i,3],"}", sep = "")
      }
    }

    if (postgres_avmd_effectsize == "large") {
      a[i,3] = paste("$^{\\ast\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd_effectsize == "medium") {
      a[i,3] = paste("$^{\\ast\\ast}$",a[i,3], sep = "")
    } else if (postgres_avmd_effectsize == "small") {
      a[i,3] = paste("$^{\\ast}$",a[i,3], sep = "")
    } else {

    }
  }
}
