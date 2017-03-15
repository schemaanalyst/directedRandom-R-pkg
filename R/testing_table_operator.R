atest <- function() {
  mutants <- directedRandomR::collect_mutanttiming()
mutants <- mutants %>% filter(type == "NORMAL")
# filtered_data <- mutants %>% filter(operator == "CCInExpressionRHSListExpressionElementR", schema != "iTrust") %>% group_by(identifier, dbms)
# first_schema <- filtered_data[1,3]
#
# test
#
# itrust_mutants <- mutants %>% filter(schema == "iTrust", operator == "CCInExpressionRHSListExpressionElementR", dbms == "SQLite")
# filtered_data <- filtered_data %>% filter(dbms == "SQLite")
#
# ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "directedRandom") %>% select(identifier,dbms,schema,operator,type) %>% unique
# ids$number=1:nrow(ids)
# filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test
#
#
# dr_minsitrust <- test %>% filter(generator == "directedRandom") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
# dr_itrust <- mutants %>% filter(schema == "iTrust", generator == "directedRandom", type == "NORMAL", dbms == "SQLite", operator == "CCInExpressionRHSListExpressionElementR") %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
# dr_itrust$number=1:nrow(dr_itrust)
#
# dr <- rbind(dr_minsitrust, dr_itrust)
#
# ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avs") %>% select(identifier,dbms,schema,operator,type) %>% unique
# ids$number=1:nrow(ids)
# filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test
#
# avs_minsitrust <- test %>% filter(generator == "avs") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
# avs_itrust <- mutants %>% filter(schema == "iTrust", generator == "avs", type == "NORMAL", dbms == "SQLite", operator == "CCInExpressionRHSListExpressionElementR") %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
# avs_itrust$number=1:nrow(avs_itrust)
#
# avs <- rbind(avs_minsitrust, avs_itrust)
#
# ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avsDefaults") %>% select(identifier,dbms,schema,operator,type) %>% unique
# ids$number=1:nrow(ids)
# filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test
#
#
# avsd_minsitrust <- test %>% filter(generator == "avsDefaults") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
# avsd_itrust <- mutants %>% filter(schema == "iTrust", generator == "avsDefaults", type == "NORMAL", dbms == "SQLite", operator == "CCInExpressionRHSListExpressionElementR") %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
# avsd_itrust$number=1:nrow(avsd_itrust)
#
# avsd <- rbind(avsd_minsitrust, avsd_itrust)

# itrust_mutants <- mutants %>% filter(schema == "iTrust", generator == "directedRandom", type == "NORMAL", dbms == "SQLite", operator == "FKCColumnPairE")
#itrust_mutants <- mutants %>% filter(schema == "iTrust", operator == "CCInExpressionRHSListExpressionElementR") %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))

postgres_dr <- NULL
postgres_avm <- NULL
postgres_avmd <- NULL
postgres_rand <- NULL

hsql_dr <- NULL
hsql_avm <- NULL
hsql_avmd <- NULL
hsql_rand <- NULL

sqlite_dr <- NULL
sqlite_avm <- NULL
sqlite_avmd <- NULL
sqlite_rand <- NULL
browser()

dbs <- as.vector(distinct(mutants, dbms))[[1]]
d1 <- mutants
selected_operator <- "CCInExpressionRHSListExpressionElementR"

for (db in dbs) {
    filtered_data <- d1 %>% filter(operator == selected_operator, schema != "iTrust", dbms == db) %>% group_by(identifier, dbms)
    first_schema <- filtered_data[1,3]

    test <- NULL

    #itrust_mutants <- d1 %>% filter(schema == "iTrust", operator == selected_operator, dbms == db)
    #filtered_data <- filtered_data %>% filter(dbms == "SQLite")

    ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "directedRandom") %>% select(identifier,dbms,schema,operator,type) %>% unique
    ids$number=1:nrow(ids)
    filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test


    dr_minsitrust <- test %>% filter(generator == "directedRandom") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
    dr_itrust <- d1 %>% filter(schema == "iTrust", generator == "directedRandom", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
    dr_itrust$number=1:nrow(dr_itrust)

    dr <- rbind(dr_minsitrust, dr_itrust)

    ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avs") %>% select(identifier,dbms,schema,operator,type) %>% unique
    ids$number=1:nrow(ids)
    filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test

    avs_minsitrust <- test %>% filter(generator == "avs") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
    avs_itrust <- d1 %>% filter(schema == "iTrust", generator == "avs", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
    avs_itrust$number=1:nrow(avs_itrust)

    avm <- rbind(avs_minsitrust, avs_itrust)

    ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avsDefaults") %>% select(identifier,dbms,schema,operator,type) %>% unique
    ids$number=1:nrow(ids)
    filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test


    avsd_minsitrust <- test %>% filter(generator == "avsDefaults") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
    avsd_itrust <- d1 %>% filter(schema == "iTrust", generator == "avsDefaults", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
    avsd_itrust$number=1:nrow(avsd_itrust)

    avmd <- rbind(avsd_minsitrust, avsd_itrust)

    ran_minsitrust <- test %>% filter(generator == "random") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
    ran_itrust <- d1 %>% filter(schema == "iTrust", generator == "random", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
    ran_itrust$number=1:nrow(avsd_itrust)

    rand <- rbind(ran_minsitrust, ran_itrust)

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



}
