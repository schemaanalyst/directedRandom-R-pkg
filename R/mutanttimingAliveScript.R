# dbmss <- as.vector(dplyr::distinct(d, dbms))[[1]]
# cases <- as.vector(dplyr::distinct(d, schema))[[1]]
# generators <- as.vector(dplyr::distinct(d, generator))[[1]]
#
# for (db in dbmss) {
#   for (gen in generators)
#   for (case in cases) {
#     data <- d %>% filter(dbms == db, schema == case, generator != "random")
#     caseStudyFileName <- paste(case, "-" , db,".dat", sep="")
#     fileSaving <- sprintf("mutanttiming/%s",caseStudyFileName)
#     write.csv(data, file = fileSaving)
#
#   }
# }
#
# for (db in dbmss) {
#   for (gen in generators) {
#     if (gen != "random") {
#       data <- d %>% filter(dbms == db, generator == gen)
#       caseStudyFileName <- paste(gen, "-" , db,".dat", sep="")
#       fileSaving <- sprintf("mutanttiming-generators/%s",caseStudyFileName)
#       write.csv(data, file = fileSaving)
#     }
#   }
# }
