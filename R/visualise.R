

#' FUNCTION: plot_overall
#'

#' Perform a plotting for coverage, mutation score, evaluations and testgenerationtimeGraph
#' @export

plot_overall <- function(d) {
  library(ggplot2)
  library(dplyr)
  # Reodering of data generators
  d$datagenerators_f = factor(d$datagenerator, levels=c('avsDefaults', 'directedRandom', 'random'))

  # Mutation Score Graph
  mutationscoreGraph <- d %>% mutate(realcasestudy = as.character(gsub("parsedcasestudy.","",realcasestudy))) %>% group_by(dbms, datagenerators_f, realcasestudy) #%>% summarise(mutationscore = mean((scorenumerator/scoredenominator) * 100))
  mutationscoreGraphPlot <- mutationscoreGraph %>%  filter(datagenerators_f != "random") %>% ggplot(aes(datagenerators_f, ((scorenumerator/scoredenominator) * 100))) + geom_boxplot() + facet_grid(dbms ~ realcasestudy) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "Mutation Score (%)")

  png(filename = "plots/mutationscoreGraph.png", width = 1920, height = 1080, units = "px")
  plot(mutationscoreGraphPlot)
  dev.off()

  # Coverage Graph
  coverageGraph <- d %>% mutate(realcasestudy = as.character(gsub("parsedcasestudy.","",realcasestudy))) %>% group_by(dbms, datagenerators_f, realcasestudy)# %>% summarise(averageCoverage = mean(coverage))
  coverageGraphPlot <- coverageGraph %>% filter(datagenerators_f != "random") %>% ggplot(aes(datagenerators_f, coverage)) + geom_boxplot(aes(fill = datagenerators_f)) + facet_grid(dbms ~ realcasestudy) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "Coverage (%)")

  png(filename = "plots/coverageGraph.png", width = 1920, height = 1080, units = "px")
  plot(coverageGraphPlot)
  dev.off()

  # evaluations Graph
  evaluationsGraph <- d %>% mutate(realcasestudy = as.character(gsub("parsedcasestudy.","",realcasestudy))) %>% group_by(dbms, datagenerators_f, realcasestudy)# %>% summarise(evaluations = mean(evaluations))
  evaluationsGraphPlot <- evaluationsGraph %>%  filter(datagenerators_f != "random") %>% ggplot(aes(datagenerators_f, evaluations)) + geom_boxplot() + facet_grid(dbms ~ realcasestudy) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "Number of Evaluations (Mean)")
  #evaluationsGraphPlot <- evaluationsGraph %>% ggplot(aes(realcasestudy, evaluations)) + geom_bar(aes(fill = dbms), stat="identity") + facet_grid(. ~ datagenerator) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "Number of Evaluations (Mean)")

  png(filename = "plots/evaluationsGraph.png", width = 1920, height = 1080, units = "px")
  plot(evaluationsGraphPlot)
  dev.off()

  # testgenerationtime Graph CSV
  testgenerationtimeGraph <- d %>% mutate(realcasestudy = as.character(gsub("parsedcasestudy.","",realcasestudy))) %>% group_by(dbms, datagenerators_f, realcasestudy)# %>% summarise(testgenerationtime = mean(testgenerationtime))
  testgenerationtimeGraphPlot <- testgenerationtimeGraph %>%  filter(datagenerators_f != "random") %>% ggplot(aes(datagenerators_f, testgenerationtime)) + geom_boxplot() + facet_grid(dbms ~ realcasestudy) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "Test Generation Time (Mean)")
  #testgenerationtimeGraphPlot <- testgenerationtimeGraph %>% ggplot(aes(realcasestudy, testgenerationtime)) + geom_bar(aes(fill = dbms), stat="identity") + facet_grid(. ~ datagenerator) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "Test Generation Time (Mean)")

  png(filename = "plots/testgenerationtimeGraph.png", width = 1920, height = 1080, units = "px")
  plot(testgenerationtimeGraphPlot)
  dev.off()
}

#' FUNCTION: plot_heatmap_mutanttiming
#'

#' Perform a heating plot for each case
#' @export
plot_heatmap_mutanttiming <- function(d) {
  library(ggplot2)
  library(dplyr)
  library(reshape2)
  allFrames <- d
  cases <- as.vector(distinct(allFrames, schema))[[1]]

  for(case in cases) {

    newdata <- allFrames %>% group_by(schema, generator, operator) %>% filter(schema == case, type == "NORMAL") %>% summarise(killed_mutants = (length(killed[killed == "true"]) / (length(killed[killed == "false"]) + (length(killed[killed == "true"])))) * 100 )
    data <- melt(newdata, id=c("schema", "generator", "operator"))
    heatmapGraph <- data  %>% ggplot(aes(operator, generator)) + facet_grid(. ~ schema) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")+geom_text(aes(label=format(round(value, 2), nsmall = 2)))  + theme(axis.text.x = element_text(angle = 45, hjust = 1))

    caseStudyFileName <- paste(case, "-HeatMapDiagram" , ".png", sep="")
    fileSaving <- sprintf("plots/%s",caseStudyFileName)
    # Plot
    png(filename = fileSaving, width = 1920, height = 1080, units = "px")
    plot(heatmapGraph)
    dev.off()

  }

  #return(heatmapGraph)

}


#' FUNCTION: plot_heatmap_mutanttiming_allinone
#'

#' Perform a heating plot for all cases
#' @export
plot_heatmap_mutanttiming_allinone <- function(d) {
  library(ggplot2)
  library(dplyr)
  library(reshape2)
  allFrames <- d

  newdata <- allFrames %>% group_by(schema, generator, operator) %>% filter(generator != "random" ,type == "NORMAL") %>% summarise(killed_mutants = (length(killed[killed == "true"]) / (length(killed[killed == "false"]) + (length(killed[killed == "true"])))) * 100 )
  data <- melt(newdata, id=c("schema", "generator", "operator"))
  heatmapGraph <- data  %>% ggplot(aes(operator, generator)) + facet_grid(schema ~ .) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")+geom_text(aes(label=format(round(value, 2), nsmall = 2)))  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(strip.text.y = element_text(angle = 360))

  caseStudyFileName <- paste("allcasestudies", "-HeatMapDiagram" , ".png", sep="")
  fileSaving <- sprintf("plots/%s",caseStudyFileName)
  # Plot
  png(filename = fileSaving, width = 1920, height = 1080, units = "px", res = 72)
  plot(heatmapGraph)
  dev.off()

  return(heatmapGraph)
}

#' FUNCTION: plot_heatmap_mutanttiming_allinone_dbms
#'

#' Perform a heating plot for all cases with dbms
#' @export
plot_heatmap_mutanttiming_allinone_dbms <- function(d) {
  library(ggplot2)
  library(dplyr)
  library(reshape2)
  allFrames <- d

  newdata <- allFrames %>% group_by(schema, generator, operator, dbms) %>% filter(generator != "random" ,type == "NORMAL") %>% summarise(killed_mutants = (length(killed[killed == "true"]) / (length(killed[killed == "false"]) + (length(killed[killed == "true"])))) * 100 )
  data <- melt(newdata, id=c("schema", "generator", "operator", "dbms"))
  heatmapGraph <- data  %>% ggplot(aes(dbms, generator)) + facet_grid(schema ~ operator) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")+geom_text(aes(label=format(round(value, 0), nsmall = 0)))  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(strip.text.y = element_text(angle = 360))

  caseStudyFileName <- paste("allcasestudies-DBMSs", "-HeatMapDiagram" , ".png", sep="")
  fileSaving <- sprintf("plots/%s",caseStudyFileName)
  # Plot
  png(filename = fileSaving, width = 1920, height = 1080, units = "px", res = 72)
  plot(heatmapGraph)
  dev.off()

  return(heatmapGraph)
}

#' FUNCTION: testtimegeneration_table_bolding
#'

#' Perform a latex bolding and returning a dataframe
#' @export
testtimegeneration_table_bolding <- function(d) {
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

      postgres_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$testgenerationtime, (avm %>% filter(dbms == "Postgres"))$testgenerationtime)$size
      postgres_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$testgenerationtime, (avmd %>% filter(dbms == "Postgres"))$testgenerationtime)$size
      postgres_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "Postgres"))$testgenerationtime, (rand %>% filter(dbms == "Postgres"))$testgenerationtime)$size
      postgres <- NULL
      if (postgres_avm == "large" & postgres_rand == "large" & postgres_avmd == "large") {
        postgres <- "^{\\ast\\ast\\ast}"
      } else if (postgres_avm == "medium" & postgres_rand == "medium" & postgres_avmd == "medium") {
        postgres <- "^{\\ast\\ast}"
      } else {
        postgres <- "^{\\ast}"
      }

      sqlite_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$testgenerationtime, (avm %>% filter(dbms == "SQLite"))$testgenerationtime)$size
      sqlite_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$testgenerationtime, (avmd %>% filter(dbms == "SQLite"))$testgenerationtime)$size
      sqlite_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "SQLite"))$testgenerationtime, (rand %>% filter(dbms == "SQLite"))$testgenerationtime)$size
      sqlite <- NULL
      if (sqlite_avm == "large" & sqlite_rand == "large" & sqlite_avmd == "large") {
        sqlite <- "^{\\ast\\ast\\ast}"
      } else if (sqlite_avm == "medium" & sqlite_rand == "medium" & sqlite_avmd == "medium") {
        sqlite <- "^{\\ast\\ast}"
      } else {
        sqlite <- "^{\\ast}"
      }

      hsql_avm <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$testgenerationtime, (avm %>% filter(dbms == "HyperSQL"))$testgenerationtime)$size
      hsql_avmd <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$testgenerationtime, (avmd %>% filter(dbms == "HyperSQL"))$testgenerationtime)$size
      hsql_rand <- directedRandomR::effectsize_accurate((dr %>% filter(dbms == "HyperSQL"))$testgenerationtime, (rand %>% filter(dbms == "HyperSQL"))$testgenerationtime)$size
      hsql <- NULL
      if (hsql_avm == "large" & hsql_rand == "large" & hsql_avmd == "large") {
        hsql <- "^{\\ast\\ast\\ast}"
      } else if (hsql_avm == "medium" & hsql_rand == "medium" & hsql_avmd == "medium") {
        hsql <- "^{\\ast\\ast}"
      } else {
        hsql <- "^{\\ast}"
      }

      a[i,] = ifelse(min(as.numeric(a[i,])) == as.numeric(a[i,]), paste("$",postgres,"$","\\textbf{", a[i,], "}", sep = ""), as.numeric(a[i,]))
      b[i,] = ifelse(min(as.numeric(b[i,])) == as.numeric(b[i,]), paste("$",sqlite,"$","\\textbf{", b[i,], "}", sep = ""), as.numeric(b[i,]))
      c[i,] = ifelse(min(as.numeric(c[i,])) == as.numeric(c[i,]), paste("$",hsql,"$","\\textbf{", c[i,], "}", sep = ""), as.numeric(c[i,]))
    }
    d <- cbind(a1,a,b,c)
    #return(d)
    return(print(xtable(d), include.rownames=FALSE ,sanitize.text.function = function(x){x}))
}
