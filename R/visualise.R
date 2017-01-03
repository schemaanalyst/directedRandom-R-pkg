

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

    newdata <- allFrames %>% group_by(schema, generator, operator) %>% filter(schema == case) %>% summarise(killed_mutants = (length(killed[killed == "true"]) / (length(killed[killed == "false"]) + (length(killed[killed == "true"])))) * 100 )
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

  newdata <- allFrames %>% group_by(schema, generator, operator) %>% summarise(killed_mutants = (length(killed[killed == "true"]) / (length(killed[killed == "false"]) + (length(killed[killed == "true"])))) * 100 )
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
