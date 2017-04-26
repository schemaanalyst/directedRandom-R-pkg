#' FUNCTION: plot_overall
#'
#' Perform a plotting for coverage, mutation score, evaluations and testgenerationtime
#' File saved in plots/
#' @param d Data frame of analysis
#' @importFrom magrittr %>%
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

  png(filename = "plots/testgenerationtimeGraph.png", width = 1920, height = 1080, units = "px")
  plot(testgenerationtimeGraphPlot)
  dev.off()
}

#' FUNCTION: plot_heatmap_mutanttiming
#'
#' Perform a heating plot for each case
#' File saved in plots/
#' @param d Data frame of analysis
#' @importFrom magrittr %>%
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
}


#' FUNCTION: plot_heatmap_mutanttiming_allinone
#'
#' Perform a heating plot for all cases
#' File saved in plots/
#' @param d Data frame of analysis
#' @return A Plot
#' @importFrom magrittr %>%
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
#' File saved in plots/
#' @param d Data frame of analysis
#' @return A Plot
#' @importFrom magrittr %>%
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

#' FUNCTION: visual_grid_ten_schemas
#'
#' generates a plot grid of top 10 schemas
#' @param d Data frame of analysis
#' @return A Plot
#' @importFrom magrittr %>%
#' @export
visual_grid_ten_schemas <- function(d) {
  mutationanalysistiming %>% dplyr::group_by(casestudy, datagenerator, dbms) %>% dplyr::filter(datagenerator != "random", casestudy != "ArtistSimilarity", casestudy != "ArtistTerm", casestudy != "BankAccount", casestudy != "BookTown", casestudy != "Cloc",casestudy != "CoffeeOrders", casestudy != "DellStore", casestudy != "Employee", casestudy != "Examination", casestudy != "FrenchTowns", casestudy != "Inventory", casestudy != "Iso3166", casestudy != "IsoFlav_R2", casestudy != "JWhoisServer", casestudy != "MozillaExtensions", casestudy != "MozillaPermissions", casestudy != "NistDML181", casestudy != "NistDML183", casestudy != "NistWeather", casestudy != "NistXTS748", casestudy != "NistXTS749", casestudy != "Person", casestudy != "StackOverflow", casestudy != "StudentResidence", casestudy != "Usda", casestudy != "WordNet") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_wrap( ~ casestudy, scales = "free") +  labs(y = "Test Generation Time For all runs per Schema (In Sceconds)")

  a <- mutationanalysistiming %>% dplyr::group_by(casestudy, datagenerator, dbms) %>% dplyr::filter(datagenerator != "random", casestudy == "iTrust") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") +  labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  b <- mutationanalysistiming %>% dplyr::group_by(casestudy, datagenerator, dbms) %>% dplyr::filter(datagenerator != "random", casestudy == "BrowserCookies") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  c <- mutationanalysistiming %>% dplyr::group_by(casestudy, datagenerator, dbms) %>% dplyr::filter(datagenerator != "random", casestudy == "CustomerOrder") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  d <- mutationanalysistiming %>% dplyr::group_by(casestudy, datagenerator, dbms) %>% dplyr::filter(datagenerator != "random", casestudy == "Flights") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  e <- mutationanalysistiming %>% dplyr::group_by(casestudy, datagenerator, dbms) %>% dplyr::filter(datagenerator != "random", casestudy == "NistDML182") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  f <- mutationanalysistiming %>% dplyr::group_by(casestudy, datagenerator, dbms) %>% dplyr::filter(datagenerator != "random", casestudy == "Products") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  g <- mutationanalysistiming %>% dplyr::group_by(casestudy, datagenerator, dbms) %>% dplyr::filter(datagenerator != "random", casestudy == "RiskIt") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))
  h <- mutationanalysistiming %>% dplyr::group_by(casestudy, datagenerator, dbms) %>% dplyr::filter(datagenerator != "random", casestudy == "UnixUsage") %>% ggplot(aes(datagenerator, testgenerationtime / 1000)) + geom_boxplot() + facet_grid(dbms ~ casestudy, scales = "free") + labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(angle = 90), axis.text.x = element_text(angle = 90, hjust = 1))  + scale_x_discrete(labels=c("directedRandom" = "DR", "avs" = "AVM", "avsDefaults" = "AVMD"))

  p <- gridExtra::grid.arrange(a, b, c, d, e, f, g, h, nrow =1, left="Test Generation Time For all runs per Schema (In Sceconds)", bottom = "Generator")

  return(p)
}
