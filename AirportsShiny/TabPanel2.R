source("MainData.R")

# read airlines.dat
airlinesData <- read.table("airlines.dat", sep = ",")
colnames(airlinesData) <- c("AirlineID", "Name", "Alias", "IATA", "ICAO", "Callsign", "Country", "Active")
airlinesData <- airlinesData %>% filter(IATA %in% unlist(allianceData) & Active == 'Y')
# airlinesData %>% filter(IATA %in% IATA[duplicated(IATA)]) %>% View()
airlinesData <- airlinesData %>% distinct(IATA, .keep_all = TRUE) %>%
  filter(IATA %in% unlist(allianceData) & Active == 'Y') %>% select(IATA, Name) %>% arrange(Name)

# return subgraph of 'graph' with edges attribute 'Airline' is in 'airline'
CurrentAirlinesGraph <- function (graph, airline){
  graph <- simplify(graph, remove.multiple = FALSE, remove.loops = TRUE)
  return(subgraph.edges(graph, E(graph)[E(graph)$Airline %in% airline], delete.vertices = TRUE))
}