library(ggplot2)
library(igraph)
library(dplyr)

CompareGraphObjects <- function(a, b){
  a <- igraph::as_data_frame(a)
  b <- igraph::as_data_frame(b)
  return(identical(a, b))
}

# read routes.dat
routes <- read.table("routes.dat", sep = ",")
colnames(routes) <- c("Airline", "AirlineID",
                      "SourceAirport", "SourceAirportID",
                      "DestinationAirport", "DestinationAirportID",
                      "Codeshare", "Stops", "Equipment")
routes <- routes %>% select(SourceAirport, DestinationAirport, Airline)

# read airports.dat
airports <- read.table("airports.dat", sep = ",", encoding = "UTF-8")
colnames(airports) <- c("AirportID", "Name", "City", "Country", "IATA", "ICAO",
                        "Latitude", "Longitude", "Altitude",
                        "Timezone", "DST", "Tz", "Type", "Source")
airports <- airports %>% select(Name, Country, IATA, Latitude, Longitude)

# set allianceData
allianceData <- list(SA = c('A3', 'AC', 'CA', 'AI', 'NZ', 'NH', 'OZ', 'OS', 'AV', 'SN',
                            'CM', 'OU', 'MS', 'ET', 'BR', 'LO', 'LH', 'SK', 'ZH', 'SQ',
                            'SA', 'LX', 'TP', 'TG', 'TK', 'UA'),
                     ST = c('AR', 'AM', 'UX', 'AF', 'CI', 'MU', 'OK', 'DL', 'GA', 'AZ',
                            'KQ', 'KL', 'KE', 'ME', 'SV', 'RO', 'VN', 'VS', 'MF'),
                     OW = c('AS', 'AA', 'BA', 'CX', 'AY', 'IB', 'JL', 'MH', 'QF', 'QR',
                            'AT', 'RJ', 'UL', 'FJ', 'WY')
)
allianceData <- lapply(allianceData, sort)

# returns routes operated by airlines from the alliance excluding Russian airports
getAllianceData <- function (allianceCode, routes, airports){
  filteredData <- routes %>% filter(Airline %in% allianceData[[allianceCode]])

  #removing russian flights which are not operating nowadays (because of war conflict in Ukraine)
  airportsRussia <- airports %>% filter(Country == "Russia")
  filteredData <- filteredData %>% filter(!SourceAirport %in% airportsRussia$IATA)
  filteredData <- filteredData %>% filter(!DestinationAirport %in% airportsRussia$IATA)

  return(filteredData)
}

# creates graph
fullDataGraph <- function(routesAlliance){
  vertices <- sort(unique(c(routesAlliance$SourceAirport, routesAlliance$DestinationAirport)))
  graph <- graph_from_data_frame(routesAlliance, directed = FALSE, vertices = vertices)
  return(graph)
}

# returns ggplot of the graph
graphForPlot <- function (graph, airports, ranges){
  graph <- simplify(graph)
  airports <- airports %>% filter(IATA %in% V(graph)$name)

  #load world map
  worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')

  #define graph for plot
  graphForShiny <- ggplot() +
    geom_sf(data = worldmap) +
    coord_sf(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
    geom_point(data = airports, aes(x = Longitude, y = Latitude), size = 8, colour = "yellow") +
    geom_text(data = airports, aes(x = Longitude, y = Latitude, label = IATA), size = 4)
  return(graphForShiny)
}

# returns subgraph of the graph with top n airports according to Degree Centrality
topGraph <- function(graph, n = 100){
  degreeMulti <- degree(
    graph,
    v = V(graph),
    mode = "all",
    loops = FALSE,
    normalized = FALSE
  )
  topAirports <- sort(degreeMulti, decreasing = TRUE)[1:n]

  return(list(g = induced_subgraph(graph, names(topAirports)),
              df = data.frame(airport = names(topAirports), degree = topAirports)))
}