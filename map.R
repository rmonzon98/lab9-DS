library(geojsonio)
library(leaflet)

covid <- read.csv("fxm.csv")

geojson <- readLines("lab3BD.geojson", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON()

geojson$style = list(
  weight = 1,
  color = "#555555",
  opacity = 1,
  fillOpacity = 0.8
)

covid_cases <- sapply(geojson$features, function(feat) {
  name = toupper(feat$properties$departamentos)
  by_dept<-filter(covid, departamento==name)
  by_dept[1:5] <- list(NULL)
  if(nrow(by_dept) > 0) {
    melted_by_dept <- melt(by_dept)
    summed = sum(melted_by_dept[c("value")])
    feat$properties$covid_cases <- summed
  } else {
    feat$properties$covid_cases <- 0
    0
  }
})

population <- sapply(geojson$features, function(feat) {
  name = toupper(feat$properties$departamentos)
  by_dept<-filter(covid, departamento==name)
  by_dept<-subset(by_dept, grepl('^\\d+$', by_dept$poblacion))[c("poblacion")]
  max(sum(as.numeric(unlist(by_dept[c("poblacion")]))), 1)
})

# Color by per-capita GDP using quantiles
pal <- colorQuantile("Greens", (covid_cases / population) * 10000)
# Add a properties$style list to each feature
geojson$features <- lapply(geojson$features, function(feat) {
  name = toupper(feat$properties$departamentos)
  by_dept<-filter(covid, departamento==name)
  by_dept<-subset(by_dept, grepl('^\\d+$', by_dept$poblacion))[c("poblacion")]
  pop = max(sum(as.numeric(unlist(by_dept[c("poblacion")]))), 1)
  
  name = toupper(feat$properties$departamentos)
  by_dept<-filter(covid, departamento==name)
  by_dept[1:5] <- list(NULL)
  cases = if(nrow(by_dept) > 0) {
    melted_by_dept <- melt(by_dept)
    summed = sum(melted_by_dept[c("value")])
    feat$properties$covid_cases <- summed
  } else {
    feat$properties$covid_cases <- 0
    0
  }
  
  feat$properties$style <- list(
    fillColor = pal(
      ( cases / pop) * 10000
    )
  )
  feat
})
leaflet() %>% addGeoJSON(geojson)

