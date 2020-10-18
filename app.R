#install.packages("shiny")
#install.packages("reshape")
#install.packages("DT")
library(shiny)
library(reshape)
library(DT)
library(dplyr)
library(ggplot2)
library(leaflet)
library(rjson)
library(geojsonio)
library(shinydashboard)
library(leaflet.extras)

#*******************************************************************************
#Variables
#*******************************************************************************
fxm <- read.csv('fxm.csv')
txm <- read.csv('txm.csv')
cxm <- read.csv('cxm.csv')
covid <- read.csv("fxm.csv")

#*******************************************************************************
#Functions
#*******************************************************************************
melted_by_mun_c <- function(mun) {
    byMun <- cxm[fxm$municipio == mun, ];
    byMun[1:5] <- list(NULL);
    return(melt(byMun));
}

prueba_plot <- function(mun) {
    reduce_dt <- filter(fxm, codigo_municipio == mun);
    reduce_dt <- reduce_dt[6:length(reduce_dt)];
    return(melt(reduce_dt));
}

melted_by_mun <- function(mun) {
    byMun <- fxm[fxm$municipio == mun, ];
    byMun[1:5] <- list(NULL);
    return(melt(byMun));
}

melted_by_mun_t <- function(mun) {
    byMun <- txm[fxm$municipio == mun, ];
    byMun[1:5] <- list(NULL);
    return(melt(byMun));
}

all_mun_name <- function(){
    munis <- fxm[[3]]
    byMun[1] <- list(NULL);
    vector = c();
    for (i in munis){
        vector <- c(vector, i);
    }
    return(vector);
}

all_mun_tot <- function(){
    munis <- fxm[[3]]
    byMun[1] <- list(NULL);
    vector = c();
    for (i in munis){
        vector <- c(vector, count_mun_tot(i));
    }
    print(vector)
    return(vector);
}

#Contar el total de un departamenteo
count_mun_tot <- function(mun) {
    byMun <- fxm[fxm$municipio == mun, ];
    byMun[1:5] <- list(NULL);
    return(sum(1:244));
}

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

#*******************************************************************************
#UI
#*******************************************************************************

header1 <- dashboardHeader(
    title = "Dashboard desde 0"
)

sidebar1 <- dashboardSidebar(
    sidebarMenu() #sidebarMenu
)

body1 <- dashboardBody(
    tabsetPanel(
        tabPanel("Fallecidos por municipio",
                 sidebarPanel(
                     selectInput("municipio", "Seleccione municipio:",
                                 fxm[ , c("municipio")])
                 ),
                 mainPanel(
                     plotOutput("fxm_s", width = "auto")
                 )
        ),
        
        tabPanel("confirmados por municipio",
                 sidebarPanel(
                     selectInput("municipio_c", "Seleccione municipio:",
                                 cxm[ , c("municipio")])
                 ),
                 mainPanel(
                     plotOutput("cxm_s", width = "auto")
                 )
        ),
        
        tabPanel("Tamizados por municipio",
                 sidebarPanel(
                     selectInput("municipio_t", "Seleccione municipio:",
                                 txm[ , c("municipio")])
                 ),
                 mainPanel(
                     plotOutput("txm_s", width = "auto")
                 )
        ),
        tabPanel("Mapa",leafletOutput("Map1", width = "100%", height = 550)),
        tabPanel("Tablas",
                 verticalLayout(
                     DT::dataTableOutput("fxmRaw"),
                     DT::dataTableOutput("txmRaw"),
                     DT::dataTableOutput("cxmRaw")
                 )
        )
    )
)

# Define UI for application that draws a histogram
ui <- fluidPage(header1, body1)


#*******************************************************************************
#Server
#*******************************************************************************
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$fxmRaw <- DT::renderDataTable({
        DT::datatable({
            fxm
        })
    })
    
    output$txmRaw <- DT::renderDataTable({
        DT::datatable({
            txm
        })
    })
    
    output$Map1 <- renderLeaflet({
        leaflet() %>% addGeoJSON(geojson )%>%
        setView(-90.5626017, 14.6263757, zoom = 5)
        })
    
    output$cxmRaw <- DT::renderDataTable({
        DT::datatable({
            cxm
        })
    })
    
    output$fxm_s <- renderPlot({
       plot(melted_by_mun(input$municipio)$variable, melted_by_mun(input$municipio)$value)
    })
    
    output$cxm_s <- renderPlot({
        plot(melted_by_mun_c(input$municipio_c)$variable, melted_by_mun_c(input$municipio_c)$value)
    })
    
    output$txm_s <- renderPlot({
        plot(melted_by_mun_t(input$municipio_t)$variable, melted_by_mun_t(input$municipio_t)$value)
    })
    
    output$pruebaPlot <- renderPlot({ggplot( prueba_plot(input$cod_mun_f),
        aes(x = variable, y = value))
        #y_var <- c();
        #for (i in ejemplo){y <- c(y_var,i);}
        #ggplot(reduce_dt, aes(x = melt(reduce_dt)$variable, y = melt(reduce_dt)$value))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)




