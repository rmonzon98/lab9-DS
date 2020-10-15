#install.packages("shiny")
#install.packages("reshape")
#install.packages("DT")
library(shiny)
library(reshape)
library(DT)


#*******************************************************************************
#Functions
#*******************************************************************************
melted_by_mun_c <- function(mun) {
    byMun <- cxm[fxm$municipio == mun, ];
    byMun[1:5] <- list(NULL);
    return(melt(byMun));
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


#*******************************************************************************
#Variables
#*******************************************************************************
fxm <- read.csv("fxm.csv")
txm <- read.csv("txm.csv")
cxm <- read.csv("cxm.csv")


#*******************************************************************************
#UI
#*******************************************************************************
# Define UI for application that draws a histogram
ui <- fluidPage(
    
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
        
        tabPanel("confirmados por departamento",
                 sidebarPanel(
                     selectInput("municipio_c", "Seleccione departamento:",
                                 cxm[ , c("municipio")])
                     ),
                 mainPanel(
                     plotOutput("cxm_s", width = "auto")
                     )
                ),
        
        tabPanel("Tamizados",
                     sidebarPanel(
                         selectInput("municipio_t", "Seleccione departamento:",
                                     txm[ , c("municipio")])
                     ),
                     mainPanel(
                         plotOutput("txm_s", width = "auto")
                     )
                 ),
        tabPanel("Tablas",
                 verticalLayout(
                     DT::dataTableOutput("fxmRaw"),
                     DT::dataTableOutput("txmRaw"),
                     DT::dataTableOutput("cxmRaw")
                     )
                 )
    )
)


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
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)




