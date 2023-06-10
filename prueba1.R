
library(shiny)
library(shinydashboard)
library(shinyLP)
library(dplyr)
library(ggplot2)
library(haven)
load("Data/EH2021.RData")
eh21p 

header=dashboardHeader(title = "Dashboard Proyecto")

sidebar=dashboardSidebar(
  sidebarMenu(
    menuItem("Inicio",tabName = "home",icon = icon("list-alt")),
    menuItem("Tasa de desempleo",tabName = "ind1",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind11"),
             menuSubItem("Ficha del indicador",tabName = "ind12")
             )
    )
  )

body=dashboardBody(
  tabItems(
    tabItem(tabName = "home"),
    tabItem(tabName = "ind11",
            h2("TASA DE DESEMPLEO ABIERTO"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "250px",
                     tabPanel("Tab1", "First tab content"),
                     tabPanel("Tab2", "First tab content"),
                     tabPanel("Tab3", "First tab content")),
              tabBox(title = "Desagregacion",height = "250px",
                     tabPanel("cab1", "First tab content"),
                     tabPanel("cab2", "First tab content"),
                     tabPanel("cab3", "First tab content")))
            ),
    
    tabItem(tabName = "ind12",
            includeCSS("Ficha1.html"))
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output) {
  
  output$plot1=renderPlot({
    
    aux1=input$desc
    
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)

    barplot(height=d1$td, name=d1$depto)
    
    })
}


shinyApp(ui, server)

