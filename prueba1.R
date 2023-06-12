
library(shiny)
library(shinydashboard)
library(shinyLP)
library(dplyr)
library(ggplot2)
library(haven)
library(labelled)
library(sparklyr)

load("Data/EH2021.RData")

eh21p=eh21p %>% 
  mutate(nini=(((s03a_04==2 & s03a_05!=13)) & s04a_01==2))
eh21p$nini[is.na(eh21p$nini)]=FALSE

eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% group_by(depto) %>% summarise(ninis=mean(nini)*100)




header=dashboardHeader(title = "Dashboard Proyecto")

sidebar=dashboardSidebar(
  sidebarMenu(
    menuItem("Inicio",tabName = "home",icon = icon("list-alt")),
    menuItem("Tasa de desempleo",tabName = "ind1",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind11"),
             menuSubItem("Ficha del indicador",tabName = "ind12")
             ),
    menuItem("Proporcion de jovenes(entre 15 y 24 años) que no cursan estudios, no estan empleados ni reciben capacitacion",tabName ="ind2",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind21"),
             menuSubItem("Ficha del indicador",tabName = "ind22")
             )
    )
  )

body=dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            includeCSS("Informe.html")),
    #################### indicador 1#################################
    tabItem(tabName = "ind11",
            h2("TASA DE DESEMPLEO ABIERTO"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab11", "Departamento", 
                              tableOutput("tabp11")),
                     tabPanel("Tab12", "Area", 
                              tableOutput("tabp12")),
                     tabPanel("Tab13", "Sexo", 
                              tableOutput("tabp13"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra11", "Departamento", 
                              plotOutput("plot11", height = 250)),
                     tabPanel("gra12", "Area", 
                              plotOutput("plot12", height = 250)),
                     tabPanel("gra13", "Sexo", 
                              plotOutput("plot13", height = 250))))
            ),
    tabItem(tabName = "ind12",
            includeCSS("Ficha1.html")),
    ########################indicador 2#####################################
    tabItem(tabName = "ind21",
            h2("Proporcion de jovenes(entre 15 y 24 años) que no cursan estudios, no estan empleados ni reciben capacitacion"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab21", "Departamento", 
                              tableOutput("tabp21")),
                     tabPanel("Tab22", "Area", 
                              tableOutput("tabp22")),
                     tabPanel("Tab23", "Sexo", 
                              tableOutput("tabp23"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra21", "Departamento", 
                              plotOutput("plot21", height = 250)),
                     tabPanel("gra22", "Area", 
                              plotOutput("plot22", height = 250)),
                     tabPanel("gra23", "Sexo", 
                              plotOutput("plot23", height = 250))))
    ),
    tabItem(tabName = "ind22",
            includeCSS("Ficha2.html"))
    
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output) {
  ######################Indicador 1###########################
  output$tabp11=renderTable({
    
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    d1$depto=factor(d1$depto,c(1:9),unique(to_factor(d1$depto)))
    cbind(nro=1:9,d1)
    })
  
  output$tabp12=renderTable({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    d1$area=factor(d1$area,c(1:2),unique(to_factor(d1$area)))
    cbind(nro=1:2,d1)
    })
  
  output$tabp13=renderTable({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    d1$s01a_02=factor(d1$s01a_02,c(1:2),unique(to_factor(d1$s01a_02)))
    cbind(nro=1:2,d1)
    })
  
  
  output$plot11=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    
    barplot(height=d1$td, name=d1$depto)
    })
  output$plot12=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    
    barplot(height=d1$td, name=d1$area)
    })
  output$plot13=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    
    barplot(height=d1$td, name=d1$s01a_02)
    })
  ######################Indicador 2###########################
  output$tabp21=renderTable({
    
    d1=eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% 
      group_by(depto) %>% summarise(ninis=round(mean(nini)*100,2))
    d1$depto=factor(d1$depto,c(1:9),unique(to_factor(d1$depto)))
    cbind(nro=1:9,d1)
  })
  
  output$tabp22=renderTable({
    d1=eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% 
      group_by(area) %>% summarise(ninis=round(mean(nini)*100,2))
    d1$area=factor(d1$area,c(1:2),unique(to_factor(d1$area)))
    cbind(nro=1:2,d1)
  })
  
  output$tabp23=renderTable({
    d1=eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% 
      group_by(s01a_02) %>% summarise(ninis=round(mean(nini)*100,2))
    d1$s01a_02=factor(d1$s01a_02,c(1:2),unique(to_factor(d1$s01a_02)))
    cbind(nro=1:2,d1)
  })
  
  
  output$plot21=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% 
      group_by(depto) %>% summarise(ninis=round(mean(nini)*100,2))
    
    barplot(height=d1$ninis, name=d1$depto)
  })
  output$plot22=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% 
      group_by(area) %>% summarise(ninis=round(mean(nini)*100,2))
    
    barplot(height=d1$ninis, name=d1$area)
  })
  output$plot23=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% 
      group_by(s01a_02) %>% summarise(ninis=round(mean(nini)*100,2))
    
    barplot(height=d1$ninis, name=d1$s01a_02)
  })
  
  ######################Indicador 3########################### 
  
}


shinyApp(ui, server)

