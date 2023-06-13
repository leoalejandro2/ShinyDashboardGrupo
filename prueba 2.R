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
    ),
    menuItem("Tasa de cesantia",tabName = "ind7",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind71"),
             menuSubItem("Ficha del indicador",tabName = "ind72")
    ),
    menuItem("Tasa de aspirantes",tabName = "ind8",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind81"),
             menuSubItem("Ficha del indicador",tabName = "ind82")
    ),
    menuItem("Tasa de inactividad",tabName = "ind9",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind91"),
             menuSubItem("Ficha del indicador",tabName = "ind92")
    ),
    menuItem("Tasa de desocupacion",tabName = "ind10",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind101"),
             menuSubItem("Ficha del indicador",tabName = "ind102")
    ),
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
            includeCSS("Ficha2.html")),
    
    ########################indicador 7#####################################
    tabItem(tabName = "ind71",
            h2("TASA DE CESANTIA"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab71", "Departamento", 
                              tableOutput("tabp71")),
                     tabPanel("Tab72", "Area", 
                              tableOutput("tabp72")),
                     tabPanel("Tab73", "Sexo", 
                              tableOutput("tabp73"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra71", "Departamento", 
                              plotOutput("plot71", height = 250)),
                     tabPanel("gra72", "Area", 
                              plotOutput("plot72", height = 250)),
                     tabPanel("gra73", "Sexo", 
                              plotOutput("plot73", height = 250))))
    ),
    tabItem(tabName = "ind72",
            includeCSS("FichaTasaCesantia.html")),
    
    ####################indicador 8#################################
    tabItem(tabName = "ind81",
            h2("TASA DE ASPIRANTES"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab81", "Departamento", 
                              tableOutput("tabp81")),
                     tabPanel("Tab82", "Area", 
                              tableOutput("tabp82")),
                     tabPanel("Tab83", "Sexo", 
                              tableOutput("tabp83"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra81", "Departamento", 
                              plotOutput("plot81", height = 250)),
                     tabPanel("gra82", "Area", 
                              plotOutput("plot82", height = 250)),
                     tabPanel("gra83", "Sexo", 
                              plotOutput("plot83", height = 250))))
    ),
    tabItem(tabName = "ind82",
            includeCSS("FichaTasaAspirantes.html")),
    
    ####################indicador 9#################################
    tabItem(tabName = "ind91",
            h2("TASA DE INACTIVIDAD"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab91", "Departamento", 
                              tableOutput("tabp91")),
                     tabPanel("Tab92", "Area", 
                              tableOutput("tabp92")),
                     tabPanel("Tab93", "Sexo", 
                              tableOutput("tabp93"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra91", "Departamento", 
                              plotOutput("plot91", height = 250)),
                     tabPanel("gra92", "Area", 
                              plotOutput("plot92", height = 250)),
                     tabPanel("gra93", "Sexo", 
                              plotOutput("plot93", height = 250))))
    ),
    tabItem(tabName = "ind92",
            includeCSS("FichaTasaInactividad.html")),
    
    ####################indicador 10#################################
    tabItem(tabName = "ind101",
            h2("TASA DE DESOCUPACION"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab101", "Departamento", 
                              tableOutput("tabp101")),
                     tabPanel("Tab102", "Area", 
                              tableOutput("tabp102")),
                     tabPanel("Tab103", "Sexo", 
                              tableOutput("tabp103"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra101", "Departamento", 
                              plotOutput("plot101", height = 250)),
                     tabPanel("gra102", "Area", 
                              plotOutput("plot102", height = 250)),
                     tabPanel("gra103", "Sexo", 
                              plotOutput("plot103", height = 250))))
    ),
    tabItem(tabName = "ind102",
            includeCSS("FichaDesocupado.html"))
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
  
  ######################Indicador 7###########################
  output$tabp71=renderTable({
    
    d7=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(tc=sum(cesante)/sum(pea)*100)
    d7$depto=factor(d7$depto,c(1:9),unique(to_factor(d7$depto)))
    cbind(nro=1:9,d7)
  })
  
  output$tabp72=renderTable({
    d7=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(tc=sum(cesante)/sum(pea)*100)
    d7$area=factor(d7$area,c(1:2),unique(to_factor(d7$area)))
    cbind(nro=1:2,d7)
  })
  
  output$tabp73=renderTable({
    d7=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(tc=sum(cesante)/sum(pea)*100)
    d7$s01a_02=factor(d7$s01a_02,c(1:2),unique(to_factor(d7$s01a_02)))
    cbind(nro=1:2,d7)
  })
  
  
  output$plot71=renderPlot({
    d7=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(tc=sum(cesante)/sum(pea)*100)
    
    barplot(height=d7$td, name=d7$depto)
  })
  output$plot72=renderPlot({
    d7=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(tc=sum(cesante)/sum(pea)*100)
    
    barplot(height=d7$td, name=d7$area)
  })
  output$plot73=renderPlot({
    d7=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(tc=sum(cesante)/sum(pea)*100)
    
    barplot(height=d7$td, name=d7$s01a_02)
  }) 
  
  ######################Indicador 8###########################
  output$tabp81=renderTable({
    
    d8=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(ta=sum(aspirante)/sum(pea)*100)
    d8$depto=factor(d8$depto,c(1:9),unique(to_factor(d8$depto)))
    cbind(nro=1:9,d8)
  })
  
  output$tabp82=renderTable({
    d8=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(ta=sum(aspirante)/sum(pea)*100)
    d8$area=factor(d8$area,c(1:2),unique(to_factor(d8$area)))
    cbind(nro=1:2,d8)
  })
  
  output$tabp83=renderTable({
    d8=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(ta=sum(aspirante)/sum(pea)*100)
    d8$s01a_02=factor(d8$s01a_02,c(1:2),unique(to_factor(d8$s01a_02)))
    cbind(nro=1:2,d8)
  })
  
  
  output$plot81=renderPlot({
    d8=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(ta=sum(aspirante)/sum(pea)*100)
    
    barplot(height=d8$td, name=d8$depto)
  })
  output$plot82=renderPlot({
    d8=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(ta=sum(aspirante)/sum(pea)*100)
    
    barplot(height=d8$td, name=d8$area)
  })
  output$plot83=renderPlot({
    d8=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(ta=sum(aspirante)/sum(pea)*100)
    
    barplot(height=d8$td, name=d8$s01a_02)
  })
  
  ######################Indicador 9###########################
  output$tabp91=renderTable({
    
    d9=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>%
      summarise(ti=sum(pei)/sum(pet)*100)
    d9$depto=factor(d9$depto,c(1:9),unique(to_factor(d9$depto)))
    cbind(nro=1:9,d9)
  })
  
  output$tabp92=renderTable({
    d9=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>%
      summarise(ti=sum(pei)/sum(pet)*100)
    d9$area=factor(d9$area,c(1:2),unique(to_factor(d9$area)))
    cbind(nro=1:2,d9)
  })
  
  output$tabp93=renderTable({
    d9=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>%
      summarise(ti=sum(pei)/sum(pet)*100)
    d9$s01a_02=factor(d9$s01a_02,c(1:2),unique(to_factor(d9$s01a_02)))
    cbind(nro=1:2,d9)
  })
  
  
  output$plot91=renderPlot({
    d9=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>%
      summarise(ti=sum(pei)/sum(pet)*100)
    
    barplot(height=d9$td, name=d9$depto)
  })
  output$plot92=renderPlot({
    d9=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>%
      summarise(ti=sum(pei)/sum(pet)*100)
    
    barplot(height=d9$td, name=d9$area)
  })
  output$plot93=renderPlot({
    d9=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>%
      summarise(ti=sum(pei)/sum(pet)*100)
    
    barplot(height=d9$td, name=d9$s01a_02)
  })
  
  ######################Indicador 10###########################
  output$tabp101=renderTable({
    
    d10=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    d10$depto=factor(d10$depto,c(1:9),unique(to_factor(d10$depto)))
    cbind(nro=1:9,d10)
  })
  
  output$tabp102=renderTable({
    d10=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    d10$area=factor(d10$area,c(1:2),unique(to_factor(d10$area)))
    cbind(nro=1:2,d10)
  })
  
  output$tabp103=renderTable({
    d10=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    d10$s01a_02=factor(d10$s01a_02,c(1:2),unique(to_factor(d10$s01a_02)))
    cbind(nro=1:2,d10)
  })
  
  
  output$plot101=renderPlot({
    d10=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    
    barplot(height=d10$td, name=d10$depto)
  })
  output$plot102=renderPlot({
    d10=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    
    barplot(height=d10$td, name=d10$area)
  })
  output$plot103=renderPlot({
    d10=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    
    barplot(height=d10$td, name=d10$s01a_02)
  })
}


shinyApp(ui, server)
