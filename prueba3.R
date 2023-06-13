
library(shiny)
library(shinydashboard)
library(shinyLP)
library(dplyr)
library(ggplot2)
library(haven)
library(labelled)
library(sparklyr)

load("Data/EH2021.RData")
(eh21p$s01a_02)
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
    menuItem("Proporcion de jovenes(entre 15 y 24 aÃ±os) que no cursan estudios, no estan empleados ni reciben capacitacion",tabName ="ind2",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind21"),
             menuSubItem("Ficha del indicador",tabName = "ind22")
             ),
    menuItem("Tasa Global de Participacion",tabName ="ind3",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind31"),
             menuSubItem("Ficha del indicador",tabName = "ind32")
            ),
    menuItem("Tasa de Ocupacion",tabName ="ind4",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind41"),
             menuSubItem("Ficha del indicador",tabName = "ind42")
            ),
    menuItem("Indice de Carga Economica",tabName ="ind5",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind51"),
             menuSubItem("Ficha del indicador",tabName = "ind52")
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
            h2("Proporcion de jovenes(entre 15 y 24 aÃ±os) que no cursan estudios, no estan empleados ni reciben capacitacion"),
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
    ########################indicador 3#####################################
    tabItem(tabName = "ind31",
            h2("Tasa Global de Participacion"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab31", "Departamento", 
                              tableOutput("tabp31"),"La tasa global de participación laboral 
                              muestra el porcentaje de las personas que trabajan o buscan 
                              activamente un trabajo sobre el total de la población en edad de trabajar (14 años y más).
                              En este caso el departamento de Potosí es el que presenta la mayor tasa de particpacion global
                              y Pando el que menos"),
                     tabPanel("Tab32", "Area", 
                              tableOutput("tabp32")), "El area rural presenta una tasa de participacion global superior a la urbana.",
                     tabPanel("Tab33", "Sexo", 
                              tableOutput("tabp33")),"El genero masculino tiene una tasa de participacion global mayor al genero femenino."),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra31", "Departamento", 
                              plotOutput("plot31", height = 250)),
                     tabPanel("gra32", "Area", 
                              plotOutput("plot32", height = 250)),
                     tabPanel("gra33", "Sexo", 
                              plotOutput("plot33", height = 250))))
    ),
    tabItem(tabName = "ind32",
            includeCSS("FichaTGP.html")),
    #################### indicador 4#################################
    tabItem(tabName = "ind41",
            h2("TASA DE OCUPACION"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab41", "Departamento", 
                              tableOutput("tabp41"),"La tasa de ocupación laboral es un ratio empleado para calcular qué 
                              porcentaje de la población se encuentra trabajando activamente con respecto al total de la 
                              población en edad de trabajar (PET) (14 años y más). A nivel departamental se evidencia una 
                              tendencia mayor en Potosí 
                              y La Paz el que menos"),
                     tabPanel("Tab12", "Area", 
                              tableOutput("tabp42"), "El area rural presenta una tasa de ocupacion superior a la urbana."),
                     tabPanel("Tab43", "Sexo", 
                              tableOutput("tabp43"),"Los hombres tienen una tasa de ocupacion mayor a las mujeres.")),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra41", "Departamento", 
                              plotOutput("plot41", height = 250)),
                     tabPanel("gra42", "Area", 
                              plotOutput("plot42", height = 250)),
                     tabPanel("gra43", "Sexo", 
                              plotOutput("plot43", height = 250))))
    ),
    tabItem(tabName = "ind42",
            includeCSS("FichaOcup.html")),
    #################### indicador 5#################################
    tabItem(tabName = "ind51",
            h2("INDICE DE CARGA ECONOMICA"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab51", "Departamento", 
                              tableOutput("tabp51"),"El indice de carga economica más alto lo presenta el departamento de Beni
                              y el que presenta un indice menor es Potosí."),
                     tabPanel("Tab52", "Area", 
                              tableOutput("tabp52"),"El area urbana presenta un indice de carga economica superior a la rural."),
                     tabPanel("Tab53", "Sexo", 
                              tableOutput("tabp53"),"Las mujeres tienen un indice de carga economica superior a los hombres.")),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra51", "Departamento", 
                              plotOutput("plot51", height = 250)),
                     tabPanel("gra52", "Area", 
                              plotOutput("plot52", height = 250)),
                     tabPanel("gra53", "Sexo", 
                              plotOutput("plot53", height = 250))))
    ),
    tabItem(tabName = "ind12",
            includeCSS("FichaIce.html")),
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
  output$tabp31=renderTable({
    
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(tgp=sum(pea)/sum(pet)*100) 
    d1$depto=factor(d1$depto,c(1:9),unique(to_factor(d1$depto)))
    cbind(nro=1:9,d1)
  })
  
  output$tabp32=renderTable({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(tgp=sum(pea)/sum(pet)*100)
    d1$area=factor(d1$area,c(1:2),unique(to_factor(d1$area)))
    cbind(nro=1:2,d1)
  })
  
  output$tabp33=renderTable({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(tgp=sum(pea)/sum(pet)*100)
    d1$s01a_02=factor(d1$s01a_02,c(1:2),unique(to_factor(d1$s01a_02)))
    cbind(nro=1:2,d1)
  })
  
  
  output$plot31=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(tgp=sum(pea)/sum(pet)*100)
    
    barplot(height=d1$tgp, name=d1$depto)
  })
  output$plot32=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(tgp=sum(pea)/sum(pet)*100)
    
    barplot(height=d1$tgp, name=d1$area)
  })
  output$plot33=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(tgp=sum(pea)/sum(pet)*100)
    
    barplot(height=d1$tgp, name=d1$s01a_02)
  })
  
  ######################Indicador 4########################### 
  output$tabp41=renderTable({
    
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(toc=sum(ocupado)/sum(pea)*100) 
    d1$depto=factor(d1$depto,c(1:9),unique(to_factor(d1$depto)))
    cbind(nro=1:9,d1)
  })
  
  output$tabp42=renderTable({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(toc=sum(ocupado)/sum(pea)*100)
    d1$area=factor(d1$area,c(1:2),unique(to_factor(d1$area)))
    cbind(nro=1:2,d1)
  })
  
  output$tabp43=renderTable({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(toc=sum(ocupado)/sum(pea)*100)
    d1$s01a_02=factor(d1$s01a_02,c(1:2),unique(to_factor(d1$s01a_02)))
    cbind(nro=1:2,d1)
  })
  
  
  output$plot41=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(toc=sum(ocupado)/sum(pea)*100)
    
    barplot(height=d1$toc, name=d1$depto)
  })
  output$plot42=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(toc=sum(ocupado)/sum(pea)*100)
    
    barplot(height=d1$toc, name=d1$area)
  })
  output$plot43=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(toc=sum(ocupado)/sum(pea)*100)
    
    barplot(height=d1$toc, name=d1$s01a_02)
  })
  
  ######################Indicador 5########################### 
  output$tabp51=renderTable({
    
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(ice=sum(pei)/sum(pea)*100) 
    d1$depto=factor(d1$depto,c(1:9),unique(to_factor(d1$depto)))
    cbind(nro=1:9,d1)
  })
  
  output$tabp52=renderTable({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(ice=sum(pei)/sum(pea)*100)
    d1$area=factor(d1$area,c(1:2),unique(to_factor(d1$area)))
    cbind(nro=1:2,d1)
  })
  
  output$tabp53=renderTable({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(ice=sum(pei)/sum(pea)*100)
    d1$s01a_02=factor(d1$s01a_02,c(1:2),unique(to_factor(d1$s01a_02)))
    cbind(nro=1:2,d1)
  })
  
  
  output$plot51=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(ice=sum(pei)/sum(pea)*100)
    
    barplot(height=d1$ice, name=d1$depto)
  })
  output$plot52=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(ice=sum(pei)/sum(pea)*100)
    
    barplot(height=d1$ice, name=d1$area)
  })
  output$plot43=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(ice=sum(pei)/sum(pea)*100)
    
    barplot(height=d1$ice, name=d1$s01a_02)
  })
}


shinyApp(ui, server)

