library(shiny)
library(shinydashboard)
library(shinyLP)
library(dplyr)
library(ggplot2)
library(haven)
library(labelled)
library(sparklyr)
install.packages("devtools")

load("Data/EH2021.RData")

eh21p=eh21p %>% 
  mutate(nini=(((s03a_04==2 & s03a_05!=13)) & s04a_01==2))
eh21p$nini[is.na(eh21p$nini)]=FALSE

eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% group_by(depto) %>% summarise(ninis=mean(nini)*100)

eh21p=eh21p %>% 
  mutate(hrsSem=(s04b_16aa*s04b_15),subocupado=hrsSem<=40)


header=dashboardHeader(title = "Dashboard Proyecto")

sidebar=dashboardSidebar(
  sidebarMenu(
    menuItem("Inicio",tabName = "home",icon = icon("list-alt")),
    menuItem("Tasa de desempleo abierto",tabName = "ind1",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind11"),
             menuSubItem("Ficha del indicador",tabName = "ind12")
    ),
    menuItem("Proporcion de jovenes(entre 15 y 24 aÃ±os) que no cursan estudios, no estan empleados ni reciben capacitacion",tabName ="ind2",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind21"),
             menuSubItem("Ficha del indicador",tabName = "ind22")
    ),
    menuItem("Brecha de ingresos por mes",tabName = "ind6",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind61"),
             menuSubItem("Ficha del indicador",tabName = "ind62")
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
    ),
    menuItem("Tasa de Cesantia",tabName = "ind7",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind71"),
             menuSubItem("Ficha del indicador",tabName = "ind72")
    ),
    menuItem("Tasa de Aspirantes",tabName = "ind8",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind81"),
             menuSubItem("Ficha del indicador",tabName = "ind82")
    ),
    menuItem("Tasa de Inactividad",tabName = "ind9",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind91"),
             menuSubItem("Ficha del indicador",tabName = "ind92")
    ),
    menuItem("Tasa de Subempleo",tabName = "ind10",icon = icon("android"),
             menuSubItem("Reportes",tabName = "ind101"),
             menuSubItem("Ficha del indicador",tabName = "ind102")
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
                              tableOutput("tabp11"),"Esto es un comentario"),
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
                              plotOutput("plot13", height = 250)))),
            fluidRow(
              box(title = "Departamento", width = 4, background = "purple",
                  "La tasa de desempleo abierto es la proporcion de la poblacion activa que no trabajo ni una hora y que busco activamente un empleo asalariado o una actividad por cuenta propia.
      La Paz es el departamento con una mayor tasa de desempleo en comparacion con el resto de departamentos"),
              box(title = "Area", width = 4, background = "yellow",
                  "Si lo desagregamos por area el area urbana tiene una mayor tasa de desempleo abierta en comparacion con el area rural"),
              box(title = "Sexo", width = 4, background = "red",
                  "Por sexo las mujeres tienen una mayor tasa de desempleo abierto en comparacion con los hombres")
            )
    ),
    tabItem(tabName = "ind12",
            includeCSS("Ficha1.html")),
    ########################indicador 2#####################################
    tabItem(tabName = "ind21",
            h2("Proporcion de jovenes(entre 15 y 24 aÃ±os) que no cursan estudios, no estan empleados ni reciben capacitacion"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "500px",
                     tabPanel("Tab21", "Departamento", 
                              tableOutput("tabp21")),
                     tabPanel("Tab22", "Area", 
                              tableOutput("tabp22")),
                     tabPanel("Tab23", "Sexo", 
                              tableOutput("tabp23"))),
              tabBox(title = "Desagregacion",height = "500px",
                     tabPanel("gra21", "Departamento", 
                              plotOutput("plot21", height = 250)),
                     tabPanel("gra22", "Area", 
                              plotOutput("plot22", height = 250)),
                     tabPanel("gra23", "Sexo", 
                              plotOutput("plot23", height = 250)))),
            fluidRow(
              box(title = "Departamento", width = 4, background = "orange",
                  "Se muestra la proporcion de jovenes que no estudian ni trabajan por departamento, en donde Beni es el departamento con un mayor porcentaje de ninis(17.6%)"),
              box(title = "Area", width = 4, background = "maroon",
                  "Si lo desagragamos por area vemos que el area Rural tiene un mayor porcentaje de ninis(15.3%)"),
              box(title = "Sexo", width = 4, background = "blue",
                  "El porcentaje es mucho mas alto para las mujeres(15.23%) que para los hombres(6%)")
            )
    ),
    tabItem(tabName = "ind22",
            includeCSS("Ficha2.html")),
    ########################indicador 6###################################
    tabItem(tabName = "ind61",
            h2("Brecha de Ingresos por mes"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab61", "Departamento", 
                              tableOutput("tabp61")),
                     tabPanel("Tab62", "Area", 
                              tableOutput("tabp62"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra61", "Departamento", 
                              plotOutput("plot61", height = 250)),
                     tabPanel("gra62", "Area", 
                              plotOutput("plot62", height = 250)))),
            fluidRow(
              box(title = "Departamento", width = 6, background = "blue",
                  "La brecha de ingresos entre hombres y mujeres, favorecen al genero masculino en todos los departamentos"),
              box(title = "Area", width = 6, background = "green",
                  "De la misma forma se da si lo desagregamos por area en donde la diferencia de ingresos es mas notorio para el area rural")
            )
    ),
    tabItem(tabName = "ind62",
            includeCSS("Ficha3.html")),
    
    ########################indicador 3#####################################
    tabItem(tabName = "ind31",
            h2("Tasa Global de Participacion"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab31", "Departamento", 
                              tableOutput("tabp31")),
                     tabPanel("Tab32", "Area", 
                              tableOutput("tabp32")),
                     tabPanel("Tab33", "Sexo", 
                              tableOutput("tabp33"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra31", "Departamento", 
                              plotOutput("plot31", height = 250)),
                     tabPanel("gra32", "Area", 
                              plotOutput("plot32", height = 250)),
                     tabPanel("gra33", "Sexo", 
                              plotOutput("plot33", height = 250)))),
            fluidRow(
              box(title = "Departamento", width = 4, background = "orange",
                  "La tasa global de participación laboral 
                              muestra el porcentaje de las personas que trabajan o buscan 
                              activamente un trabajo sobre el total de la población en edad de trabajar (14 años y más).
                              En este caso el departamento de Potosí es el que presenta la mayor tasa de particpacion global
                              y Pando el que menos"),
              box(title = "Area", width = 4, background = "maroon",
                  "El area rural presenta una tasa de participacion global superior a la urbana."),
              box(title = "Sexo", width = 4, background = "blue",
                  "El genero masculino tiene una tasa de participacion global mayor al genero femenino.")
            )
    ),
    tabItem(tabName = "ind32",
            includeCSS("FichaTGP.html")),
    #################### indicador 4#################################
    tabItem(tabName = "ind41",
            h2("TASA DE OCUPACION"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab41", "Departamento", 
                              tableOutput("tabp41")),
                     tabPanel("Tab12", "Area", 
                              tableOutput("tabp42")),
                     tabPanel("Tab43", "Sexo", 
                              tableOutput("tabp43"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra41", "Departamento", 
                              plotOutput("plot41", height = 250)),
                     tabPanel("gra42", "Area", 
                              plotOutput("plot42", height = 250)),
                     tabPanel("gra43", "Sexo", 
                              plotOutput("plot43", height = 250)))),fluidRow(
                                box(title = "Departamento", width = 4, background = "orange",
                                    "La tasa de ocupación laboral es un ratio empleado para calcular qué 
                              porcentaje de la población se encuentra trabajando activamente con respecto al total de la 
                              población en edad de trabajar (PET) (14 años y más). A nivel departamental se evidencia una 
                              tendencia mayor en Potosí 
                              y La Paz el que menos"),
                                box(title = "Area", width = 4, background = "maroon",
                                    "El area rural presenta una tasa de ocupacion superior a la urbana."),
                                box(title = "Sexo", width = 4, background = "blue",
                                    "Los hombres tienen una tasa de ocupacion mayor a las mujeres.")
                              )
    ),
    tabItem(tabName = "ind42",
            includeCSS("FichaOcup.html")),
    #################### indicador 5#################################
    tabItem(tabName = "ind51",
            h2("INDICE DE CARGA ECONOMICA"),
            fluidRow(
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("Tab51", "Departamento", 
                              tableOutput("tabp51")),
                     tabPanel("Tab52", "Area", 
                              tableOutput("tabp52")),
                     tabPanel("Tab53", "Sexo", 
                              tableOutput("tabp53"))),
              tabBox(title = "Desagregacion",height = "400px",
                     tabPanel("gra51", "Departamento", 
                              plotOutput("plot51", height = 250)),
                     tabPanel("gra52", "Area", 
                              plotOutput("plot52", height = 250)),
                     tabPanel("gra53", "Sexo", 
                              plotOutput("plot53", height = 250)))),
            fluidRow(
              box(title = "Departamento", width = 4, background = "orange",
                  "El indice de carga economica más alto lo presenta el departamento de Beni
                              y el que presenta un indice menor es Potosí." ),
              box(title = "Area", width = 4, background = "maroon",
                  "El area urbana presenta un indice de carga economica superior a la rural."),
              box(title = "Sexo", width = 4, background = "blue",
                  "Las mujeres tienen un indice de carga economica superior a los hombres.")
            )
    ),
    tabItem(tabName = "ind52",
            includeCSS("FichaIce.html")),
    
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
                              plotOutput("plot73", height = 250)))),
            fluidRow(
              box(title = "Departamento", width = 4, background = "orange",
                  "Se puede observar que la tasa de cesantia no supera el 7% para ningun departamento donde los departamentos con una mayor incidencia para este indicador son Chuquisaca y La Paz, además el departamento con menor tasa de cesantía es Potosí"),
              box(title = "Area", width = 4, background = "maroon",
                  "Se puede observar que la tasa de cesantia es más elevada en el área urbana"),
              box(title = "Sexo", width = 4, background = "blue",
                  "Se puede observar que la tasa de cesantia en más elevada para las mujeres")
            )
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
                              plotOutput("plot83", height = 250)))),
            fluidRow(
              box(title = "Departamento", width = 4, background = "orange",
                  "Se puede observar que la tasa de aspirantes no es muy elevada en nuestro país donde todos los departamentos se encuentran debajo del 1%"),
              box(title = "Area", width = 4, background = "maroon",
                  "Se puede observar que la tasa de aspirantes es más elevada en el área urbana"),
              box(title = "Sexo", width = 4, background = "blue",
                  "Se puede observar que la tasa de aspirantes en más elevada para las mujeres")
            )
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
                              plotOutput("plot93", height = 250)))),
            fluidRow(
              box(title = "Departamento", width = 4, background = "orange",
                  "Se puede observar que la tasa de inactividad ronda el 30% en nuestro país donde el más bajo es Potosí los más elevados son los departamentos de Pando, Beni y Oruro"),
              box(title = "Area", width = 4, background = "maroon",
                  "Se puede observar que la tasa de inactividad es más elevada en el área urbana"),
              box(title = "Sexo", width = 4, background = "blue",
                  "Se puede observar que la tasa de inactividad en más elevada para las mujeres")
            )
    ),
    tabItem(tabName = "ind92",
            includeCSS("FichaTasaInactividad.html")),
    
    ####################indicador 10#################################
    tabItem(tabName = "ind101",
            h2("TASA DE SUBEMPLEO"),
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
                              plotOutput("plot103", height = 250)))),
            fluidRow(
              box(title = "Departamento", width = 4, background = "orange",
                  "Potosi es el departamento con la tasa  de subempleo mas alta y el departamento con menor tasa es Santa Cruz"),
              box(title = "Area", width = 4, background = "maroon",
                  "El area rural presenta una tasa mas alta de subempleo en comparacion al area urbana"),
              box(title = "Sexo", width = 4, background = "blue",
                  "Las mujeres presenta una tasa mas alta de subempleo en comparacion que los hombres")
            )
    ),
    tabItem(tabName = "ind102",
            includeCSS("Ficha10.html"))
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output) {
  ######################Indicador 1###########################
  output$tabp11=renderTable({
    
    d1=eh21p %>% filter(s01a_03>=14 & s01a_03<=60) %>% group_by(depto) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    d1$depto=factor(d1$depto,c(1:9),unique(to_factor(d1$depto)))
    cbind(nro=1:9,d1)
  })
  
  output$tabp12=renderTable({
    d1=eh21p %>% filter(s01a_03>=14 & s01a_03<=60) %>% group_by(area) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    d1$area=factor(d1$area,c(1:2),unique(to_factor(d1$area)))
    cbind(nro=1:2,d1)
  })
  
  output$tabp13=renderTable({
    d1=eh21p %>% filter(s01a_03>=14 & s01a_03<=60) %>% group_by(s01a_02) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    d1$s01a_02=factor(d1$s01a_02,c(1:2),unique(to_factor(d1$s01a_02)))
    cbind(nro=1:2,d1)
  })
  
  
  output$plot11=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(depto) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    
    barplot(height=d1$td, name=d1$depto,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4"))
  })
  output$plot12=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    
    barplot(height=d1$td, name=d1$area,col=c("cadetblue4","cadetblue1"))
  })
  output$plot13=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(td=sum(desocupado)/sum(pea)*100)
    
    barplot(height=d1$td, name=d1$s01a_02,col=c("blue","pink"))
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
    
    barplot(height=d1$ninis, name=d1$depto,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4"))
  })
  output$plot22=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% 
      group_by(area) %>% summarise(ninis=round(mean(nini)*100,2))
    
    barplot(height=d1$ninis, name=d1$area,col=c("cadetblue4","cadetblue1"))
  })
  output$plot23=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14,s01a_03<=24) %>% 
      group_by(s01a_02) %>% summarise(ninis=round(mean(nini)*100,2))
    
    barplot(height=d1$ninis, name=d1$s01a_02,col=c("blue","pink"))
  })
  
  ######################Indicador 6###########################
  output$tabp61=renderTable({
    
    aux=eh21p %>% filter(s01a_03>=25 & s01a_03<=60, s04a_01==1 | s04a_02!=8 | s04a_03 !=11) %>% 
      group_by(depto,s01a_02) %>% summarise(t=sum(ylab,na.rm=T)/n()) 
    
    h=aux %>% filter(s01a_02==1)
    m=aux %>% filter(s01a_02==2)
    h$depto=factor(h$depto,c(1:9),unique(to_factor(h$depto)))
    
    d2=cbind(nro=as.numeric(m$depto),h %>% select(depto),bi=h$t/m$t)
  })
  
  output$tabp62=renderTable({
    aux=eh21p %>% filter(s01a_03>=25 & s01a_03<=60, s04a_01==1 | s04a_02!=8 | s04a_03 !=11) %>% 
      group_by(area,s01a_02) %>% summarise(t=sum(ylab,na.rm=T)/n()) 
    
    h=aux %>% filter(s01a_02==1)
    m=aux %>% filter(s01a_02==2)
    h$area=factor(h$area,c(1:2),unique(to_factor(h$area)))
    
    d2=cbind(nro=as.numeric(m$area),h %>% select(area),bi=h$t/m$t)
  })
  
  output$plot61=renderPlot({
    aux=eh21p %>% filter(s01a_03>=25 & s01a_03<=60, s04a_01==1 | s04a_02!=8 | s04a_03 !=11) %>% 
      group_by(depto,s01a_02) %>% summarise(t=sum(ylab,na.rm=T)/n()) 
    
    h=aux %>% filter(s01a_02==1)
    m=aux %>% filter(s01a_02==2)
    h$depto=factor(h$depto,c(1:9),unique(to_factor(h$depto)))
    
    d2=cbind(nro=as.numeric(m$depto),h %>% select(depto),bi=h$t/m$t)
    
    barplot(height=d2$bi, name=d2$nro,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4"))
  })
  output$plot62=renderPlot({
    aux=eh21p %>% filter(s01a_03>=25 & s01a_03<=60, s04a_01==1 | s04a_02!=8 | s04a_03 !=11) %>% 
      group_by(area,s01a_02) %>% summarise(t=sum(ylab,na.rm=T)/n()) 
    
    h=aux %>% filter(s01a_02==1)
    m=aux %>% filter(s01a_02==2)
    h$area=factor(h$area,c(1:2),unique(to_factor(h$area)))
    
    d2=cbind(nro=as.numeric(m$area),h %>% select(area),bi=h$t/m$t)
    barplot(height=d2$bi, name=d2$nro,col=c("cadetblue4","cadetblue1"))
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
    
    barplot(height=d1$tgp, name=d1$depto,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4"))
  })
  output$plot32=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(tgp=sum(pea)/sum(pet)*100)
    
    barplot(height=d1$tgp, name=d1$area,col=c("cadetblue4","cadetblue1"))
  })
  output$plot33=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(tgp=sum(pea)/sum(pet)*100)
    
    barplot(height=d1$tgp, name=d1$s01a_02,col=c("blue","pink"))
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
    
    barplot(height=d1$toc, name=d1$depto,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4"))
  })
  output$plot42=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(toc=sum(ocupado)/sum(pea)*100)
    
    barplot(height=d1$toc, name=d1$area,col=c("cadetblue4","cadetblue1"))
  })
  output$plot43=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(toc=sum(ocupado)/sum(pea)*100)
    
    barplot(height=d1$toc, name=d1$s01a_02,col=c("blue","pink"))
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
    
    barplot(height=d1$ice, name=d1$depto,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4"))
  })
  output$plot52=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(ice=sum(pei)/sum(pea)*100)
    
    barplot(height=d1$ice, name=d1$area,col=c("cadetblue4","cadetblue1"))
  })
  output$plot53=renderPlot({
    d1=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(ice=sum(pei)/sum(pea)*100)
    
    barplot(height=d1$ice, name=d1$s01a_02,col=c("blue","pink"))
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
    
    barplot(height=d7$tc, name=d7$depto,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4"))
  })
  output$plot72=renderPlot({
    d7=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(tc=sum(cesante)/sum(pea)*100)
    
    barplot(height=d7$tc, name=d7$area,col=c("cadetblue4","cadetblue1"))
  })
  output$plot73=renderPlot({
    d7=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(tc=sum(cesante)/sum(pea)*100)
    
    barplot(height=d7$tc, name=d7$s01a_02,col=c("blue","pink"))
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
    
    barplot(height=d8$ta, name=d8$depto,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4"))
  })
  output$plot82=renderPlot({
    d8=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>% 
      summarise(ta=sum(aspirante)/sum(pea)*100)
    
    barplot(height=d8$ta, name=d8$area,col=c("cadetblue4","cadetblue1"))
  })
  output$plot83=renderPlot({
    d8=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>% 
      summarise(ta=sum(aspirante)/sum(pea)*100)
    
    barplot(height=d8$ta, name=d8$s01a_02,col=c("blue","pink"))
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
    
    barplot(height=d9$ti, name=d9$depto,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4"))
  })
  output$plot92=renderPlot({
    d9=eh21p %>% filter(s01a_03>=14) %>% group_by(area) %>%
      summarise(ti=sum(pei)/sum(pet)*100)
    
    barplot(height=d9$ti, name=d9$area,col=c("cadetblue4","cadetblue1"))
  })
  output$plot93=renderPlot({
    d9=eh21p %>% filter(s01a_03>=14) %>% group_by(s01a_02) %>%
      summarise(ti=sum(pei)/sum(pet)*100)
    
    barplot(height=d9$ti, name=d9$s01a_02,col=c("blue","pink"))
  })
  
  ######################Indicador 10###########################
  output$tabp101=renderTable({
    
    d10=eh21p %>% filter(s01a_03>=14&ocupado==1) %>% group_by(depto) %>% 
      summarise(ts=sum(subocupado,na.rm=TRUE)/sum(pea)*100) 
    d10$depto=factor(d10$depto,c(1:9),unique(to_factor(d10$depto)))
    cbind(nro=1:9,d10)
  })
  
  output$tabp102=renderTable({
    d10=eh21p %>%filter(s01a_03>=14&ocupado==1) %>% group_by(area) %>% 
      summarise(ts=sum(subocupado,na.rm=TRUE)/sum(pea)*100) 
    d10$area=factor(d10$area,c(1:2),unique(to_factor(d10$area)))
    cbind(nro=1:2,d10)
  })
  
  output$tabp103=renderTable({
    d10=eh21p %>%filter(s01a_03>=14&ocupado==1) %>% group_by(s01a_02) %>% 
      summarise(ts=sum(subocupado,na.rm=TRUE)/sum(pea)*100) 
    d10$s01a_02=factor(d10$s01a_02,c(1:2),unique(to_factor(d10$s01a_02)))
    cbind(nro=1:2,d10)
  })
  
  
  output$plot101=renderPlot({
    d10=eh21p %>% filter(s01a_03>=14&ocupado==1) %>% group_by(depto) %>% 
      summarise(ts=sum(subocupado,na.rm=TRUE)/sum(pea)*100) 
    barplot(height=d10$ts, name=d10$depto ,col=c("snow","red4","skyblue1","red1","red3","firebrick3","seagreen2","seagreen3","seagreen4")
    )
  })
  output$plot102=renderPlot({
    d10=eh21p %>% filter(s01a_03>=14&ocupado==1) %>% group_by(area) %>%
      summarise(ts=sum(subocupado,na.rm=TRUE)/sum(pea)*100) 
    
    
    barplot(height=d10$ts, name=d10$area,col=c("cadetblue4","cadetblue1"))
  })
  output$plot103=renderPlot({
    d10=eh21p %>% filter(s01a_03>=14&ocupado==1) %>% group_by(s01a_02) %>%
      summarise(ts=sum(subocupado,na.rm=TRUE)/sum(pea)*100) 
    
    barplot(height=d10$ts, name=d10$s01a_02,col=c("blue","pink"))
  })
}


shinyApp(ui, server)
