#eh21p <- read_sav("C:/Users/PC12/Downloads/EH2021_Persona.sav")
#library(dplyr)
#install.packages("labelled")
#library(labelled)


eh21p=eh21p %>% 
  mutate(hrsSem=(s04b_16aa*s04b_15),subocupado=hrsSem<=40) 


so<-eh21p %>%filter(s01a_03>=14&ocupado==1) %>% 
 summarise(ts=sum(subocupado,na.rm=TRUE)/sum(pea)*100) 

  
  menuItem("Tasa de subocpacion",tabName ="ind10",icon = icon("android"),
         menuSubItem("Reportes",tabName = "ind101"),
         menuSubItem("Ficha del indicador",tabName = "ind102")
),

########################indicador 10#####################################
tabItem(tabName = "ind101",
        h2("Tasa de subocupacion"),
        fluidRow(
          tabBox(title = "Desagregacion",height = "400px",
                 tabPanel("Tab101", "Departamento", 
                          tableOutput("tabp101"),
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
        includeCSS("Ficha10.html")),

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