# Calling out all needed libraries

library(shiny)
library(ggplot2)
library (plotly)
library(dplyr) 
library(tidyr) 
library(viridis)
library(janitor)
library(leaflet)
library(jsonlite)
library(rnaturalearth)
library(sp)
library(gapminder)
library(gifski)
library(caTools)
library(png)


#Loading and preparing data for analysis and visualization


a2015 <- read.csv("data/2015.csv") %>% clean_names() %>%select(year,country,region,happiness_rank,happiness_score,economy_gdp_per_capita, social_support, health_life_expectancy, freedom, trust_government_corruption, generosity, dystopia_residual)

b2016 <- read.csv("data/2016.csv") %>% clean_names() %>%select(year,country,region,happiness_rank,happiness_score,economy_gdp_per_capita, social_support, health_life_expectancy, freedom, trust_government_corruption, generosity, dystopia_residual)

c2017 <- read.csv("data/2017.csv") %>% clean_names() %>%select(year,country,region,happiness_rank,happiness_score,economy_gdp_per_capita, social_support, health_life_expectancy, freedom, trust_government_corruption, generosity, dystopia_residual)

d2018 <- read.csv("data/2018.csv") %>% clean_names() %>%select(year,country,region,happiness_rank,happiness_score,economy_gdp_per_capita, social_support, health_life_expectancy, freedom, trust_government_corruption, generosity, dystopia_residual)

e2019 <- read.csv("data/2019.csv") %>% clean_names() %>%select(year,country,region,happiness_rank,happiness_score,economy_gdp_per_capita, social_support, health_life_expectancy, freedom, trust_government_corruption, generosity, dystopia_residual)

f2020 <- read.csv("data/2020.csv") %>% clean_names() %>%select(year,country,region,happiness_rank,happiness_score,economy_gdp_per_capita, social_support, health_life_expectancy, freedom, trust_government_corruption, generosity, dystopia_residual)

g2021 <- read.csv("data/2021.csv") %>% clean_names() %>%select(year,country,region,happiness_rank,happiness_score,economy_gdp_per_capita, social_support, health_life_expectancy, freedom, trust_government_corruption, generosity, dystopia_residual)

h2022 <- read.csv("data/2022.csv") %>% clean_names() %>%select(year,country,region,happiness_rank,happiness_score,economy_gdp_per_capita, social_support, health_life_expectancy, freedom, trust_government_corruption, generosity, dystopia_residual)


a2015$year <- 2015
b2016$year <- 2016
c2017$year <- 2017
d2018$year <- 2018
e2019$year <- 2019
f2020$year <- 2020
g2021$year <- 2021
h2022$year <- 2022

all = rbind(a2015,b2016,c2017,d2018,e2019,f2020,g2021,h2022)
all$happiness_rank <- as.numeric(all$happiness_rank)
all$trust_government_corruption <- as.numeric(all$trust_government_corruption)
all$health_life_expectancy <- as.numeric(all$health_life_expectancy)
all$economy_gdp_per_capita <- as.numeric(all$economy_gdp_per_capita)



# Code for animated plot in 4th tab

#fil=c("Sub-Saharan Africa" )
#all %>% filter(region%in%fil) %>%
  #ggplot(aes(x=happiness_rank,y=as.factor(country),size=happiness_score,color=as.factor(country)))+
  #geom_point()+
  #scale_color_viridis(option="D",discrete = T)+
  #xlab("Koht ??nnelikkuse tabelis")+
  #ylab("Riigid")+
  #ggtitle("Happiness Rank of Asian Countries")+
  #theme_light()+
  #theme(legend.position = "none",plot.title = element_text(face="bold",hjust = 0.5))+
  #transition_time(as.integer(year)) +
  #labs(title = "Aasta: {frame_time}") +
  #shadow_wake(wake_length = 0.2, alpha = FALSE)




# Define UI for app that compares world happiness measures 2015-2022 ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Maailma ??nneraportid 2015-2022"),
  helpText("Maailma Terviseorganisatsiooni poolt kogutud andmete p??hjal maailma ??nnelikkuse raportite tulemuste visualiseerimine"),
  helpText("Autor Qahdit Raag"),
  br(),
  tabsetPanel(
    tabPanel("Rakenduse kirjeldus",
             sidebarLayout(
               sidebarPanel(
                 img(src = "pilt.png", height = 180, width = 200)
               ),
               mainPanel(
                 h3 ("Projekti kirjeldus"),
                 p("Maailma ??nnelikkuse raport on globaalse ??nnelikkuse uurimus, kus on hinnatud 156 riigi heaolutaset. Uuring v??imaldab valitsustel, kogukondadel ja organisatsioonidel kasutada sobivaid andmeid ??nnelikkuse registreerimiseks, et luua paremad elutingimusi. Aruandes vaadeldakse t??nap??eva maailma ??nnelikkuse seisundit ja seda, kuidas teadlased selgitavad ??nnelikkust isiklikul ning riiklikul tasandil." ),
                 p("Projekti koostamise aluseks on v??etud Maailma Terviseorganisatsiooni poolt koostatud iga-aastane maailma ??nnelikkuse raport (World Happiness Report). Andmed p??rinevad veebikeskkonnas Kaggle ning on esitatud .csv formaadis. Andmed on leitavad:"),a("https://www.kaggle.com/datasets/mathurinache/world-happiness-report") ,
                 br(),
                 br(),
                 br(),
                 h4 ("Projekti eesm??rgid:"),
                 p(" - anda ??levaade aastatel 2015-2022 Maailma Terviseorganisatsiooni poolt koostatud raportite andmestikust"),
                 p(" - anal????sida ning visualiseerida: "),
                 p("     1. ??nneindeksit seletavate tunnuste osakaalu eri riikides"),
                 p("     2. 20 k??ige ??nnetuma ja ??nnelikuma riigi j??rjestust"),
                 p("     3. ??nneindeksi seoseid eri tunnuste vahel"),
                 p("     4. Kuidas on muutunud riikide kohad pingereas aja jooksul"),
                 br(),
                 h4 ("Rakenduse ??levaade"),
                 h5 ("Vaheleht 1: Andmed"),
                 p("- tunnuste kirjeldus, andmetabel ning selle struktuur"),
                 h5 ("Vaheleht 2: ??nnevalem"),
                 p("- ??nneindeksi v????rtust seletavate tunnuste osakaal riigiti"),
                 h5 ("Vaheleht 3: J??rjestus"),
                 p("- 20 k??ige ??nnelikuma ning ??nnetuma riigi j??rjestus eri aastatel"),
                 h5 ("Vaheleht 4: ??nneindeksi seosed"),
                 p("- ??nneindeksi seosed eri tunnuste vahel"),
                 h5 ("Vaheleht 5: ??nnelikkuse muutus ajas"),
                 p("- riikide pingerea j??rjestuse muutus 2015-2022")
               ))),
    
    tabPanel("Andmed",
             sidebarLayout(
               sidebarPanel(h3("??lemaailmne ??nnelikkus 2015-2022"),
                            p("Andmed on kogutud 156 eri riigist aastatel 2015-2022"),
                            h3("Andmete ??levaade"),
                            p("Andmestikus on kokku 12 tulpa ja 1251 rida. Andmed on kogutud k??sitluse teel ning vastajatel on palutud hinnata oma heaolu 10 palli skaalal, kus 10 on k??ige parem tulemus. ??nneindeksi tulemust aitavad seletada kuus m????detavat tunnust ning kaugus d??stoopia v????rtusest."),
                            h3("Tunnused"),
                            p(strong("year"),": aasta"),
                            p(strong("country"),": riik"),
                            p(strong("region"),": regioon"),
                            p(strong("happiness_rank"),": koht ??nnelikkuse pingereas"),
                            p(strong("happiness_score"),": ??nnelikkuse indeks"),
                            p(strong("economy_gdp_per_capita"),": SKP elaniku kohta"),
                            p(strong("social_support"),": sotsiaalne toetus (kas h??tta sattumise korral oleks vastajal sugulasi v??i s??pru, kelle poole p????rduda) "),
                            p(strong("health_life_expectancy"),": tervelt elatud eluea pikkus"),
                            p(strong("freedom"),":vabadus teha oma elus valikuid (kas vastaja tunneb, et tal on vabadus ise oma elu kohta otsuseid teha)"),
                            p(strong("trust_government_corruption"),":usaldus korruptsiooni puudumise ja valitsuse vastu (kas vastaja tunneb, et tema valitsus on korrupeerunud)"),
                            p(strong("generosity"),": lahkus (kas vastaja on viimase kuu jooksul heategevuseks raha annetanud)"),
                            p(strong("dystopia_residual"),": kaugus d??stoopiast (d??stoopia on v??jam??eldud riik, kus on k??igi kuue m????detava tunnuse v????rtus k??ige madalam)")
               ),
               mainPanel(br(),
                         h4("Andmetabel"),
                         dataTableOutput("tabel"),
                         br(),
                         h4("Andmestiku kirjeldus"),
                         htmlOutput("profileSummary")
        
             ))),
    tabPanel("??nnevalem",sidebarLayout(
      sidebarPanel(selectInput("var31",
                               label = h3("Vali aasta"),
                               choices = list("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")),
                   br(),
                   radioButtons("var32",
                                label = "Vali regioon",
                                choices  = list("Aasia" = "fil1", "Ameerika ja okeaania" = "fil2", "Kesk-ja Ida-Euroopa" = "fil3", "L????ne-Euroopa"= "fil4", "L??his-Ida ja P??hja Aafrika"= "fil5", "Kesk- ja L??una-Aafrika"= "fil6"),
                                selected = "fil1")),
      
      mainPanel(plotlyOutput("distPlot3", width = "100%",
                             height="auto"))
    )
            ),
    tabPanel("J??rjestus", sidebarLayout(
      sidebarPanel(radioButtons("var1", label = "Vali j??rjestus",
                                choices = list("K??ige ??nnelikumad" = 1 , "K??ige ??nnetumad" = 2), 
                                selected = 1),
                   br(),
                   selectInput("var",
                               label = "Vali aasta",
                               choices = list("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"))),
      mainPanel(plotlyOutput("distPlot", width = "100%",
                             height="auto")
      ))
    ),
    tabPanel("??nneindeksi seosed",
             sidebarLayout(
               sidebarPanel(sliderInput("var21", label = h3("Vali aasta"), min = 2015, 
                                        max = 2022, sep = "",  animate = animationOptions(interval = 700, loop = FALSE), value = 2015),
                            br(),
                            radioButtons("var22", label = "Vali s??ltuvus",
                                         choices = c("GDP" = 1 , "Eluea pikkus" = 2, "Usaldus korruptsiooni puudumise ja valitsuse vastu" = 3 ,"Lahkus" = 6, "Vabadus teha oma elus valikuid" = 4, "Sotsiaalne toetus" = 5), 
                                         selected = 1),
                            checkboxInput("checkbox", label = "Trendijoon", value = FALSE)
               ),
               
               mainPanel(plotlyOutput("distPlot2", width = "100%",
                                      height="auto"))
             )),
    tabPanel("??nnelikkuse muutus ajas",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var4",
                             label = "Vali regioon",
                             choices = list("Aasia", "Ameerika ja okeaania", "Kesk-ja Ida-Euroopa", "L????ne-Euroopa", "L??his-Ida ja P??hja Aafrika", "Kesk- ja L??una-Aafrika"))
               ),
               mainPanel(imageOutput("distPlot4") )
             ))))

  


# Define server logic required to illustrate the results ----
server <- function(input, output, session) {
  
  #Vaheleht 1
  
  output$tabel <- renderDataTable( all, options =
                                     list(searching = FALSE,ordering=F, lengthMenu = c(5, 10, 20),
                                          pageLength = 5,scrollX = TRUE))
  
  output$profileSummary <- renderUI({
    summarytools::view(summarytools::dfSummary(all[,-c(1,2,3)]), method = "render")})
  
  
  #Vaheleht 2
  
  output$distPlot3 <- renderPlotly ({
    
    if(input$var32=="fil1") fil=c("Southeastern Asia", "Eastern Asia", "Southern Asia", "East Asia ", "Southeast Asia", "South Asia")
    if(input$var32=="fil2") fil=c("North America","Latin America and Caribbean", "North America and ANZ")
    if(input$var32=="fil3") fil=c("Central and Eastern Europe", "Commonwealth of Independent States" )
    if(input$var32=="fil4") fil=c("Western Europe" )
    if(input$var32=="fil5") fil=c("Middle East and Northern Africa", "Middle East and North Africa" )
    if(input$var32=="fil6") fil=c("Sub-Saharan Africa" )
    
    
    
    
    all %>%
      filter (year == as.character(input$var31),region%in%fil ) %>%
      pivot_longer(cols = c("economy_gdp_per_capita", "health_life_expectancy", "freedom", "trust_government_corruption", "generosity", "social_support", "dystopia_residual" ), names_to = "value_name" , values_to = "values"  ) %>%
      mutate(country=reorder(country,happiness_score))%>%
      ggplot(aes(fill = as.factor(value_name), x=country,y=values))+
      geom_bar( stat="identity")+
      coord_flip()+
      xlab("Riigi nimi")+
      ylab("??nnelikkuse indeks")+
      ggtitle(paste("Eri tunnuste osakaal ??nneindeksist aastal", input$var31))+
      labs(fill='V????rtused') 
      
    
  })
  

 
# Vaheleht 3
  
  output$distPlot <- renderPlotly({
    
    if (input$var1 == 1) {
      all %>%
        filter (year == input$var) %>%
        top_n(20,happiness_score) %>%
        mutate(country=reorder(country,happiness_score))%>%
        ggplot(aes(x=country,y=happiness_score,fill=happiness_score))+
        geom_bar(stat = "identity")+
        scale_fill_viridis(option = "D")+
        xlab("Riigi nimi")+
        ylab("??nnelikkuse indeks")+
        ggtitle(paste("20 k??ige ??nnelikkumat riiki", input$var))+
        coord_flip()+
        theme_light()+
        theme(plot.title = element_text(face="bold",hjust = 0.5),legend.title = element_blank())+
        labs(fill='??nneindeks',) 
    } else {
      all %>%
        filter (year == input$var) %>%
        top_n(-20,happiness_score) %>%
        mutate(country=reorder(country,-happiness_score))%>%
        ggplot(aes(x=country,y=happiness_score,fill=happiness_score))+
        geom_bar(stat = "identity")+
        scale_fill_viridis(option = "F")+
        xlab("Riigi nimi")+
        ylab("??nnelikkuse indeks")+
        ggtitle(paste("20 k??ige ??nnetumat riiki", input$var))+
        coord_flip()+
        theme_light()+
        theme(plot.title = element_text(face="bold",hjust = 0.5),legend.title = element_blank())+
        labs(fill='??nneindeks') 
    }
  })
  
  #Vaheleht 4
  output$distPlot2 <- renderPlotly({
    
    if (input$checkbox == TRUE){
    
    if (input$var22 == 1){
      all %>%
        filter (year == as.character(input$var21)) %>%
        ggplot(aes(x = happiness_score, 
                   y = economy_gdp_per_capita,
                   color = economy_gdp_per_capita)) +
        geom_point() +
        geom_smooth(method=lm)+
        labs(title = paste("??nneindeksi suhe v????rtusesse: SKP elaniku kohta",input$var21), 
             x = "??nnelikkuse indeks", 
             y = "SKP elaniku kohta",
             color = "SKP elaniku kohta") +
        scale_color_gradient(low = "#000428", high = "#a1c4fd")
        
    } else if (input$var22 == 2) {
      all %>%
        filter (year == as.character(input$var21)) %>%
        ggplot(aes(x = happiness_score, 
                   y = health_life_expectancy, 
                   color = health_life_expectancy)) +
        geom_point() +
        geom_smooth(method=lm)+
        labs(title = paste("??nneindeksi suhe v????rtusesse: eluea pikkus",input$var21), 
             x = "??nnelikkuse indeks", 
             y = "Eluea pikkus",
             color = "Eluea pikkus") +
        scale_color_gradient(low = "#000428", high = "#a1c4fd")
      
    } else if (input$var22 == 3) {
      all %>%
        filter (year == as.character(input$var21)) %>%
        ggplot(aes(x = happiness_score, 
                   y = trust_government_corruption, 
                   color = trust_government_corruption)) +
        geom_point() +
        geom_smooth(method=lm)+
        labs(title = paste("??nneindeksi suhe v????rtusesse: usaldus korruptsiooni puudumise ja valitsuse vastu",input$var21), 
             x = "??nnelikkuse indeks", 
             y = "Usaldus",
             color = "Usaldus",
             tag = "A") +
        scale_color_gradient(low = "#000428", high = "#a1c4fd")
      
    } else if (input$var22 == 4) {
      all %>%
        filter (year == as.character(input$var21)) %>%
        ggplot(aes(x = happiness_score, 
                   y = freedom, 
                   color = freedom)) +
        geom_point() +
        geom_smooth(method=lm)+
        labs(title = paste("??nneindeksi suhe v????rtusesse: vabadus teha oma elus valikuid",input$var21), 
             x = "??nnelikkuse indeks", 
             y = "Vabadus",
             color = "Vabadus",
             tag = "A") +
        scale_color_gradient(low = "#000428", high = "#a1c4fd")
      
    }else if (input$var22 == 5) {
      all %>%
        filter (year == as.character(input$var21)) %>%
        ggplot(aes(x = happiness_score, 
                   y = social_support, 
                   color = social_support)) +
        geom_point() +
        geom_smooth(method=lm)+
        labs(title = paste("??nneindeksi suhe v????rtusesse: sotsiaalne toetus",input$var21), 
             x = "??nnelikkuse indeks", 
             y = "Sotsiaalne toetus",
             color = "Sotsiaalne toetus",
             tag = "A") +
        scale_color_gradient(low = "#000428", high = "#a1c4fd")
      
    }else {
      all %>%
        filter (year == as.character(input$var21)) %>%
        ggplot(aes(x = happiness_score, 
                   y = generosity, 
                   color = generosity)) +
        geom_point() +
        geom_smooth(method=lm)+
        labs(title = paste("??nneindeksi suhe v????rtusesse: lahkus",input$var21), 
             x = "??nnelikkuse indeks", 
             y = "Lahkus",
             color = "Lahkus") +
        scale_color_gradient(low = "#000428", high = "#a1c4fd")
      
    }} else {
      if (input$var22 == 1){
        all %>%
          filter (year == as.character(input$var21)) %>%
          ggplot(aes(x = happiness_score, 
                     y = economy_gdp_per_capita,
                     color = economy_gdp_per_capita)) +
          geom_point() +
          labs(title = paste("??nneindeksi suhe v????rtusesse: SKP elaniku kohta",input$var21), 
               x = "??nnelikkuse indeks", 
               y = "SKP elaniku kohta",
               color = "SKP elaniku kohta") +
          scale_color_gradient(low = "#000428", high = "#a1c4fd")
        
      } else if (input$var22 == 2) {
        all %>%
          filter (year == as.character(input$var21)) %>%
          ggplot(aes(x = happiness_score, 
                     y = health_life_expectancy, 
                     color = health_life_expectancy)) +
          geom_point() +
          labs(title = paste("??nneindeksi suhe v????rtusesse: eluea pikkus",input$var21), 
               x = "??nnelikkuse indeks", 
               y = "Eluea pikkus",
               color = "Eluea pikkus") +
          scale_color_gradient(low = "#000428", high = "#a1c4fd")
        
      } else if (input$var22 == 3) {
        all %>%
          filter (year == as.character(input$var21)) %>%
          ggplot(aes(x = happiness_score, 
                     y = trust_government_corruption, 
                     color = trust_government_corruption)) +
          geom_point() +
          labs(title = paste("??nneindeksi suhe v????rtusesse: usaldus korruptsiooni puudumise ja valitsuse vastu",input$var21), 
               x = "??nnelikkuse indeks", 
               y = "Usaldus",
               color = "Usaldus",
               tag = "A") +
          scale_color_gradient(low = "#000428", high = "#a1c4fd")
        
      } else if (input$var22 == 4) {
        all %>%
          filter (year == as.character(input$var21)) %>%
          ggplot(aes(x = happiness_score, 
                     y = freedom, 
                     color = freedom)) +
          geom_point() +
          labs(title = paste("??nneindeksi suhe v????rtusesse: vabadus teha oma elus valikuid",input$var21), 
               x = "??nnelikkuse indeks", 
               y = "Vabadus",
               color = "Vabadus",
               tag = "A") +
          scale_color_gradient(low = "#000428", high = "#a1c4fd")
        
      }else if (input$var22 == 5) {
        all %>%
          filter (year == as.character(input$var21)) %>%
          ggplot(aes(x = happiness_score, 
                     y = social_support, 
                     color = social_support)) +
          geom_point() +
          labs(title = paste("??nneindeksi suhe v????rtusesse: sotsiaalne toetus",input$var21), 
               x = "??nnelikkuse indeks", 
               y = "Sotsiaalne toetus",
               color = "Sotsiaalne toetus",
               tag = "A") +
          scale_color_gradient(low = "#000428", high = "#a1c4fd")
        
      }else {
        all %>%
          filter (year == as.character(input$var21)) %>%
          ggplot(aes(x = happiness_score, 
                     y = generosity, 
                     color = generosity)) +
          geom_point() +
          labs(title = paste("??nneindeksi suhe v????rtusesse: lahkus",input$var21), 
               x = "??nnelikkuse indeks", 
               y = "Lahkus",
               color = "Lahkus") +
          scale_color_gradient(low = "#000428", high = "#a1c4fd") 
    }
    
    
    }})
  
 
  
  # Vaheleht 5
  
  output$distPlot4 <- renderImage({ 
    if(input$var4=="Aasia") Leg<-"www/asianPlot.gif"
    if(input$var4=="Ameerika ja okeaania") Leg<-"www/ameerika.gif"
    if(input$var4=="Kesk-ja Ida-Euroopa") Leg<-"www/kesjaidaEuroopa.gif"
    if(input$var4=="L????ne-Euroopa") Leg<-"www/laaneEuroopa.gif"
    if(input$var4=="L??his-Ida ja P??hja Aafrika") Leg<-"www/lahisidajapohjaaafrika.gif"
    if(input$var4=="Kesk- ja L??una-Aafrika") Leg<-"www/aafrika.gif"
   
    list(src=Leg)
  })

}


# Run the app ----
shinyApp(ui = ui, server = server)




