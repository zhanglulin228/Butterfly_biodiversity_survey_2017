library(ggplot2)
library(lubridate)
library(dplyr)
library(leaflet)
library(scales)# color
library(shiny)
butterfly <- read.csv("Butterfly_biodiversity_survey_2017_PE2.csv")
#change date type from chr -> posixlt
butterfly$Datetime = strptime(butterfly$Datetime, format = "%m/%d/%Y %I:%M:%S %p")

#Q2
Q2 <- butterfly %>% 
  filter(year(Datetime) == 2017) %>% #find year is 2017
  group_by(Site) %>%  #clo is sit
  summarise(TotalBFCount = sum(ButterflyCount)) #add col TotalBFCount base on ButterflyCount
Q2_sort <- Q2[order(Q2$TotalBFCount,decreasing = TRUE),] # sort by decreasing
Q2_top5 <- Q2_sort[1:5,]# find first 5 data

Q2_ggplot <- Q2_top5 %>%  #geom_bar has count , stat need be identity
  ggplot(aes(Site, TotalBFCount))+ geom_bar(stat = "identity") + coord_flip()# flip is change x,and y forThe x coordinates overlap  
Q2_ggplot

#Q3
Q3 <- butterfly %>% 
  filter(year(Datetime) == 2017) %>% #find year is 2017
  filter(Site %in% Q2_top5$Site) %>% 
  group_by(Site,date(Datetime)) %>% 
  summarise(TotalBFCount = sum(ButterflyCount)) #add col TotalBFCount base on ButterflyCount

Q3_ggplot <- Q3 %>% 
  ggplot(aes(x=`date(Datetime)`, y=TotalBFCount))+
  geom_line(aes(color=Site))+
  theme(legend.position = "bottom" )
Q3_ggplot


#Q4
Q4 <- butterfly %>%
  filter(year(Datetime) == 2017) %>% 
  group_by(Site) %>% 
  summarise(Lat=mean(Lat),Lon=mean(Lon), TotalBFcount = sum(ButterflyCount))

color <- hue_pal()(length(Q4$Site))
pal <- colorFactor(color,domain = Q4$Site)#give color for each color
#https://www.rdocumentation.org/packages/scales/versions/0.4.1/topics/hue_pal
#https://rstudio.github.io/leaflet/colors.html



Q4_map <- Q4 %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng = ~Lon,
    lat = ~Lat,
    popup = ~paste('<b>Site name:</b>',Site,'<b>TOTAL BUTTERFLY COUNT:</b>', TotalBFcount),# symbol is clicked, show a tooltip, <b>,and</b> is for bold
    radius = ~TotalBFcount,
    color = ~pal(Site)
  )
Q4_map



#Q5

range_slider <- range(Q4$TotalBFcount)
range_slider[2]

ui <- fixedPage(
  fixedRow(
    titlePanel("SURVEY OF BUTTERFLIES IN MELBOURNE 2017")
  ),
  
  fixedRow("[observed butterfly biodiversity and flower-butterfly interactions in the City of Melbourne between January - March 2017]"),
  
  fixedRow(
    
    column(
      4,
      tags$b('Location of the survey'),#bold of the word in web
      tags$br(),#A newline "br"
      '[the spatial positions of all 15 sites in the dataset]'
    ),
    
    column(
      8,
      sliderInput('range',"Range", min = range_slider[1], max=range_slider[2],value = range_slider),
      leafletOutput('Q4_map')
    )
  ),
  
  fixedRow(
    column(
      4,
      plotOutput('Q2_ggplot')
    ),
    column(
      4,
      tags$b('Top Sites for Butterflies'),#bold of the word in web
      tags$br(),
      '[top 5 sites in the data based on the total number of butterflies observed in 2017, and total number of butterflies observed each day at the same 5 sites in Step 2 over the course of 2017 ]'
    ),
    column(
      4,
      plotOutput('Q3_ggplot')
    )
  )
)



server <- function(input, output){
  output$Q2_ggplot <- renderPlot(
    Q2_ggplot
  )
  output$Q3_ggplot <- renderPlot(
    Q3_ggplot
  )
  output$Q4_map <- renderLeaflet(
    Q4 %>% 
      filter(TotalBFcount>=input$range[1],
             TotalBFcount<=input$range[2]) %>% #Scale and map are linked
      leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(
        lng = ~Lon,
        lat = ~Lat,
        popup = ~paste('<b>Site name:</b>',Site,'<b>TOTAL BUTTERFLY COUNT:</b>', TotalBFcount),# symbol is clicked, show a tooltip, <b>,and</b> is for bold
        radius = ~TotalBFcount,
        color = ~pal(Site)
      )
  )
  
}

shinyApp(ui,server)

