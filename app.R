library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)


ui <- dashboardPage(
    dashboardHeader(title = "World Happiness Index"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "page1", icon = icon("list-alt")),
            menuItem("Trend Over Time", tabName = "page2", icon = icon("line-chart")),
            menuItem("Map", tabName = "page3", icon = icon("map-o")),
            menuItem("Explorer", tabName = "page4", icon = icon("area-chart")),
            menuItem("Data", tabName = "page5", icon = icon("database"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("page1",
                    fluidRow(
                        box(
                            title = "About the Application", solidHeader = TRUE,
                            status = "success", width = 12, collapsible = TRUE,
                            column(12, 
                                   tags$div(
                                       tags$span(
                                           "This is the shiny dashboard application designed to explore the happiness index for countries all over the world with", tags$strong("5"), "factors, including:", style = "font-size:16px"),
                                       br(), br(),
                                       fluidRow(column(6, tags$li("Economy, GDP per capita"), tags$li("Social support"), tags$li("Generosity")), 
                                                column(6, tags$li("Freedom to make life changes"), tags$li("Corruption Perception"))),
                                       br(),
                                       fluidRow(tags$mark(tags$i("* Please note that the original editors for this data are", tags$strong("John Helliwell, Richard Layard, Jeffrey D. Sachs, and Jan Emmanuel De Neve, Co-Editors; Lara Aknin, Haifang Huang and Shun Wang, Associate Editors; and Sharon Paculor, Production Editor")))
                                       )
                                   )
                            )
                        )
                    ),
                    fluidRow (
                        box(
                            title = "About the Dataset", solidHeader = TRUE,
                            status = "primary", width = 12, collapsible = TRUE,
                            column(12, 
                                   tags$div(
                                       tags$span("This dataset is about the bliss score for different countries in the world. The score are based on answers to the most life evaluation address inquired within the Gallup World Survey, and are from broadly agent tests for the a long time 2013-2016 and utilize the Gallup weights to create the gauges agent.
                                    Five variables – Economy, Social support, Freedom to change, Corruption, and Generosity – contribute to making life assessments higher in each nation than they are in Dystopia, a theoretical nation that has values rise to to the world’s least national midpoints for each of the six variables. "),
                                       br(), br(),
                                       tags$li(tags$strong("Source: "),tags$a(href = "https://www.kaggle.com/mathurinache/world-happiness-report-20152021")),
                                       tags$li("The filtered dataset for this application contains total",tags$strong("935"), "cases (in ", tags$strong("8"), "columns) from 2015 to 2020.")
                                   )
                            )
                        )
                    ),
                    fluidRow (
                        box(
                            title = "About Us", solidHeader = TRUE, 
                            status = "info", width = 12, collapsible = TRUE,
                            column(12, 
                                   tags$div(
                                       fluidRow (
                                           column(8, tags$div("Team members are all from MSBA program at Carey Business School, Johns Hopkins University:", style = "font-size:16px"),
                                                  br(),
                                                  tags$li(tags$strong("Cheng Qian")),
                                                  tags$li(tags$strong("Jiarui Qi")), 
                                                  tags$li(tags$strong("Alison Yin")), 
                                                  tags$li(tags$strong("Jiaxin Feng")), 
                                                  tags$li(tags$strong("Yue Cheng"))
                                           )
                                       )
                                   ),
                                   br(),
                            )
                        )
                    )
            )
            
            tabItem(tabName = "page2",
                    sliderInput("year", "Year:", min = 2015, max = 2020, value = 1, 
                                step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                    numericInput("num", "Number of countries to show:",
                                 value = 5, min = 0, max = 15, step = 1),
                    plotOutput("plot1")
                    ),
            tabItem(tabName = "page3",
                    sliderInput("year","Year:", min=2005, max=2020, value = 1, step=1, animate = animationOptions(interval = 2000, loop = FALSE)),
                    leafletOutput("myMap", width="100%")
                    ),
            tabItem(tabName = "page4",
                    checkboxInput("holiday", label = "Show holidays", value = FALSE),
                    plotlyOutput("plot2", height = 500)),
            tabItem(tabName = "page5", dataTableOutput("myTable"),
                            (a("Data Source",href='https://www.kaggle.com/mathurinache/world-happiness-report?select=2015.csv', target="_blank"))
                            
                    )
            )
        )
    )



server <- function(input, output, session) {

    data <- read_csv("happi_data_15to20.csv")
    
    
    output$plot1 = renderPlot({
        thisYear = input$year
        filteredData = data %>% filter(year==thisYear)
        cnum=input$num
        countriesToShow= filteredData %>% arrange(Happiness_Rank) %>% pull(Country) %>% head(cnum)
        
        ggplot(data=filteredData,aes(x = "Health_(Life_Expectancy)", y ="Happiness_Score" )) +
            geom_point(mapping=aes(size="Economy_(GDP_per_Capita)"))+
            annotate("text", size=20, x=7.5, y=35000,color="grey80",label =input$year )+
            theme_wsj()+
            theme(plot.title = element_text(size = rel(0.6)), 
                  legend.title = element_text(size = rel(0.5)), 
                  axis.title=element_text(face = "bold", size = rel(0.6)))+
            scale_size_discrete(limits = c(0, 2))+ 
            scale_y_discrete(limits = c(0,10))+ 
            scale_x_discrete(limits = c(0,2))+
            labs(
                title = "Average number of years of education vs GDP per capita",
                color = "Number of out-of-school child is higher among:",
                size = "population in millions:",
                x="Average number of years of education",
                y="GDP per capita")+
            geom_text(data= filteredData %>% filter(Country %in% countriesToShow), 
                      mapping = aes(label = Country))
        
        
    })
    
    output$plot2 = renderPlotly({
        
    })
    
    output$myMap = renderLeaflet({
        loc_data <- data%>% 
            group_by(lng=round(Long,3),lat=round(Lat,3))
        loc_data <-  loc_data%>%
            mutate(popusText=paste("In", year, ", the Happiness Score of", `Country`, "is", `Happiness_Score`,
                                   ", its rank is", `Happiness_Rank`),
                   rank=1/Happiness_Rank)
                   m=loc_data %>%
                       filter(year==input$year) %>%
                       leaflet() %>% 
                       addTiles()%>%
                       setView(46.82,30.22, zoom=2)%>%
                       addCircleMarkers(lng = ~lng, 
                                  lat = ~lat, 
                                  fillOpacity = ~rank*3, 
                                  fillColor = "red", 
                                  label = ~Country,
                                  popup = ~popusText)
                   m
        
    })
    
    output$myTable = renderDataTable({
        return(datatable(data, rownames= FALSE))
    })

}

shinyApp(ui = ui, server = server)
