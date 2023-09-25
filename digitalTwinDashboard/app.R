## app.R ##
library(shiny)
library(shinydashboard)
library(gganatogram)
library(dplyr)
library(plotly)
library(shinyscreenshot)
library(shinyjs)
library(reticulate)

#charge python script for sending data to start recording
source_python('data/oscStart.py')

#create a default csv
tibble(organ ="nose", value=1, time=0) %>%
  write.csv("data/digitalFilter.csv", row.names = FALSE)

#start defining the user interface
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Digital Twin"),
  
  #define scene and passino variables
  dashboardSidebar(
    selectInput("scene", "Select Scene:", choices = c("SCENE 1", "SCENE 2", "SCENE 3", "SCENE 4")),
    selectInput("passion", "Represented Passion:", choices = c("Admiration", "Love", "Hate", "Desire",
                                                                "Happiness", "Sadness")),
    #activate js interactive elements of the shinyjs library
    useShinyjs(),
    
    #change some css elements
    tags$head(tags$style("#sessionTitle{color: #3c8dbc;
                                 font-size: 20px;
                                 font-style: bold;
                                 }
                         #start {background-color:black;
                                  color: white;}
                         #pause {background-color:black;
                                  color: white;}
                         #stop {background-color:black;
                                  color: white;}"
    )
    ),
    
    #define sidebar menu
    sidebarMenu(
      menuItem("Physiology", tabName="physiology", icon = icon("heart-pulse")),
      menuItem("Brain", tabName="brain", icon=icon("brain")),
      menuItem("Words", tabName="words", icon = icon("a")),
      
      actionButton("start", "Start", width = "87%"),
      actionButton("pause", "Pause", width = "87%"),
      actionButton("stop", "Stop", width = "87%"),
      

      #define sidebar inputs (not needed for the final script) 
      sliderInput("brainColor",
                      "Brain",  min = 0, max = 1, value = runif(1, min=0, max=1)),
      sliderInput("heartColor",
                      "Heart",
                      min = 0, max = 1, value = runif(1, min=0, max=1)),
      sliderInput("skinColor",
                      "Skin", min = 0, max = 1, value = runif(1, min=0, max=1)),
      sliderInput("circulationColor",
                      "Circulation", min = 0, max = 1, value = runif(1, min=0, max=1)),
      sliderInput("bodyColor",
                      "Temperature",
                      min = 0, max = 1, value = round(runif(1, min=0, max=1), 1), step = 0.1),
      screenshotButton(selector = "section.content", filename = paste0("NHLab_"
                                                                       #, init$time
                                                                       ), timer = 0.5, ns = shiny::NS(NULL))
    )
  ),
  
  #define the body
  dashboardBody(
    textOutput("sessionTitle"),
    #tabitems
    tabItems(
      tabItem(tabName = "physiology", 
              column(4, 
                     box(title = "Biometrics", width = NULL, solidHeader = TRUE, status = "primary",
                     plotOutput("distPlot"))),
              column(8, 
                     box(title = "Biometrics along time", width = NULL, solidHeader = TRUE, status = "primary",
                         plotlyOutput("distPlot2"))),
              infoBoxOutput("emotionDetected")
              )#,
      # tabItem(tabName = "brain"
      #         ),
      # tabItem(tabName = "words" 
      #         )
    ),
    fluidRow(), 
    #the row for the emotions and concentration prediction
    fluidRow()
    )
  )

###server function
server <- function(input, output, session) {
  
  #define default reactive values for user and time
  user <- reactiveValues(time=format(Sys.time(), "%H%M%S_%Y%m%d"))
  init <- reactiveValues(time=Sys.time())
  
  #change the title when the scene changes
  observeEvent(input$scene, {
    output$sessionTitle <- renderText({paste("USER", user$time, 
                                             input$scene, input$passion, sep = " ")})
    })
  
  #create a set of colors for body plottinig
  bodyColor <- reactive( 
    ifelse (input$bodyColor==0, "#0d0887ff", 
            ifelse(input$bodyColor==0.1, "#450597ff", 
                   ifelse(input$bodyColor==0.2, "#6a00a8ff",
                          ifelse(input$bodyColor==0.3, "#91169cff",
                                 ifelse(input$bodyColor==0.4, "#b12a90ff",
                                        ifelse(input$bodyColor==0.5, "#cb497aff",
                                               ifelse(input$bodyColor==0.6, "#e16462ff",
                                                      ifelse(input$bodyColor==0.7, "#ef8650ff",
                                                             ifelse(input$bodyColor==0.8, "#fca636ff",
                                                                    ifelse(input$bodyColor==0.9, "#f8d02fff",
                                                                           "#f0f921ff"))))))))))
  )
  
  #create a reactive timer for simulating data string
  autoInvalidate <- reactiveTimer(2000)
  
  observe({
    autoInvalidate()
    
    updateNumericInput(session, "skinColor", value = runif(1, min=0, max=1))
    updateNumericInput(session, "brainColor", value = runif(1, min=0, max=1))
    updateNumericInput(session, "heartColor", value = runif(1, min=0, max=1))
    updateNumericInput(session, "circulationColor", value = runif(1, min=0, max=1))
    updateNumericInput(session, "bodyColor", value = round(runif(1, min=0, max=1), 1))
  })
  
  digitalFilter <- reactive ({
    #select specific organs from the gganatogram library for plotting
    #the values of skin and nose were chosen for normalize the other ones, and set to 0 and 1
    hgMale_key %>% 
      filter(organ %in% c("skin", "heart", "brain", "tonsil", "leukocyte", "nose")) %>% 
      select(organ, value) %>%
      mutate(value=c(1, 
                     input$skinColor, 
                     input$brainColor,
                     input$heartColor,
                     input$circulationColor,
                     0)) %>% 
      mutate(time=difftime(Sys.time(), init$time))
  }
  )
  
  observeEvent(digitalFilter(),
               write.table(digitalFilter(), 
                           "data/digitalFilter.csv", 
                           append = TRUE, 
                           row.names = FALSE,
                           col.names = FALSE, 
                           sep = ",")
               )
  
  output$distPlot <- renderPlot({
    digitalFilter() %>% 
      gganatogram(organism = "human", sex = "male",
                  fill = "value", outline = TRUE,
                  fillOutline = bodyColor(), ggplot2_only = FALSE) +
      theme_void() + 
      coord_fixed()+ 
      scale_fill_viridis_c(begin = 0, 
                           end = 1,
                           option="plasma")
    })
  
  output$emotionDetected <- renderInfoBox(infoBox("Emotion detected", paste0("SURPRISE", " ", 10 * 2, "%"),
                                                  fill=TRUE,
                                                  color = "blue",
                                                  icon = icon("heart-pulse")))
  
  #cumulative animation https://campus.datacamp.com/courses/intermediate-interactive-data-visualization-with-plotly-in-r/animating-graphics?ex=12
  
  values <- reactive({
    autoInvalidate()
    read.csv("data/digitalFilter.csv", row.names = NULL) %>% as_tibble()
    })
  
  output$distPlot2 <- renderPlotly({
    timePlot <- values() %>% 
      filter(!organ%in%c("nose", "tonsil")) %>% #these values where chosen just for start the plot
      ggplot(aes(time, value, group=1))+
      geom_line()+
      ylim(0,1)+
      xlim(0,180)+
      xlab("")+
      ylab("")+
      facet_wrap(~organ, ncol = 1)
    
    ggplotly(timePlot) %>% layout(showlegend = FALSE)
  })
  
  envio <- reactiveValues(start=FALSE)
  shinyjs::disable("pause")
  
  observeEvent(input$start, {
    user$time <- format(Sys.time(), "%H%M%S_%Y%m%d")
    init$time <- Sys.time()
    envio$start <- TRUE
    shinyjs::disable("scene")
    shinyjs::disable("start")
    shinyjs::disable("passion")
    shinyjs::enable("pause")
    runjs('document.getElementById("start").style.backgroundColor = "green";')
  })
  
  times <- reactiveValues(clicks = 0) # Defining & initializing the reactiveValues object
  
  #observe the Next button and restart it when it reaches the length of the input$number
  observeEvent(input$pause, {
    envio$start <- !envio$start
    times$clicks <- times$clicks + 1 
    })
  
  observeEvent(input$stop, {
    times$clicks <- 0 
  })
  
  observeEvent(input$pause, {
    if(times$clicks%%2==0)  {
      shinyjs::disable("scene")
      runjs('document.getElementById("pause").style.backgroundColor = "black";')
    } else {
      shinyjs::enable("scene")
      runjs('document.getElementById("pause").style.backgroundColor = "orange";')
    }
    }
    )
  
  observeEvent(input$stop, {
    shinyjs::enable("start") 
    shinyjs::enable("scene")
    shinyjs::enable("passion")
    shinyjs::disable("pause")
    envio$start <- FALSE
    runjs('document.getElementById("start").style.backgroundColor = "black";')
    runjs('document.getElementById("pause").style.backgroundColor = "black";')
  })
  
  #sent the single text by OSC with the python function "enviar"
  observeEvent (input$start, {
    input_value <- envio$start
    userId <- paste(user$time,input$scene, sep = "_")
    py$enviar(input_value)
    py$userId(userId)
  }
  )
  
  observeEvent (input$pause, {
    input_value <- envio$start
    py$enviar(input_value)
  }
  )
  
  observeEvent (input$stop, {
    input_value <- envio$start
    py$enviar(input_value)
  }
  )
  
  
}
  

shinyApp(ui, server)