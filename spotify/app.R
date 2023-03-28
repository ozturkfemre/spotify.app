library(jsonlite)
library(tidyverse)
library(ggplot2)
library(wesanderson)
library(paletteer) 
library(ggthemes)
library(circular)
library(shiny)
library(shinyWidgets)
library(shinythemes)
# UI
ui <- fluidPage(
  theme = shinytheme(theme = "cosmo"),


  setBackgroundColor(
    color = "#75944BFF",
    gradient = "linear",
    direction = "bottom"
  ),
  
  # File Input
  fileInput(inputId = "file", label = "Upload your StreamingHistory0.json file"),
  
  # Tabs
  navbarPage(
    title = "Spotify Listening History",
    id = "tabs",
    
    # Artists Tab
    tabPanel("Artists",
             fluidRow(
               tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #000000;
                                                  border-top: 1px solid #000039 ;
                                                  border-bottom: 1px solid #000039 ;}
                           .irs-from, .irs-to, .irs-single { background: #000069 }'
               ))
               ),
               column(width = 12,
                      plotOutput("artists_plot", height = "600px"),
                      sliderInput("n_value", "How many artists you want to see:", min = 5, max = 15, step = 1, value = 10)
               )
             )),
    
    # Tracks Tab
    tabPanel("Tracks",
             fluidRow(
               column(width = 12,
                      selectInput("plot_selection", "Select a plot to display",
                                  choices = c("Top Tracks", "Top Tracks From Most Listened Artist")),
                      plotOutput("plot", height = "400px")
               )
             )),
    
    # Listening Habits Tab
    tabPanel("Listening Habits",
             fluidRow(
               column(width = 12,
                      selectInput("plot1_selection", "Select a plot to display",
                                  choices = c("Usage by Day of Week", "Usage by Hour")),
                      plotOutput("plot1", height = "400px")
               )
             ))
  )
)

# Server
server <- function(input, output) {
  
   # Read JSON file
  df <- reactive({
    req(input$file)
    inFile <- input$file
    df <- fromJSON(inFile$datapath, flatten = TRUE)
    df
  })
  
  
 
  # Artists Plot
  output$artists_plot <- renderPlot({
    cp <- c("#202547FF", "#323649FF", "#41474BFF", "#4E5A4CFF", "#5C6C4CFF", "#68804CFF", "#75944BFF", "#81A949FF", "#8EBE45FF", "#9AD340FF", "#A6E939FF", "#B2FF2EFF")
    
    df() %>%
      count(artistName, sort = TRUE) %>%
      top_n(input$n_value) %>%
      mutate(artistName = reorder(artistName, n)) %>%
      ggplot(aes(x = artistName, y = n)) +
      geom_bar(aes(fill=n),   
               stat="identity") +
      scale_fill_distiller(palette= cp) +
      xlab(NULL) +
      coord_flip() +
      labs(x = "Artists",
           title = "Artists you listened most to",
           fill = "Count",
           y = " ") +
      theme_dark()+
      theme(plot.background = element_rect(fill = "gray20")) +
      theme(panel.background = element_rect(fill = "gray20")) + 
      theme(legend.background = element_rect(fill = "gray20")) + 
      theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))
  })
  
  output$plot <- renderPlot({
    cp <- c("#202547FF", "#323649FF", "#41474BFF", "#4E5A4CFF", "#5C6C4CFF", "#68804CFF", "#75944BFF", "#81A949FF", "#8EBE45FF", "#9AD340FF", "#A6E939FF", "#B2FF2EFF")
    
    if (input$plot_selection == "Top Tracks") {
      df() %>%
        count(trackName, sort = TRUE) %>%
        top_n(10) %>%
        mutate(trackName = reorder(trackName, n)) %>%
        ggplot(aes(x = trackName, y = n)) +
        geom_bar(aes(fill=n),   
                 stat="identity") +
        scale_fill_distiller(palette=cp) +
        xlab(NULL) +
        coord_flip() +
        labs(y = " ",
             title = "Tracks you listened most to",
             fill = "Count")+ 
        theme_dark()+
        theme(plot.background = element_rect(fill = "gray20")) +
        theme(panel.background = element_rect(fill = "gray20")) + 
        theme(legend.background = element_rect(fill = "gray20")) + 
        theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))
    } else {
      topartist <- df() %>%
        count(artistName, sort = TRUE) %>%
        top_n(1) %>%
        mutate(artistName = reorder(artistName, n))
      
      topartist <- topartist$artistName
      df() %>%
        filter(artistName == topartist) %>%
        count(trackName, sort = TRUE) %>%
        top_n(10) %>%
        mutate(trackName = reorder(trackName, n)) %>%
        ggplot(aes(x = trackName, y = n)) +
        geom_bar(aes(fill=n),   
                 stat="identity") +
        scale_fill_distiller(palette=cp) +
        xlab(NULL) +
        coord_flip() +
        labs(y = " ",
             title = "Top tracks by your most listened artist",
             fill = "Count") +
        theme_dark()+
        theme(plot.background = element_rect(fill = "gray20")) +
        theme(panel.background = element_rect(fill = "gray20")) + 
        theme(legend.background = element_rect(fill = "gray20")) + 
        theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))
    }
  })
  
  # listening habits server
  output$plot1 <- renderPlot({
    cp <- c("#202547FF", "#323649FF", "#41474BFF", "#4E5A4CFF", "#5C6C4CFF", "#68804CFF", "#75944BFF", "#81A949FF", "#8EBE45FF", "#9AD340FF", "#A6E939FF", "#B2FF2EFF")
    
    df1 <- df() %>% select(endTime)
    colnames(df1) <- "endTime"
    df1$endTime <- as.POSIXct(df1$endTime,format="%Y-%m-%d %H:%M",tz="GMT")
    df1$weekofday <- as.POSIXlt(df1$endTime)$wday
    df1$weekofday <- strftime(df1$endTime,'%A')
    
    df1$period <- format(df1$endTime, "%p")
    df1$hour <- format(df1$endTime, "%I")
    df1$month <- format(df1$endTime, "%b")
    df1$weekofday <- weekdays(df1$endTime)
    
    if (input$plot1_selection == "Usage by Day of Week") {
      ggplot(data.frame(days = names(table(df1$weekofday)), freq = as.numeric(table(df1$weekofday))),
             aes(x = days, y = freq)) +
        geom_bar(stat = "identity", fill = "#75944BFF") +
        labs(title = "Usage by Day of Week",
             x = " ", y = " ")+  
        theme_dark()+
        theme(plot.background = element_rect(fill = "gray20")) +
        theme(panel.background = element_rect(fill = "gray20")) + 
        theme(legend.background = element_rect(fill = "gray20")) + 
        theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))+
        coord_flip() 
    } else {
      df2 <- df1 %>% add_count(period,hour) %>% distinct(period, hour, n)
      
      ggplot(df2, aes(x = as.factor(hour), y = n, fill = period)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("#B2FF2EFF", "#75944BFF")) +
        labs(title = "Usage by Hour", x = "", y = "") +
        theme_dark()+
        theme(plot.background = element_rect(fill = "gray20")) +
        theme(panel.background = element_rect(fill = "gray20")) + 
        theme(legend.background = element_rect(fill = "gray20")) + 
        theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))+
        coord_flip() 
    }
  })
}
  

shinyApp(ui,server)
