library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tools)
library(rsconnect)
library(directlabels)


Data <- readRDS("Issues_Index_R_data.rds")


# Define UI for application that plots features of movies
ui <- navbarPage("An R Shiny project: making the Ipsos MORI Issues Index interactive",
                 tabPanel("Plot",
  
  
  fluidPage(theme = shinytheme("yeti"),
                
                fluidRow(
                  checkboxGroupInput(inputId = "Issue", label=strong("Select an issue:"), choices = unique(Data$Issue), selected = c("EU / europe / common market / single currency", "Flu pandemic / bird flu"), inline = TRUE),
                  dateRangeInput(inputId = "Date_range", 
                                           label=strong("Select a date range:"),
                                           start = min(Data$Month_year),
                                           end   = max(Data$Month_year),
                                           format="M/yyyy",
                                           startview = "decade")),
                  
                h5("Q: What do you see as the most/other important issues facing Britain today?"),
                
                plotOutput(outputId = "lineplot", height="700px", click = "plot_click"),
                    
                tags$i(h6("Click on a marker to be shown the % value:")),
                    
                verbatimTextOutput("info")
                    

                    )),
tabPanel("Source details",
         
         fluidPage(theme = shinytheme("yeti"),
                   fluidRow(
                            tags$a(href="https://www.ipsos.com/ipsos-mori/en-uk/issues-index-archive", "Source: Ipsos MORI Issues Index"),
                            h6("Ipsos MORI has been running the Issues Index since the 1970s. This app contains data from 1984 onwards, which is the date when the data began to be collected on more or less a monthly basis. The list of issues includes all issues coded by Ipsos MORI since 1984 that received 5% of responses on at least one occasion over that period."),
                            h6("Code for this dashboard can be found on Github", (tags$a(href="https://www.linkedin.com/in/tom-frere-smith-075a1512/?originalSubdomain=uk", "here")))
                     
                   ))))


# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  Data_subset <- reactive({
    req(input$Date_range, input$Issue)

    Data %>%
      filter(Issue %in% input$Issue, Month_year >= input$Date_range[1] & Month_year <= input$Date_range[2])

  })
  
  output$lineplot <- renderPlot({
      ggplot(Data_subset(), aes(x=Month_year, y=Importance_pc)) + 
      geom_line(aes(colour=Issue), size=1) + 
      theme(legend.position = "bottom") +
      theme(text = element_text(size=15)) +
      theme(legend.title = element_blank()) +
      theme(legend.text=element_text(size=14)) +
      xlab("") +
      ylab("Importance %") +
      scale_y_continuous(breaks = seq(0,100,5)) +
      geom_point(aes(colour=Issue), size=2)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("")
      paste0(round(e$y, 0), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("")
      paste0(" ymin=", round(e$ymin, 0), " ymax=", round(e$ymax, 0))
    }
    
    paste0(

      "Importance %: ", xy_str(input$plot_click)
      
    )
  })
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)

