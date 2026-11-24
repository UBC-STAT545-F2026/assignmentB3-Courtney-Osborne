
library(shiny)
library(ggplot2)
library(tidyverse)
library(pak)
pak::pak("diverse-data-hub/diversedata")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How Couples Meet and Stay Together"),
    
    sidebarLayout(
      sidebarPanel(radioButtons("qualityInput", "relationship_quality",
                                choices = c("excellent", "good", "fair", "poor", "very_poor"),
                                selected = "excellent"),
                   sliderInput("ageInput", "subject_age", min = 20, max = 100,
                               value = c(30, 40))
                   
                
      ),
      
      # Show a plot of the generated distribution
      mainPanel(plotOutput("Durationplot")
                #br(), br(),
                #tableOutput("Meantable")
                )
      )
    )


server <- function(input, output) {
  
  hcmstfilt <- reactive({ diversedata::hcmst %>%
      filter(subject_age >= input$ageInput[1],
             subject_age <= input$ageInput[2],
             relationship_quality == input$qualityInput)})

  output$Durationplot <- renderPlot({
    ggplot(hcmstfilt(), aes(relationship_duration))+
      geom_histogram()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
