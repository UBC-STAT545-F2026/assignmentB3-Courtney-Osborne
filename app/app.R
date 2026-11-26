
library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(pak)
pak::pak("diverse-data-hub/diversedata")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Relationship Duration of Couples Across Covid"),
    
    p("From the 'hcmst' dataset, this data investigates relationships during the covid epidemic"),
    
    sidebarLayout(
      sidebarPanel(radioButtons("qualityInput", "relationship_quality",
                                choices = c("excellent", "good", "fair", "poor", "very_poor"),
                                selected = "excellent"),
                   sliderInput("ageInput", "subject_age", min = 20, max = 100,
                               value = c(30, 40)),
                   downloadButton("download_data", "Download Filtered Data"),
      ),
    
      
      # Show a plot of the generated distribution
      mainPanel(plotOutput("Durationplot"),
                br(), br(),
                DT::dataTableOutput("Covidtable")
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
  
  output$Covidtable <- DT::renderDataTable({
    hcmstfilt() %>% 
      relocate(relationship_duration, rel_change_during_pandemic, agree_covid_approach, subject_vaccinated, partner_vaccinated) %>%
      arrange(relationship_duration)})
  
  output$download_data <- downloadHandler(
    filename = function() {
      "hcmst.csv"
    },
    content = function(file) {
      write.csv(hcmstfilt(), file, row.names = FALSE)})
    
}


# Run the application 
shinyApp(ui = ui, server = server)
