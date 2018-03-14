library(shiny)
library(ggplot2) 
library(dplyr)

religionData <- read.csv("religion.csv")

ui <- fluidPage(
  titlePanel("World Religion Trend from 1945 - 2010"),
  h5(em("Data From The World Religion Dataset, 1945 to 2010: Logic, Estimates, and Trends")),
  sidebarLayout(
    sidebarPanel(
      helpText("Please select a Region, Religion and a Year Range"),
      
      selectInput("Region", 
                  label = "Choose a Region",
                  choices = c("West Hemisphere", 
                              "Asia-Pacific",
                              "Africa", 
                              "Middle East",
                              "Europe"),
                  selected = "West Hemisphere"),
      checkboxGroupInput("Religion", 
                         label = "Choose Religion(s)",
                        
                         choices = list("Christianity" = "Christianity", 
                                        "Judaism" = "Judaism", 
                                        "Buddhism" = "Buddhism",
                                        "Islam" = "Islam",
                                        "Hinduism" = "Hinduism",
                                        "Other religion" = "Other religion",
                                        "No religion" = "No religion"),
                         selected = "Christianity"),
      
      sliderInput("range", 
                  label = "Year Range:",
                  min = 1945, max = 2010, value = c(1945, 2010), step = 5)
    ),
    
    mainPanel(
      textOutput("selected_Region"),
      textOutput("selected_Religion"),
      textOutput("selected_range"),
      textOutput("title"),
      tags$head(tags$style("#title{color: Black;
                                 font-size: 20px;
                                 }"
      )
      )
    ,
      
      plotOutput("plot")
    )
  )
)


server <- function(input, output){
  
  output$plot <- renderPlot({
    Asia <- subset(testReligion, region == "Asia")
    WestH <- subset(testReligion, region == "West. Hem")
    Africa <- subset(testReligion, region == "Africa")
    MidE <- subset(testReligion, region == "Mideast")
    Europe <- subset(testReligion, region == "Europe")
    if (input$Region == "West Hemisphere"){
      selectedRegion <- WestH
    } else if (input$Region == "Asia-Pacific"){
      selectedRegion <- Asia
    } else if (input$Region == "Africa") {
      selectedRegion <- Africa
    } else if (input$Region == "Middle East"){
      selectedRegion <- MidE
    } else {
      selectedRegion <- Europe
    }
    temp <- select(selectedRegion, c(year, christianity_percent, judaism_percent, buddhism_percent, islam_percent, noreligion_percent, otherreligion_percent))
    p <- ggplot(data = temp) + geom_line(aes(x = temp$year, y = temp$christianity_percent)) + ylim(0,1)
    p <- p + xlim(input$range[1], input$range[2]) + xlab("Year") + ylab(paste("Percentage of Selected Religion(s) in ", input$Region))
    p
  
  })
  
  output$title <- renderText({
    paste(c("Percentage of ", input$Religion , "in ", input$Region))
  })
  
  output$selected_Region <- renderText({ 
    paste("You have selected", input$Region)
  })
  output$selected_Religion <- renderText({
    paste(c("You have selected", input$Religion))
  })
  output$selected_range <- renderText({ 
    paste("You have chosen a range that goes from",
          input$range[1], "to", input$range[2])
  })

}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
