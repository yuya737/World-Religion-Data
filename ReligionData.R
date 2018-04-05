library(shiny)
library(ggplot2) 
library(dplyr)

religionData <- read.csv("religion.csv")

ui <- fluidPage(
  titlePanel("World Religion Trend from 1945 - 2010"),
  h5(em("Data From Zeev Maoz and Errol A. Henderdon, The World Religion Dataset, 1945 to 2010: Logic, Estimates, and Trends")),
  sidebarLayout(
    sidebarPanel(
      helpText("Please select Region(s), Religion(s) and a Year Range"),
      checkboxGroupInput("Region", 
                         label = "Choose Region(s)",
                         
                         choices = list("Africa" = "Africa",
                                        "Asia-Pacific" = "Asia-Pacific", 
                                        "Europe" = "Europe",
                                        "Middle East" = "Middle East",
                                        "West Hemisphere" = "West Hemisphere"
                                        ),
                         selected = "Africa"),
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
      textOutput("title"),
      tags$head(tags$style("#title{color: Black;
                                 font-size: 20px;
                                 }"
      )
      )
    ,
      plotOutput("plot", height = 630 )
    )
  )
)


server <- function(input, output){
  
  returnReligion <- function(x) {
    if (x == "Christianity"){
      return ("christianity_percent")
    } else if (x == "Judaism"){
      return ("judaism_percent")
    } else if (x == "Buddhism"){
      return ("buddhism_percent")
    } else if (x == "Islam"){
      return ("islam_percent")
    } else if (x == "Other religion"){
      return ("otherreligion_percent")
    } else if (x == "Hinduism"){
      return ("hinduism_percent")  
    } else {
      return ("noreligion_percent")
    }
  }
  
  regionlist <- c(
    'Africa'="Africa",
    'Europe'="Europe",
    'Mideast'="Middle East",
    'West. Hem'="West Hemisphere",
    'Asia' = "Asia-Pacific"
  )

  
  
  
  
  output$plot <- renderPlot({
    Asia <- subset(religionData, region == "Asia")
    WestH <- subset(religionData, region == "West. Hem")
    Africa <- subset(religionData, region == "Africa")
    MidE <- subset(religionData, region == "Mideast")
    Europe <- subset(religionData, region == "Europe")
    regionLen <- length(input$Region)
    firstRegion <- input$Region[1]
    if (firstRegion == "West Hemisphere"){
      regionFinal <- WestH
    }
    else if (firstRegion == "Asia-Pacific"){
      regionFinal <- Asia
    }
    else if (firstRegion == "Africa"){
      regionFinal <- Africa
    } 
    else if (firstRegion == "Middle East"){
      regionFinal <- MidE
    }
    else {
      regionFinal <- Europe
    }
    if (regionLen > 1){
      for (i in 2 : regionLen){
        if (input$Region[i] == "West Hemisphere"){
          regionFinal <- rbind(regionFinal, WestH)
        }
        else if (input$Region[i] == "Asia-Pacific"){
          regionFinal <- rbind(regionFinal, Asia)
        }
        else if (input$Region[i] == "Africa"){
          regionFinal <- rbind(regionFinal, Africa)
        } 
        else if (input$Region[i] == "Middle East"){
          regionFinal <- rbind(regionFinal, MidE)
        }
        else {
          regionFinal <- rbind(regionFinal, Europe)
        }
      }
    }
    # if (input$Region == "West Hemisphere"){
    #   selectedRegion <- WestH
    # } else if (input$Region == "Asia-Pacific"){
    #   selectedRegion <- Asia
    # } else if (input$Region == "Africa") {
    #   selectedRegion <- Africa
    # } else if (input$Region == "Middle East"){
    #   selectedRegion <- MidE
    # } else {
    #   selectedRegion <- Europe
    # }
    len <- length(input$Religion)
    religionVec <- vector()
    if (len != 0){
      religionVec <- c(religionVec, returnReligion(input$Religion[1]))
      if (len > 1){
        for (i in 2:len){
          religionVec <- c(religionVec, returnReligion(input$Religion[i]))
        }
      }
    }
    selected_data <- select(regionFinal, c("region", religionVec, "year"))
    #print(dim(selected_data))
    #print(head(selected_data))
    p <- ggplot(data = selected_data, aes(x = selected_data$year))+ ylim(0,1) +  scale_color_discrete(name  ="Religion") + theme(legend.text=element_text(size=15)) + theme(legend.title=element_text(size=16))
    p <- p + xlim(input$range[1], input$range[2]) + xlab("Year") + ylab(paste("Percentage of Selected Religion(s) in Selected Region(s)")) + theme(axis.text=element_text(size=15), axis.title=element_text(size=15)) 
    for (i in 1:len){
      if (religionVec[i] == "christianity_percent"){
        p <- p + geom_line(size = 1, aes(y = selected_data$christianity_percent, color = "Christianity")) + geom_point(aes(y = selected_data$christianity_percent, color = "Christianity"), size = 2)
      } else if (religionVec[i] == "judaism_percent"){
        p <- p + geom_line(size = 1, aes(y = selected_data$judaism_percent, color = "Judaism")) + geom_point(aes(y = selected_data$judaism_percent, color = "Judaism"), size = 2)
      } else if (religionVec[i] == "hinduism_percent"){
        p <- p + geom_line(size = 1, aes(y = selected_data$hinduism_percent, color = "Hinduism"))+ geom_point(aes(y = selected_data$hinduism_percent, color = "Hinduism"), size = 2)
      } else if (religionVec[i] == "buddhism_percent"){
        p <- p + geom_line(size = 1, aes(y = selected_data$buddhism_percent, color = "Buddhism"))+ geom_point(aes(y = selected_data$buddhism_percent, color = "Buddhism"), size = 2)
      } else if (religionVec[i] == "islam_percent"){
        p <- p + geom_line(size = 1, aes(y = selected_data$islam_percent, color = "Islam"))+ geom_point(aes(y = selected_data$islam_percent, color = "Islam"), size = 2)
      } else if (religionVec[i] == "otherreligion_percent"){
        p <- p + geom_line(size = 1, aes(y = selected_data$otherreligion_percent, color = "Other Religion"))+ geom_point(aes(y = selected_data$otherreligion_percent,color = "Other Religion"), size = 2)
      } else {
        p <- p  + geom_line(size = 1, aes(y = selected_data$noreligion_percent,color = "No Religion"))+ geom_point(aes(y = selected_data$noreligion_percent, color = "No Religion"), size = 2)
      }
    }
    p <- p + facet_grid( ~ region , labeller = as_labeller(regionlist))+ theme(strip.text.x = element_text(size = 15))
   
    p
  })
  
  output$title <- renderText({
    strOut <- input$Religion[1]
    len <- length(input$Religion)
    if (len > 1){
      if (len == 2){
        strOut <- paste(strOut, paste("and ", input$Religion[2]))
      } else {
        for (i in 2:len){
          if (i != len){
            strOut <- paste(strOut, paste("," , input$Religion[i]))
          } else{
            strOut <- paste(strOut, paste(", and", input$Religion[i]))
          }
        }
      }
    }
    len2 <- length(input$Region)
    strOut2 <- input$Region[1]
    if (len2 > 1){
      if (len2 == 2){
        strOut2 <- paste(strOut2, paste("and ", input$Region[2]))
      } else {
        for (i in 2:len2){
          if (i != len2){
            strOut2 <- paste(strOut2, paste("," , input$Region[i]))
          } else{
            strOut2 <- paste(strOut2, paste(", and", input$Region[i]))
          }
        }
      }
    }
    paste("Percentage of ", strOut , "in ", strOut2)
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
