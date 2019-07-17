library(shiny)
library(ggplot2)

fluidPage(
  titlePanel("Player DataTable"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("man",
                       "Nationality:",
                       c("All",
                         unique(as.character(fifa$Nationality))))
    ),
    column(4,
           selectInput("trans",
                       "Club:",
                       c("All",
                         unique(as.character(fifa$Club))))
    ),
    column(4,
           selectInput("cyl",
                       "Preferred Foot:",
                       c("All",
                         unique(as.character(fifa$Preferred.Foot))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)