# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- fifa
    if (input$man != "All") {
      data <- fifa[fifa$Nationality == input$man,]
    }
    if (input$cyl != "All") {
      data <- fifa[data$Club == input$cyl,]
    }
    if (input$trans != "All") {
      data <- fifa[data$Prefered.Foot == input$trans,]
    }
    data
  }))
  
}