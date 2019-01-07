
# Define the formatNames function
formatNames <- function(input) {
        
        grep(", ", input, value = T)
        
        input <- strsplit(input, ", ")
        input <- unlist(input)
        
        input <- grep("& ", input, value = T) %>%
                strsplit(., "& ") %>%
                unlist(.) %>%
                c(input, .)
        
        input <- input[grep("& ", input, invert = T)]
        
        input <- gsub("^ ", "", input)
        input <- gsub(" $", "", input)
        
        input2 <- gsub(".*\\s", "", input)
        input2 <- gsub("$", ", ", input2)
        
        input <- gsub("\\s\\w*$", "", input)
        
        input3 <- paste(input2, input)
        cat(paste(input3, sep = "\n", collapse = "\n"))
        
}

library(dplyr)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Format Author Names"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
              textAreaInput("text", label = "", value = "Johnny Depp, Arnold Schwarzenegger, Jim Carrey")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
              verbatimTextOutput("value")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        
   
        output$value <- renderPrint({ formatNames(input = input$text) })
}

# Run the application 
shinyApp(ui = ui, server = server)

