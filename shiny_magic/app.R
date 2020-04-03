library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
        
        navbarPage("This is Magic",
                    
            tabPanel("Model",
                 
                 titlePanel("Breaking Down Tournament Decks")
        ),
        
            tabPanel("Discussion",
                 
                 titlePanel("Further Analysis")
        ),
        
            tabPanel("Takeaways",
                 
                 titlePanel("Takeaways")
        ),
        
            tabPanel("About",
                 
                 titlePanel("A Background in Magic")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
