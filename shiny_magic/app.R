library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cyborg"),
        
        navbarPage("This is Magic",
                    
            tabPanel("Model",
                 
                 titlePanel("Model"),
                 
                 h3("Breaking Down Tournament Decks")
        ),
        
            tabPanel("Discussion",
                 
                 titlePanel("Discussion"),
                 
                 h3("Further Analysis")
        ),
        
            tabPanel("Takeaways",
                 
                 titlePanel("Takeaways")
        ),
        
            tabPanel("About",
                 
                 titlePanel("About"),
                 
                 h3("A Background in Magic")
        )
    )
)

server <- function(input, output) {

    
}

# Run the application
shinyApp(ui = ui, server = server)
