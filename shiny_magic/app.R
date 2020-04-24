library(shiny)
library(shinythemes)
library(rmarkdown)
library(tidyverse)

ui <- fluidPage(theme = shinytheme("cyborg"),
        
        navbarPage("This is Magic",
                    
            tabPanel("Model",
                 
                 titlePanel("Model"),
                 
                 h3("Breaking Down Tournament Decks and Players"),
                 
                 mainPanel(
                     plotOutput("earnings"),
                     
                     checkboxGroupInput("meow", options = c("Option 1", "Option 2"))
                     
                     PlotOutput
                 )
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
                 
                 h3("A Background in Magic"),
                 
                 HTML(readLines("about.html"))
        )
    )
)

server <- function(input, output) {

    output$earnings <- renderImage({
        list(src = "earnings.png",
             contentType = "image/png",
             width = 800,
             height = 800,
             alt = "This is alternate text")
    }, deleteFile = FALSE)
    
    output$PLottt <- renderPlot({}
        filter(power = input$meow)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
