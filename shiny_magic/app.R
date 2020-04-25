library(shiny)
library(shinythemes)
library(rmarkdown)
library(plotly)
library(tidyverse)

ui <- fluidPage(theme = shinytheme("cyborg"),
        
        navbarPage("This is Magic",
                    
            tabPanel("Mode",
                 
                 titlePanel("Model"),
                 
                 h3("Breaking Down Tournament Decks and Players"),
                 
                 mainPanel(
                     img(src = "earnings.png",
                         style = "display: block; margin-left: auto; margin-right: auto",
                         height = "500vw"),
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
                 
                 br(),
                 
                 h3("A Background in Magic"),
                 
                 br(),
                 
                 h4("What is", tags$em("Magic: The Gathering"), "?"),
                 
                 p(tags$em("Magic: The Gathering"), "was the first collectible trading card game ever made,
                    created by Wizards of the Coast in 1993. It has received regular expansions to
                    its collection of cards for over 25 years, and is played in both physical and
                    digital formats."),
                    
                 p("Two players construct a deck of 60 cards full of creatures, lands, artifacts,
                    and more and battle against each other to bring their opponent's life total
                    from 20 to 0. With over 20,000 unique cards, the possibilities can be daunting to new players
                    trying to find the best combination of cards. The players who are able to find
                    these combinations, and play them optimally, can be invited to",
                    tags$em("Magic's"), "premier tournament: the", tags$em("Magic"),"World Championship."),
                    
                 p("What I seek to do with this project is understand what goes into a top-tier",
                    tags$em("Magic's"), "deck, and, because it is a 'collectible' card game with a thriving
                    marketplace, find out the total cost of the cards that comprise the deck. Many
                    games these days are accused of being 'play-to-win,' where those who can afford
                    to buy the best cards are destined to win. But what is the cost of victory? And
                    how does victory affect the cost of the cards? Feel free to explore the site!"),
                 
                 br(),
                 
                 img(src = "card_anatomy.jpg",
                     style = "display: block; margin-left: auto; margin-right: auto",
                     height = "500vw"),
                 
                 h5("Anatomy of a Magic Card", align = "center"),
                 
                 br(),
                 
                 h3("Project Details"),
                 
                 br(),
                 
                 h4("Data Sources"),
                 
                 p("I used up-to-date data from a website called", a("MTGJSON", href = "https://mtgjson.com/"),
                    "which aptly provides card and price data in .json format as well as a multitude of other file formats.
                    In addition, I scraped the decks of the top 16 players for each World Championship from
                    2015-2020. That data and similar data from other tournaments can be found on
                    ", a(tags$em("Magic's"), " official website.", href = "https://magic.wizards.com/en/events/coverage?source=MX_Nav2020")),
                 
                 h4("About the Creator"),
                 
                 p("This project's GitHub repository can be found", a("here.", href = "https://github.com/Juggereggnog/final_project_v1/")),
                 
                 
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
    
    output$anatomy <- renderImage({
        list(src = "card_anatomy.jpg",
             contentType = "image/jpg",
             width = 500,
             height = 400,
             alt = "This is alternate text")
    }, deleteFile = FALSE)

}

# Run the application
shinyApp(ui = ui, server = server)
