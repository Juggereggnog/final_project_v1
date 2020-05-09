# At the outset, it may be helpful to remember that this will all be converted
# to HTML, and can be thought of as such beneath all of the R / Tidyverse
# functionality

library(shiny) # "Basic functionality" package 
library(DT) # "Showing data tables" package
library(shinythemes) # "CSS EZ-PZ" package
library(ggthemes) # "Pretty ggplot" package
library(scales) # "Fancy scales" package
library(plotly) # "ggplot alternative (hover!)" package
library(tidyverse) # "The" package

# Reading in data tables from read_in.Rmd

full_table <- read_rds("full_table.rds")

decks_full_table <- read_rds("decks_full_table.rds")

tournament_cards <- read_rds("tournament_cards.rds")

# Creating lists for selectInput() later

player_names <- unique(decks_full_table$player)

tables <- list("All Cards", "Tournament Cards")


ui <- fluidPage(theme = shinytheme("flatly"),
        
        # Centering all text
        
        fluidRow(
            
            column(12,
                   align = "center",
                   
        # End of centering text code
        
                   navbarPage("This is Magic",
                    
                   
            tabPanel("Model",
                 
                 titlePanel("Model"),
                 
                 h3("Breaking Down Tournament Decks"),
                 
                 tabsetPanel(
                     
                     tabPanel("Deck Breakdown",
                              
                              h3("Deck Breakdowns by Year and Card Type"),
                              
                              # Use br() to create aesthetic space between elements
                              
                              br(),
                              
                         sidebarLayout(
                             
                             sidebarPanel(
                                 
                                 helpText("Select Player and Card Types to View Deck Breakdown"),
                                 
                                 selectInput("player_names_breakdown", h3("Player:"), choices = player_names,
                                             selected = "Paulo Vitor Damo da Rosa"),
                                 
                                 p("About the Graph: Creature cards were favored heavily in the 2016 and 2017 championships,
                                   indicating a slower metagame based on the sets that were in Standard at the time. In contrast,
                                   creature cards were given less focus in favor of instants in the 2015 and 2020 championships,
                                   indicating a faster-paced metagame. Of course, this all depends on the player's style of play,
                                   so these are more general statements.")
                             ),
                             
                             mainPanel(
                                 
                                 plotlyOutput("breakdown")
                             )
                         )
                     ),
                     
                     tabPanel("Earnings",
                              
                              h3("Total Player Earnings 2015-2020"),
                              
                              br(),
                              
                         sidebarLayout(
                             
                             sidebarPanel(
                                 
                                 helpText("Select Player to View Career Earnings"),
                                 
                                 selectInput("player_names_earnings", h3("Player:"), choices = player_names,
                                             selected = c("Paulo Vitor Damo da Rosa", "Seth Manfield",
                                                          "Brian Braun-Duin", "Jean-Emmanuel Depraz"), multiple = TRUE),
                                 
                                 p("About the Graph: Damo da Rosa is considered one of the best Magic players of all time;
                                   highest earners are skewed towards recent years due to higher prize pools")
                             ),
                              
                             mainPanel(
                                 
                                 plotOutput("earnings")
                             )
                         )
                     )
                 )
            ),
        
            tabPanel("Explore",
                 
                 titlePanel("Explore"),
                 
                 h3("Further Analysis"),
                 
                 br(),
                 
                 tabsetPanel(
                     
                     tabPanel("Statistical Analysis",
                              
                              h3("Price by Release Date and Rarity"),
                              
                              sidebarLayout(
                                  
                                  sidebarPanel(
                                      
                                      p("About the Graph: Mythic-rare cards were introduced in the early 2010s, and their price has the smallest associated
                                        decrease. They are incredibly hard to find, which makes it a likely candidate as a contributor to price.
                                        That said, it is obvious from this graph that on average, the price of all cards is decreasing over time.
                                        This is mainly the result of Wizards of the Coast refining their card R&D, where cards no longer overthrow
                                        the meta with every set as was common in the past. In addition, older cards have been gaining value as both
                                        antique collectibles and overpowered cards that escaped while Magic was still stabilizing.")
                                  ),
                                  
                                  mainPanel(
                                      plotlyOutput("rare_regression")
                                  )
                              )
                     ),
                     
                     tabPanel("Card Data",
                              
                              h3("Poke Around!"),
                              
                              br(),
                              
                              selectInput("table", h3("Dataset:"), choices = tables,
                                                  selected = "All Cards"),
                                  
                              column(12,
                                     dataTableOutput("dataset"), style = "overflow-x: scroll;"
                             )
                     )
                 )
        ),
        
            tabPanel("About",
                 
                 titlePanel("About"),
                 
                 br(),
                 
                 h3("A Background in Magic"),
                 
                 br(),
                 
                 h4("What is", em("Magic: The Gathering"), "?"),
                 
                 p(em("Magic: The Gathering"), "was the first collectible trading card game ever made,
                    created by Wizards of the Coast in 1993. It has received regular expansions to
                    its collection of cards for over 25 years, and is played in both physical and
                    digital formats."),
                    
                 p("Two players construct a deck of 60 cards full of creatures, lands, artifacts,
                    and more and battle against each other to bring their opponent's life total
                    from 20 to 0. With over 20,000 unique cards, the possibilities can be daunting to new players
                    trying to find the best combination of cards. The players who are able to find
                    these combinations, and play them optimally, can be invited to",
                    em("Magic's"), "premier tournament: the", em("Magic"),"World Championship."),
                    
                 p("What I seek to do with this project is understand what goes into a top-tier",
                    em("Magic's"), "deck, and, because it is a 'collectible' card game with a thriving
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
                    ", a(em("Magic's"), " official website.", href = "https://magic.wizards.com/en/events/coverage?source=MX_Nav2020")),
                 
                 br(),
                 
                 h4("Methodological Limitations"),
                 
                 tags$ul(
                     tags$li("Sideboard cards are included in deck breakdowns: sideboard cards deserve less weight in analysis than main board cards"),
                     tags$li("Textbox is not included in analysis: evergreen keywords may also have a strong effect on price and tournament viability")
                 ),
                 
                 br(),
                 
                 # h4("Areas for Improvement"),
                 # 
                 # tags$ul(
                 #     tags$li("Adding card name and textbox to regression plot's hover template (currently ggplotly() is confusing to work with"),
                 #     tags$li("Adding 'year' (with 'all') inputs for both graphs in Model tab (especially for earnings plot"),
                 #     tags$li("Adding analysis and takeaways for each plot (and, if some already exists, possibly adding more)"),
                 #     tags$li("Converting shinytheme to 'cyborg' while keeping data tables in the Explore tab visible (currently unable to)"),
                 #     tags$li("Making all header text mythic orange (possible with 'includeCSS()')"),
                 #     tags$li("Adding functionality to exploring datasets (selecting columns, etc)"),
                 #     tags$li("Commenting all code"),
                 #     tags$li("Adding more statistical analysis graphs (tournament viability specifically, but also more concerning price)"),
                 #     tags$li("slider input for max price of statistical analysis graph")
                 # ),
                 # 
                 # br(),
                 
                 h4("About the Creator"),
                 
                 p("Hi, my name is Elias DeLeon, and I am currently an undergraduate at Harvard University, studying
                    philosophy and government, and computer science, and any other subject I can
                    get my hands on."),
                    
                 p("You can contact me at eliasdeleon@college.harvard.edu or view my other
                    projects on my", a("GitHub.", href = "https://github.com/Juggereggnog")),
                 
                 p("This project's GitHub repository can be found", a("here.", href = "https://github.com/Juggereggnog/final_project_v1/"))
        )
    )
)))

server <- function(input, output) {

    output$breakdown <- renderPlotly({
        breakdown <- decks_full_table %>% 
            mutate(types = ifelse(str_detect(types, "Creature") == TRUE, "Creature", types)) %>% 
            filter(player %in% input$player_names_breakdown) %>% 
            count(year, types, duplicates) %>% 
            group_by(year, types) %>% 
            summarize(num_cards = sum(duplicates * n)) %>%
            ggplot(aes(x = types, y = num_cards, fill = year)) +
            geom_col() +
            labs(x = "Types",
                 y = "Count",
                 fill = "Player") +
            theme_bw()
        
        ggplotly(breakdown)
    })
    
    output$earnings <- renderPlot({
        ggdecks <- decks_full_table %>% 
            group_by(player, winnings_dollars) %>% 
            summarize() %>% 
            ungroup() %>% 
            group_by(player) %>% 
            summarize(total_winnings = sum(winnings_dollars)) %>% 
            filter(player %in% input$player_names_earnings) %>% 
            arrange(desc(total_winnings))
        
        ggplot(ggdecks, aes(fct_reorder(player, total_winnings), total_winnings)) +
            geom_col() +
            coord_flip() +
            labs(x = "Player",
                 y = "Total Winnings ($)",
                 caption = "Source: Wizards of the Coast Event Coverage") +
            theme_bw() +
            theme(axis.text.y = element_text(angle = 40)) +
            scale_y_continuous(labels = comma)
    })
    
    output$dataset <- renderDataTable(datatable({
        if(input$table == "All Cards") {
            data <- full_table
        }
        
        if(input$table == "Tournament Cards") {
            data <- tournament_cards
        }
        
        data
    }))
    
    output$rare_regression <- renderPlotly({
        rare_regression <- full_table %>%
            filter(! is.na(price), price < 50, str_detect(types, "Creature") == TRUE) %>%
            ggplot(aes(x = releaseDate, y = price, color = rarity)) +
            geom_jitter(width = 0.1, alpha = 0.4) +
            geom_smooth(method = "lm", se = FALSE) +
            theme_bw() +
            labs(x = "Release Date", y = "Price", color = "Rarity") +
            scale_color_manual(values = c("black", "cyan", "yellow", "darkorange"))
        
        ggplotly(rare_regression)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
