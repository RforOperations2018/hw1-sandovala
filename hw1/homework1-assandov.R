
# Homework 9/6/18 alex.sandoval- Grid
# upload packages
library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(DT)

# upload data
starwars <- starwars

# take out some fields
starwars$films <- NULL
starwars$vehicles <- NULL
starwars$starships <- NULL

# melt starwars
meltwars <- melt(starwars, id = "name")
meltwars$name <- as.factor(meltwars$name)

pdf(NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Star Wars Characters Vary by Age and Mass"),
  fluidRow(
    column(12,
           plotlyOutput("plot")
    ),
    column(6,
           wellPanel(
             # an input element to select various characters from the semi-popular film known as 'Star Wars'
             # Jokes in the comments! Next time you might want to try the inputPanel, which places inputs next to eachother. A few pts off UI.
             selectInput("char_select",
                         "Characters:",
                         choices = levels(meltwars$name),
                         multiple = TRUE,
                         selectize = TRUE,
                         selected = c("Luke Skywalker", "Darth Vader", "Han Solo", "Yoda", "Jabba Desilijic Tiure",
                                      "Obi-Wan Kenobi", "R2-D2", "Dexter Jettster")
                         ),
             # an input element for a slider range to view the year a character was born
             sliderInput("birthSelect",
                         "Birth Year:",
                         min = min(starwars$birth_year, na.rm = T),
                         max = max(starwars$birth_year, na.rm = T),
                         value = c(min(starwars$birth_year, na.rm = T), max(starwars$birth_year, na.rm = T)), 
                         step = 100
                        )
                    )       
          )
          ),
  fluidRow(
    # add white space to clean things up
    tags$style(type = "text/css", "#table {padding-left: 15px; padding-right: 25px;}"),
    DT::dataTableOutput("table")
          )
                )

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlotly({
    starwars.sub <- subset(starwars, name %in% input$char_select & birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])

    ggplot(data = starwars.sub, 
           aes(x = name,
               y = mass, 
               fill = name)) + 
      geom_bar(stat = "identity") +
      ylab("Mass") +  # y axis title
      xlab("") +  # no title on the x axis
      # edit the text of the x axis
      theme(axis.text.x = element_text(angle = 30, 
                                       hjust = 1,
                                       size = 8),
            # remove the title of the legend
            legend.title = element_blank())
  })
  output$table <- DT::renderDataTable({
    subset(starwars, name %in% input$char_select, 
           select = c(name, birth_year, height, mass, gender, species, homeworld))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
