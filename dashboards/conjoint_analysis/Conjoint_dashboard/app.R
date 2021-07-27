library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Conjoint analysis"),
    tabsetPanel(
        tabPanel(title = "Survey Data", 
                 DT::dataTableOutput("survey_table")
        ),
        tabPanel(title = "Choice Counts", 
                 radioButtons(inputId = "choice_of_count",
                     label = "How many respondents chose each feature",
                     choices = c(
                         "Type of service" = "service",
                         "Environmentally friendly process" = "enviro",
                         "Segment of car" = "segment",
                         "Price band of car" = "price",
                         "Segment & Price of car" = "segment_price"
                     )
                 ),
                 DT::dataTableOutput("choice_count_table")
        ),
        tabPanel(title = "Generated Model", 
                 DT::dataTableOutput("model_table")
        ),
        tabPanel(title = "Hypothetical Market Share",
                h2("Hypothetical products competing in the market"),
                DT::dataTableOutput("hypothetical_products"),
                h2("Resultant market shares"),
                plotOutput("market_shares")
        )
    )
)
            

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Display survey data
    output$survey_table <- DT::renderDataTable({
        data <- read.csv("..\\wash_choice_wide.csv")
        data
    })
    
    # Display choice counts
        # Get user defined choice
    choice_data_source <- reactive({
        if (input$choice_of_count == "service") {
            data <- read.csv("service_count.csv")
        } 
        else if (input$choice_of_count == "enviro") {
            data <- read.csv("enviro_count.csv")
        }
        else if (input$choice_of_count == "segment") {
            data <- read.csv("segment_count.csv")
        }
        else if (input$choice_of_count == "price") {
            data <- read.csv("price_count.csv")
        }
        else if (input$choice_of_count == "segment_price") {
            data <- read.csv("segment_price_count.csv")
        }
        return(data)
    })
        # Display the choice counts
    output$choice_count_table <- DT::renderDataTable({
        choice_data_source()
    })
    
    # Display generated model
    output$model_table <- DT::renderDataTable({
        data <- read.csv("saved_model.csv")
        data
    })
    
    # Display hypothetical products competing for market share
    output$hypothetical_products <- DT::renderDataTable({
        data <- read.csv("hypothetical_products.csv")
        data
    })
    
    # Get the shares already calculated
    shares <- reactive({
        data <- read.csv("shares.csv")
        data$share
    })
    
    # Display shares as barplot
    output$market_shares <- renderPlot({
        barplot(shares(), 
                horiz = TRUE, 
                col = "tomato2",
                xlab = "Predicted Market Share",
                names.arg = c("Competitor 1", "Our Offering"),
                xlim = c(0.0, 1.0))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
