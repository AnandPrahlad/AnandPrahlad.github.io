library(shiny)

ui <- fluidPage(
    titlePanel("Promotion analysis"),
    tabsetPanel(
        tabPanel(title = "Sales under promotions", 
                 DT::dataTableOutput("sales_data")
        ),
        tabPanel(title = "Impact of promotions",
                 h3("A combo of display ads + coupons have the highest positive impact on sales"),
                 DT::dataTableOutput("promo_model")
        ),
        tabPanel(title = "Sales under competiton",
                 DT::dataTableOutput("choice_data")
        ),
        tabPanel(title = "Impact of competing promotions",
                 h3("1. Competitor undercutting us in price has the highest negative impact on sales"),
                 h3("2. A combo of display ads + coupons have the highest positive impact on sales"),
                 DT::dataTableOutput("competing_promo_margin")
        )
    )
)

server <- function(input, output) {
    output$sales_data <- DT::renderDataTable({
        sales <- read.csv("..\\sales_data.csv")
        sales
    })
   
    output$promo_model <- DT::renderDataTable({
        promo <- read.csv("promo_model.csv")
        promo
    })
    
    output$choice_data <- DT::renderDataTable({
        choice <- read.csv("..\\choice_data.csv")
        choice
    })
    
    output$competing_promo_margin <- DT::renderDataTable({
        competing_margin <- read.csv("competing_promo_margin.csv")
        competing_margin
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
