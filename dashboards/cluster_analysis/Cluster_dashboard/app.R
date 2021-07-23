library(dplyr)
library(ggplot2)
library(purrr)
library(dendextend)
library(cluster)
library(broom)

cust_spend <- read.csv("..\\cust_spend.csv")
cust_spend <- scale(cust_spend[,-1])
cust_spend <- as.data.frame(cust_spend)

get_dendrogram <- function() {
    dist_cust <- dist(cust_spend, method = "euclidean")
    hc_cust <- hclust(dist_cust, method = "complete")
    dend <- as.dendrogram(hc_cust)
    dend_color <- color_branches(dend, k = 2)
    dend_color
}

set.seed(42)

get_elbow_data <- function() {
    tot_withinss <- map_dbl(1:10,  function(k){
        model <- kmeans(x = cust_spend, centers = k)
        model$tot.withinss
    })
    elbow_df <- data.frame(
        k = 1:10 ,
        tot_withinss = tot_withinss
    )
    elbow_df
}

get_silhouette_data <- function() {
    sil_width <- map_dbl(2:10,  function(k){
        model <- pam(x = cust_spend, k = k)
        model$silinfo$avg.width
    })
    sil_df <- data.frame(
        k = 2:10,
        sil_width = sil_width
    )
    sil_df
}

get_cluster_assignment <- function() {
    model_customers <- kmeans(cust_spend, centers = 2)
    clust_customers <- model_customers$cluster
    segment_customers <- mutate(cust_spend, cluster = clust_customers)
    segment_customers
}

get_cluster_sizes <- function() {
    segment_customers <- get_cluster_assignment()
    cluster_sizes <- count(segment_customers, cluster)
    cluster_sizes
}

get_cluster_characteristics <- function() {
    # Calculate the mean for each feature within each cluster
    cluster_char <- get_cluster_assignment() %>% 
        group_by(cluster) %>% 
        summarise_all(list(mean))
    cluster_char
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Cluster analysis"),
    tabsetPanel(
        tabPanel(title = "Customer spend data", 
                 DT::dataTableOutput("spend_table")
        ),
        tabPanel(title = "Dendrogram", 
                 h3("2 clusters found to be optimal from visual inspection"),
                 plotOutput("dendrogram")
        ), 
        tabPanel(title = "K-means",
                 h3("2 clusters found to be optimal from elbow plot"),
                 plotOutput("elbow_plot"), 
                 h3("3 clusters found to be optimal from silhouette plot"),
                 plotOutput("silhouette_plot")
        ),
        tabPanel(title = "Cluster characteristics",
                 h3("Size of each cluster"),
                 DT::dataTableOutput("cluster_size_table"),
                 h3("Mean spending across clusters"),
                 DT::dataTableOutput("cluster_char_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Display customer spend data
    output$spend_table <- DT::renderDataTable({
        data <- read.csv("..\\cust_spend.csv")
        data
    })
    
    output$dendrogram <- renderPlot({
        plot(get_dendrogram())
    })
    
    output$elbow_plot <- renderPlot({
        ggplot(get_elbow_data(), 
            aes(x = k, y = tot_withinss)) + 
            geom_line() +
            scale_x_continuous(breaks = 1:10)
    })
    
    output$silhouette_plot <- renderPlot({
        ggplot(get_silhouette_data(), 
            aes(x = k, y = sil_width)) +
            geom_line() +
            scale_x_continuous(breaks = 2:10)
    })
    
    output$cluster_size_table <- DT::renderDataTable({
        get_cluster_sizes()
    })
    
    output$cluster_char_table <- DT::renderDataTable({
        get_cluster_characteristics()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
