# Set maximum upload size to 500 MB
options(shiny.maxRequestSize = 500 * 1024^2)

library(shiny)
library(dendextend)
library(tidyverse)
library(readxl)

ui <- fluidPage(
  titlePanel("Dendrogram Tree Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Normalised Data Matrix", multiple = FALSE, accept = c(".xlsx")),
      helpText("Please upload an Excel file with the first column as 'Geneid' and the other columns containing normalized counts for each sample.")
    ),
    mainPanel(
      plotOutput("dendrogramPlot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$file, {
    inFile <- input$file
    req(inFile)
    data <- read_excel(inFile$datapath, col_names = TRUE) %>% column_to_rownames(var = "Geneid") %>% rowwise() %>%
      mutate(variance = c_across(everything()) %>% var()) %>%
      ungroup() %>%
      slice_max(n = 150, order_by = variance) %>%
      select(-c(variance)) %>% 
      as.matrix() %>% 
      t()
    
    print('running dist')
    # Calculate distance and cluster the data
    dist_matrix <- dist(data, method = "euclidean")
    print('dist done')
    
    hclust_result <- hclust(dist_matrix, method = "complete")
    print('hclust done')
    
    # Create dendrogram
    dend <- as.dendrogram(hclust_result)
    print('done')
    
    # Plot the dendrogram
    output$dendrogramPlot <- renderPlot({
      par(mar = c(5, 4, 2, 8) + 0.1)
      plot(dend, horiz = TRUE, main = "Sample Dendrogram", xlab = "Euclidean distance")
    })
  })
}

shinyApp(ui, server)
