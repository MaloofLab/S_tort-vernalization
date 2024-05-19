library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

ui <- fluidPage(
  titlePanel("Dynamic Data Loading and Visualization with Group Selection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset",
                  "Choose a Dataset:",
                  choices = list("Dataset 1" = "Sdiv_ptg000010l_2315",
                                 "Dataset 2" = "Sdiv_ptg000009l_0614")),
      selectInput("group",
                  "Select Group:",
                  choices = list("v0i0" = "v0i0",
                                 "v1i1" = "v1i1",
                                 "v2i2" = "v2i2"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)


server <- function(input, output) {
  # Reactive expression to load data based on the user's selection
  datasetInput <- reactive({
    data_path <- switch(input$dataset,
                        "Sdiv_ptg000010l_2315" = "tmp.data_Sdiv_ptg000010l_2315.csv",
                        "Sdiv_ptg000009l_0614" = "tmp.data_Sdiv_ptg000009l_0614.csv")
    read_csv(data_path)
  })
  
  # Generate plot based on the loaded and processed data
  output$plot <- renderPlot({
    data <- datasetInput()
    
    # Filter data based on the selected group
    if (input$group == "v0i0") {
      data <- data %>% filter(Vern %in% c("v0", "i0"))
    } else if (input$group == "v1i1") {
      data <- data %>% filter(Vern %in% c("v1", "i1"))
    } else if (input$group == "v2i2") {
      data <- data %>% filter(Vern %in% c("v2", "i2"))
    }
    
    # Check if data is loaded and has columns 'Vern', 'log2_cpm', 'Pop'
    if (!("Vern" %in% names(data) && "log2_cpm" %in% names(data) && "Pop" %in% names(data))) {
      return(ggplot() + labs(title = "Data format not suitable or missing necessary columns"))
    }
    
    # Calculate the average log2_cpm for each combination of Vern and Pop
    average_data <- data %>%
      group_by(Vern, Pop) %>%
      summarise(avg_log2_cpm = mean(log2_cpm, na.rm = TRUE), .groups = 'drop')
    
    # Plotting
    ggplot(average_data, aes(x = Vern, y = avg_log2_cpm, color = Pop, group = Pop)) +
      geom_point() +
      geom_line() +
      labs(title = paste("Average Log2 CPM for", input$dataset, "and Group", input$group),
           x = "Vernacular Group",
           y = "Average Log2 CPM") +
      scale_color_viridis_d()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
