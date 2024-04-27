library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

combined.full.histogram.data <- read.csv('combined_full_data_table_v0v1v2.csv')

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Gene Regulation Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("groupSelection",
                  "Select Population Group:",
                  choices = c("Group 1 (tm & lv)", 
                              "Group 2 (ihl & wl)", 
                              "Group 3 (bhr & yo)", 
                              "Group 4 (kc & sq)")),
      selectInput("comparisonSelection",
                  "Select Comparison:",
                  choices = NULL)  
    ),
    mainPanel(
      plotOutput("regulationPlot")
    )
  )
)

# Define Server 
server <- function(input, output, session) {
  
  
  observe({
    group_data <- switch(input$groupSelection,
                         "Group 1 (tm & lv)" = combined.full.histogram.data %>% filter(Population %in% c("tm", "lv")),
                         "Group 2 (ihl & wl)" = combined.full.histogram.data %>% filter(Population %in% c("ihl", "wl")),
                         "Group 3 (bhr & yo)" = combined.full.histogram.data %>% filter(Population %in% c("bhr", "yo")),
                         "Group 4 (kc & sq)" = combined.full.histogram.data %>% filter(Population %in% c("kc", "sq")))
    
  
    comparisons <- unique(group_data$Comparison)
    
   
    updateSelectInput(session, "comparisonSelection",
                      choices = comparisons)
  })
  
  # Reactive expression to filter and process data based on selected group and comparison
  processedData <- reactive({
    group_data <- switch(input$groupSelection,
                         "Group 1 (tm & lv)" = combined.full.histogram.data %>% filter(Population %in% c("tm", "lv")),
                         "Group 2 (ihl & wl)" = combined.full.histogram.data %>% filter(Population %in% c("ihl", "wl")),
                         "Group 3 (bhr & yo)" = combined.full.histogram.data %>% filter(Population %in% c("bhr", "yo")),
                         "Group 4 (kc & sq)" = combined.full.histogram.data %>% filter(Population %in% c("kc", "sq")))
    
    group_data <- group_data %>%
      filter(Comparison == input$comparisonSelection) %>%
      gather(key = "Regulation", value = "Count", Down:Up) %>%
      unite("Population_Comparison", c("Population", "Comparison", "Elevation"), sep = "/", remove = FALSE) %>%
      mutate(Population_Comparison = fct_recode(Population_Comparison,
                                                "v0v1" = "tm/v0_vs_v1/low",
                                                "v0v2" = "tm/v0_vs_v2/high",
                                                "v1v2" = "lv/v1_vs_v2/low"))
    return(group_data)
  })
  
  # Render the plot based on the processed data
  output$regulationPlot <- renderPlot({
    data_for_plot <- processedData()
    
    ggplot(data_for_plot, aes(x = Population_Comparison, y = Count, fill = Regulation)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +  
      geom_text(aes(label = Count), vjust = -0.3, position = position_dodge(width = 0.9)) + 
      labs(title = "Gene Regulation of Selected Elevation Group Comparison ",
           x = "Population (Comparison/Elevation)",
           y = "Count of Genes") +
      theme_minimal() +
      scale_fill_brewer(palette = "Dark2") +  # Set a color palette
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
  })
}

# Run the app
shinyApp(ui = ui, server = server)
