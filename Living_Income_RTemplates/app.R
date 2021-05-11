
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(vroom)
library(knitr)
library(scales)
library(tidyverse)
options(shiny.maxRequestSize = 30*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Living Income R Graph Templates"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose .csv or .tsv File",
                      multiple = FALSE,
                      accept = c(".csv", ".tsv")),
            
        selectInput("total_hh_income", "Total income", list("Location")),
        selectInput("income_main_crop", "Income from main crop", list("Location")),
        
        h3("Enter Graph Labels"),         
            # Replace graph labels
            textInput("main_crop", "Name of main crop", placeholder = "e.g: cocoa"),
            textInput("currency", "Currency", placeholder = "eg: USD"),
            
        h3("Color Selection"),
            textInput("gap_color", "Gap color", value = "#ed3833"),
            textInput("other_color", "Other color", value = "#b3dceb"),
            textInput("main_color", "Main color", value = "#b3b3fa"),
        
        #actionButton("goButton", "Generate Plot", class = "btn-success"),
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barGraph")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    reactive_data <- reactive({
        print(input$file$datapath)
        data <- read_csv(input$file$datapath)
        return(data)
    })
    
    output$barGraph <- renderPlot({
        
        data <- reactive_data()

        updateSelectInput(session, "total_hh_income",
                          choices = colnames(data),
                          selected = input$total_hh_income) # this keeps the input on the last thing selected on tab-change
        
        updateSelectInput(session, "income_main_crop",
                          choices = colnames(data),
                          selected = input$income_main_crop) # this keeps the input on the last thing selected on tab-change
        
        #input$goButton
        
        main_crop <- input$main_crop
        currency <- input$currency
        gap_color <- input$gap_color
        other_color <- input$other_color
        main_color <- input$main_color
        
        colnames(data)[which(colnames(data)== input$total_hh_income)] = "total_hh_income"
        colnames(data)[which(colnames(data)== input$income_main_crop)] = "income_main_crop"
    
        ## The first section of this code summarizes and formats the data to be graph-ready
       plot <- data %>% 
            # Group by household type
            group_by(grouping) %>% 
            # For each household type, summarize the mean gap to the living income, 
            # the mean other income, and the mean main_crop income 
            summarise(Gap = mean(benchmark - total_hh_income),
                      Other = mean(total_hh_income - income_main_crop),
                      MainCrop = mean(income_main_crop)) %>% 
            # Gather each income components into one column so the data is in 'long' format
            gather(key = "Component", value = "Income", Gap:MainCrop) %>% 
            # Re-level the income factors for the order you want them stacked on the graph  
            mutate(Component = factor(Component, 
                                      levels = c("Gap", "Other", "MainCrop"))) %>% 
            # Generate ggplot graph for income by groupings and income component  
            ggplot(aes(y = Income, x = grouping, fill = Component)) +
            # Assign graph as stacked bar chart  
            geom_bar(position = "stack", stat = "identity") +
            # Label the graph title, axis, and caption  
            labs(title = "Mean values", 
                 y = paste("(", currency, "/year/household)", sep = ""),
                 x = "",
                 # Add caption with observation numbers for each household type  
                 caption = paste("Based on: \n", 
                                 paste(names(table(data$grouping)), 
                                       ":", 
                                       as.numeric(table(data$grouping)), 
                                       "observations \n ", collapse = ''), collapse = '')) +
            # Label the legend and assign custom colors 
            scale_fill_manual(values=c(gap_color, other_color, main_color),
                              breaks=c("Gap", "Other", "MainCrop"),
                              labels=c("Gap to the Living Income Benchmark",
                                       "Other income",
                                       paste("Income from", input$main_crop))) +
            # Format y-axis labels with a comma  
            scale_y_continuous(labels = comma) +
            # Remove x-axis grid lines and tick marks  
            theme(panel.grid.major.x = element_blank(), 
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank(),
                  # Center plot title        
                  plot.title = element_text(hjust = 0.5),
                  # Move the legend to the bottom of the graph
                  legend.position="bottom",
                  # Remove legend title
                  legend.title = element_blank(),
                  # Put a box around the legend
                  legend.box.background = element_rect(),
                  # Move caption to desired location
                  plot.caption = element_text(hjust = 0)) +
            # Add incomes to prospective graph components  
            geom_text(aes(label = round(Income)), 
                      position = position_stack(vjust = 0.5), 
                      size = 3) 
       
     print(plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
