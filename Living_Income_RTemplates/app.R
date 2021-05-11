
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
        
        actionButton("goButton", "Generate Plot", class = "btn-success"),
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barGraph")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # df <- reactive({
    #     req(input$file)
    #     ext <- file_ext(input$file$name)
    #     switch(ext,
    #            csv = vroom(input$file$datapath, delim = ","),
    #            tsv = vroom(input$file$datapath, delim = "\t"),
    #            validate("Invalid file; Please upload a .csv or .tsv file")
    #     )
    # })
    
#     df <- eventReactive(input$file{
#         inFile <- input$file
#         # Instead # if (is.null(inFile)) ... use "req"
#         req(inFile)
#         # Changes in read.table 
#         #df <- read.table(inFile$datapath)
#         df <- read_csv(inFile)
#         vars <- names(df)
#         # Update select input immediately after clicking on the action button. 
#         updateVarSelectInput(session, "total_hh_income", choices = vars)
#     
# })
    
    reactive_data <- reactive({
        print(input$file$datapath)
        data <- read_csv(input$file$datapath)
        return(data)
    })
    
    output$barGraph <- renderPlot({
        
        data <- reactive_data()
        # inFile <- input$file
        # req(input$file)
        # df <- read_csv(inFile$datapath)
        updateSelectInput(session, "total_hh_income",
                          choices = colnames(data),
                          selected = input$total_hh_income) # this keeps the input on the last thing selected on tab-change
        
        updateSelectInput(session, "total_hh_income",
                          choices = colnames(data),
                          selected = input$total_hh_income) # this keeps the input on the last thing selected on tab-change
        
        
        input$goButton
        
        main_crop <- input$main_crop
        currency <- input$currency
        gap_color <- input$gap_color
        other_color <- input$other_color
        main_color <- input$main_color
        
        total_hh_income < - colnames(data)[colnames(data) == input$total_hh_income]
        
        ## The first section of this code summarizes and formats the data to be graph-ready
       plot <- data %>% 
            # Group by household type
            group_by(grouping) %>% 
            # For each household type, summarize the mean gap to the living income, 
            # the mean other income, and the mean main_crop income 
            summarise(Gap = mean(benchmark - total_hh_income),
                      Other = mean(total_hh_income - income_main_crop),
                      main_crop = mean(income_main_crop)) %>% 
            # Gather each income components into one column so the data is in 'long' format
            gather(key = "Component", value = "Income", Gap:main_crop) %>% 
            # Re-level the income factors for the order you want them stacked on the graph  
            mutate(Component = factor(Component, 
                                      levels = c("Gap", "Other", "main_crop"))) %>% 
            # Generate ggplot graph for income by groupings and income component  
            ggplot(aes_string(y = Income, x = grouping, fill = Component)) +
            # Assign graph as stacked bar chart  
            geom_bar(position = "stack", stat = "identity") +
            # Label the graph title, axis, and caption  
            labs(title = "Mean values", 
                 y = paste("(", currency, "/year/household)", sep = ""),
                 x = "",
                 # Add caption with observation numbers for each household type  
                 caption = paste("Based on: \n", 
                                 paste(names(table(df$grouping)), 
                                       ":", 
                                       as.numeric(table(df$grouping)), 
                                       "observations \n ", collapse = ''), collapse = '')) +
            # Label the legend and assign custom colors 
            scale_fill_manual(values=c(gap_color, other_color, main_color),
                              breaks=c("Gap", "Other", main_crop),
                              labels=c("Gap to the Living Income Benchmark",
                                       "Other income",
                                       paste("Income from", main_crop))) +
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
