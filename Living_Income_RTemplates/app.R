
# App coded by Molly Leavens, a consultant of the Sustainable Food Lab, May 2021
# Built for use by the Living Income Community of Practice

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# load required libraries 
library(rsconnect)
library(shiny)
library(knitr)
library(scales)
library(extrafont)
library(tidyverse)

#font_import()
fontdf <- as.data.frame.matrix(fonttable())

# Increase the Max upload size for user file upload from the default
options(shiny.maxRequestSize = 30*1024^2)

################### UI ###################
ui <- fluidPage(
    

    # Application title
    titlePanel("Living Income Graph Generator"),
    
    p("Add app description here"),
    
    h3("File Upload"),    
    p("If your original data file is saved as an Excel, export
                    it as a CSV file (from Excel, 'save as') before importing it
                    here. Ensure the top row of the Excel/CSV is only column names"),
    
    fileInput("file", "Choose .csv File",
              multiple = FALSE,
              accept = c(".csv")),
    
    h3("Select Variables"), 
    
    p("After uploading your data file above, the dropdown menus for 
                   variable section will autopopulate. Select the column name 
                   from your data set that corresponds to each variable."),   
    
    selectInput("total_hh_income", "Total income", choices = NULL),
    selectInput("income_main_crop", "Income from main crop", choices = NULL),
    
    h3("Enter Graph Labels"),         
    # Replace graph labels
    textInput("main_crop", "Name of main crop", placeholder = "e.g: cocoa"),
    textInput("currency", "Currency", placeholder = "eg: USD"),
    
    # This extends the font drop down above the navegation bar
    tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))),
    
    h3("Font Selection"),
    selectInput("font", "Select graph font", list('Arial' = list("Arial-Black", "Arial-BoldItalicMT", "Arial-BoldMT", "Arial-ItalicMT"),
                                                       'Arial Narrow' = list("ArialNarrow", "ArialNarrow-Bold", "ArialNarrow-BoldItalic", "ArialNarrow-Italic"),
                                                       'Times New Roman' = list("TimesNewRomanPS-BoldItalicMT","TimesNewRomanPS-BoldMT", "TimesNewRomanPS-ItalicMT", "TimesNewRomanPSMT"),
                                                       'Courier' = list("CourierNewPS-BoldItalicMT", "CourierNewPS-BoldMT", "CourierNewPS-ItalicMT", "CourierNewPSMT"),  
                                                       'Verdana' = list("Verdana", "Verdana-Bold", "Verdana-BoldItalic", "Verdana-Italic"), 
                                                       'Georgia' = list("Georgia", "Georgia-Bold", "Georgia-BoldItalic", "Georgia-Italic"),
                                                       'Trebuchet' = list("Trebuchet-BoldItalic", "TrebuchetMS", "TrebuchetMS-Bold", "TrebuchetMS-Italic"), 
                                                       'Other' = list("BodoniSvtyTwoSCITCTT-Book", "ArialRoundedMTBold", "ArialUnicodeMS", "Bradley-Hand-Bold", "BrushScriptMT", "DINCondensed-Bold")
                                                   ),
                ),
    
    actionButton("plotButton", "Generate Plot", class = "btn-success"),
                   
                                                              
    navbarPage(" ",
               tabPanel("Gap to the Living Income Benchmark - Bar Graphs",
                        sidebarLayout(
                        mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Absolute Mean", 
                                                       plotOutput("abMeanBarGraph"),
                                                       downloadButton('foo'))
                                              # tabPanel("Relative Mean",
                                              #          plotOutput("relMeanBarGraph")),
                                              # tabPanel("Absolute Median",
                                              #          plotOutput("abMedianBarGraph")),
                                              # tabPanel("Relative Median",
                                              #          plotOutput("relMedianBarGraph"))
                                              )
                                 ),
                         
                         # tabPanel("Distribution Graphs",
                         #     tabsetPanel(type = "tabs",
                         #                  tabPanel("By Type",           
                         #                           plotOutput("typeDisGraph")),
                         #                  tabPanel("With Mean and Median",
                         #                            plotOutput("mDisGraph")),
                         #                 )
                         #          ),
                         #     
                         # tabPanel("Foster–Greer–Thorbecke (FGT) index",
                         #          plotOutput("FGTGraph"))
    
    # Sidebar  
                        sidebarPanel(
            
                    p("Replacing the default graph colors below is optional, but may be 
                    helpful in matching the graphs to reports/presentations. 
                    You have two options for entering colors into the boxes below. 
                    1) List on of the 657 built-in colors recognized by this app. 
                    See all colors here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
                    2) List a hex color code (as is currently there now)"), 
        
        h3("Color Selection"),
            textInput("gap_color", "Gap color", value = "#ed3833"),
            textInput("other_color", "Other color", value = "#b3dceb"),
            textInput("main_color", "Main color", value = "#b3b3fa")
    
     

        )
)
               )   
)

)
################### Server ###################
server <- function(input, output, session) {
    
    reactive_data <- reactive({
        print(input$file$datapath)
        data <- read_csv(input$file$datapath)
        return(data)
        
       })
    
    observe({
        
    updateSelectInput(session, "total_hh_income",
                          choices = colnames(data),
                          selected = input$total_hh_income) # this keeps the input on the last thing selected on tab-change
    
    updateSelectInput(session, "income_main_crop",
                      choices = colnames(data),
                      selected = input$income_main_crop) # this keeps the input on the last thing selected on tab-change
    })
    
    observeEvent(input$plotButton, {
        
    output$abMeanBarGraph <- renderPlot({
        
        f <- input$font
        theme_set(theme_get() + theme(text = element_text(family = f)))
        
        data <- reactive_data()

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

       output$foo = downloadHandler(
           filename = 'Graph.png',
           content = function(file) {
               device <- function(..., width, height) {
                   grDevices::png(..., width = width, height = height,
                                  res = 1500, units = "in")
               }
               ggsave(file, plot = plot, device = device)
           })   
    
    })
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
