library(shiny)
library(data.table)
library(skimr)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data overview"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Copy the line below to make a file upload manager
            fileInput("file", label = h3("File input")),
            uiOutput("select_columns"),
            uiOutput("multiple_choice"),
            actionButton("refresh_table", "Refresh"),
            actionButton("browser", "Browser"),
            
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Table",
                         DT::dataTableOutput("table")),
                tabPanel("Summary",
                         DT::dataTableOutput("data_summary")) 
            )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    data <- reactive({
        req(input$file)
        fread(input$file$datapath)
        })
    
    numeric_vars <- reactive({
        names(data()[ , .SD, .SDcols = is.numeric])
    })
    
    selected_cols <- eventReactive(c(input$refresh_table, input$server_columns), {
        input$server_columns
    })
    
    output$table <- DT::renderDataTable({
        input$file
        cols_vector <- selected_cols()
            DT::datatable(data()[, ..cols_vector]) 
            
    })
    
    output$data_summary <- DT::renderDataTable({
        input$file
        cols_vector <- selected_cols()
        tb_data <- skim(data()[, ..cols_vector])
        DT::datatable(tb_data) 
        
    })
    
    output$value <- renderPrint({
        str(input$file)
    })
    
    output$select_columns <- renderUI({
        req(data())
        checkboxGroupInput("server_columns", "Select columns to display", names(data()), selected = names(data()), inline = TRUE, width = '90%')
    })
    
    output$multiple_choice <- renderUI({
        req(data())
        radioButtons("server_multi", "Multiple choice", choices = c("All", "None", "Numeric", "Non-numeric"), inline = TRUE)
    })
    
    observeEvent(input$browser, browser())
}

# Run the application 
shinyApp(ui = ui, server = server)
