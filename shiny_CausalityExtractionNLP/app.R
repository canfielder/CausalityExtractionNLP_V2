# Import and Pre-processing ---------------------------------------------------
##  Execute All Scripts
script_path <- "./www/R"
file_paths <- list.files(path = script_path, pattern = "*.R", full.names = TRUE)

print(file_paths)

for (file in file_paths){
    source(file)
}

# Install Packages
shiny_install_packages()

# Install Tika Jar File
tika_install_jar()

# Import LIME Explainer
explainer <- import_lime_explainer()
print(explainer)

# Import Patterns File
patterns_col <- c("remove","comments")
patterns_raw <- read_excel(path = "./www/data/text_processing/patterns.xlsx",
                           col_names = patterns_col)
patterns <- patterns_raw %>% pull(remove)


# UI --------------------------------------------------------------------------
ui <- fluidPage(
    
    titlePanel("Causal Knowledge Extraction"),
    
    # SIDEBAR
    sidebarLayout(
        sidebarPanel(
            fileInput(
                inputId = "file1", 
                label = "Upload PDF File",
                multiple = FALSE,
                accept = c(".pdf"),
                width = '90%'
            )
        ),
        # MAIN
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Reference",
                    textOutput("file1_path"),
                    uiOutput(outputId = "pdf_view")
                ),
                tabPanel(
                    "Text - Raw",
                    textOutput("text_raw")
                ),
                tabPanel(
                    "Text - Processed",
                    textOutput("text_pr")
                ),
                tabPanel(
                    "Hypothesis Table",
                    DTOutput("hypothesis")
                ),
                tabPanel(
                    "LIME Explanation - Table",
                    DTOutput("lime_explanation_table")
                ), 
                tabPanel(
                    "LIME Explanation - Plot",
                    text_explanations_output("text_explanations_plot")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
# Server ----------------------------------------------------------------------
server <- function(input, output) {
    
    # - Reactive Values -------------------------------------------------------
    
    # Convert Uploaded PDF to Text
    pdf_txt_reactive <- reactive({
        # Wait until File is Uploaded
        req(input$file1)
        
        # Convert PDF to Raw Text
        pdf_raw <- tika_text(input$file1$datapath)
        pdf_raw
    })
    
    # Extract Path to Uploaded PDF
    pdf_path_reactive <- reactive({
        # Wait until File is Uploaded
        req(input$file1)
        
        input$file1$datapath
    })
    
    # Generate Explanations - REactive
    explanation_reactive <- reactive({
        req(hypotheses_reactive)
        
        # Define Hypotheses
        def_hypo_xtr <- hypotheses_reactive()
        
        # Extract  Hypothesis Text as Vector
        vec_hypo_xtr <- def_hypo_xtr %>% 
            select(hypothesis) %>% 
            pull()
        
        explanation <- lime::explain(
            x = vec_hypo_xtr, 
            explainer = explainer, 
            n_labels = 1, 
            n_features = 20
        )
        explanation
    })
    
    output$file1_path <- renderText({
        pdf_path_reactive()
    })
    
    # Generate Hypotheses - Reactively
    hypotheses_reactive <- reactive({
        # Wait Until Text is Processed
        req(pdf_txt_reactive)
        
        # Convert PDF to Raw Text
        pdf_txt_raw <- pdf_txt_reactive()
        
        # Process Text
        pdf_txt_pr <- process_text(input_text = pdf_txt_raw,
                                   removal_patterns = patterns)
        # Extract Hypotheses
        hypo_xtr <- extract_hypothesis(pdf_txt_pr)
        hypo_xtr
    })
    
    ## Outputs to UI ----------------------------------------------------------
    
    output$pdf_view <- renderUI({
        tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                    src=pdf_path_reactive())
    })
    
    output$text_raw <- renderText({
        pdf_txt_reactive()
    })
    
    output$text_pr <- renderText({
        pdf_txt_raw <- pdf_txt_reactive()
        pdf_txt_pr <- process_text(input_text = pdf_txt_raw,
                                   removal_patterns = patterns)
        
        pdf_txt_pr
    })
    
    output$hypothesis <- DT::renderDT({
        hypotheses_reactive()
    })
    
    output$lime_explanation_table <- DT::renderDT({
        explanation_reactive()
    })
    
    output$text_explanations_plot <- render_text_explanations({
        explanation <- explanation_reactive()
        plot_text_explanations(explanation)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
