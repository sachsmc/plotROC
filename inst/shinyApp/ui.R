
shinyUI(navbarPage("ROC Plot",
                   tabPanel("Plot an ROC Curve", 
                            
                            fluidPage(title = "ROC Plot", 
                                      
                                      fluidRow(
                                        column(6, 
                                               h4("Interactive Plot"), 
                                               htmlOutput("intPlot"),
                                               downloadButton("activeDownload", label = "Download", class = NULL)
                                               ), 
                                        column(6, 
                                               h4("Print Plot"), 
                                               plotOutput("printPlot", width = "450px", height = "450px"), 
                                               downloadButton("printDownload", label = "Download", class = NULL)
                                               )
                                        ), 
                                      
                                      fluidRow(
                                        column(4, 
                                               h4("Select and upload data"), 
                                               selectInput("data", "Data set to analyze", choices = c("example"), selected = "example"),
                                               fileInput("upload", "Upload Data", accept = c('text/csv', 'text/comma-separated-values,text/plain'), multiple = TRUE), 
                                               helpText("Files must be comma-delimited, with header containing column names. At least one column must contain the binary (0/1) outcome, and one must contain the continuous marker or predictor.")), 
                                        column(4, 
                                               h4("Specify Variables"),
                                               selectInput("D", "Outcome variable", choices = colnames(example), selected = colnames(example)[3]), 
                                               selectInput("M", "Marker", choices = colnames(example)), selected = colnames(example)[1]), 
                                        column(4, 
                                               h4("Plot options"),
                                               textInput("title", "Plot Title", value = ""), 
                                               numericInput("n.cuts", "Number of cutoff labels", 20, min = 0, max = 20), 
                                               numericInput("font.size", "Label Font Size", 12, min = 1, max = 32)
                                               )
                                      )
                                      
                                      )
                            
                            ), 
                   tabPanel("Help and Documentation"), 
                   tabPanel("About"))
        
) ## end ui

