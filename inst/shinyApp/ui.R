
shinyUI(navbarPage("ROC Plot",
                   tabPanel("Plot an ROC Curve", 
                            
                            fluidPage(title = "ROC Plot", 
                                      
                                      fluidRow(
                                        column(6, 
                                               h4("Interactive Plot"), 
                                               helpText("Mouse over to view cutoff labels. Click to view confidence region."),
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
                                               h4("Upload and select data"), 
                                               fileInput("upload", strong("Upload Data"), accept = c('text/csv', 'text/comma-separated-values,text/plain'), multiple = TRUE), 
                                               helpText("Files must be comma-delimited, with header containing column names. At least one column must contain the binary (0/1) outcome, and one must contain the continuous marker or predictor."),
                                               selectInput("data", strong("Select dataset to analyze"), choices = c("example"), selected = "example"))
                                               , 
                                        column(4, 
                                               h4("Specify Variables"),
                                               selectInput("D", "Outcome variable", choices = colnames(example), selected = colnames(example)[3]), 
                                               checkboxInput("multi", "Check to plot multiple curves"), 
                                               conditionalPanel("input.multi == false",
                                                  selectInput("M", "Marker", choices = colnames(example), selected = colnames(example)[1])
                                               ), 
                                               conditionalPanel("input.multi == true", 
                                                  selectInput("Ms", "Marker", choices = colnames(example), multiple = TRUE, selected = colnames(example)[1])              
                                                                ), 
                                               numericInput("alpha", "Significance level", .05, min = .01, max = .99, step = .01), 
                                               textInput("ci.at", "Cutoffs for CIs (separate multiple by commas, leave blank to autoselect, NULL for no CIs)", value = "")), 
                                        column(4, 
                                               h4("Plot options"),
                                               textInput("title", "Plot Title", value = ""), 
                                               textInput("label", "Curve Labels", value = ""), 
                                               helpText("separate multiple curve labels with commas"),
                                               numericInput("adj.x", "label adjust X", 0, min = -1, max = 1, step = .01),
                                               numericInput("adj.y", "label adjust Y", 0, min = -1, max = 1, step = .01),
                                               numericInput("angle", "label angle", 45, min = 0, max = 180, step = 1), 
                                               numericInput("n.cuts", "Number of cutoff values", 20, min = 0, max = 20), 
                                               numericInput("round", "Number of significant digits", 1, min = 0, max = 4, step = 1)
                                               )
                                      )
                                      
                                      )
                            
                            ), 
                   tabPanel("Help and Documentation", 
                            bootstrapPage(
                              includeHTML("help.html")
                                   )
                            ), 
                   tabPanel("About", 
                            bootstrapPage(
                              includeHTML("about.html")
                              )
                            ))
        
) ## end ui

