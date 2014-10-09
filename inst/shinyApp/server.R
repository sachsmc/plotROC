

shinyServer(function(input, output, session){
  
  library(plotROC)
  library(ggthemes)
  library(ggplot2)
 
  
  data <- reactive({ 
    tmp <- get(input$data)  ## remove missing values
    #subset(tmp, !is.na(get(input$M)) & !is.na(get(input$D)))
    tmp
  })
  
  rocdata <- reactive({
      
      calculate_roc(data()[,input$M], data()[,input$D])
    
  })
  
  
  observe({
    
    if(length(input$upload) != 0){ 
      
      for(j in input$upload$datapath){
        
        assign(input$upload$name, read.csv(j), envir = .GlobalEnv)
        
      }
      
      
      updateSelectInput(session, "data",
                        choices = c("example", input$upload$name), selected = input$upload$name)
      
      
    }
    
  })
  
  observe({
    
    if(length(input$upload) != 0){ 
    cne <- colnames(data())
    
    updateSelectInput(session, "M", choices = cne, selected = cne[1])
    updateSelectInput(session, "D", choices = cne, selected = cne[2])
    
    updateNumericInput(session, "n.cuts", max = nrow(data()))
    }
  })
  
  output$printPlot <- renderPlot({
    
    validate(
      need(length(unique(data()[,input$D])) == 2, 'Outcome must only have 2 levels.'),
      need(input$M != '', 'Please choose a marker variable.')
    )
    
    printize_roc(ggroc(rocdata()) + ggtitle(input$title), rocdata(), font.size = input$font.size/4, n.cuts = input$n.cuts)
    
    })
  
  output$intPlot <- renderUI({
    
    validate(
      need(length(unique(data()[,input$D])) == 2, 'Outcome must only have 2 levels.'),
      need(input$M != '', 'Please choose a marker variable.')
    )
    
    HTML(svgize_roc(ggroc(rocdata()) + ggtitle(input$title), rocdata()$c, font.size = paste0(input$font.size, "px")))
    
  })
  
  
  output$printDownload <- downloadHandler(
       filename = function() {
         paste('ROC-Plot-', Sys.Date(), '.pdf', sep='')
       },
       content = function(file) {
         
         ggsave(filename = file, 
                plot = printize_roc(ggroc(rocdata()) + ggtitle(input$title), rocdata(), font.size = input$font.size/4, n.cuts = input$n.cuts), 
                width = 7, height = 7, device = pdf)
         
         
       }
    )
  
  output$activeDownload <- downloadHandler(
    filename = function() {
      paste('ROC-Interactive-Plot-', Sys.Date(), '.html', sep='')
    },
    content = function(file) {
      
      cat("<!DOCTYPE html>
<html xmlns=\"http://www.w3.org/1999/xhtml\">
\n", file = file)
      cat(svgize_roc(ggroc(rocdata()) + ggtitle(input$title), rocdata()$c, font.size = paste0(input$font.size, "px")), file = file, append = TRUE)
      cat("\n</html>", file = file, append = TRUE)
      
      
    }
  )
  
})
  
  
  
  
  
  
  
  
