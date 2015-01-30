

shinyServer(function(input, output, session){
  
  library(plotROC)
  library(ggthemes)
  library(ggplot2)
  library(gridSVG)
  
  data <- reactive({ 
    tmp <- get(input$data)  ## remove missing values
    #subset(tmp, !is.na(get(input$M)) & !is.na(get(input$D)))
    tmp
  })
  
  rocdata <- reactive({
      
    if(input$multi == TRUE & length(input$Ms) > 1){
      
      calculate_multi_roc(data(), input$Ms, input$D)
      
    } else {
      
      calculate_roc(data()[,input$M], data()[,input$D], ci = TRUE, alpha = input$alpha)
      
    }
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
    updateSelectInput(session, "Ms", choices = cne, selected = cne[1])
    updateSelectInput(session, "D", choices = cne, selected = cne[2])
    
    updateNumericInput(session, "n.cuts", max = nrow(data()))
    }
  })
  
  thisggroc <- reactive({
    
    if(input$multi == TRUE & length(input$Ms) > 1){
        validate(
          need(length(unique(data()[,input$D])) == 2, 'Outcome must only have 2 levels.'),
          need(input$Ms != '', 'Please choose a marker variable.')
        )
        
        if(input$label == "") lab <- NULL else { 
            lab <- sapply(strsplit(input$label, ",")[[1]], trim)
            validate(need(length(lab) == length(rocdata()), "Separate labels with commas"))
          } 
        multi_ggroc(rocdata(), label = lab, label.adj.x = rep(input$adj.x, length(rocdata())), 
                    label.adj.y = rep(input$adj.y, length(rocdata())), label.angle = rep(input$angle, length(rocdata()))) + ggtitle(input$title)
    } else {
    
      if(input$label == "") lab <- NULL else lab <- input$label 
        ggroc(rocdata(), ci = TRUE, label = lab, label.adj.x = input$adj.x, label.adj.y = input$adj.y, label.angle = input$angle) + ggtitle(input$title)
        
    }
    
  })
  
  
  output$printPlot <- renderPlot({
    
    if(input$ci.at == "") ci.at <- NULL else {
      
      ci.at <- as.numeric(sapply(strsplit(input$ci.at, ",")[[1]], trim))
      
    }
    plot_journal_roc(thisggroc(), font.size = input$font.size/4, n.cuts = input$n.cuts, ci.at = ci.at)
    
    })
  
  output$intPlot <- renderUI({
    
    HTML(export_interactive_roc(thisggroc(), font.size = paste0(input$font.size, "px")))
    
  })
  
  
  output$printDownload <- downloadHandler(
       filename = function() {
         paste('ROC-Plot-', Sys.Date(), '.pdf', sep='')
       },
       content = function(file) {
         
         if(input$ci.at == "") ci.at <- NULL else {
           
           ci.at <- as.numeric(sapply(strsplit(input$ci.at, ",")[[1]], trim))
           ci.at <- ci.at[!is.na(ci.at)]
           
         }
         
         ggsave(filename = file, 
                plot = plot_journal_roc(thisggroc(), font.size = input$font.size/4, n.cuts = input$n.cuts, ci.at = ci.at), 
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
      cat(export_interactive_roc(thisggroc(), font.size = paste0(input$font.size, "px")), file = file, append = TRUE)
      cat("\n</html>", file = file, append = TRUE)
      
      
    }
  )
  
})
  
  
  
  
  
  
  
  
