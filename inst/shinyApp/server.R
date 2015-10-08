

shinyServer(function(input, output, session){
  
  library(plotROC)
  
  data <- reactive({ 
    tmp <- get(input$data)  ## remove missing values
    #subset(tmp, !is.na(get(input$M)) & !is.na(get(input$D)))
    tmp
  })
  
  rocdata <- reactive({
      
    if(input$multi == TRUE & length(input$Ms) > 1){
      
      print(input$Ms)
      melt_roc(data(), input$D, unlist(input$Ms))
      
    } else {
      
      data()
      
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
        
        
        p1 <- ggplot(rocdata(), aes_string(d = "D", m = "M", color = "name")) + 
          geom_roc(show.legend = input$label == "" | is.null(input$label), n.cuts = input$n.cuts, labelround = input$round) + ggtitle(input$title)
        
    } else {
      
        p1 <- ggplot(rocdata(), aes_string(d = input$D, m = input$M)) + 
          geom_roc(show.legend = input$label == "" | is.null(input$label), n.cuts = input$n.cuts, labelround = input$round) + ggtitle(input$title)
        
    }
    
    
    
    p1
    
  })
  
  
  output$printPlot <- renderPlot({
    
    if(input$ci.at == "") ci.at <- NULL else {
      
      ci.at <- as.numeric(sapply(strsplit(input$ci.at, ",")[[1]], trim))
      
    }
    
    if(input$multi == TRUE & length(input$Ms) > 1){
      p1 <- thisggroc() + style_roc() + aes(color = NULL, linetype = name) + 
        geom_rocci(ci.at = ci.at, show.legend = input$label == "" | is.null(input$label))
    } else {
      p1 <- thisggroc() + style_roc() + 
        geom_rocci(ci.at = ci.at, show.legend = input$label == "" | is.null(input$label))
    }
    
    if(!is.null(input$label) & input$label != ""){
      
      labs <- sapply(strsplit(input$label, ",")[[1]], trim)
      p1 <- direct_label(p1, labels = labs, label.angle = input$angle, nudge_x =input$adj.x, nudge_y = input$adj.y)
      
    }
    p1
    
    })
  
  output$intPlot <- renderUI({
    p1 <- thisggroc()
    
    if(!is.null(input$label) & input$label != ""){
      
      labs <- sapply(strsplit(input$label, ",")[[1]], trim)
      p1 <- direct_label(p1, labels = labs, label.angle = input$angle, nudge_x =input$adj.x, nudge_y = input$adj.y)
      
    }
    HTML(export_interactive_roc(p1, hide.points = input$n.cuts > 20))
    
  })
  
  
  output$printDownload <- downloadHandler(
       filename = function() {
         paste('ROC-Plot-', Sys.Date(), '.pdf', sep='')
       },
       content = function(file) {
         
         if(input$ci.at == "") ci.at <- NULL else {
           
           ci.at <- as.numeric(sapply(strsplit(input$ci.at, ",")[[1]], trim))
           
         }
         
         if(input$multi == TRUE & length(input$Ms) > 1){
           p1 <- thisggroc() + style_roc() + aes(color = NULL, linetype = name) + 
             geom_rocci(ci.at = ci.at, show.legend = input$label == "" | is.null(input$label))
         } else {
           p1 <- thisggroc() + style_roc() + 
             geom_rocci(ci.at = ci.at, show.legend = input$label == "" | is.null(input$label))
         }
         
         if(!is.null(input$label) & input$label != ""){
           
           labs <- sapply(strsplit(input$label, ",")[[1]], trim)
           p1 <- direct_label(p1, labels = labs, label.angle = input$angle, nudge_x =input$adj.x, nudge_y = input$adj.y)
           
         }
         
         
         ggsave(filename = file, 
                plot = p1,
                width = 7, height = 7, device = pdf)
         
         
       }
    )
  
  output$activeDownload <- downloadHandler(
    filename = function() {
      paste('ROC-Interactive-Plot-', Sys.Date(), '.html', sep='')
    },
    content = function(file) {
      p1 <- thisggroc()
      
      if(!is.null(input$label) & input$label != ""){
        
        labs <- sapply(strsplit(input$label, ",")[[1]], trim)
        p1 <- direct_label(p1, labels = labs, label.angle = input$angle, nudge_x =input$adj.x, nudge_y = input$adj.y)
        
      }
      cat("<!DOCTYPE html>
<html xmlns=\"http://www.w3.org/1999/xhtml\">
\n", file = file)
      cat(export_interactive_roc(p1, hide.points = input$n.cuts > 20), file = file, append = TRUE)
      cat("\n</html>", file = file, append = TRUE)
      
      
    }
  )
  
})
  
  
  
  
  
  
  
  
