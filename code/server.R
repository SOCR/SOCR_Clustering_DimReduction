library(ClusterR)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(kernlab)
library(plotly)
library(ppclust)
source("ui.R")
library(e1071)
library(caTools)
library(rpart)
library(tree)
options(shiny.maxRequestSize=50*1024^2)  # Limit file import to 50 MB
library(flexmix)
library(MASS)
library(htmlwidgets)
library(htmltools)
library(stringr)


Group1_global <- NULL
Group2_global <- NULL
p_values_grid_global <- NULL
f_wetch_global <- NULL
p_value_global <- NULL
omega_global <- NULL
obs_global <- NULL
CI_first_global <- NULL
CI_second_global <- NULL


server <- shinyServer(function(input, output, session){
  df <- reactive({
    if (is.null(input$csv_file))
      return(read.csv("iris.csv"))
    data<-read.csv(input$csv_file$datapath)
    return(data)})
  test_set <- reactive({
    if (is.null(input$test_file))
      return(read.csv("iris.csv"))
    data<-read.csv(input$test_file$datapath)
    validate(
      need(setequal(colnames(df()), colnames(data)), "The column names or amount mismatch to the training data!")
    )
    return(data)})
  observe({
    # Can also set the label and select items
    if(ncol(df()) < 2){
      return
    }
    
    if(ncol(df()) > 2){
      updateSelectInput(session, "inSelect3",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_Spectral",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_Kmeans",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_Cmeans",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_DT",
                        label = paste("Select which columns as feature:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_HC",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_PC",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_BC",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
    }
    updateSelectInput(session, "inSelect",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    updateSelectInput(session, "inSelect_Spectral",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2_Spectral",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    updateSelectInput(session, "inSelect_HC",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2_HC",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    updateSelectInput(session, "inSelect_PC",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2_PC",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    #---------- binomial 2d---------------
    updateSelectInput(session, "inSelect0_BC",
                      label = paste("Select success/failure column:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    updateSelectInput(session, "inSelect_BC",
                      label = paste("Select total number column:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-2]
    )
    updateSelectInput(session, "inSelect2_BC",
                      label = paste("Select factor column:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect4_BC",
                      label = paste("Select fixed column:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-3]
    )
    #---------- Kmeans 2d---------------
    updateSelectInput(session, "inSelect_Kmeans",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2_Kmeans",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    
    updateSelectInput(session, "inSelect_Cmeans",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2_Cmeans",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    updateSelectInput(session, "inSelect_DT",
                      label = paste("Select which columns as feature:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    updateSelectInput(session, "inSelect2_DT",
                      label = paste("Select which columns as feature:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-2]
    )
    updateSelectInput(session, "inSelect_label_DT",
                      label = paste("Select which columns as label:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "data_column", choices = names(df()), selected=names(df())[1])
    updateSelectInput(session, "group_column", choices = names(df()), selected=names(df())[length(names(df()))])
  })
  output$rawdata <- renderTable(df())
  
  #----------------------------------------------------------------------------------------------
  # k-means
  data_Kmeans_result <- reactive({
    data_input <- df()
    if (input$kmeans_plot == "2D"){
      validate(
        need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
      )
      validate(
        need(input$inSelect_Kmeans != input$inSelect2_Kmeans, "Choose different columns of dataset!")
      )
      data_input_plot_Kmeans <- data.frame(X1 = data_input[input$inSelect_Kmeans], X2 = data_input[input$inSelect2_Kmeans])
      colnames(data_input_plot_Kmeans) = c("X1", "X2")
      data_input_plot_Kmeans$group <- kkmeans(data.matrix(data_input_plot_Kmeans),centers=input$clustnum_kmeans)[1:nrow(data_input_plot_Kmeans)]
      return(data_input_plot_Kmeans)
    } else {
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      validate(
        need(input$inSelect_Kmeans != input$inSelect2_Kmeans && input$inSelect_Kmeans != input$inSelect3_Kmeans && input$inSelect2_Kmeans != input$inSelect3_Kmeans, "Choose different columns of dataset!")
      )
      data_input_plot_3D_K <- data.frame(X1 = data_input[input$inSelect_Kmeans], X2 = data_input[input$inSelect2_Kmeans], X3 = data_input[input$inSelect3_Kmeans])
      colnames(data_input_plot_3D_K) = c("X1", "X2", "X3")
      data_input_plot_3D_K$group <- kkmeans(data.matrix(data_input_plot_3D_K),centers=input$clustnum_kmeans)[1:nrow(data_input_plot_3D_K)]
      return(data_input_plot_3D_K)
    }
  })
  # KMeans download result
  output$KM_download_result <- downloadHandler(
    filename = function() {
      paste("kmeans-result-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_Kmeans_result(), file, row.names = FALSE)
    }
  )
  
  # KMeans download result in html
  output$downloadPlot_KM <- downloadHandler(
    filename = function() {
      paste("plot_K-Means", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      data_to_plot <- data_Kmeans_result()  # This should be your reactive data source
      data_to_plot$group <- as.factor(data_to_plot$group)  # Ensure group is a factor
      
      if (input$kmeans_plot == "2D") {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~group, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "2D K-Means Model", xaxis = list(title = input$inSelect_Kmeans), yaxis = list(title = input$inSelect2_Kmeans))
      } else {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~group, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "3D K-Means Model",scene = list(xaxis = list(title = input$inSelect_Kmeans), yaxis = list(title = input$inSelect2_Kmeans), zaxis = list(title = input$inSelect3_Kmeans)))
      }
      
      plotly_built <- plotly::plotly_build(p)
      htmlwidgets::saveWidget(plotly_built, file)
    }
  )
  #----------------------------------------------------------------------------------------------
  # Gaussian
  data_Guassian_result <- reactive({
    data_input <- df()
    data_input <- data_input[complete.cases(data_input), ]
    if (input$GMM_plot == "2D"){
      validate(
        need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
      )
      validate(
        need(input$inSelect != input$inSelect2, "Choose different columns of dataset!")
      )
      data_input_plot <- data.frame(X1 = data_input[input$inSelect], X2 = data_input[input$inSelect2])
      colnames(data_input_plot) = c("X1", "X2")
      
      validate(
        need(!is.character(data_input_plot$X1) && !is.character(data_input_plot$X2), "Gaussian Mixture currently does not support string input.")  
      )
      gmm <- GMM(data_input_plot,input$clustnum_GMM)
      data_input_plot$group <- predict(gmm,data_input_plot)
      return(data_input_plot)
    } 
    else{
      
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      
      validate(
        need(input$inSelect != input$inSelect2 && input$inSelect != input$inSelect3 && input$inSelect2 != input$inSelect3, "Choose different columns of dataset!")
      )
      
      data_input_plot <- data.frame(X1 = data_input[input$inSelect], X2 =  data_input[input$inSelect2], X3 = data_input[input$inSelect3])
      colnames(data_input_plot) <- c("X1", "X2", "X3")
      validate(
        need(!is.character(data_input_plot$X1) && !is.character(data_input_plot$X2)&& !is.character(data_input_plot$X3), "Gaussian Mixture currently does not support string input.")  
      )
      gmm <- GMM(data_input_plot,input$clustnum_GMM)
      data_input_plot$group <- predict(gmm,data_input_plot)
      return(data_input_plot)
    }
  })
  # Gaussian download result
  output$GS_download_result <- downloadHandler(
    filename = function() {
      paste("gaussian-result-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_Guassian_result(), file, row.names = FALSE)
    }
  )
  # Gaussian plot html download
  output$downloadPlot_GS <- downloadHandler(
    filename = function() {
      paste("plot_gaussian", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      data_to_plot <- data_Guassian_result()  # This should be your reactive data source
      data_to_plot$group <- as.factor(data_to_plot$group)  # Ensure group is a factor
      
      if (input$GMM_plot == "2D") {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~group, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "2D Gaussian Mixture Model", xaxis = list(title = input$inSelect), yaxis = list(title = input$inSelect2))
      } else {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~group, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "3D Gaussian Mixture Model", scene = list(xaxis = list(title = input$inSelect), yaxis = list(title = input$inSelect2), zaxis = list(title = input$inSelect3)))
      }
      
      plotly_built <- plotly::plotly_build(p)
      htmlwidgets::saveWidget(plotly_built, file)
    }
  )
  #----------------------------------------------------------------------------------------------
  # Spectral
  data_Spectral_result <- reactive({
    data_input <-df()
    if (input$Spectral_plot == "2D"){
      validate(
        need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
      )
      validate(
        need(input$inSelect_Spectral != input$inSelect2_Spectral, "Choose different columns of dataset!")
      )
      data_input_plot_Spectral <- data.frame(X1 = data_input[input$inSelect_Spectral], X2 = data_input[input$inSelect2_Spectral])
      colnames(data_input_plot_Spectral) <- c("X1", "X2")
      data_input_plot_Spectral$group <- specc(data.matrix(data_input_plot_Spectral),centers=input$clustnum_spec)
      return(data_input_plot_Spectral)
    }else{
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      validate(
        need(input$inSelect_Spectral != input$inSelect2_Spectral && input$inSelect_Spectral != input$inSelect3_Spectral && input$inSelect2_Spectral != input$inSelect3_Spectral, "Choose different columns of dataset!")
      )
      
      data_input_plot_Spectral <- data.frame(X1 = data_input[input$inSelect_Spectral], X2 = data_input[input$inSelect2_Spectral], X3 = data_input[input$inSelect3_Spectral])
      colnames(data_input_plot_Spectral) <- c("X1", "X2", "X3")
      data_input_plot_Spectral$group <- specc(data.matrix(data_input_plot_Spectral),centers=input$clustnum_spec)
      return(data_input_plot_Spectral)
    }
  })
  # spec download result
  output$Spec_download_result <- downloadHandler(
    filename = function() {
      paste("spectral-result-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_Spectral_result(), file, row.names = FALSE)
    }
  )
  # wpec download html result
  output$downloadPlot_SP <- downloadHandler(
    filename = function() {
      paste("plot_spectral", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      data_to_plot <- data_Spectral_result()  # This should be your reactive data source
      data_to_plot$group <- as.factor(data_to_plot$group)  # Ensure group is a factor
      
      if (input$Spectral_plot == "2D") {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~group, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "2D Spectral Clustering", xaxis = list(title = input$inSelect_Spectral), yaxis = list(title = input$inSelect2_Spectral))
      } else {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~group, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "3D Spectral Clustering", scene = list(xaxis = list(title = input$inSelect_Spectral), yaxis = list(title = input$inSelect2_Spectral), zaxis = list(title = input$inSelect3_Spectral)))
      }
      
      plotly_built <- plotly::plotly_build(p)
      htmlwidgets::saveWidget(plotly_built, file)
    }
  )
  #----------------------------------------------------------------------------------------------
  # Hierarchal
  data_hierarchical_result <- reactive({
    data_input <- df() # assuming df() is a reactive data source
    
    if (input$HC_plot == "2D") {
      validate(
        need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
      )
      validate(
        need(input$inSelect_HC != input$inSelect2_HC, "Choose different columns of dataset!")
      )
      data_for_clustering <- data.frame(X1 = data_input[[input$inSelect_HC]], X2 = data_input[[input$inSelect2_HC]])
      
    } else {
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      validate(
        need(input$inSelect_HC != input$inSelect2_HC && input$inSelect_HC != input$inSelect3_HC && input$inSelect2_HC != input$inSelect3_HC, "Choose different columns of dataset!")
      )
      data_for_clustering <- data.frame(X1 = data_input[[input$inSelect_HC]], X2 = data_input[[input$inSelect2_HC]], X3 = data_input[[input$inSelect3_HC]])
    }
    
    hc <- hclust(dist(data_for_clustering))
    cluster_assignments <- cutree(hc, k = input$clustnum_HC)
    data_for_clustering$cluster <- factor(cluster_assignments)
    return(data_for_clustering)
  })
  
  output$clusterchart_HC <- plotly::renderPlotly({
    data_to_plot <- data_hierarchical_result()
    
    if (input$HC_plot == "2D") {
      p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~cluster, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
        layout(title = "2D Hierarchical Clustering", xaxis = list(title = input$inSelect_HC), yaxis = list(title = input$inSelect2_HC))
    } else {
      p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~cluster, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
        layout(scene = list(xaxis = list(title = input$inSelect_HC), yaxis = list(title = input$inSelect2_HC), zaxis = list(title = input$inSelect3_HC)))
    }
    
    return(p)
  })
  
  #download as HTML plot
  output$downloadPlot_HC <- downloadHandler(
    filename = function() {
      paste("plot_Hierarchical", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      data_to_plot <- data_hierarchical_result()  # This should be your reactive data source
      
      if (input$HC_plot == "2D") {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~cluster, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "2D Hierarchical Clustering", xaxis = list(title = input$inSelect_HC), yaxis = list(title = input$inSelect2_HC))
      } else {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~cluster, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "3D Hierarchical Clustering", scene = list(xaxis = list(title = input$inSelect_HC), yaxis = list(title = input$inSelect2_HC), zaxis = list(title = input$inSelect3_HC)))
      }
      
      plotly_built <- plotly::plotly_build(p)  # Build the plotly object
      htmlwidgets::saveWidget(plotly_built, file)
    }
  )
  
  # HC download result
  output$HC_downloadResult <- downloadHandler(
    filename = function() {
      paste("hierarchal-result-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_hierarchical_result(), file, row.names = FALSE)
    }
  )
  
  #----------------------------------------------------------------------------------------------
  # Binomial
  data_binomial_result <- reactive({
    data_input <- df() 
    
    validate(
      need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
    )
    validate(
      need("No.Success" %in% colnames(data_input) && "No.Failure" %in% colnames(data_input),
           "The dataset must contain columns named 'No.Success' and 'No.Failure'!")
    )
    
    if (input$BC_plot == "2D") {
      validate(
        need(input$inSelect_BC != input$inSelect2_BC, "Choose different columns of dataset!"),
        need(input$inSelect0_BC != input$inSelect_BC, "Choose different columns of dataset!")
      )
      data_for_clustering <- data.frame(X1 = data_input[[cbind(input$inSelect0_BC,input$inSelect_BC - input$inSelect0_BC)]], X2 = data_input[[input$inSelect2_BC]], X4 = data_input[[input$inSelect4_BC]])
      
    } else {
      validate(
        need(input$inSelect0_BC != input$inSelect_BC && input$inSelect_BC != input$inSelect2_BC && input$inSelect_BC != input$inSelect3_BC && input$inSelect2_BC != input$inSelect3_BC, "Choose different columns of dataset!")
      )
      data_for_clustering <- data.frame(X1 = data_input[[cbind(input$inSelect0_BC,input$inSelect_BC - input$inSelect0_BC)]], X2 = data_input[[input$inSelect2_BC]], X3 = data_input[[input$inSelect3_BC]], X4 = data_input[[input$inSelect4_BC]])
    }
    
    # Using flexmix for clustering
    betaMix <- flexmix(X1 ~ 1 | X2, data = data_for_clustering, k = input$clustnum_PC, model = FLXMRglm(family = 'binomial', fixed = ~X4))
    #string <- str(model)
    #clus_sizes <- data.frame(string@size)
    #colnames(clus_sizes) <- c("size")
    #clus_sizes$ID <- seq.int(nrow(clus_sizes))
    
    #cluster_assignments <- clusters(clus_sizes)
    #data_for_clustering$cluster <- factor(cluster_assignments)
    
    return(betaMix)
  })
  output$clusterchart_BC <- renderPlot({
    # Create a plot based on your betaMix model
    # You can customize the plot as needed
    betaMix <- data_binomial_result()
    plot(betaMix)
  })
  
  #output$clusterchart_BC <- 
    #renderPlot({
    #data_to_plot <- plot(data_binomial_result())
    
    #if (input$BC_plot == "2D") {
    #  p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~cluster, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
    #    layout(title = "2D Clustering using flexmix", xaxis = list(title = input$inSelect_BC), yaxis = list(title = input$inSelect2_BC))
    #  
    #} else {
    #  p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~cluster, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
    #    layout(scene = list(xaxis = list(title = input$inSelect_BC), yaxis = list(title = input$inSelect2_BC), zaxis = list(title = input$inSelect3_BC)))
    #}
    
    #return(data_to_plot)
    
  #})
  
  # PC download result
  output$BC_downloadResult <- downloadHandler(
    filename = function() {
      paste("Binomial-result-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_binomial_result(), file, row.names = FALSE)
    }
  )
  
  #download as HTML plot
  output$downloadPlot_BC <- downloadHandler(
    filename = function() {
      paste("plot_Binomial", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      data_to_plot <- data_binomial_result()  # This should be your reactive data source
      
      if (input$PC_plot == "2D") {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~cluster, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "2D Binomial Clustering", xaxis = list(title = input$inSelect_BC), yaxis = list(title = input$inSelect2_BC))
      } else {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~cluster, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "3D Binomial Clustering", scene = list(xaxis = list(title = input$inSelect_BC), yaxis = list(title = input$inSelect2_BC), zaxis = list(title = input$inSelect3_BC)))
      }
      
      plotly_built <- plotly::plotly_build(p)  # Build the plotly object
      htmlwidgets::saveWidget(plotly_built, file)
    }
  )
  
  #----------------------------------------------------------------------------------------------
  # Poisson
  data_poisson_result <- reactive({
    data_input <- df() 
    
    validate(
      need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
    )
    
    if (input$PC_plot == "2D") {
      validate(
        need(input$inSelect_PC != input$inSelect2_PC, "Choose different columns of dataset!")
      )
      data_for_clustering <- data.frame(X1 = data_input[[input$inSelect_PC]], X2 = data_input[[input$inSelect2_PC]])
      
    } else {
      validate(
        need(input$inSelect_PC != input$inSelect2_PC && input$inSelect_PC != input$inSelect3_PC && input$inSelect2_PC != input$inSelect3_PC, "Choose different columns of dataset!")
      )
      data_for_clustering <- data.frame(X1 = data_input[[input$inSelect_PC]], X2 = data_input[[input$inSelect2_PC]], X3 = data_input[[input$inSelect3_PC]])
    }
    
    # Using flexmix for clustering
    tryCatch({
      model <- flexmix(X1 ~ 1 | X2, data = data_for_clustering, k = input$clustnum_PC, model = FLXMCmvpois())
      cluster_assignments <- clusters(model)
      data_for_clustering$cluster <- factor(cluster_assignments)
    }, error = function(e) {
      validate(need(FALSE, "Error occured while clustering, please check the dataset and parameters. Please notice that Poisson Clustering doesn't allow text data."))
    })
    
    
    return(data_for_clustering)
  })
  
  output$clusterchart_PC <- plotly::renderPlotly({
    data_to_plot <- data_poisson_result()
    
    if (input$PC_plot == "2D") {
      p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~cluster, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
        layout(title = "2D Clustering using flexmix", xaxis = list(title = input$inSelect_PC), yaxis = list(title = input$inSelect2_PC))
      
    } else {
      p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~cluster, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
        layout(scene = list(xaxis = list(title = input$inSelect_PC), yaxis = list(title = input$inSelect2_PC), zaxis = list(title = input$inSelect3_PC)))
    }
    
    return(p)
  })
  
  # PC download result
  output$PC_downloadResult <- downloadHandler(
    filename = function() {
      paste("Poisson-result-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_poisson_result(), file, row.names = FALSE)
    }
  )
  
  #download as HTML plot
  output$downloadPlot_PC <- downloadHandler(
    filename = function() {
      paste("plot_Poisson", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      data_to_plot <- data_poisson_result()  # This should be your reactive data source
      
      if (input$PC_plot == "2D") {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~cluster, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "2D Poisson Clustering", xaxis = list(title = input$inSelect_PC), yaxis = list(title = input$inSelect2_PC))
      } else {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~cluster, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "3D Poisson Clustering", scene = list(xaxis = list(title = input$inSelect_PC), yaxis = list(title = input$inSelect2_PC), zaxis = list(title = input$inSelect3_PC)))
      }
      
      plotly_built <- plotly::plotly_build(p)  # Build the plotly object
      htmlwidgets::saveWidget(plotly_built, file)
    }
  )
  
  #----------------------------------------------------------------------------------------------
  # Fuzzy c-means
  data_Cmeans_result <- reactive({
    data_input <-df()
    if (input$cmeans_plot == "2D"){
      validate(
        need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
      )
      validate(
        need(input$inSelect2_Cmeans != input$inSelect_Cmeans, "Choose different columns of dataset!")
      )
      data_input_plot_Cmeans <- data.frame(X1 = data_input[input$inSelect_Cmeans], X2 = data_input[input$inSelect2_Cmeans])
      colnames(data_input_plot_Cmeans) <- c("X1", "X2")
      data_input_plot_Cmeans$group <- fcm(data.matrix(data_input_plot_Cmeans),centers=input$clustnum_cmeans)$cluster
      return(data_input_plot_Cmeans)
    }else{
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      validate(
        need(input$inSelect_Cmeans != input$inSelect2_Cmeans && input$inSelect_Cmeans != input$inSelect3_Cmeans && input$inSelect2_Cmeans != input$inSelect3_Cmeans, "Choose different columns of dataset!")
      )
      data_input_plot_Cmeans <- data.frame(X1 = data_input[input$inSelect_Cmeans], X2 = data_input[input$inSelect2_Cmeans], X3 = data_input[input$inSelect3_Cmeans])
      colnames(data_input_plot_Cmeans) <- c("X1", "X2", "X3")
      data_input_plot_Cmeans$group <- fcm(data.matrix(data_input_plot_Cmeans),centers=input$clustnum_cmeans)$cluster
      return(data_input_plot_Cmeans)
    }
  })
  
  # FC download result
  output$FC_download_result <- downloadHandler(
    filename = function() {
      paste("fuzzy-cmeans-result-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_Cmeans_result(), file, row.names = FALSE)
    }
  )
  
  # FC download HTML result
  output$downloadPlot_FC <- downloadHandler(
    filename = function() {
      paste("plot_Fuzzy_C-means", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      data_to_plot <- data_Cmeans_result()  # This should be your reactive data source
      data_to_plot$group <- as.factor(data_to_plot$group)  # Ensure group is a factor
      
      if (input$cmeans_plot == "2D") {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~group, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "2D Fuzzy c-means Clustering", xaxis = list(title = input$inSelect_Cmeans), yaxis = list(title = input$inSelect2_Cmeans))
      } else {
        p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~group, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
          layout(title = "3D Fuzzy c-means Clustering", scene = list(xaxis = list(title = input$inSelect_Cmeans), yaxis = list(title = input$inSelect2_Cmeans), zaxis = list(title = input$inSelect3_Cmeans)))
      }
      
      plotly_built <- plotly::plotly_build(p)
      htmlwidgets::saveWidget(plotly_built, file)
    }
  )
  
  #---------cluster chart-------------------------------------------------------------------------------------
  output$kmeans_clusterchart <- plotly::renderPlotly({
    data_input_plot_Kmeans <- data_Kmeans_result()
    if (input$kmeans_plot == "2D"){
      plot_ly(x=data_input_plot_Kmeans$X1, y=data_input_plot_Kmeans$X2, z=data_input_plot_Kmeans$X3, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(data_input_plot_Kmeans$group))
    }
    else{
      plot_ly(x=data_input_plot_Kmeans$X1, y=data_input_plot_Kmeans$X2, z=data_input_plot_Kmeans$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_Kmeans$group))
    }
  })
  
  output$Gaussian_clusterchart <- plotly::renderPlotly({
    data_input_plot <- data_Guassian_result()
    if (input$GMM_plot == "2D"){
      plot_ly(x=data_input_plot$X1, y=data_input_plot$X2, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(data_input_plot$group))
    } 
    else{
      colnames(data_input_plot) <- c("X1", "X2", "X3","group")
      plot_ly(x=data_input_plot$X1, y=data_input_plot$X2, z=data_input_plot$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot$group))
    }
  })
  
  output$clusterchart_spectral <- plotly::renderPlotly({
    data_input_plot_Spectral <- data_Spectral_result()
    if (input$Spectral_plot == "2D"){
      plot_ly(x=data_input_plot_Spectral$X1, y=data_input_plot_Spectral$X2, z=data_input_plot_Spectral$X3, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(data_input_plot_Spectral$group))
    }else{
      plot_ly(x=data_input_plot_Spectral$X1, y=data_input_plot_Spectral$X2, z=data_input_plot_Spectral$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_Spectral$group))
    }
  })
  
  output$clusterchart_cmeans <- plotly::renderPlotly({
    data_input_plot_Cmeans <- data_Cmeans_result()
    if (input$cmeans_plot == "2D"){
      plot_ly(x=data_input_plot_Cmeans$X1, y=data_input_plot_Cmeans$X2, z=data_input_plot_Cmeans$X3, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(data_input_plot_Cmeans$group))
    }else{
      plot_ly(x=data_input_plot_Cmeans$X1, y=data_input_plot_Cmeans$X2, z=data_input_plot_Cmeans$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_Cmeans$group))
    }
  })
  training_test_index_DT <-reactive({
    data_input <- df()
    
    
    
    if(input$DT_plot == "2D") {
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      
      datasets <- data.frame(X1 = data_input[input$inSelect_DT],
                             X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3")
      split = sample.split(datasets$X3, SplitRatio = as.double(input$test_ratio_DT)/100)
      return (split)
    }
    else{
      validate(
        need(ncol(data_input) >= 4, "Need at least four columns in dataset!")
      )
      datasets <- data.frame(X1 = data_input[input$inSelect_DT],
                             X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect3_DT],
                             X4 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3", "X4")
      split = sample.split(datasets$X4, SplitRatio = as.double(input$test_ratio_DT)/100)
      return (split)
    }
    
  })
  classifier_DT <- reactive({
    data_input <- df()
    if(input$DT_plot == "2D"){
      datasets <- data.frame(X1 = data_input[input$inSelect_DT], X2 =  data_input[input$inSelect2_DT], X3 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3")
      training_set = datasets[training_test_index_DT() == FALSE,]
      test_set = datasets[training_test_index_DT() == TRUE,]
      tree = rpart(training_set$X3 ~., data = training_set)
      return (tree)
      
    }
    else{
      datasets <- data.frame(X1 = data_input[input$inSelect_DT], X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect3_DT],X4 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3", "X4")
      training_set = datasets[training_test_index_DT() == FALSE,]
      test_set = datasets[training_test_index_DT() == TRUE,]
      tree = rpart(training_set$X4 ~., data = training_set)
      return (tree)
    }
  })
  predicted_result_DT <- reactive({
    data_input <- df()
    
    if(input$DT_plot == "2D") {
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      datasets <- data.frame(X1 = data_input[input$inSelect_DT], X2 =  data_input[input$inSelect2_DT], X3 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3")
      test_set = datasets[training_test_index_DT() == TRUE,]
      result = predict(classifier_DT(), test_set, type = 'c')
      return (result)
    }
    else{
      validate(
        need(ncol(data_input) >= 4, "Need at least four columns in dataset!")
      )
      datasets <- data.frame(X1 = data_input[input$inSelect_DT], X2 =  data_input[input$inSelect2_DT],
                             x3 = data_input[input$inSelect3_DT],
                             X4 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3", "X4")
      test_set = datasets[training_test_index_DT() == TRUE,]
      result = predict(classifier_DT(), test_set, type = 'c')
      return (result)
    }
  })
  output$clusterchart_DT<- plotly::renderPlotly({
    data_input <- df()
    if(input$DT_plot == "2D") {
      datasets <- data.frame(X1 = data_input[input$inSelect_DT],
                             X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3")
      result = predicted_result_DT()
      index = as.integer(names(result))
      correct = datasets[training_test_index_DT() == TRUE,]
      accuracy = 0
      labels = c()
      for (i in 1:length(result)) {
        if (toString(result[i]) == toString(correct$X3[i])) {
          accuracy = accuracy + 1
        }
        labels <- append(labels, toString(result[i]))
      } 
      accuracy = toString(accuracy/length(correct$X3) * 100)
      fig1 <-  plot_ly(x=datasets[index,]$X1, y=datasets[index,]$X2, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(labels))%>%
        layout(title = paste('Accuracy: ', accuracy, "%",sep=""))
    }
    else {
      datasets <- data.frame(X1 = data_input[input$inSelect_DT],
                             X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect3_DT],
                             X4 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3", "X4")
      result = predicted_result_DT()
      index = as.integer(names(result))
      correct = datasets[training_test_index_DT() == TRUE,]
      accuracy = 0
      labels = c()
      for (i in 1:length(result)) {
        if (toString(result[i]) == toString(correct$X4[i])) {
          accuracy = accuracy + 1
        }
        labels <- append(labels, toString(result[i]))
      }
      accuracy = toString(accuracy/length(correct$X4) * 100)
      fig1 <-  plot_ly(x=datasets[index,]$X1, y=datasets[index,]$X2,
                       z = datasets[index,]$X3,
                       type="scatter3d", marker=list(size = 12),
                       mode="markers", color=as.factor(labels))%>%
        layout(title = paste('Accuracy: ', accuracy, "%",sep=""))
    }
  })
  output$clusterchart_DT_correct <- plotly::renderPlotly({
    data_input <- df()
    if(input$DT_plot == "2D") {
      datasets <- data.frame(X1 = data_input[input$inSelect_DT], X2 =  data_input[input$inSelect2_DT], X3 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3")
      correct = datasets[training_test_index_DT() == TRUE,]
      fig1 <-  plot_ly(x=correct$X1, y=correct$X2, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(correct$X3))%>%
        layout(title = "Actual test set")
    }
    else{
      datasets <- data.frame(X1 = data_input[input$inSelect_DT],
                             X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect3_DT],
                             X4 = data_input[input$inSelect_label_DT])
      colnames(datasets) <- c("X1", "X2", "X3","X4")
      correct = datasets[training_test_index_DT() == TRUE,]
      fig1 <-  plot_ly(x=correct$X1, y=correct$X2, z = correct$X3,
                       type="scatter3d", marker=list(size = 12), mode="markers", color=as.factor(correct$X4))%>%
        layout(title = "Actual test set")
    }
    
  })
  predict_result <- reactive({
    data_input <- df()
    test_file <- test_set()
    if(input$DT_plot == "2D") {
      datasets <- data.frame(X1 = data_input[input$inSelect_DT],
                             X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect_label_DT])
      test_set = data.frame(X1 = test_file[input$inSelect_DT],
                            X2 =  test_file[input$inSelect2_DT])
      colnames(datasets) <- c("X1", "X2", "X3")
      colnames(test_set) <- c("X1", "X2")
      tree = rpart(datasets$X3 ~., data = datasets)
      result = predict(tree, test_set, type = 'class')
      labels = c()
      for (i in 1:length(result)) {
        labels <- append(labels, toString(result[i]))
      }
      test_file$DT_result <- labels
      return(test_file)
    } else{
      datasets <- data.frame(X1 = data_input[input$inSelect_DT],
                             X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect3_DT],
                             X4 = data_input[input$inSelect_label_DT])
      test_set = data.frame(X1 = test_file[input$inSelect_DT],
                            X2 =  test_file[input$inSelect2_DT],
                            X3 = data_input[input$inSelect3_DT])
      colnames(datasets) <- c("X1", "X2", "X3", "X4")
      colnames(test_set) <- c("X1", "X2", "X3")
      tree = rpart(datasets$X4 ~., data = datasets)
      result = predict(tree, test_set, type = 'class')
      labels = c()
      for (i in 1:length(result)) {
        labels <- append(labels, toString(result[i]))
      }
      test_file$DT_result <- labels
      return(test_file)
    }
  })
  output$test_plot_DT <- plotly::renderPlotly({
    test_file <- test_set()
    data_input <- df()
    if(input$DT_plot == "2D") {
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      datasets <- data.frame(X1 = data_input[input$inSelect_DT],
                             X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect_label_DT])
      test_set = data.frame(X1 = test_file[input$inSelect_DT],
                            X2 =  test_file[input$inSelect2_DT])
      colnames(datasets) <- c("X1", "X2", "X3")
      colnames(test_set) <- c("X1", "X2")
      tree = rpart(datasets$X3 ~., data = datasets)
      result = predict(tree, test_set, type = 'c')
      labels = c()
      for (i in 1:length(result)) {
        labels <- append(labels, toString(result[i]))
      }
      fig1 <-  plot_ly(x=test_set$X1, y=test_set$X2, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(labels))%>%
        layout(title = "test set result")
    } else{
      validate(
        need(ncol(data_input) >= 4, "Need at least four columns in dataset!")
      )
      datasets <- data.frame(X1 = data_input[input$inSelect_DT],
                             X2 =  data_input[input$inSelect2_DT],
                             X3 = data_input[input$inSelect3_DT],
                             X4 = data_input[input$inSelect_label_DT])
      test_set = data.frame(X1 = test_file[input$inSelect_DT],
                            X2 =  test_file[input$inSelect2_DT],
                            X3 = data_input[input$inSelect3_DT])
      colnames(datasets) <- c("X1", "X2", "X3", "X4")
      colnames(test_set) <- c("X1", "X2", "X3")
      tree = rpart(datasets$X4 ~., data = datasets)
      result = predict(tree, test_set, type = 'class')
      labels = c()
      for (i in 1:length(result)) {
        labels <- append(labels, toString(result[i]))
      }
      fig1 <-  plot_ly(x=test_set$X1, y=test_set$X2, z = test_set$X3,
                       type="scatter3d", marker=list(size = 12), mode="markers", color=as.factor(labels))%>%
        layout(title = "test set result")
    }
  })
  result <- reactive({
    data_input <- df()
    validate(
      need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
    )
    if(input$GMM_plot == "3D" || input$Spectral_plot == "3D" || input$cmeans_plot == "3D" || input$kmeans_plot == "3D"){
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
    }
    result_kmean <- data_Kmeans_result()$group
    result_GMM <- data_Guassian_result()$group
    result_spec <- data_Spectral_result()$group
    result_cmeans <- data_Cmeans_result()$group
    data_input$Gaussian_result <- as.integer(result_GMM)
    data_input$Kmean_result <- result_kmean[1:nrow(data_input)]
    data_input$Spectral_result <- result_spec[1:nrow(data_input)]
    data_input$cmeans_result <- as.integer(result_cmeans)
    return(data_input)
  })
  output$clustered_data <- renderTable(result())
  
  # DT download result
  output$DT_download_result <- downloadHandler(
    filename = "prediction_result.csv",
    content = function(fname){rediction_result.csv
      write.csv(predict_result(),fname)
    }
  )
  
  output$download <- downloadHandler(
    filename = "processed.csv",
    content = function(fname){
      write.csv(result(), fname)
    }
  )

output$anova_results <- renderTable({
  data_input <- df()
  if (is.null(data_input)) {
    return(NULL)
  }
  
  data_format <- input$data_format
  
  tryCatch({
    if (data_format == "Two columns") {
      if (ncol(data_input) != 2) {
        return("Fail to do computation: You have selected the 'two columns' format, but your data is not in the 'Two columns' format. Please select the correct format and try again.")
      }
      # Get the selected data column and group column
      data_col <- input$data_column
      group_col <- input$group_column
      
      if (data_col == "" || group_col == "") {
        return(NULL)
      }
      
      res <- aov(data_input[[data_col]] ~ data_input[[group_col]])
    } else if (data_format == "Multiple columns") {
      if (ncol(data_input) <= 2) {
        return("Fail to do computation: You have selected the 'Multiple columns' format, but your data is in the 'Two columns' format. Please select the correct format and try again.")
      }
      # Each column is a group, and each row is a data point
      res <- aov(as.formula(paste("values ~ ind", sep = "")), data = stack(data_input))
    }
    
    sum_res <- summary(res)
    sum_res_df <- as.data.frame(sum_res[[1]])
    sum_res_df
  }, error = function(e) {
    return("Error, incorrect format of the input data, please read the top section to know the data format")
  })
})

  #support for mutiple files downloading in rmd
  output$download_combined_plots_rmd <- downloadHandler(
    filename = function() {
        paste("combined_plot", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
        selected_methods <- isolate(input$model_selection)
        plot_names <- list()
        plot_codes <- list()
        
        # For each selected clustering method, add the plot code and its name to the lists

            if ("Poisson Clustering" %in% selected_methods) {
                # print("in poisson - rmd")
                current_inSelect_PC <- input$inSelect_PC
                current_inSelect2_PC <- input$inSelect2_PC
                current_inSelect3_PC <- input$inSelect3_PC
                data_to_plot_poisson <- data_poisson_result()  # This should be your reactive data source
                write.csv(data_to_plot_poisson, "data_to_plot_poisson.csv", row.names = FALSE)
                
                if (input$PC_plot == "2D") {
                    plot_code <- paste0("data_to_plot_poisson <- read.csv('data_to_plot_poisson.csv')\n",
                    "plot_ly(data = data_to_plot_poisson, x = ~X1, y = ~X2, color = ~cluster, type = 'scatter', mode = 'markers', marker = list(size = 10)) %>%",
                    "layout(title = '2D Poisson Clustering', xaxis = list(title = '", current_inSelect_PC, "'), yaxis = list(title = '", current_inSelect2_PC, "'))")
                } else {
                    plot_code <- paste0("data_to_plot_poisson <- read.csv('data_to_plot_poisson.csv')\n",
                    "plot_ly(data = data_to_plot_poisson, x = ~X1, y = ~X2, z = ~X3, color = ~cluster, type = 'scatter3d', mode = 'markers', marker = list(size = 10)) %>%",
                    "layout(title = '3D Poisson Clustering', scene = list(xaxis = list(title = '", current_inSelect_PC, "'), yaxis = list(title = '", current_inSelect2_PC, "'), zaxis = list(title = '", current_inSelect3_PC, "')))"
                    )
                }
                # Replace with actual plot generation code for Poisson Clustering
                plot_codes[[length(plot_codes) + 1]] <- plot_code
                plot_names[[length(plot_names) + 1]] <- "Poisson Clustering"
            }
            # ... Add similar if conditions for other methods
            # Fuzzy C-Means
            if ("Fuzzy C-Means" %in% selected_methods) {
                data_to_plot <- data_Cmeans_result()  # This should be your reactive data source
                data_to_plot$group <- as.factor(data_to_plot$group)  # Ensure group is a factor
                if (input$cmeans_plot == "2D") {
                p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~group, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
                    layout(title = "2D Fuzzy c-means Clustering", xaxis = list(title = input$inSelect_Cmeans), yaxis = list(title = input$inSelect2_Cmeans))
                } else {
                p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~group, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
                    layout(title = "3D Fuzzy c-means Clustering", scene = list(xaxis = list(title = input$inSelect_Cmeans), yaxis = list(title = input$inSelect2_Cmeans), zaxis = list(title = input$inSelect3_Cmeans)))
                }
                # Replace with actual plot generation code for Fuzzy C-Means
                plot_codes[[length(plot_codes) + 1]] <- p
                plot_names[[length(plot_names) + 1]] <- "Fuzzy C-Means"
            }
        # ... and so on for each clustering method
        
        
        # Generate a markdown string for all plots
        markdown_string <- ""
        for (i in seq_along(plot_names)) {
        markdown_string <- paste0(markdown_string, 
                                    "## ", plot_names[[i]], "\n", 
                                    "```{r, echo=FALSE}\n", 
                                    plot_codes[[i]], "\n", 
                                    "```\n\n")
        }
        
        # Write the markdown content to a temporary .Rmd file
        rmd_file <- tempfile(fileext = ".Rmd")
        writeLines(markdown_string, rmd_file)
        
        # Render the .Rmd to the HTML file
        rmarkdown::render(rmd_file, output_file = file, quiet = TRUE)

    }
  )
  
  # support for multiple files downloading
  output$download_combined_plots <- downloadHandler(
    filename = function() {
    #   print("in download combined function - filename")
      paste("combined_plot", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
    #   print("in download combined function - content")
      selected_methods <- isolate(input$model_selection)
      plots <- list()
      plot_names <- list()
      
      if ("Poisson Clustering" %in% selected_methods) {
        data_to_plot <- data_poisson_result()  # This should be your reactive data source
        
        if (input$PC_plot == "2D") {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~cluster, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "2D Poisson Clustering", xaxis = list(title = input$inSelect_PC), yaxis = list(title = input$inSelect2_PC))
        } else {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~cluster, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "3D Poisson Clustering", scene = list(xaxis = list(title = input$inSelect_PC), yaxis = list(title = input$inSelect2_PC), zaxis = list(title = input$inSelect3_PC)))
        }
        
        plots[[length(plots) + 1]] <- p
        plot_names[[length(plot_names) + 1]] <- "Poisson Clustering"
      }
      
      if ("Fuzzy C-Means" %in% selected_methods) {
        data_to_plot <- data_Cmeans_result()  # This should be your reactive data source
        data_to_plot$group <- as.factor(data_to_plot$group)  # Ensure group is a factor
        if (input$cmeans_plot == "2D") {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~group, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "2D Fuzzy c-means Clustering", xaxis = list(title = input$inSelect_Cmeans), yaxis = list(title = input$inSelect2_Cmeans))
        } else {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~group, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "3D Fuzzy c-means Clustering", scene = list(xaxis = list(title = input$inSelect_Cmeans), yaxis = list(title = input$inSelect2_Cmeans), zaxis = list(title = input$inSelect3_Cmeans)))
        }
        plots[[length(plots) + 1]] <- p
        plot_names[[length(plot_names) + 1]] <- "Fuzzy C-Mean"
      }
      
      if ("Hierarchical Clustering" %in% selected_methods) {
        data_to_plot <- data_hierarchical_result()  # This should be your reactive data source
        
        if (input$HC_plot == "2D") {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~cluster, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "2D Hierarchical Clustering", xaxis = list(title = input$inSelect_HC), yaxis = list(title = input$inSelect2_HC))
        } else {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~cluster, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "3D Hierarchical Clustering", scene = list(xaxis = list(title = input$inSelect_HC), yaxis = list(title = input$inSelect2_HC), zaxis = list(title = input$inSelect3_HC)))
        }
        
        plots[[length(plots) + 1]] <- p
        plot_names[[length(plot_names) + 1]] <- "Hierarchical Clustering"
      }
      
      if ("K-means" %in% selected_methods) {
        data_to_plot <- data_Kmeans_result()  # This should be your reactive data source
        data_to_plot$group <- as.factor(data_to_plot$group)  # Ensure group is a factor
        
        if (input$kmeans_plot == "2D") {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~group, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "2D K-Means Model", xaxis = list(title = input$inSelect_Kmeans), yaxis = list(title = input$inSelect2_Kmeans))
        } else {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~group, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "3D K-Means Model",scene = list(xaxis = list(title = input$inSelect_Kmeans), yaxis = list(title = input$inSelect2_Kmeans), zaxis = list(title = input$inSelect3_Kmeans)))
        }
        
        plots[[length(plots) + 1]] <- p
        plot_names[[length(plot_names) + 1]] <- "K-means Clustering"
      }
      
      if ("Gaussian Mixture" %in% selected_methods) {
        data_to_plot <- data_Guassian_result()  # This should be your reactive data source
        data_to_plot$group <- as.factor(data_to_plot$group)  # Ensure group is a factor
        
        if (input$GMM_plot == "2D") {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~group, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "2D Gaussian Mixture Model", xaxis = list(title = input$inSelect), yaxis = list(title = input$inSelect2))
        } else {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~group, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "3D Gaussian Mixture Model", scene = list(xaxis = list(title = input$inSelect), yaxis = list(title = input$inSelect2), zaxis = list(title = input$inSelect3)))
        }
        plots[[length(plots) + 1]] <- p
        plot_names[[length(plot_names) + 1]] <- "Gaussian Mixture Clustering"
      }
      
      if ("Spectral" %in% selected_methods) {
        data_to_plot <- data_Spectral_result()  # This should be your reactive data source
        data_to_plot$group <- as.factor(data_to_plot$group)  # Ensure group is a factor
        
        if (input$Spectral_plot == "2D") {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, color = ~group, type = "scatter", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "2D Spectral Clustering", xaxis = list(title = input$inSelect_Spectral), yaxis = list(title = input$inSelect2_Spectral))
        } else {
          p <- plot_ly(data = data_to_plot, x = ~X1, y = ~X2, z = ~X3, color = ~group, type = "scatter3d", mode = "markers", marker = list(size = 10)) %>%
            layout(title = "3D Spectral Clustering", scene = list(xaxis = list(title = input$inSelect_Spectral), yaxis = list(title = input$inSelect2_Spectral), zaxis = list(title = input$inSelect3_Spectral)))
        }
        plots[[length(plots) + 1]] <- p
        plot_names[[length(plot_names) + 1]] <- "Spectral Clustering"
      }
      
      
      
      # Validate that there is at least one plot
      validate(
        need(length(plots) > 0, "No plots were selected to download.")
      )
      
      
      # Save each plot as an HTML file
      plot_files <- lapply(1:length(plots), function(i) {
        filename <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(plots[[i]], filename)
        return(filename)
      })
      
      # Create HTML content with dropdown to select plots
      html_content <- "
<html>
<head>
  <script>
    function showPlot(plotId) {
      var plots = document.getElementsByClassName('plot');
      for (var i = 0; i < plots.length; i++) {
        plots[i].style.display = 'none';
      }
      document.getElementById(plotId).style.display = 'block';
    }
  </script>
</head>
<body>
  <select id='plotSelect' onchange='showPlot(this.value)'>"
      
      for (i in 1:length(plot_files)) {
        html_content <- paste0(html_content, "<option value='plot", i, "'>", plot_names[[i]], "</option>")
      }
      
      html_content <- paste0(html_content, "
  </select>")
      for (i in 1:length(plot_files)) {
        html_content <- paste0(html_content, "<div id='plot", i, "' class='plot'", if(i == 1) " style='display: block;'" else " style='display: none;'", "><iframe src='", plot_files[[i]], "' style='width: 100%; height: 100vh;'></iframe></div>"
        )
      }
      
      # Close HTML content
      html_content <- paste0(html_content, "
</body>
</html>
")
      
      # Write HTML content to file
      writeLines(html_content, file)
      
      
      # Combine plots into a single plot
      # if (length(plots) >= 2) {
      #   combined_plot <- plotly::subplot(plots[[1]], plots[[2]], nrows = 2, shareX = TRUE, shareY = TRUE)
      # } else {
      #   combined_plot <- plots[[1]]
      # }
      
      # plotly_built <- plotly::plotly_build(combined_plot)
      # htmlwidgets::saveWidget(plotly_built, file)
      
    }
  )
  
  #==============================================ANOVA PAIRWISE======================================================
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    # print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })
  
#   inputVal <-
#     InputValidator$new()
#   inputVal$add_rule("df_upload_file", sv_required(message = "Upload a file is required"))
#   inputVal$enable()
  
  ########################################
  # Data Viewer
  #
  # Before any manipulation, we should
  # have a glimpse on our data.
  #
  #
  # The data should have at least two
  # different treamtments or groups at
  # the first column
  #
  ########################################

  ########################################
  # 1. we check the data frame, at this
  #    step, we rename the first column as
  #    Treatment.
  #    This reactive function is called
  #    rct_df_data
  #    return: tibble
  #    render: DT
  ########################################
  
  rct_df_data <- reactive({
    tryCatch({
      df <- read_csv(input$csv_file$datapath)
      tr_name <- colnames(df)[1]
      rename(df, "Treatment" = all_of(tr_name)) %>%
        mutate(Treatment = as.factor(Treatment))
    }, error = function(e) {
      # Handle the error here
      if(grepl("Error in vroom::vroom:", e$message)) {
        return("Fail to do computation: You have selected the 'two columns' format, but your data is not in the 'Two columns' format. Please select the correct format and try again.")
      }
      # You might want to return NULL or some other value to indicate failure
      NULL
    })
  })

  ########################################
  # Render
  #    panel: main - DataViewer
  #    rct_df_data - DT
  ########################################

  output$df_com <-
    renderDT(rct_df_data())

  ########################################
  # Distribution Detector
  #
  # Before compare data across treatments
  # We should determine the distribution
  # of our data.
  #
  # If original data does not meet normal
  # distribution, what about transformed
  # data?
  ########################################

  ########################################
  # 0. check data column by column by
  #    Shapiro-Wilk Normality Test
  #    return p value
  #    This reactive function is called
  #    rct_df_sw_test_pv
  #    return: tibble
  #    render: DT
  ########################################

  rct_df_sw_test_pv <- reactive({
    rct_df_data()[, -1] %>%
      map_dfc(function(col_dat) {
        p_v <- shapiro.test(col_dat) %>%
          .[["p.value"]]
      })
  })

  ########################################
  # 1. detect the distribution by sw_pv
  #    This reactive function is called
  #    rct_df_data
  #    return: tibble
  #    render: DT
  ########################################

  rct_dist_detect <- reactive({
    rct_df_sw_test_pv() %>%
      map_dfc(~ if_else(.x >= as.numeric(input$sw_signif_level),
        "normal",
        "non-normal"
      ))
  })

  ########################################
  # 2. To determine wehter to use
  #    parametric test or nonparametric
  #    test.
  #
  #    Once a skewed distribution is
  #    found in each
  #    variable, the parametric test is
  #    not suitable any more.
  #    This section is seperated into
  #    two reactive functions.
  #    rct_condition prepare conditions
  #    rct_analysis_method generate tibble
  #    return: tibble
  #    render: DT
  ########################################
  rct_condition_ls <-
    reactive({
      # to determine the number of treatment
      # and whether the sample numbers of each treatment
      # is equal or not
      sum_df <-
        rct_df_data()$`Treatment` %>%
        table() %>%
        as.data.frame() %>%
        set_names("Variable", "Freq")

      list(
        Treatment_num = length(sum_df$Variable),
        is_equal = length(unique(sum_df$Freq)) == 1,
        to_levene = length(sum_df$Variable) == 2
      )
    })

  rct_analysis_method <- reactive({
    # determine the detailed test method
    map(rct_dist_detect(), function(x) {
      if (input$is_perm == "perm") {
        "Permutation test"
      } else {
        if (x == "normal") {
          if (rct_condition_ls()$Treatment_num == 2) {
            # Only the number of each group is equal can apply paired test
            if (rct_condition_ls()$is_equal && input$try_paired == "paired") {
              "Paired t test"
            } else {
              "t test"
            }
          } else {
            "one-way ANOVA"
          }
        } else {
          if (rct_condition_ls()$Treatment_num == 2) {
            if (rct_condition_ls()$is_equal && input$try_paired == "paired") {
              "Wilcoxon Signed rank test"
            } else {
              "Wilcoxon Rank Sum test"
            }
          } else {
            "Kruskal Wallis H test"
          }
        }
      }
    }) %>%
      as.data.frame() %>%
      as_tibble()
  })

  ########################################
  # 2.5. apply Levene’s test to check
  #    variance homogeneity
  #    This reactive function is called
  #    rct_levene_p
  #    return: tibble
  #    render: DT
  #    colnames: Levene's Test (p.value)
  ########################################

  rct_levene_p <- reactive({
    if (rct_condition_ls()$to_levene) {
      # rct_df_data[, -1] %>%
      rct_df_data()[, -1] %>%
        map_dfc(function(value_by_col) {
          # leveneTest(value_by_col, group = rct_df_data[, 1]) %>%
          leveneTest(value_by_col, group = rct_df_data()[, 1]) %>%
            tidy() %>%
            select(p.value)
        }) %>%
        # set_names(names(rct_analysis_method))
        set_names(names(rct_analysis_method()))
    }
  })

  ########################################
  # 3. we combine the tables of
  #    distribution and methods selection
  #    This reactive function is called
  #    rct_df_dist_n_method
  #    return: tibble
  #    render: DT
  #    colnames:
  ########################################
  rct_df_dist_n_method <- reactive({
    tb_sw_test_pv_long <-
      pivot_longer(rct_df_sw_test_pv(),
        everything(),
        names_to = "Varible",
        values_to = "p.value"
      ) %>%
      mutate(
        "p.value" =
          sprintf("%.4f", `p.value`)
      ) %>%
      rename(`Shapiro-Wilk (p.value)` = `p.value`)


    tb_dist_detect_long <-
      pivot_longer(rct_dist_detect(),
        everything(),
        names_to = "Varible",
        values_to = "Distribution"
      )


    tb_analysis_method <-
      pivot_longer(rct_analysis_method(),
        everything(),
        names_to = "Varible",
        values_to = "Method"
      )


    final_dist_n_method <-
      if (rct_condition_ls()$to_levene) {
        tb_levene_p <-
          pivot_longer(rct_levene_p(),
            everything(),
            names_to = "Varible",
            values_to = "Levene test (p.value)"
          ) %>%
          mutate(`Levene test (p.value)` = as.numeric(`Levene test (p.value)`))

        # print(tb_levene_p)

        list(
          tb_sw_test_pv_long,
          tb_dist_detect_long,
          tb_levene_p,
          tb_analysis_method
        ) %>%
          reduce(left_join, by = "Varible") %>%
          mutate(Method = if_else(Method == "t test",
            if_else(`Levene test (p.value)` <= .05,
              "t test (unequal variance)",
              "t test (equal variance)"
            ),
            Method
          ))
      } else {
        list(
          tb_sw_test_pv_long,
          tb_dist_detect_long,
          tb_analysis_method
        ) %>%
          reduce(left_join, by = "Varible")
      }
    # print(final_dist_n_method)

    final_dist_n_method
  })



  ########################################
  # 4. we plot the histgram of each
  #    variable.
  #    This reactive function is called
  #    rct_ggplot_hist
  #    return: ggobject
  #    render: plot
  #    structure:
  #       wrap: name (Variable)
  #       axes: both x and y are free
  ########################################

  rct_ggplot_hist <- reactive({
    rct_df_data()[, -1] %>%
      pivot_longer(everything()) %>%
      mutate(name = factor(name, levels = unique(name))) %>%
      ggplot(aes(value)) +
      geom_density(fill = "skyblue") +
      facet_wrap(vars(name),
        scales = "free"
      ) +
      theme_classic()
  })

  ########################################
  # 6. we plot the qqplot of each
  #    variable.
  #    This reactive function is called
  #    rct_ggplot_qq
  #    return: ggobject
  #    render: plot
  #    structure:
  #       wrap: name (Variable)
  #       axes: both x and y are free
  ########################################

  rct_ggplot_qq <- reactive({
    rct_df_data()[, -1] %>%
      pivot_longer(everything()) %>%
      mutate(name = factor(name, levels = unique(name))) %>%
      ggplot(aes(sample = value)) +
      stat_qq() +
      stat_qq_line() +
      facet_wrap(vars(name),
        scales = "free"
      ) +
      theme_classic()
  })

  ########################################
  # 7. Provide options to determine
  #    what kind of statics
  #    would you like manually
  #    This reactive function is called
  #    rct_var_select_method_determine
  #    return: uiOutput
  ########################################
  rct_select_method_determine <- reactive({
    map2(rct_df_dist_n_method()$Varible, rct_df_dist_n_method()$Method, function(var, method) {
      alternative_method <-
        if (method %in% c(
          "Wilcoxon Rank Sum test",
          "t test (unequal variance)",
          "t test (equal variance)"
        )) {
          c(
            "Wilcoxon Rank Sum test",
            "t test (unequal variance)",
            "t test (equal variance)"
          )
        } else if (method %in% c(
          "Kruskal Wallis H test",
          "one-way ANOVA"
        )) {
          c(
            "Kruskal Wallis H test",
            "one-way ANOVA"
          )
        } else if (method %in% c(
          "Wilcoxon Signed rank test",
          "Paired t test"
        )) {
          c(
            "Wilcoxon Signed rank test",
            "Paired t test"
          )
        } else {
          "Permutation test"
        }

      column(
        width = 4,
        selectInput(
          inputId = paste0("var_", var),
          label = paste("Method for", var),
          choices = alternative_method,
          selected = method
        )
      )
    })
  })

  ########################################
  # Render
  #    panel: main - Distribution Determine
  ###    rct_dist_detect - DT
  ###    rct_analysis_method - DT
  #    rct_df_dist_n_method - DT
  #    rct_ggplot_hist - plot
  #    rct_ggplot_qq - plot
  ########################################
  output$df_dist_n_method <-
    renderDT(rct_df_dist_n_method())

  output$method_determine_select <-
    renderUI(rct_select_method_determine())

  output$ggplot_hist <-
    renderPlot(rct_ggplot_hist())

  output$ggplot_qq <-
    renderPlot(rct_ggplot_qq())
  ########################################
  # comparison
  #
  # Time to compare data across treatments
  ########################################

  ########################################
  # 1. compare data across treatments,
  #    the comparison method was
  #    determined by rct_analysis_method.
  #    This reactive function is called
  #    rct_compare_ls
  #    return: tibble
  #    render: DT - with Buttons
  ########################################
  rct_compare_ls <- reactive({
    map2(rct_df_dist_n_method()$Varible, rct_df_data()[, -1], function(var_name, var_data) {
      # get input from dynamic uiOutput
      # I need to use a new trick to access the values the input values.
      # So far we’ve always accessed the components of input with $, e.g. input$col1.
      # But here we have the input names in a character vector,
      # like var <- "col1". $ no longer works in this scenario,
      # so we need to swich to [[, i.e. input[[var]].

      var_method <-
        input[[paste0("var_", var_name)]]

      switch(var_method,
        "Kruskal Wallis H test" =
        # the fallback method is kruskal.test
          kruskal.test(var_data, rct_df_data()$Treatment, na.action = na.omit) %>%
            broom::glance(),
            # mutate(df = as.character(df)),
        "one-way ANOVA" =
          aov(var_data ~ rct_df_data()$Treatment, na.action = na.omit) %>%
            broom::tidy() %>%
            filter(term != "Residuals") %>%
            mutate(
              method = "one-way ANOVA",
              df = as.character(df)
            )
        # TRUE ~ stop("Find an issue in `rct_compare_ls` section, please note chenhan28@gmail.com")
      )
    }) %>%
      bind_rows() %>%
      bind_cols(
        tibble(variable = colnames(rct_analysis_method())),
        .
      )
  })

      # Render the reactive output as a table
    # output$compareResults <- renderDataTable({
    #     rct_compare_ls()  # This calls the reactive function to get the data frame
    # })

    output$compareResults <- DT::renderDataTable({
        # Your data frame
        df <- rct_compare_ls()  # Assuming this is your reactive data frame
        # try to round all the numbers in this list to 4 decimal
        df <- df %>% mutate(across(where(is.numeric), round, digits = 4))

        # DT options
        datatable(df, options = list(
            columnDefs = list(list(width = '150%', targets = "_all")),
            pageLength = 10,     # Set number of rows per page
            scrollX = TRUE,      # Enable horizontal scrolling
            autoWidth = TRUE     # Auto-adjust column widths
        ))
    })

  

  ########################################
  # 2. compare data across treatments,
  #    the comparison method was
  #    determined by rct_analysis_method.
  #    This reactive function is called
  #    rct_compare_ls
  #    return: tibble
  #    render: DT - with Buttons
  ########################################

  rct_compare_table <- reactive({
    map2(colnames(rct_df_data()[, -1]), rct_df_data()[, -1], function(x, y) {
      df_tr_var <-
        bind_cols(
          Treatment =
            rct_df_data()[, 1],
          var = y
        )

      df_comp_ls <-
        df_tr_var %>%
        group_by(Treatment) %>%
        summarize(
          Mean = mean(var, na.rm = TRUE),
          SE = sd(var, na.rm = TRUE),
          Median = median(var, na.rm = TRUE),
          IQR = IQR(var, na.rm = TRUE)
        )

      ls_perm_res <-
        pairwisePermutationMatrix(var ~ Treatment,
          data = df_tr_var,
          method = input$p_adjust_method
        )

      df_pair_perm <-
        multcompLetters(
          ls_perm_res$Adjusted,
          # compare="<",
          threshold = 0.05,
          Letters = letters,
          reversed = FALSE
        )

      bind_cols(list(
        Variable = x,
        df_comp_ls,
        sig_level = df_pair_perm$Letters
      ))
    }) %>%
      bind_rows()
  })

  ########################################
  # 2. Post-Hoc Tests
  #    the comparison method was
  #    determined by rct_analysis_method.
  #    This reactive function is called
  #    rct_gg_post_hoc
  #    return: ggobject
  #    render: plot
  #
  #   Customized parameter
  #
  #       xlab = input$plot_x_lab,
  #       ylab = input$plot_y_lab,
  #       pairwise.display = input$pairwise_display,
  #       pairwise.annotation = input$pairwise_annotation,
  #       title.prefix = input$title_prefix,
  ########################################
rct_gg_post_hoc_interactive <- reactive({
  # Prepare the data
  rct_df_data_by_tr <- 
    rct_df_data() %>%
    pivot_longer(-Treatment, names_to = "name", values_to = "value") %>%
    mutate(name = factor(name, levels = colnames(rct_df_data())[-1]))

  # Calculate means
  means <- rct_df_data_by_tr %>%
    group_by(Treatment, name) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')

  # Create the plotly violin plot
  p <- plot_ly(data = rct_df_data_by_tr, x = ~Treatment, y = ~value, type = 'violin', split = ~name) %>%
      add_trace(data = means, x = ~Treatment, y = ~mean_value, type = 'scatter', mode = 'markers', 
                marker = list(size = 10, color = 'red'), showlegend = FALSE) %>%
      layout(xaxis = list(title = input$plot_x_lab), yaxis = list(title = input$plot_y_lab))

# Maximum Y value for positioning
  y_max <- max(rct_df_data_by_tr$value, na.rm = TRUE)
  y_increment <- (max(rct_df_data_by_tr$value, na.rm = TRUE) - min(rct_df_data_by_tr$value, na.rm = TRUE)) / 20
  y_position_for_annotations <- y_max + 3 * y_increment # Adjust the multiplier as needed

  annotation_font <- list(family = "Arial, sans-serif", size = 14, color = "red", weight = "bold")

  # Add annotations horizontally
  annotations <- list(
    list(x = 0.1, y = y_position_for_annotations, text = paste("F Welch:", f_wetch_global), showarrow = FALSE, xref='paper', yref='y', font=annotation_font),
    list(x = 0.3, y = y_position_for_annotations, text = paste("P-value:", p_value_global), showarrow = FALSE, xref='paper', yref='y', font=annotation_font),
    list(x = 0.5, y = y_position_for_annotations, text = paste("Omega:", omega_global), showarrow = FALSE, xref='paper', yref='y', font=annotation_font),
    list(x = 0.7, y = y_position_for_annotations, text = paste("OBS:", obs_global), showarrow = FALSE, xref='paper', yref='y', font=annotation_font),
    list(x = 0.9, y = y_position_for_annotations, text = paste("CI(95%):[", CI_first_global, ", ", CI_second_global,"]"), showarrow = FALSE, xref='paper', yref='y', font=annotation_font)
  )

  # Create the plot with annotations
  p <- plot_ly(data = rct_df_data_by_tr, x = ~Treatment, y = ~value, type = 'violin', split = ~name) %>%
    add_trace(data = means, x = ~Treatment, y = ~mean_value, type = 'scatter', mode = 'markers', 
              marker = list(size = 10, color = 'red'), showlegend = FALSE) %>%
    layout(xaxis = list(title = input$plot_x_lab), yaxis = list(title = input$plot_y_lab),
           annotations = annotations, title = "Pairwise test: Games-Howell Bars shown: significant")

  # Return the interactive plotly plot
  return(p)

})

    observeEvent(input$plot_figure_interactive, {
            output$gg_post_hoc_interactive <- renderPlotly({
                rct_gg_post_hoc_interactive()
            })
            #title of the data grid
            output$dataTableTitle <- renderUI({
                # Check if the action button has been clicked
                if(input$plot_figure_interactive > 0){
                    h3("Pairwise Comparisons of P-value", style = "text-align: center;")
                }
            })
            # Render the Data Table
            output$pairwiseTable <- DT::renderDataTable({
                # Create a data frame from your global variables
                pairwise_df <- combine_to_df(Group1_global, Group2_global, p_values_grid_global)
                # print(pairwise_df)
                DT::datatable(pairwise_df, options = list(pageLength = 5, caption = 'Pairwise Comparisons of p-values'))
            })
    })

    combine_to_df <- function(group1, group2, p_values) {
        data.frame(Group1 = group1, Group2 = group2, P_Value = p_values)
    }

  rct_gg_post_hoc <-
    reactive({{
    ########################################
    # Tame data by Treatment,
    # It will be used in rct_df_data_by_tr
    # (distribution plot) and
    # rct_gg_post_hoc (post hoc tests).
    # therefore, we wrap it up as reactive
    # function
    ########################################
    rct_df_data_by_tr <- 
      rct_df_data() %>%
        pivot_longer(-Treatment) %>%
        ########################################
        # To make the orders of figure follow
        # the input table.
        # We convert the variable name into
        # factor and control the order of plot
        # by the level
        ########################################
        dplyr::mutate(
          name =
            factor(name,
              levels = colnames(rct_df_data()) %>%
                .[-1]
            )
        )

    post_hoc_data <-
      rct_df_data_by_tr %>%
      nest(-name) %>%
      mutate(data = map2(name, data, function(x, y) {
        bind_cols(name = x, y)
      }))

    method_by_select <-
      rct_df_dist_n_method()$Varible %>%
      map(~ input[[paste0("var_", .x)]])
    
    # get input from dynamic uiOutput
    # I need to use a new trick to access the values the input values.
    # So far we’ve always accessed the components of input with $, e.g. input$col1.
    # But here we have the input names in a character vector,
    # like var <- "col1". $ no longer works in this scenario,
    # so we need to swich to [[, i.e. input[[var]].

    list(
      method_by_select,
      post_hoc_data$data,
      post_hoc_data$name
    ) %>%
      pmap(function(x, y, z) {
        withProgress(expr = {
          setProgress(message = "Calculation in progress",
                      detail = "This may take a while...",
                      value = 1)
          if (x %in%
              c("Wilcoxon Signed rank test",
                "Wilcoxon Rank Sum test",
                "Kruskal Wallis H test")) {
            analysis_method <- "nonparametric"
          } else if (x == "t test (equal variance)") {
            # t test (equal variance) should be considered
            # a special name
            analysis_method <- "parametric_variance_equal"
          } else {
            # In ggstatsplot, parametric test will performed by
            # games howell post-hoc test
            # This is similar to tukey-karmer test
            analysis_method <- "parametric"
          }
          
          showtext_auto()
          
          mpc_df <- pairwise_comparisons(
            data            = y,
            x               = Treatment,
            y               = value,
            type = if_else(
              analysis_method == "parametric_variance_equal",
              "parametric",
              analysis_method
            )
          )
        #   print(mpc_df$group1)
        #   print(mpc_df$group2)
        #   print(mpc_df$p.value)
          Group1_global <<- mpc_df$group1
          Group2_global <<- mpc_df$group2
          p_values_rounded <- round(mpc_df$p.value, 4)
          p_values_grid_global <<- p_values_rounded

          test_results <-ggbetweenstats(
            data = y,
            x = Treatment,
            y = value,
            grouping.var = name,
            type = if_else(
              analysis_method == "parametric_variance_equal",
              "parametric",
              analysis_method
            ),
            nboot = 10,
            plot.type = "box",
            pairwise.comparisons = TRUE,
            # pairwise comparison
            results.subtitle = identical(input$show_statis, "show"),
            # F welch result
            var.equal = identical(analysis_method, "parametric_variance_equal"),
            mean.plotting = FALSE,
            sample.size.label = FALSE,
            ggtheme = theme_classic(),
            ########################################
            # Customized parameter
            ########################################
            xlab = input$plot_x_lab,
            ylab = z,
            pairwise.display = input$pairwise_display,
            # Outdate and unavaible, Remove
            p.adjust.method = input$p_adjust_method,
            # ggplot theme
            ggplot.component = theme(text = element_text(family = "wqy-microhei")),
            output = "object"
          )
            # Print the test results for annotation
            # print(str(test_results$labels$subtitle))
            # Your string
            result_string <- test_results$labels$subtitle
            # print(str(result_string))
            # Convert language objects to strings
            result_list_strings <- lapply(result_string, function(x) deparse(x))

            # print(result_list_strings[[5]])
            # print(result_list_strings[[6]])
            # Regular expression to extract the number after '==' for F(wetch)
            f_wetch <- sub('.*== "\\s*([^"]+)"', '\\1', result_list_strings[[2]])
            p_value <- sub('.*== "\\s*([^"]+)"', '\\1', result_list_strings[[3]])
            omega <- sub('.*== "\\s*([^"]+)"', '\\1', result_list_strings[[4]])
            obs <- sub('.*== "\\s*([^"]+)"', '\\1', result_list_strings[[7]])
            CI_first <- sub('.*\\* "\\s*([^"]+)"', '\\1', result_list_strings[[5]])
            CI_second <- sub('^"([0-9.]+)".*$', '\\1', result_list_strings[[6]])
            # print(CI_first)
            # print(CI_second)
            # print(f_wetch)
            # print(p_value)
            # print(omega)
            # print(obs)
            f_wetch_global <<- f_wetch
            p_value_global <<- p_value
            omega_global <<- omega
            obs_global <<- obs
            CI_first_global <<- CI_first
            CI_second_global <<- CI_second
            test_results
        })
      }) %>%
      plot_grid(
        plotlist = .,
        ncol = input$figure_ncol,
        align = "h",
        labels = input$cow_lab
      ) }})
  
  event_rct_gg_post_hoc <- eventReactive(
    input$plot_figure,
    rct_gg_post_hoc()
  )

  output$compare_ls <-
    renderDT({
      rct_compare_ls()
    })
  output$dl_compare_ls <-
    downloadHandler(
      filename = "compare_list.csv",
      content = function(file) {
        write_csv(
          x = rct_compare_ls(),
          path = file
        )
      }
    )

  output$compare_table <- renderDT({
  df <- rct_compare_table()  # Get your data frame
  rounded_df <- round_df(df)  # Apply rounding
  datatable(rounded_df, options = list(pageLength = 10, autoWidth = TRUE))
})

    round_df <- function(df, digits = 4) {
        df %>% mutate(across(where(is.numeric), ~round(., digits)))
    }
  output$dl_compare_table <-
    downloadHandler(
      filename = "compare_table.csv",
      content = function(file) {
        write_csv(
          x = rct_compare_table(),
          file = file
        )
      }
    )

  output$gg_post_hoc <-
    renderPlot({
      showtext_auto()
      event_rct_gg_post_hoc()
    })
  ########################################
  # 3. save and download figure as PDF
  #    This downloadhandler is called
  #    dl_gg
  ########################################
  output$dl_gg <-
    downloadHandler(
      filename = "post_hoc_figure.pdf",
      content = function(file) {
        showtext_auto()
        ggsave(
          file,
          plot = rct_gg_post_hoc(),
          width = input$figure_width,
          height = input$figure_height
        )
      }
    )
#==============================================ANOVA PAIRWISE======================================================

}
)

shinyApp(ui = ui, server = server)
