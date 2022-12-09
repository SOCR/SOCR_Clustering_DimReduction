library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)

ui <- shinyUI(
  
  dashboardPage(
    dashboardHeader(title ="SOCR Clustering Calculator"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload Data",tabName = "upload",icon=icon("table")),
        menuItem("Raw Data",tabName = "raw",icon=icon("table")),
        menuItem("K-means clustering",tabName= "kmeans",icon=icon("users",lib="font-awesome")),
        menuItem("Gaussian Mixture clustering",tabName= "Guassin_Mixture",icon=icon("users",lib="font-awesome")),
        menuItem("Spectral clustering",tabName= "Spectral",icon=icon("users",lib="font-awesome")),
        menuItem("Fuzzy C-Means", tabName= "cmeans",icon=icon("users",lib="font-awesome")),
        menuItem("Clustered Results", tabName = "Cluster_Result",icon=icon("table")),
        menuItem("Help", tabName = "Help",icon=icon("cog"))
      )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "upload", h1("Upload your csv files"),fileInput("csv_file", "Choose CSV File:",
                                                                          multiple = FALSE,
                                                                          accept = c(".csv"))),
        tabItem(tabName = "raw",h1("Raw Data"),fluidRow(column(5,tableOutput("rawdata")))),
        tabItem(tabName = "kmeans",h1("K-means clustering"),
                fluidRow(
                  box(selectInput("kmeans_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect_Kmeans", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_Kmeans", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.kmeans_plot == '3D'", box(width = 3,selectInput("inSelect3_Kmeans", "Select input", c("Item A", "Item B", "Item C")))),
                  box(width = 12,plotly::plotlyOutput("kmeans_clusterchart", width="100%",height = 750)),
                  box(sliderInput("clustnum_kmeans","Number of clusters",2,8,6))
                )),
        tabItem(tabName = "Guassin_Mixture", h1("Gaussian Mixture clustering"),
                fluidRow(
                  box(selectInput("GMM_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.GMM_plot == '3D'", box(width = 3,selectInput("inSelect3", "Select input", c("Item A", "Item B", "Item C")))),
                  box(width = 12,plotly::plotlyOutput("Gaussian_clusterchart",width="100%",height = 750)),
                  box(sliderInput("clustnum_GMM","Number of clusters",2,8,6))
                )),
        tabItem(tabName = "Spectral", h1("Spectral clustering"),
                fluidRow(
                  box(selectInput("Spectral_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect_Spectral", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_Spectral", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.Spectral_plot == '3D'", box(width = 3,selectInput("inSelect3_Spectral", "Select input", c("Item A", "Item B", "Item C")))),
                  box(width = 12,plotly::plotlyOutput("clusterchart_spectral",width="100%",height = 750)),
                  box(sliderInput("clustnum_spec","Number of clusters",2,8,6))
                )),
        tabItem(tabName = "cmeans", h1("Fuzzy C-means clustering"),
                fluidRow(
                  box(selectInput("cmeans_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect_Cmeans", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_Cmeans", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.cmeans_plot == '3D'", box(width = 3,selectInput("inSelect3_Cmeans", "Select input", c("Item A", "Item B", "Item C")))),
                  box(width = 12,plotly::plotlyOutput("clusterchart_cmeans",width="100%",height = 750)),
                  box(sliderInput("clustnum_cmeans","Number of clusters",2,8,6))
                )),
        tabItem(tabName = "Cluster_Result", h1("Clustered Data Result"),
                downloadButton('download',"Download the data"),
                fluidRow(column(5,tableOutput("clustered_data")))),
        tabItem(tabName = "Help", h1("Help"), 
                h2("This interactive SOCR RShiny app demonstrates 2D and 3D data clustering. It facilitates data import, parameter setting, and dynamic visualization of multiple clustering algorithms including k-means, spectral, Gaussian mixture modeling, etc. The app allows users to upload their own data (CSV format) for just-in-time visualization.",style="font-size:25px;"),h2("The app expects the data to be in CSV file format where the first row includes the names of all data elements (variables or features) and the rows contain the samples or cases. The user may select distinct features to include in the data clustering. One default dataset is provided as an example to illustrate the app functionality and users can select and load another dataset from a local CSV file.",style="font-size:25px;"),
                p("Developers: Shihang Li (shihangl@umich.edu) Yongxiang Zhao (zyxleo@umich.edu) Simeone Marino (simeonem@umich.edu)."),
                # p("the SOCR Team (https://www.socr.umich.edu/people/)."),
                shiny::a(h4("SOCR Team", class = "btn btn-default action-button" , 
                            style = "fontweight:600"), target = "_blank",
                         href = paste0("https://www.socr.umich.edu/people", "/")),
                style="font-size:20px;color:blue;")
      ),
      tags$footer(
        div(shinyUI(bootstrapPage(div(
          includeHTML("SOCR_cluster_tracker_html")
        )))),)),
    
    )) 