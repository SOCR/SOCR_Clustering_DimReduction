library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)

library(markdown)
library(PMCMRplus)
#
# Package for Shiny
library(shinythemes)
library(DT)
library(shinydisconnect)
library(shiny.i18n)
library(shinyvalidate)

# Package for data manipulation
library(tidyr)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(broom)
library(cowplot)

# Package for tests

### Levene’s test
library(car)

### Permutation test
library(coin)

### Multiple Compare
library(rcompanion)
library(multcompView)

### Plot Multiple Compare
library(ggstatsplot)

# Fix font of CJK
library(showtext)

ui <- shinyUI(fluidPage(
  
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
        menuItem("Decision Tree", tabName= "DT",icon=icon("users",lib="font-awesome")),
        menuItem("Hierarchal Clustering", tabName= "HC",icon=icon("users",lib="font-awesome")),
        menuItem("Poisson Clustering", tabName= "PC",icon=icon("users",lib="font-awesome")),
        menuItem("1-way ANOVA Computation", tabName="AC", icon=icon("users", lib="font-awesome")),
        menuItem("ANOVA Pairwise Comparison", tabName="APC", icon=icon("users", lib="font-awesome")),
        menuItem("Clustered Results", tabName = "Cluster_Result",icon=icon("table")),
        # menuItem("Clustered Results in rmd", tabName = "Cluster_Result_rmd", icon=icon("table")),
        menuItem("Help", tabName = "Help",icon=icon("cog"))
      )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "upload", h1("Upload your csv files"),fileInput("csv_file", "Choose CSV File:",
                                                                          multiple = FALSE,
                                                                          accept = c(".csv"))),
        tabItem(tabName = "raw",h1("Raw Data"), fluidRow(column(5,tableOutput("rawdata")))),
        tabItem(tabName = "kmeans",h1("K-means clustering"),
                fluidRow(
                  box(selectInput("kmeans_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect_Kmeans", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_Kmeans", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.kmeans_plot == '3D'", box(width = 3,selectInput("inSelect3_Kmeans", "Select input", c("Item A", "Item B", "Item C")))),
                  box(sliderInput("clustnum_kmeans","Number of clusters",2,8,6)),
                  box(width = 12,plotly::plotlyOutput("kmeans_clusterchart", width="100%",height = 750)),
                  box(downloadButton('KM_download_result',"Download the prediction result"),width = 3),
                  box(downloadButton('downloadPlot_KM',"Download the prediction result"),width = 3)
                )),
        tabItem(tabName = "Guassin_Mixture", h1("Gaussian Mixture clustering"),
                fluidRow(
                  box(selectInput("GMM_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.GMM_plot == '3D'", box(width = 3,selectInput("inSelect3", "Select input", c("Item A", "Item B", "Item C")))),
                  box(sliderInput("clustnum_GMM","Number of clusters",2,8,6)),
                  box(width = 12,plotly::plotlyOutput("Gaussian_clusterchart",width="100%",height = 750)),
                  box(downloadButton('GS_download_result',"Download the prediction result"),width = 3),
                  box(downloadButton('downloadPlot_GS', 'Download the prediction result in HTML'), width = 3)
                )),
        tabItem(tabName = "Spectral", h1("Spectral clustering"),
                fluidRow(
                  box(selectInput("Spectral_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect_Spectral", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_Spectral", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.Spectral_plot == '3D'", box(width = 3,selectInput("inSelect3_Spectral", "Select input", c("Item A", "Item B", "Item C")))),
                  box(sliderInput("clustnum_spec","Number of clusters",2,8,6)),
                  box(width = 12,plotly::plotlyOutput("clusterchart_spectral",width="100%",height = 750)),
                  box(downloadButton('Spec_download_result',"Download the prediction result"),width = 3),
                  box(downloadButton('downloadPlot_SP', 'Download the prediction result in HTML'), width = 3)
                )),
        tabItem(tabName = "cmeans", h1("Fuzzy C-means clustering"),
                fluidRow(
                  box(selectInput("cmeans_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect_Cmeans", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_Cmeans", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.cmeans_plot == '3D'", box(width = 3,selectInput("inSelect3_Cmeans", "Select input", c("Item A", "Item B", "Item C")))),
                  box(sliderInput("clustnum_cmeans","Number of clusters",2,8,6)),
                  box(width = 12,plotly::plotlyOutput("clusterchart_cmeans",width="100%",height = 750)),
                  box(downloadButton('FC_download_result',"Download the prediction result"),width = 3),
                  box(downloadButton('downloadPlot_FC',"Download the prediction result"),width = 3)
                )),
        tabItem(tabName = "DT", h1("Decision Tree supervised learning"),
                fluidRow(
                  box(selectInput("DT_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect_DT", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_DT", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.DT_plot == '3D'", box(width = 3,selectInput("inSelect3_DT","Select input",c("Item A", "Item B", "Item C")))),
                  box(selectInput("inSelect_label_DT", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(sliderInput("test_ratio_DT","Train set ratio",min = 0, max = 40, post  = " %", value = 20)),
                ),
                fluidRow((
                  splitLayout(cellWidths = c("50%", "50%"), plotly::plotlyOutput("clusterchart_DT",width="100%",height = 750), plotly::plotlyOutput("clusterchart_DT_correct",width="100%",height = 750))
                )),
                fluidRow(
                  box(fileInput("test_file", "Upload test set(same column names required):",
                                multiple = FALSE,
                                accept = c(".csv")), width = 7),
                  box(width = 12,plotly::plotlyOutput("test_plot_DT",width="100%",height = 750)),
                  box(downloadButton('DT_download_result',"Download the prediction result"),width = 3)
                )),
        tabItem(tabName = "HC",h1("Hierarchal clustering"),
                fluidRow(
                  box(selectInput("HC_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect_HC", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_HC", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.HC_plot == '3D'", box(width = 3,selectInput("inSelect3_HC", "Select input", c("Item A", "Item B", "Item C")))),
                  box(sliderInput("clustnum_HC","Number of clusters",2,8,6)),
                  box(width = 12,plotly::plotlyOutput("clusterchart_HC", width="100%",height = 750)),
                  box(downloadButton('HC_downloadResult',"Download the prediction result"),width = 3),
                  box(downloadButton("downloadPlot_HC", "Download Plot in HTML file"), width = 3)
                )),
        tabItem(tabName = "PC",h1("Poisson clustering"),
                fluidRow(
                  box(selectInput("PC_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect_PC", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_PC", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  conditionalPanel(condition = "input.PC_plot == '3D'", box(width = 3,selectInput("inSelect3_PC", "Select input", c("Item A", "Item B", "Item C")))),
                  box(sliderInput("clustnum_PC","Number of clusters",2,8,6)),
                  box(width = 12,plotly::plotlyOutput("clusterchart_PC", width="100%",height = 750)),
                  box(downloadButton('PC_downloadResult',"Download the prediction result"),width = 3),
                  box(downloadButton('downloadPlot_PC', "Download the prediction result"), width = 3)
                )),
        tabItem(tabName = "BC",h1("Binomial clustering"),
                #h3("If using Binomial clustering, please name your inSelect0_BC and 
               # your inSelect_BC to 'No.Success' and 'No.Failure'."),
                fluidRow(
                  box(selectInput("BC_plot", "Select Plot type:", choices=c("2D","3D")),width = 2),
                  box(selectInput("inSelect0_BC", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect_BC", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect2_BC", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  box(selectInput("inSelect4_BC", "Select input", c("Item A", "Item B", "Item C")), width = 3),
                  
                  conditionalPanel(condition = "input.BC_plot == '3D'", box(width = 3,selectInput("inSelect3_BC", "Select input", c("Item A", "Item B", "Item C")))),
                  box(sliderInput("clustnum_BC","Number of clusters",2,8,6)),
                  box(width = 12,plotly::plotlyOutput("clusterchart_BC", width="100%",height = 750)),
                  box(downloadButton('BC_downloadResult',"Download the prediction result"),width = 3),
                  box(downloadButton('downloadPlot_BC', "Download the prediction result"), width = 3)
                )),
        tabItem(tabName = "Cluster_Result", h1("Clustered Data Result - Please run the clustering first and then download"),
                checkboxGroupInput("model_selection", "Select Models", 
                                   choices = c("K-means", "Gaussian Mixture", "Spectral", "Fuzzy C-Means", "Decision Tree", "Hierarchical Clustering", "Poisson Clustering"),
                                   selected = c("K-means")
                ),
                box(downloadButton('download_combined_plots', "Download Combined Plots"), width=5)
        ),
        tabItem(tabName = "Cluster_Result_rmd", h1("Clustered Data Result in rmd"),
                checkboxGroupInput("model_selection", "Select Models", 
                                   choices = c("K-means", "Gaussian Mixture", "Spectral", "Fuzzy C-Means", "Decision Tree", "Hierarchical Clustering", "Poisson Clustering"),
                                   selected = c("K-means")
                ),
                box(downloadButton('download_combined_plots_rmd', "Download Combined Plots in rmd"), width=6)
        ),
        tabItem(tabName = "AC",
            h1("1-way ANOVA Computation"),
            HTML("Please begin by specifying the format of your dataset. You have the option to upload your data in one of the following formats:
          <ol>
            <li>A two-column format, where the first column contains the data values and the second column contains the corresponding group labels ('data', 'group').</li>
            <li>A multi-column format, where each column represents a different group, and the rows contain the data values for each group ('group1', 'group2', 'group3', ...).</li>
          </ol>
          Select the format that best matches your dataset to proceed."),
            selectInput("data_format", "Data format", c("Two columns", "Multiple columns")),
            conditionalPanel(
                condition = "input.data_format == 'Two columns'",
                p("For data with two columns, one for the data and one for the group."),
                selectInput("data_column", "Select data column", ""),
                selectInput("group_column", "Select group column", "")
            ),
            tableOutput("anova_results")
        ),
        tabItem(tabName = "APC",
                    fluidPage(
        # shiny.i18n::usei18n(i18n),
        disconnectMessage2(),
        tags$head(tags$style(HTML(
            "
                h3 {
                    font-weight:bold
                }
                                "
        ))),
        # Application title
        theme = shinytheme("flatly"),
        navbarPage(
            "SOCR Anova Pairwise Comparison",
            tabPanel(
            title = t("Analysis"),
                # Show a plot of the generated distribution
                mainPanel(tabsetPanel(
                tabPanel(
                    title =
                    t("Data Viewer"),
                    h4(t("This section required the two columns data format, the first column is the assigned group, and the second
                    column is the corresponding value. Please double check your data format if you see nothing")),
                    DTOutput("df_com", width="150%")
                ),
                tabPanel(
                    t("Comparisons"),
                    conditionalPanel(
                    condition = "input.data_source == 'file'",
                    fileInput(
                    inputId = "df_upload_file",
                    label = NULL,
                    accept = ".csv"
                    )
                ),
                h4(t("Significant level of Shapiro-Wilk test")),
                textInput(
                    "sw_signif_level",
                    t("Set threshold of Shapiro-Wilk test"),
                    value = "0.05"
                ),
                h4(t("One and two sample tests")),
                selectInput(
                    inputId = "try_paired",
                    label = t("Whether you want a paired t-test/Wilcoxon Signed-Rank test?"),
                    choices = c(
                    "2-Sample/Unpaired",
                    "1-Sample/Paired" = "paired"
                    ),
                    selected = "2-Sample/Unpaired"
                ),
                helpText(t("Applicable for cases which each group has same number of observation.")),
                h4(t("Significance test between groups")),
                selectInput(
                    inputId = "is_perm",
                    label = t("Whether you want a permutation test?"),
                    choices = c("No",
                    "Monte Carlo Permutation Tests" = "perm"
                    ),
                    selected = "No"
                ),
                helpText(t("The Monte Carlo Permutation Tests may be involved in the unknown distribution or small sample size.")),
                h4(t("Post-Hoc Test")),
                selectInput(
                    inputId = "p_adjust_method",
                    label = t("Adjustment method for p-value for multiple comparisons."),
                    choices = p.adjust.methods,
                    selected = ifelse(
                    length(grep("bonferroni", p.adjust.methods)) == 0,
                    p.adjust.methods[[1]],
                    "bonferroni"
                    )
                ),
                helpText(
                    t("The details of adjustment method fo p-value for multiple comparisons"),
                    a(
                    href = "https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/p.adjust",
                    t("can be found here.")
                    ),
                    t("You can also found them from the vignette of"),
                    code("stat::p.adjust")
                ),
                    h3(t("Select statistic methods")),
                    fluidRow(column(width = 12, uiOutput("method_determine_select"))),
                    helpText(t("Select statistic methods automatically is not always suitable for every case. Histgram and Q-Q plot were also helpful for method selection.")),
                    h3(t("Significance test between groups")),
                    # dataTableOutput("compareResults"),
                    DT::dataTableOutput("compareResults", width = "150%"),
                    # downloadButton(
                    # "dl_compare_ls",
                    # t("Download")
                    # ),
                    # DTOutput("compare_ls"),
                    helpText(t("When p-value >= 0.05, there is no statistical significance between these groups, accepting null hypothesis.")),
                    helpText(t("When p-value < 0.05, there is the statistical significance between groups, accepting alternative hypothesis. If you want to find the significant differences between groups, please continue with Post-Hoc Test in the side bar.")),
                    ########################################
                    # Why don't use the DT tool button?
                    # Becasue the DT solve data table works
                    # on the server rather than local by
                    # default.
                    #
                    # We can use the server = FALSE to load
                    # all the data to local, but once the
                    # data is a super large table, or
                    # your device is not high performance,
                    # it will frozen the browser or cause
                    # some crash.
                    #
                    # So, we seperate the DT and Download
                    # to make things work well.
                    ########################################
                    h3(t("Data Summary")),
                    downloadButton(
                    "dl_compare_table",
                    t("Download")
                    ),
                    DTOutput("compare_table", width = "150%"),
                    helpText(t("For groups/treatments with same significant level (sig_level above), it means there is no significance has been observed between these groups.")),
                    h3(t("Post-Hoc Test (Please firstly click \"Plot Start\" and then click \"Interactive Plot Start\")")),
                    fluidRow(
                    column(
                        3,
                        textInput(
                        inputId = "plot_x_lab",
                        label = t("Label of X axis"),
                        placeholder = "Treatment"
                        )
                    ),
                    column(
                        3,
                        selectInput(
                        inputId = "cow_lab",
                        label = t("Label of each plot"),
                        choices = c(
                            "UPPER CASE" = "AUTO",
                            "lower case" = "auto"
                        ),
                        selected = "auto"
                        ),
                    )
                    ),
                    fluidRow(
                    column(
                        3,
                        selectInput(
                        inputId = "show_statis",
                        label = t("Show the parameters of statistic?"),
                        choices = c(
                            "Show" = "show",
                            "Hide" = "hide"
                        ),
                        selected = "Show"
                        )
                    ),
                    column(
                        3,
                        selectInput(
                        inputId = "pairwise_display",
                        label = t("Which pairwise comparisons to display?"),
                        choices = c(
                            "Significant" = "significant",
                            "All" = "all",
                            "Non-significant" = "non-significant"
                        ),
                        selected = "significant"
                        )
                    ),
                    # Outdate and unavaible, Remove
                    # column(
                    #   3,
                    #   selectInput(
                    #     'pairwise_annotation',
                    #     'Annotations to use for pairwise comparisons',
                    #     choices = c("p.value",
                    #                 "asterisk"),
                    #     selected = 'asterisk'
                    #   )
                    # )
                    ),
                    fluidRow(
                    column(
                        3,
                        numericInput(
                        inputId = "figure_ncol",
                        label = t("Figure numbers per row (up to 10)"),
                        min = 1,
                        max = 10,
                        value = 3
                        )
                    ),
                    column(
                        3,
                        numericInput(
                        inputId = "figure_width",
                        label = t("Downloaded figure width (inch)"),
                        min = 3,
                        max = 20,
                        value = 12
                        )
                    ),
                    column(
                        3,
                        numericInput(
                        inputId = "figure_height",
                        label = t("Downloaded figure height (inch)"),
                        min = 3,
                        max = 20,
                        value = 10
                        )
                    )
                    ),
                    p(
                    "Once setting parameter up, click",
                    strong("Plot Start"),
                    "to plot figure or click",
                    strong("Download Figure"),
                    "to download it directly."
                    ),
                    helpText(
                    t("P.S. showing figure on this page and downloading figure implement in different code, even the figure in browser lookes massy or wired, the downloaded figure still can meet the standar of publication level with appropriate parameter sets.")
                    ),
                    actionButton(
                    "plot_figure",
                    label = "Plot Start",
                    icon = icon("paint-roller")
                    ),
                    downloadButton("dl_gg", label = "Download Figure"),
                    plotOutput("gg_post_hoc", width = "150%"),
                    actionButton(
                        "plot_figure_interactive",
                        label = "Interactive Plot Start",
                        icon = icon("paint-roller")
                    ),
                    # Space to render the plot
                    plotly::plotlyOutput("gg_post_hoc_interactive", width = "150%"),
                    uiOutput("dataTableTitle"),
                    DTOutput("pairwiseTable", width = "150%")
                )
                ))
            
            ),
            tabPanel(
            title = t("References"),
            includeMarkdown("resource/page/acknowledgements.md")
            )
        )
        )
        ),
        tabItem(tabName = "Help", h1("Help"), 
                h2("This interactive SOCR RShiny app demonstrates 2D and 3D data clustering. It facilitates data import, parameter setting, and dynamic visualization of multiple clustering algorithms including k-means, spectral, Gaussian mixture modeling, etc. The app allows users to upload their own data (CSV format) for just-in-time visualization.",style="font-size:25px;"),h2("The app expects the data to be in CSV file format where the first row includes the names of all data elements (variables or features) and the rows contain the samples or cases. The user may select distinct features to include in the data clustering. One default dataset is provided as an example to illustrate the app functionality and users can select and load another dataset from a local CSV file.",style="font-size:25px;"),
                p("Developers: Yanghe Liu (yanghel@umich.edu) Shihang Li (shihangl@umich.edu) Yongxiang Zhao (zyxleo@umich.edu) Ding (Eric) Ding (ericding@umich.edu) Simeone Marino (simeonem@umich.edu)."),
                # p("the SOCR Team (https://www.socr.umich.edu/people/)."),
                shiny::a(h4("SOCR Team", class = "btn btn-default action-button" , 
                            style = "fontweight:600"), target = "_blank",
                         href = paste0("https://www.socr.umich.edu/people", "/")),
                style="font-size:20px;color:blue;")
      )),
    
  )) 
)
