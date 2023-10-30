##### User Interface script #####

# Load packages ----------------------------------------------------------------
setwd("C:/Users/catdu/OneDrive/DTU/11. semester/SpecialCourse")
library(shiny)
library(DT)
library(ggplot2)
library(shinyFeedback)
library(bslib)
library(tools)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)
library(ggpubr)
library(gridExtra)
library(data.table)
library(shinyBS)
library(hrbrthemes) # ipsum theme for boxplot
library(stringr)
library(xfun)
# library(RColorBrewer)
library(ggridges)
library(class)
library(fresh)
library(plotly)
library(MASS)
library(readr)
set.seed(1234)


# Load data --------------------------------------------------------------------
hungarian <- readRDS("HungarianData1.rds")
switzerland <- readRDS("SwitzerlandData1.rds")
longBeach <- readRDS("LongBeachData1.rds")
combData <- readRDS("SwitzHungLongBeachData1.rds")
largeCombData <- readRDS("mainData1.rds")
# vars = setdiff(names(largeCombData), "Species")


# Styling ----------------------------------------------------------------------
mainStyling <- "font-size:15px; font-family:'sans-serif'; "
sideBarPanelStyling <- "background:#F4F4F4; left:10; top:50; width:200px; position:fixed; border:2px solid #112446; border-color:#112446; background-color:#EDF2FA"
downloadStyling <- "display:inline-block; width:100%; "
theme <- create_theme(theme = "default",
                     bs_vars_navbar(padding_horizontal = "0px",
                                    default_bg = "#5B779A",                # background
                                    default_link_hover_bg = "#3F536B",     # hover color for tabs
                                    default_link_color = "#112446",        # navbar text color
                                    default_link_active_color = "#112446", # ?
                                    default_link_hover_color = "#2974CE",  # ?
                                    default_border = "#2974CE"), 
                     bs_vars_button(font_weight = 500,
                                    border_radius_base = 1,
                                    default_border = "#07AB3B",
                                    primary_color = "#FFF",
                                    primary_bg = "#07AB3B",
                                    primary_border = "#112446"),
                     bs_vars_tabs(border_color = "#3F536B",
                                  active_link_hover_bg = "#FFF",
                                  active_link_hover_color = "#112446",
                                  active_link_hover_border_color = "#5B779A",
                                  link_hover_border_color = "#5B779A"),
                     output_file = NULL)




# UI start ---------------------------------------------------------------------
ui <- tagList(
  
  tags$head(tags$style(HTML('* {font-family:"sans-serif"; font-size:15px;};')),
            tags$style(HTML("hr {border-top: 2px solid #000000;}")),  # Styling of hr() used in below code
            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #5B779A}")),
            tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #5B779A}")),
            tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #5B779A}")),
            tags$style(HTML(" .navbar-default:hover .navbar-nav>li>a:hover {color:white;}")),
            tags$style(HTML('table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color: #9CADC2 !important;}'))),
  
  # Fluidpage: sets up all the HTM, CSS and Javascript needed
  fluidPage( 
    list(tags$head(HTML('<link rel="icon", href="heart.jpg", type="image/png" />'))),
    div(style = "padding: 1px 0px; width:100%",
        titlePanel(title = "", 
                   windowTitle = "Cardiovascular analysis")),

  # Navbar beginning + styling -------------------------------------------------
  navbarPage(
             # Title settings
             title = div(img(src = "heart.png", height = "75", width = "75", style = "display: flex; margin:-30px -25px; padding-right:10px"), 
                         div(class = "my-title",
                             h4('Cardiovascular disease analysis'),
                             tags$head(tags$style(HTML(".my-title :is(h4){color: white; font-weight: bold; position:relative; margin:-45px 30px;}"))))),
             
             # Color settings using 'fresh' package
             header <- use_theme(theme = theme),
             
             # Generel information panel ------------------------------------------
             navbarMenu("About",
                        tabPanel("About CVD", includeMarkdown("aboutCVD.Rmd")),
                        tabPanel("The data", includeMarkdown("dataInformation.Rmd")),
                        tabPanel("Pre-processing", includeMarkdown("datacleaning.Rmd"))),
             
             # Data panel ------------------------------------------------------
             tabPanel("Data",
                      sidebarLayout( 
                        sidebarPanel(width = 2, 
                                     style = sideBarPanelStyling,
                                     selectInput("dataFile", 
                                                 label = "Select a dataframe", 
                                                 choices = c("Hungarian",
                                                             "Switzerland", 
                                                             "Long Beach VA", 
                                                             "Combined data (small)", 
                                                             "Combined data (large)"),
                                                 selected = hungarian),
                                     br(),
                                     radioButtons("downloadType", "Select download type",
                                                  choices = c(".csv" = ".csv",
                                                              ".rds" = ".rds",
                                                              ".txt" = ".txt"),
                                                  inline = TRUE,
                                                  selected = ".csv"),
                                     helpText("NOTE: Separators for .rds files are needless."),
                                     selectInput("separator", "Table separator", choices = c("Tab (\\t)", "Comma (,)", "Semi-colon (;)", "Colon (:)", "Space"), selected = "Tab (\\t)"), 
                                     downloadButton("downloadTable", "Download", class = "btn-primary", style = downloadStyling),
                                     helpText("NOTE: In the large combined data, columns with NAs only is removed.")),
                      mainPanel(width = 10,
                                h3("Data"),
                                p(HTML(paste0("In this tab, the chosen data in the left sidepanel is presented by a table. 
                                  The table contains multiple variables as described in <em>About</em> tab. 
                                  It possible to sort the tables by a column from pressing the arrows aside each variable.
                                  The table shows a pre-processed version of the selected data. The steps made can be seen in the <em>About</em> tab as well.
                                  There is ", textOutput("uniqueID", inline=T), " unique patients in this data. <br/>
                                  To download the table press the download button in the left panel. If wanted the separator of the table when exported can be modifyed."))),
                                DTOutput("table"),
                                br(),
                                p("For the chosen data the number of missing values (NA) for each variable is seen below."),
                                DTOutput("NAtable"),
                                hr(),
                                h3("Summary"),
                                p(HTML("Statistical summary of table shown above. The factor variables such as <b>fbsFactor</b>, <b>cpFactor</b>, <b>datasetOrig</b> and <b>gender</b> is not shown here.")),
                                DTOutput("summary")))), #verbatimTextOutput
             
             # Visualization panel ---------------------------------------------
             tabPanel("Visualization", 
                      tabPanel("", 
                               sidebarLayout(
                                 sidebarPanel(width = 2, 
                                              style = sideBarPanelStyling,
                                              selectInput("dataFilePlot", 
                                                          label = print(tags$strong("Select a dataframe")),
                                                          choices = c("Hungarian",
                                                                      "Switzerland", 
                                                                      "Long Beach VA", 
                                                                      "Combined data (small)", 
                                                                      "Combined data (large)"),
                                                          selected = hungarian),
                                              
                                              # Sidebar panel for histograms
                                              conditionalPanel(condition = "input.conditionedPanel == 'Histogram'",
                                                  print(tags$strong("Select variables")),
                                                  selectInput('xcolHist', 'Left plot', ""),
                                                  selectInput('ycolHist', 'Right plot', ""),
                                                  checkboxGroupInput("logHist",
                                                                     "Log-transformation",
                                                                     choices = list("Left plot", "Right plot"),
                                                                     inline = TRUE),
                                                  sliderInput("bins1", HTML("Number of bins<br/>(left plot)"), min = 1, max = 50, value = 30),
                                                  sliderInput("bins2", HTML("Number of bins<br/>(right plot)"), min = 1, max = 50, value = 30)),
                                              
                                              # Sidebar panel for scatter plot
                                              conditionalPanel(condition = "input.conditionedPanel == 'Scatter plot'",
                                                               print(tags$strong("Select variables")),
                                                               selectInput('xcolScatter', 'X variable', ""),
                                                               selectInput('ycolScatter', 'Y variable', ""),
                                                               checkboxGroupInput("logScatter",
                                                                                  "Log-transformation",
                                                                                  choices = list("Log X", "Log Y"),
                                                                                  inline = TRUE),
                                                               uiOutput("range"),
                                                               selectInput('color_scatter', 'Select color', "black"),
                                                               br(),
                                                               # print(tags$strong("Download plot")),
                                                               radioButtons("extensionS", "Select File Extension",
                                                                            choices = c("png", "pdf", "jpeg"),
                                                                            selected = "png",
                                                                            inline = TRUE),
                                                               downloadButton("downloadPlotscatter", HTML("Download\nplot"), class = "btn-primary", style = downloadStyling)),
                                              
                                              # Side bar panel for boxplot
                                              conditionalPanel(condition = "input.conditionedPanel == 'Box plot'",
                                                               # print(tags$strong("Select variable")),
                                                               selectInput('xcolBox', 'Variable', ""),
                                                               checkboxInput('addScatter', label = "Add scatter to plot", value = FALSE, width = '100%'),
                                                               # print(tags$strong("Download plot")),
                                                               radioButtons("extensionB", "Select File Extension",
                                                                            choices = c("png", "pdf", "jpeg"),
                                                                            selected = "png",
                                                                            inline = TRUE),
                                                               downloadButton("downloadPlotbox", HTML("Download\nplot"), class = "btn-primary", style = downloadStyling))),
                                            
                               mainPanel(width = 10,
                                         tabsetPanel(id = "conditionedPanel",
                                                     # Histograms
                                                     tabPanel("Histogram",
                                                              h3("Histograms"),
                                                              p(HTML("The following graphs that shows the frequency of the chosen variables from the selected data(s). 
                                                                     You can add logarithmic transformation to the plots plus select the number of bins that groups the data to X bins with equal width.
                                                                     For continuous variables a vertical striped line is visual to the plot(s). This represents the mean of the selected variable.
                                                                     Lastly it's possible to color the variable values by a categorical variable such as the presence of cardiovascular disease <b>cvdPresent</b>.")),
                                                              fluidRow(
                                                                column(6, id = "histogram", 
                                                                       bsTooltip(id = "histogram",
                                                                                 title = "histogram1", 
                                                                                 trigger = "hover", 
                                                                                 placement = "top"),
                                                                       br(),
                                                                       plotOutput("histogramPlot1"),
                                                                       br(),
                                                                       hr(),
                                                                       selectInput('facetH1', 'Color by variable', ""),
                                                                       helpText("NOTE: Unlabelled observations are marked as 'unknown' or 'NA'"),
                                                                       br(),
                                                                       radioButtons("extensionH1", "Select file extension for download:",
                                                                                    choices = c("png", "pdf", "jpeg"),
                                                                                    selected = "png",
                                                                                    inline = TRUE),
                                                                       downloadButton("downloadPlothist1", HTML("Download\nplot"), class = "btn-primary")),
                                                                column(6, 
                                                                       id = "histogram2", 
                                                                       bsTooltip(id = "histogram2", 
                                                                                 title = "histogram2", 
                                                                                 trigger = "hover", 
                                                                                 placement = "top"),
                                                                       br(),
                                                                       plotOutput("histogramPlot2"),
                                                                       br(),
                                                                       hr(),
                                                                       selectInput('facetH2', 'Color by variable', ""),
                                                                       helpText("NOTE: Unlabelled observations are marked as 'unknown' or 'NA'"),
                                                                       br(),
                                                                       radioButtons("extensionH2", "Select file extension for download:",
                                                                                    choices = c("png", "pdf", "jpeg"),
                                                                                    selected = "png",
                                                                                    inline = TRUE),
                                                                       downloadButton("downloadPlothist2", HTML("Download\nplot"), class = "btn-primary")))),
                                                     
                                                     # Scatter plot
                                                     tabPanel("Scatter plot", 
                                                              h3("Scatter plot"),
                                                              p(HTML("The following graph displays points representing values for the two chosen variables. This plot type can show possible relatioships between variables. 
                                                                     In the sidepanel logarithmic transformation can be applied to any of the axes. It's possible to color the points in 5 different colors and change the x-axis limits.")),
                                                              bsTooltip(id = "scatter", 
                                                                        title = "scatterplot", 
                                                                        trigger = "hover", 
                                                                        placement = "bottom"),
                                                              br(),
                                                              plotOutput("scatter")),
                                                     
                                                     # Boxplot
                                                     tabPanel("Box plot", 
                                                              h3("Box plot"),
                                                              p(HTML("The following graph displays the distribution of continuous variables over the presence of cardiovascular disease. 
                                                                     The amount of patients with or witout cardiovascular disease is shown in the labels under each box. 
                                                                     The box shows the minimum, first quartile, median, third quartile and maximum values of the variable chosen. Outliers are marked as dusty green plusses (+). 
                                                                     To see the actual placement of the variable values, scatter can be added overlaying the boxes.")),
                                                              bsTooltip(id = "box", 
                                                                        title = "boxplot",
                                                                        trigger = "hover", 
                                                                        placement = "bottom"),
                                                              br(),
                                                              plotOutput("box"))))))),
                      
             
             
             # Modelling panel -------------------------------------------------
             tabPanel("Models", 
                      sidebarLayout(
                        sidebarPanel(width = 2, 
                                     style = sideBarPanelStyling,
                                     
                                     # Logistic regression
                                     conditionalPanel(condition = "input.conditionedPanel2 == 'Logistic regression'",
                                                      selectInput("dataFileModel", 
                                                                  label = print(tags$strong("Select a dataframe")),
                                                                  choices = c("Hungarian",
                                                                              "Switzerland", 
                                                                              "Long Beach VA", 
                                                                              "Combined data (small)", 
                                                                              "Combined data (large)"),
                                                                  selected = hungarian),
                                                      p(HTML(paste0("Dependent variable: ", strong("cvdPresent")))),
                                                      br(),
                                                      checkboxGroupInput("independentVars", "Select independent variables", choices = "")),
                                     
                                     # K-Nearest Neighbor
                                     conditionalPanel(condition = "input.conditionedPanel2 == 'K-Nearest Neighbor'",
                                                      selectInput("dataFileModelKNN", 
                                                                  label = print(tags$strong("Select a dataframe")),
                                                                  choices = c("Hungarian",
                                                                              # "Switzerland",
                                                                              "Long Beach VA", 
                                                                              "Combined data (small)"), 
                                                                  selected = hungarian),
                                                      numericInput("k", "Number of Neighbors (k)", 3, min = 1, max = 30),
                                                      selectInput("targetVar1", "Select variable 1", ""),
                                                      selectInput("targetVar2", "Select variable 2", ""))),
                        
                        mainPanel(width = 10,
                                  tabsetPanel(id = "conditionedPanel2",
                                              
                                              # Logistic regression
                                              tabPanel("Logistic regression", 
                                                       fluidRow(
                                                         column(12, id = "logreg", 
                                                                bsTooltip(id = "logreg", 
                                                                          title = "logisticRegression", 
                                                                          trigger = "hover", 
                                                                          placement = "bottom")),
                                                         includeMarkdown("logRegExplained.rmd"),
                                                         hr(),
                                                         verbatimTextOutput("logregSummary"),
                                                         br(),
                                                         h4("Accuracy"),
                                                         p(HTML(paste0("The accuracy for the model trained on 80% of data is ", textOutput("accuracyTrain", inline=T),
                                                           ", while the testing accurcary tested on 20% of unseen data is ", textOutput("accuracyTest", inline = T), "."))))),
                                              
                                              # K-Nearest Neighbor
                                              tabPanel("K-Nearest Neighbor", 
                                                       bsTooltip(id = "KNN", 
                                                                 title = "knn", 
                                                                 trigger = "hover",
                                                                 placement = "bottom"),
                                                       # h3("K-Nearest Neighbors"),
                                                       includeMarkdown("KNNExplained.rmd"),
                                                       hr(),
                                                       p(HTML("Below is a visualization of misclassification of the variable <b>cvdPresent</b> based on the chosen variables. 
                                                         The blue line represents the misclassification of 80% of the data, where the KNN model is being trained on, 
                                                         while the red line represents the prediction ability of unseen data (20% of the data) with the trained model.")),
                                                       plotly::plotlyOutput("knnEPlot"),
                                                       # plotOutput("knnEPlot"),
                                                       br(),
                                                       htmlOutput("knnInfo"),
                                                       hr(),
                                                       p(HTML("Based on the chosen K-value the plots below shows the results of prediction from the KNN model,
                                                              The left plot shows the prediction, while the right plot show the true and original class.")),
                                                       splitLayout(cellWidths = c("50%", "50%"), 
                                                                   plotOutput("knnPredictionPlot1"), 
                                                                   plotOutput("knnPredictionPlot2")),
                                                       h4("Accuracy"),
                                                       p(HTML(paste0("The testing accurcary tested on 20% of the data being unseen with the chosen k is ", textOutput("accuracyTestKNN", inline = T), "."))))))))
             )
  )
)