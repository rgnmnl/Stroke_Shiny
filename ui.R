library(shiny)
library(RSQLite)
library(DT) ## Not sure if you have to load packages in both ui.r and server.r
library(DBI)
library(qqman)
library(manhattanly)
library(plotly)
library(magrittr)
library(data.table)
library(dplyr)

navbarPage(
  title="",
  ### TAB 1
  tabPanel("Welcome", 
    fluidPage(
      includeMarkdown("Dashboard.Rmd")
    )
  ), ## END TAB 1
  ### TAB 2
  tabPanel("Results",
    sidebarLayout(
      sidebarPanel(
        selectInput("user_stroketype", label="Choose a stroke type", choices = c('Intracerebral Haemorrhagic' = "ih", 'Ischaemic' = "is", 'Subarachnoid Haemorrhagic' = "sh"), selected = 'is', multiple=FALSE),
        radioButtons("user_modtype", label="Choose a model", choiceValues = c('m1','m2','m3'), choiceNames = c('1','2','3')),
        # selectInput("user_rsID", label='Enter an RS ID', choices=NULL, multiple=FALSE, selectize=TRUE) ##TEST 1
        # uiOutput("list_rsid"), ## TEST 2
        # selectizeInput("user_rsID", choices = NULL, label="Enter an rsID", options = list(maxOptions = 1000)),
        radioButtons("user_lookuptype", label="Look up SNP by:", choiceValues = c('rsID','chrpos'), choiceNames = c('rsID','Position')),
        uiOutput("lookup_out"),
        # textInput("user_chrpos", label="Enter chromosome position:", value = "99792894"),##CHECK TO MAKE SURE TABLES/PLOTS ARE WORKING
        em("Note: SNP IDs and positions are reported in Genome Reference Consortium Human genome
                    build 37 co-ordinates (GRChb37).", style = "font-size:10px"),
        br(),
        br(),
        actionButton("update_table", "Update Table",
                     style="color: #fff; background-color: #1b6863; border-color: #1b6863"),
        br(),
        br(),
        # selectInput('col', 'Column ID', choices = c('Chromosome')),
        # actionButton('select2', 'Select Column'),
        h4("Useful links"),
        downloadButton("downloadData", "Download Table",
                       style="color: #fff; background-color: #1b6863; border-color: #1b6863"),
        br(),
        br(),
        h4("Look up SNP in:"),
        uiOutput("GWASlink"),
        # actionButton(inputId = 'ab1', label = "Look up SNP in GWAS Catalog", icon = icon("link"), 
        #              onclick = "window.open('https://www.ebi.ac.uk/gwas/', '_blank')",
        #              style="color: #fff; background-color: #1b6863; border-color: #1b6863"),
        uiOutput("GTEXlink"),
        uiOutput("ISGClink")
        # actionButton(inputId = 'ab1', label = "Look up SNP in GTEX Portal", icon = icon("link"), 
        #              onclick = "window.open('https://www.gtexportal.org/home/', '_blank')",
        #              style="color: #fff; background-color: #1b6863; border-color: #1b6863")
        # tags$style(
        #   HTML('.well { 
        #           background-color: #f4ce42;
        #        }
        #        button { 
        #           font-family: "Arial";
        #        }'
        #        #body, label, input, select{}'
        # ))
        #width = 3
      ),
      mainPanel(
        fluidPage(
          h4("Results:"),
          br(),
          # conditionalPanel(condition = "input$user_rsID == NULL",
          #                  verbatimTextOutput("warnmsg")),
          dataTableOutput("gwas_user_rsID"),
          br(),
          br(),
          h4("Annotations for selected rs ID:"),
          br(),
          dataTableOutput("user_rsID_annot"),
          br(),
          br(),
          h4("Results for nearby SNPs:"),
          br(),
          dataTableOutput("user_snp_window")
          # conditionalPanel(condition = "")
        )
      )
    )
  ), ### END TAB 2
  ### TAB 3
  tabPanel("Regional Plot",
        div(plotlyOutput("manhattan", width="60%", height="auto"), align = "center")
    # sidebarLayout(
    #   sidebarPanel(
    #     radioButtons("user_plottype", label="Choose a plot", choiceNames = c('QQ-Plot', 'Manhattan'), choiceValues = c("qq", "manhattan"), selected = "qq")
    #   ),
    #   mainPanel(
    #     fluidPage(
    #       # plotOutput("plot_user")
    #       conditionalPanel(
    #         condition = "input.user_plottype == 'qq'", plotOutput("qq")),
    #       conditionalPanel(
    #         condition = "input.user_plottype == 'manhattan'", plotlyOutput("manhattan"))
    #       # br(),
    #       # br(),
    #       # "Annotations for selected rs ID:",
    #       # br(),
    #       # dataTableOutput("TableInfo")
    #     )
    #   )
    # )
  ) ### END TAB 3
) ### END NAVBAR PAGE


# 
# 
# 
# 
# 