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

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    title="",
    ### TAB 1
    tabPanel("Welcome", 
             fluidPage(
               includeMarkdown("Dashboard.Rmd")
             )
    ), ## END TAB 1
    ### TAB 2
    tabPanel("Genome-wide Results",
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
                 uiOutput("GTEXlink")
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
    tabPanel("Plots",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("user_plottype", label="Choose a plot", choiceNames = c('QQ-Plot', 'Manhattan'), choiceValues = c("qq", "manhattan"), selected = "qq")
               ),
               mainPanel(
                 fluidPage(
                   # plotOutput("plot_user")
                   conditionalPanel(
                     condition = "input.user_plottype == 'qq'", plotOutput("qq")),
                   conditionalPanel(
                     condition = "input.user_plottype == 'manhattan'", plotlyOutput("manhattan"))
                   # br(),
                   # br(),
                   # "Annotations for selected rs ID:",
                   # br(),
                   # dataTableOutput("TableInfo")
                 )
               )
             )
    ) ### END TAB 3
  ) ### END NAVBAR PAGE
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$lookup_out <- renderUI({
    if(input$user_lookuptype == 'rsID'){
      textInput("user_rsID", label="Enter rsID:", value = "rs34148057")##CHECK TO MAKE SURE TABLES/PLOTS ARE WORKING
    } else {
      numericInput("user_chrpos", label = "Enter chromosome position:", value = "99792894")
    }
  })
  
  output$GWASlink <- renderUI({
    if(input$user_lookuptype == 'rsID'){
      gwas <- paste0("https://www.ebi.ac.uk/gwas/search?query=", req(input$user_rsID))
      # actionButton(inputId = 'ab1', label = "Look up SNP in GWAS Catalog", icon = icon("link"),
      #              onclick = "window.open(gwas, '_blank')",
      #              style="color: #fff; background-color: #1b6863; border-color: #1b6863")
      shiny::a(h4("GWAS Catalog" , id = "button1",
                  style = "font-size:13px; background-color: #1b6863; color: white; padding: 14px 25px; text-align: center;
                  text-decoration: none; display: inline-block; border-radius: 4px; width: 150px;"),
               target = "_blank", href = gwas)
    } else {
      sqldb <- dbConnect(SQLite(), dbname = "annot_comb.sqlite")
      gwasrs <- dbGetQuery(sqldb, paste0("select ID from ", req(input$user_stroketype), "_annot where POS = '", req(input$user_chrpos), "'"))[1]
      gwas <- paste0("https://www.ebi.ac.uk/gwas/search?query=", gwasrs)
      shiny::a(h4("GWAS Catalog" , id = "button1",
                  style = "font-size:13px; background-color: #1b6863; color: white; padding: 14px 25px; text-align: center;
                  text-decoration: none; display: inline-block; border-radius: 4px; width: 150px;"),
               target = "_blank", href = gwas)
    }
  })
  
  output$GTEXlink <- renderUI({
    if(input$user_lookuptype == 'rsID'){
      gtex <- paste0("https://gtexportal.org/home/snp/", req(input$user_rsID))
      # actionButton(inputId = 'ab1', label = "Look up SNP in GWAS Catalog", icon = icon("link"),
      #              onclick = "window.open(gwas, '_blank')",
      #              style="color: #fff; background-color: #1b6863; border-color: #1b6863")
      shiny::a(h4("GTEX Portal" , id = "button1",
                  style = "font-size:13px; background-color: #1b6863; color: white; padding: 14px 25px; text-align: center;
                  text-decoration: none; display: inline-block; border-radius: 4px; width: 150px;"),
               target = "_blank", href = gtex)
    } else {
      sqldb <- dbConnect(SQLite(), dbname = "annot_comb.sqlite")
      gtexrs <- dbGetQuery(sqldb, paste0("select ID from ", req(input$user_stroketype), "_annot where POS = '", req(input$user_chrpos), "'"))[1]
      gtex <- paste0("https://gtexportal.org/home/snp/", gtexrs)
      shiny::a(h4("GTEX Portal" , id = "button1",
                  style = "font-size:13px; background-color: #1b6863; color: white; padding: 14px 25px; text-align: center;
                  text-decoration: none; display: inline-block; border-radius: 4px; width: 150px;"),
               target = "_blank", href = gtex)
    }
  })
  
  #### USING NEW DB FORMAT ####
  output$gwas_user_rsID <- renderDataTable({
    input$update_table
    sqldb <- dbConnect(SQLite(), dbname = "annot_comb.sqlite")
    if(input$user_lookuptype == 'rsID'){
      validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
      rsrow <- isolate(dbGetQuery(sqldb, paste0("select * from ", req(input$user_stroketype), "_annot where ID = '", req(input$user_rsID), "'")))
      result <- select(rsrow, c("CHROM","POS","ID","REF","ALT1", starts_with(req(input$user_modtype))))
    } else {
      validate(need(isolate(input$user_chrpos != ""), "Please select table parameters."))
      rsrow <- isolate(dbGetQuery(sqldb, paste0("select * from ", req(input$user_stroketype), "_annot where POS = '", req(input$user_chrpos), "'")))
      result <- select(rsrow, c("CHROM","POS","ID","REF","ALT1", starts_with(req(input$user_modtype))))
    }
    dbDisconnect(sqldb) ## Disconnect when done
    names(result) <- sub(".*_","",names(result))
    DT::datatable(result, options=list(columnDefs = list(list(visible=FALSE, targets=c(4:12))), dom = 't'))
  })
  
  output$user_rsID_annot <- renderDataTable({
    input$update_table
    validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
    sqldb <- dbConnect(SQLite(), dbname = "annot_comb.sqlite")
    if(input$user_lookuptype == 'rsID'){
      validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
      annot <- isolate(dbGetQuery(sqldb, paste0("select * from ", req(input$user_stroketype), "_annot where ID = '", req(input$user_rsID), "'")))
    } else {
      validate(need(isolate(input$user_chrpos != ""), "Please select table parameters."))
      annot <- isolate(dbGetQuery(sqldb, paste0("select * from ", req(input$user_stroketype), "_annot where POS = '", req(input$user_chrpos), "'")))
    }
    dbDisconnect(sqldb) ## Disconnect when done
    DT::datatable(annot, options=list(columnDefs = list(list(visible=FALSE, targets=c(6:38, 44))), dom = 't'))
  })
  
  output$user_snp_window <- renderDataTable({
    input$update_table
    sqldb <- dbConnect(SQLite(), dbname = "annot_comb.sqlite")
    if(input$user_lookuptype == 'rsID'){
      validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
      marker <- isolate(dbGetQuery(sqldb, paste0("select POS from ", req(input$user_stroketype), "_annot where ID = '", req(input$user_rsID), "'"))[,1])
      max <- marker + 100000
      min <- marker - 100000
      marker_chrom <- isolate(dbGetQuery(sqldb, paste0("select CHROM from ", req(input$user_stroketype), "_annot where ID = '", req(input$user_rsID), "'"))[1,1])
      range <- isolate(dbGetQuery(sqldb, paste0("SELECT * FROM ", req(input$user_stroketype), "_annot WHERE CHROM = ", marker_chrom, " AND POS > ", min, " AND POS < ", max)))
      mod_range <- select(range, c("CHROM","POS","ID","REF","ALT1", starts_with(req(input$user_modtype))))
    } else {
      validate(need(isolate(input$user_chrpos != ""), "Please select table parameters."))
      max <- input$user_chrpos + 100000
      min <- input$user_chrpos - 100000
      marker_chrom <- isolate(dbGetQuery(sqldb, paste0("select CHROM from ", req(input$user_stroketype), "_annot where POS = '", req(input$user_chrpos), "'"))[1,1])
      range <- isolate(dbGetQuery(sqldb, paste0("SELECT * FROM ", req(input$user_stroketype), "_annot WHERE CHROM = ", marker_chrom, " AND POS > ", min, " AND POS < ", max)))
      mod_range <- select(range, c("CHROM","POS","ID","REF","ALT1", starts_with(req(input$user_modtype))))
    }
    dbDisconnect(sqldb) ## Disconnect when done
    names(mod_range) <- sub(".*_","",names(mod_range)) 
    DT::datatable(mod_range, options=list(columnDefs = list(list(visible=FALSE, targets=c(4:12)))))
  })
  #### END USING NEW DB FORMAT ####
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$user_stroketype, "_", input$user_modtype, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(range, file, row.names = FALSE)
    }
  )
  
  output$qq <- renderPlot({
    sqldb <- dbConnect(SQLite(), dbname = "annot_comb.sqlite")
    if(input$user_lookuptype == 'rsID'){
      validate(need(isolate(input$user_rsID != ""), "Please select table parameters in the previous tab."))
      marker <- dbGetQuery(sqldb, paste0("select POS from ", input$user_stroketype, "_annot where ID = '", input$user_rsID, "'"))[,1]
      max <- marker + 500000
      min <- marker - 500000
      marker_chrom <- dbGetQuery(sqldb, paste0("select CHROM from ", input$user_stroketype, "_annot where ID = '", input$user_rsID, "'"))[1,1]
      range <- dbGetQuery(sqldb, paste0("SELECT * FROM ", input$user_stroketype, "_annot WHERE CHROM = ", marker_chrom, " AND POS > ", min, " AND POS < ", max))
      mod_range <- select(range, c("CHROM","POS","ID","REF","ALT1", starts_with(req(input$user_modtype))))
      range_plot <- range[,c(3,1,2,16)]
      names(range_plot) <- c('SNP', 'CHR', 'BP', 'P')
    } else {
      validate(need(isolate(input$user_rsID != ""), "Please select table parameters in the previous tab."))
      max <- input$user_chrpos + 500000
      min <- input$user_chrpos - 500000
      marker_chrom <- dbGetQuery(sqldb, paste0("select CHROM from ", input$user_stroketype, "_annot where POS = '", req(input$user_chrpos), "'"))[1,1]
      range <- dbGetQuery(sqldb, paste0("SELECT * FROM ", input$user_stroketype, "_annot WHERE CHROM = ", marker_chrom, " AND POS > ", min, " AND POS < ", max))
      mod_range <- select(range, c("CHROM","POS","ID","REF","ALT1", starts_with(req(input$user_modtype))))
      range_plot <- range[,c(3,1,2,16)]
      names(range_plot) <- c('SNP', 'CHR', 'BP', 'P')
    }
    qq(range_plot$P, main=paste0("QQ-Plot Plot for Stroke Type=", input$user_stroketype, " and SNP =", input$user_rsID))
    dbDisconnect(sqldb) ## Disconnect when done
    # plotly_IMAGE(range_plot$P, format = "png", out_file = paste0(input$user_plottype, "_", input$user_stroketype, "_", input$user_modtype, ".png", sep = ""))
  })
  
  man_plot <- function(){
    sqldb <- dbConnect(SQLite(), dbname = "annot_comb.sqlite")
    if(input$user_lookuptype == 'rsID'){
      validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
      marker <- isolate(dbGetQuery(sqldb, paste0("select POS from ", req(input$user_stroketype), "_annot where ID = '", req(input$user_rsID), "'"))[,1])
      max <- marker + 100000
      min <- marker - 100000
      marker_chrom <- isolate(dbGetQuery(sqldb, paste0("select CHROM from ", req(input$user_stroketype), "_annot where ID = '", req(input$user_rsID), "'"))[1,1])
      range <- isolate(dbGetQuery(sqldb, paste0("SELECT * FROM ", req(input$user_stroketype), "_annot WHERE CHROM = ", marker_chrom, " AND POS > ", min, " AND POS < ", max)))
      mod_range <- select(range, c("CHROM","POS","ID", starts_with(req(input$user_modtype)),"Func.refGene","Gene.refGene","GeneDetail.refGene","ExonicFunc.refGene","AAChange.refGene"))
      names(mod_range) <- sub(".*_","",names(mod_range)) 
      range_plot <- manhattanr(mod_range, chr = "CHROM", snp = "ID", bp = "POS", p = "P", gene = "Gene.refGene", 
                               annotation1 ="Func.refGene", annotation2 = "GeneDetail.refGene",
                               annotation3 = "ExonicFunc.refGene", annotation4 = "AAChange.refGene",
                               logp = TRUE)
    } else {
      validate(need(isolate(input$user_chrpos != ""), "Please select table parameters."))
      max <- input$user_chrpos + 100000
      min <- input$user_chrpos - 100000
      marker_chrom <- isolate(dbGetQuery(sqldb, paste0("select CHROM from ", req(input$user_stroketype), "_annot where POS = '", req(input$user_chrpos), "'"))[1,1])
      range <- isolate(dbGetQuery(sqldb, paste0("SELECT * FROM ", req(input$user_stroketype), "_annot WHERE CHROM = ", marker_chrom, " AND POS > ", min, " AND POS < ", max)))
      mod_range <- select(range, c("CHROM","POS","ID",starts_with(req(input$user_modtype)), "Func.refGene","Gene.refGene","GeneDetail.refGene","ExonicFunc.refGene","AAChange.refGene"))
      names(mod_range) <- sub(".*_","",names(mod_range))
      range_plot <- manhattanr(mod_range, chr = "CHROM", snp = "ID", bp = "POS", p = "P", gene = "Gene.refGene", 
                               annotation1 ="Func.refGene", annotation2 = "GeneDetail.refGene",
                               annotation3 = "ExonicFunc.refGene", annotation4 = "AAChange.refGene",
                               logp = TRUE)
    }
    man_plot <- manhattanly(range_plot, point_size=3, col = '#FF5733')
    dbDisconnect(sqldb) ## Disconnect when done
    man_plot %<>% plotly::layout(man_plot, xaxis = list(range = c(min, max)), hoverlabel = list(bgcolor = '#ffA07A', 
                                                                                                font = list(color = '#000000', family = 'Arial', size = 14, opacity = 0.7))
    ) 
    man_plot
  }
  
  output$manhattan <- renderPlotly({man_plot()})
}

# Run the application 
shinyApp(ui = ui, server = server)

