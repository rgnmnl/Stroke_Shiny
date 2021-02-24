library(shiny)
library(DT)
library(RSQLite)
# uniq_rsid <- fread("~/Documents/Stroke Shiny App/stroke_app/data/uniq_rsid.txt")

function(input, output, session) {
  # sqlite_file <- "ukbb.sqlite"
  # sqlite_file2 <- "annot.sqlite"
  output$lookup_out <- renderUI({
    if(input$user_lookuptype == 'rsID'){
      textInput("user_rsID", label="Enter rsID:", value = "rs34148057")##CHECK TO MAKE SURE TABLES/PLOTS ARE WORKING
    } else {
      numericInput("user_chrpos", label = "Enter chromosome position:", value = "99792894")
    }
    # if(input$user_lookuptype == 'chrpos'){
    #   
    # }
  })
  
  # out_snp_names <- reactive({
  #   sqldb <- dbConnect(SQLite(), dbname = sqlite_file)
  #   # c("rs10744777","rs2634074","rs2107595","rs34166160","rs11833579","rs505922")
  #   snp_names <- as.list(dbGetQuery(sqldb, paste0("select ID from ", input$user_stroketype,"_", input$user_modtype))[,1])
  #   dbDisconnect(sqldb)
  #   # return(snp_names)
  # })
  
  # uniq_rsid <- fread("~/Documents/Stroke Shiny App/stroke_app/data/uniq_rsid.txt", header=FALSE, sep="\t")
  # updateSelectizeInput(session, "user_rsID", choices = uniq_rsid$V1[1:1000], selected = NULL, server = TRUE)
  # 
  # errstat <- reactive({
  #   ifelse (input$data=="mtcars",T,F)
  # })
  # 
  # output$warnmsg <- renderPrint({
  #   if (errstat()){
  #     print("Warning message - blah blah blah")
  #     print(input$data)
  #     head(data())
  #   } else {
  #     print("No error")
  #   }
  # })
  # output$warnstat <- renderText({ifelse(errstat(),"Error","No error") })
  
  ## OUTPUT rsID only
  # output$gwas_user_rsID <- renderDataTable({
  #   input$update_table
  #   sqldb <- dbConnect(SQLite(), dbname = "annot.sqlite")
  #   if(input$user_lookuptype == 'rsID'){
  #     validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
  #     result <- isolate(dbGetQuery(sqldb, paste0("select * from ", req(input$user_stroketype), "_annot_", req(input$user_modtype), " where ID = '", req(input$user_rsID), "'")))
  #   } else {
  #     validate(need(isolate(input$user_chrpos != ""), "Please select table parameters."))
  #     result <- isolate(dbGetQuery(sqldb, paste0("select * from ", req(input$user_stroketype), "_annot_", req(input$user_modtype), " where POS = '", req(input$user_chrpos), "'")))
  #   }
  #   dbDisconnect(sqldb) ## Disconnect when done
  #   DT::datatable(result, options=list(columnDefs = list(list(visible=FALSE, targets=c(6:9, 11:12, 15, 17:22))), dom = 't'))
  # })
  
  # output$user_rsID_annot <- renderDataTable({
  #   input$update_table
  #   validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
  #   sqldb <- dbConnect(SQLite(), dbname = "annot_comb.sqlite")
  #   if(input$user_lookuptype == 'rsID'){
  #     validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
  #     annot <- isolate(dbGetQuery(sqldb, paste0("select * from ", req(input$user_stroketype), "_annot_", req(input$user_modtype), " where ID = '", req(input$user_rsID), "'")))
  #   } else {
  #     validate(need(isolate(input$user_chrpos != ""), "Please select table parameters."))
  #     annot <- isolate(dbGetQuery(sqldb, paste0("select * from ", req(input$user_stroketype), "_annot_", req(input$user_modtype), " where POS = '", req(input$user_chrpos), "'")))
  #   }
  #   dbDisconnect(sqldb) ## Disconnect when done
  #   DT::datatable(annot, options=list(columnDefs = list(list(visible=FALSE, targets=c(1, 2, 4:16, 22))), dom = 't'))
  # })
  # 
  # # ORIGINAL
  # output$user_snp_window <- renderDataTable({
  #   input$update_table
  #   sqldb <- dbConnect(SQLite(), dbname = "annot.sqlite")
  #   if(input$user_lookuptype == 'rsID'){
  #   validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
  #   marker <- isolate(dbGetQuery(sqldb, paste0("select POS from ", req(input$user_stroketype), "_annot_", req(input$user_modtype), " where ID = '", req(input$user_rsID), "'"))[,1])
  #   max <- marker + 100000
  #   min <- marker - 100000
  #   marker_chrom <- isolate(dbGetQuery(sqldb, paste0("select CHROM from ", req(input$user_stroketype), "_annot_", req(input$user_modtype), " where ID = '", req(input$user_rsID), "'"))[1,1])
  #   range <- isolate(dbGetQuery(sqldb, paste0("SELECT * FROM ", req(input$user_stroketype), "_annot_", input$user_modtype, " WHERE CHROM = ",
  #                                             marker_chrom, " AND POS > ", min, " AND POS < ", max)))
  #   } else {
  #     validate(need(isolate(input$user_chrpos != ""), "Please select table parameters."))
  #     max <- input$user_chrpos + 100000
  #     min <- input$user_chrpos - 100000
  #     marker_chrom <- isolate(dbGetQuery(sqldb, paste0("select CHROM from ", req(input$user_stroketype), "_annot_", req(input$user_modtype), " where POS = '", req(input$user_chrpos), "'"))[1,1])
  #     range <- isolate(dbGetQuery(sqldb, paste0("SELECT * FROM ", req(input$user_stroketype), "_annot_", input$user_modtype, " WHERE CHROM = ",
  #                                               marker_chrom, " AND POS > ", min, " AND POS < ", max)))
  #   }
  #   dbDisconnect(sqldb) ## Disconnect when done
  #   DT::datatable(range, options=list(columnDefs = list(list(visible=FALSE, targets=c(6:9, 11:12, 14:15, 16:22)))))
  # })
  
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
  
  
  # proxy = dataTableProxy('gwas_user_rsID')
  # 
  # observeEvent(input$select2, {
  #   proxy %>% selectColumns(input$col)
  # })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$user_stroketype, "_", input$user_modtype, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(range, file, row.names = FALSE)
    }
  )
  
  # output$manhattan <- renderPlotly({
  #   validate(need(isolate(input$user_rsID != ""), "Please select table parameters in the previous tab."))
  #   sqldb <- dbConnect(SQLite(), dbname = "ukbb.sqlite")
  #   marker <- dbGetQuery(sqldb, paste0("select POS from ", input$user_stroketype, "_", input$user_modtype," where ID = '", input$user_rsID, "'"))[,1]
  #   max <- marker + 500000
  #   min <- marker - 500000
  #   marker_chrom <- dbGetQuery(sqldb, paste0("select CHROM from ", input$user_stroketype, "_", input$user_modtype," where ID = '", input$user_rsID, "'"))[1,1]
  #   range <- dbGetQuery(sqldb, paste0("SELECT * FROM ", input$user_stroketype,
  #                                     "_mod1 WHERE CHROM = ",
  #                                     marker_chrom, " AND POS > ", min, " AND POS < ", max))
  #   range_plot <- range[,c(3,1,2,16)]
  #   man_plot <- manhattanly(range_plot, chr = "CHROM", snp = "ID", bp = "POS", range = c(min,max), annotatePval = 0.005, #highlight = range_plot$SNP,
  #                           title = paste0("Manhattan Plot for Stroke Type=", input$user_stroketype, " and SNP =", input$user_rsID, " range ", min(range$POS), " & ", max(range$POS)))
  #   dbDisconnect(sqldb) ## Disconnect when done
  #   plotly::ggplotly(man_plot)
  #   
  #   # plotly_IMAGE(man_plot, format = "png", out_file = paste0(input$user_plottype, "_", input$user_stroketype, "_", input$user_modtype, ".png", sep = ""))
  #   
  # })
  
  # output$qq <- renderPlot({
  #   validate(need(isolate(input$user_rsID != ""), "Please select table parameters in the previous tab."))
  #   sqldb <- dbConnect(SQLite(), dbname = "annot_comb.sqlite")
  #   all_pval <- dbGetQuery(sqldb, paste0("select CHROM, POS, ID, m1_P, m2_P, m3_P from ", input$user_stroketype, "_annot"))
  #   mod_pval <- select(all_pval, c("ID","CHROM","POS", starts_with(req(input$user_modtype))))
  #   names(mod_pval) <- c('SNP', 'CHR', 'BP', 'P')
  #   qq(mod_pval$P, main=paste0("QQ-Plot Plot for Stroke Type = ", input$user_stroketype, " and Model = ", input$user_modtype))
  #   dbDisconnect(sqldb)
  # })
  
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
  
  # man_plot <- function(){
  #   validate(need(isolate(input$user_rsID != ""), "Please select table parameters in the previous tab."))
  #   sqldb <- dbConnect(SQLite(), dbname = "annot.sqlite")
  #   marker <- dbGetQuery(sqldb, paste0("select POS from ", input$user_stroketype, "_annot_", input$user_modtype," where ID = '", input$user_rsID, "'"))[,1]
  #   max <- marker + 100000
  #   min <- marker - 100000
  #   marker_chrom <- dbGetQuery(sqldb, paste0("select CHROM from ", input$user_stroketype, "_annot_", input$user_modtype," where ID = '", input$user_rsID, "'"))[1,1]
  #   range <- dbGetQuery(sqldb, paste0("SELECT * FROM ", input$user_stroketype, "_annot_", input$user_modtype, " WHERE CHROM = ",
  #                                     marker_chrom, " AND POS > ", min, " AND POS < ", max))
  #   range_plot <- manhattanr(range, chr = "CHROM", snp = "ID", bp = "POS", p = "P", gene = "Gene.refGene", 
  #                            annotation1 ="Func.refGene", annotation2 = "GeneDetail.refGene",
  #                            annotation3 = "ExonicFunc.refGene", annotation4 = "AAChange.refGene",
  #                            logp = TRUE)
  #   man_plot <- manhattanly(range_plot, point_size=2, col = '#ff0000')
  #   # range_plot <- range[,c(1:3,16:21)]
  #   # range_plot$significantSNP <- ifelse(range_plot$P < 0.05, range_plot$ID, "")
  #   # # man_plot <- manhattanly(range_plot, chr = "CHROM", snp = "ID", bp = "POS", annotatePval = 0.05, annotation1 ="Func.refGene", annotation2 = "Gene.refGene", annotation3 = "GeneDetail.refGene",
  #   #             annotation4 = "ExonicFunc.refGene", annotation4 = "AAChange.refGene", point_size=2, col = '#ff0000', highlight = significantSNP)
  #   # plotData <- manhattanr(range_plot, chr = "CHROM", snp = "ID", bp = "POS")[["data"]]
  #   # annotate <- plotData[order(plotData$P),][1:10,]
  #   # xc <- annotate$pos
  #   # yc <- annotate$logp
  #   dbDisconnect(sqldb) ## Disconnect when done
  #   man_plot %<>% plotly::layout(man_plot, xaxis = list(range = c(min, max)), hoverlabel = list(bgcolor = '#ffcccc', 
  #                                font = list(color = '#000000', family = 'Arial', size = 14, opacity = 0.7))
  #                                ) 
  #   # plotly::ggplotly(man_plot) 
  #   # plot %>% add_annotations(x = xc,
  #   #                          y = yc)
  #                            # ,xref = "x",
  #                            # yref = "y",
  #                            # showarrow = TRUE,
  #                            # arrowhead = 4,
  #                            # arrowsize = .5,
  #                            # ax = 20,
  #                            # ay = -40)
  #   man_plot
  # }
  
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
      man_plot <- manhattanly(range_plot, point_size=2, col = '#ff0000')
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
      man_plot <- manhattanly(range_plot, point_size=2, col = '#ff0000')
    }
    dbDisconnect(sqldb) ## Disconnect when done
    man_plot %<>% plotly::layout(man_plot, xaxis = list(range = c(min, max)), hoverlabel = list(bgcolor = '#ffcccc', 
                                 font = list(color = '#000000', family = 'Arial', size = 14, opacity = 0.7))
    ) 
    man_plot
  }
  
  output$manhattan <- renderPlotly({man_plot()})
  
  
  # output$downloadman <- downloadHandler(
  #   filename = function() {
  #     paste0(input$user_plottype, "_", input$user_stroketype, "_", input$user_modtype, ".png", sep = "")
  #   },
  #   content = function(file) {
  #     plotly_IMAGE(man_plot(), format = "png") #, out_file = filename)
  #     # print(man_plot())
  #     # dev.off()
  #   }
  # )

  
  # output$TableInfo <- renderDataTable({
  #   # input$update_table
  #   # validate(need(isolate(input$user_rsID != ""), "Please select table parameters."))
  #   annotdb <- dbConnect(SQLite(), dbname = "annot.sqlite")
  #   marker <- dbGetQuery(annotdb, paste0("select POS from ", req(input$user_stroketype), "_annot_", input$user_modtype, " where ID = '", req(input$user_rsID), "'"))[,1]
  #   max <- marker + 50
  #   min <- marker - 50
  #   marker_chrom <- dbGetQuery(annotdb, paste0("select CHROM from ", req(input$user_stroketype), "_annot_", input$user_modtype, " where ID = '", req(input$user_rsID), "'"))[1,1]
  #   inforange <- dbGetQuery(annotdb, paste0("SELECT * FROM ", req(input$user_stroketype), "_annot_", input$user_modtype, " WHERE CHROM = ",
  #                                           marker_chrom, " AND POS > ", min, " AND POS < ", max))
  #   dbDisconnect(annotdb) ## Disconnect when done
  #   DT::datatable(inforange, options=list(columnDefs = list(list(visible=FALSE, targets=c(6:15, 22)))))
  # })
  
  
}


