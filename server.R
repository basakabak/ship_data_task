#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#sudo su - -c "R -e \"install.packages('visNetwork', repos='https://cran.rstudio.com/')\""

options("scipen"=100, "digits"=4)

library(shiny)
library(dplyr)
library(maftools)
library(g3viz)
library(stringr)
library(data.table)
library(tidyr)
library(igraph)
library(plotly)
library(DT)
library(survival)
library(survminer)
library(purrr)
library(TCGAbiolinks)
library(visNetwork)
library(networkD3)
library(cicerone)

#dbCon <- org.Hs.eg_dbconn()
# write your SQL query
#sqlQuery <- 'SELECT * FROM ensembl, gene_info, alias WHERE ensembl._id == gene_info._id AND alias._id == gene_info._id;'

# execute the query on the database
#aliasSymbol <- dbGetQuery(dbCon, sqlQuery)
#aliasSymbol2 <- aliasSymbol[,c(1,2,5,7)]
#aliasSymbol2 <- aliasSymbol2 %>% group_by(symbol) %>% mutate(alias_symbol = paste(unique(unlist(strsplit(paste(alias_symbol,collapse=","), ","))),collapse=','))
#aliasSymbol2 <- aliasSymbol2 %>% group_by(symbol) %>% mutate(ensembl_id = paste(unique(unlist(strsplit(paste(ensembl_id,collapse=","), ","))),collapse=','))

#aliasSymbol2 <- aliasSymbol2  %>% rowwise() %>% mutate(value = paste(ensembl_id,alias_symbol,sep=","))
#aliasSymbol2 <- aliasSymbol2 %>% rowwise() %>% mutate(value2= paste0("c(\"",paste(strsplit(value,",")[[1]],collapse="\",\""),"\")"))

#aliasSymbol2 <- aliasSymbol2[,c(3,5,6)]
#aliasSymbol2 <- unique(aliasSymbol2)

#write.table(aliasSymbol2, file="data/gene_info.tsv", sep="\t",row.names=FALSE,quote = FALSE)
aliasSymbol2 <- read.delim("data/gene_info.tsv", sep="\t",check.names = FALSE,stringsAsFactors = FALSE,header = T,quote = "")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

        tasks <-  reactiveValues(
          
        )
        
        observeEvent(input$rows, {
          
          output$clip <- renderUI({
            rclipButton("clipbtn", "", paste(unlist(unname(lapply(isolate(reactiveValuesToList(tasks)), `[[`, 1))),collapse=", "), icon("copy"))
            
          })
        })
        
        observeEvent(input$rows, {output$genes <- renderUI({
          #total <- ""
          tasks2 <- unlist(unname(lapply(isolate(reactiveValuesToList(tasks)), `[[`, 1)))
          ul <- tags$ul()
          ul$children <- lapply(tasks, function(el) {
            #total <- paste(el$text,total,sep = ", ")
            #notificationItem(text = el$text,icon=shiny::icon("dna"),inputId=el$id)
            if(!is.null(el$text)){
              #x <- paste0("<li id=\"",el$text,"\"onclick=\"Shiny.setInputValue(\'deleteItem\',\'",el$text,"\')\"><i class=\"fa fa-dna\"></i>",el$text,"</li>")
              #HTML(x)
              #☻print(length(tasks))
              
              shiny::tags$li(tags$input(type = "checkbox",class="myCheckbox",id=el$id),el$text)
              
              #runjs(paste0('
              #    var element1 = document.getElementById(\'',el$id,'\');
              #    element1.setAttribute("onclick",Shiny.setInputValue(\'deleteItem\',\'',el$id,'\'));
              
              # '))
            } 
            
            #print(total)
          })
          
        })
        #print("TP53" %in% unlist(unname(lapply(isolate(reactiveValuesToList(tasks)), `[[`, 1))))
        })
        
        
        observeEvent(input$rows, {
          runjs("$(\"#clipbtn\").attr('title', 'title2');")
          #print(input$rows)
          tasks2 <- unlist(unname(lapply(isolate(reactiveValuesToList(tasks)), `[[`, 1)))
          print(tasks2)
          if(length(tasks2) <5){
            if(!is.null(tasks[[paste0("id",input$rows)]])){
              # sendSweetAlert(
              #   session = session,
              #   title = NULL,
              #   text = tags$div(tags$b(paste0(input$rows," is already added to My Genes"))),
              #   html = TRUE,
              #   width = "200px"
              # )
              toast(
                title = sprintf("%s is already added to My Genes", input$rows),
                options = list(
                  class = "bg-fuchsia",
                  autohide = TRUE,
                  position = "topRight"
                )
              )
            }else {
              tasks[[paste0("id", input$rows)]] <- list(
                id = input$rows,
                text = input$rows
              )
              # sendSweetAlert(
              #   session = session,
              #   title = NULL,
              #   text = tags$div(tags$b(paste0(input$rows," added to My Genes"))),
              #   html = TRUE,
              #   btn_labels = NA,
              #   width = "200px"
              # )
              toast(
                title = sprintf("%s added to My Genes", input$rows),
                options = list(
                  class = "bg-fuchsia",
                  autohide = TRUE,
                  position = "topRight"
                )
              )
            }
            
          } else {
            # sendSweetAlert(
            #   session = session,
            #   title = NULL,
            #   text = tags$div(tags$b("Max size is 5. Can not add more than 5 genes to the My Genes")),
            #   html = TRUE,
            #   width = "200px"
            # )
            toast(
              title = sprintf("Max size is 5. Can not add more than 5 genes to the My Genes"),
              options = list(
                class = "bg-fuchsia",
                autohide = TRUE,
                position = "topRight"
              )
            )
          }
          
          #shinyalert::shinyalert("Oops!", "Something went wrong.", type = "error")
          #print(input$sumtable)
        })
        
        observeEvent(input$deleteItem, {
          #tasks2 <- unlist(unname(lapply(isolate(reactiveValuesToList(tasks)), `[[`, 1)))
          lapply(input$deleteItem, function(item) {
            print(item)
            tasks[[paste0("id",item)]] <- NULL
            
            runjs(paste0('$("#',item,'").remove();console.log("',item,' deleted");'))
          })
          
          #print(length(tasks2))
          #tasks2 <- tasks2[tasks2 != input$deleteItem]; 
          #print(tasks[[paste0("id",input$deleteItem)]])
          
        })
        
        # app button --------------------------------------------------------------
        output$btnVal <- renderText(input$myAppButton)
        observeEvent(input$myAppButton, {
            showModal(modalDialog("Thanks for clicking me!", easyClose = TRUE))
        })
        
        # alerts ------------------------------------------------------------------
        
        observeEvent(input$show_alert, {
            print("created")
            createAlert(
                id = "alert_anchor",
                options = list(
                    title = "Be Careful!",
                    status = "danger",
                    closable = TRUE,
                    width = 12,
                    content = "Danger alert preview. This alert is dismissable. 
          A wonderful serenity has taken possession of my entire soul, 
          like these sweet mornings of spring which 
          I enjoy with my whole heart."
                )
            )
        })

        
        observeEvent(input$reload, {
            showNotification("Yeah!", duration = 1, type = "default")
        })
        
        ###############Home Part###############
        
        projects <- read.delim("data/home.tsv", sep="\t",check.names = FALSE,stringsAsFactors = FALSE,header = T)
        #projects <- subset(projects,grepl("TCGA",`Cohort`))
        
        output$projects_chart <- renderHighchart({
                projects %>%
                        hchart(
                                "pie", hcaes(x = `Cancer type`, y = Samples),
                                name = "Samples",allowPointSelect=T,
                        ) %>%
                        hc_plotOptions(series = list(events = list(click = JS("function(event) {console.log(event);Shiny.onInputChange('datacat', event.point.name);}"),
                                                                   render = JS("function(event) {Shiny.onInputChange('datacat', 'Lung adenocarcinoma');$('#projects_chart').highcharts().series[0].points[1].firePointEvent('click');}")))) %>%
                        hc_exporting(enabled = TRUE, filename = "TCGAnalyzeR",buttons=list(contextButton=list(menuItems=list("downloadPNG"))))
        })
        
        output$project_table <- DT::renderDataTable({
                
                DT::datatable(projects,escape = FALSE,rownames = F,extensions = 'Buttons',selection = 'none',
                              options = list(dom = 'Bfrtip', buttons = list(list(
                                      extend = 'csv',
                                      extension='.tsv',
                                      fieldSeparator="\t",
                                      filename="TCGAnalyzeR",
                                      className= 'fa fa-download fa-2x btn btn-default action-button shiny-bound-input',
                                      text = ''
                              )),#columnDefs = list(list(className = 'dt-center', targets = "_all")), 
                              autoWidth = F, scrollX = T),class = "row-border hover") %>%
                        DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='60%',`font-size` = '14px')
                
        })
        
        observeEvent(input$datacat, {
                output$projectDetail <- DT::renderDataTable({
                        f_projects <- projects[projects$`Cancer type` == input$datacat,]
                        f_projects <- data.frame(t(f_projects))
                        f_projects$A <- rownames(f_projects)
                        f_projects <- f_projects[,c(2,1)]
                        DT::datatable(f_projects,escape = FALSE,rownames = F,style='bootstrap',extensions = 'Buttons',selection = 'none',callback = JS('$("#projectDetail thead").remove()'),
                                      options = list(searching= FALSE, paging= FALSE, info= FALSE,ordering=F,#dom = 'Bfrtip', buttons = c('colvis'),pageLength=10,
                                                     columnDefs = list(list(className = 'dt-center', targets = "_all"),list(targets=list(1),className= 'bolded')), autoWidth = F, scrollX = T),class = "table-bordered table-condensed") %>%
                                DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='80%',`font-size` = '14px') %>%
                                DT::formatStyle(1, fontWeight = 'bold')
                        
                })
        })
        
        ###############Onco Plot Part###############
        
        output$oncoplot_ui <- renderUI({
        
                column(width=12,
                       box(id="oncoplot_div",title = "Onco Plot",width = 12, status = "primary",solidHeader = F,#closable=T,
                    sidebar = bs4CardSidebar(id="mysidebar",width=25,icon = HTML('<i class="fa fa-filter">Filter</i>'),
                                             numericInput("onco_top", "Number of Top Genes:", 10, min = 5, max = 50),
                                             pickerInput(
                                                     inputId = "onco_gene",
                                                     label = "Genes",
                                                     choices = c(),
                                                     multiple = T,
                                                     width = '300px',
                                                     options = list(`live-search` = TRUE,
                                                                    `live-search-normalize` = TRUE,
                                                                    `live-searc-placeholder`="Enter gene name",
                                                                    `size`=5,
                                                                    `actions-box` = TRUE,
                                                                    `max-options`=7)
                                                     
                                             ),
                                             #textInput.typeahead(id="onco_gene",
                                             #                    placeholder="Enter gene name",
                                             #                    local=data.frame(name=c(unique(aliasSymbol$symbol))),
                                             #                    valueKey = "name",
                                             #                    tokens=c(1:length(unique(aliasSymbol$symbol))),
                                             #                    template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")),
                                             br(),
                                             actionButton("onco_button", "Plot"),
                                             fileInput("file1", "Choose File",accept = c(
                                                     "text/tsv",
                                                     "text/tab-separated-values,text/plain",
                                                     ".tsv")),
                                             actionButton("onco_reset", "Reset"),
                    ),
                    downloadButton('downloadPlot', 'Download Plot'),
                    plotOutput("oncoplot") %>% withSpinner()
                    
                    #uiOutput('oncoplot_ui')
                ),
                box(title = "Table",width = 12,status = "primary",solidHeader =F,
                    withSpinner(DT::dataTableOutput("onco_table",width = "100%"))))
        })
        
        maf <- eventReactive(c(input$lollipop_button,input$onco_button,
                               req(input$mdata_snv),req(input$pairedTotal_snv)), {
                                       #print(input$pairedTotal)
                                       # reactive is not needed if no user input is used for creating this data
                                       #mdata <- read.delim(paste0("data/",input$mdata,"/",input$mdata,"_paired_",input$datacategory,"_results_summary_table.txt"), sep = "\t",check.names = FALSE,stringsAsFactors = FALSE)
                                       
                                       load(paste0("data/",tolower(input$pairedTotal_snv),"/",input$mdata_snv,"/SNV/",input$mdata_snv, "_",tolower(input$pairedTotal_snv),"_maf.RData"))
                                       aliasSymbol3 <- subset(aliasSymbol2,symbol %in% maf@data$Hugo_Symbol)
                                       updatePickerInput(session = session, inputId = "onco_gene",
                                                   choices = unique(aliasSymbol3$symbol),
                                                   choicesOpt = list(
                                                     tokens = eval(parse(text= paste0("list(",paste(aliasSymbol3$value2,collapse=","),")")))
                                                   ))
                                       maf
                               })
        
        onco_top_handle <- eventReactive(c(input$onco_button,input$onco_reset,
                                           req(input$mdata_snv),req(input$pairedTotal_snv)), {
                                                   
                                                   input$onco_top
                                           })
        
        onco_gene_handle <- eventReactive(c(input$onco_button,input$onco_reset,
                                            req(input$mdata_snv),req(input$pairedTotal_snv)), {
                                                    
                                                    input$onco_gene
                                            })
        
        rv <- reactiveValues(data = NULL)
        
        observe({
                req(input$file1)
                rv$data <- read.csv(input$file1$datapath, header = F)
        })
        
        observeEvent(input$onco_reset, {
                rv$data <- NULL
                reset('file1')
        })
        
        output$oncoplot <- renderPlot({
                print(onco_gene_handle())
                print(unique(c(head(maf()@gene.summary$Hugo_Symbol,onco_top_handle()),onco_gene_handle())))
                #maftools::oncoplot(maf = maf(), top = strsplit(onco_handle(),",")[[1]][1],removeNonMutated = TRUE,genes = c(head(maf()@gene.summary$Hugo_Symbol,strsplit(onco_handle(),",")[[1]][1]),strsplit(onco_handle(),",")[[1]][2]))
                inFile <- input$file1
                #print(rv$data)
                if (is.null(rv$data)){
                        #print(strsplit(onco_handle(),",")[[1]][2])
                        oncoplot(maf = maf(), top = onco_top_handle(),removeNonMutated = F,genes = unique(c(head(maf()@gene.summary$Hugo_Symbol,onco_top_handle()),onco_gene_handle())))
                } else {
                        common_cases_maf1 <- read.csv(inFile$datapath, header = F)
                        
                        cases_maf <- maf()@clinical.data[["Tumor_Sample_Barcode"]]
                        common_cases_maf2 <- cases_maf[unique(substring(cases_maf, 1, 12)) %in% as.list(common_cases_maf1$V1)]
                        common_cases_maf2 <- as.character(unlist(common_cases_maf2))
                        maf1 = subsetMaf(maf(), tsb = common_cases_maf2, mafObj = TRUE)
                        oncoplot(maf = maf1,top=onco_top_handle(),removeNonMutated = TRUE)
                }
        },height = function(){25*as.integer(onco_top_handle())},width = "auto")
        
        output$onco_table <- DT::renderDataTable({
                inFile <- input$file1
                if (is.null(rv$data)){
                        mdata <- maf()@gene.summary %>% 
                                dplyr::rowwise() %>%
                                mutate(Hugo_Symbol = paste0('<a href="#" onclick="Shiny.setInputValue(\'rows\',\'',Hugo_Symbol,'\')" id="addtomylist" class="',Hugo_Symbol,'">',Hugo_Symbol,'</a>'))
                        
                        DT::datatable(mdata,escape = FALSE,rownames = F,extensions = 'Buttons',selection = 'none',
                                      options = list(dom = 'Bfrtip', buttons = list(list(
                                              extend = 'csv',
                                              extension='.tsv',
                                              fieldSeparator="\t",
                                              filename="TCGAnalyzeR_OncoTable",
                                              className= 'fa fa-download fa-2x btn btn-default action-button shiny-bound-input',
                                              text = ''
                                      )),#columnDefs = list(list(className = 'dt-center', targets = "_all")), 
                                      autoWidth = F, scrollX = T,
                                      searchHighlight = TRUE,scrollX = T),class = "row-border hover") %>%
                                DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='60%',`font-size` = '14px')
                } else {
                        common_cases_maf1 <- read.csv(inFile$datapath, header = F)
                        
                        cases_maf <- maf()@clinical.data[["Tumor_Sample_Barcode"]]
                        common_cases_maf2 <- cases_maf[unique(substring(cases_maf, 1, 12)) %in% as.list(common_cases_maf1$V1)]
                        common_cases_maf2 <- as.character(unlist(common_cases_maf2))
                        maf1 = subsetMaf(maf(), tsb = common_cases_maf2, mafObj = TRUE)
                        mdata <- maf1@gene.summary %>% 
                                dplyr::rowwise() %>%
                                mutate(Hugo_Symbol = paste0('<a href="#" onclick="Shiny.setInputValue(\'rows\',\'',Hugo_Symbol,'\')" id="addtomylist" class="',Hugo_Symbol,'">',Hugo_Symbol,'</a>'))
                        
                        DT::datatable(mdata,escape = FALSE,rownames = F,extensions = 'Buttons',selection = 'none',
                                      options = list(dom = 'Bfrtip', buttons = list(list(
                                              extend = 'csv',
                                              extension='.tsv',
                                              fieldSeparator="\t",
                                              filename="TCGAnalyzeR",
                                              className= 'fa fa-download fa-2x btn btn-default action-button shiny-bound-input',
                                              text = ''
                                      )),#columnDefs = list(list(className = 'dt-center', targets = "_all")), 
                                      autoWidth = F, scrollX = T,
                                      searchHighlight = TRUE,scrollX = T),class = "row-border hover") %>%
                                DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='60%',`font-size` = '14px')
                }
                
        })
        
        output$downloadPlot <- downloadHandler(
                filename =  function() {
                        "OncoPlot"
                },
                # content is a function with argument file. content writes the plot to the device
                content = function(file) {
                        
                        png(file) # open the png device
                        if (is.null(rv$data)){
                                oncoplot(maf = maf(), top = onco_top_handle(),removeNonMutated = TRUE,genes = c(unique(head(maf()@gene.summary$Hugo_Symbol,onco_top_handle()),onco_gene_handle())))
                                
                        } else {
                                common_cases_maf1 <- rv$data
                                print(common_cases_maf1)
                                cases_maf <- maf()@clinical.data[["Tumor_Sample_Barcode"]]
                                common_cases_maf2 <- cases_maf[unique(substring(cases_maf, 1, 12)) %in% as.list(common_cases_maf1$V1)]
                                common_cases_maf2 <- as.character(unlist(common_cases_maf2))
                                maf1 = subsetMaf(maf(), tsb = common_cases_maf2, mafObj = TRUE)
                                oncoplot(maf = maf1,top=onco_top_handle(),removeNonMutated = TRUE)
                        }
                        dev.off()  # turn the device off
                        
                } 
        )

        ###############Lollipop Part###############
        
        gene_lollipop <- eventReactive(c(input$lollipop_button,
                                         req(input$mdata_snv),req(input$pairedTotal_snv)),{
                                                 
                                                 c(input$lollipop_gene)
                                         })
        
        output$lollipopplot_ui <- renderUI({
                
                fluidRow(
                        column(class="mydiv",
                               width = 12,
                               box(title = "Lollipop Plot",width = 12, status = "primary",solidHeader =F,
                                   sidebar = bs4CardSidebar(id="lollipopsidebar4",width=25,class="ortaaak",icon = HTML('<i class="fa fa-filter">Filter</i>'),
                                                            pickerInput(
                                                                    inputId = "lollipop_gene",
                                                                    label = "Genes",
                                                                    selected = c("TTN","TP53"),
                                                                    choices = c("TTN","TP53"),
                                                                    multiple = T,
                                                                    width = '300px',
                                                                    options = list(`live-search` = TRUE,
                                                                                   `live-search-normalize` = TRUE,
                                                                                   `live-searc-placeholder`="Enter gene name",
                                                                                   `size`=5,
                                                                                   `actions-box` = TRUE,
                                                                                   `max-options`=7)
                                                            ),
                                                            br(),
                                                            actionButton("lollipop_button", "Plot")
                                   ),
                                   
                                   
                                   uiOutput("lollipop_tabs")
                                   
                               ),
                               box(title = "Table",width = 12,status = "primary",solidHeader =F,
                                   withSpinner(uiOutput("lollipop_tables") 
                                   ))))
                
        })
        
        
        observeEvent(gene_lollipop(),{
                aa=maf()@data
                
                updatePickerInput(session = session, inputId = "lollipop_gene",
                                  selected = c("TTN","TP53"),choices = unique(aa$Hugo_Symbol))
                
        })
        
        observeEvent(gene_lollipop(),{
                
                output$lollipop_tabs <- renderUI({
                        
                        do.call(bs4Dash::tabsetPanel, c(id='t',lapply(1:length(gene_lollipop()), function(i) {
                                tabPanel(
                                        title=gene_lollipop()[i], 
                                        g3LollipopOutput(paste0("lollipopplot_",gene_lollipop()[i])) %>% withSpinner()
                                )
                        })))
                        
                })
        })
        
        observeEvent(gene_lollipop(),{
                
                output$lollipop_tables <- renderUI({
                        
                        do.call(bs4Dash::tabsetPanel, c(id='t',lapply(1:length(gene_lollipop()), function(i) {
                                tabPanel(
                                        title=gene_lollipop()[i], 
                                        DT::dataTableOutput(paste0("lollipoptable_",gene_lollipop()[i]),width = "100%") %>% withSpinner()
                                )
                        })))
                        
                })
        })
        
        
        observeEvent(gene_lollipop(),{
                #print(input$lollipop)
                lapply(1:length(gene_lollipop()), function(a) {
                        
                        output[[paste0("lollipopplot_",gene_lollipop()[a])]] <- renderG3Lollipop({
                                
                                aa=maf()@data
                                aa <- mutate(aa,AA_Position=str_extract(HGVSp_Short, "[[:digit:]]+"))
                                aa <- mutate(aa,Mutation_Class=Variant_Classification)
                                
                                aa <- aa[,c("Hugo_Symbol", "Chromosome","Start_Position","End_Position","Strand","Variant_Classification",
                                            "Variant_Type", "Reference_Allele","Tumor_Seq_Allele1","Tumor_Seq_Allele2","HGVSp",
                                            "HGVSp_Short","COSMIC","Mutation_Class","AA_Position")]
                                
                                plot.options <- g3Lollipop.options(chart.width = "1500",
                                                                   chart.type = "circle",
                                                                   lollipop.track.height = 260,
                                                                   lollipop.track.background = "transparent",
                                                                   lollipop.pop.max.size = 4,
                                                                   lollipop.pop.min.size = 4,
                                                                   # set larger than lollipop.pop.max.size to turn off pop info
                                                                   lollipop.pop.info.limit = 4.1,
                                                                   # y-axis label
                                                                   lollipop.line.color = "grey",
                                                                   lollipop.line.width = 0.5,
                                                                   lollipop.circle.color = "grey",
                                                                   lollipop.circle.width = 0.5,
                                                                   lollipop.color.scheme = "bottlerocket2",
                                                                   y.axis.line.color = "transparent",
                                                                   # domain annotation bar
                                                                   anno.bar.fill = "#969696",
                                                                   anno.bar.margin = list(top = 4, bottom = 8),
                                                                   # domain track options
                                                                   domain.margin = list(top = 2, bottom = 6),
                                                                   domain.color.scheme = "bottlerocket1",
                                                                   domain.text.font = "normal 12px Arial",
                                                                   domain.text.color = "white",
                                                                   # highlight text
                                                                   highlight.text.angle = 45,
                                                                   # disable brush
                                                                   brush = FALSE,
                                                                   # disable legend
                                                                   legend = TRUE)
                                
                                
                                g3Lollipop(aa,
                                           gene.symbol = gene_lollipop()[a],
                                           btn.style = "blue", # gray-style chart download buttons
                                           plot.options = plot.options,
                                           save.svg.btn =F,
                                           output.filename = paste0("TCGAnalyzeR_LollipopPlot_",gene_lollipop()[a]))
                                
                                
                        })
                        
                })
        })
        
        
        
        observeEvent(gene_lollipop(),{
                #print(input$lollipop)
                lapply(1:length(gene_lollipop()), function(a) {
                        
                        output[[paste0("lollipoptable_",gene_lollipop()[a])]] <- DT::renderDataTable({
                                
                                mdata <- subset(maf()@data,Hugo_Symbol == gene_lollipop()[a])
                                mdata <- mdata %>% 
                                        dplyr::rowwise() %>%
                                        mutate(Hugo_Symbol = paste0('<a href="#" onclick="Shiny.setInputValue(\'rows\',\'',Hugo_Symbol,'\')" id="addtomylist" class="',Hugo_Symbol,'">',Hugo_Symbol,'</a>'))
                                
                                #DT::datatable(mdata,escape = FALSE,rownames = F,filter = 'none', options = list(
                                #  columnDefs = list(list(className = 'dt-center', targets = "_all")), autoWidth = F, scrollX = T),selection = 'none') %>%
                                #  DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='60%',`font-size` = '12px')
                                
                                DT::datatable(mdata,escape = FALSE,rownames = F,extensions = 'Buttons',selection = 'none',
                                              options = list(dom = 'Bfrtip', buttons = list(list(
                                                      extend = 'csv',
                                                      extension='.tsv',
                                                      fieldSeparator="\t",
                                                      filename=paste0("TCGAnalyzeR_LollipopTable_",gene_lollipop()[a]),
                                                      className= 'fa fa-download fa-2x btn btn-default action-button shiny-bound-input',
                                                      text = ''
                                              )),#columnDefs = list(list(className = 'dt-center', targets = "_all")), 
                                              autoWidth = F, scrollX = T,
                                              searchHighlight = TRUE,scrollX = T),class = "row-border hover") %>%
                                        DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='60%',`font-size` = '14px')
                                
                                
                        })
                        
                })
        })
        
        
        ###############Cancer Driver Part###############
        
        output$cancerdriverplot_ui <- renderUI({
                fluidRow(
                        column(class="mydiv",
                               width = 12,
                               box(title = "Cancer Driver Genes",width = 12,status = "primary",solidHeader =F,
                                   sidebar = bs4CardSidebar(id="lollipopsidebar2",width=25,icon = HTML('<i class="fa fa-filter">Filter</i>'),
                                                            numericInput("cancer_top", "Number of Top Genes:", 10, min = 1, max = 20),
                                                            br(),
                                                            sliderInput("cancer_threshold", "Score threshold",
                                                                        min = 0, max = 1, value = 0.4, step = 0.1),
                                                            br(),
                                                            #textInput("cancer_genee", "Gene name", value = "Taxol", placeholder = "Enter gene name"),
                                                            #textInput.typeahead(id="cancer_gene",
                                                            #                    placeholder="Enter gene name",
                                                            #                    local=data.frame(name=c(unique(aliasSymbol$symbol))),
                                                            #                    valueKey = "name",
                                                            #                   tokens=c(1:length(unique(aliasSymbol$symbol))),
                                                            #                    template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")),
                                                            pickerInput(
                                                                    inputId = "cancer_gene",
                                                                    label = "Genes",
                                                                    selected = NULL,
                                                                    choices = c("TP53","TTN"),
                                                                    multiple = T,
                                                                    width = '300px',
                                                                    options = list(`live-search` = TRUE,
                                                                                   `live-search-normalize` = TRUE,
                                                                                   `live-searc-placeholder`="Enter gene name",
                                                                                   `size`=5,
                                                                                   `actions-box` = TRUE,
                                                                                   `max-options`=7)
                                                            ),
                                                            br(),
                                                            actionButton("cancer_button", "Plot")
                                   ),
                                   shiny::tags$p(shiny::tags$a("Oncogene score: ",href="https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-015-0555-7#Fig6",style="color:black;font-weight:bold;"),"The percent of the missense mutations clustering at recurrent positions across different tumour samples using the  gene-specific background mutation rate",style="color:black;"),
                                   shiny::tags$p(shiny::tags$a("Tumor suppressor gene score: " ,href="https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-015-0555-7#Fig6",style="color:black;font-weight:bold;"),"The percent of the number of protein-truncating mutations throughout the entire gene length",style="color:black;"),
                                   highchartOutput("cancerdriverplot") %>% withSpinner()
                                   #uiOutput('cancerdriverplot_ui')
                                   
                               ),
                               box(title = "Table",width = 12,status = "primary",solidHeader =F,
                                   withSpinner(DT::dataTableOutput("cancer_table",width = "100%")))
                        ))
        })
        
        mData <- eventReactive(c(input$cancer_button,
                                 req(input$mdata_snv),req(input$pairedTotal_snv)), {
                                         # reactive is not needed if no user input is used for creating this data
                                         mData <- read.table(paste0("data/",tolower(input$pairedTotal_snv),"/",input$mdata_snv,"/SNV/",input$mdata_snv, "_",tolower(input$pairedTotal_snv),"_SomInaClust_results.txt"), check.names = FALSE,stringsAsFactors = FALSE)
                                         mData
                                         #load(paste0("data/",input$mdata,"/",input$mdata, "_paired_maf.RData"))
                                 })
        
        cancer_handle <- eventReactive(c(input$cancer_button,
                                         req(input$mdata_snv),req(input$pairedTotal_snv)), {
                                                 
                                                 c(input$cancer_top,as.double(input$cancer_threshold))
                                                 
                                         })
        
        cancer_handle2 <- eventReactive(c(input$cancer_button,
                                          req(input$mdata_snv),req(input$pairedTotal_snv)), {
                                                  
                                                  input$cancer_gene
                                                  
                                          })
        
        observeEvent(cancer_handle(),{
                mydata1 <- mData()
                
                updatePickerInput(session = session, inputId = "cancer_gene",
                                  selected = cancer_handle2(),choices = unique(rownames(mydata1)))
                
                
        })
        
        output$cancerdriverplot <- renderHighchart({
                
                
                mydata1 <- mData()
                print(cancer_handle2())
                print(length(cancer_handle2()))
                if(length(cancer_handle2()) == 0){
                        mydata1 <- head(mydata1,cancer_handle()[1])
                        mydata <- mydata1
                        #print("a")
                }else{
                        mydata2 <- subset(mydata1,rownames(mydata1) %in% cancer_handle2())
                        mydata1 <- head(mydata1,cancer_handle()[1])
                        mydata <- rbind(mydata1,mydata2)
                        mydata <- unique(mydata)
                        #print("b")
                }
                
                mydata$rname <- rownames(mydata)
                mydata <- mydata %>%
                        rowwise() %>%
                        mutate(OG_score=OG_score*-1)
                
                highchart() %>%
                        hc_add_series(mydata$OG_score, type = "bar", name = "OG Score",color="red") %>%
                        hc_add_series(mydata$TSG_score, type = "bar", name = "TSG Score",color="blue") %>%
                        hc_xAxis(categories = mydata$rname) %>%
                        hc_yAxis(plotLines=list(
                                list(color = "red",
                                     width = 2,
                                     value = as.double(cancer_handle()[2]),
                                     zIndex= 4,
                                     dashStyle="Dash",
                                     label=list(text=as.double(cancer_handle()[2]))#,style=list(display="none"),rotation=90),
                                     #events=list(mouseover=JS("function (e) {this.label.element.style.display='block';}"),
                                     #             mouseout=JS("function (e) {this.label.element.style.display='none';}"))
                                ),
                                list(color = "red",
                                     width = 2,
                                     value = -1*as.double(cancer_handle()[2]),
                                     dashStyle="Dash",
                                     zIndex= 4,
                                     label=list(text=as.double(cancer_handle()[2])))
                        )) %>%
                        hc_plotOptions(series = list(events = list(click = JS("function(event) {console.log(event);Shiny.onInputChange('rows', event.point.category);}")),stacking = "normal")) %>%
                        hc_exporting(enabled = TRUE, filename = "TCGAnalyzeR_CancerDriverPlot",buttons=list(contextButton=list(menuItems=list("downloadPNG"))))
                
                
        })
        
        output$cancer_table <- DT::renderDataTable({
                mdata <- mData()
                mdata$Gene <- rownames(mdata)
                mdata <- mdata[,c(ncol(mdata),1:ncol(mdata)-1)]
                mdata <- mdata %>% 
                        dplyr::rowwise() %>%
                        mutate(Gene = ifelse(!is.na(Gene),paste0('<a href="#" onclick="Shiny.setInputValue(\'rows\',\'',na.omit(Gene),'\')" id="addtomylist" class="',na.omit(Gene),'">',na.omit(Gene),'</a>'),""))
                #DT::datatable(mdata,escape = FALSE,rownames = F,filter = 'none', options = list(
                #  columnDefs = list(list(className = 'dt-center', targets = "_all")), autoWidth = F, scrollX = T),selection = 'none') %>%
                #  DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='60%',`font-size` = '14px')
                
                DT::datatable(mdata,escape = FALSE,rownames = F,extensions = 'Buttons',selection = 'none',
                              options = list(dom = 'Bfrtip', buttons = list(list(
                                      extend = 'csv',
                                      extension='.tsv',
                                      fieldSeparator="\t",
                                      filename="TCGAnalyzeR_CancerDriverGenesTable",
                                      className= 'fa fa-download fa-2x btn btn-default action-button shiny-bound-input',
                                      text = ''
                              )),#columnDefs = list(list(className = 'dt-center', targets = "_all")), 
                              autoWidth = F, scrollX = T,
                              searchHighlight = TRUE,scrollX = T),class = "row-border hover") %>%
                        DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='60%',`font-size` = '14px')
        })
        ###############Cnv Plot Part###############
        
        output$cnvplot_ui <- renderUI({
                fluidRow(
                        column(class="mydiv",
                               width = 12,
                               box(title = "CNV Plot",width = 12, status = "primary",solidHeader =F,
                                   highchartOutput("cnv_plot") %>% withSpinner()
                                   
                               ),
                               box(title = "Table",width = 12,status = "primary",solidHeader =F,
                                   withSpinner(DT::dataTableOutput("cnv_table",width = "100%")))
                        ))
        })
        
        cnv_data <- eventReactive(c(input$cnv_button,
                                    req(input$mdata_cnv),req(input$pairedTotal_cnv)), {
                                            # reactive is not needed if no user input is used for creating this data
                                            #mdata <- read.delim(paste0("data/",input$mdata,"/",input$mdata,"_paired_",input$datacategory,"_results_summary_table.txt"), sep = "\t",check.names = FALSE,stringsAsFactors = FALSE)
                                            
                                            load(paste0("data/",tolower(input$pairedTotal_cnv),"/",input$mdata_cnv,"/CNV/",input$mdata_cnv, "_",tolower(input$pairedTotal_cnv),"_CNV_results_genes.rda"))
                                            load(paste0("data/",tolower(input$pairedTotal_cnv),"/",input$mdata_cnv,"/CNV/",input$mdata_cnv, "_",tolower(input$pairedTotal_cnv),"_CNV_results.rda"))
                                            #names(AmpDel_genes)[names(AmpDel_genes) == "q-value"] <- "qval"
                                            agg=aggregate(AmpDel_genes$GeneSymbol,list(AmpDel_genes$AberrantRegion),  paste, collapse=",")
                                            
                                            colnames(agg) <- c("AberrantRegion","Genes")
                                            agg$AberrantRegion <- as.character(agg$AberrantRegion)
                                            
                                            agg <- agg %>% 
                                                    rowwise() %>% 
                                                    mutate(`Chromosome` = unlist(strsplit(AberrantRegion,":"))[1],
                                                           `Region Start [bp]`=unlist(strsplit(unlist(strsplit(AberrantRegion,":"))[2],"-"))[1],
                                                           `Region End [bp]`=unlist(strsplit(unlist(strsplit(AberrantRegion,":"))[2],"-"))[2])
                                            agg$Chromosome <- as.integer(agg$Chromosome)
                                            agg$`Region Start [bp]` <- as.integer(agg$`Region Start [bp]`)
                                            agg$`Region End [bp]` <- as.integer(agg$`Region End [bp]`)
                                            
                                            
                                            mdata <- setDT(data.table(RecCNV))[ i = data.table(agg), Genes := i.Genes,
                                                                             on = .(`Chromosome`,`Region Start [bp]`,`Region End [bp]`)]
                                            
                                            
                                            mdata
                                    })
        
        output$cnv_plot <- renderHighchart({
                mdata <- cnv_data()
                
                
                mdata <- mdata %>% 
                        dplyr::rowwise() %>%
                        mutate(`Aberration Kind` = ifelse(`Aberration Kind` == 0,"Del","Amp"))
                
                mdata <- mdata %>%
                        rowwise() %>%
                        mutate(score=ifelse(`Aberration Kind`=="Del",score*-1,score))
                
                names(mdata)[names(mdata) == "q-value"] <- "qval"
                
                qq=mdata %>%
                        group_nest(Chromosome,`Aberration Kind`) %>%
                        mutate(
                                id = paste0(Chromosome,`Aberration Kind`),
                                name=`Aberration Kind`,
                                type = "column",
                                data = map(data, mutate, y = score,color=ifelse(score<0,"blue","red")),
                                data = map(data, list_parse)
                        )
                
                lang <- getOption("highcharter.lang")
                lang$drillUpText <- "< Back to Chromosomes"
                options(highcharter.lang = lang)
                
                highchart() %>%
                        #hc_caption(text='<b>Del</b>',verticalAlign='bottom',align='center',useHTML=T) %>%
                        #hc_subtitle(text='Amp',verticalAlign='top',align='center') %>%
                        hc_chart(events=list(drilldown=JS("function(e) {
        console.log(e.point)
                                          $('#cnv_plot').highcharts().xAxis[0].setTitle({'text':'Chr'+e.point.Chromosome});
                                          $('#cnv_plot').highcharts().yAxis[0].setTitle({'text':'log(pvalue)'});
                                          $('#cnv_plot').highcharts().xAxis[0].update({labels: {
                                              enabled: false 
                                          }})
                                          $('#cnv_plot').highcharts().legend.update({enabled:true})
                                          /*if($('#cnv_plot').highcharts().yAxis[0].min == 0 ){
                                          $('#cnv_plot').highcharts().yAxis[0].update({min:$('#cnv_plot').highcharts().yAxis[0].max * -1})
                                          }
                                          if($('#cnv_plot').highcharts().yAxis[0].max == 0 ){
                                          $('#cnv_plot').highcharts().yAxis[0].update({max:$('#cnv_plot').highcharts().yAxis[0].min * -1})
                                          }*/
                                          }"),
                                             drillup=JS("function(e) {
                                        $('#cnv_plot').highcharts().xAxis[0].setTitle({'text':'Chromosomes'});
                                        $('#cnv_plot').highcharts().yAxis[0].setTitle({'text':'Aberration Count'});
                                        //$('#cnv_plot').highcharts().yAxis[0].update({min:0});
                                        $('#cnv_plot').highcharts().xAxis[0].update({labels: {
                                            enabled: true 
                                        }})
                                        $('#cnv_plot').highcharts().legend.update({enabled:false})
                             }")
                        )
                        ) %>% 
                        hc_title(text='Click on the labels to select the chromosome',verticalAlign='bottom',align='center',style=list(fontSize="12px")) %>%
                        hc_legend(enabled= F) %>%
                        hc_xAxis(type="category",categories=sort(unique(mdata$Chromosome)),title=list(text="Chromosomes"),
                                 labels=list(
                                         events=list(
                                                 click=JS("function() {
              console.log(this)
            }")
                                         )
                                 )) %>%
                        hc_yAxis(title=list(text="Aberration Count"),softMin=ifelse(min(mdata$score)<0 ,min(mdata$score),max(mdata$score)*-1),softMax=ifelse(max(mdata$score)>0, max(mdata$score),min(mdata$score)*-1)) %>% #,min=ifelse(min(mdata$score)<0 ,min(mdata$score),max(smdata$core)*-1),max=ifelse(max(mdata$score)>0, max(mdata$score),min(mdata$score)*-1)
                        hc_tooltip(
                                headerFormat="",
                                pointFormat = "<b>Score {point.score} </b><br><b>q-value {point.qval}</b>", 
                                useHTML = TRUE 
                        ) %>%
                        hc_plotOptions(series = list(dataLabels= list(enabled=F))) %>%
                        hc_add_series(data=as.data.frame(count(mdata,Chromosome, `Aberration Kind`) %>%
                                                                 spread(`Aberration Kind`,n,fill = 0) %>%
                                                                 mutate(n= Amp + Del)),type="column",hcaes(y = `Del`, drilldown = paste0(Chromosome,"Del"),name=Chromosome),name="Chromosomes",showInLegend =T,
                                      tooltip = list(pointFormat = "<b>Chromosomes {point.Chromosome}</b><br><b>Del {point.Del}</b><br><b>Total {point.n}</b>",useHTML = TRUE ),
                                      color="blue") %>%
                        hc_add_series(data=as.data.frame(count(mdata,Chromosome, `Aberration Kind`) %>%
                                                                 spread(`Aberration Kind`,n,fill = 0) %>%
                                                                 mutate(n= Amp + Del)),type="column",hcaes(y = `Amp`, drilldown = paste0(Chromosome,"Amp"),name=Chromosome),name="Chromosomes",showInLegend =T,
                                      tooltip = list(pointFormat = "<b>Chromosomes {point.Chromosome}</b><br><b>Amp {point.Amp}</b><br><b>Total {point.n}</b>",useHTML = TRUE ),
                                      color="red") %>%
                        hc_drilldown(series=list_parse(qq))
                
                
                
        })
        
        
        
        output$cnv_table <- DT::renderDataTable({
                #mdata <- read.delim(paste0("data/",input$mdata,"/",input$mdata,"_gaia.txt"), sep = "\t",check.names = FALSE,stringsAsFactors = FALSE,header = T)
                #mdata <- subset(mdata,`q-value` <= input$qvalThreshold)
                
                mdata <- cnv_data()
                mdata <- as.data.frame(mdata)
                #mdata <- mdata[mdata[,"q-value"] <= cnv_handle(),]
                #mdata <- as.data.frame(mdata)
                mdata <- mdata %>% 
                        dplyr::rowwise() %>%
                        mutate(`Aberration Kind` = ifelse(`Aberration Kind` ==0,"Del","Amp"))
                
                
                DT::datatable(cbind(' ' = '<i class="fas fa-plus-circle"></i>',mdata),selection='none',extensions = 'Buttons', escape = -2,options = list(scrollX=T,autoWidth = F,dom = 'Bfrtip', buttons = list(list(
                        extend = 'csv',
                        extension='.tsv',
                        fieldSeparator="\t",
                        filename="TCGAnalyzeR_CnvTable",
                        className= 'fa fa-download fa-2x btn btn-default action-button shiny-bound-input',
                        text = '',
                        exportOptions=list(columns=c(2:9))
                )),
                columnDefs = list(
                        list(visible = FALSE, targets = c(0,9)),
                        list(orderable = FALSE, className = 'details-control', targets = 1)
                )), callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:#eee; padding: .5em;width:90%;word-break: break-word;\"> Genes: ' +
            d[9]+'</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('<i class=\"fas fa-plus-circle\"></i>');
    } else {
      row.child(format(row.data())).show();
      td.html('<i class=\"fas fa-minus-circle\"></i>');
    }
  });"
                )) #selection = 'none'
        })
        ###############Volcano Plot Part###############
        
        output$volcanoplot_ui <- renderUI({
                fluidRow(
                        column(class="mydiv",
                               width = 12,
                               box(title = "Volcano Plot",width = 12, status = "primary",solidHeader =F,
                                   sidebar = bs4CardSidebar(id="mysidebar4",width=25,icon = HTML('<i class="fa fa-filter">Filter</i>'),
                                                            sliderInput("volcano_p", "adj.P.Val",
                                                                        min = 0, max = 0.001, value = 0.001, step = 0.0001),
                                                            actionButton("volcano_button", "Plot")
                                   ),
                                   highchartOutput("volcanoplot",height="100%") %>% withSpinner()
                                   
                               ),
                               box(title = "Table",width = 12,status = "primary",solidHeader =F,
                                   withSpinner(DT::dataTableOutput("volcano_table",width = "100%")))))
        })
        
        volcano_handle <- eventReactive(c(input$volcano_button,
                                          req(input$mdata_rnaseq),tolower(input$pairedTotal_dea)), {
                                                  
                                                  input$volcano_p
                                                  
                                          })
        
        sign_genes <- eventReactive(c(input$volcano_button,
                                      req(input$mdata_rnaseq),req(input$pairedTotal_dea)), {
                                              # reactive is not needed if no user input is used for creating this data
                                              load(paste0("data/",tolower(input$pairedTotal_dea),"/",input$mdata_rnaseq,"/DEA/",input$mdata_rnaseq, "_",tolower(input$pairedTotal_dea),"_sign_genes.rda"))
                                              sign_genes
                                              #load(paste0("data/",input$mdata,"/",input$mdata, "_paired_maf.RData"))
                                      })
        
        sign_genes_2 <- eventReactive(c(input$volcano_button,
                                        req(input$mdata_rnaseq),req(input$pairedTotal_dea)), {
                                                # reactive is not needed if no user input is used for creating this data
                                                sign_genes_2 <- read.delim(paste0("data/",tolower(input$pairedTotal_dea),"/",input$mdata_rnaseq,"/DEA/",input$mdata_rnaseq, "_sign_degs_with_ids.tsv"), sep="\t")
                                                names(sign_genes_2)[9] <- "SYMBOL"
                                                sign_genes_2
                                                #load(paste0("data/",input$mdata,"/",input$mdata, "_paired_maf.RData"))
                                        })
        
        output$volcanoplot <- renderHighchart({
                #msign_genes <- setDT(sign_genes())[ i = sign_genes_2(), SYMBOL := i.external_gene_name,
                #                                    on = .(ENSEMBL)]
                
                msign_genes <- sign_genes_2()
                msign_genes$expression = ifelse(msign_genes$B >= 3 & abs(msign_genes$logFC) >= 1, 
                                                ifelse(msign_genes$logFC> 1 ,'Up regulated','Down regulated'),
                                                'Unsignificant')
                
                # p <-ggplot(data = msign_genes, 
                #            aes(x = logFC, 
                #                y = B, 
                #                colour=expression,
                #                text = SYMBOL)) +
                #   geom_point(alpha=0.5, size=3.5) +
                #   scale_color_manual(values=c("blue", "black","red"))+
                #   geom_vline(xintercept=c(-1,1),lty=4,col="black",lwd=0.8) +
                #   geom_hline(yintercept = 1.301,lty=4,col="black",lwd=0.8)  +
                #   theme_classic()+
                #   theme(plot.title = element_text(hjust = 0.5), 
                #         legend.position="right", 
                #         legend.title = element_blank())
                # 
                # ggplotly(p,tooltip = c("text"))%>%
                #   style(hoverinfo = "none", traces = 2)
                
                #msign_genes <- as.data.frame(msign_genes)
                #   colnames(msign_genes)[2] <- "EFFECTSIZE"
                #   colnames(msign_genes)[6] <- "P"
                #   msign_genes <- subset(msign_genes,P <= volcano_handle())
                #   manhattanly::volcanoly(x = msign_genes,gene="SYMBOL",snp = "ENSEMBL",text="SYMBOL") %>%
                #       onRender("
                #   function(el) { 
                #     el.on('plotly_click', function(d) { 
                #       Shiny.setInputValue(\'rows\',d.points[0].text.match(/SYMBOL: (.*)<br><br>/)[1]);
                #       //console.log(d.points[0]);
                #       //console.log(d.points[0].text.match(/SYMBOL: (.*)<br><br>/)[1]);
                #     });
                #   }
                # ")
                #dashbioVolcano(dataframe = msign_genes,p="adj.P.Val",effect_size ="logFC",gene="SYMBOL",snp = "ENSEMBL")
                
                hchart(msign_genes, "scatter", hcaes(logFC, B, group = expression, value = SYMBOL),
                       color = c("rgba(0,0,255, 0.3)","rgba(192,192,192,0.3)","rgba(255,0,0,0.3)"), 
                       showInLegend = T, marker = list(radius = 4)) %>%
                        hc_tooltip(formatter=JS("function() {if(this.point.expression !== 'Unsignificant') { return this.point.SYMBOL } return false;}")) %>%
                        hc_xAxis(title = list(text = "Log2 fold change"), gridLineWidth = 0,plotLines=list(
                                list(color = "black",
                                     width = 2,
                                     value = 1,
                                     zIndex= 4,
                                     dashStyle="Dash"),
                                list(color = "black",
                                     width = 2,
                                     value = -1,
                                     zIndex= 4,
                                     dashStyle="Dash")
                        )) %>%
                        hc_yAxis(title = list(text = "-Log10 p-value"),gridLineWidth = 0,plotLines=list(
                                list(color = "black",
                                     width = 2,
                                     value = 1,
                                     zIndex= 4,
                                     dashStyle="Dash")
                        ))%>%
                        hc_plotOptions(series = list(events = list(click = JS("function(event) {console.log(event);Shiny.onInputChange('rows', event.point.SYMBOL);}")))) %>%
                        hc_exporting(enabled = TRUE, filename = "TCGAnalyzeR_VolcanoPlot",buttons=list(contextButton=list(menuItems=list("downloadPNG"))))
        })
        
        
        output$volcano_table <- DT::renderDataTable({
                #sign_genes <- sign_genes()
                #sign_genes_2 <- sign_genes_2()
                #mdata <- setDT(sign_genes)[ i = sign_genes_2, SYMBOL := i.SYMBOL,
                #                            on = .(ENSEMBL)]
                
                mdata <- sign_genes_2()
                mdata <- mdata %>% 
                        dplyr::rowwise() %>%
                        mutate(SYMBOL = ifelse(!is.na(SYMBOL),paste0('<a href="#" onclick="Shiny.setInputValue(\'rows\',\'',SYMBOL,'\')" id="addtomylist" class="',SYMBOL,'">',SYMBOL,'</a>'),""))
                
                mdata <- mdata[,c((ncol(mdata)-1),1:(ncol(mdata)-2))]
                #DT::datatable(mdata,escape = FALSE,rownames = F,filter = 'top', options = list(
                #    columnDefs = list(list(className = 'dt-center', targets = "_all")), autoWidth = F, scrollX = T),selection = 'none') #selection = 'none'
                
                DT::datatable(mdata,escape = FALSE,rownames = F,extensions = 'Buttons',selection = 'none',
                              options = list(dom = 'Bfrtip', buttons = list(list(
                                      extend = 'csv',
                                      extension='.tsv',
                                      fieldSeparator="\t",
                                      filename="TCGAnalyzeR_VolcanoTable",
                                      className= 'fa fa-download fa-2x btn btn-default action-button shiny-bound-input',
                                      text = ''
                              )),#columnDefs = list(list(className = 'dt-center', targets = "_all")), 
                              autoWidth = F, scrollX = T,
                              searchHighlight = TRUE,scrollX = T),class = "row-border hover") %>%
                        DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='60%',`font-size` = '14px')
        })
        
        
        ###############Enrichment Plots Part###############
        
        
        output$enrichment_ui <- renderUI({
                fluidRow(
                        column(class="mydiv",
                               width = 12,
                               box(title = ifelse(input$checkGroup3=='bar', "Bar Plot",
                                                  ifelse(input$checkGroup3=='heat',"Heat Plot",
                                                         ifelse(input$checkGroup3=='cnet',"Cnet Plot",
                                                                ifelse(input$checkGroup3=='boxp',"Box Plot","")))),
                                   width = 12,status = "primary",solidHeader =F,
                                   sidebar = bs4CardSidebar(id="heatsidebar",width=25,icon = HTML('<i class="fa fa-filter">Filter</i>'),
                                                      conditionalPanel("input.checkGroup3=='bar' | input.checkGroup3=='heat' | input.checkGroup3=='cnet'",
                                                            selectInput("enrch_filter", "De-regulated Pathways:",
                                                                        c("Total" = "total","Up" = "up","Down" = "down"),selected = "total"),
                                                            sliderInput("enrc_pval", "p.adjust",
                                                                        min = 0, max = 0.05, value = 0.05, step = 0.01),
                                                            uiOutput("enrc_values"),
                                                            br(),
                                                            uiOutput("enrc_genes")
                                                    ),
                                                    conditionalPanel("input.checkGroup3=='boxp'",
                                                                     uiOutput("box_genes")
                                                    ),
                                                    br(),
                                                    actionButton("enrc_button", "Plot")
                                   ),
                                   conditionalPanel("input.checkGroup3=='bar'",
                                                    highchartOutput("barplot",height="100%") %>% withSpinner()
                                   ),
                                   conditionalPanel("input.checkGroup3=='heat'",
                                                    highchartOutput("heatplot",height=700) %>% withSpinner()
                                   ),
                                   conditionalPanel("input.checkGroup3=='cnet'",
                                               visNetworkOutput("cnettt",height=700) %>% withSpinner()
                                   ),
                                   conditionalPanel("input.checkGroup3=='boxp'",
                                                    highchartOutput("boxplot",height=700) %>% withSpinner()
                                                    #withSpinner(uiOutput('boxplot_ui'))
                                   )
                                         
                               ),
                               box(title = "Table",width = 12,status = "primary",solidHeader =F,
                                   withSpinner(DT::dataTableOutput("enrc_table",width = "100%")))))
        })
        
        
        EA <- eventReactive(c(input$enrc_button,
                              req(input$mdata_rnaseq),req(input$pairedTotal_dea)),{
                                      EA <- read.delim(paste0("data/",tolower(input$pairedTotal_dea),"/",input$mdata_rnaseq,"/DEA/",input$mdata_rnaseq,"_pathways_",input$enrch_filter,".tsv"), sep="\t")
                                      
                                      #load(paste0("data/",tolower(input$pairedTotal_dea),"/",input$mdata_rnaseq,"/DEA/",input$mdata_rnaseq,"_EA.RData"))
                                      EA
                              })
        
        output$enrc_values <- renderUI({
                
                aa <- cbind(EA()$ID,EA()$Description,EA()$geneID,EA()$Count)
                aa <- as.data.frame(aa)
                
                vchoices <- 1:nrow(aa)
                names(vchoices) <- aa$V2
                
                #selectizeInput("enrc_val","Select Pathway",choices=vchoices,
                #               selected=NULL,
                #               multiple = T,options = list(maxItems = 10))
                
                pickerInput(
                        inputId = "enrc_val",
                        label = "Select Pathway",
                        choices = unique(names(vchoices)),
                        multiple = T,
                        selected=enrc_handle(),
                        width = '300px',
                        options = list(`live-search` = TRUE,
                                       `live-search-normalize` = TRUE,
                                       `live-searc-placeholder`="Enter Pathway",
                                       `size`=5,
                                       `width`= 'fit',
                                       `actions-box` = TRUE,
                                       `max-options`=10)
                )
        })
        
        output$enrc_genes <- renderUI({
                
                mdata=data.frame(EA()[,c("Description","geneID","p.adjust")],row.names = NULL)
                
                df = tidyr::separate_rows(mdata,2,sep = "/")
                
                pickerInput(
                        inputId = "enrc_gene",
                        label = "Genes",
                        choices = unique(df$geneID[sort.list(df$geneID)]),
                        selected=enrc_handle2(),
                        multiple = T,
                        width = '300px',
                        options = list(`live-search` = TRUE,
                                       `live-search-normalize` = TRUE,
                                       `live-searc-placeholder`="Enter gene name",
                                       `size`=5,
                                       `actions-box` = TRUE,
                                       `max-options`=7)
                )
        })
        
        
        
        enrc_handle <- eventReactive(c(input$enrc_button,
                                       req(input$mdata_rnaseq),req(input$pairedTotal_dea)), {
                                               input$enrc_val
                                               
                                       })
        
        enrc_handle3 <- eventReactive(c(input$enrc_button,
                                       req(input$mdata_rnaseq),req(input$pairedTotal_dea)), {
                                         input$enrc_pval
                                         
                                       })
        
        enrc_handle2 <- eventReactive(c(input$enrc_button,
                                        req(input$mdata_rnaseq),req(input$pairedTotal_dea)), {
                                                input$enrc_gene
                                                
                                        })
        
        
        output$cnettt <- renderVisNetwork({
                #print(enrc_handle3())
                aa <- cbind(EA()$ID,EA()$Description,EA()$geneID,EA()$p.adjust,EA()$Count)
                aa <- as.data.frame(aa)
                
                #aa <- subset(aa,V2 == "Hypertrophic cardiomyopathy" | V2 == "Renin-angiotensin system" | V2 == "Renin secretion")
                
                
                aa <- as.data.frame(aa)
                
                if(length(enrc_handle()) != 0){
                        #aa <- aa[enrc_handle(),]
                        aa <- subset(aa,V2 %in% enrc_handle())
                } else {
                        aa <- aa[c(1:10),]
                }
                
                
                aa <- subset(aa,V4 <= enrc_handle3())
                
                
                bb=data.frame(separate_rows(aa,V3,sep="/"))
                colnames(bb)[2] <- "from"
                colnames(bb)[3] <- "to"
                
                nodes <- data.frame(id = unique(c(bb$from,bb$to)),title=unique(c(bb$from,bb$to)),
                                    label=unique(c(bb$from,bb$to)),
                                    value=c(rep(200,length(unique(bb$from))),rep(150,length(unique(c(bb$from,bb$to)))-length(unique(bb$from)))))
                edges <- data.frame(from = bb$from, to = bb$to)
                
                
                visNetwork(nodes, edges) %>%
                  visNodes(shadow = T,font=list(size=38,align="left")) %>%
                  visExport() 
                
        })
        
        output$heatplot <- renderHighchart({
                #load(paste0("data/",tolower(input$pairedTotal_cnv),"/",input$mdata_rnaseq,"/DEA/",input$mdata_rnaseq,"_",tolower(input$pairedTotal_dea),"_sign_genes_2.rda"))
                sign_genes_2 <- sign_genes_2()
                print(colnames(sign_genes_2)[9])
                colnames(sign_genes_2)[9] <- "geneID"
                
                # #EA_ <- head(EA,30)
                # #m <- matrix(rnorm(ncol(EA_)*nrow(EA_)), nrow = nrow(EA_), ncol = length(sign_genes_3))
                # enrichplot::heatplot(EA, foldChange=sign_genes_3, showCategory = 30) + ggtitle("Heatmap plot of enriched terms")
                # #plot_ly(x = sign_genes_3, y = EA_$Description,z = EA_, type = "heatmap")
                mdata=data.frame(EA()[,c("Description","geneID","p.adjust")],row.names = NULL)
                
                mdata_ <- subset(mdata,Description %in% enrc_handle())
                
                if(nrow(mdata_) == 0) {
                        mdata <- head(mdata,10)
                } else {
                        mdata <- mdata_
                }
                
                mdata <- subset(mdata,p.adjust <= enrc_handle3())
                
                #EA_ <- unique(rbind(EA_,EA__))
                mdata <- mdata %>% rowwise() %>%
                        mutate(url=paste0("https://www.ncbi.nlm.nih.gov/gene/?term=(" , paste(strsplit(geneID,"/")[[1]],collapse = "%5BGN%5D+OR+") ,"%5BGN%5D)+AND+%22Homo+sapiens%22%5Bporgn%3A__txid9606%5D","---",Description))
                
                df = tidyr::separate_rows(mdata,2,sep = "/")
                
                df <- setDT(df)[ i = sign_genes_2, logFC := i.logFC,
                                 on = .(geneID)]
                
                
                
                if(length(enrc_handle2()) != 0) {
                        df <- subset(df, geneID %in% enrc_handle2())
                }
                #hc$x$hc_opts$series[[1]]$data[[1]]
                
                highchart() %>%
                        hc_chart(
                                type = 'heatmap'
                        ) %>%
                        hc_add_series(
                                data = df,
                                type = 'heatmap',
                                hcaes(x = geneID, y = url,value="logFC"),
                                borderWidth = 1,
                                borderColor="white"
                        ) %>%
                        hc_tooltip(formatter = JS("function(){ return ( this.point.Description + ' ~' + this.point.geneID + ' <br> ' +this.point.value)}")
                        ) %>%
                        hc_title(verticalAlign = "bottom",text = "fold change", style = list(fontSize = "14px"),margin=5) %>% 
                        hc_xAxis(
                                categories = unique(df$geneID[sort.list(df$geneID)]),
                                title=NULL,
                                max= ifelse(length(df$geneID)>80 ,length(df$geneID)/2,ifelse(length(df$geneID)>40,30,length(df$geneID)-1)),
                                scrollbar = list(enabled=T,showFull=F)
                        ) %>%
                        hc_yAxis(
                                categories = unique(df$url[sort.list(df$url)]),
                                title=list(text = "Pathway"),
                                scrollbar = list(enabled=T,showFull=F),
                                labels=list(formatter=JS("function(){ if(this.value.split(\"---\")[1]){return ( '<a  href=\"'+this.value.split(\"---\")[0]+'\" target=\"_blank\" style=\"cursor:\"pointer\";color:\"blue\";\">'+ this.value.split(\"---\")[1] + '</a> ');} else {return ( '<a  href=\"'+this.axis.categories.split(\"---\")[0]+'\" target=\"_blank\" style=\"cursor:\"pointer\";color:\"blue\";\">'+ this.axis.categories.split(\"---\")[1] + '</a> ');}}"),useHTML=T)
                        ) %>% hc_colorAxis(stops = color_stops(2, c("green","red")))%>%
                        hc_plotOptions(series=list(dataLabels =list(overflow= 'none',
                                                                    crop= T))) %>%
                        hc_exporting(enabled = TRUE,sourceWidth=1200, sourceHeight=400,filename = "TCGAnalyzeR_HeatPlot",buttons=list(contextButton=list(menuItems=list("downloadPNG"))))
                
        })
        
        gem_norm <- eventReactive(c(input$enrc_button,
                                       req(input$mdata_rnaseq),req(input$pairedTotal_dea)), {
                                         load(paste0("data/",tolower(input$pairedTotal_dea),"/",input$mdata_rnaseq,"/DEA/",input$mdata_rnaseq,"_gem_normalized.rda"))
                                         gem_norm
                                       })
        
        ids <- eventReactive(c(input$enrc_button,
                                    req(input$mdata_rnaseq),req(input$pairedTotal_dea)), {
                                      ids <- read.delim(paste0("data/",tolower(input$pairedTotal_dea),"/",input$mdata_rnaseq,"/DEA/",input$mdata_rnaseq,"_all_genes_ids.tsv"), sep="\t")
                                      ids
                                    })
        
       
        
        output$box_genes <- renderUI({
          ids <- read.delim(paste0("data/",tolower(input$pairedTotal_dea),"/",input$mdata_rnaseq,"/DEA/",input$mdata_rnaseq,"_all_genes_ids.tsv"), sep="\t")
          pickerInput(
            inputId = "box_gene",
            label = "Genes",
            choices = unique(ids$ensembl_gene_id),
            multiple = F,
            width = '300px',
            options = list(`live-search` = TRUE,
                           `live-search-normalize` = TRUE,
                           `live-searc-placeholder`="Enter gene name",
                           `size`=5)
          )
        })
        
        gene_box <- eventReactive(c(input$enrc_button,
                                    req(input$mdata_rnaseq),input$pairedTotal_dea), {
                                      input$box_gene
                                    })
        
        output$boxplot <- renderHighchart({

          sample_normal <- TCGAquery_SampleTypes(colnames(gem_norm()[["counts"]]), c("NT"))
          sample_tumor <- TCGAquery_SampleTypes(colnames(gem_norm()[["counts"]]), c("TP"))
          print(gene_box())
          gene <- ifelse(is.null(gene_box()),"ENSG00000000003",gene_box())
          gene_exp <- gem_norm()[["counts"]][gene,]
          box_df <- as.data.frame(gene_exp)
          
          box_df$sample_group <- NA
          box_df[which(rownames(box_df) %in% sample_normal), "sample_group"] <- "Normal Samples"
          box_df[which(rownames(box_df) %in% sample_tumor), "sample_group"] <- "Cancer Samples"
          #box_df <- subset(box_df,!is.na(sample_group))
          #gene_exp <- subset(gene_exp,colnames(gene_exp) %in% rownames(box_df))
          #p <- ggplot(box_df, aes(x=sample_group, y=gene_exp, color=sample_group)) + geom_boxplot(outlier.colour = "black", outlier.shape = 1)
          #p + labs(x="Sample Groups", y = "Normalized Expression (HTSeq Counts)") + 
          #  scale_x_discrete(limits=c("Normal Samples", "Cancer Samples")) + theme_classic() 

          dat <- data_to_boxplot(box_df, gene_exp, sample_group, name = "Sample Groups",add_outliers=T)
          
          highchart() %>%
            hc_title(text = ifelse(is.null(gene_box()),"ENSG00000000003",gene_box())) %>%
            hc_xAxis(type = "category") %>%
            hc_yAxis(title = list(text = "Normalized Expression (HTSeq Counts)")) %>%
            hc_add_series_list(dat)
          
          
        })
        
        output$barplot <- renderHighchart({
                
                EA_ <- EA()[,c(2,6,7,8,9)]
                EA__ <- subset(EA_,Description %in% enrc_handle())
                
                if(nrow(EA__) == 0) {
                        EA_ <- head(EA_,10)
                } else {
                        EA_ <- EA__
                }
                
                EA_ <- subset(EA_,p.adjust <= enrc_handle3())
                
                #EA_ <- unique(rbind(EA_,EA__))
                EA_ <- EA_[order(EA_$qvalue),]
                EA_ <- EA_ %>% rowwise() %>%
                        mutate(url=paste0("https://www.ncbi.nlm.nih.gov/gene/?term=(" , paste(strsplit(geneID,"/")[[1]],collapse = "%5BGN%5D+OR+") ,"%5BGN%5D)+AND+%22Homo+sapiens%22%5Bporgn%3A__txid9606%5D","---",Description))
                hchart(EA_, "bar", hcaes(x = url, y = Count), colorKey = "qvalue") %>%
                        hc_xAxis(title=list(text = "Pathway"),labels=list(formatter=JS("function(){ return ( '<a  href=\"'+this.value.split(\"---\")[0]+'\" target=\"_blank\" style=\"cursor:\"pointer\";color:\"blue\";\">'+ this.value.split(\"---\")[1] + '</a> ')}"),useHTML=T)) %>% 
                        hc_yAxis(title=list(text="Gene Count")) %>% 
                        hc_colorAxis(min = min(EA_$qvalue), max = max(EA_$qvalue),minColor = "red",
                                     maxColor = "yellow") %>% 
                        hc_title(verticalAlign = "bottom",text = "p-value", style = list(fontSize = "14px"),margin=5) %>% 
                        hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal") %>%
                        hc_tooltip(formatter = JS("function(){ return ( '<b>'+this.point.Description + '</b> <br><b>q-value:  </b>' +this.point.qvalue + ' <br><b>Count: </b>' + this.point.Count )}")) %>%
                        hc_exporting(enabled = TRUE, filename = "TCGAnalyzeR_BarDriverPlot",buttons=list(contextButton=list(menuItems=list("downloadPNG"))))
        })
        
        output$enrc_table <- DT::renderDataTable({
                aa <- cbind(EA()$ID,EA()$Description,EA()$geneID,EA()$GeneRatio,EA()$p.adjust,EA()$qvalue,EA()$Count)
                aa <- as.data.frame(aa)
                colnames(aa) <- c("KEGG ID","Pathway Name","Genes in the pathway","Gene Ratio","p.adjust","q value","Number of Genes")
                
                
                DT::datatable(cbind(' ' = '<i class="fas fa-plus-circle"></i>',aa),selection='none',extensions = 'Buttons', escape = -2,class = "row-border hover",
                              options = list(scrollX=T,autoWidth = F,dom = 'Bfrtip',searchHighlight = TRUE,
                                             buttons = list(list(
                                                     extend = 'csv',
                                                     extension='.tsv',
                                                     fieldSeparator="\t",
                                                     filename="TCGAnalyzeR_EnrichmentTable",
                                                     className= 'fa fa-download fa-2x btn btn-default action-button shiny-bound-input',
                                                     text = '',
                                                     exportOptions=list(columns=c(2:5))
                                             )),
                                             columnDefs = list(
                                                     list(visible = FALSE, targets = c(0,4)),
                                                     list(orderable = FALSE, className = 'details-control', targets = 1)
                                             )), callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                  return '<div style=\"background-color:#eee; padding: .5em;width:90%;word-break: break-word;\"> Genes in the pathway: ' +
                          d[4]+'</div>';
                };
                table.on('click', 'td.details-control', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                    row.child.hide();
                    td.html('<i class=\"fas fa-plus-circle\"></i>');
                  } else {
                    row.child(format(row.data())).show();
                    td.html('<i class=\"fas fa-minus-circle\"></i>');
                  }
                });"))%>%
                        DT::formatStyle( 0, target= 'row',color = 'black', lineHeight='60%',`font-size` = '14px')
                
        })
        ###############Clnical Part###############
        
        clinical_data <- eventReactive(c(req(input$mdata_clinical),req(input$pairedTotal_clinical)),{
                load(paste0("data/",tolower(input$pairedTotal_clinical),"/",input$mdata_clinical,"/clinical/",input$mdata_clinical,"_parsed_clinical_data.rda"))
                #load(paste0("clinical_total/LUAD_parsed_clinical_data.rda"))
                cfu
        })
        
        clinical_data2 <- eventReactive(c(req(input$mdata_clinical),req(input$pairedTotal_clinical)),{
                load(paste0("data/",tolower(input$pairedTotal_clinical),"/",input$mdata_clinical,"/clinical/",input$mdata_clinical,"_parsed_clinical_data.rda"))
                #load(paste0("clinical_total/LUAD_parsed_clinical_data.rda"))
                for (i in c("days_to_diagnosis","age_at_diagnosis","year_of_diagnosis","pack_years_smoked","cigarettes_per_day","years_smoked","age_at_index",
                            "days_to_birth","year_of_birth","year_of_death","days_to_death")) {
                        
                        if(min(cfu[[i]],na.rm = T)!= max(cfu[[i]],na.rm = T) & !is.null(cfu[[i]])){
                                cfu$group <- cut(cfu[[i]],c(min(cfu[[i]],na.rm = T),
                                                            (min(cfu[[i]],na.rm = T)+max(cfu[[i]],na.rm = T))/3,
                                                            (min(cfu[[i]],na.rm = T)+max(cfu[[i]],na.rm = T))/3*2,
                                                            max(cfu[[i]],na.rm = T)
                                ))
                                
                                
                                levels(cfu$group) <- c(paste0(as.character(min(cfu[[i]],na.rm = T)),"-",as.character((min(cfu[[i]],na.rm = T)+max(cfu[[i]],na.rm = T))/3)),
                                                       paste0(as.character((((min(cfu[[i]],na.rm = T)+max(cfu[[i]],na.rm = T))/3)+1)),"-",as.character((((min(cfu[[i]],na.rm = T)+max(cfu[[i]],na.rm = T))/3)*2))),
                                                       paste0(as.character((((min(cfu[[i]],na.rm = T)+max(cfu[[i]],na.rm = T))/3)*2)),"-",as.character(max(cfu[[i]],na.rm = T))))
                                
                                cfu[[i]] <- cfu$group
                        }
                        
                }
                
                cfu <- cfu[ , -which(names(cfu) %in% c("submitter_id","updated_datetime","diagnosis_id","exposure_id","demographic_id","treatments_pharmaceutical_treatment_id","treatments_radiation_treatment_id",
                                                       "bcr_patient_barcode","disease","group"))]
                cfu
        })
        
        output$selector <- renderUI({
                
                clinical <- clinical_data2()
                vchoices <- 1:ncol(clinical)
                names(vchoices) <- names(clinical)
                
                selectInput("columns","Select Columns",choices=vchoices,
                            selected=which(colnames(clinical)=="vital_status" | colnames(clinical)=="gender" |
                                                   colnames(clinical)=="tumor_stage"),
                            multiple = T)
        })
        
        observeEvent(input$columns,{
                clinical <- clinical_data2()
                output$boxes <- renderUI({
                        
                        lapply(1:length(input$columns), function(a) {
                                box(title = paste0(colnames(clinical[as.integer(input$columns[a])])),width = 6, status = "primary",solidHeader =F,
                                    id=paste0(colnames(clinical[as.integer(input$columns[a])])),
                                    bs4Dash::tabsetPanel(
                                            type="tabs",
                                            id = "tabcard1",
                                            tabPanel(
                                                    title = "Plot",
                                                    active = FALSE,
                                                    highchartOutput(paste0("chart_",a)) #%>% withSpinner(),
                                            ),
                                            tabPanel(
                                                    title = "Survival Plot",
                                                    active = TRUE,
                                                    plotlyOutput(paste0("sur_chart_",a))# %>% withSpinner(),
                                            )
                                    ),
                                    footer =  DT::dataTableOutput(paste0("chart_table_",a),width = "100%")
                                )
                        })
                })
        })
        
        observeEvent(input$columns,{
                lapply(1:length(input$columns), function(a) {
                        output[[paste0("chart_",a)]] <- renderHighchart({
                                clinical <- clinical_data2()
                                tmp <- data.frame()
                                
                                k <- as.data.frame(table(clinical[,as.integer(input$columns[a])],exclude=F))
                                k2 <- data.frame(Var1=a, Freq="")
                                tmp <- rbind(k2,k)
                                
                                if(length(tmp[is.na(tmp),]$Var1) != 0){
                                        tmp[is.na(tmp),]$Var1 <- "NULL"
                                }
                                tmp$Freq <- as.integer(tmp$Freq)
                                tmp[-1,] %>%
                                        hchart(
                                                "pie", hcaes(x = Var1, y = Freq),
                                                name = "Samples",dataLabels=F,showInLegend=T,allowPointSelect=T
                                        ) %>%
                                        hc_exporting(enabled = TRUE, filename = paste0("TCGAnalyzeR_Clinical_",input$columns[a]),buttons=list(contextButton=list(menuItems=list("downloadPNG"))))
                        })
                })
        })
        
        
        observeEvent(input$columns,{
                lapply(1:length(input$columns), function(a) {
                        output[[paste0("sur_chart_",a)]] <- renderPlotly({
                                
                                clinical <- clinical_data2()
                                #clinical_ <- clinical
                                bb <- colnames(clinical)[as.integer(input$columns[a])]
                                print(bb)
                                #tt <- table(clinical[[bb]])
                                #clinical_ <- clinical_[clinical_[[bb]] %in% names(tt[tt > 1]), ]
                                
                                #clinical[[bb]] <- as.numeric(as.factor(clinical[[bb]]))
                                #print(table(clinical[[bb]]))
                                clinical <- clinical %>% dplyr::rowwise()  %>% mutate(vital_status=ifelse(vital_status=="Alive",1,2))
                                clinical$mm <- clinical[[bb]]
                                clinical$vital_status <- as.numeric(clinical$vital_status)
                                #clinical=transform(clinical, tumor_stage = as.numeric(as.factor(tumor_stage)))
                                #clinical$days_to_last_follow_up <- as.numeric(clinical$days_to_last_follow_up)
                                
                                fit <- survfit(Surv(days_to_last_follow_up, vital_status) ~ mm, data = clinical)
                                #print(fit)
                                names(fit$strata) <- gsub("mm=","",names(fit$strata))
                                p1 <- ggsurvplot(fit, data = clinical,pval = TRUE,pval.coord=c(7000,0))
                                
                                plotly::ggplotly(p1[[1]])
                                
                        })
                })
        })
        
        
        observeEvent(input$columns,{
                lapply(1:length(input$columns), function(a) {
                        output[[paste0("chart_table_",a)]] <- DT::renderDataTable({
                                #clinical <- GDCquery_clinic(project = paste0("TCGA-",input$mdata), type = "clinical")
                                clinical <- clinical_data2()
                                tmp <- data.frame()
                                
                                k <- as.data.frame(table(clinical[,as.integer(input$columns[a])],exclude=F))
                                k2 <- data.frame(Var1=colnames(clinical)[as.integer(input$columns[a])], Freq="")
                                tmp <- rbind(k2,k)
                                
                                if(length(tmp[is.na(tmp),]$Var1) != 0){
                                        tmp[is.na(tmp),]$Var1 <- "NULL"
                                }
                                DT::datatable(tmp,selection = 'none',class = 'cell-border stripe bb',rownames = FALSE,
                                              callback = JS("$('.dataTables_scrollHead').css('display', 'none');$('.dataTables_scrollBody').css('border-bottom', 'none');"),
                                              options = list(paging = F, scrollX = T,scrollY = "200px",searching = FALSE, info = FALSE)) %>% formatStyle(
                                                      'Var1','Freq',
                                                      target = 'row',
                                                      backgroundColor = styleEqual("", "gray"),
                                                      color=styleEqual("", "white"),
                                                      lineHeight='60%',`font-size` = '14px'
                                              )
                        })
                })
        })
})
