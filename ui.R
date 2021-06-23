#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(DBI)
library(org.Hs.eg.db)
library(shinycssloaders)
library(highcharter)
library(shinyjs)
library(rclipboard)
library(waiter)

projects <- read.delim("data/home.tsv", sep="\t",check.names = FALSE,stringsAsFactors = FALSE,header = T)

# gallery_2_tab ----
analysis <- tabItem(
    tabName = "cnv_tabb",
    jumbotron(
        title = "I am a Jumbotron!",
        lead = "This is a simple hero unit, a simple jumbotron-style 
                    component for calling extra attention to featured 
                    content or information.",
        "It uses utility classes for typography and spacing 
            to space content out within the larger container.",
        status = "primary",
        href = "https://www.google.fr"
    ),
    
    br(),
    
    fluidRow(
        box(
            title = "Badges",
            dashboardBadge(color = "secondary", "blabla", rounded = TRUE),
            dashboardBadge(color = "info", "blabla", rounded = TRUE)
        )
    ),
    
    br(),
    
    h4("BS4 list group"),
    fluidRow(
        listGroup(
            type = "basic",
            listGroupItem("Cras justo odio"),
            listGroupItem("Dapibus ac facilisis in"),
            listGroupItem("Morbi leo risus")
        ),
        listGroup(
            type = "action",
            listGroupItem(
                "Cras justo odio",
                active = TRUE, 
                disabled = FALSE, 
                href = "http://www.google.fr"
            ),
            listGroupItem(
                active = FALSE, 
                disabled = FALSE, 
                "Dapibus ac facilisis in",
                href = "http://www.google.fr"
            ),
            listGroupItem(
                "Morbi leo risus",
                active = FALSE, 
                disabled = TRUE, 
                href = "http://www.google.fr"
            )
        ),
        listGroup(
            type = "heading",
            listGroupItem(
                "Donec id elit non mi porta gravida at eget metus. 
         Maecenas sed diam eget risus varius blandit.",
                active = TRUE, 
                disabled = FALSE, 
                title = "List group item heading", 
                subtitle = "3 days ago", 
                footer = "Donec id elit non mi porta."
            ),
            listGroupItem(
                "Donec id elit non mi porta gravida at eget metus. 
         Maecenas sed diam eget risus varius blandit.",
                active = FALSE, 
                disabled = FALSE, 
                title = "List group item heading", 
                subtitle = "3 days ago", 
                footer = "Donec id elit non mi porta."
            )
        )
    )
)

snv_analysis <- tabItem(
    tabName = "snv_tab",
    fluidRow(
    selectInput(
        inputId = "mdata_snv", 
        label = "Data Set", 
        choices = c("Lung Adenocarcinoma" = "LUAD",
                    "Breast Invasive Carcinoma" = "BRCA",
                    "Liver Hepatocellular Carcinoma" = "LIHC",
                    "Lung Squamous Cell Carcinoma" = "LUSC"),selected = "LUAD"
    ),
    selectInput("checkGroup", "Plots:",
                c("Onco Plot" = "onco", #"Select one" = "",
                  "Lollipop Plot" = "lollipop",
                  "Cancer Driver Genes" = "cancerdriver"),selected = "onco"),
    selectInput("pairedTotal_snv", "Cohort Type:",
                c("Paired" = "paired", #"Select one" = "",
                  "Total" = "total"),selected = "paired"),
    conditionalPanel("input.mdata_snv=='LUAD' | input.mdata_snv=='LUSC'",
                     selectInput("riskGroup", "Risk Group:",
                                 c("Full" = "full",
                                   "High risk" = "high", #"Select one" = "",
                                   "Low risk" = "low"),selected = "high")
    )
    ),
    column(width=12,
           conditionalPanel("input.checkGroup == 'onco'",
                            withSpinner(uiOutput('oncoplot_ui'))
           ),
           conditionalPanel("input.checkGroup == 'lollipop'",
                            withSpinner(uiOutput('lollipopplot_ui'))
           ),
           conditionalPanel("input.checkGroup == 'cancerdriver'",
                            withSpinner(uiOutput('cancerdriverplot_ui'))
           )
          )
    
)

cnv_analysis <- tabItem(
    tabName = "cnv_tab",
    fluidRow(
        selectInput(
            inputId = "mdata_cnv", 
            label = "Data Set", 
            choices = c("Lung Adenocarcinoma" = "LUAD",
                        "Breast Invasive Carcinoma" = "BRCA",
                        "Liver Hepatocellular Carcinoma" = "LIHC",
                        "Lung Squamous Cell Carcinoma" = "LUSC"),selected = "LUAD"
        ),
        selectInput("checkGroup2", "Plots:",
                    c("Cnv Plot" = "cnv","Enrichment Plots" = "enrc_cnv"),selected = "cnv"),
        selectInput("pairedTotal_cnv", "Cohort Type:",
                    c("Paired" = "paired", #"Select one" = "",
                      "Total" = "total"),selected = "paired"),
        conditionalPanel("input.mdata_cnv=='LUAD' | input.mdata_cnv=='LUSC'",
                         selectInput("riskGroup", "Risk Group:",
                                     c("Full" = "full",
                                       "High risk" = "high", #"Select one" = "",
                                       "Low risk" = "low"),selected = "high")
        )
    ),
    column(width = 12,
           conditionalPanel("input.checkGroup2=='cnv'",
                            withSpinner(uiOutput('cnvplot_ui'))
                            
           ),
           conditionalPanel("input.checkGroup2=='enrc_cnv'",
                            withSpinner(uiOutput('cnv_enrichment_ui'))
           )
    )
    
)

rnaseq_analysis <- tabItem(
    tabName = "rnaseq_tab",
    fluidRow(
        selectInput(
            inputId = "mdata_rnaseq", 
            label = "Data Set", 
            choices = c("Lung Adenocarcinoma" = "LUAD",
                        "Breast Invasive Carcinoma" = "BRCA",
                        "Liver Hepatocellular Carcinoma" = "LIHC",
                        "Lung Squamous Cell Carcinoma" = "LUSC"),selected = "LUAD"
        ),
        selectInput("checkGroup3", "Plots:",
                    c("Volcano Plot" = "volcano", "Bar Plot" = "bar",
                      "Heat Plot" = "heat","Cnet Plot" = "cnet","Box Plot" = "boxp"),selected = "volcano"),
        selectInput("pairedTotal_dea", "Cohort Type:",
                    c("Paired" = "paired", #"Select one" = "",
                      "Total" = "total"),selected = "paired"),
        conditionalPanel("input.mdata_rnaseq=='LUAD' | input.mdata_rnaseq=='LUSC'",
                         selectInput("riskGroup", "Risk Group:",
                                     c("Full" = "full",
                                       "High risk" = "high", #"Select one" = "",
                                       "Low risk" = "low"),selected = "high")
        )
    ),
    column(width = 12,
           conditionalPanel("input.checkGroup3=='volcano'",
                            withSpinner(uiOutput('volcanoplot_ui'))
           ),
           conditionalPanel("input.checkGroup3!='volcano'",
                               withSpinner(uiOutput('enrichment_ui'))
           )
           # conditionalPanel("input.checkGroup3=='bar'",
           #                  withSpinner(uiOutput('barplot_ui'))
           # ),
           # conditionalPanel("input.checkGroup3=='heat'",
           #                  withSpinner(uiOutput('heatplot_ui'))
           # ),
           # conditionalPanel("input.checkGroup3=='cnet'",
           #                  withSpinner(uiOutput('cnetplot_ui'))
           # ),
           # conditionalPanel("input.checkGroup3=='boxp'",
           #                  withSpinner(uiOutput('boxplot_ui'))
           # )
    )
    
)

clinical_analysis <- tabItem(
    tabName = "clinical_tab",
    fluidRow(
        selectInput("mdata_clinical", "Data Set:",
                    c("Lung Adenocarcinoma" = "LUAD", #"Select one" = "",
                      "Breast Invasive Carcinoma" = "BRCA",
                      "Liver Hepatocellular Carcinoma" = "LIHC",
                      "Lung Squamous Cell Carcinoma" = "LUSC"),selected = "LUAD"),
        uiOutput("selector"),
        selectInput("pairedTotal_clinical", "Cohort Type:",
                    c("Paired" = "paired", #"Select one" = "",
                      "Total" = "total"),selected = "paired"),
        conditionalPanel("input.mdata_clinical=='LUAD' | input.mdata_clinical=='LUSC'",
                         selectInput("riskGroup", "Risk Group:",
                                     c("Full" = "full",
                                       "High risk" = "high", #"Select one" = "",
                                       "Low risk" = "low"),selected = "high")
        )
    ),
    column(width = 12,
           withSpinner(uiOutput("boxes"))
    )
    
)

home <- tabItem(
    tabName = "home_tab",
    div(fluidRow(
            column(class="mydiv",
                   width = 12,
                   box(title = "TCGA Cancer Types",width = 12, status = "primary",solidHeader =F,id="homebox",
                       bs4Dash::tabsetPanel(
                           type="tabs",
                           id = "tabcard1",
                           tabPanel(
                               tabName = "a",
                               title = "Plot",
                               active = FALSE,
                               
                               highchartOutput("projects_chart") %>% withSpinner()
                           ),
                           tabPanel(
                               title = "Table",
                               active = TRUE,
                               withSpinner(DT::dataTableOutput("project_table",width = "100%"))
                               
                           )
                       )
                   ))),
        
        fluidRow(
            column(class="mydiv",
                   width = 2,
                   box(title = "Cancer Type Details",width = 12, status = "primary",solidHeader =F,
                       withSpinner(DT::dataTableOutput("projectDetail",width = "100%")))
            )
        )
    )
    )
about <- tabItem(
    tabName = "about_tab",
    box(width = 12,title = "About",
        img(src='https://posta.mu.edu.tr/color/img/logo_msku.png',id="mylogo"),
        p("Basak ABAK, Talip ZENGIN, Tugba ONAL SUZEK"),
        a(href="http://mu.edu.tr/en", "Mugla Sitki Kocman University"),
        br(),
        a(href="http://biyoinformatik.fenbilimleri.mu.edu.tr/en", "Bioinformatics Graduate Program"),
        
        helpText("This application was created to analyze the cancer data of TCGA Pan Cancer Project"),
        br(),
        helpText("This application uses the shiny package from RStudio."),
        helpText("Please send bugs and feature requests to", a(href='https://www.mu.edu.tr/tr/personel/tugbasuzek', 'Tugba ONAL SUZEK',target="_blank")),
        br(),
        helpText("Please cite the article below if you use any result created by this application in your article:"),
        helpText(em("Basak Abak, Talip Zengin, Tugba Onal Suzek. 2021. TCGAnalyzeR: An Rshiny application for analysis of TCGA cancer data. Bioinformatics.")),
        br(),
        a("View source Code for app", href="https://github.com",target="_blank")
    )
)

drugpart <- tabItem(
    tabName = "drug_tab",
    div()
    
)

helpPage <- tabItem(
    tabName = "help_tab",
    div(id = "help",
        fluidRow(
            column(width = 2,box(title = "Help", status = "primary",width = 12,id="myfixedcolumn2",solidHeader =T,collapsible =F,
                                 tags$h5("Table of Contents"),
                                 tags$ol(
                                     tags$li(tags$a(href="#introduction","Introduction")),
                                     tags$li(tags$a(href="#home_","Home"),
                                             tags$ol(
                                                 tags$li(tags$a(href="#homeplot_","Home Plot"))
                                             )),
                                     tags$li(tags$a(href="#snvanalysis","SNV Analysis"),
                                             tags$ol(
                                                 tags$li(tags$a(href="#oncoplot_","Onco Plot")), 
                                                 tags$li(tags$a(href="#lollipopplot_","Lollipop Plot")), 
                                                 tags$li(tags$a(href="#cancerplot_","Cancer Driver Plot"))
                                             )),
                                     tags$li(tags$a(href="#cnvanalysis","CNV Analysis"),
                                             tags$ol(
                                                 tags$li(tags$a(href="#cnvplot","CNV Plot")), 
                                                 tags$li(tags$a(href="#cnvenrcplot","Enrichment Plots"),
                                                         tags$ol(
                                                             tags$li(tags$a(href="#cnvheatplot","Heat Plot")), 
                                                             tags$li(tags$a(href="#cnvcnetplot","Cnet Plot")), 
                                                             tags$li(tags$a(href="#cnvbarplot","Bar Plot"))
                                                         )
                                                 )
                                             )),
                                     tags$li(tags$a(href="#rnaseqanalysis","RNASeq Analysis"),
                                             tags$ol(
                                                 tags$li(tags$a(href="#volcanoplot","Volcano Plot")), 
                                                 tags$li(tags$a(href="#rnaseqenrcplot","Enrichment Plots"),
                                                         tags$ol(
                                                             tags$li(tags$a(href="#rnaseqheatplot","Heat Plot")), 
                                                             tags$li(tags$a(href="#rnaseqcnetplot","Cnet Plot")), 
                                                             tags$li(tags$a(href="#rnaseqbarplot","Bar Plot"))
                                                         )
                                                 )
                                             )),
                                     tags$li(tags$a(href="#clinicalanalysis","Clinical Analysis"),
                                             tags$ol(
                                                 tags$li(tags$a(href="#survplot_","Survival Plot")), 
                                                 tags$li(tags$a(href="#survplot","Survival Plot"))
                                             )),
                                     tags$li(tags$a(href="#drug","Drug Finder"))
                                     
                                 )
            )),
            column(width = 10,
                   box(class="helpdiv", width = 12,solidHeader =F,height = "83vh",id="helpDiv", headerBorder =F,collapsible =F,
                       tags$ol(
                           tags$li(tags$a(id="introduction","Introduction")),
                           tags$li(tags$a(id="home_","Home"),
                                   tags$ol(
                                       tags$li(tags$a(id="homeplot_","Home Plot"),
                                               br(),
                                               tags$img(src = "home_plot.png",width=900,height=300)
                                       )
                                   )),
                           hr(),
                           tags$li(tags$a(id="snvanalysis","SNV Analysis"),
                                   br(),
                                   tags$img(src = "snv_tab.png"),
                                   tags$ol(
                                       tags$li(tags$a(id="oncoplot_","Onco Plot"),
                                               br(),
                                               tags$img(src = "onco_plot.png",width=900,height=300)
                                       ), 
                                       tags$li(tags$a(id="lollipopplot_","Lollipop Plot"),
                                               br(),
                                               tags$img(src = "lollipop_plot.png",width=900,height=300)
                                       ), 
                                       tags$li(tags$a(id="cancerplot_","Cancer Driver Plot"),
                                               br(),
                                               tags$img(src = "cancerdriver_plot.png",width=900,height=300)
                                       )
                                   )),
                           hr(),
                           tags$li(tags$a(id="cnvanalysis","CNV Analysis"),
                                   br(),
                                   tags$img(src = "cnv_tab.png"),
                                   tags$ol(
                                       tags$li(tags$a(id="cnvplot","CNV Plot"),
                                               br(),
                                               tags$img(src = "cnv_plot_.png",width=900,height=600)
                                       ), 
                                       tags$li(tags$a(id="cnvenrcplot","Enrichment Plots"),
                                               tags$ol(
                                                   tags$li(tags$a(id="cnvheatplot","Heat Plot")), 
                                                   tags$li(tags$a(id="cnvcnetplot","Cnet Plot")), 
                                                   tags$li(tags$a(id="cnvbarplot","Bar Plot"))
                                               )
                                       )
                                   )),
                           hr(),
                           tags$li(tags$a(id="rnaseqanalysis","RNASeq Analysis"),
                                   br(),
                                   tags$img(src = "rnaseq_tab.png"),
                                   tags$ol(
                                       tags$li(tags$a(id="volcanoplot","Volcano Plot")), 
                                       tags$li(tags$a(id="rnaseqenrcplot","Enrichment Plots"),
                                               tags$ol(
                                                   tags$li(tags$a(id="rnaseqheatplot","Heat Plot")), 
                                                   tags$li(tags$a(id="rnaseqcnetplot","Cnet Plot")), 
                                                   tags$li(tags$a(id="rnaseqbarplot","Bar Plot"))
                                               )
                                       )
                                   )),
                           hr(),
                           tags$li(tags$a(id="clinicalanalysis","Clinical Analysis"),
                                   br(),
                                   tags$img(src = "clinical_tab.png"),
                                   tags$ol(
                                       tags$li(tags$a(id="survplot_","Survival Plot")), 
                                       tags$li(tags$a(id="survplot","Survival Plot")) 
                                   )),
                           hr(),
                           tags$li(tags$a(id="drug","Drug Finder")),
                           hr())
                   )
                   
                   
            )
        )
    )
)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
    dark = F,
    help = F,
    fullscreen = F,
    scrollToTop = TRUE,
    header = dashboardHeader(
        title = dashboardBrand(
            title = "TCGAnalyzeR",
            color = "primary",
            href = "http://tcganalyzer.mu.edu.tr/",
            opacity = 0.8
        ),
        fixed = TRUE,
        rightUi= shiny::tags$li(
            class = "nav-item dropdown mymenulink",
            shiny::tags$a(shiny::icon("dna"),"My Genes",
                          class = "nav-link dropdown-toggle",
                          `data-toggle` = "dropdown",
                          href = "#"
            ),
            shiny::tags$ul(class="dropdown-menu mymenu2",
                           shiny::tags$li(
                               uiOutput('genes')
                               
                           ),
                           shiny::tags$button("",icon("trash-alt"),title="Delete selected genes",
                                              onclick="delSelected()",class="btn btn-default action-button shiny-bound-input"),
                           uiOutput("clip"))
        )
    ),
    sidebar = dashboardSidebar(
        fixed = TRUE,
        skin = "light",
        status = "primary",
        id = "sidebar",
        sidebarMenu(
            id = "current_tab",
            flat = FALSE,
            compact = FALSE,
            childIndent = TRUE,
            menuItem(
                "Home",
                tabName = "home_tab",
                icon = icon("home")
            ),
            sidebarHeader("Analysis types"),
            menuItem(
                text = "Analysis",
                icon = icon("chart-bar"),
                startExpanded = FALSE,
                menuSubItem(
                    text = HTML(
                        paste(
                            "SNV Analysis"
                        )
                    ),
                    tabName = "snv_tab",
                    icon = icon("angle-double-right")
                ),
                menuSubItem(
                    text = HTML(
                        paste(
                            "CNV Analysis"
                        )
                    ),
                    tabName = "cnv_tab",
                    icon = icon("angle-double-right")
                ),
                menuSubItem(
                    text = HTML(
                        paste(
                            "RNASeq Analysis"
                        )
                    ),
                    tabName = "rnaseq_tab",
                    icon = icon("angle-double-right")
                ),
                menuSubItem(
                    text = HTML(
                        paste(
                            "Clinical Analysis"
                        )
                    ),
                    tabName = "clinical_tab",
                    icon = icon("angle-double-right")
                )
            ),
            sidebarHeader("Drug part"),
            menuItem(
                "DrugFinder",
                tabName = "drug_tab",
                icon = icon("capsules")
            ),
            sidebarHeader("Other boxes"),
            menuItem(
                "About",
                tabName = "about_tab",
                icon = icon("info-circle")
            ),
            menuItem(
                "Help",
                tabName = "help_tab",
                icon = icon("question-circle")
            )
        )
    ),
    body = dashboardBody(tags$head( #use_theme(bs4DashTheme)
        tags$link(
            rel = "stylesheet",
            type = "text/css",
            href = "my.css")
    ),
    useShinyjs(),
    rclipboardSetup(),
        tabItems(
            snv_analysis,
            cnv_analysis,
            rnaseq_analysis,
            clinical_analysis,
            home,
            about,
            analysis,
            helpPage,
            drugpart
        )
    ),
    controlbar = dashboardControlbar(disable=TRUE),
    footer = dashboardFooter(
        fixed = FALSE,
        left = a(
            href = "mailto:basakkabakk@gmail.com",
            target = "_blank", "Contact Us"
        ),
        right = "2021 SuzekLab"
    ),
    title = "TCGAnalyzeR"
)
)
