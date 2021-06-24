
library(shiny)
library(shiny.semantic)
library(leaflet)
library(shinycssloaders)
library(highcharter)
library(dplyr)
library(data.table)
library(leaftime)
library(geosphere)

mData <- fread("ships.csv", header = TRUE, sep = ",",check.names = FALSE,stringsAsFactors = FALSE,fill=T)

mySelectInput <- function(id,label) {
    ns <- NS(id)
    tagList(
        selectInput(ns("mySelect"), label,c(""),type="search selection fluid",default_text="Select vessel type")
    )
    
}

mySelectServer <- function(input, output, session,data,column) {
        
         updateSelectInput(session, "mySelect",choices = unique(data[[column]]))

}


ui <- semanticPage(
    tags$head( # must include css
        tags$style(HTML("
        .grid{
	    min-height: 680px;
        }"
        ))
    ),
    margin = "2vh 15vw 2vh 15vw",
    #theme = "superhero",
    cards(
        style="height:20vh;",
        class = "one",
        card(class = "red",style="width:100%",
             div(class="content",
                 div(class="meta", ""),
                 div(class="description", 
                     
                     flowLayout(
                         mySelectInput("vessel_type","Vessel Type:"),
                         mySelectInput("vessel","Vessel:")
                     ),
                     icon("massive ship",  style="float:right;margin-top: -6vh;")
                 )
             )
        )
    ),
    tabset(menu_class="top attached tabular inverted red ",
           tab_content_class="bottom attached grid segment",
           tabs =
               list(
                   list(menu = "All Observations", 
                        content = 
                            div(
                                br(),
                                conditionalPanel("input['vessel-mySelect'] == '' ",
                                                 label("Please select Vessel type and name", class = " red basic")
                                ),
                                conditionalPanel("input['vessel-mySelect'] != ''",
                                                 leafletOutput("mymap",height = "650px") %>% withSpinner()
                                ),
                            ),
                        id="tab1"
                   ),
                   list(menu = "Timeline", 
                        content = 
                            div(
                                br(),
                                conditionalPanel("input['vessel-mySelect'] == '' ",
                                                 label("Please select Vessel type and name", class = " red basic")
                                ),
                                conditionalPanel("input['vessel-mySelect'] != ''",
                                                 leafletOutput("mymap3",height = "650px") %>% withSpinner()
                                ),
                            ), 
                        id = "tab2"),
                   list(menu = "Longest Distance between Two Consecutive Observations", 
                        content =
                            div(
                                br(),
                                conditionalPanel("input['vessel-mySelect'] == '' ",
                                                 label("Please select Vessel type and name", class = " red basic")
                                ),
                                conditionalPanel("input['vessel-mySelect'] != ''",
                                                 leafletOutput("mymap2",height = "650px") %>% withSpinner()
                                )
                            ), 
                        id = "tab3"),
                   list(menu = "Details", 
                        content = div(
                            br(),
                            conditionalPanel("input['vessel-mySelect'] == '' ",
                                             label("Please select Vessel type and name", class = " red basic")
                            ),
                            conditionalPanel("input['vessel-mySelect'] != ''",
                                             cards(class="three",
                                                   card(class="link red",
                                                        div(class="content",
                                                            div(class="header", icon("angle double right"),"Name"),
                                                            div(class="meta", "Ship's name"),
                                                            div(class="description", 
                                                                textOutput("vessel_name",inline  = T)
                                                            )
                                                        )
                                                   ),
                                                   card(class="link red",
                                                        div(class="content",
                                                            div(class="header", icon("angle double right"),"Type"),
                                                            div(class="meta", "Ship's type"),
                                                            div(class="description", 
                                                                textOutput("vessel_typ",inline  = T)
                                                            )
                                                        )
                                                   ),
                                                   card(class="link red",
                                                        div(class="content",
                                                            div(class="header", icon("angle double right"),"Speed"),
                                                            div(class="meta", "Ship's speed in knots"),
                                                            div(class="description", 
                                                                textOutput("speed_range",inline  = T)
                                                            )
                                                        )
                                                   ),
                                                   card(class="link red",
                                                        div(class="content",
                                                            div(class="header", icon("angle double right"),"Time Sailed"),
                                                            div(class="meta", "Time in hours"),
                                                            div(class="description", 
                                                                textOutput("time_dif",inline  = T)
                                                            )
                                                        )
                                                   ),
                                                   card(class="link red",
                                                        div(class="content",
                                                            div(class="header", icon("angle double right"),"Flag"),
                                                            div(class="meta", "Ship's flag"),
                                                            div(class="description", 
                                                                uiOutput("vessel_flag",inline  = T)
                                                            )
                                                        )
                                                   ),
                                                   card(class="link red",
                                                        div(class="content",
                                                            div(class="header", icon("angle double right"),"Length"),
                                                            div(class="meta", "Ship's length in meters"),
                                                            div(class="description", 
                                                                textOutput("vessel_length",inline  = T)
                                                            )
                                                        )
                                                   ),
                                                   card(class="link red",
                                                        div(class="content",
                                                            div(class="header", icon("angle double right"),"Width"),
                                                            div(class="meta", "Ship's width in meters"),
                                                            div(class="description", 
                                                                textOutput("vessel_width",inline  = T)
                                                            )
                                                        )
                                                   ),
                                                   card(class="link red",
                                                        div(class="content",
                                                            div(class="header", icon("angle double right"),"Dwt"),
                                                            div(class="meta", "Ship's deadweight in tones"),
                                                            div(class="description", 
                                                                textOutput("vessel_dwt",inline  = T)
                                                            )
                                                        )
                                                   ),
                                                   card(class="link red",
                                                        div(class="content",
                                                            div(class="header", icon("angle double right"),"Distance"),
                                                            div(class="meta", "Longest Distance between Two Consecutive Observations"),
                                                            div(class="description", 
                                                                textOutput("vessel_dist",inline  = T)
                                                            )
                                                        )
                                                   )
                                             ),
                                             h4(icon("chart line"),"Number of Observations by Destination",class="ui horizontal divider header"),
                                             highchartOutput("obvervations") %>% withSpinner(),
                                             
                                             h4(icon("chart line"),"Speed",class="ui horizontal divider header"),
                                             highchartOutput("speed") %>% withSpinner()
                                             
                            )
                        ), id = "tab4")
               ),
           active = "tab4",
           id = "exampletabset"
    )
)


get_dist_matrix <- function(inputData){
    dist_matrix <- distm(inputData, fun = distHaversine)
    myMatrix <- c()
    for(i in c(0:ncol(dist_matrix)-1)){
        myMatrix <- c(myMatrix,dist_matrix[i+1,i])
    }
    
    myMatrix
}

server <- function(input, output) {
    
    callModule(mySelectServer,"vessel_type",data = mData,column="ship_type")
    observe({
        mData_1 <- subset(mData,ship_type == input$`vessel_type-mySelect`)
        callModule(mySelectServer,"vessel",data = mData_1,column="SHIPNAME")
    })
    
    mData_1 <- reactive({
        mData_1 <- subset(mData,ship_type == input$`vessel_type-mySelect` & SHIPNAME == input$`vessel-mySelect`)
        mData_1 <- mData_1[order(DATETIME),]
        mData_1
    })
    
    observeEvent(input$`vessel-mySelect`, {
        
        output$vessel_name <- renderText({
            input$`vessel-mySelect`
        })
        
        output$vessel_typ <- renderText({
            input$`vessel_type-mySelect`
        })
        
        output$vessel_speed <- renderText({
            input$vessel_speed
        })
        
        output$time_dif <- renderText({
            difftime(max(mData_1()$DATETIME),min(mData_1()$DATETIME),units = "hour")
        })
        
        output$speed_range <- renderText({
            paste0("Range: ",min(mData_1()$SPEED)," - ",max(mData_1()$SPEED))
        })
        
        output$vessel_length <- renderText({
            unique(mData_1()$LENGTH)
        })
        
        output$vessel_width <- renderText({
            unique(mData_1()$WIDTH)
        })
        
        output$vessel_dwt <- renderText({
            unique(mData_1()$DWT)
        })
        
        output$vessel_flag <- renderUI({
            HTML(paste0("<i class=\"",tolower(unique(mData_1()$FLAG))," flag large\"></i>"))
        })
        
        output$vessel_dist <- renderText({
            lonlat <-  mData_1()[,c('LON','LAT')]
            lonlat <- unique(lonlat)
            myMatrix <- get_dist_matrix(lonlat)
            myMax <-  max(myMatrix)
            myMax
        })
        
        
    })
    
    
    output$obvervations <- renderHighchart({
        
        mData_1() %>%
            count(DESTINATION)%>%
            arrange(n)%>%
            hchart('column', hcaes(x = DESTINATION, y = n ),name="count") %>%
            hc_yAxis(title=list(text="Observation Count"))
    })
    
    output$speed <- renderHighchart({
        
        dat <- data_to_boxplot(mData_1(), SPEED, SHIPNAME, add_outliers=T)
        
        highchart() %>%
            hc_xAxis(type = "category") %>%
            hc_yAxis(title = list(text = "Speed in knots")) %>%
            hc_add_series_list(dat)
    })
    
    output$mymap <- renderLeaflet({
        
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(lng=mData_1()$LON, lat=mData_1()$LAT, popup=paste(mData_1()$LON,mData_1()$LAT))
    })
    
    output$mymap2 <- renderLeaflet({
        lonlat <-  mData_1()[,c('LON','LAT')]
        lonlat <- unique(lonlat)
        myMatrix <- get_dist_matrix(lonlat)
        myMax <-  tail(which(grepl(max(myMatrix), myMatrix)),1)
        
        lonlat_ <- lonlat[c(myMax,myMax+1),]
        
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(lng=lonlat_$LON, lat=lonlat_$LAT, popup=paste(lonlat_$LON,lonlat_$LAT))
    })
    
    output$mymap3 <- renderLeaflet({
        mData_1 <- mData_1()
        power <- data.frame(
            "Latitude" = mData_1$LAT,
            "Longitude" = mData_1$LON,
            "start" = mData_1$DATETIME,
            "end" = mData_1$DATETIME[c(2:length(mData_1$DATETIME),mData_1$DATETIME[length(mData_1$DATETIME)]+1)]
        )
        
        # use geojsonio to convert our data.frame
        #  to GeoJSON which timeline expects
        power_geo <- geojsonio::geojson_json(power,lat="Latitude",lon="Longitude")
        
        # we can add data in addTimeline
        leaflet() %>%
            addTiles() %>%
            setView(mData_1$LON[1],mData_1$LAT[1],10) %>%
            addTimeline(data = power_geo,
                        timelineOpts = timelineOptions(
                            styleOptions = styleOptions(
                                radius = 10,
                                color = "black",
                                fillColor = "red",
                                fillOpacity = 1
                            )
                        ),width = "96%")
    })
    
}


shinyApp(ui = ui, server = server)
