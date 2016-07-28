# Jeanette Newmiller
# July 24, 2016
# R Shiny App for survey project


library(shiny)
library(ggplot2)
library(plotrix)
library(googlesheets)
library(dplyr)

# use to determine the needed key for google sheet
# (my_sheets <- gs_ls())
# # (expect a prompt to authenticate with Google interactively HERE)
# my_sheets %>% glimpse()


# Grab the Google Sheets
sheet <- gs_key("1LXBrz-JCwct2zTLYdkgsj-OKER9dofl4Qkixh2BPrEc")
sheetRiddles <- gs_key("1S5PSwSPU8co-EnkvII6BKO0DPxAOBtzjc5nCS5hpEx0")
dtaRiddles <- gs_read(sheetRiddles)

# constant 
DistOffset <- 4.6 # survey tool distance from center of dowel to measurement mark

# functions
saveData <- function(data) {
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

PlotData <- function(yCoords, xCoords, Group = NULL){
    dta <- data.frame(X = xCoords, Y = yCoords, Group = Group)
        ggplot(dta, aes( x = xCoords , y = yCoords))+
        geom_point( size = 4, aes(color=Group))+
        coord_cartesian(xlim = 0:100, ylim = 0:100)+
        coord_equal()
}

Convert2xCoords <- function(distance, angle, DistOffset){
    (distance + DistOffset) * cos(angle*pi/180)
}
Convert2yCoords <- function(distance, angle, DistOffset){
    (distance + DistOffset) * sin(angle*pi/180)
}

# ***************************

# Define UI for application 
list(
    ui <- fluidPage( headerPanel(img(src = "https://watershed.ucdavis.edu/files/cwsheader_0.png"))
        , titlePanel( h5("Girls Saving the World Through Engineering - Land Survey of a Watershed Model", style = "color:#307EAE"))
        , tabsetPanel(
        tabPanel( "Add Group Data"
                , headerPanel("Add Data")
                , sidebarLayout(
                    sidebarPanel( numericInput("group", "Group Number:", value = NULL, min = 1, max = 10)
                                , numericInput("angle", "Angle:",value = NULL, min = 0, max = 90)
                                , numericInput("distance", "Distance:", NULL, min = 0, max = 100)
                                , numericInput("elevation", "Elevation:", NULL, min = 0, max = 20)
                                , actionButton("addButton", "Submit Group Data")
                                , width = 2
                                )
                    , mainPanel( (verbatimTextOutput(outputId = "answer"))
                               , (verbatimTextOutput(outputId = "question")) 
                    )
                            )
                ),
        tabPanel( "Display Group Data"
                , headerPanel("Our Group Data")
                , sidebarLayout(sidebarPanel( tableOutput("tableGroup")
                                            , width = 4
                                            )
                               , mainPanel(
                                    verticalLayout( 
                                        plotOutput("plotGroupPolar")
                                        , plotOutput("plotGroup")
                                        )
                                    )
                                )
                ),
        tabPanel( "Display Class Data"
                , headerPanel("Class Data")
                , sidebarLayout(sidebarPanel( 
                                    verticalLayout( 
                                        actionButton("getButton", "Retrive Class Data")
                                        , tableOutput("tableClass")
                                        )
                                    , width = 4
                                    )
                               , mainPanel(
                                    verticalLayout( 
                                        plotOutput("plotClassPolar")
                                        , plotOutput("plotClass")
                                        )
                                    )
                                )
                )
            )
        )
    )



# Define server logic 
server <- function(input, output, session) {
    dta1 <- data.frame( "Group"     = numeric(0)
                      , "Angle"     = numeric(0)
                      , "Distance"  = numeric(0)
                      , "Elevation" = numeric(0)
                      )
    valuesGroup    <- reactiveValues()
    valuesGroup$df <- dta1
    valuesClass    <- reactiveValues()
    valuesClass$df <- dta1
    valuesIndx     <- reactiveValues()
    valuesIndx$MsgA <- NA
    valuesIndx$MsgB <- sample( x = (1:nrow(dtaRiddles)), size = 1 )
    observe(
        if(input$addButton > 0){
            newLine <- isolate(c( input$group
                                , input$angle
                                , input$distance
                                , input$elevation
                                )
                              )
            isolate(valuesGroup$df <- rbind(as.matrix(valuesGroup$df), unlist(newLine)))
            saveData(unlist(newLine))
        }
    )
    observe(
        if(input$addButton > 0){
            valuesIndx$MsgA <- isolate(valuesIndx$MsgB)
            valuesIndx$MsgB <- sample( x = (1:nrow(dtaRiddles)), size = 1 )
        }
    )
    observe(
        if(input$getButton > 0){
            dtaClass <- gs_read(sheet)
            valuesClass$df <- data.frame(dtaClass[-1,])
        }
    )
     observe(
         if(is.na(valuesIndx$MsgA)){
            output$answer <- renderText(paste(""))
         }else{
              output$answer <- renderText(paste( "Question: "
                                               , dtaRiddles[[2]][valuesIndx$MsgA]
                                               , "Answer: "
                                               , dtaRiddles[[3]][valuesIndx$MsgA]
                                               , sep="\n"
                                               )
                                          )
         }
     )
    output$question <- renderText(paste("Question: "
                                 , dtaRiddles[[2]][valuesIndx$MsgB]
                                 , sep="\n"
                                     )
                                 )
    
    output$tableGroup <- renderTable({valuesGroup$df[, -1, drop = FALSE]})
    output$plotGroup <- renderPlot({
        dist <- valuesGroup$df[,3]
        ang  <- valuesGroup$df[,2]
        
        PlotData( yCoords = Convert2yCoords(distance = dist, angle = ang, DistOffset = DistOffset)
                , xCoords = Convert2xCoords(distance = dist, angle = ang, DistOffset = DistOffset)
                , Group = as.factor(valuesGroup$df[,1])
                )
        })
    
    output$plotGroupPolar <- renderPlot({
        polar.plot( lengths =  valuesGroup$df[,3]
                  , polar.pos =  valuesGroup$df[,2]
                  , rp.type = 's'
#                 , labels = c(0,90,180,270)
#                 , label.pos = c(0,pi/2,pi,3*pi/2)
                  , point.symbols = 15 #c(21,22,23,24,25,8)
                  , point.col = "red"
                  , show.grid.labels = FALSE
                  , radial.lim = c(0, 100)
            )
    })
    
    output$tableClass <- renderTable(valuesClass$df)
    output$plotClass <- renderPlot({
        dist <- valuesClass$df[,3]
        ang  <- valuesClass$df[,2]
        
        PlotData( yCoords = Convert2yCoords(distance = dist, angle = ang, DistOffset = DistOffset)
                , xCoords = Convert2xCoords(distance = dist, angle = ang, DistOffset = DistOffset)
                , Group = as.factor(valuesClass$df[,1])
                )
        })
    
        output$plotClassPolar <- renderPlot({
        polar.plot( lengths =  valuesClass$df[,3]
                  , polar.pos =  valuesClass$df[,2]
                  , rp.type = 's'
#                 , labels = c(0,90,180,270)
#                 , label.pos = c(0,pi/2,pi,3*pi/2)
                  , point.symbols = 15 #c(21,22,23,24,25,8)
                  , point.col = "red"
                  , show.grid.labels = FALSE
                  , radial.lim = c(0, 100)
            )
        })

    # 
    # output$txtSession <- renderPrint({
    #     session$clientData
    # })
    }

# Run the application 
shinyApp(ui = ui, server = server)

# Host the application:
# check for IP address and enter as host = ""
# copy line to console 
# runApp('MapData/app.R', host = "168.150.102.1", port = 5050)

# Upload to shinyapps.io
#rsconnect::deployApp('MapData')
