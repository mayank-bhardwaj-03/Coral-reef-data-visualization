#Task 5
library(shiny)
library(leaflet)
library(ggplot2)
# reading the data
coraldata <- read.csv("assignment-02-data-formated.csv")
# ui start
ui <- fluidPage( 
  
  # Application title
  headerPanel("Bleaching of coral for different sites "),
  
  # Sidebar with controls to select the variable to plot against smoothers
  sidebarLayout(
    sidebarPanel(
      selectInput("coral", "Coral Type:", 
                  c(
                    "Hard corals" = "hard corals",
                    "Blue corals" = "blue corals", 
                    "Sea fans" = "sea fans",
                    "Sea pens" = "sea pens",
                    "Soft corals" = "soft corals")
      ),
      
      selectInput("smoother", "Smoother:", 
                  c("lm" = "lm", 
                    "glm" = "glm",
                    "gam" = "gam",
                    "loess" = "loess",
                    "auto" = "auto"))
      
    ),
    
    # Show the caption and plot
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("coralplot"),
      leafletOutput("coralleafplot")
    )
  )
)

server <- function(input, output) {

    # create a reactive value that will store the click position
    data_of_click <- reactiveValues(clickedMarker=NULL)
    # store the click
    observeEvent(input$coralleafplot_marker_click,{
      data_of_click$clickedMarker <- input$coralleafplot_marker_click
    })
    # Generate a plot of the requested variable 
    output$coralplot=renderPlot({
      if(is.null(data_of_click$clickedMarker)){
        ggplot(subset(coraldata,coralType==input$coral), aes(year, value)) + 
          geom_point(aes(color = coralType),size=4) +
          facet_grid(reorder(location, latitude)~coralType)+ 
          theme(axis.text.y=element_blank())+
          geom_smooth(aes(group = 1),
                      method = input$smoother,
                      color = "black", size = 1)
      }
      else{
      my_place_lat=data_of_click$clickedMarker$lat
      my_place_lng=data_of_click$clickedMarker$lng
      # coral type caption.
      formulaText <- reactive({
        paste("coralType~", input$coral,"~",input$smoother)
      })
      # Return the formula text for printing as a caption
      output$caption <- renderText({formulaText()})
      
      ggplot(subset(coraldata,coralType==input$coral & latitude==my_place_lat
                    & longitude==my_place_lng ), aes(year, value)) + 
        geom_point(aes(color = coralType),size=4) +
        facet_grid(reorder(location, latitude)~coralType)+
        
        theme(strip.text.x = element_text(size = 14),
              strip.text.y = element_text(size = 14))+
        theme(axis.text.y=element_blank())+
        theme( axis.line = element_line(colour = "Black", 
                                        size = 1, linetype = "solid"))+ 
        theme(axis.text.x = element_text(face="bold", color="#993333",size=14),
              axis.text.y = element_text(face="bold", color="#993333",size=14, angle=45))+
        theme(axis.title=element_text(size=14,face="bold"))+
        geom_smooth(aes(group = 1),
                    method = input$smoother,
                    color = "black", size = 1)
      }
          
    })
 # leaflet plot starts
  output$coralleafplot <- renderLeaflet( {pal <- colorFactor(c("navy",
                                                      "red",
                                                      "green",
                                                      "purple",
                                                      "yellow",
                                                      "pink",
                                                      "black",
                                                      "cyan"),
                                                    domain = coraldata$location)
  #applying marker for leaflet
  leaflet(coraldata) %>% addTiles() %>%
    addCircleMarkers(~longitude, ~latitude,
                     color = ~pal(location),
                     stroke = FALSE, fillOpacity = 0.5
    ) %>%
    addLegend("topright", pal = pal, values = coraldata$location,
              title = "Coral Sites",
              opacity = 1)
    })
  
}
shinyApp(ui, server)

