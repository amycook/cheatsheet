library(shiny)
library(leaflet)
library("BrisbaneBikeways")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

geom.subset <- function( spgeom, top, left, bottom, right ){
        
        coords <- data.frame(y = c(left, right, right, left ), x =c(top, top, bottom, bottom) )
        poly <-  sp::Polygon(coords)
        polys <- sp::Polygons( list(poly) , ID =0)
        polys.spatial <- SpatialPolygons(list(polys), proj4string =  spgeom@proj4string)
        inter <- rgeos::gIntersection(spgeom, polys.spatial, byid =TRUE)
        
} 
geom.subset.box <- function( spgeom, top, left, bottom, right ){
        
        coords <- data.frame(y = c(left, right, right, left ), x =c(top, top, bottom, bottom) )
        poly <-  sp::Polygon(coords)
        polys <- sp::Polygons( list(poly) , ID =0)
        polys.spatial <- SpatialPolygons(list(polys))
        
} 
bikeways <- bikeways2015[grepl('BRISBANE', bikeways2015$suburb),]

ui <- fluidPage(
        leafletOutput("mymap")
)

server <- function(input, output, session) {
        
        inter = reactive({
                if(is.null(input$mymap_bounds)){
                        print('ohno1')
                        a = bikeways
                } else {
                a = geom.subset(bikeways, top = input$mymap_bounds$north,
                            left = input$mymap_bounds$west,
                            bottom = input$mymap_bounds$south,
                            right = input$mymap_bounds$east
                                    )
                print(length(a@lines))
                }
                
                print(length(bikeways@lines))
                return(a)
                # geom.subset(bikeways2015, -27.460070, 153.020613, -27.480243, 153.030741)
        })
        
        box = reactive({
                if(is.null(input$mymap_bounds)){
                        print('ohno2')
                        bikeways
                } else {
                        geom.subset.box(bikeways, top = input$mymap_bounds$north,
                                    left = input$mymap_bounds$west,
                                    bottom = input$mymap_bounds$south,
                                    right = input$mymap_bounds$east
                        )
                }
                # geom.subset(bikeways2015, -27.460070, 153.020613, -27.480243, 153.030741)
        })
        
        #observe({ print(input$mymap_bounds$north %>% class)})
        #observe({ print(is.null(input$mymap_bounds))})
        map = leaflet() %>% addTiles()
        
        output$mymap <- renderLeaflet({
                print('hello')
                leaflet() %>% addTiles() %>%
                        setView(lat = -27.47228, lng= 153.0292, zoom = 12)
                
        })
        
         
        observe({
                leafletProxy("mymap") %>%
                        addPolylines(data=inter()) #%>%
                        #addPolylines(data = box())
                
        })
}

shinyApp(ui, server)