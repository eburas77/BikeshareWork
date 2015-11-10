# Ramnath Vaidyanathan
#
# github site: 
# https://github.com/ramnathv
#
# author of rCharts, slidify
#
# live bikeshare site and R code to create it at
#
# http://ramnathv.github.io/bikeshare/

# Code from that site (may need to be modified), retrieved 8/20/13

# Need to install the following packages

require(RJSONIO)
require(RColorBrewer)
require(httr)
require(shiny)
require(rCharts) # this needs to be installed from github, as it's not on any repository yet! 

getCenter <- function(nm, networks){
  net_ = networks[[nm]]
  lat = as.numeric(net_$lat)/10^6;
  lng = as.numeric(net_$lng)/10^6;
  return(list(lat = lat, lng = lng))
}

getData <- function(network = 'citibikenyc'){
  url = sprintf('http://api.citybik.es/%s.json', network)
  bike = fromJSON(content(GET(url)))
  lapply(bike, function(station){within(station, { 
    fillColor = cut(
      as.numeric(bikes)/(as.numeric(bikes) + as.numeric(free)), 
      breaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), 
      labels = brewer.pal(5, 'RdYlGn'),
      include.lowest = TRUE
    ) 
    popup = iconv(whisker::whisker.render(
      '<b></b><br>
        <b>Free Docks: </b>  <br>
         <b>Available Bikes:</b> 
        <p>Retrieved At: </p>'
    ), from = 'latin1', to = 'UTF-8')
    latitude = as.numeric(lat)/10^6
    longitude = as.numeric(lng)/10^6
    lat <- lng <- NULL})
  })
}

plotMap <- function(network = 'citibikenyc', width = 1600, height = 800){
  data_ <- getData(network); center_ <- getCenter(network, networks)
  L1 <- Leaflet$new()
  L1$tileLayer(provider = 'Stamen.TonerLite')
  L1$set(width = width, height = height)
  L1$setView(c(center_$lat, center_$lng), 13)
  L1$geoJson(toGeoJSON(data_), 
             onEachFeature = '#! function(feature, layer){
      layer.bindPopup(feature.properties.popup)
    } !#',
             pointToLayer =  "#! function(feature, latlng){
      return L.circleMarker(latlng, {
        radius: 4,
        fillColor: feature.properties.fillColor || 'red',    
        color: '#000',
        weight: 1,
        fillOpacity: 0.8
      })
    } !#")
  L1$enablePopover(TRUE)
  L1$fullScreen(TRUE)
  return(L1)
}

getNetworks <- function(){
  if (!file.exists('networks.json')){
    url <- 'http://api.citybik.es/networks.json'
    dat <- content(GET(url))
    writeLines(dat, 'networks.json')
  }
  networks <- RJSONIO::fromJSON('networks.json')
  nms <- lapply(networks, '[[', 'name')
  names(networks) <- nms
  return(networks)
}

networks <- getNetworks()
shinyUI(bootstrapPage( 
  tags$link(href='style.css', rel='stylesheet'),
  tags$script(src='app.js'),
  includeHTML('www/credits.html'),
  selectInput('network', '', sort(names(networks)), 'citibikenyc'),
  mapOutput('map_container')
))

shinyServer(function(input, output, session){
  output$map_container <- renderMap({
    plotMap(input$network)
  })
})