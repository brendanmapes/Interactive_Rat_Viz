#install.packages('rsconnect')
#library(rsconnect)


# imports -----------------------------------------------------------------


library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(lubridate)
library(leaflet.extras)

library(magrittr)   # chain operators, e.g. to "pipe" a value forward
#library(plyr)
library(tidyverse)
library(DT)
library(knitr)
library(maps)
library(rgdal)
library(ggmap)
library(tmap)
library(sp)
library(tmap)
library(sf)
library(stars)
library(spData)
library(classInt)
library(lattice)
library(grid)
library(pals)


# prajwal's code ----------------------------------------------------------


# Download the data from https://data.cityofnewyork.us/api/views/3q43-55fe/rows.csv?accessType=DOWNLOAD
# Alternate link: https://data.cityofnewyork.us/Social-Services/Rat-Sightings/3q43-55fe, click Export -> CSV

#rat_sightings <- read.csv("data/Rat_Sightings.csv")
# rat_sightings <- read.csv("https://data.cityofnewyork.us/api/views/3q43-55fe/rows.csv?accessType=DOWNLOAD")
# 
# rat_sightings$latitude <- rat_sightings$Latitude
# rat_sightings$longitude <- rat_sightings$Longitude
# 
# #set.seed(100)
# rat_sightings_sample <- rat_sightings[sample.int(nrow(rat_sightings), 10000),]
# #rat_sightings_sample <- rat_sightings
# latitude_colnum <- grep('latitude', colnames(rat_sightings_sample))
# longitude_colnum <- grep('longitude', colnames(rat_sightings_sample))
# rat_sightings_sample <- rat_sightings_sample[complete.cases(rat_sightings_sample[,latitude_colnum:longitude_colnum]),]




# read in the main csv file
rat_data<-read.csv("data/rat_data.csv")
rat_data <- rat_data %>%
  mutate(Borough = str_to_title(rat_data$Borough))
tonnage_data<-read.csv("data/dsny_boro_tonnage.csv", stringsAsFactors = FALSE)
#head(rat_data)
#head(tonnage_data)



ton_date <- tonnage_data %>%
  mutate(MONTH = paste(MONTH, " / 01")) %>%
  mutate(MONTH = as.Date(MONTH, format = '%Y / %m / %d')) %>%
  filter(MONTH > as.Date('2020-01-01', '%Y-%m-%d'), MONTH < as.Date('2021-03-01', '%Y-%m-%d')) %>%
  arrange(desc(MONTH))
#ton_date



rat_date <- rat_data %>%
  mutate(Created.Date = as.Date(Created.Date, "%m/%d/%Y")) %>%
  mutate(Created.Date = as.character(Created.Date)) %>%
  mutate(Created.Date = substr(Created.Date, 1, 8)) %>%
  mutate(Created.Date = paste(Created.Date, '01')) %>%
  mutate(Created.Date = as.Date(Created.Date, "%Y-%m-%d")) %>%
  group_by(Created.Date, Borough) %>%
  tally() %>%
  filter(Created.Date > as.Date('2020-01-01', '%Y-%m-%d'), Created.Date < as.Date('2021-03-01', '%Y-%m-%d')) %>%
  arrange(desc(Created.Date))
#rat_date


rat_ton_date <- merge(rat_date, ton_date, by.x = c("Created.Date", "Borough"), by.y = c("MONTH", "BOROUGH")) %>%
  mutate(rate = n / (REFUSETONSCOLLECTED / 100))
#rat_ton_date




# pratishta's code ---------------------------------------------------------------


convertToShpDistrict <- function(com_district) {
  sapply(com_district, function(com_district) { 
    split = strsplit(com_district, " ")
    boro = case_when (str_to_lower(split[[1]][2]) == 'brooklyn' ~ 'BK',
                      str_to_lower(split[[1]][2]) == 'manhattan' ~ 'MN',
                      str_to_lower(split[[1]][2]) == 'queens' ~ 'QW',
                      str_to_lower(split[[1]][2]) == 'staten' ~ 'SI',
                      str_to_lower(split[[1]][2]) == 'bronx' ~ 'BX'
    );
    ans <- paste(boro, split[[1]][1], sep="")
    return (ans)
  })
}


full_tonnage <-read.csv("sanitation_data/dsny_full_tonnage.csv", stringsAsFactors = FALSE)
full_tonnage <- full_tonnage %>%
  mutate(district =  paste(full_tonnage$COMMUNITYDISTRICT, str_to_upper(full_tonnage$BOROUGH)))



head(full_tonnage)



district =  paste(full_tonnage$COMMUNITYDISTRICT, str_to_upper(full_tonnage$BOROUGH))


ton_map <- full_tonnage %>%
  mutate(community_district = convertToShpDistrict(district)) %>% 
  group_by(community_district) %>%
  summarise(total_ton = sum(REFUSETONSCOLLECTED))
ton_map




community_district <- paste(rat_data$Community.Board, str_to_upper(rat_data$Community.Board))


rat_map <- rat_data %>%
  mutate(community_district = convertToShpDistrict(community_district)) %>% 
  group_by(community_district) %>%
  tally()
rat_map



# ton_col <- unique(full_tonnage[c("district")]) 
# ratcol <- unique(rat_data[c("Community.Board")]) %>%



rat_borough <- rat_data %>% 
  group_by(Borough) %>%
  tally()
rat_borough



ton_boro <- tonnage_data %>%
  group_by(BOROUGH) %>%
  summarise(total_ton = sum(REFUSETONSCOLLECTED))
ton_boro



rat_ton <- left_join(rat_borough, ton_boro, by = c("Borough" = "BOROUGH"))


rat <- ggplot(rat_ton, aes(y=n, x=Borough)) + 
  geom_bar(position="dodge", stat="identity")
#rat


ton <- ggplot(rat_ton, aes(y=total_ton, x=Borough)) + 
  geom_bar(position="dodge", stat="identity")
#ton



# dual y axis 
# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)
f <- ggplot(rat_ton, aes(x=Borough)) +
  
  geom_bar( aes(y=n), stat='identity', fill=temperatureColor) + 
  geom_bar( aes(y=total_ton), stat='identity', fill=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Number of Rat Sightings",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . + 10000000000000000, name="Metric Ton of Waste ")
  ) + 
  
  # theme_ipsum() +
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  ggtitle("Rat Sightings and Sanitation Waste (Feb 2020 - Feb 2021)")



#f



nyc <- readOGR("sanitation_data/CommunityDistricts/.", "geo_export_d81daad1-2b49-44c3-81d4-72436a58def3")


nyc_sp <- spTransform(nyc, CRS("+proj=longlat +datum=WGS84"))



convertBoroCDToDistrict <- function(borocd) {
  sapply(borocd, function(borocd) { 
    boro_ch = as.character(borocd) 
    boro_n = substr(boro_ch, 1, 1)
    cd_n = substr(boro_ch, 2, 3)
    
    print(boro_ch)
    
    boro = case_when (boro_n == '1' ~ 'MN',
                      boro_n == '2' ~ 'BX',
                      boro_n == '3' ~ 'BK',
                      boro_n == '4' ~ 'QW',
                      boro_n == '5' ~ 'SI'
    )
    
    # print(boro)
    ans <- paste(boro, cd_n, sep="")
    # print(ans)
    return (ans)
  })
}



nyc_sp@data <- nyc_sp@data %>%
  mutate(community_district = convertBoroCDToDistrict(boro_cd))
nyc_sp@data


nyc_sp@data <- left_join(nyc_sp@data, rat_map)
nyc_sp@data



nyc_sp@data <- left_join(nyc_sp@data, ton_map) 
nyc_sp@data


#nyc_sp



# ggplot() + 
#   geom_polygon(data = nyc_sp, aes(fill = n, x = ))
# library(geojsonio)
# spdf <- geojson_read("sanitation_data/DSNYSections/.", "DSNYSections.geojson", what = "sp")
# spdf


# tm_shape(nyc_sp) +
#   tm_fill("n", title = "Rat Sightings in Community Districts")








# shiny code


# user interface for setting layout of plots ----------------------------------------------------------


ui <- fluidPage(
  align = "center",
  titlePanel("Rats and NYC: Exploratory Visualization"),
  strong("Data Visualization (QMSS - G5063) Final Project"),
  br(),
  em("Group N: Brendan Mapes, Prajwal Seth, and Pratishta Yerakala"),
  # sidebarLayout(
  #   sidebarPanel(
  # 
  #     plotlyOutput("cityViz", height = 300),
  # 
  #     plotlyOutput("yearViz", height = 300),
  # 
  #     plotlyOutput("locationViz", height = 300),
  #   ),
  #   mainPanel(
  #     leafletOutput("map", height = "700px"),
  #   )
  # ),
  fluidRow(
    br(),
    align = "center",
    headerPanel("Rat Sightings and Sanitation Waste by Borough"),
    p("Pratishta Yerakala"),
    p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
    strong("strong() makes bold text."),
    em("em() creates italicized (i.e, emphasized) text."),
    br(),
    code("code displays your text similar to computer code"),
    div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
    br(),
    p("span does the same thing as div, but it works with",
      span("groups of words", style = "color:blue"),
      "that appear inside a paragraph."),

    plotlyOutput("pratishta1", width = "70%"),
    
    headerPanel("Hello please change this2"),
    p("p creates a paragraph of text.2"),
    
    br(),
    plotlyOutput("pratishta2", width = "70%"),
    
    headerPanel("Hello please change this3"),
    p("p creates a paragraph of text.3"),
    
    br(),
    plotlyOutput("pratishta3", width = "70%"),
    
    headerPanel("Hello please change this4"),
    p("p creates a paragraph of text4."),
    
    br(),
    plotlyOutput("pratishta4", width = "50%"),
    
    headerPanel("Hello please change this5"),
    p("p creates a paragraph of text.5"),
    
    br(),
    plotlyOutput("pratishta5", width = "50%"),

    br(),
    headerPanel("Hello please change this6"),
    p("p creates a paragraph of text.6"),
    
    plotlyOutput("pratishta6", width = "50%"),
    
    headerPanel("edit this 7!"),
    p("text 7"),
    br(),
    tmapOutput("pratishta7", width = "80%"),
    
    headerPanel("edit this 8"),
    p("text 8"),
    br(),
    tmapOutput("pratishta8", width = "80%"),
    
    headerPanel("edit this 9"),
    p("text 9"),
    br(),
    plotOutput("pratishta9", width = "50%"),
    
    br(),
  )
)


# code for generating the plots  -----------------------------------------------------------------


server <- function(input, output, session) {
  
  # points <- eventReactive(input$recalc, {
  #   cbind(rat_sightings_sample$latitude, rat_sightings_sample$longitude)
  # }, ignoreNULL = FALSE)
  #
  
  
  
  # getColor <- function(rat_sightings_sample, i) {
  #   if(rat_sightings_sample$Status[i] == "Closed") {
  #     "green"
  #   } else if(rat_sightings_sample$Status[i] == "In Progress" | rat_sightings_sample$Status[i] == "Assigned") {
  #     "orange"
  #   } else {
  #     "red"
  #   }}
  # 
  # markerColors <- rep(NA, nrow(rat_sightings_sample))
  # 
  # for (i in 1:nrow(rat_sightings_sample)){
  #   markerColors[i] <- getColor(rat_sightings_sample, i)
  # }
  # 
  # 
  # icons <- awesomeIcons(
  #   icon = 'ios-close',
  #   iconColor = 'black',
  #   library = 'ion',
  #   markerColor = markerColors
  # )
  # 
  # 
  # output$map <- renderLeaflet({
  #   leaflet(data = rat_sightings_sample) %>%
  #     addProviderTiles(providers$Stamen.TonerLite,
  #                      options = providerTileOptions(noWrap = TRUE)
  #     ) %>%
  #     setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
  #     addAwesomeMarkers( ~longitude, ~latitude, clusterOptions = markerClusterOptions() ,icon = icons, 
  #                        popup = as.character(paste('Created date:', rat_sightings_sample$Created.Date,'<br>',
  #                                                   'Complaint type:',rat_sightings_sample$Complaint.Type,'<br>',
  #                                                   'Descriptor:',rat_sightings_sample$Descriptor,'<br>',
  #                                                   'Address:',rat_sightings_sample$Incident.Address,'<br>',
  #                                                   'Status:', rat_sightings_sample$Status))) %>%
  #     addHeatmap( ~longitude, ~latitude, group = "heat",max=1, blur = 30) %>% addLegend("topleft", 
  #                                                                                       colors =c('green',  "orange", "red"),
  #                                                                                       labels= c("Closed", "In Progress/Assigned","Open/Pending"),
  #                                                                                       title= "Case status",
  #                                                                                       opacity = 1)
  #   
  # })
  # 
  # output$cityViz <- renderPlotly({
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   tmp <- (zipsInBounds() %>% count(City))
  #   tmp <- tmp[order(-tmp$n),]
  #   tmp <- tmp[1:5,]
  #   ggplotly(
  #     ggplot(tmp, aes(x=City, y=n, fill = City)) + geom_bar(stat="identity") + ylab("Top 5 visible buroughs") + theme(legend.position = "none") + scale_color_brewer(palette="Dark2")+
  #       theme(axis.title.x=element_blank(),
  #             axis.ticks.x=element_blank())
  #   )
  # })
  # 
  # 
  # output$locationViz <- renderPlotly({
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   tmp <- (zipsInBounds() %>% count(Location.Type))
  #   tmp <- tmp[order(-tmp$n),]
  #   tmp <- tmp[1:5,]
  #   ggplotly(
  #     ggplot(tmp, aes(x=Location.Type, y=n, fill = Location.Type, show.legend = FALSE)) + geom_bar(stat="identity") + ylab("Visible location types") +
  #       theme(axis.title.x=element_blank(),
  #             axis.text.x=element_blank(),
  #             axis.ticks.x=element_blank())
  #   )
  # })
  # 
  output$pratishta1 <- renderPlotly({
    p <- ggplot(rat_ton_date, aes(x=Created.Date, y=REFUSETONSCOLLECTED)) +
      geom_line(aes(color = Borough)) +
      geom_point(aes(color = Borough)) +
      xlab("Date by Months") +
      ylab("Weight of Waste (Tons)")
    p
  })

  output$pratishta2 <- renderPlotly({
    p <- ggplot(rat_ton_date, aes(x=Created.Date, y=n)) +
      geom_line(aes(color = Borough)) +
      geom_point(aes(color = Borough)) +
      xlab("Date by Months") +
      ylab("Number of rat sightings")
    p
  })

  output$pratishta3 <- renderPlotly({
    p <- ggplot(rat_ton_date, aes(x=Created.Date, y=rate)) +
      geom_line(aes(color = Borough)) +
      geom_point(aes(color = Borough)) +
      xlab("Date by Months") +
      ylab("Rate of rats per kiloton of waste")
    p
  })
  
  output$pratishta4 <- renderPlotly({
    ton
    
  })
  
  output$pratishta5 <- renderPlotly({
    rat
    
  })
  
  
  
  output$pratishta6 <- renderPlotly({
    f

  })
  
  
  
  output$pratishta7 <- renderTmap({
    ## ------------------------------------------------------------------------
    tm_shape(nyc_sp) +
      tm_fill("n", title = "Rat Sightings in Community Districts")


  })

  output$pratishta8 <- renderTmap({

    ## ------------------------------------------------------------------------
    tm_shape(nyc_sp) +
      tm_fill("total_ton", title = "Tones of Waste and Rat Sightings by DSNY Districts")




  })

  output$pratishta9 <- renderPlot({
    ## ------------------------------------------------------------------------
    legend_creator = function(col.regions, xlab, ylab, nbins){
      bilegend = levelplot(matrix(1:(nbins * nbins), nrow = nbins),
                           axes = FALSE, col.regions = col.regions,
                           xlab = xlab, ylab = ylab,
                           cuts = 8, colorkey = FALSE, scales = list(draw = 0))
      bilegend
    }
    add_new_var = function(x, var1, var2, nbins, style = "quantile"){
      class1 = suppressWarnings(findCols(classIntervals(c(x[[var1]]),
                                                        n = nbins,
                                                        style = style)))

      class2 = suppressWarnings(findCols(classIntervals(c(x[[var2]]),
                                                        n = nbins,
                                                        style = style)))

      x$new_class = class1 + nbins * (class2 - 1)
      return(x)
    }


    ## ------------------------------------------------------------------------
    nyc_cd <- nyc_sp


    ## ------------------------------------------------------------------------
    nyc_cd = add_new_var(nyc_cd,
                         var1 = "n",
                         var2 = "total_ton",
                         nbins = 3)


    ## ------------------------------------------------------------------------
    bilegend = legend_creator(stevens.pinkblue(n = 9),
                              xlab = "rats",
                              ylab = "total tonnes",
                              nbins = 3)
    
    vp = viewport(x = 0.25, y = 0.25, width = 0.25, height = 0.25)
    pushViewport(vp)
    #print(bilegend, newpage = FALSE)
    
    bimap = tm_shape(nyc_cd) +
      tm_fill("new_class", style = "cat", palette = stevens.pinkblue(n = 9), legend.show = FALSE) +
      tm_layout(legend.show = FALSE)
    grid.newpage()
    vp = viewport(x = 0.35, y = 0.75, width = 0.25, height = 0.25)
    print(bimap, vp = viewport())
    pushViewport(vp)
    print(bilegend, newpage = FALSE, vp = vp)
  })
  
  
  
  
  
  #plot_created_year
  
  # output$yearViz <- renderPlotly({
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   created_date_sample <- data.table(zipsInBounds()$Created.Date)
  #   created_date_sample$dates <- parse_date_time(created_date_sample$V1, '%m/%d/%y %I:%M:%S %p')
  #   plot_created_year <- data.frame(table(year(date(created_date_sample$dates))))
  #   for (i in 2010:2021){
  #     if ((i %in% plot_created_year$Var1)==FALSE) {
  #       #print(i)
  #       tmp_df <- data.frame(toString(i), 0)
  #       names(tmp_df) <- c('Var1','Freq')
  #       plot_created_year <- rbind(plot_created_year, tmp_df)
  #     }
  #   }
  #   plot_created_year$Var1 <- as.numeric(as.character(plot_created_year$Var1))
  #   plot_created_year <- plot_created_year[order(plot_created_year$Var1),]
  #   plot_created_year <- filter(plot_created_year, Var1 != 2021)
  # 
  #   ggplotly(
  #     ggplot(data=plot_created_year, aes(x=Var1, y=Freq)) + geom_path(stat="identity") + ylab('Rat sightings in last 10 years') +
  #       theme(axis.title.x=element_blank(),
  #             axis.text.x=element_blank(),
  #             axis.ticks.x=element_blank())
  #   )
  # })
  # 
  # zipsInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(zipdata[FALSE,])
  #   bounds <- input$map_bounds
  #   #print(bounds)
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #   #print(latRng)
  #   
  #   subset(rat_sightings_sample,
  #          latitude >= latRng[1] & latitude <= latRng[2] &
  #            longitude >= lngRng[1] & longitude <= lngRng[2])
  # })
  
}
shinyApp(ui = ui, server = server)