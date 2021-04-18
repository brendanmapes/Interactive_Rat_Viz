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
rat_sightings <- read.csv("https://data.cityofnewyork.us/api/views/3q43-55fe/rows.csv?accessType=DOWNLOAD")

rat_sightings$latitude <- rat_sightings$Latitude
rat_sightings$longitude <- rat_sightings$Longitude

#set.seed(100)
rat_sightings_sample <- rat_sightings[sample.int(nrow(rat_sightings), 20000),]
#rat_sightings_sample <- rat_sightings
latitude_colnum <- grep('latitude', colnames(rat_sightings_sample))
longitude_colnum <- grep('longitude', colnames(rat_sightings_sample))
rat_sightings_sample <- rat_sightings_sample[complete.cases(rat_sightings_sample[,latitude_colnum:longitude_colnum]),]
rat_sightings_sample$year_created <- year(parse_date_time(rat_sightings_sample$Created.Date, '%m/%d/%y %I:%M:%S %p'))

rat_sightings_buroughs <- as.character(unique(unlist(rat_sightings_sample$Borough)))
rat_sightings_case_status <- as.character(unique(unlist(rat_sightings_sample$Status)))


#c("BROOKLYN", "QUEENS","STATEN ISLAND")
#rat_sightings_buroughs <- c("BROOKLYN", "QUEENS","STATEN ISLAND")


# PRATISHTA'S DATA FETCH -------------------------------------------------------
# read in the main csv file
rat_data<-read.csv("data/rat_data.csv")
rat_data <- rat_data %>%
  mutate(Borough = str_to_title(rat_data$Borough))
tonnage_data<-read.csv("data/dsny_boro_tonnage.csv", stringsAsFactors = FALSE)

ton_date <- tonnage_data %>%
  mutate(MONTH = paste(MONTH, " / 01")) %>%
  mutate(MONTH = as.Date(MONTH, format = '%Y / %m / %d')) %>%
  filter(MONTH > as.Date('2020-01-01', '%Y-%m-%d'), MONTH < as.Date('2021-03-01', '%Y-%m-%d')) %>%
  arrange(desc(MONTH))

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

rat_ton_date <- merge(rat_date, ton_date, by.x = c("Created.Date", "Borough"), by.y = c("MONTH", "BOROUGH")) %>%
  mutate(rate = n / (REFUSETONSCOLLECTED / 100))
# END OF PRATISHTA'S DATA FETCH ------------------------------------------------

# PRATISHTA'S CODE -------------------------------------------------------------

# community district conversion functions 

convertBoroCDToDistrict <- function(borocd) {
  sapply(borocd, function(borocd) { 
    boro_ch = as.character(borocd) 
    boro_n = substr(boro_ch, 1, 1)
    cd_n = substr(boro_ch, 2, 3)
    
    boro = case_when (boro_n == '1' ~ 'MN',
                      boro_n == '2' ~ 'BX',
                      boro_n == '3' ~ 'BK',
                      boro_n == '4' ~ 'QW',
                      boro_n == '5' ~ 'SI'
    )
    
    ans <- paste(boro, cd_n, sep="")
    return (ans)
  })
}

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

# reading in data and modify string format of community district column
full_tonnage <-read.csv("sanitation_data/dsny_full_tonnage.csv", stringsAsFactors = FALSE)
full_tonnage <- full_tonnage %>%
  mutate(district =  paste(full_tonnage$COMMUNITYDISTRICT, str_to_upper(full_tonnage$BOROUGH)))

district =  paste(full_tonnage$COMMUNITYDISTRICT, str_to_upper(full_tonnage$BOROUGH))

# creating data to be mapped
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

ton <- ggplot(rat_ton, aes(y=total_ton, x=Borough)) + 
  geom_bar(position="dodge", stat="identity")

# map data
nyc <- readOGR("sanitation_data/CommunityDistricts/.", "geo_export_d81daad1-2b49-44c3-81d4-72436a58def3")

nyc_sp <- spTransform(nyc, CRS("+proj=longlat +datum=WGS84"))

nyc_sp@data <- nyc_sp@data %>%
  mutate(community_district = convertBoroCDToDistrict(boro_cd))
nyc_sp@data

nyc_sp@data <- left_join(nyc_sp@data, rat_map)
nyc_sp@data

nyc_sp@data <- left_join(nyc_sp@data, ton_map) 
nyc_sp@data
# END OF PRATISHTA'S CODE-------------------------------------------------------

# shiny code


# user interface for setting layout of plots ----------------------------------------------------------


ui <- fluidPage(
  align = "center",
  h1("Rats and NYC: Exploratory Visualization"),
  strong("Data Visualization (QMSS - G5063) Final Project"),
  br(),
  em("Group N: Brendan Mapes, Prajwal Seth, and Pratishta Yerakala"),
  fluidRow(
    align = "center",
    headerPanel("Hello 1!"),
    p("p creates a paragraph of text."),
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
  ),
  fluidRow(
    style = 'margin-right:0px;',
    sidebarLayout(
      sidebarPanel(
        width = 5,
        style = 'margin-right:0px;',
        sliderInput(
          "year_input",
          label = h4("Select years"),
          min = 2010,
          max = 2021,
          value = c(2010, 2021),
          step = 1,
          format = "####"
        ),
        
        
        #selected = rat_sightings_buroughs[1:length(multiInput)])
        #selected = rat_sightings_buroughs,
        
        selectizeInput(
          "burough_input",
          label = h4("Select boroughs"),
          choices = rat_sightings_buroughs,
          multiple = TRUE,
          selected = rat_sightings_buroughs
        ),
        selectizeInput(
          "case_status",
          label = h4("Select status"),
          choices = rat_sightings_case_status,
          multiple = TRUE,
          selected = rat_sightings_case_status
        ),
        
        #plotlyOutput("cityViz", height = 300),
        
        plotlyOutput("yearViz", height = 300),
        
        #plotlyOutput("locationViz", height = 300),
      ),
      mainPanel(width = 7,
                leafletOutput("map", height = 700),)
    ),
  ),
  # PRATISHTA'S WRITEUP --------------------------------------------------------
  fluidRow(
    align = "center",
    h2("Rat Sightings and Sanitation Waste by Borough"),
    h3("Pratishta Yerakala"),
    br(),
    
  ),
  fluidRow(
    tags$style(".padding {
                            margin-left:30px;
                            margin-right:30px;
                          }"),
    tags$style(".leftAlign{float:left;}"),
      align = "left",
      div(class='padding',
          h4("Data used:"),
          h5(a("Rat sightings", href="https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9"), h6("filtered for rodent sightings between Feb 2020 and Feb 2021.")),
          h5(a("DSNY Monthly Tonnage", href="https://data.cityofnewyork.us/City-Government/DSNY-Monthly-Tonnage-Data/ebb7-mvp5"), h6("filtered for months between Feb 2020 and Feb 2021.")),
          h5(a("NYC Community Districts Shapefile", href="https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4")),
          ),
  
      div(class='padding', 
      h4("Background:"),
      p("The large rodent population in New York City is no secret. Rats have been
      associated with the city for a long time whether it's from the famous", a('pizza rat', href='https://knowyourmeme.com/memes/pizza-rat'),
      "or to the rising concern from residents who have noticed changes since the COVID-19 pandemic.
      Many businesses and normally occurring procedures have been slowed down or 
      halted completely. One such example in particular with the Department of 
      Sanitation of NY (DSNY) where limited resources and budget cuts since the 
      pandemic have caused an", a("increased amount of litter and waste production", href="https://patch.com/new-york/new-york-city/city-state-leaders-decry-sanitation-setback-trash-piles"),
      "."),
      )
  ),
  fluidRow(
    align = "center",
    div(class='padding',
        h4(align = "left", "Visualizations:"),
    ),
    
    # descriptive charts 
    h3("Total Number of Rat Sightings (2020-2021)"),
    h6("Chart 1"),
    plotlyOutput("pratishta4", width = "50%"),
    br(),
    
    h3("Total Waste Produced (2020-2021)"),
    h6("Chart 2"),
    plotlyOutput("pratishta5", width = "50%"),
    br(),
    
    p(class = "padding", align = "left", "We can see  in Chart 1 that Brooklyn produces the most tons of waste followed by
      Queens, and then by Bronx and Manhattan. Staten Island is last with the 
      least amount of waste. Chart 2 shows the number of rat sightings per 
      borough and naturally, we have Brooklyn at the top with around 6,000 
      sightings. But Instead of Queens, Manhattan follows with most rat s
      ightings. Then Queens and Bronx. From here it seems that Staten Island and
      Bronx are boroughs that have some what proportional sightings to waste 
      produced. However, though Queens produces a lot of waste, it does not have
      nearly the same rate of rat sightings. Conversely, Manhattan doesn't quite
      produce the same amount of waste as Queens but seems to have far more rat 
      sightings. Brooklyin is consistenly infested."),
    
    # time series charts
    h3("Waste per Month (2020-2021)"),
    h6("Chart 3"),
    plotlyOutput("pratishta1", width = "70%"),
    br(), 
    
    h3("Rat Sightings per Month (2020-2021)"),
    h6("Chart 4"),
    plotlyOutput("pratishta2", width = "70%"),
    br(),
    
    h3("Rate of Rats per Waste Ton per Month"),
    h6("Chart 5"),
    plotlyOutput("pratishta3", width = "70%"),
    br(),
    
    p(class = "padding", align = "left", "Charts 3 to 5 show a time series line graphs. Chart 3 shows the tons of 
      waste per month generated by each borough. It seems that around March and 
      April of 2020 there was a change in the trend. Though the waste was 
      rising, it flattened out - or even fell like with Manhattan for example
      - between March and April of 2020. But after april there was a more mellow
      rise and then gentle decline closer to Fall of 2020 and early 2021. This 
      exploratory chart is good to possibly check out Manhattan and why the 
      waste production went down. Perhaps for restaurants closing?"),
    p(class = "padding", align = "left", "Chart 4 shows that all boroughs saw an increase in rat sightings, 
      especially Brooklyn. It did seem to peak around summer of 2020 and decline
      again to almost normal rates. These sightings might be due to the 
      sanitation departments' limits as mentioned earlier."),
    p(class = "padding", align = "left", "Chart 5 looks at the 'rate' at which a rat sighting is possible for every
      kilo-ton of waste produced in each borough per month. It seems that these 
      rates also follow a similar path of an increase around April 2020 and then
      a peak in summer of 2020 and then a mellow decline. However Manhattan's 
      rate shot up for early 2021 with around 0.75 (sightings per ton of waste) 
      to 1.5 (sightings per ton of waste). Perhaps this could be due to frequent
      testings, vaccinations, and re-opening of restaurants (producing more 
      waste)?"),

    # maps
    h3("Number of Rat Sightings per Month"),
    h6("Chart 6"),
    tmapOutput("pratishta7", width = "80%"),
    br(),

    
    h3("Waste Produced in Tons"),
    h6("Chart 7"),
    tmapOutput("pratishta8", width = "80%"),
    br(),
    
    h3("Rat Sightings and Waste Produced By Community District"),
    h6("Chart 8"),
    plotOutput("pratishta9", width = "60%"),
    br(),
    
    p(class = "padding", align = "left", "Here we show choropleth maps of NYC by
      community district. Community district as the geographical feature here is
      because DSNY also records their information with that feature. It seems 
      that there is one community districte that's seeing a severe rise in rat 
      sightings (BK03). It may be worth it to take a closer look at that 
      particular district if analysis or further studie are done."),
    p(class = "padding", align = "left", "As expected it looks like Queens 
      produces a massive amount of waste and the particular districts are 
      highlighted in deep red (100,000 - 120,000 tons category). And though 
      Brooklyn also produces a lot of waste it seems to be spread out amongst 
      the community districts."),
    p(class = "padding", align = "left", "Chart 8 depicts a bivariate choropleth
      map of the community districts. This make-shift rendering of a map using 
      the tm_map package is from", a("this currently open issue from the tmap GitHub
      repsitory", href="https://github.com/mtennekes/tmap/issues/183#issuecomment-670554921"),
      ". As the districts become more blue in color, the more right sightings 
      that have been reported in that district. The more purple a district, the 
      more amount of waste produced there. As the color goes to indigo, there's 
      a high rat sighting and a high waste production (a high high category if 
      further spatial dependence analysis was conducted). Light grey represents 
      few rat sightings and little waste production (a 'low low' again for further
      spatial research). This map demonstrates that hough there are high rat 
      sightings in downtown Manhattan and parts of Brooklyn, it's not 
      necessarily tied to waste production. In the same way, the outer boroughs 
      have a huge waste production but not many rat sightings. But there are 
      some districts (indigo) that do exhibit both features in high amounts. 
      This exploratory data visulaization provides the insight to further look 
      in those districts.")
  ),
  # END OF PRATISHTA'S WRITEUP -------------------------------------------------
)


# code for generating the plots  -----------------------------------------------------------------

server <- function(input, output, session) {

  # points <- eventReactive(input$recalc, {
  #   cbind(rat_sightings_sample$latitude, rat_sightings_sample$longitude)
  # }, ignoreNULL = FALSE)
  # 

  observe({
    min_year <- input$year_input[1]
    max_year <- input$year_input[2]
    burough <- input$burough_input
    case_status1 <- input$case_status
    #print('buroughh')
    #print(burough)
    #filter_rat_sightings <- rat_sightings_sample %>% filter(year_created >= min_year, year_created <= max_year, Borough %in% burough)
    filter_rat_sightings <- rat_sightings_sample %>% filter(year_created >= min_year, year_created <= max_year)
    #filter_rat_sightings <- filter_rat_sightings[,burough]
    filter_rat_sightings <- filter_rat_sightings %>% filter(Borough %in% burough)
    filter_rat_sightings <- filter_rat_sightings %>% filter(Status %in% case_status1)
    # if (nrow(event_data("plotly_selecting"))>0){
    #   filter_rat_sightings <- filter_rat_sightings %>% filter(year_created %in% event_data("plotly_selecting")$Var1)
    # }

    
    getColor <- function(filter_rat_sightings, i) {
      if(filter_rat_sightings$Status[i] == "Closed") {
        "green"
      } else if(filter_rat_sightings$Status[i] == "In Progress" | filter_rat_sightings$Status[i] == "Assigned") {
        "orange"
      } else {
        "red"
      }}
    
    markerColors <- rep(NA, nrow(filter_rat_sightings))
    
    for (i in 1:nrow(filter_rat_sightings)){
      markerColors[i] <- getColor(filter_rat_sightings, i)
    }


    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = markerColors
    )
    
    
    output$map <- renderLeaflet({
      leaflet(data = filter_rat_sightings) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
        addAwesomeMarkers( ~longitude, ~latitude, clusterOptions = markerClusterOptions() ,icon = icons, 
                           popup = as.character(paste('Created date:', filter_rat_sightings$Created.Date,'<br>',
                                                      'Complaint type:',filter_rat_sightings$Complaint.Type,'<br>',
                                                      'Descriptor:',filter_rat_sightings$Descriptor,'<br>',
                                                      'Address:',filter_rat_sightings$Incident.Address,'<br>',
                                                      'Status:', filter_rat_sightings$Status, '<br>',
                                                      'Location type:', filter_rat_sightings$Location.Type))) %>%
        addHeatmap( ~longitude, ~latitude, group = "heat",max=1, blur = 45, minOpacity = 0.8) %>% addLegend("topleft", 
                                                                                          colors =c('green',  "orange", "red"),
                                                                                          labels= c("Closed", "In Progress/Assigned","Open/Pending"),
                                                                                          title= "Case status",
                                                                                          opacity = 1)
      
    })

    output$cityViz <- renderPlotly({
      if (nrow(zipsInBounds()) == 0)
        return(NULL)
      
      tmp <- (zipsInBounds() %>% count(City))
      tmp <- tmp[order(-tmp$n),]
      tmp <- tmp[1:5,]
      ggplotly(
        ggplot(tmp, aes(x=City, y=n, fill = City)) + geom_bar(stat="identity") + ylab("Top 5 visible buroughs") + theme(legend.position = "none") + scale_color_brewer(palette="Dark2")+
          theme(axis.title.x=element_blank(),
                axis.ticks.x=element_blank())
      )
    })
    
    
    output$locationViz <- renderPlotly({
      if (nrow(zipsInBounds()) == 0)
        return(NULL)
      
      tmp <- (zipsInBounds() %>% count(Location.Type))
      tmp <- tmp[order(-tmp$n),]
      tmp <- tmp[1:5,]
      ggplotly(
        ggplot(tmp, aes(x=Location.Type, y=n, fill = Location.Type, show.legend = FALSE)) + geom_bar(stat="identity") + ylab("Visible location types") +
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
      )
    })
  
    output$yearViz <- renderPlotly({
      if (nrow(zipsInBounds()) == 0)
        return(NULL)
      
      created_date_sample <- data.table(zipsInBounds()$Created.Date)
      created_date_sample$dates <- parse_date_time(created_date_sample$V1, '%m/%d/%y %I:%M:%S %p')
      plot_created_year <- data.frame(table(year(date(created_date_sample$dates))))
      for (i in 2010:2021){
        if ((i %in% plot_created_year$Var1)==FALSE) {
          #print(i)
          tmp_df <- data.frame(toString(i), 0)
          names(tmp_df) <- c('Var1','Freq')
          plot_created_year <- rbind(plot_created_year, tmp_df)
        }
      }
      plot_created_year$Var1 <- as.numeric(as.character(plot_created_year$Var1))
      names(plot_created_year)[names(plot_created_year) == "Var1"] <- "Year"
      plot_created_year <- plot_created_year[order(plot_created_year$Year),]
      #plot_created_year <- filter(plot_created_year, Var1 != 2021)
      plot_created_year <- filter(plot_created_year, Year >= min_year, Year <= max_year)
      
      p_years <- ggplotly(
        ggplot(data=plot_created_year, aes(x=Year, y=Freq)) + geom_path(stat="identity") + ylab('Rat sightings') + geom_point()+
          theme(axis.title.x=element_blank()) +  scale_x_continuous(breaks=seq(min_year, max_year, 1))
      )
    })
    
    zipsInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(zipdata[FALSE,])
      bounds <- input$map_bounds
      #print(bounds)
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      #print(latRng)
      
      subset(filter_rat_sightings,
             latitude >= latRng[1] & latitude <= latRng[2] &
               longitude >= lngRng[1] & longitude <= lngRng[2])
    })
    
  })

  # PRATISHTA'S VISUALIZATIONS -------------------------------------------------
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
    ton +
    xlab("Boroughs") +
    ylab("Weight of Waste (Tons)")
    
  })
  
  output$pratishta5 <- renderPlotly({
    rat +
    xlab("Boroughs") +
    ylab("Number of Rat Sightings")
    
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
  # END OF PRATISHTA'S VISUALIZATIONS ------------------------------------------
}

shinyApp(ui = ui, server = server)