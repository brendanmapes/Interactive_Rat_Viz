#install.packages('rsconnect')
#library(rsconnect)
#install.packages("shinyjs")


# imports -----------------------------------------------------------------


library(shiny)
library(shinyjs)
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

rat_sightings <- read.csv("https://data.cityofnewyork.us/api/views/3q43-55fe/rows.csv?accessType=DOWNLOAD")
#rat_sightings <- read.csv("data/Rat_Sightings.csv")
rat_sightings <- rat_sightings %>% filter(!(Status=='Open'))
rat_sightings <- rat_sightings %>% filter(!(Status=='Draft'))
rat_sightings <- rat_sightings %>% filter(!(Borough=='Unspecified'))



rat_sightings$latitude <- rat_sightings$Latitude
rat_sightings$longitude <- rat_sightings$Longitude

#set.seed(100)



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

rat_sightings_buroughs <- as.character(unique(unlist(rat_sightings$Borough)))
rat_sightings_case_status <- as.character(unique(unlist(rat_sightings$Status)))

ui <- fluidPage(
  fluidRow(align = "center",
                h1("Rats and NYC: Exploratory Visualization"),
                strong("Data Visualization (QMSS - G5063) Final Project"),
                br(),
                em("Group N: Brendan Mapes, Prajwal Seth, and Pratishta Yerakala"),),
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
  fluidRow(style='margin-right:0px;',
    sidebarLayout(
      sidebarPanel(width = 6, style='margin-right:0px;',
        sliderInput("num_sample", label = h4("Select number of random samples"), min = 1,
                               max = nrow(rat_sightings), value = 10000, step = 1000),
        
        sliderInput("year_input", label = h4("Select years"), min = 2010,
                    max = 2021, value = c(2010, 2021), step = 1, format = "####"),
        
        
        #selected = rat_sightings_buroughs[1:length(multiInput)])
        #selected = rat_sightings_buroughs, 
        
        
        
        selectizeInput("burough_input", label=h4("Select boroughs"), choices =rat_sightings_buroughs, multiple = TRUE, selected = rat_sightings_buroughs),
        selectizeInput("case_status", label=h4("Select status"), choices =rat_sightings_case_status, multiple = TRUE, selected = rat_sightings_case_status),
        
        #plotlyOutput("cityViz", height = 300),
  
        plotlyOutput("yearViz", height = 210),
        plotlyOutput("locationViz", height = 220),
        
  
        #plotlyOutput("locationViz", height = 300),
      ),
      mainPanel(width = 6,
        leafletOutput("map", height = 825),
      )
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
    ),
    fluidRow(
      tmapOutput("pratishta7", width = "100%"),
      br(),
    ),
    fluidRow(align = "center",
      h3("Waste Produced in Tons"),
      h6("Chart 7"),
    ),
    fluidRow(
      tmapOutput("pratishta8", width = "100%"),
      br(),
    ),
    fluidRow(
      align = "center",
      
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
  ),
)

  
# code for generating the plots  -----------------------------------------------------------------

server <- function(input, output, session) {
  

# prajwal's code for generating interactive map backend -------------------

  

  # points <- eventReactive(input$recalc, {
  #   cbind(rat_sightings_sample$latitude, rat_sightings_sample$longitude)
  # }, ignoreNULL = FALSE)
  #
  
  observe({
    min_year <- input$year_input[1]
    max_year <- input$year_input[2]
    burough <- input$burough_input
    case_status1 <- input$case_status
    
    rat_sightings_sample <- rat_sightings[sample.int(nrow(rat_sightings), input$num_sample),]
    #rat_sightings_sample <- rat_sightings
    latitude_colnum <- grep('latitude', colnames(rat_sightings_sample))
    longitude_colnum <- grep('longitude', colnames(rat_sightings_sample))
    rat_sightings_sample <- rat_sightings_sample[complete.cases(rat_sightings_sample[,latitude_colnum:longitude_colnum]),]
    rat_sightings_sample$year_created <- year(parse_date_time(rat_sightings_sample$Created.Date, '%m/%d/%y %I:%M:%S %p'))
    
    
    
    
    
    #print('buroughh')
    #print(burough)
    #filter_rat_sightings <- rat_sightings_sample %>% filter(year_created >= min_year, year_created <= max_year, Borough %in% burough)
    check_rows_of_filter <- nrow(rat_sightings_sample %>% filter(year_created >= min_year, year_created <= max_year, 
                                                                 Borough %in% burough,
                                                                 Status %in% case_status1))
    
    rat_sightings_buroughs2 <- as.character(unique(unlist(rat_sightings_sample$Borough)))
    rat_sightings_case_status2 <- as.character(unique(unlist(rat_sightings_sample$Status)))
    
    # print('buroughs 2')
    # print(rat_sightings_buroughs2)
    # print('case statuses 2')
    # print(rat_sightings_case_status2)
    
    if (check_rows_of_filter <= 0){
      
      #updateSliderInput(session, "year_input", value = c(2010, 2021))
      #updateSliderInput(session, "burough_input", value = rat_sightings_buroughs2)
      #updateSliderInput(session, "case_status", value = rat_sightings_case_status2)
      
      
      # filter_rat_sightings2 <- rat_sightings_sample
      # reset("year_input")
      # reset("burough_input")
      # reset("burough_input")
      # print('in the case of 0 rows, resetting to entire df')
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        setView(lng = -73.98928, lat = 40.75042, zoom = 10)
    }
    else{
      filter_rat_sightings2 <- rat_sightings_sample %>% filter(year_created >= min_year, year_created <= max_year,
                                                               Borough %in% burough,
                                                               Status %in% case_status1)
      filter_rat_sightings <- filter_rat_sightings2
      
      
      getColor <- function(filter_rat_sightings, i) {
        if(filter_rat_sightings$Status[i] == "Closed") {
          "green"
        } 
        else if(filter_rat_sightings$Status[i] == "In Progress") {
          "lightblue"
        } 
        else if(filter_rat_sightings$Status[i] == "Assigned") {
          "orange"
        }
        else if(filter_rat_sightings$Status[i] == "Open") {
          "purple"
        }
        else if(filter_rat_sightings$Status[i] == "Pending") {
          "darkred"
        }
        else if(filter_rat_sightings$Status[i] == "Draft") {
          "blue"
        }}
      
      markerColors <- rep(NA, nrow(filter_rat_sightings))
      
      for (i in 1:nrow(filter_rat_sightings)){
        markerColors[i] <- getColor(filter_rat_sightings, i)
      }
      
      
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'cadetblue',
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
                                                        'Closed Date:', filter_rat_sightings$Closed.Date,'<br>',
                                                        #'Complaint type:',filter_rat_sightings$Complaint.Type,'<br>',
                                                        #'Descriptor:',filter_rat_sightings$Descriptor,'<br>',
                                                        'Address:',filter_rat_sightings$Incident.Address,'<br>',
                                                        'Status:', filter_rat_sightings$Status, '<br>',
                                                        'Location Type:', filter_rat_sightings$Location.Type))) %>%
          addHeatmap( ~longitude, ~latitude, group = "heat",max=1, blur = 45, minOpacity = 0.8) %>% addLegend("topleft", 
                                                                                                              colors =c('green', "darkred", "lightblue", "orange"),
                                                                                                              #"purple", "#56a0d1"
                                                                                                              labels= c("Closed", "Pending", "In Progress","Assigned"),
                                                                                                              # "Open", "Draft"
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
        ggplotly(tooltip = c("n"),
          ggplot(tmp, aes(x=Location.Type, y=n)) + geom_bar(stat="identity", aes(fill = Location.Type)) + ggtitle('Visible location types') + ylab("Visible location types") +
            theme(axis.title.y=element_blank(),
                  #axis.text.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()) + labs(fill = "Visible location types") + coord_flip() + theme(legend.position = "none")
        )
      })
      
      output$yearViz <- renderPlotly({
        if (nrow(zipsInBounds()) == 0)
          return(NULL)
        
        
        # total cases
        
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
        plot_created_year <- filter(plot_created_year, Year >= min_year, Year <= max_year)
        plot_created_year$case_status <- 'Total'
        
        # closed cases
        
        plot_closed<- filter(zipsInBounds(), Status == 'Closed')
        flag_closed <- 0
        if (nrow(plot_closed) == 0) {
          flag_closed <- 1
        }
        if (flag_closed == 0){
          plot_closed <- data.table(plot_closed$Created.Date)
          plot_closed$dates <- parse_date_time(plot_closed$V1, '%m/%d/%y %I:%M:%S %p')
          plot_closed_year <- data.frame(table(year(date(plot_closed$dates))))
          #print(plot_closed_year)
          
          
          for (i in 2010:2021){
            if ((i %in% plot_closed_year$Var1)==FALSE) {
              tmp_df <- data.frame(toString(i), 0)
              names(tmp_df) <- colnames(plot_closed_year)
              plot_closed_year <- rbind(plot_closed_year, tmp_df)
            }
          }
          
          
          plot_closed_year$Var1 <- as.numeric(as.character(plot_closed_year$Var1))
          names(plot_closed_year)[names(plot_closed_year) == "Var1"] <- "Year"
          plot_closed_year <- plot_closed_year[order(plot_closed_year$Year),]
          plot_closed_year <- filter(plot_closed_year, Year >= min_year, Year <= max_year)
          plot_closed_year$case_status <- 'Closed'
        }
        
        
        # assigned cases
        
        plot_assigned<- filter(zipsInBounds(), Status == 'Assigned')
        flag_assigned <- 0
        if (nrow(plot_assigned) == 0) {
          flag_assigned <- 1
        }
        if (flag_assigned == 0){
          plot_assigned <- data.table(plot_assigned$Created.Date)
          plot_assigned$dates <- parse_date_time(plot_assigned$V1, '%m/%d/%y %I:%M:%S %p')
          plot_assigned_year <- data.frame(table(year(date(plot_assigned$dates))))
          #print(plot_assigned_year)
          
          
          for (i in 2010:2021){
            if ((i %in% plot_assigned_year$Var1)==FALSE) {
              tmp_df <- data.frame(toString(i), 0)
              names(tmp_df) <- colnames(plot_assigned_year)
              plot_assigned_year <- rbind(plot_assigned_year, tmp_df)
            }
          }
          
          
          plot_assigned_year$Var1 <- as.numeric(as.character(plot_assigned_year$Var1))
          names(plot_assigned_year)[names(plot_assigned_year) == "Var1"] <- "Year"
          plot_assigned_year <- plot_assigned_year[order(plot_assigned_year$Year),]
          plot_assigned_year <- filter(plot_assigned_year, Year >= min_year, Year <= max_year)
          plot_assigned_year$case_status <- 'Assigned'
          #print('assigned or in progress')
          #print(plot_assigned_year)
        }
        
        # assigned or in progress cases
        
        plot_in_progress<- filter(zipsInBounds(), Status == 'In Progress')
        flag_in_progress <- 0
        if (nrow(plot_in_progress) == 0) {
          flag_in_progress <- 1
        }
        if (flag_in_progress == 0){
          plot_in_progress <- data.table(plot_in_progress$Created.Date)
          plot_in_progress$dates <- parse_date_time(plot_in_progress$V1, '%m/%d/%y %I:%M:%S %p')
          plot_in_progress_year <- data.frame(table(year(date(plot_in_progress$dates))))
          #print(plot_in_progress_year)
          
          
          for (i in 2010:2021){
            if ((i %in% plot_in_progress_year$Var1)==FALSE) {
              tmp_df <- data.frame(toString(i), 0)
              names(tmp_df) <- colnames(plot_in_progress_year)
              plot_in_progress_year <- rbind(plot_in_progress_year, tmp_df)
            }
          }
          
          
          plot_in_progress_year$Var1 <- as.numeric(as.character(plot_in_progress_year$Var1))
          names(plot_in_progress_year)[names(plot_in_progress_year) == "Var1"] <- "Year"
          plot_in_progress_year <- plot_in_progress_year[order(plot_in_progress_year$Year),]
          plot_in_progress_year <- filter(plot_in_progress_year, Year >= min_year, Year <= max_year)
          plot_in_progress_year$case_status <- 'In Progress'
          #print('assigned or in progress')
          #print(plot_in_progress_year)
        }
        
        
        
        # open cases
        
        plot_open<- filter(zipsInBounds(), Status == 'Open')
        flag_open <- 0
        if (nrow(plot_open) == 0) {
          flag_open <- 1
        }
        
        if (flag_open == 0){
          
          plot_open <- data.table(plot_open$Created.Date)
          plot_open$dates <- parse_date_time(plot_open$V1, '%m/%d/%y %I:%M:%S %p')
          plot_open_year <- data.frame(table(year(date(plot_open$dates))))
          #print(plot_open_year)
          
          
          for (i in 2010:2021){
            if ((i %in% plot_open_year$Var1)==FALSE) {
              tmp_df <- data.frame(toString(i), 0)
              names(tmp_df) <- colnames(plot_open_year)
              plot_open_year <- rbind(plot_open_year, tmp_df)
            }
          }
          
          
          plot_open_year$Var1 <- as.numeric(as.character(plot_open_year$Var1))
          names(plot_open_year)[names(plot_open_year) == "Var1"] <- "Year"
          plot_open_year <- plot_open_year[order(plot_open_year$Year),]
          plot_open_year <- filter(plot_open_year, Year >= min_year, Year <= max_year)
          plot_open_year$case_status <- 'Open'
          #print('open or pending')
          #print(plot_open_year)
          
          
          # print('created')
          # print(plot_created_year)
          # print('closed')
          # print(plot_closed_year)
          # print('combined')
        }
        
        # pending cases
        
        plot_pending<- filter(zipsInBounds(), Status == 'Pending')
        flag_pending <- 0
        if (nrow(plot_pending) == 0) {
          flag_pending <- 1
        }
        
        if (flag_pending == 0){
          
          plot_pending <- data.table(plot_pending$Created.Date)
          plot_pending$dates <- parse_date_time(plot_pending$V1, '%m/%d/%y %I:%M:%S %p')
          plot_pending_year <- data.frame(table(year(date(plot_pending$dates))))
          #print(plot_pending_year)
          
          
          for (i in 2010:2021){
            if ((i %in% plot_pending_year$Var1)==FALSE) {
              tmp_df <- data.frame(toString(i), 0)
              names(tmp_df) <- colnames(plot_pending_year)
              plot_pending_year <- rbind(plot_pending_year, tmp_df)
            }
          }
          
          
          plot_pending_year$Var1 <- as.numeric(as.character(plot_pending_year$Var1))
          names(plot_pending_year)[names(plot_pending_year) == "Var1"] <- "Year"
          plot_pending_year <- plot_pending_year[order(plot_pending_year$Year),]
          plot_pending_year <- filter(plot_pending_year, Year >= min_year, Year <= max_year)
          plot_pending_year$case_status <- 'Pending'
          #print('open or pending')
          #print(plot_pending_year)
          
          
          # print('created')
          # print(plot_created_year)
          # print('closed')
          # print(plot_closed_year)
          # print('combined')
        }
        
        
        #plot_this <- plot_created_year
        plot_this <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(plot_this) <- c("Year",'Freq','case_status')
        rownames(plot_created_year)
        
        # print('flag open')
        # print(flag_open)
        # print('flag pending')
        # print(flag_pending)
        # print('flag assigned')
        # print(flag_assigned)
        # print('flag in progress')
        # print(flag_in_progress)
        # print('flag closed')
        # print(flag_closed)
        
        if (flag_open == 0){
          plot_this <- rbind(plot_this, plot_open_year)
        }
        if (flag_pending == 0){
          plot_this <- rbind(plot_this, plot_pending_year)
        }
        
        if (flag_assigned == 0) {
          plot_this <- rbind(plot_this, plot_assigned_year)
        }
        if (flag_in_progress == 0) {
          plot_this <- rbind(plot_this, plot_in_progress_year)
        }
        if (flag_closed == 0){
          plot_this <- rbind(plot_this, plot_closed_year)
        }
        #print(plot_this)
        
        # p_years <- ggplotly(
        #   ggplot(data=plot_created_year, aes(x=Year, y=Freq)) + geom_path(stat="identity") + ylab('Rat sightings') + geom_point()+
        #     theme(axis.title.x=element_blank()) +  scale_x_continuous(breaks=seq(min_year, max_year, 1))
        # )
        colors_for_case_status <- rep(NA, nrow(plot_this))
        for (i in 1:nrow(plot_this)){
          if (plot_this$case_status[i] == "Closed"){
            colors_for_case_status[i] <- "green"
          }
          else if (plot_this$case_status[i] == "Pending"){
            colors_for_case_status[i] <- "darkred"
          }
          else if (plot_this$case_status[i] == "Assigned"){
            colors_for_case_status[i] <- "orange"
          }
          else if (plot_this$case_status[i] == "In Progress"){
            colors_for_case_status[i] <- "cadetblue"
          }
          else if (plot_this$case_status[i] == "Draft"){
            colors_for_case_status[i] <- "blue"
          }
          else if (plot_this$case_status[i] == "Open"){
            colors_for_case_status[i] <- "purple"
          }
          
        }
        
        p_years <- ggplotly(
          ggplot(data=plot_this, aes(x=Year, y=Freq)) + geom_line(aes(color=case_status))
          #+ scale_colour_manual(name = 'Case status',values =c('green'='green','cadetblue' = 'cadetblue', 'orange'='orange', 'darkred'='darkred'), labels = c("closed","in progress", "assigned",'pending'))
          + ggtitle('Case status trend') +  scale_x_continuous(breaks=seq(min_year, max_year, 1))
          + theme(axis.title.y=element_blank(),
                #axis.text.y=element_blank(),
                axis.title.x=element_blank(),)
                #axis.text.x=element_blank())
          + theme(legend.title = element_blank()) 
          #+ theme(legend.position="left")
          #+ labs(color='Status') 
          +
            theme(axis.title.x=element_blank())
          
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
      
    }
    
    #filter_rat_sightings <- filter_rat_sightings[,burough]

    # if (nrow(event_data("plotly_selecting"))>0){
    #   filter_rat_sightings <- filter_rat_sightings %>% filter(year_created %in% event_data("plotly_selecting")$Var1)
    # }
    
    #print('reached here')
    #print(filter_rat_sightings)
    #print('end reached here')

    
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
      tm_fill("n", title = "Rat Sightings in Community Districts")+tm_view(set.view = c(-73.98928, 40.70042,10))
    
    
  })
  
  output$pratishta8 <- renderTmap({
    
    ## ------------------------------------------------------------------------
    tm_shape(nyc_sp) +
      tm_fill("total_ton", title = "Tones of Waste and Rat Sightings by DSNY Districts")+tm_view(set.view = c(-73.98928, 40.70042,10))
    
    
    
    
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
    vp = viewport(x = 0.37, y = 0.75, width = 0.25, height = 0.25)
    print(bimap, vp = viewport())
    pushViewport(vp)
    print(bilegend, newpage = FALSE, vp = vp)
  })
  
  # END OF PRATISHTA'S VISUALIZATIONS ------------------------------------------
}

shinyApp(ui = ui, server = server)