#install.packages('rsconnect')
#library(rsconnect)
#install.packages("shinyjs")

options(scipen=999)

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


# prajwal's data fetch ----------------------------------------------------------


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

rat <- ggplot(rat_ton, aes(y=n, x=Borough, fill = Borough)) + 
  geom_bar(position="dodge", stat="identity")

ton <- ggplot(rat_ton, aes(y=total_ton, x=Borough, fill = Borough)) + 
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




# brendan's imports and code -------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(gridExtra)
library(dplyr)
library(readr)
library(leaflet)
library(leaflet.extras)
library(magrittr)

library(dplyr)
library(tidyr)
library(wordcloud)
library(png)
library(ggwordcloud)
library(tidytext)
library(readr)
library(png)
library(plotly)
#setwd("~/Bayesian")


open <- read.csv("data/Open_Restaurant_Applications.csv")
inspection <- read.csv("data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
rat_311 <- read.csv("data/311_Service_Requests_from_2010_to_Present.csv")


restaurant_borough <- count(open, Borough)
names(restaurant_borough)[names(restaurant_borough) == "n"] <- "count"

rat_borough <- count(rat_311, Borough)
borough <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island", "none")

rat_borough <- cbind(rat_borough, borough) %>% filter(borough!= "none")
rat_borough <- select(rat_borough, borough, n)
names(rat_borough)[names(rat_borough) == "n"] <- "count"
names(rat_borough)[names(rat_borough) == "borough"] <- "Borough"

inspection_bc <- inspection %>% filter(GRADE == "B" | GRADE == "C")
inspection_count_2020 <- count(inspection_bc, BORO)
names(inspection_count_2020)[names(inspection_count_2020) == "n"] <- "count"
names(inspection_count_2020)[names(inspection_count_2020) == "BORO"] <- "Borough"


street_seating <- filter(open, Approved.for.Roadway.Seating == "yes")
count_street <- count(street_seating, Borough)
names(count_street)[names(count_street) == "n"] <- "count"

sidewalk_seating <- filter(open, Approved.for.Sidewalk.Seating == "yes")
count_sidewalk <- count(sidewalk_seating, Borough)
names(count_sidewalk)[names(count_sidewalk) == "n"] <- "count"

manhattan_311 <- filter(rat_311, Borough == "MANHATTAN")
#manhattan_311 <- read.csv("manhattan311.csv")
manhattan_open <- read.csv("data/manhattan open restaurants.csv")
manhattan_311 <- filter(manhattan_311, manhattan_311$Complaint.Type=="Rodent")
manhattan_311 <- data.frame(manhattan_311$Latitude, manhattan_311$Longitude, manhattan_311$Incident.Address, manhattan_311$Created.Date, manhattan_311$Descriptor)

icon <- makeIcon(iconUrl= "https://cdn3.iconfinder.com/data/icons/farm-animals/128/mouse-512.png", iconWidth=25, iconHeight = 20)

#manhattan_311b <- read.csv("manhattan311.csv")
#manhattan_311b <- read.csv("https://nycopendata.socrata.com/api/views/erm2-nwe9/rows.csv?accessType=DOWNLOAD")


# code for making wordcloud (not used in live version for processing power reasons) -----------------------------------------------


# manhattan_311b <- filter(rat_311, Borough == "MANHATTAN")
# nrow(manhattan_311b)
# 
# manhattan_311b <- manhattan_311b %>% filter(Complaint.Type != "Noise = Street/Sidewalk" & Complaint.Type != "Noise - Residential" & Complaint.Type != "HEAT/HOT WATER" & Complaint.Type != "Illegal Parking" & Complaint.Type != "Non-Emergency Police Matter" & Complaint.Type != "Noise" & Complaint.Type != "Noise - Vehicle" & Complaint.Type != " Noise - Commercial")
# descriptors <- manhattan_311b %>% select(Descriptor)
# descriptors_fix <- as.character(descriptors$Descriptor)
# text_df <- tibble(line = 1:length(descriptors_fix), text = descriptors_fix)
# descriptors <- text_df %>% unnest_tokens(word, text)
# 
# descriptors <- count(descriptors, word) 
# descriptors2 <- filter(descriptors, n > 2000)
# col <- c(ifelse(descriptors2$word == "pests" | descriptors2$word == "rat" | descriptors2$word == "sighting" | descriptors2$word == "rodents", "red", "black"))
# descriptors3 <- cbind(descriptors2, col)
# descriptors3 <- filter(descriptors3, word != "n" & word != "a" & word != "not" & word != "business" & word != "no" & word!= "compliance" & word != "or" & word != "in" & word != "of" & word!= "to" & word!= "non" & word!= "on" & word != "has" & word!= "for")
# #setwd("~/Bayesian")
# img <- readPNG("data/rat.png")
# #img <- icon
# descriptors3 <- descriptors3 %>% filter(word != "loud" & word!= "music" & word != "party")
# set.seed(14)

# (wordcloud1 <- ggplot(descriptors3, aes(label=word, size=n, color=col)) + geom_text_wordcloud_area(mask=img, rm_outside = TRUE) + scale_size_area(max_size=5) + theme_classic())
# (wordcloud2 <- ggplot(descriptors3, aes(label=word, size=n, color=col)) + geom_text_wordcloud_area() + scale_size_area())



# user interface for setting layout of plots ----------------------------------------------------------


# sliders and interactive map ---------------------------------------------


rat_sightings_buroughs <- as.character(unique(unlist(rat_sightings$Borough)))
rat_sightings_case_status <- as.character(unique(unlist(rat_sightings$Status)))

ui <- fluidPage(
  

# header description ------------------------------------------------------

  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .row {
        margin-left: 0;
        margin-right:0;
      }"))
  ),
  
  
  fluidRow(align = "center",
                h1("Rats and NYC: Exploratory Visualization"),
                strong("Data Visualization (QMSS - G5063) Final Project, Spring 2021"),
                br(),
                em("Group N: Brendan Mapes, Prajwal Seth, and Pratishta Yerakala"),
           h3(a("Link to code and process book", href="https://github.com/QMSS-G5063-2021/Group_N_NYCdata")),
              
           br(),br(),br(),
           p("In this project, we will explore in detail New York City's rat problem. 
             New York has always dealt with a relatively high population of rats, as do many 
             other large metropolitan areas. However, since the beginning of the COVID-19 pandemic 
             rat sightings have been on the rise. Rats are being seen more often, during new times of day, 
             and are acting more aggressive. Through the following visualizations we hope to find some explanation 
             for this recent uptick in rat sightings. The way restaurants and residents handle their trash plays 
             a large role in the survival and behavior of rats in the city. So through exploration of city 
             sanitation data, restaurant registration data, and 311 calls in the city, we hope to find some 
             potential explanations as to why New York's rat problem has gotten so bad."),),
br(),br(),br(),
  

# prajwal's description ---------------------------------------------------

  
  # fluidRow(
  #   align = "center",
  #   headerPanel("Hello 1!"),
  #   p("p creates a paragraph of text."),
  #   p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
  #   strong("strong() makes bold text."),
  #   em("em() creates italicized (i.e, emphasized) text."),
  #   br(),
  #   code("code displays your text similar to computer code"),
  #   div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
  #   br(),
  #   p("span does the same thing as div, but it works with",
  #     span("groups of words", style = "color:blue"),
  #     "that appear inside a paragraph."),
  # ),

  fluidRow(
    align = "center",
    style='margin-left:0px; margin-right: 0px;',
    h2("Interactive map of rodent complaints in NYC since 2010"),
    h3("Prajwal Seth"),
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
        h5(a("Rat sightings (automatically updated daily)", href="https://data.cityofnewyork.us/Social-Services/Rat-Sightings/3q43-55fe"),
        br(),
         h4("Background:"),
         p("In this section, I have visualized rodent complaints from 2010 till today submitted to the NYC311 portal.
           Feel free to play around with the provided filters for number of samples,
           years, boroughs, and case status (however, due to processing constraints on shinyapps.io,
           the website will crash if you set the number of samples too high). The map on the left will dynamically update as you
           change the filters (or will not update if there is no data to display after the filters are applied).
           The plot for the trend in complaint status also updates according to the
           rat complaints that are visible in the map. Upon zooming into the map, you will see that the color of the
           marker for each complaint is set according to the complaint status (refer to the legend of the map). 
           Also provided is a tooltip displaying the complaint's created date, closed date, address, status, and
           location type. There is a layer of heat added to the map, with the intensity of the heat
           being calculated based on the number of rat sightings in the area."),

    ),
    
    
  ),


# sliders and map etc -----------------------------------------------------

  
  
  fluidRow(
    sidebarLayout(position = "right",
      
      sidebarPanel(width = 6, 
        sliderInput("num_sample", label = h4("Select number of random samples"), min = 1,
                               max = nrow(rat_sightings), value = 1000, step = 1000),
        
        sliderInput("year_input", label = h4("Select years"), min = 2010,
                    max = 2021, value = c(2010, 2021), step = 1, format = "####"),
        
        
        #selected = rat_sightings_buroughs[1:length(multiInput)])
        #selected = rat_sightings_buroughs, 
        
        
        
        selectizeInput("burough_input", label=h4("Select boroughs"), choices =rat_sightings_buroughs, multiple = TRUE, selected = rat_sightings_buroughs),
        selectizeInput("case_status", label=h4("Select status"), choices =rat_sightings_case_status, multiple = TRUE, selected = rat_sightings_case_status),
        
        #plotlyOutput("cityViz", height = 300),
  
        plotlyOutput("yearViz", height = 250),
        #plotlyOutput("locationViz", height = 220),
        
  
        #plotlyOutput("locationViz", height = 300),
      ),
      mainPanel(width = 6, style='margin-top:40px;',
                leafletOutput("map", height = 825),
      ),
    ),
    # PRATISHTA'S WRITEUP --------------------------------------------------------
    fluidRow(
      align = "center",
      style='margin-left:0px; margin-right: 0px;',
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
      plotlyOutput("pratishta5", width = "50%"),
      br(),
      
      h3("Total Waste Produced (2020-2021)"),
      h6("Chart 2"),
      plotlyOutput("pratishta4", width = "50%"),
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
    # Brendan's writeup  -------------------------------------------------
    fluidRow(
      align = "center",
      h2("Rat Sightings and Restaurants by Borough"),
      h3("Brendan Mapes"),
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
          h5(a("Rat sightings", href="https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9"), h6("Filtered to 311 calls from Jan 1 2020 to Dec 31 2020, of complaint type “rodent”.")),
          h5(a("Open Restaurants", href="https://data.cityofnewyork.us/Transportation/Open-Restaurant-Applications/pitm-atqc/data"),),
      ),
      
      div(class='padding', 
          h4("Background:"),
          p("It is well documented that the COVID-19 pandemic has led to a rise in rat sightings throughout New York
City. It is also well known that the pandemic has been especially hard on the restaurant industry. Hit
especially hard by stay-at-home orders and social distancing mandates, restaurants have been forced to
innovate their operations. Now more than ever, restaurants are serving customers outside in the
streets. We suspect this change in the way restaurants do their business may be contributing to the
increase in rat sightings. We explore this possibility a bit further in the next few visualizations."),
      ),
      div(class='padding',
          h4("Visualizations:"),
      ),
    ),
    
    
    # descriptive charts 
    
#     fluidRow(align = "center",
#              br(),
#              h3("NYC311 rodent complaints in 2020 and number of restaurants by type"),
#              h6("Chart 9"),br(),
#              plotOutput("brendan_chart1", width = "80%"),
#              plotOutput("brendan_chart2", width = "80%"),
#              br(),
#              br(),
#     p(class = "padding", align = "left", "In all four figures, we can see that Manhattan is far above the rest of the boroughs in restaurants
# approved for outdoor dining, in sidewalk and street dining. However, it is Brooklyn that is far above the
# rest of the boroughs in rodent reports in 2020. This suggests that perhaps another factor is contributing
# to the rat problem in the Brooklyn borough. If restaurants were fully to blame for it’s rat problem, we
# would expect to see it having high numbers of restaurants approved for outdoor street and sidewalk
# dining, a number comparable to the borough of Manhattan. 
# 
# The first bar plot displays the number of rodent related 311 reports in the year 2020 by borough.
# Brooklyn leads the way with well over 10,000 rodent related calls in the year, while the next closest
# borough, Manhattan, only has about 8,000 rodent related calls in the year. In the bar plots related to
# restaurants, we see Manhattan leads the way across the board. In the restaurants with outdoor dining,
# sidewalk and street dining, Manhattan has twice as many restaurants than any other borough. Because
# of this vast difference in the number of restaurants in Manhattan compared to the other boroughs, we
# will narrow our focus to Manhattan in the next visualization."),),

fluidRow(align = "center",
         br(),
         br(),
         br(),
         
         # code for generating these plots

         #   title: "brendan2"
         # author: "Brendan Mapes"
         # date: "4/17/2021"
         # output: html_document
         # ---
         #   
         #   ```{r setup, include=TRUE}
         # knitr::opts_chunk$set(echo = TRUE)
         # library(ggplot2)
         # library(ggthemes)
         # library(gridExtra)
         # library(dplyr)
         # library(plotly)
         # open <- read.csv("Open_Restaurant_Applications.csv")
         # inspection <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
         # rat_311 <- read.csv("311_Service_Requests_from_2010_to_Present.csv")
         # restaurant_borough <- count(open, Borough)
         # names(restaurant_borough)[names(restaurant_borough) == "n"] <- "count"
         # rat_borough <- count(rat_311, Borough)
         # borough <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island", "none")
         # rat_borough <- cbind(rat_borough, borough) %>% filter(borough!= "none")
         # rat_borough <- select(rat_borough, borough, n)
         # names(rat_borough)[names(rat_borough) == "n"] <- "count"
         # names(rat_borough)[names(rat_borough) == "borough"] <- "Borough"
         # inspection_bc <- inspection %>% filter(GRADE == "B" | GRADE == "C")
         # inspection_count_2020 <- count(inspection_bc, BORO)
         # names(inspection_count_2020)[names(inspection_count_2020) == "n"] <- "count"
         # names(inspection_count_2020)[names(inspection_count_2020) == "BORO"] <- "Borough"
         # street_seating <- filter(open, Approved.for.Roadway.Seating == "yes")
         # count_street <- count(street_seating, Borough)
         # names(count_street)[names(count_street) == "n"] <- "count"
         # sidewalk_seating <- filter(open, Approved.for.Sidewalk.Seating == "yes")
         # count_sidewalk <- count(sidewalk_seating, Borough)
         # names(count_sidewalk)[names(count_sidewalk) == "n"] <- "count"
         # plot1 <- ggplot() + geom_bar(data=rat_borough, aes(x=Borough, y=count, fill=Borough), stat="identity") + ggtitle("2020 rodent reports") + ylab("Number of 311 calls\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.position="none") +  theme(axis.text.x = element_text(angle = 40, vjust=.7)) 
         # plot2 <- ggplot() + geom_bar(data=restaurant_borough, aes(x =Borough, y= count, fill =Borough), stat="identity") + ggtitle("Outdoor restaurants") + ylab("Applications approved\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), legend.position="none", axis.title.y=element_text(face="bold")) + theme(axis.text.x = element_text(angle = 40, vjust=.7))
         # plot3 <- ggplot() + geom_bar(data=inspection_count_2020, aes(x=Borough, y=count, fill=Borough), stat="identity") + ggtitle("Restaurants w/ B or C inspection scores") + ylab("Restaurants\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), legend.position="none", axis.title.y=element_text(face="bold")) +  theme(axis.text.x = element_text(angle = 40, vjust=.7))
         # plot4 <- ggplot() + geom_bar(data=count_street, aes(x=Borough, y=count, fill=Borough), stat="identity") + ggtitle("Street dining") + ylab("Approved restaurants\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.position="none") +  theme(axis.text.x = element_text(angle = 40, vjust=.7))
         # plot5 <- ggplot() + geom_bar(data=count_sidewalk, aes(x=Borough, y=count, fill=Borough), stat="identity") + ggtitle("Sidewalk dining") + ylab("Approved restaurants\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.position="none") +  theme(axis.text.x = element_text(angle = 40, vjust=.7))
         # plot1a <- ggplotly(plot1, tooltip=c("x", "y"))
         # plot2a <- ggplotly(plot2, tooltip=c("x", "y"))
         # plot3a <- ggplotly(plot3, tooltip=c("x", "y"))
         # plot4a <- ggplotly(plot4, tooltip=c("x", "y"))
         # plot5a <- ggplotly(plot5, tooltip=c("x", "y"))
         # plot1a
         # plot2a
         # plot3a
         # plot4a
         # plot5a
         
         
         
         h6("Chart 9"),
         img(src='1.png',width="50%"),
         br(),
         h6("Chart 10"),
         img(src='2.png',width="50%"),
         br(),
         h6("Chart 11"),
         img(src='3.png',width='50%'),
         br(),
         h6("Chart 12"),
         img(src='4.png',width='50%'),
         br(),
         h6("Chart 13"),
         img(src='5.png',width='50%'),
         br(),
         p(class = "padding", align = "left", "In all five figures, we can see that Manhattan is far above the rest of the boroughs 
               in restaurants approved for outdoor dining, in sidewalk and street dining, and B and C graded restaurants. However, it is 
               Brooklyn that is far above the rest of the boroughs in rodent reports in 2020. This suggests that perhaps another factor is 
               contributing to the rat problem in the Brooklyn borough. If restaurants were fully to blame for it’s rat problem, we would 
               expect to see it having high numbers of restaurants approved for outdoor street and sidewalk dining, a number comparable to 
               the borough of Manhattan. The first bar plot displays the number of rodent related 311 reports in the year 2020 by borough. 
               Brooklyn leads the way with well over 10,000 rodent related calls in the year, while the next closest borough, Manhattan, only 
               has about 8,000 rodent related calls in the year. In the bar plots related to restaurants, we see Manhattan leads the way across 
               the board. In the restaurants with outdoor dining, sidewalk and street dining, Manhattan has twice as many restaurants than any 
               other borough. Because of this vast difference in the number of restaurants in Manhattan compared to the other boroughs, we will 
               narrow our focus to Manhattan in the next visualization."),br(),br(),
),
    
    ),
  br(),
  fluidRow(
    tags$style(".padding {
                            margin-left:30px;
                            margin-right:30px;
                          }"),
    tags$style(".leftAlign{float:left;}"),
    align = "left",
    div(class='padding',
        h4("Data used:"),
        h5(a("Rat sightings", href="https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9"), h6("Filtered to 311 calls from Jan 1 2020 to Dec 31 2020, of complaint type “rodent” in Manhattan borough.")),
        h5(a("Open Restaurants", href="https://data.cityofnewyork.us/Transportation/Open-Restaurant-Applications/pitm-atqc/data"),h6("Filtered to Manhattan borough.")),
    ),
    div(class='padding',
        h4("Visualization:"),
    ),
    
    fluidRow(align = "center",
             h3("Rat sightings in 2020 overlaid on restaurant locations in Manhattan"),
             h6("Chart 14"),),br(),
    fluidRow(
      
      leafletOutput("brendan_map", height = 500, width = "100%"),
      br(),
      br(),
      p(class = "padding", align = "left", "Based off exploratory analysis of the restaurants and rat reports across all boroughs, it’s clear
Manhattan’s restaurant industry may be most closely linked to the rat problem than in other boroughs.
For that reason, we have provided an interactive map visualization of the Manhattan borough
specifically. In the map, restaurants are plotted and viewers can see the location and name of the
restaurant, along with whether or not the restaurant is available for open street or sidewalk dining. Also
charted on the map are the location of rat sightings in the 2020 311 calls data set, the same data used
for previous visualizations. With no zoom, clusters of the rats are displayed in the visualization. After
zooming in further, those clusters break into the individual rat sighting locations of which they consist.
Rat sighting locations are represented by small rat icons on the map."),
    p(class = "padding", align = "left", "This visualization allows viewers to identify densely rat populated locations on the map and relate that
information to the data provided for restaurants in the same locations. In real time, such a map would
be useful for avoidance of rat “hot spots” when choosing places to dine. It also allows users to explore
which restaurants have practices that may be contributing to the rat problem the most, with lots of rat
sightings nearby."),),
    
    fluidRow(
      tags$style(".padding {
                            margin-left:30px;
                            margin-right:30px;
                          }"),
      tags$style(".leftAlign{float:left;}"),
      align = "left",
      div(class='padding',
          h4("Data used:"),
          h5(a("Rat sightings", href="https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9"), 
             h6("Filtered to 311 calls from Jan 1 2020 to Dec 31 2020, of complaint type 'rodent' in Manhattan borough.
Calls related to noise, parking violations, and other non-emergency police related matters are also
excluded from this visualization.")),
      ),
      div(class='padding',
          h4("Visualization:"),
      ),
      
    ),
    
    

    fluidRow(align = "center",
             h3("Wordcloud of the descriptor variable of all NYC311 complaints in 2020"),
             br(),
             h6("Chart 15"),
      img(src='Capture.PNG',width="50%"),
      br(),
      h6("Chart 16"),
      img(src='Picture2.png',width="50%"),
      br(),
      h6("For reference:"),
      img(src='rat.png',),
      br(),
      br(),
      p(class = "padding", align = "left", "For required text analysis, we have again referred to the 2020 rodent related 311 reports, specifically on
the descriptor variable, where various notes are left on the nature or details of the complaint. Two word
clouds are presented. The first is a basic wordcloud created with the ggwordcloud package. Words
related to rat sightings are differentiated from others by color. Viewers can see in this wordcloud that
the descriptor variable does have lots of mentions of rodent related issues. The second wordcloud
presented is also from the ggwordcloud package, but with an added mask, intended to create a
wordcloud the shape of a rat. This visualization is slightly more visually appealing, but reveals the exact
same information to the reader. Rat sightings are often mentioned in the descriptor variable of the data
set."),br(),br()
    ),
    
    
    # fluidRow(
    #   align = "center",
    #   plotOutput("brendan_wc1"),
    #   plotOutput("bendan_wc2"),
    # ),
  ),
)

  
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
                                                                                                              title= "Complaint status",
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
            theme_light()+
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
          ggplot(data=plot_this, aes(x=Year, y=Freq)) + geom_line(aes(color=case_status)) + geom_point(aes(colour=case_status))
          #+ scale_colour_manual(name = 'Case status',values =c('green'='green','cadetblue' = 'cadetblue', 'orange'='orange', 'darkred'='darkred'), labels = c("closed","in progress", "assigned",'pending'))
          + ggtitle('Complaint status trend') +  scale_x_continuous(breaks=seq(min_year, max_year, 1)) + theme_light()
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
      ylab("Weight of Waste (Tons)") + theme_light()
    p
  })
  
  output$pratishta2 <- renderPlotly({
    p <- ggplot(rat_ton_date, aes(x=Created.Date, y=n)) +
      geom_line(aes(color = Borough)) +
      geom_point(aes(color = Borough)) +
      xlab("Date by Months") +
      ylab("Number of rat sightings") + theme_light()
    p
  })
  
  output$pratishta3 <- renderPlotly({
    p <- ggplot(rat_ton_date, aes(x=Created.Date, y=rate)) +
      geom_line(aes(color = Borough)) +
      geom_point(aes(color = Borough)) +
      xlab("Date by Months") +
      ylab("Rate of rats per kiloton of waste") + theme_light()
    p
  })
  
  output$pratishta4 <- renderPlotly({
    ton +
      xlab("Boroughs") +
      ylab("Weight of Waste (Tons)") + theme_light()

  })
  
  output$pratishta5 <- renderPlotly({
    rat +
      xlab("Boroughs") +
      ylab("Number of Rat Sightings") + theme_light()

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

  # brendan's viz -----------------------------------------------------------

  output$brendan_chart1 <- renderPlotly({
    plot1 <-ggplot() + geom_bar(data=rat_borough, aes(x=Borough, y=count), stat="identity", fill=rainbow(n=5)) + ggtitle("2020 rodent reports") + ylab("Number of 311 calls\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.position="none") +  theme(axis.text.x = element_text(angle = 40, vjust=.7)) 
    plot1
})
  output$brendan_chart2 <- renderPlotly({
    plot2 <- ggplot() + geom_bar(data=restaurant_borough, aes(x =Borough, y= count), stat="identity", fill=rainbow(n=5)) + ggtitle("Outdoor restaurants") + ylab("Applications approved\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), legend.position="none", axis.title.y=element_text(face="bold")) + theme(axis.text.x = element_text(angle = 40, vjust=.7))
    plot2
})
  output$brendan_chart3 <- renderPlotly({
    plot3 <- ggplot() + geom_bar(data=inspection_count_2020, aes(x=Borough, y=count), stat="identity", fill=rainbow(n=5)) + ggtitle("Restaurants w/ B or C inspection scores") + ylab("Restaurants\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), legend.position="none", axis.title.y=element_text(face="bold")) +  theme(axis.text.x = element_text(angle = 40, vjust=.7))
    plot3
})
  output$brendan_chart4 <- renderPlotly({
    plot4 <- ggplot() + geom_bar(data=count_street, aes(x=Borough, y=count), stat="identity", fill=rainbow(n=5)) + ggtitle("Street dining") + ylab("Approved restaurants\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.position="none") +  theme(axis.text.x = element_text(angle = 40, vjust=.7))
    plot4
})
    output$brendan_chart5 <- renderPlotly({
    plot5 <- ggplot() + geom_bar(data=count_sidewalk, aes(x=Borough, y=count), stat="identity", fill=rainbow(n=5)) + ggtitle("Sidewalk dining") + ylab("Approved restaurants\n") + xlab("\nBorough") + theme_light() + theme(plot.title=element_text(face="bold"), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.position="none") +  theme(axis.text.x = element_text(angle = 40, vjust=.7))
    plot5
})

    
  
  
  
  output$brendan_map <- renderLeaflet({

    
    
    interactive_map <- leaflet() %>% addProviderTiles(providers$Stamen.TonerLite,
                                                      options = providerTileOptions(noWrap = TRUE)) %>% addCircleMarkers(data=manhattan_open, lng=~Longitude, lat=~Latitude, radius=1, color= "red", fillOpacity=1, popup=~paste('Restuarant:', manhattan_open$Restaurant.Name, '<br>', 'Street Dining?', manhattan_open$Approved.for.Roadway.Seating, '<br>', 'Sidewalk Dining?', manhattan_open$Approved.for.Sidewalk.Seating, '<br>')) %>%  addMarkers(data=manhattan_311,lng=~manhattan_311.Longitude, lat=~manhattan_311.Latitude, icon=icon, popup=~paste('Address:',manhattan_311$manhattan_311.Incident.Address,'<br>', 'Call Date:', manhattan_311$manhattan_311.Created.Date,'<br>','Descriptor:', manhattan_311$manhattan_311.Descriptor,'<br>'), clusterOptions= markerClusterOptions(disableClusteringAtZoom=16))  %>%
      setView(lng = -73.98928, lat = 40.77042, zoom = 12)
      
      
      
      
    })

  # output$brendan_wc1 <- renderPlot({
  #   (wordcloud1 <- ggplot(descriptors3, aes(label=word, size=n, color=col)) + geom_text_wordcloud_area(mask=img, rm_outside = TRUE) + scale_size_area(max_size=5) + theme_classic())
  #   
  # })
  # 
  # output$brendan_wc2 <- renderPlot({
  #   (wordcloud2 <- ggplot(descriptors3, aes(label=word, size=n, color=col)) + geom_text_wordcloud_area() + scale_size_area())
  #   
  # })
  
  
}

shinyApp(ui = ui, server = server)
