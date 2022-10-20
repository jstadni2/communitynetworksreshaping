# devtools::install_github("r-spatial/mapview@develop")
# devtools::install_github("bhaskarvk/leaflet.extras")
# devtools::install_github("r-spatial/mapedit")
#install.packages("shinyjs")

#install.packages(c("utf8", "data.table", "tidyverse", "sf", "tmaptools", "classInt", "RColorBrewer", "leaflet"))
#lapply(c("data.table", "tidyverse", "readxl", "sf", "RColorBrewer", "leaflet"), require, character.only = TRUE)

library(utf8)
library(data.table)
library(tidyverse)
library(readxl)
library(sf)
library(RColorBrewer)
library(leaflet)
library(mapview)
library(mapedit)
library(shiny)
library(leafem)
library(shinyjs)

#setwd("C:\\Users\\user\\path\\to\\CommunityNetworksReshaping")

#Import Census data using tigris instead?

##Import data and shape files

S1701_Poverty <- fread("ACSST5Y2019.S1701_data_with_overlays_2021-03-11T131230.csv",
                       skip=1,
                       select = c('id',
                                  'Geographic Area Name',
                                  'Estimate!!Total!!Population for whom poverty status is determined',
                                  'Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level'))

Community_Networks <- read_excel("NEW MASTER SNAP-Ed Community Networks_Updated 3.23.21.xlsx", sheet = "Networks")

wic_sites <- fread("wic_sites_geocodio.csv")

fcrc_sites <- fread("fcrc_sites_geocodio.csv")

FQHCs <- read_excel("FQHCs.xlsx")

IL_tracts_sf <- st_read("cb_2019_17_tract_500k")
#IL_tracts_sf <- st_transform(IL_tracts_sf, 32618)

IL_counties_sf <- st_read("cb_2019_il_county_500k")

IL_cities_sf <- st_read("IL_Census_Populated_Places_Areas")

CCAs_sf <- st_read("Boundaries - Community Areas (current)")

##Census Poverty Data

S1701_Poverty <- S1701_Poverty %>% rename(AFFGEOID = 'id',
                                          total_population = 'Estimate!!Total!!Population for whom poverty status is determined',
                                          individuals_income_below_185_percent_poverty_level = 'Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level')

C_Map_Poverty <- S1701_Poverty %>% separate('Geographic Area Name', c("Census Tract","County", "State"), sep = ",")

C_Map_Poverty$`Census Tract` <- gsub('Census Tract ', '', C_Map_Poverty$`Census Tract`, fixed=TRUE)
C_Map_Poverty$`County` <- gsub(' County', '', C_Map_Poverty$`County`, fixed=TRUE)

C_Map_Poverty$snap_eligibility_percent<- round(100 * (C_Map_Poverty$individuals_income_below_185_percent_poverty_level / C_Map_Poverty$total_population))

IL_tracts_sf_merged <- merge(IL_tracts_sf, C_Map_Poverty, by = "AFFGEOID")

##FCS Community Networks

IL_cities_sf$NAME <- gsub('De Pue', 'DePue', IL_cities_sf$NAME, fixed=TRUE)

commuity_network_cols <- c("ID#", "Name of the SNAP-Ed Community Network", "Unit", "Reshaped Census Tracts", "Updated City/County descriptors", "Is City Network", "Is County Network", "Network, no programming")

reshaped_community_networks <- Community_Networks[commuity_network_cols] %>%
  filter(!is.na(Community_Networks$`Reshaped Census Tracts`)) %>%
  separate_rows("Reshaped Census Tracts", sep=",")
reshaped_community_networks$"Reshaped Census Tracts" <- trimws(reshaped_community_networks$"Reshaped Census Tracts", which = c("both"))
reshaped_community_networks <- inner_join(IL_tracts_sf, reshaped_community_networks, by = c("GEOID" = "Reshaped Census Tracts"))

FCS_Community_Networks <- Community_Networks %>% separate_rows("Updated City/County descriptors", sep=",")

FCS_Community_Networks$`Updated City/County descriptors` <- trimws(FCS_Community_Networks$`Updated City/County descriptors`, which = c("both"))

county_networks <- FCS_Community_Networks[commuity_network_cols] %>% 
  filter(FCS_Community_Networks$`Is County Network` == "Y" & is.na(FCS_Community_Networks$"Reshaped Census Tracts"))
county_networks$`Updated City/County descriptors` <- gsub(' County', '', county_networks$`Updated City/County descriptors`, fixed=TRUE)

county_networks <- inner_join(IL_counties_sf, county_networks, by = c("NAME" = "Updated City/County descriptors"))

city_networks <- FCS_Community_Networks[commuity_network_cols] %>% 
  filter(FCS_Community_Networks$`Is City Network` == "Y" & is.na(FCS_Community_Networks$"Reshaped Census Tracts"))
city_networks <- inner_join(IL_cities_sf, city_networks, by = c("NAME" = "Updated City/County descriptors"))
#check_cities <- filter(city_networks, !city_networks$"Updated City/County descriptors" %in% city_networks2$"NAME")
city_networks <- st_transform(city_networks, 4269)

fcs_networks_sf_cols <- c("NAME", "ID#", "Name of the SNAP-Ed Community Network", "Unit", "Is City Network", "Is County Network", "Network, no programming", "geometry")       
fcs_networks_sf <- rbind(reshaped_community_networks[fcs_networks_sf_cols], county_networks[fcs_networks_sf_cols])
fcs_networks_sf <- rbind(fcs_networks_sf, city_networks[fcs_networks_sf_cols])

fcs_networks_sf <-  fcs_networks_sf %>% rename(community_network_name = "Name of the SNAP-Ed Community Network")

fcs_networks_sf <- fcs_networks_sf  %>%
  group_by(fcs_networks_sf$community_network_name) %>%
  summarise() %>%
  ungroup() %>% st_as_sf()

fcs_networks_sf <-  fcs_networks_sf %>% rename(community_network_name = "fcs_networks_sf$community_network_name")

fcs_networks_sf['total_population'] <- NA
fcs_networks_sf['individuals_income_below_185_percent_poverty_level'] <- NA

fcs_networks_sf2  <- sf::st_simplify(fcs_networks_sf, preserveTopology = TRUE, dTolerance = 0.01)

##CPHP Community Networks

CPHP_cols <- c("ID#", "Name of the SNAP-Ed Community Network", "Unit")
CPHP_Community_Networks <- filter(Community_Networks[CPHP_cols], (Unit == 'CPHP'))

CCAs_sf$community = str_to_title(CCAs_sf$community)

CPHP_Community_Networks_sf <- inner_join(CCAs_sf, CPHP_Community_Networks, by = c("community" = "Name of the SNAP-Ed Community Network"))

CPHP_Community_Networks_sf <- st_transform(CPHP_Community_Networks_sf, 4269)

CPHP_Community_Tracts_sf <- st_join(CPHP_Community_Networks_sf, IL_tracts_sf)
#although coordinates are longitude/latitude, st_intersects assumes that they are planar

CPHP_Community_Tracts_sf <- merge(CPHP_Community_Tracts_sf, C_Map_Poverty, by = "AFFGEOID")

CPHP_Community_Networks_sf2 <- CPHP_Community_Tracts_sf  %>%
  group_by(CPHP_Community_Tracts_sf$community) %>%
  summarise(total_population = sum(total_population),
            individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level),
            do_union = TRUE) %>%
  ungroup() %>% st_as_sf()

CPHP_Community_Networks_sf2 <- CPHP_Community_Networks_sf2 %>% rename(community_network_name = 'CPHP_Community_Tracts_sf$community')
#why does group_by rename label?

##SNAP-Ed Community Networks

snap_ed_networks_sf <- rbind(fcs_networks_sf2, CPHP_Community_Networks_sf2)

snap_ed_networks_sf $snap_eligibility_percent<- round(100 * (snap_ed_networks_sf $individuals_income_below_185_percent_poverty_level / snap_ed_networks_sf $total_population))

#remove target network columns?
network_cols <- c("ID#", "Name of the SNAP-Ed Community Network", "Unit", "FY20 target network", "FY21 target network", "Network, no programming")
snap_ed_networks_sf <- left_join(snap_ed_networks_sf, Community_Networks[network_cols], by = c("community_network_name"= "Name of the SNAP-Ed Community Network"))

#snap_ed_networks_sf <- left_join(snap_ed_networks_sf, Unit_Regions[c("Unit number", "Region")] %>% distinct(), by = c("Unit"= "Unit number"))
#Integrate with Shiny to create filters
#https://stackoverflow.com/questions/46186014/changing-leaflet-map-according-to-input-without-redrawing-multiple-polygons

#Create groups/attributes for CPHP and FCS

fqhc_sites <- filter(FQHCs, (FQHCs$"Grant #" != '[No Data]' & FQHCs$"Services Delivered at Site" == "Yes"))

##Leaflet Map Visuals

pal1 <- colorNumeric("Blues", domain=IL_tracts_sf_merged$snap_eligibility_percent)
pal2 <- colorNumeric("Reds", domain=IL_tracts_sf_merged$individuals_income_below_185_percent_poverty_level)

popup_sb1 <- paste0("County: \n", as.character(IL_tracts_sf_merged$"County"), 
                    "</br/> Census Tract: \n", as.character(IL_tracts_sf_merged$"Census Tract"), 
                    "</br/> # of Eligible Individuals: \n", as.character(IL_tracts_sf_merged$individuals_income_below_185_percent_poverty_level), 
                    "</br/> % of Individuals Below 185% of poverty level (SNAP Eligibility): \n", as.character(IL_tracts_sf_merged$snap_eligibility_percent))
popup_sb2 <- paste0("Community Network: \n", as.character(snap_ed_networks_sf$community_network_name), 
                    "</br/> # of Eligible Individuals: \n", as.character(snap_ed_networks_sf$individuals_income_below_185_percent_poverty_level), 
                    "</br/> % of Individuals Below 185% of poverty level (SNAP Eligibility): \n", as.character(snap_ed_networks_sf$snap_eligibility_percent))

icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = 'black')
icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion')

##Shiny App

ui <- fluidPage(
  fluidRow(
    shinyjs::useShinyjs(),
    column(
      8,
      h3("Select Census Tracts"),
      # our new select module ui
      selectModUI("selectmap", height = 800)
    ),
    column(
      4,
      h3("Selected Census Tracts"),
      textInput("community_network_name", "Community Network Name", value = "", width = NULL, placeholder = NULL), 
      shinyjs::hidden(downloadButton("downloadts","Export")),
      dataTableOutput("table")
    )
)
)

server <- function(input, output, session) {
  t_sel <- callModule(
    selectMod,
    "selectmap",
  leaflet() %>%
    addProviderTiles("CartoDB.Voyager") %>% 
    addPolygons(data = IL_tracts_sf_merged, 
                fillColor = ~pal1(IL_tracts_sf_merged$snap_eligibility_percent), 
                fillOpacity = 0.5, 
                weight = 0.9, 
                smoothFactor = 0.2, 
                stroke=TRUE,
                color="white",
                popup = ~popup_sb1,
                group = "Eligibility %") %>% 
    addPolygons(data = IL_tracts_sf_merged, 
                fillColor = ~pal2(IL_tracts_sf_merged$individuals_income_below_185_percent_poverty_level), 
                fillOpacity = 0.5, 
                weight = 0.9, 
                smoothFactor = 0.2, 
                stroke=TRUE,
                color="white",
                popup = ~popup_sb1,
                group = "Individuals") %>% 
    addPolygons(data = snap_ed_networks_sf, #single polygon for SNAP-Ed networks, 
                fillOpacity = 0,
                
                highlight = highlightOptions(
                  weight = 3,
                  fillOpacity = 0,
                  color = "green",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = FALSE),
                
                weight = 2, 
                smoothFactor = 0.2, 
                stroke=TRUE,
                color="green",
                popup = ~popup_sb2,
                group = "Networks",
                
                label = as.character(snap_ed_networks_sf$community_network_name),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>% 
    addFeatures(IL_tracts_sf, 
                layerId = ~IL_tracts_sf$GEOID,
                fillColor = "white",
                color = "black",
                fillOpacity = 1,
                group = "Census Tract Selection",
                label = as.character(IL_tracts_sf$GEOID),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addAwesomeMarkers(data=wic_sites, 
                      lng=~Longitude, 
                      lat=~Latitude,
                      icon = icon.glyphicon,
                      popup=~as.character(site_name), 
                      label = ~as.character(site_name),
                      group = "WIC Sites") %>% 
    addAwesomeMarkers(data=fcrc_sites, 
                      lng=~Longitude, 
                      lat=~Latitude,
                      icon = icon.fa,
                      popup=~as.character(site_name), 
                      label = ~as.character(site_name),
                      group = "FCRC Sites") %>% 
    addAwesomeMarkers(data=fqhc_sites, 
                      lng=~Longitude, 
                      lat=~Latitude,
                      icon = icon.ion,
                      popup=~as.character(fqhc_sites$"Site Name"), 
                      label = ~as.character(fqhc_sites$"Site Name"),
                      group = "FQHC Sites") %>% 
    addLegend(pal = pal1, 
              values = IL_tracts_sf_merged$snap_eligibility_percent, 
              position = "bottomright", 
              title = 'SNAP Eligibility %',
              group = "Eligibility %") %>%
    addLegend(pal = pal2, 
              values = IL_tracts_sf_merged$individuals_income_below_185_percent_poverty_level, 
              position = "bottomright", 
              title = 'Eligible Individuals',
              group = "Individuals") %>%
    addLayersControl(
      overlayGroups = c("Census Tract Selection", "Eligibility %", "Individuals", "Networks", "WIC Sites", "FCRC Sites", "FQHC Sites"),
      options = layersControlOptions(collapsed = FALSE))
  )
  
  rv <- reactiveValues(selecttract=NULL)
  
  observe({
    ts <- t_sel()
    Selected_Census_Tracts <- ts[which(ts$selected==TRUE), "id"]
    output$table <- renderDataTable(as.data.frame((Selected_Census_Tracts)))
  })
  
  output$downloadts <- downloadHandler(
    filename = function() {
      paste(input$community_network_name, ' Census Tracts.csv', sep='')
    },
    content = function(file) {
      ts <- t_sel()
      ts_2 <- ts[which(ts$selected==TRUE), ]
      ts_2$community_network_name <- input$community_network_name
      ts_2 <- ts_2 %>% rename("GEOID" = "id")
      ts_2 <- ts_2 %>% add_row(GEOID = paste(ts_2$GEOID, collapse=', ' ), community_network_name = "Census Tract List", .before = 1)
      
      write.csv(ts_2[c("community_network_name", "GEOID")], file, row.names = FALSE)
    }
  )
  
  observeEvent(input$community_network_name, {
    if (input$community_network_name == "")
      shinyjs::hide("downloadts")
    else
      shinyjs::show("downloadts")
  })
  
}
shinyApp(ui, server)
