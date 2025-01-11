library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(DBI)        # For database connection
library(RPostgres)
library(scales)
library(leaflet.extras)

# Database connection details  
host <- "postgresql-benamollo.alwaysdata.net"        # e.g., "localhost" or IP address
port <- "5432"             # Default PostgreSQL port
dbname <- "benamollo_spatialdb"  # Replace with your database name
user <- "benamollo"    # Your PostgreSQL username
password <- "5242473@Amollo"  # Your PostgreSQL password

# Connect to the PostgreSQL database
con <- dbConnect(
  RPostgres::Postgres(),
  host = host,
  port = port,
  dbname = dbname,
  user = user,
  password = password
)

# List tables in the database (optional, to explore)
tables <- dbListTables(con)
print(tables)

# Define a function to read a spatial table based on the table name
read_spatial_table <- function(con, table_name) {
  # Use st_read to read the spatial table from the database
  st_read(con, query = sprintf('SELECT * FROM "%s"', table_name))
}

# List of spatial tables you need to read
spatial_tables <- c("Alego_Usonga_Wards", "Alego_Usonga_Schools_m", 
                    "Alego_Usonga__Health_Facilities", "AU_clipped_roads", "Alego_Usonga")

# Read each spatial table and assign to corresponding variable
AU_Wards <- read_spatial_table(con, spatial_tables[1])
AU_Schools <- read_spatial_table(con, spatial_tables[2])
AU_HFs <- read_spatial_table(con, spatial_tables[3])
AU_Roads <- read_spatial_table(con, spatial_tables[4])
AU_Const <- read_spatial_table(con, spatial_tables[5])

# Define the target CRS (WGS84)
target_crs <- 4326

# Transform each dataset to WGS84
AU_Wards <- st_transform(AU_Wards, crs = target_crs)
AU_Schools <- st_transform(AU_Schools, crs = target_crs)
AU_HFs <- st_transform(AU_HFs, crs = target_crs)
AU_Roads <- st_transform(AU_Roads, crs = target_crs)
AU_Const <- st_transform(AU_Const, crs = target_crs)
# View the data (optional, to check the results)
# print(AU_Wards)
# print(AU_Schools)
# print(AU_HFs)
# print(AU_Roads)
# print(AU_Const)
AU_Const$Nam <- "Alego Usonga"

# Create a function to assign thickness based on 'highway' attribute
line_thickness <- function(highway_type) {
  case_when(
    highway_type %in% c("primary", "trunk", "secondary") ~ 4,        # Primary and trunk roads - thick
    highway_type == "unclassified" ~ 1,                     # Secondary roads - medium thickness
    highway_type == "tertiary" ~ 1,                      # Tertiary roads - thinner
    highway_type == "residential" ~ 1,                   # Residential roads - thinnest
    TRUE ~ 2                                             # Other types (path, footway, etc.) - default thickness
  )
}

# Add thickness based on the highway attribute
AU_Roads$thickness <- sapply(AU_Roads$highway, line_thickness)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Alego Usonga Portal"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # palette <- colorBin(palette = "YlGnBu", domain = AU_Wards$pop2009, bins = 5)
  
  
  output$distPlot <- renderLeaflet({
    palette <- colorBin(palette = "YlGnBu", domain = AU_Wards$pop2009, bins = 5)
    
    leaflet() %>%
      # Set initial view of the map (latitude, longitude, zoom level)
      setView(lng = 34.2338, lat = 0.06163787, zoom = 10) %>%
      
      # Add Esri World Imagery basemap
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      
      # Add OpenTopoMap basemap
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
      
      # Add CartoDB Positron basemap
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      
      # Add OpenStreetMap basemap
      addProviderTiles(providers$OpenStreetMap, group = "Open StreetMap") %>%
      
      # Add the shapefile layers to the map
      addPolygons(data = AU_Wards, 
                  color = "green", 
                  weight = 2, 
                  opacity = 1, 
                  fillOpacity = 0.2,
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "red",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                  ),
                  label = ~ward, # Tooltip with ward name and population
                  labelOptions = labelOptions(
                    style = list("font-weight" = "bold"),
                    textsize = "12px",
                    direction = "auto"
                  ),
                  group = "Wards") %>%
      addPolygons(data = AU_Const, 
                  color = "blue", 
                  weight = 3, 
                  opacity = 1, 
                  fillOpacity = 0.9,
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "red",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                  ),
                  label = ~Nam, 
                  labelOptions = labelOptions(
                    style = list("font-weight" = "bold"),
                    textsize = "12px",
                    direction = "auto"
                  ),
                  group = "Constituency") %>%
      addPolygons(data = AU_Wards,
                  fillColor = ~palette(pop2009), # Set color based on pop2009
                  weight = 1, # Border weight
                  opacity = 1, # Border opacity
                  color = "yellow", # Border color
                  group = "Population Distribution",
                  fillOpacity = 0.7, # Fill opacity
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "blue",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                  ),
                  label = ~paste0(ward, ": Has a Population of ", comma(pop2009), " People"), # Tooltip with ward name and population
                  labelOptions = labelOptions(
                    style = list("font-weight" = "bold"),
                    textsize = "12px",
                    direction = "auto"
                  )
      ) %>%
      addCircleMarkers(data = AU_Schools, 
                       label = ~school_nam, 
                       color = "red", 
                       radius = 5, 
                       group = "Schools") %>%
      addCircleMarkers(data = AU_HFs, 
                       label = ~f_name,
                       color = "black", 
                       radius = 5, 
                       group = "Health Facilities") %>%
      addPolylines(
        data = AU_Roads,
        color = ~ifelse(
          highway == "primary" | highway == "trunk", "red", 
          ifelse(highway == "unclassified", "red", 
                 ifelse(highway == "tertiary", "orange", "gray"))
        ),  # Color by highway type
        weight = ~thickness,  # Line thickness based on the attribute
        opacity = 1,
        popup = ~highway,  # Popup with highway type
        group = "Connectivity"
      ) %>%
      addFullscreenControl() %>%
      # Add layer control to toggle layers and basemaps
      addLayersControl(
        baseGroups = c("CartoDB Positron", "Esri World Imagery", "Open Topo Map", "Open StreetMap"),
        overlayGroups = c("Constituency", "Wards", "Population Distribution", "Schools", "Health Facilities", "Connectivity"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Constituency", "Population Distribution", "Schools", "Health Facilities", "Connectivity"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
