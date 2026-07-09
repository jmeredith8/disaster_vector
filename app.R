
# Flooding and Mosquitoes Articles Dashboard

# setwd("R:/Ryan_Lab/Jakob_M/Projects/NewsDash")

# Load in packages
library(shiny)
library(leaflet)
library(sf)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(tidyverse) 
library(maps)
library(mapproj)
library(mapdata)
library(ggthemes)
library(maps)
library(ggplot2)
library(viridis)
library(viridisLite)
library(gridExtra)
library(ggspatial)
library(tigris)
library(ggpubr)
library(tidyterra)
library(paletteer)
library(classInt)
library(shinyWidgets)
library(rsconnect)
library(here)
library(treemap)

# addResourcePath("www", file.path(getwd(), "www"))
addResourcePath("www", "www")

# Load in global map and set projection to match points
world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% 
  st_transform(4326) |> 
  mutate(geounit = case_when(
    geounit == "São Tomé and Principe" ~ "Sao Tome and Principe",
    TRUE ~ geounit
  ))

# Use column geounit for joining purposes if needed
print(world$geounit)

# Load in locations of news articles
locations <- read_csv(here("locations.csv"), locale = locale(encoding = "UTF-8")) %>%
  mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "UTF-8", sub = "")))

locations <- locations %>% 
  filter(!is.na(lat), !is.na(long)) %>% 
  mutate(
    long = as.numeric(gsub("[^0-9.-]", "", iconv(long, "UTF-8", "ASCII", sub = ""))),
    lat = as.numeric(gsub("[^0-9.-]", "", iconv(lat, "UTF-8", "ASCII", sub = ""))),
    location = iconv(location, "UTF-8", "ASCII", sub = "")
    )

locations <- locations %>% 
  mutate(
    lat_j = jitter(lat, amount = 0.005),
    long_j = jitter(long, amount = 0.005)
  )
  
locations %>% 
  filter(is.na(lat_j) | is.na(long_j) | !is.finite(lat_j) | !is.finite(long_j))

locations <- locations %>% 
  mutate(
    disease = replace_na(disease, "Nuisance"),
    disease = if_else(trimws(disease) == "None", "Nuisance", disease),
    link = gsub("#.*$", "", locations$link),
    unique_id = row_number(),
    species = replace(species, species == "Unknown mosquitoes", "Unknown"))

locations$species

expand_var <- function(data, var) {
  data %>%
    mutate(selected_var = as.character(.data[[var]])) %>%
    mutate(selected_var = ifelse(
      is.na(selected_var) | trimws(selected_var) == "" | selected_var == "NA",
      "Unknown", selected_var
    )) %>%
    mutate(split_var = strsplit(selected_var, ",\\s*")) %>%
    tidyr::unnest(split_var) %>%
    mutate(selected_var = trimws(split_var)) %>%
    mutate(selected_var = ifelse(
      is.na(selected_var) | trimws(selected_var) == "",
      "Unknown", selected_var
    )) %>%
    select(-split_var)
}

plot_data <- locations %>%
  filter(!is.na(lat_j), !is.na(long_j)) %>%
  mutate(
    disease = replace_na(disease, "Nuisance"),
    disease = if_else(trimws(disease) == "None", "Nuisance", disease),
    primary_disease = trimws(sapply(strsplit(as.character(disease), ","), '[', 1)),
    vector_family = case_when(
      grepl("Aedes", species, ignore.case = TRUE) ~ "Aedes",
      grepl("Anopheles", species, ignore.case = TRUE) ~ "Anopheles",
      grepl("Culex", species, ignore.case = TRUE) ~ "Culex",
      grepl("Culicoides", species, ignore.case = TRUE) ~ "Culicoides",
      grepl("Culiseta", species, ignore.case = TRUE) ~ "Culiseta",
      grepl("Mansonia", species, ignore.case = TRUE) ~ "Mansonia",
      grepl("Ochlerotatus", species, ignore.case = TRUE) ~ "Ochlerotatus",
      grepl("Psorophora", species, ignore.case = TRUE) ~ "Psorophora",
      grepl("Blackflies", species, ignore.case = TRUE) ~ "Blackflies",
      grepl("Sandflies", species, ignore.case = TRUE) ~ "Sandflies",
      grepl("Unknown", species, ignore.case = TRUE) ~ "Unknown"
    ))

ui <- navbarPage("Vectorborne Diseases and Natural Disasters",
    
    header = tags$head(
      tags$style(HTML("
          #Make the sidebars look cleaner
          .sidebar-header {
            color: #2c3e50;
            font-weight: bold;
            margin-top: 20px;
          }
          
          # Style the main instructional text
          .instruction_text {
          font-size: 16px;
          line-height: 1.6;
          color: #444444;
          background-color: #f9f9f9;
          padding: 15px;
          border-left: 5px solid #007bc2;
          border-radius: 4px;
          }
          
          #Title text
          .title-header {
          text-align: center;
          padding: 20px;
          color: #2c3e50;
          width: 100%;
          }
          "))
                 ),
                              
    tabPanel(
      "Reported Events Map",
      titlePanel(
        HTML("<div style='text-align: center; font-size: 1.25em; font-weight: bold; '>Global Natural Disasters and Disease Outbreaks Dashboard</div>")
      ),
      
      sidebarLayout(
        sidebarPanel(
          p("Select a location to zoom the camera to that region"),
          
          selectInput("region_select", "Zoom to region", choices = NULL),
          selectInput("country_select", "Zoom to country", choices = NULL),
          
          hr(),
          
          radioButtons(
            inputId = "color_by",
            label = "Color Markers By:",
            choices = c(
              "Total Observations" = "none",
              "Event Type" = "event",
              "Disease" = "disease",
              "Vector Species" = "species"
            ),
            selected = "none"
          ),
          
          p("Use the map to click on the markers for more details"),
          
          hr(),
          
          uiOutput("click_info")
          
        ),
        
        mainPanel(
          leafletOutput("map", height = 700),
          # hr(),
          # uiOutput("click_info")
        )
      ),
      
      hr(),
      fluidRow(
        column(5,
               div(
                 style = "text-align: center;",
                 img(src = "www/tiptoss_pic.png", height = 500, width = 575),
               ),
        ),
        column(7,
               tags$div(
                 id = "comp_prep",
                 style = "flex: 1; padding: 12px; font-size: 14px; border: none; outline: none; overflow-y: auto; line-height: 1.6;",
                 tags$h3("How Do I Protect Myself?"),
                 p("Prevention is critical when trying to stop disease spread following natural disasters"),
                 p("If we can stop the insects that spread the disease, we can stop it from circulating"),
                 p("Personal Protection: Stopping Insects from Biting You"),
                 tags$ul(
                   tags$li("Wear clothes that cover as much of your body as possible"),
                   tags$li("Use mosquito nets if sleeping during the day"),
                   tags$li("Install window screens in your home"),
                   tags$li("Use approved mosquito repellents (containing DEET, Picaridin, or IR3535)",),
                   tags$li("Install vaporizers and traps for mosquitoes outside of your home"),
                   tags$li("Avoid going outside during peak biting hours (dawn and dusk)"),
                 ),
                 p("Environmental Protection: Tip and Toss"),
                 tags$ul(
                   tags$li("Trying to reduce the places where mosquitoes like to reproduce to reduce their populations"),
                   tags$li("Tip over any containers with excess standing water"),
                   tags$li("Toss any containers or other items that collect rain"),
                   tags$li("Apply approved insecticides to outdoor water containers that cannot be tossed"),
                   tags$li("Ask your local health department or vector control unit about “Mosquitofish” which can be added to water containers and feed on mosquito larvae")
                 ),
                 p("Many of these efforts can be inhibited by natural disaster impacts"),
                 tags$ul(
                   tags$li("Encourage your local health department and emergency management agency to have plans to deal with insect vectors that are native to your area before disasters strike"),
                   tags$li("Rapid cleanup following natural disasters is critical to reducing artificial breeding sites for mosquitos and other vectors"),
                 ),
                 p("Ask your local health department or vector control unit to trap and test mosquitoes or other insects found on your property to better support their prevention efforts"),
               ) 
        )
      )
    ),
    
    tabPanel(
      "Charts and Graphs",
      titlePanel(
        HTML("<div style='text-align: center; font-size: 1.25em; font-weight: bold; '>Charts and Graphs</div>")
      ),
      
      sidebarLayout(
        sidebarPanel(
          
          p("Select a variable to view a treemap and pie chart of variable occurrence"),
          
          radioButtons(
            inputId = "treemap_var",
            label = "Display Treemap and Pie Chart By:",
            choices = c(
              "Event Type" = "event",
              "Disease Reported" = "primary_disease",
              "Vector Species" = "vector_family"
            ),
            selected = "event"
          ),
          
          h4("Treemap/Pie Chart"),
          div(
            tags$p("Each figure to the right shows the frequency that different factors were reported over the scope of the review. Items that appear in larger boxes (treemap) or wedges (pie chart) are mentioned more often than others.")
          ),
          h4("Stacked Bar Chart"),
          div(
            tags$p("Each column shows the total number of articles cited for each year in the range of the study period. Within each column, the different colors correspond to the number of times a specific item within each variable was mentioned, to show the difference in characteristics of the criteria seen over time.")
          ),
          
          hr(),
          uiOutput("total_stats")
          
          
          # p("Use the map to click on the markers for more details"),
          # 
          # hr(),
          # 
          # uiOutput("hover_checklist"),
          # 
          # hr(),
          # 
          # uiOutput("total_stats"),
          # 
          # hr(),
          # 
          # uiOutput("click_info")
        ),
        
        mainPanel(
          fluidRow(
            style = "margin-top: 50px;",
            column(
              width = 12,
              # offset = 0.5,
              plotOutput("tree_var", height = 600)),
          ),
          
          fluidRow(
            style = "margin-top: 50px;",
            column(
              width = 12,
              # offset = 1, 
              plotOutput("pie_var", height = 600))
          )
        )
      ),
      
      fluidRow(
        style = "margin-top: 50px;",
        column(
          width = 10, 
          offset = 1, 
          plotOutput("chart_year", height = 600))
      ),
      
      hr(),
      fluidRow(
        column(12,
               h3("Heat Map Interpretation", class = "sidebar-header"),
               p("What do these show?"),
               tags$ul(
                 tags$li("Heat maps are used to show the number of times that two factors overlap, 
                    and are used to understand patterns when there are a lot of options to consider within these two factors"),
                 tags$li("Colors are representative of the number of times that the variable overlap, and is shown in the legend to the right of the graph")
               ),
               p("Natural Events vs. Vector Family"),
               tags$ul(
                 tags$li("Shows the number of times that certain families of insect vectors (such as Aedes, Anopheles, Culex, etc.) were seen following different disasters (such as typhoons, floods, earthquakes, etc.)"),
                 tags$li("Lowest values are in dark red, highest values are in bright yellow"),
                 tags$li("Number of occurrences is written inside of each square")
               ),
               p("Natural Events vs. Disease"),
               tags$ul(
                 tags$li("Shows the number of times that certain diseases (such as Dengue Fever, Malaria, Zika, etc.) were seen following different disasters"),
                 tags$li("Lowest values are in dark red, highest values are in bright yellow"),
                 tags$li("Number of occurrences is written inside of each square")
               ),
               p("General Themes from the Data"),
               tags$ul(
                 tags$li("Flooding, climate change, and hurricanes are the three most frequently cited natural hazards related to increased burden of disease vectors"),
                 tags$li("Aedes, Anopheles, and Culex mosquitoes are most frequently cited, along with unknown or unmentioned mosquito species"),
                 tags$li("Flooding, climate change, and hurricanes also caused the most diverse array of diseases, with flooding in particular leading to a large range of outcomes"),
                 tags$li("Dengue Fever, Malaria, and Chikungunya were the most common diseases cited as occurring after natural disasters, with general nuisance biting being the most common issue overall")
               ),
               p("What does this mean?"),
               tags$ul(
                 tags$li("Natural disasters which introduce large amounts of water into new areas lead to mosquito growth"),
                 tags$li("Aedes family of mosquitoes, which happen to be the vectors of the most common diseases, tend to have population growth after natural disasters"),
                 tags$li("Broadly, a lack of specificity when reporting on these issues could lead to reduced ability to stop the vectors and their diseases, with Unknown and Nuisance being the two most common answers by far for Vector Family and Disease Transmitted"),
                 tags$li("Policymakers need to incorporate vector control, specifically for Aedes mosquitoes, as well as disease surveillance into their disaster preparedness plans to avoid outbreaks")
               )
        )),
      
      hr(),
      fluidRow(
        column(6, plotOutput("heatmap_disease", height = 600)),
        column(6, plotOutput("heatmap_species", height = 600))
      )
    ),
    
    tabPanel(
      "Site-Specific Analysis",
      titlePanel(
        HTML("<div style='text-align: center; font-size: 1.25em; font-weight: bold; '>Site-Specific Analysis</div>")
             ),
      
      fluidRow(
        style = "margin-top: 20px; padding: 0 15px;",
        
        column(
          width = 2,
          style = "background-color: #f9f9f9; border-radius: 8px; padding: 15px; border: 1px solid #ddd;",
          h4("Options", style = "color: #2c3e50; font-weight: bold;"),
          selectInput("foc_country", "Select Country",
                      choices = NULL),
          radioButtons(
            inputId = "foc_var",
            label = "Color Map By:",
            choices = c(
              "Event Type" = "event",
              "Disease Reported" = "primary_disease",
              "Vector Species" = "vector_family"
            ),
            selected = "event"
          ),
          hr(),
          uiOutput("foc_stats")
        ),
        
        column(
          width = 5,
          style = "padding: 0 10px;",
          h4("Event Locations", style = "color: #2c3e50; font-weight: bold; text-align: center;"),
          leafletOutput("foc_map", height = 550)
        ),
        
        column(
          width = 5,
          style = "padding: 0 10px;",
          h4("Occurrences Over Time", style = "color: #2c3e50; font-weight: bold; text-align: center;"),
          plotOutput("foc_chart", height = 550)
        )
      )
    ),
    
    tabPanel("About",
             titlePanel(
               HTML("<div style='text-align: center; font-size: 1.25em; font-weight: bold; '>Welcome to Your Global Dashboard of Natural Disasters and Disease Vectors</div>")
             ),
             
             sidebarLayout(
               sidebarPanel(
                 div(
                   style = "text-align: center;",
                   img(src = "www/logo_pic.png", height = 125, width = 275),
                 ),
                 h3("About Us", class = "sidebar-header"),
                 p("The Quantitative Disease Ecology and Conservation Lab group led by Dr. Sadie Ryan
                   at the University of Florida is an interdisciplinary team of geographers, public health scientists, 
                   and disease ecologists using innovative geospatial modeling techniques to providing communities 
                   with the information they need to combat diseases transmitted by mosquitoes and ticks. One of the main
                   objectives of this research team is to analyze the effects of climate conditions on the survival likelihood of 
                   insects capable of transmitting disease, known as 'disease vectors' in different habitats.
                   This dashboard is designed to communicate these scientific concepts in a way that is understandable and
                   interactive, which aligns closely with QDEC's goals to be kind and do good science. Thank you for taking
                   the time to use this dashboard."
                 ),
                 p("Learn more about the QDEC lab here:",
                   tags$a(href = "https://qdec.geog.ufl.edu/", "https://qdec.geog.ufl.edu/", target = "_blank")),
                 hr(),
                 h3("About the Dashboard", class = "sidebar-header"),
                 p("The idea for this dashboard came from the lack of any singular source on global events of natural disasters followed by increased burden of biting insects 
                   and the diseases that they cause. While it is common knowledge that these two processes are closely related, the lack of available data showing this connection 
                   prompted us to conduct a review of all mentions of vectorborne disease and insect population booms following natural disasters. The goal is to show that there 
                   have been repeated occurrences, with increasing frequency in recent decades due to climate change, of this process along with highlighting the systemic issues 
                   contributing to increased disease and general vector burden."
                 ),
                 hr(),
                 h3("Note From the Creators", class = "sidebar-header"),
                 p("While the data collection process for this dashboard attempted to be as comprehensive as possible (more details provided below) there are undoubtedly instances of natural disasters 
               impacting vector populations that were not accounted for in this dashboard due to the sheer volume and frequency of occurrences. However, we went to great lengths to search for occurrences 
               in every country in order to provide an overview of global patterns at play. It is our goal to update this dashboard as future events occur to continue strengthening the results and 
               providing people with the information that they need to understand this issue."),
                 h3("About the Data", class = "sidebar-header"),
                 p("How was it collected?"),
                 tags$ul(
                   tags$li("Countries for analysis were gathered from the official World Health Organization designated list"),
                   tags$li("For every country, an internet search was conducted using keywords such as “floods” and “mosquitoes”, and depending on the region “hurricanes”, “typhoons”, “monsoons” 
                                 or just generally “natural disasters” with the word “mosquitoes” or “insects” to find mentions of insect populations growing and diseases spreading after natural disasters")
                 ),
                 p("Gaps in the Data"),
                 tags$ul(
                   tags$li("The two primary gaps in the data pertained to the diseases reported or of concern as well as the vector species reported, as many places reported upticks in insect populations 
                                 but did not say that they caused a specific disease or mention the actual name of the species"),
                   tags$li("For events with no disease being mentioned in the source, they were categorized as “Nuisance” in the display because while they may not have been documented as causing a disease 
                                 outbreak they were consistently reported as biting people, therefore making them a nuisance"),
                   tags$li("For events with no specific insect vector being mentioned in the source, they were categorized as “Unknown mosquitoes” since the articles would frequently simply say “mosquitoes” 
                                 or “disease-transmitting mosquitoes” which clearly shows the connection between disasters and vectors but just doesn’t say a certain species")
                 )
               ),
               
               mainPanel(
                 h1("How Do Natural Disasters Affect Vector Populations?"),
                 p("What is a vector?"),
                 tags$ul(
                   tags$li("A disease vector is a living organism, usually one which feeds on animal and human blood, that transmits infections between humans and animals"),
                   tags$li("Examples include:",
                           tags$ul(
                             tags$li("Mosquitoes"),
                             tags$li("Ticks"),
                             tags$li("Fleas"),
                             tags$li("Biting flies (sandflies and deerflies)")
                           )),
                 ),
                 div(
                   style = "display: flex; justify-content: center; gap: 20px",
                   img(src = "www/mosquito_pic.png", height = 150, width = 225),
                   img(src = "www/blackfly_pic.png", height = 150, width = 225),
                 ),
                 div(
                   style = "display: flex; justify-content: center; gap: 20px",
                   img(src = "www/sandfly_pic.png", height = 150, width = 225),
                   img(src = "www/tick_pic.png", height = 150, width = 225),
                 ),
                 p("What is a natural disaster?"),
                 tags$ul(
                   tags$li("Major negative event in a vulnerable community that is caused by the impacts of a natural hazard and which typically involves human injury, death, or damage to property"),
                   tags$li("Examples include:",
                           tags$ul(
                             tags$li("Tropical Cyclones",
                                     tags$ul(
                                       tags$li("Known by different names in different parts of the world (Hurricanes, Tropical Storms, Typhoons, Cyclones)"),
                                       tags$li("Rotating storm system with strong winds and heavy rain")
                                     )),
                             tags$li("Tornadoes",
                                     tags$ul(
                                       tags$li("Rotating, short-lived storm system with extremely strong winds that extend from a thunder cloud to the ground as a funnel")
                                     )),
                             tags$li("Floods",
                                     tags$ul(
                                       tags$li("Rapid, unexpected, large amounts of water arriving at a place where they are not usually"),
                                       tags$li("Can occur from extreme rainfall, snowmelt, rivers and lakes overflowing, storm surge from tropical storms"),
                                       tags$li("Can cause landslides and mudslides which move a large amount of dirt and water")
                                     )),
                             tags$li("Earthquakes",
                                     tags$ul(
                                       tags$li("Shaking of the Earth’s surface due to collisions between the tectonic plates underlying the surface of the world")
                                     )),
                             tags$li("Wildfires",
                                     tags$ul(
                                       tags$li("Large, uncontrolled fires that burn in dry areas with plenty of vegetation and can spread to human settlements if not contained")
                                     )),
                           )
                   )
                 ),
                 div(
                   style = "display: flex; justify-content: center; gap: 20px",
                   img(src = "www/flood_pic.png", height = 150, width = 225),
                   img(src = "www/hurricane_pic.png", height = 150, width = 225),
                 ),
                 div(
                   style = "display: flex; justify-content: center; gap: 20px",
                   img(src = "www/quake_pic.png", height = 150, width = 225),
                   img(src = "www/damage_pic.png", height = 150, width = 225),
                 ),
                 p("What is climate change?"),
                 tags$ul(
                   tags$li("The ongoing process of changing of environmental conditions including temperature and precipitation patterns in different areas around the world"),
                   tags$li("Human activity, mainly contribution of “greenhouse gas” emissions like carbon dioxide, have accelerated the amount of climate change occurring"),
                   tags$li("Impacts different places in different ways, but generally makes places hotter and either extremely wet or extremely dry")
                 ),
                 p("How is climate change related to natural disasters and vectors?"),
                 tags$ul(
                   tags$li("Climate change is directly related to so many natural disasters because as temperatures rise, the frequency and severity of many disasters like floods, 
                           tropical storms, droughts, wildfires, and even seasonal weather patterns increase significantly"),
                   tags$li("With more frequent and severe natural disasters, there is a growing potential for insects that transmit disease to reproduce and spread to new areas that they have not previously lived in"),
                   tags$li("This exposes an increasingly larger amount of people to the diseases that these insects carry")
                 )
               )
             ),
             hr(),
             div(
               style = "text-align: center;",
               img(src = "www/systems_pic.png", height = 1000, width = 1375),
             )
    ),
    
    tabPanel("User Guide",
             titlePanel(
               HTML("<div style='text-align: center; font-size: 1.25em; font-weight: bold; '>Dashboard User Guide</div>")
             ),
             
                 h3("Options Overview", class = "sidebar-header"),
                 p("This dashboard has dropdown menus that allow you to select and zoom in on different areas around the world for easier navigation depending on what your area of interest is. You can also 
               freely zoom wherever you want in the map, but this tool is to aid with slightly faster navigation. This dashboard also has buttons for selecting what variables you want to view in the map display."),
                 h3("Customizable Options", class = "sidebar-header"),
                 p("Geographic Area"),
                 tags$ul(
                   tags$li("Region:",
                           tags$ul(
                             tags$li("This level is by continent, with all of the options (North America, South America, Africa, Asia, Europe, and Oceania) zooming in on different continents around the world for a slightly more concentrated view of the events."),
                             tags$li("Disclaimer: for continents with countries that have overseas dependencies, such as many European countries, there will not be a significant difference from the world view and the continent view.")
                           )),
                   tags$li("Country:",
                           tags$ul(
                             tags$li("This level is by country, with all of the options zooming in on different countries around the world for a more specific view of the events.")
                           )),
                 ),
                 p("Total Observations"),
                 tags$ul(
                   tags$li("Selecting this button will populate a map view that has each of the world’s countries color coded based on the number of observations found online that associate natural disasters with changes in insect population and disease outbreak"),
                   tags$li("You can then mouse over the map and as you go over each country, a label will appear that will tell you the number of observations seen in that country."),
                   div(
                     style = "text-align: center;",
                     img(src = "www/areaselect_pic.png", height = 375, width = 750),
                   ),
                 ),
                 hr(),
                 p("Event Type"),
                 tags$ul(
                   tags$li("Selecting this button will populate a map view that has points representing every location mentioned in any of the articles color coded by the type of natural event that occurred there"),
                   tags$li("By hovering over any of the points, all of the general information about the event will populate in a label. By clicking on any of the points, the information will populate below the map along with a link to the original source for more information if you are curious"),
                   tags$li("Note: “Climate” and “Seasonal” are mentioned alongside more acute natural disasters. This was done intentionally to demonstrate the long-term impacts that climate change is having in specific areas that are cited as having insect populations growing and more disease outbreaks because of climate change and severe seasonal weather changes")
                 ),
                 div(
                   style = "text-align: center;",
                   img(src = "www/eventselect_pic.png", height = 375, width = 750),
                 ),
                 p("Disease"),
                 tags$ul(
                   tags$li("Selecting this button will populate a map view that has points representing every location mentioned in any of the articles color coded by the type of disease (if any) that was reported to increase after the event"),
                   tags$li("By hovering over any of the points, all of the general information about the event will populate in a label. By clicking on any of the points, the information will populate below the map along with a link to the original source for more information if you are curious"),
                   tags$li("Note: each point on the map is color coded only by one disease, but many of them have multiple diseases reported as increasing following natural disasters. For each article, the primary disease reported or main disease of concern was reported as the disease used for the point color to reduce the number of diseases displayed in the legend. However, all diseases of concern and those reported are included in the hover label and description")
                 ),
                 div(
                   style = "text-align: center;",
                   img(src = "www/diseaseselect_pic.png", height = 375, width = 750),
                 ),
                 p("Vector Species"),
                 tags$ul(
                   tags$li("Selecting this button will populate a map view that has points representing every location mentioned in any of the articles color coded by the species of insect (if any) that was reported to increase after the event"),
                   tags$li("By hovering over any of the points, all of the general information about the event will populate in a label. By clicking on any of the points, the information will populate below the map along with a link to the original source for more information if you are curious"),
                   tags$li("Note: as with the disease display, each point on the map is color coded only by one family of insect vector to consolidate the information for the sake of displaying in the map. This was decided either by finding the vector that was mentioned most throughout an article or by selecting a vector that had not been seen frequently throughout the rest of the sources (such as Blackflies and Sandflies) to provide more diversity in the data rather than reporting solely mosquitoes for all points")
                 ),
                 div(
                   style = "text-align: center;",
                   img(src = "www/vectorselect_pic.png", height = 375, width = 750),
                 ),
               )
    
    
)

server <- function(input, output, session) {
  
  observe({
    reg_choices <- sort(unique(locations$continent[!is.na(locations$continent)]))
    updateSelectInput(session, "region_select",
                      choices = c("Jump to region..." = "", reg_choices))
  })
  
  observeEvent(input$region_select, {
    if(input$region_select == "") {
      df_c <- locations
    } else {
      df_c <- locations %>% filter(continent == input$region_select)
    }
    
    c_choices <- sort(unique(df_c$country[!is.na(df_c$country)]))
    updateSelectInput(session, "country_select",
                      choices = c("Jump to country..." = "", c_choices))
  })

  marker_colors <- reactive({
    color_by <- input$color_by
    
    n <- nrow(plot_data)
    
    if (n == 0) return(character(0))
    
    if (is.null(color_by) || color_by == "none") {
      return(rep("blue", n))
    }
    
    col <- case_when(
      color_by == "species" ~ "vector_family",
      color_by == "disease" ~ "primary_disease",
      TRUE ~ color_by
    )
    
    values <- plot_data[[col]]
    
    values[is.na(values) | trimws(values) == ""] <- "Unknown"
    
    family_colors <- c(
      "Aedes" = "#0D0887FF",
      "Anopheles" = "#4C02A1FF",
      "Culex" = "#A92395FF",
      "Culicoides" = "#CC4678FF",
      "Culiseta" = "#D59CFC",
      "Mansonia" = "#f89441",
      "Ochlerotatus" = "#E56B5DFF",
      "Psorophora" = "#fdc328",
      "Blackflies" = "#7E03A8FF",
      "Sandflies" = "#f0f921",
      "Unknown" = "#999999"
    )
    
    if (color_by == "species") {
      palette <- colorFactor(
        palette = family_colors,
        domain = names(family_colors)
      ) 
    } else {
      unique_vals <- unique(values)
      palette <- colorFactor(
                               palette = plasma(length(unique_vals)),
                               domain = unique_vals)
    }
    
    # palette <- colorFactor(
    #   palette = rainbow(length(unique(values))),
    #   domain = unique(values)
    # )
    
    palette(values)
  })
  
  nrow(plot_data)
  head(plot_data)
  
  # output$map <- renderLeaflet({
  #   
  #   leaflet() %>% 
  #     addTiles() %>% 
  #     setView(lng = 0, lat = 30, zoom = 1.75) %>% 
  #     addCircleMarkers(data = plot_data,
  #                     lng = ~long_j,
  #                     lat = ~lat_j,
  #                     radius = 4,
  #                     color = "blue",
  #                     fillOpacity = 0.8,
  #                     label = lapply(paste0(
  #                     "<b>Location:</b> ", plot_data$location, ", ", plot_data$country, "<br>",
  #                     "<b>Year:</b> ", plot_data$year, "<br>",
  #                     "<b>Event:</b> ", plot_data$event, "<br>",
  #                     "<b>Reported Disease:</b> ", plot_data$disease, "<br>",
  #                     "<b>Disease of Concern:</b> ", plot_data$concern, "<br>",
  #                     "<b>Species:</b> ", plot_data$species),
  #                     HTML),
  #                     layerId = ~paste0(lat_j, "_", long_j))
  # })
  
  output$map <- renderLeaflet({
    
    country_counts <- plot_data %>% 
    count(country) %>% 
      rename(n_events = n)
    
    world_counts <- world %>% 
      left_join(country_counts, by = c("geounit" = "country")) %>% 
      mutate(n_events = replace_na(n_events, 0))
    
    pal <- colorNumeric(
      palette = plasma(100),
      domain = world_counts$n_events,
      na.color = "#999999"
    )
    
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = 0, lat = 30, zoom = 1.75) %>% 
      addPolygons(
        data = world_counts,
        fillColor = ~pal(n_events),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = lapply(paste0(
          "<b>", world_counts$geounit, "</b><br>",
          "Observations: ", world_counts$n_events),
          HTML)
        ) %>% 
      addLegend(
        position = "bottomright",
        pal = pal,
        values = world_counts$n_events,
        title = "# of Observations",
        opacity = 1
      )
  })
  
  observe({
    colors <- marker_colors()
    color_by <- input$color_by
    
    proxy <- leafletProxy("map")
    proxy %>% clearMarkers() %>% clearControls()
    
    if (color_by == "none") {
      proxy %>% clearShapes()
      
      country_counts <- plot_data %>% 
      count(country) %>% 
        rename(n_events = n)
      
      world_counts <- world %>% 
        left_join(country_counts, by = c("geounit" = "country")) %>% 
        mutate(n_events = replace_na(n_events, 0))
      
      pal <- colorNumeric(
        palette = plasma(100),
        domain = world_counts$n_events,
        na.color = "#999999"
      )
      
      proxy %>% 
        addPolygons(
          data = world_counts,
          fillColor = ~pal(n_events),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          label = lapply(paste0(
            "<b>", world_counts$geounit, "</b><br>",
            "Observations: ", world_counts$n_events),
            HTML)
        ) %>% 
        addLegend(
          position = "bottomright",
          pal = pal,
          values = world_counts$n_events,
          title = "# of Observations",
          opacity = 1
        ) 
        
    } else {
      proxy %>% clearShapes()
      
      proxy %>% 
        addPolygons(
          data = world,
          color = "white",
          weight = 1,
          fillOpacity = 0.1
        )
    
      selected <- input$country_select
      if (!is.null(selected) && nchar(trimws(selected)) > 0) {
        highlighted <- world |> 
          filter(geounit == selected) |> 
          st_transform(4326)
        
        if (nrow(highlighted) > 0) {
          leafletProxy("map") |> 
            addPolygons(
              data = highlighted,
              layerId = "country_highlight",
              color = "black",
              weight = 4,
              fillOpacity = 0,
              options = pathOptions(interactive = FALSE)
            )
        }
      }
      
    proxy %>% 
      addCircleMarkers(data = plot_data,
                       lng = ~long_j,
                       lat = ~lat_j,
                       radius = 4,
                       color = "black",
                       weight = 1,
                       fillColor = colors,
                       fillOpacity = 0.8,
                       stroke = TRUE,
                       label = lapply(paste0(
                         "<b>Location:</b> ", plot_data$location, ", ", plot_data$country, "<br>",
                         "<b>Year:</b> ", plot_data$year, "<br>",
                         "<b>Event:</b> ", plot_data$event, "<br>",
                         "<b>Reported Disease:</b> ", plot_data$disease, "<br>",
                         "<b>Disease of Concern:</b> ", plot_data$concern, "<br>",
                         "<b>Species:</b> ", plot_data$species),
                         HTML),
                       layerId = ~paste0(lat_j, "_", long_j))
    
    # if (!is.null(color_by) && color_by != "none") {
      
    family_colors <- c(
      "Aedes" = "#0D0887FF",
      "Anopheles" = "#4C02A1FF",
      "Culex" = "#A92395FF",
      "Culicoides" = "#CC4678FF",
      "Culiseta" = "#D59CFC",
      "Mansonia" = "#f89441",
      "Ochlerotatus" = "#E56B5DFF",
      "Psorophora" = "#fdc328",
      "Blackflies" = "#7E03A8FF",
      "Sandflies" = "#f0f921",
      "Unknown" = "#999999"
    )
      
      col <- case_when(
        color_by == "species" ~ "vector_family",
        color_by == "disease" ~ "primary_disease",
        TRUE ~ color_by
      )
      values <- plot_data[[col]]
      values[is.na(values) | trimws(values) == ""] <- "Unknown"
      
      # palette <- colorFactor(
      #   palette = rainbow(length(unique(values))),
      #   domain = unique(values)
      # )
      
      if (color_by == "species") {
        palette <- colorFactor(
          palette = family_colors,
          domain = names(family_colors)
        ) 
      } else {
        unique_vals <- unique(values)
        palette <- colorFactor(
          palette = plasma(length(unique_vals)),
          domain = unique_vals
        )
      }
      
      proxy %>% 
        addLegend(
          position = "bottomright",
          pal = palette,
          values = values,
          title = switch(color_by,
                         "event" = "Event Type",
                         "disease" = "Disease",
                         "species" = "Vector Family"),
          opacity = 1
        )
    }
  })
  
  family_colors <- c(
    "Aedes" = "#0D0887FF",
    "Anopheles" = "#4C02A1FF",
    "Culex" = "#A92395FF",
    "Culicoides" = "#CC4678FF",
    "Culiseta" = "#D59CFC",
    "Mansonia" = "#f89441",
    "Ochlerotatus" = "#E56B5DFF",
    "Psorophora" = "#fdc328",
    "Blackflies" = "#7E03A8FF",
    "Sandflies" = "#f0f921",
    "Unknown" = "#999999"
  )
  
  output$heatmap_disease <- renderPlot({
    heat_data <- plot_data %>% 
      filter(!is.na(event), !is.na(vector_family),
             nchar(trimws(event)) > 0) %>% 
      count(event, vector_family) %>% 
      complete(event, vector_family, fill = list(n = 0))
    
    ggplot(heat_data, aes(x = vector_family, y = event, fill = n)) +
      geom_tile(color = "lightgray") +
      geom_text(aes(label = ifelse(n == 0, "", n)), color = "gray", size = 10) +
      scale_fill_gradientn(
        colors = c("gray80", plasma(100)), 
        values = scales::rescale(c(0, 0.001, 1)),
        name = "Count") +
      labs(
        title = "Natural Events vs. Vector Family",
        x = "Vector Family",
        y = "Event Type"
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid = element_blank()
      )
  })
  
  output$heatmap_species <- renderPlot({
    heat_data <- plot_data %>% 
      filter(!is.na(event), !is.na(primary_disease),
             nchar(trimws(event)) > 0,
             nchar(trimws(primary_disease)) > 0) %>% 
      count(event, primary_disease) %>% 
      complete(event, primary_disease, fill = list(n = 0))
    
    ggplot(heat_data, aes(x = primary_disease, y = event, fill = n)) +
      geom_tile(color = "lightgray") +
      geom_text(aes(label = ifelse(n == 0, "", n)), color = "gray", size = 10) +
      scale_fill_gradientn(
        colors = c("gray80", plasma(100)), 
        values = scales::rescale(c(0, 0.001, 1)),
        name = "Count") +
      labs(
        title = "Natural Events vs. Disease",
        x = "Disease",
        y = "Event Type"
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid = element_blank()
      )
  })
  
  # Reactive text box below selection panel in charts and graphs page
  output$total_stats <- renderUI({
    var <- input$treemap_var
    
    label <- switch(var,
                    "event" = "Event Type",
                    "primary_disease" = "Disease Reported",
                    "vector_family" = "Vector Species")
    
    counts <- plot_data |> 
      expand_var(var) |> 
      dplyr::count(selected_var) |> 
      arrange(desc(n))
    
    total <- sum(counts$n)
    
    rows <- lapply(seq_len(nrow(counts)), function(i) {
      pct <- round(100 * counts$n[i] / total, 1)
      tags$tr(
        tags$td(style = "font-weight: bold; padding: 4px;", counts$selected_var[i]),
        tags$td(style = "padding: 4px;", paste0(counts$n[i], " / ", total, " (", pct, "%)"))
      )
    }
      )
    
    div(
      style = "margin-top: 10px;",
      h4(paste(label, "Summary"), style = "color: #2c3e50; font-weight: bold;"),
      tags$table(style = "width: 100%; font-size: 14px;", do.call(tagList, rows))
    )
  })
  
  # Stacked bar chart
  output$chart_year <- renderPlot({
    var <- input$treemap_var
    
    label <- switch(var,
                    "event" = "Event Type",
                    "primary_disease" = "Disease Reported",
                    "vector_family" = "Vector Species")
    
    chart_data <- plot_data %>%
      dplyr::rename(selected_var = all_of(var)) %>%
      mutate(selected_var = ifelse(
        is.na(selected_var) | trimws (as.character(selected_var)) == "",
        "Unknown", as.character(selected_var)
      )) %>%
      filter(!is.na(year)) %>%
      dplyr::count(year, selected_var)
    
    unique_vals <- unique(chart_data$selected_var)
    print(paste("unique vals:", length(unique_vals)))
    
    if (var == "vector_family") {
      fill_scale <- scale_fill_manual(values = family_colors, name = label)
    } else {
      fill_scale <- scale_fill_manual(values = plasma(length(unique_vals)), name = label)
    }
    
    ggplot(chart_data, aes(x = year, y = n, fill = selected_var)) +
      geom_bar(stat = "identity", color = "white", linewidth = 0.2) +
      scale_fill_manual(values = plasma(length(unique_vals)), name = label) +
      scale_x_continuous(
        breaks = function(x) seq(floor(min(x, na.rm = TRUE)), 
                                 ceiling(max(x, na.rm = TRUE)), by = 1)
      ) +
      labs(
        title = paste("Occurrences Per Year by", label),
        x = "Year",
        y = "Number of Occurrences"
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size = 10)
        # panel.grid = element_blank()
      )
})
  
  # Treemap code
  output$tree_var <- renderPlot({
    var <- input$treemap_var
    label <- switch(var,
                    "event" = "Event Type",
                    "primary_disease" = "Disease Reported",
                    "vector_family" = "Vector Species")
    
    counts <- plot_data %>% 
      expand_var(var) %>% 
      dplyr::count(selected_var) %>% 
      arrange(desc(n))
    
    if (var == "vector_family") {
      palette <- unname(family_colors[counts$selected_var])
    } else {
      palette <- plasma(nrow(counts))
    }
    
    treemap(counts,
            index = "selected_var",
            vSize = "n",
            type = "index",
            vColor = "selected_var",
            palette = plasma(nrow(counts)),
            title = paste("Treemap -", label),
            fontsize.labels = 18,
            fontsize.title = 20,
            legend = FALSE)
  })
  
  output$pie_var <- renderPlot({
    var <- input$treemap_var
    label <- switch(var,
                    "event" = "Event Type",
                    "primary_disease" = "Disease Reported",
                    "vector_family" = "Vector Species")
    counts <- plot_data %>% 
      expand_var(var) %>% 
      dplyr::count(selected_var) %>% 
      arrange(desc(n))
    
    if (var == "vector_family") {
      fill_scale <- scale_fill_manual(values = family_colors, name = label)
    } else {
      fill_scale <- scale_fill_manual(values = plasma(nrow(counts)), name = label)
    }
    
    ggplot(counts, aes(x = "", y = n, fill = selected_var)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = plasma(nrow(counts)), name = label) +
      labs(title = paste("Pie Chart -", label)) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "right",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size = 10)
      )
  })
  
  # Manual bounding box overrides for countries/territories with large geometries
  country_bbox_overrides <- list(
    "United States of America" = list(lng1 = -125, lat1 = 24, lng2 = -66, lat2 = 50, zoom = 4),
    "France" = list(lng1 = -5, lat1 = 41, lng2 = 10, lat2 = 51, zoom = 5),
    "Norway" = list(lng1 = 4, lat1 = 57, lng2 = 31, lat2 = 71, zoom = 5),
    "Portugal" = list(lng1 = -10, lat1 = 36, lng2 = -6, lat2 = 42, zoom = 6),
    "New Zealand" = list(lng1 = 165, lat1 = -48, lng2 = 178, lat2 = -34, zoom = 5),
    "Australia" = list(lng1 = 105, lat1 = -7, lng2 = 160, lat2 = -45, zoom = 4),
    "Canada" = list(lng1 = -150, lat1 = 40, lng2 = -45, lat2 = 72, zoom = 3),
    "Netherlands" = list(lng1 = 9, lat1 = 54, lng2 = 1, lat2 = 50, zoom = 6),
    "Fiji" = list(lng1 = 175, lat1 = -15, lng2 = 179, lat2 = -20, zoom = 7),
    "Kiribati" = list(lng1 = 179, lat1 = -3, lng2 = 167, lat2 = 4, zoom = 7), 
    "Tuvalu" = list(lng1 = 173, lat1 = -4, lng2 = 179, lat2 = -10, zoom = 7),
    "Mayotte" = list(lng1 = 44, lat1 = -14, lng2 = 46, lat2 = -12, zoom = 9)
  )
  
  region_bbox_overrides <- list(
    "North America" = list(lng1 = -35, lat1 = 0, lng2 = -175, lat2 = 80, zoom = 3),
    "Europe" = list(lng1 = -30, lat1 = 30, lng2 = 50, lat2 = 75, zoom = 3),
    "Oceania" = list(lng1 = 130, lat1 = 15, lng2 = 160, lat2 = -45, zoom = 3)
  )
  
  fly_to_override_or_bbox <- function(map_id, override, bbox) {
    if (!is.null(override)) {
      leafletProxy(map_id) |> 
        flyTo(
          lng = (override$lng1 + override$lng2) / 2,
          lat = (override$lat1 + override$lat2) / 2,
          zoom = override$zoom
        )
    } else {
      leafletProxy(map_id) |> 
        flyToBounds(
          lng1 = as.numeric(bbox["xmin"]),
          lat1 = as.numeric(bbox["ymin"]),
          lng2 = as.numeric(bbox["xmax"]),
          lat2 = as.numeric(bbox["ymax"]),
          options = list(padding = c(50, 50))
        )
    }
  }
  
  # Zoom for region selections
  observeEvent(input$region_select, {
    req(nchar(trimws(input$region_select)) > 0)
    selected_region <- world |> 
      filter(continent == input$region_select) |> 
      st_transform(4326)
    if(nrow(selected_region) > 0) {
      override <- region_bbox_overrides[[input$region_select]]
      bbox <- st_bbox(selected_region)
        fly_to_override_or_bbox("map", override, bbox)
    }
  })
  
  # Zoom for country selections
  observeEvent(input$country_select, {
    req(nchar(trimws(input$country_select)) > 0)
    
    override <- country_bbox_overrides[[input$country_select]]
    
    if (!is.null(override)) {
      leafletProxy("map") |> 
        flyTo(
          lng = (override$lng1 + override$lng2) / 2,
          lat = (override$lat1 + override$lat2) / 2,
          zoom = override$zoom
        )
    } else {
      selected_country <- world |> 
        filter(geounit == input$country_select) |> 
        st_transform(4326)
    
    if(nrow(selected_country) > 0) {
      bbox <- st_bbox(selected_country)
        leafletProxy("map") |> 
          flyToBounds(
            lng1 = as.numeric(bbox["xmin"]),
            lat1 = as.numeric(bbox["ymin"]),
            lng2 = as.numeric(bbox["xmax"]),
            lat2 = as.numeric(bbox["ymax"]),
            options = list(padding = c(50, 50))
          )
    }
    }
    
  })
  
    # # Zoom for region selections
    # observeEvent(input$region_select, {
    # 
    #   req(nchar(trimws(input$region_select)) > 0)
    # 
    #   selected_region <- world %>%
    #     filter(continent == input$region_select) %>%
    #     st_transform(4326)
    # 
    #   if (nrow(selected_region) > 0) {
    #     bbox <- st_bbox(selected_region)
    # 
    #     xmin <- as.numeric(bbox["xmin"])
    #     ymin <- as.numeric(bbox["ymin"])
    #     xmax <- as.numeric(bbox["xmax"])
    #     ymax <- as.numeric(bbox["ymax"])
    # 
    #     leafletProxy("map") %>%
    #       flyToBounds(
    #         # lng1 = bbox["xmin"],
    #         # lat1 = bbox["ymin"],
    #         # lng2 = bbox["xmax"],
    #         # lat2 = bbox["ymax"],
    #         lng1 = xmin,
    #         lat1 = ymin,
    #         lng2 = xmax,
    #         lat2 = ymax,
    #         options = list(padding = c(50, 50))
    #       )
    #   }
    # 
    # })
    # 
    # # Zoom for country selections
    # observeEvent(input$country_select, {
    # 
    #   req(nchar(trimws(input$country_select)) > 0)
    # 
    #   selected_country <- world %>%
    #     filter(geounit == input$country_select) %>%
    #     st_transform(4326)
    # 
    #   if (nrow(selected_country) > 0) {
    #     bbox <- st_bbox(selected_country)
    # 
    #     xmin <- as.numeric(bbox["xmin"])
    #     ymin <- as.numeric(bbox["ymin"])
    #     xmax <- as.numeric(bbox["xmax"])
    #     ymax <- as.numeric(bbox["ymax"])
    # 
    #     leafletProxy("map") %>%
    #       flyToBounds(
    #         # lng1 = bbox["xmin"],
    #         # lat1 = bbox["ymin"],
    #         # lng2 = bbox["xmax"],
    #         # lat2 = bbox["ymax"],
    #         lng1 = xmin,
    #         lat1 = ymin,
    #         lng2 = xmax,
    #         lat2 = ymax,
    #         options = list(padding = c(50, 50))
    #       )
    #   }
    # 
    # })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    print("click detected")
    print(paste("lat:", click$lat, "lng:", click$lng))
    
    
    # Identify the row by matching the lat/long of the clicked marker
    row <- locations %>% 
      filter(abs(lat_j - click$lat) < 0.0001, abs(long_j - click$lng) < 0.0001) %>% 
      slice(1)
    print(paste("rows found:", nrow(row)))
    print(row)
    
    
      output$click_info <- renderUI({
        print("inside renderUI")

        tryCatch({   
        
          safe <- function(x) {
            x <- as.character(x)
            # x <- ifelse(is.na(x) | trimws(x) == "" | x == "NA", "Not reported", x)
            x <- gsub('"', '', x)   # remove embedded quotes
            x <- gsub("'", "", x)   # remove embedded single quotes
            x
          }
        
        source_display <- if (is.na(row$link) || trimws(row$link) == "" || 
                              !grepl("^https?://", trimws(row$link))) {
          tags$td(style = "padding: 5px;", "No source available")
        } else {
          tags$td(style = "padding: 5px;",
                  tags$a(href = trimws(row$link),
                         target = "_blank",
                         "Website Link"))
        }
        
        div(
          style = "padding: 15px; background-color: #f9f9f9; border-radius: 8px; border: 1px solid #ddd;",
          h3(paste0(row$title)),
          tags$table(
            style = "width: 100%; font-size: 16px;",
            tags$tr(
              tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Location:"),
              tags$td(style = "padding: 5px;", row$location, ", ", row$country)
            ),
            tags$tr(
              tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Year:"),
              tags$td(style = "padding: 5px;", row$year)
            ),
            tags$tr(
              tags$td(style = "font-weight: bold; padding: 5px;", "Natural Event:"),
              tags$td(style = "padding: 5px;", row$event)
            ),
            tags$tr(
              tags$td(style = "font-weight: bold; padding: 5px;", "Reported Disease:"),
              tags$td(style = "padding: 5px;", row$disease)
            ),
            tags$tr(
              tags$td(style = "font-weight: bold; padding: 5px;", "Disease of Concern:"),
              tags$td(style = "padding: 5px;", row$concern)
            ),
            tags$tr(
              tags$td(style = "font-weight: bold; padding: 5px;", "Vector Species:"),
              tags$td(style = "padding: 5px;", row$species)
            ),
            tags$tr(
              tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Source:"),
              tags$td(style = "padding: 5px;",
                      tags$a(href = row$link,
                             target = "_blank",
                             "Source Website Link")
              )
            )
          )
        )
          
      }, error = function(e) {
        div(
          style = "padding: 15px; background-color: #fff3cd; border-radius: 8px; border: 1px solid #ffc107;",
          h4("Could not load details for this marker."),
          p(paste("Error:", e$message))
        )
      })
  })
})
  
  observe({
    country_choices <- sort(unique(plot_data$country[!is.na(plot_data$country)]))
    updateSelectInput(session, "foc_country", choices = c("Select a country..." = "", country_choices))
  })
  
  foc_data <- reactive({
    req(input$foc_country)
    plot_data |> filter(country == input$foc_country)
  })
  
  output$foc_map <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  observe({
    req(input$foc_country, nrow(foc_data()) > 0)
    
    data <- foc_data()
    var <- input$foc_var
    
    col <- case_when(
      var == "vector_family" ~ "vector_family",
      var == "primary_disease" ~ "primary_disease",
      TRUE ~ var
    )
    
    values <- data[[col]]
    values[is.na(values) | trimws(values) == ""] <- "Unknown"
    
    if (var == "vector_family") {
      palette <- colorFactor(palette = family_colors, domain = names(family_colors))
    } else {
      unique_vals <- unique(values)
      palette <- colorFactor(palette = plasma(length(unique_vals)), domain = unique_vals)
    }
    
    colors <- palette(values)
    
    override <- country_bbox_overrides[[input$foc_country]]
    
    if (!is.null(override)) {
      lng_center <- (override$lng1 + override$lng2) / 2
      lat_center <- (override$lat1 + override$lat2) / 2
      zoom <- override$zoom
    } else {
      selected_country <- world |> 
        filter(geounit == input$foc_country) |> 
        st_transform(4326)
      
      if (nrow(selected_country) > 0) {
        bbox <- st_bbox(selected_country)
        lng_center <- mean(c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"])))
        lat_center <- mean(c(as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"])))
        zoom <- 5
      } else {
        lng_center <- 0
        lat_center <- 20
        zoom <- 2
      }
    }
    
    label <- switch(var,
                    "event" = "Event Type",
                    "primary_disease" = "Disease Reported",
                    "vector_family" = "Vector Species")
    leafletProxy("foc_map") |> 
      clearMarkers() |> 
      clearControls() |> 
      clearShapes() |> 
      setView(lng = lng_center, lat = lat_center, zoom = zoom) |> 
      addPolygons(
        data = world |> filter(geounit == input$foc_country) |> st_transform(4326),
        color = "black",
        weight = 2,
        fillOpacity = 0.05,
        options = pathOptions(interactive = FALSE)
      ) |> 
      addCircleMarkers(
        data = data,
        lng = ~long_j,
        lat = ~lat_j,
        radius = 5,
        color = "black",
        weight = 1,
        fillColor = colors,
        fillOpacity = 0.9,
        stroke = TRUE,
        label = lapply(paste0(
          "<b>Location:</b> ", data$location, "<br>",
          "<b>Year:</b> ", data$year, "<br>",
          "<b>Event:</b> ", data$event, "<br>",
          "<b>Reported Disease:</b> ", data$disease, "<br>",
          "<b>Species:</b> ", data$species),
          HTML)
      ) |> 
      addLegend(
        position = "bottomright",
        pal = palette,
        values = values,
        title = label,
        opacity = 1
      )
    
  })
  
  output$foc_stats <- renderUI({
    req(input$foc_country, nrow(foc_data()) > 0)
    
    var <- input$foc_var
    
    label <- switch(var,
                    "event" = "Event Type",
                    "primary_disease" = "Disease Reported",
                    "vector_family" = "Vector Species")
    
    counts <- foc_data() |> 
      expand_var(var) |> 
      dplyr::count(selected_var) |> 
      dplyr::arrange(-n)
    
    total <- sum(counts$n)
    
    rows <- lapply(seq_len(nrow(counts)), function(i) {
      pct <- round(100 * counts$n[i] / total, 1)
      tags$tr(
        tags$td(style = "font-weight: bold; padding: 3px; font-size: 12px;", counts$selected_var[i]),
        tags$td(style = "padding: 3px; font-size: 12px;", paste0(counts$n[i], " (", pct, "%)"))
      )
    })
    
    div(
      style = "margin-top: 10px;",
      h4(paste(label, "Breakdown"), style = "color: #2c3e50; font-weight: bold; font-size: 13px;"),
      p(paste("Total Observations: ", total), style = "font-size: 12px; color: #555;"),
      tags$table(style = "width: 100%;", do.call(tagList, rows))
    )
    
  })
  
  output$foc_chart <- renderPlot({
    req(input$foc_country, nrow(foc_data()) > 0)
    
    var <- input$foc_var
    
    label <- switch(var,
                    "event" = "Event Type",
                    "primary_disease" = "Disease Reported",
                    "vector_family" = "Vector Species")
    
    chart_data <- foc_data() |> 
      dplyr::rename(selected_var = all_of(var)) |> 
      mutate(selected_var = ifelse(
        is.na(selected_var) | trimws(as.character(selected_var)) == "",
        "Unknown", as.character(selected_var)
      )) |> 
      filter(!is.na(year)) |> 
      dplyr::count(year, selected_var)
    
    unique_vals <- unique(chart_data$selected_var)
    
    if(var == "vector_family") {
      fill_scale <- scale_fill_manual(values = family_colors, name = label)
    } else {
      fill_scale <- scale_fill_manual(values = plasma(length(unique_vals)), name = label)
    }
    
    ggplot(chart_data, aes(x = year, y= n, fill = selected_var)) +
      geom_bar(stat = "identity", color = "white", linewidth = 0.2, width = 0.6) +
      fill_scale +
      scale_x_continuous(
        breaks = function(x) seq(floor(min(x, na.rm = TRUE)),
                                 ceiling(max(x, na.rm = TRUE)), by = 1)
      ) +
      scale_y_continuous(
        breaks = function(x) seq(0, floor(max(x, na.rm = TRUE)), by = 1),
        labels = scales::label_number(accuracy = 1)
      ) +
      labs(
        title = paste(input$foc_country, "-", label),
        x = "Year",
        y = "Number of Occurrences"
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "right",
        legend.text = element_text(size = 8),
        legend.title = element_text(face = "bold", size = 9)
      )
  })
  
}

shinyApp(ui, server)


#   tabPanel("Reported Events Map",
#       
#       titlePanel("Selection Panel"),
#       
#       sidebarLayout(
#         sidebarPanel(
#           
#           # Region selection panel
#           selectInput(
#             inputId = "region_select",
#             label = "Select a Region",
#             choices = NULL,
#             selected = NULL
#           ),
#           
#           # Country selection panel
#           selectInput(
#             inputId = "country_select",
#             label = "Select a Country",
#             choices = NULL,
#             selected = NULL
#           ),
#           
#           # City selection panel
#           conditionalPanel(
#             condition = "input.country_select != ''",
#             selectInput(
#               inputId = "city_select",
#               label = "Select a City",
#               choices = NULL,
#               selected = NULL
#             )
#           ),
#           
#           # Date range selection panel
#           selectInput(
#             inputId = "date_select",
#             label = "Select a Date Range",
#             choices = c(
#               "All Decades" = "",
#               "1990-2000" = "1990",
#               "2000-2010" = "2000",
#               "2010-2020" = "2010",
#               "2020-Present" = "2020"
#             ),
#             selected = ""
#           ),
#           
#           hr(),
#           
#           # Event type selection
#           selectInput(
#             inputId = "event_select",
#             label = "Select a Natural Event",
#             choices = NULL,
#             selected = NULL
#           ),
#           
#           # Disease selection
#           selectInput(
#             inputId = "disease_select",
#             label = "Select a Disease",
#             choices = NULL,
#             selected = NULL
#           ),
#           
#           # Mosquito selection
#           selectInput(
#             inputId = "species_select",
#             label = "Select a Vector Species",
#             choices = c(
#               "Select a species..." = "")),
#           
#           actionButton("reset_btn", "Reset View")
#           
#         ),
#         
#         # Single comparisons map
#         mainPanel(
#           leafletOutput("map", height = 600),
#           hr(),
#           uiOutput("click_info")
#         )
#       ),
#                  
#   tabPanel("Systems Map"),
#   
#   )               
# )
# 
# server <- function(input, output, session) {
#   
#   # Continent filter
#   # observeEvent(TRUE, once = TRUE, {
#   #   region_choices <- locations %>% 
#   #     filter(!is.na(continent), nchar(trimws(continent)) > 0) %>% 
#   #     pull(continent) %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   region_choices <- c("Select a Region..." = "", region_choices)
#   #   
#   #   updateSelectInput(
#   #     session,
#   #     "region_select",
#   #     choices = region_choices
#   #   )
#   # })
#   # 
#   # # Country filter
#   # observeEvent(TRUE, once = TRUE, {
#   #   country_choices <- locations %>% 
#   #   filter(!is.na(country), nchar(trimws(country)) > 0) %>% 
#   #     pull(country) %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   country_choices <- c("Select a Country..." = "", country_choices)
#   #   
#   #   updateSelectInput(
#   #     session,
#   #     "country_select",
#   #     choices = country_choices
#   #   )
#   # })
#   
#   observe({
#     region_choices <- locations %>% 
#       filter(!is.na(continent), nchar(trimws(continent)) > 0) %>% 
#       pull(continent) %>% 
#       unique() %>% 
#       sort()
#     
#     updateSelectInput(
#       session,
#       "region_select",
#       choices = c("Select a Region..." = "", region_choices)
#     )
#   })
#   
#   observeEvent(input$region_select, {
#     # req(input$region_select != "")
#     freezeReactiveValue(input, "country_select")
#     freezeReactiveValue(input, "city_select")
#     updateSelectInput(session, "country_select", selected = "")
#     updateSelectInput(session, "city_select", selected = "")
#   }, ignoreInit = TRUE)
#   # 
#   observeEvent(input$country_select, {
#     # req(input$country_select != "")
#     # match_continent <- locations %>%
#     #   filter(country == input$country_select) %>%
#     #   pull(continent) %>%
#     #   first()
# # 
#     updateSelectInput(session, "city_select", selected = "")
#   }, ignoreInit = TRUE)
#   
#   # observeEvent(input$region_select, {
#   #   if (is.null(input$region_select) || input$region_select == "") {
#   #     df_filtered <- locations
#   #   } else {
#   #     df_filtered <- locations %>% filter(continent == input$region_select)
#   #     updateSelectInput(session, "country_select", selected = "")
#   #     updateSelectInput(session, "city_select", selected = "")
#   #   }
#   # 
#   # country_choices <- df_filtered %>%
#   #   filter(!is.na(country), nchar(trimws(country)) > 0) %>%
#   #   pull(country) %>%
#   #   unique() %>%
#   #   sort()
#   # 
#   # updateSelectInput(
#   #   session,
#   #   "country_select",
#   #   choices = c("Select a Country..." = "", country_choices),
#   #   selected = input$country_select
#   # )
#   # })
#   
#   observe({
#     # Determine country list based on region
#     if (is.null(input$region_select) || input$region_select == "") {
#       df_temp <- locations 
#     } else {
#       df_temp <- locations %>% filter(continent == input$region_select)
#     }
#     
#     c_choices <- df_temp %>%
#       filter(!is.na(country), nchar(trimws(country)) > 0) %>%
#       pull(country) %>% unique() %>% sort()
#     
#     updateSelectInput(session, "country_select",
#                       choices = c("Select a Country..." = "", c_choices),
#                       selected = input$country_select)
#   })
#   
#   # observe({
#   #   df_for_countries <- if (is.null(input$region_select) || input$region_select == "") {
#   #     locations
#   # } else {
#   #   locations %>% filter(continent == input$region_select)
#   # }
#   # 
#   # c_choices <- df_for_countries %>% 
#   #   filter(!is.na(country), nchar(trimws(country)) > 0) %>% 
#   #   pull(country) %>% unique() %>% sort()
#   # 
#   # updateSelectInput(session, "country_select",
#   #                   choices = c("Select a Country..." = "", c_choices),
#   #                   selected = input$country_select)
#   # })
#   
#   # City filter
#   # observeEvent(input$country_select, {
#   #   req(input$country_select != "")
#   #   
#   #   city_choices <- locations %>% 
#   #     filter(grepl(input$country_select, country)) %>% 
#   #     pull(location) %>% 
#   #     sort()
#   #   
#   #   # city_choices <- c("Select a City..." = "", city_choices)
#   #   
#   #   updateSelectInput(
#   #     session,
#   #     "city_select",
#   #     choices = c("Select a City..." = "", city_choices))
#   # })
#   
#   # active_selection <- reactiveVal("none")
#   
#   # Reactive filter for determining options in variable categories below
#   # filtered_locations <- reactive({
#   #   result <- locations
#   #   
#   #   if(!is.null(input$region_select) && input$region_select != ""){
#   #     result <- result %>% filter(continent == input$region_select)
#   #   }
#   #   
#   #   if(!is.null(input$country_select) && input$country_select != ""){
#   #     result <- result %>% filter(grepl(input$country_select, country))
#   #   }
#   #   
#   #   if(!is.null(input$date_select) && input$date_select != "") {
#   #     decade_start <- as.numeric(input$date_select)
#   #     decade_end <- if(decade_start == 2020) as.numeric(format(Sys.Date(), "%Y")) else decade_start + 10
#   #     
#   #     result <- result %>% 
#   #       filter(year >= decade_start & year < decade_end)
#   #   }
#   #   
#   #   result
#   # })
#   
#   #ORIGINAL MAP FILTER
#   # map_filter <- reactive({
#   #   
#   #   result <- locations %>% 
#   #     filter(!is.na(lat_j), !is.na(long_j))
#   #   
#   #   if (!is.null(input$region_select) && input$region_select != "") {
#   #     result <- result %>% filter(continent == input$region_select)
#   #   }
#   #   
#   #   if (!is.null(input$country_select) && input$country_select != "") {
#   #     result <- result %>% filter(grepl(input$country_select, country))
#   #   }
#   #   
#   #   if (!is.null(input$city_select) && input$city_select != "") {
#   #     result <- result %>% filter(location == input$city_select)
#   #   }
#   #   
#   #   if (!is.null(input$date_select) && input$date_select != "") {
#   #     decade_start <- as.numeric(input$date_select)
#   #     decade_end <- if (decade_start == 2020) as.numeric(format(Sys.Date(), "%Y")) else decade_start + 10
#   #     result <- result %>% 
#   #       filter(as.numeric(year) >= decade_start & as.numeric(year) < decade_end)
#   #   }
#   #   
#   #   if (!is.null(input$event_select) && input$event_select != "") {
#   #     result <- result %>% filter(event == input$event_select)
#   #   }
#   #   
#   #   if (!is.null(input$disease_select) && input$disease_select != "") {
#   #     result <- result %>% filter(disease == input$disease_select)
#   #   }
#   #   
#   #   if (!is.null(input$species_select) && input$species_select != "") {
#   #     result <- result %>% filter(grepl(input$species_select, species))
#   #   }
#   #   
#   #   result
#   #   
#   # }) 
#   
#   map_filter <- reactive({
#     result <- locations %>% 
#       filter(!is.na(lat_j), !is.na(long_j))
#     
#     if (!is.null(input$region_select) && input$region_select != "") {
#       result <- result %>% filter(continent == input$region_select)
#     }
#     
#     if (!is.null(input$country_select) && input$country_select != "") {
#       result <- result %>% filter(country == input$country_select)
#     }
#     
#     if (!is.null(input$city_select) && input$city_select != "") {
#       result <- result %>% filter(location == input$city_select)
#     }
#     
#     if (!is.null(input$date_select) && input$date_select != "") {
#       decade_start <- as.numeric(input$date_select)
#       decade_end <- if (decade_start == 2020) as.numeric(format(Sys.Date(), "%Y")) else decade_start + 10
#       result <- result %>%
#         filter(as.numeric(year) >= decade_start & as.numeric(year) < decade_end)
#     } 
#     
#     if (!is.null(input$event_select) && input$event_select != "") {
#       result <- result %>% filter(event == input$event_select)
#     }
# 
#     if (!is.null(input$disease_select) && input$disease_select != "") {
#       result <- result %>% filter(disease == input$disease_select)
#     }
# 
#     if (!is.null(input$species_select) && input$species_select != "") {
#       result <- result %>% filter(grepl(input$species_select, species))
#     }
# 
#     return(result)
#   })
#   
#   # Species
#   # observeEvent(map_filter(), {
#   #   species_choices <- map_filter() %>% 
#   #     filter(!is.na(species), nchar(trimws(species)) > 0) %>% 
#   #     pull(species) %>% 
#   #     strsplit(",") %>% 
#   #     unlist() %>% 
#   #     trimws() %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   updateSelectInput(session, "species_select",
#   #                     choices = c("Select a Species..." = "", species_choices),
#   #                     selected = "")
#   # })
#   
#   # FILTER OBSERVE
#   # observe({
#   #   data <- map_filter()
#   # 
#   #   updateSelectInput(session, "event_select",
#   #                     choices = c("All Events" = "", sort(unique(data$event))))
#   #   updateSelectInput(session, "disease_select",
#   #                     choices = c("All Diseases" = "", sort(unique(data$disease))))
#   # 
#   #   spec <- data$species %>% strsplit(",") %>% unlist() %>% trimws() %>% unique() %>% sort()
#   #   updateSelectInput(session, "species_select",
#   #                     choices = c("All Species" = "", spec))
#   # })
# 
#   # Events
#   # observeEvent(map_filter(), {
#   #   event_choices <- map_filter() %>% 
#   #     filter(!is.na(event), nchar(trimws(event)) > 0) %>% 
#   #     pull(event) %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   updateSelectInput(session, "event_select",
#   #                     choices = c("Select an Event..." = "", event_choices),
#   #                     selected = "")
#   #   
#   # })
#   # 
#   # # Diseases
#   # observeEvent(map_filter(), {
#   #   disease_choices <- map_filter() %>% 
#   #     filter(!is.na(disease), nchar(trimws(disease)) > 0) %>% 
#   #     pull(disease) %>% 
#   #     unique() %>% 
#   #     sort()
#   #   
#   #   updateSelectInput(session, "disease_select",
#   #                     choices = c("Select a Disease..." = "", disease_choices),
#   #                     selected = "")
#   #   
#   # })
#   
#   # map_filter <- reactive({
#   #   
#   #   result <- locations %>% 
#   #     filter(!is.na(lat_j), !is.na(long_j))
#   #   
#   #   if (!is.null(input$region_select) && input$region_select != "") {
#   #     result <- result %>% filter(continent == input$region_select)
#   #   }
#   #   
#   #   if (!is.null(input$country_select) && input$country_select != "") {
#   #     result <- result %>% filter(grepl(input$country_select, country))
#   #   }
#   #   
#   #   if (!is.null(input$city_select) && input$city_select != "") {
#   #     result <- result %>% filter(location == input$city_select)
#   #   }
#   #   
#   #   if (!is.null(input$date_select) && input$date_select != "") {
#   #     decade_start <- as.numeric(input$date_select)
#   #     decade_end <- if (decade_start == 2020) as.numeric(format(Sys.Date(), "%Y")) else decade_start + 10
#   #     result <- result %>% 
#   #       filter(as.numeric(year) >= decade_start & as.numeric(year) < decade_end)
#   #   }
#   #   
#   #   if (!is.null(input$event_select) && input$event_select != "") {
#   #     result <- result %>% filter(event == input$event_select)
#   #   }
#   # 
#   #   if (!is.null(input$disease_select) && input$disease_select != "") {
#   #     result <- result %>% filter(disease == input$disease_select)
#   #   }
#   # 
#   #   if (!is.null(input$species_select) && input$species_select != "") {
#   #     result <- result %>% filter(grepl(input$species_select, species))
#   #   }
#   #   
#   #   result
#   #   
#   # }) 
#   
#   
#   # output$map <- renderLeaflet({
#   #   leaflet() %>% 
#   #     addTiles() %>% 
#   #     # addPolygons(data = world,
#   #     #             color = "orange",
#   #     #             weight = 1,
#   #     #             fillOpacity = 0.3) %>% 
#   #     # addCircleMarkers(data = map_filter(),
#   #     #                  lng = ~long_j,
#   #     #                  lat = ~lat_j,
#   #     #                  radius = 4,
#   #     #                  color = "blue",
#   #     #                  fillOpacity = 0.8,
#   #     #                  popup = ~paste(location))
#   #     addCircleMarkers(lng = -80.19, lat = 25.77, popup = "test")
#   # })
#   
#   # output$map <- renderLeaflet({
#   #   data <- map_filter()
#   #   
#   #   leaflet() %>%
#   #     addTiles() %>%
#   #     setView(lng = 0, lat = 30, zoom = 1.75) %>% 
#   #     # addPolygons(data = world,
#   #     #             color = "orange",
#   #     #             weight = 1,
#   #     #             fillOpacity = 0.3) %>%
#   #     addCircleMarkers(data = data,
#   #                      lng = ~long_j,
#   #                      lat = ~lat_j,
#   #                      radius = 4,
#   #                      color = "blue",
#   #                      fillOpacity = 0.8,
#   #                      label = lapply(paste0(
#   #                        "<b>Location:</b> ", data$location, ", ", data$country, "<br>",
#   #                        "<b>Year:</b> ", data$year, "<br>",
#   #                        "<b>Event:</b> ", data$event, "<br>",
#   #                        "<b>Reported Disease:</b> ", data$disease, "<br>",
#   #                        "<b>Disease of Concern:</b> ", data$concern, "<br>",
#   #                        "<b>Species:</b> ", data$species),
#   #                                     HTML))
#   # })
#   
#   # output$map <- renderLeaflet({
#   #   leaflet() %>%
#   #     addTiles() %>%
#   #     setView(lng = 0, lat = 30, zoom = 1.75)
#   # })
#   
#   # map_data <- reactiveVal(locations %>% filter(!is.na(lat_j), !is.na(long_j)))
#   # 
#   # observe({
#   #   map_data(map_filter())
#   # })
#   
#   # observe({
#   #   data <- map_filter()
#   #   
#   #   proxy <- leafletProxy("map") 
#   #   proxy %>% clearMarkers()
#   #     
#   #   if (nrow(data) > 0) {
#   #     proxy %>% 
#   #       addCircleMarkers(data = data,
#   #                        lng = ~long_j,
#   #                        lat = ~lat_j,
#   #                        radius = 4,
#   #                        color = "blue",
#   #                        fillOpacity = 0.8,
#   #                        label = lapply(paste0(
#   #                          "<b>Location:</b> ", data$location, ", ", data$country, "<br>",
#   #                          "<b>Year:</b> ", data$year, "<br>",
#   #                          "<b>Event:</b> ", data$event, "<br>",
#   #                          "<b>Reported Disease:</b> ", data$disease, "<br>",
#   #                          "<b>Disease of Concern:</b> ", data$concern, "<br>",
#   #                          "<b>Species:</b> ", data$species),
#   #                          HTML)
#   #       )
#   #   }  
#   # })
#   
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addTiles() %>%
#       setView(lng = 0, lat = 30, zoom = 1.75)
#   })
#   
#   observe({
#     data <- map_filter()
#     proxy <- leafletProxy("map") 
#     proxy %>% clearMarkers()
#     
#     if (nrow(data) > 0) {
#       proxy %>% 
#       # clearMarkers() %>%
#       addCircleMarkers(
#         data = data,
#         lng = ~long_j, lat = ~lat_j,
#         radius = 4, color = "blue", fillOpacity = 0.8,
#         label = ~paste0(location, ", ", country)
#       )
#     }
#   })
#   
# #   ORIGINAL OBSERVE BLOCK
#   # observe({
#   #   data <- map_filter()
#   #   
#   #   leafletProxy("map") %>%
#   #     clearMarkers() %>%
#   #     addCircleMarkers(data = data,
#   #                      lng = ~long_j,
#   #                      lat = ~lat_j,
#   #                      radius = 4,
#   #                      color = "blue",
#   #                      fillOpacity = 0.8,
#   #                      label = lapply(paste0(
#   #                        "<b>Location:</b> ", data$location, ", ", data$country, "<br>",
#   #                        "<b>Year:</b> ", data$year, "<br>",
#   #                        "<b>Event:</b> ", data$event, "<br>",
#   #                        "<b>Reported Disease:</b> ", data$disease, "<br>",
#   #                        "<b>Disease of Concern:</b> ", data$concern, "<br>",
#   #                        "<b>Species:</b> ", data$species),
#   #                        HTML))
#   # })
#   
#   # Zoom for region selections
#   observeEvent(input$region_select, {
#     
#     req(nchar(trimws(input$region_select)) > 0)
#     
#     selected_region <- world %>% 
#       filter(continent == input$region_select) %>% 
#       st_transform(4326)
#     
#     if (nrow(selected_region) > 0) {
#       bbox <- st_bbox(selected_region)
#       
#       xmin <- as.numeric(bbox["xmin"])
#       ymin <- as.numeric(bbox["ymin"])
#       xmax <- as.numeric(bbox["xmax"])
#       ymax <- as.numeric(bbox["ymax"])
#       
#       leafletProxy("map") %>%
#         flyToBounds(
#           # lng1 = bbox["xmin"],
#           # lat1 = bbox["ymin"],
#           # lng2 = bbox["xmax"],
#           # lat2 = bbox["ymax"],
#           lng1 = xmin,
#           lat1 = ymin,
#           lng2 = xmax,
#           lat2 = ymax,
#           options = list(padding = c(50, 50))
#         )
#     }
#     
#   })
#   
#   # Zoom for country selections
#   observeEvent(input$country_select, {
#     
#     req(nchar(trimws(input$country_select)) > 0)
#     
#     selected_country <- world %>% 
#       filter(geounit == input$country_select) %>% 
#       st_transform(4326)
#     
#     if (nrow(selected_country) > 0) {
#       bbox <- st_bbox(selected_country)
#       
#       xmin <- as.numeric(bbox["xmin"])
#       ymin <- as.numeric(bbox["ymin"])
#       xmax <- as.numeric(bbox["xmax"])
#       ymax <- as.numeric(bbox["ymax"])
#       
#       leafletProxy("map") %>%
#         flyToBounds(
#           # lng1 = bbox["xmin"],
#           # lat1 = bbox["ymin"],
#           # lng2 = bbox["xmax"],
#           # lat2 = bbox["ymax"],
#           lng1 = xmin,
#           lat1 = ymin,
#           lng2 = xmax,
#           lat2 = ymax,
#           options = list(padding = c(50, 50))
#         )
#     }
#     
#   })
#   
#   clicked_info <- reactiveVal(NULL)
#   
#   observeEvent(input$map_marker_click, {
#     click <- input$map_marker_click
#     
#     clicked_row <- map_filter() %>% 
#       filter(abs(lat_j - click$lat) < 0.0001,
#              abs(long_j - click$lng) < 0.0001) %>% 
#       slice(1)
#     
#     clicked_info(clicked_row)
#   })
#   
#   output$click_info <- renderUI({
#     req(clicked_info())
#     row <- clicked_info()
#     
#     div(
#       style = "padding: 15px; background-color: #f9f9f9; border-radius: 8px; border: 1px solid #ddd;",
#       h3(paste0(row$title)),
#       tags$table(
#         style = "width: 100%; font-size: 16px;",
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Location:"),
#           tags$td(style = "padding: 5px;", row$location, ", ", row$country)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Year:"),
#           tags$td(style = "padding: 5px;", row$year)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px;", "Natural Event:"),
#           tags$td(style = "padding: 5px;", row$event)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px;", "Reported Disease:"),
#           tags$td(style = "padding: 5px;", row$disease)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px;", "Disease of Concern:"),
#           tags$td(style = "padding: 5px;", row$concern)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px;", "Vector Species:"),
#           tags$td(style = "padding: 5px;", row$species)
#         ),
#         tags$tr(
#           tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Source:"),
#           tags$td(style = "padding: 5px;",
#                   tags$a(href = row$link,
#                          target = "_blank",
#                          row$link)
#           )
#         ),
#       )
#     )
#   })
#   
#   observeEvent(input$reset_btn, {
#     # 1. Freeze everything so the map doesn't flicker while resetting
#     freezeReactiveValue(input, "region_select")
#     freezeReactiveValue(input, "country_select")
#     freezeReactiveValue(input, "city_select")
#     freezeReactiveValue(input, "event_select")
#     freezeReactiveValue(input, "disease_select")
#     freezeReactiveValue(input, "species_select")
#     
#     # 2. Hard reset all inputs
#     updateSelectInput(session, "region_select", selected = "")
#     updateSelectInput(session, "country_select", selected = "")
#     updateSelectInput(session, "city_select", selected = "")
#     updateSelectInput(session, "date_select", selected = "")
#     updateSelectInput(session, "event_select", selected = "")
#     updateSelectInput(session, "disease_select", selected = "")
#     updateSelectInput(session, "species_select", selected = "")
#     
#     # 3. Clear the click info and reset zoom
#     clicked_info(NULL)
#     leafletProxy("map") %>% flyTo(lng = 0, lat = 30, zoom = 1.75)
#   })
#   
# }

# shinyApp(ui = ui, server = server)








