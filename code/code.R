# Move history of  resighted Cape Sugarbirds from SAFRING data

# loading packages -------------------------------------------------------
library(magrittr)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

# data wrangling and converting capture-recapture data into linestring entries ----
# for resighted individuals 
Capture_recapture_data$Ring_number <- as.factor(Capture_recapture_data$Ring_number)
Capture_recapture_data <-  Capture_recapture_data %>% 
  drop_na(`Latitude decimal`, `Longitude decimal`, Code)
Capture_recapture_data$Code <- as.numeric(Capture_recapture_data$Code)

Capture_recapture_data <- st_as_sf(Capture_recapture_data, 
                                   coords = c("Longitude decimal", "Latitude decimal"), 
                                   crs = 4326) 
Capture_recapture_data <- Capture_recapture_data %>% 
  group_by(Ring_number) %>%
  summarize(m = mean(Code)) %>%
  filter(st_geometry_type(.) == "MULTIPOINT") %>%
  st_cast("LINESTRING")

st_length(Capture_recapture_data)
mean(st_length(Capture_recapture_data))
# 7456.571m on average

# plotting the linestring entries onto basemap of southwestern Cape ------------
world <- ne_countries(scale = "medium", returnclass = "sf")

move_hist <- ggplot() + 
  geom_sf(data = world) + 
  geom_sf(data = Capture_recapture_data, lwd = 1, lineend = "round") +
  coord_sf(xlim = c(17.8, 25.8), ylim = c(-32.5, -35), expand = FALSE)
