# agent-based modelling 
# Ashleigh Smith

# loading packages ----------------------------------------
library(abmR)
library(sf)
library(terra)
library(tidyverse)
library(dplyr)
library(raster)
library(pwr)

# practice simulation -----------------------------------------------------------
am.pop.1 = as.species(x = -98.7, y = 34.7)
prac <- energySIM(replicates = 250,
  days = 10,
  modeled_species = am.pop.1,
  env_rast = ex_raster,
  optimum_lo = 0.6,
  optimum_hi = 0.8,
  dest_x = -108.6,
  dest_y = 26.2,
  mot_x = 1,
  mot_y = 1,
  search_radius = 400,
  direction = "S",
  sigma = 1,
  mortality = TRUE,
  init_energy = 100,
  single = TRUE)
energyVIZ(prac, title = 'Visualising EnergySIM results', type = 'plot', aspect_ratio = 5/3,
          label = TRUE)
energyVIZ(prac, type = "summary_table")

# power analysis -----------------------------------------
pwr.t.test(n = 250, d = 0.99, sig.level = 0.05)
# 28.9 groups for power value of 80 so 100 is more than enough 

# veld age map -----------------------------------------------------------------
# load the raster of age
age <- rast("C:/Quantitative biology/Cape Sugarbird Project/data/vegage_karoo.tif")

# visualize untransformed veld age maps for all years from 1980 to 2020
age %>% as.data.frame(xy = TRUE) %>% 
  pivot_longer(cols = as.character(1980:2020), names_to = "Year") %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = value)) +
  facet_wrap(.~Year)

# convert veld age raster to RasterStack and convert NA values to be 0 (justification for this in Methods)
age_stack <- raster::stack(age)
age_stack[is.na(age_stack[])] <- 0
age_stack <- raster::stack(age_stack)

plot(age_stack$X2020) # visualizing transformed raster 

# model 1: study system based on literature with motivation 1 so most amount of -------
# motivation to move away from fire 
am.pop = as.species(x = 22.4, y = -33.35)
sim1 <- energySIM(replicates = 100,
                  days = 10,
                  modeled_species = am.pop,
                  env_rast = age_stack,
                  optimum_lo = 4,
                  optimum_hi = 80,
                  dest_x = 22.25,
                  dest_y = -33.9,
                  mot_x = 1,
                  mot_y = 1,
                  search_radius = 4,
                  direction = "S",
                  sigma = 0.3,
                  mortality = T,
                  init_energy = 50)

p1 <- energyVIZ(sim1, title = 'Visualising EnergySIM results', type = 'plot', aspect_ratio = 5/3,
          label = TRUE, xlim = c(21, 23), ylim = c(-34.5,-33))
energyVIZ(sim1, type = "summary_table")
tidy_results(sim1, type = "results")

# model 2: "" motivation 0.5 -----------------------------------------------------
sim2 <- energySIM(replicates = 100,
                  days = 10,
                  modeled_species = am.pop,
                  env_rast = age_stack,
                  optimum_lo = 4,
                  optimum_hi = 80,
                  dest_x = 22.25,
                  dest_y = -33.9,
                  mot_x = 0.5,
                  mot_y = 0.5,
                  search_radius = 4,
                  direction = "S",
                  sigma = 0.3,
                  mortality = T,
                  init_energy = 50)
energyVIZ(sim2, title = 'Visualising EnergySIM results', type = 'plot', aspect_ratio = 5/3,
          label = TRUE, xlim = c(21, 23), ylim = c(-34.5,-33))
energyVIZ(sim2, type = "summary_table")
tidy_results(sim2, type = "results")

# model 3: "" motivation 0 -------------------------------------------------------
sim3 <- energySIM(replicates = 100,
                  days = 10,
                  modeled_species = am.pop,
                  env_rast = age_stack,
                  optimum_lo = 5,
                  optimum_hi = 80,
                  dest_x = 22.25,
                  dest_y = -33.9,
                  mot_x = 0,
                  mot_y = 0,
                  search_radius = 4,
                  direction = "S",
                  sigma = 0.3,
                  mortality = T,
                  init_energy = 50)
energyVIZ(sim3, title = 'Visualising EnergySIM results', type = 'plot', aspect_ratio = 5/3,
          label = TRUE, xlim = c(21, 23), ylim = c(-34.5,-33))
energyVIZ(sim3, type = "summary_table")
tidy_results(sim3, type = "results")




