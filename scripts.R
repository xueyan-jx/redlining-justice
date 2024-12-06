### Library and data preparation
  
## Load libraries and data

# Load libraries
library(here)
library(tidyverse)
library(sf)
library(viridisLite)
library(tmap)
library(tmaptools)      # for reading OpenStreetMap data
library(knitr)          # for generating table
library(scales)         # for using function "percent()"
library(ggplot2)        
library(patchwork)      # for combining multiple ggplot2 graphics
library(testthat)

# Load data
ejscreen <- sf::st_read(here::here("data","data", "ejscreen", "EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.gdb"), quiet = TRUE)

HOLC_grade <- sf::st_read(here::here("data","data","mapping-inequality","mapping-inequality-los-angeles.json"),quiet = TRUE)

gbif_birds_LA <- sf::st_read(here::here("data","data","gbif-birds-LA", "gbif-birds-LA.shp"), quiet = TRUE)

# Filter ejscreen to LA county
LA <- ejscreen %>%
  dplyr::filter(CNTY_NAME %in% c("Los Angeles County"))

## Check and transform coordinate reference systems

# Check if coordinate reference systems (CRS) match
if((st_crs(LA) == st_crs(HOLC_grade)) && 
   (st_crs(LA) == st_crs(gbif_birds_LA))){
  print("all match!")
}else{
  print("not match")
}

# Transform CRS so that they can match each other
LA_trans <- st_transform(LA, crs = st_crs(HOLC_grade))

# Test the match of CRS again
match_test_result <- expect_true(all(st_crs(LA_trans) == st_crs(HOLC_grade) & 
                                       st_crs(HOLC_grade) == st_crs(gbif_birds_LA)))

## Check the validity of geometries and make geometry valid if it is invalid
if(all(st_is_valid(HOLC_grade))){
  print("geometry is valid")
}else{
  HOLC_grade_valid <- st_make_valid(HOLC_grade)
}

### Part 1: Legacy of redlining in current environmental (in)justice

## Extract base map information from Open Street Map (OSM) data

# Download base map from OSM of the bounding box
background_data <- tmaptools::read_osm(sf::st_bbox(HOLC_grade_valid))

## Map of historical redlining neighborhoods
tmap_mode("plot")
tm_shape(background_data) + 
  tm_rgb() + 
  tm_shape(HOLC_grade_valid) +
  tm_polygons(col = "grade",
              palette = viridisLite::viridis(4),
              title = "HOLC grade",
              border.col = NA,
              alpha = 0.8) +
  tm_credits("© OpenStreetMap contributors",
             position=c("right", "bottom")) +
  tm_layout(legend.text.size = 0.85,
            legend.title.size = 1.2,
            legend.text.fontface = "bold",
            legend.title.fontface = "bold",
            legend.outside = TRUE,
            legend.outside.position = ("right"),
            legend.outside.size = 0.24) +
  tm_scale_bar(position = c(0,0),
               breaks = c(0,5,10,15)) +
  tm_compass(position = c(0,0.8),
             size = 1.3)

## Summarize the percent of current census block groups within each HOLC grade (or none)
# Join HOLC grade information to ejscreen data of LA 
HOLC_grade_LA <- st_join(LA_trans, HOLC_grade_valid)

# Filter joined dataset to target columns
HOLC_grade_LA_filtered <- HOLC_grade_LA %>%
  select("ID", "grade", "LOWINCPCT", "P_PM25","P_LIFEEXPPCT")

# Summarize the percent of current census block groups within each HOLC grade (or none)
summary_HOLC_grade <- HOLC_grade_LA_filtered %>%
  group_by(grade) %>%
  summarize(percentage = percent(n()/nrow(HOLC_grade_LA_filtered))) %>%
  st_drop_geometry()

# Display the summary table
kable(summary_HOLC_grade, col.names = c("Grade", "Percentage"), caption = "Table 1 The Percent of Census Block Groups within Each HOLC Grade (or none)")


## Summarize and plot mean % low income, mean percentile for PM2.5, and mean percentile for low life expectancy within each HOLC grade (or none)
# Summarize mean % low income within each HOLC grade (or none)
summary_LowIncomePct <- HOLC_grade_LA_filtered %>%
  group_by(grade) %>%
  summarize(low_income = 100 * mean(LOWINCPCT, na.rm = TRUE), 
            .groups = 'drop')

# Summarize mean percentile for Particulate Matter 2.5 within each HOLC grade (or none)
summary_PPM2.5 <- HOLC_grade_LA_filtered %>%
  group_by(grade) %>%
  summarize(PM2.5 = mean(P_PM25, na.rm = TRUE), 
            .groups = 'drop')

# Summarize mean percentile for low life expectancy within each HOLC grade (or none)
summary_LifeExpPct <- HOLC_grade_LA_filtered %>%
  group_by(grade) %>%
  summarize(P_LifeExpPct = mean(P_LIFEEXPPCT, na.rm = TRUE), 
            .groups = 'drop')

# Plot the above summary information

Plot_LOWINCPCT <- 
  ggplot(summary_LowIncomePct, 
         aes(x = grade, y = low_income, fill = "Mean % Low Income")) +
  geom_bar(stat = "identity") +
  labs(y = "Mean % Low Income") +
  scale_fill_manual(values = "skyblue", name = "") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), # delete x label
        axis.title.y = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8, face = "bold"))

Plot_PPM2.5 <- 
  ggplot(summary_PPM2.5, 
         aes(x = grade, y = PM2.5, fill = "Mean Percentile for PM2.5")) +
  geom_bar(stat = "identity") +
  labs(y = "Mean Percentile for PM2.5") +
  scale_fill_manual(values = "grey", name = "") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), # delete x label
        axis.title.y = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8, face = "bold"))

Plot_LifeExpPct <- 
  ggplot(summary_LifeExpPct, 
         aes(x = grade, y = P_LifeExpPct, fill = "Mean Percentile for Low Life Expectancy")) +
  geom_bar(stat = "identity") +
  labs(x = "HOLC Grade",
       y = "Mean Percentile for Low Life Expectancy") +
  scale_fill_manual(values = "darkgreen", name = "") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8, face = "bold"))

# Combine the plots and align legend
combined_plot <- (Plot_LOWINCPCT / Plot_PPM2.5 / Plot_LifeExpPct) +
  plot_layout(guides = "collect",
              heights = c(1, 1, 1)) +
  theme(legend.position = "right", 
        legend.justification = "right", 
        legend.text = element_text(hjust = 0),
        plot.margin = margin(20, 20, , ))
print(combined_plot)


### Part 2: Legacy of redlining in biodiversity observations

## Join datasets and summarize bird observation within each HOLC grade

# Find bird observation records within HOLC grade 
# Include records on the boundaries
HOLC_bird_LA <- st_join(HOLC_grade_valid, gbif_birds_LA, join = st_intersects)

# Summarize bird observation within each HOLC grade
# Exclude observation without HOLC grade (marked as none)
summary_bird <- HOLC_bird_LA %>%
  filter(!is.na(grade)) %>%
  group_by(grade) %>%
  summarize(bird = n()/nrow(HOLC_bird_LA[!is.na(HOLC_bird_LA$grade), ]),
            .groups = 'drop')

## Plot the percent of observations within redlined neighborhoods within each HOLC grade
# Make labels for pie plot and calculate suitable positions for them
summary_bird <- summary_bird %>%
  arrange(desc(grade)) %>%
  mutate(cumulative = cumsum(bird) - bird / 2,
         bird_label = scales::percent(bird, accuracy = 0.1))
# Draw pie plot
Bird_observation <- 
  ggplot(summary_bird, 
         aes(x = "", y = bird, fill = grade)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("forestgreen","cornflowerblue",
                               "goldenrod1","firebrick3"),
                    name = "HOLC Grade") +
  geom_text(aes(label = scales::percent(bird, accuracy = 0.1), 
                y = cumulative), 
            color = "white", 
            size = 4) +
  theme_void()
print(Bird_observation)
