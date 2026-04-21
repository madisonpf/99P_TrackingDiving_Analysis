##### TRACKING DATA ANALYSIS - DESCRIPTIVE STATISTICS ##### 

##### HOUSEKEEPING #####
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(terra)
library(purrr)
library(stringr)
library(tibble)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidytext)
library(readr)
library(ggrepel)
library(sf)
library(sp)
library(adehabitatHR)

#Set working directory 
setwd("~/Documents/PhD Documents/99P Analysis/99P_TrackingDiving_Analysis")

#Download data - change path here if necessary 
all_groups_tracking <- read.csv("./RawData/all_groups_tracking.csv", stringsAsFactors = FALSE)
all_groups_diving <- read.csv("./RawData/all_groups_diving.csv", stringsAsFactors = FALSE)

#Pre-set world settings 
world <- ne_countries(scale = "medium", returnclass = "sf")
# Basemap data for Pacific-centered plotting just in case world general doesn't have the correct lat/lon
world2_df <- map_data("world2")

##### PREP DATA + MAPPING TRACKS #####

# Create unique trip identifier for each deployment
all_tracking_clean <- all_tracking_clean %>%
  mutate(
    trip_id = coalesce(
      as.character(TOPPID),
      as.character(deployment_ID)
    )
  )

# Create initial plotting dataset with valid coordinates and group labels
track_map_df <- all_tracking_clean %>%
  filter(!is.na(track_lon), !is.na(track_lat)) %>%
  mutate(
    sex_group = case_when(
      dataset_source == "female" ~ "Female",
      dataset_source == "male" ~ "Male",
      dataset_source == "99P" ~ "99P"
    )
  )

## Assign trip type based on departure month ----
# PB = post-breeding trip (Feb-Apr)
# PM = post-molt trip (May-Jan)
# 99P is manually assigned as PMtrip_lookup <- all_tracking_clean %>%
  distinct(dataset_source, trip_id, depart_datetime, arrival_datetime) %>%
  mutate(
    depart_month = month(depart_datetime),
    
    trip = case_when(
      dataset_source == "99P" ~ "PM",
      depart_month %in% c(2, 3, 4) ~ "PB",
      depart_month %in% c(5, 6, 7, 8, 9, 10, 11, 12, 1) ~ "PM",
      TRUE ~ NA_character_
    )
  ) %>%
  select(dataset_source, trip_id, trip)

# Merge inferred trip back into main tracking file
all_tracking_clean <- all_tracking_clean %>%
  left_join(
    trip_lookup %>% rename(trip_lookup = trip),
    by = c("dataset_source", "trip_id")
  ) %>%
  mutate(
    trip = coalesce(trip, trip_lookup)
  ) %>%
  select(-trip_lookup)

# Build tracking map ---- 
#Filter tracking data for PM trips only, valid coordinates, and acceptable QC flags
track_map_df <- all_tracking_clean %>%
  filter(
    trip == "PM",
    !is.na(track_lon),
    !is.na(track_lat),
    is.na(Data_Track_QCFlag) | Data_Track_QCFlag <= 3
  ) %>%
  mutate(
    sex_group = case_when(
      dataset_source == "female" ~ "Female",
      dataset_source == "male" ~ "Male",
      dataset_source == "99P" ~ "99P"
    ),
    plot_time = coalesce(DateTime, time, date),
    
    # Convert longitudes to 0-360 for Pacific-centered plotting
    track_lon_wrap = ifelse(track_lon < 0, track_lon + 360, track_lon)
  ) %>%
  arrange(trip_id, plot_time)


# Check! Count track records by group
track_map_df %>%
  count(sex_group)

# Check! Summarize QC filtering by group
track_map_df %>%
  group_by(sex_group) %>%
  summarise(
    n_qc_missing = sum(is.na(Data_Track_QCFlag)),
    n_qc_le_3 = sum(!is.na(Data_Track_QCFlag) & Data_Track_QCFlag <= 3),
    .groups = "drop"
  )

# choose one label position per group
label_df <- track_map_df %>%
  group_by(sex_group) %>%
  summarise(
    label_x = quantile(track_lon_wrap, 0.55, na.rm = TRUE),
    label_y = quantile(track_lat, 0.80, na.rm = TRUE),
    .groups = "drop"
  )

# Plot horizontal movement tracks for Female, Male, and 99P ----
# Use world2df to account for crossing dateline 
# Color by sex group
p_track_map_PM <- ggplot() +
  geom_polygon(
    data = world2_df,
    aes(x = long, y = lat, group = group),
    fill = "grey97",
    color = "grey65",
    linewidth = 0.25
  ) +
  
  geom_path(
    data = track_map_df %>% filter(sex_group == "Female"),
    aes(x = track_lon_wrap, y = track_lat, group = trip_id),
    color = "#F4A6B7",
    alpha = 0.12,
    linewidth = 0.9
  ) +
  
  geom_path(
    data = track_map_df %>% filter(sex_group == "Male"),
    aes(x = track_lon_wrap, y = track_lat, group = trip_id),
    color = "#386fa4",
    alpha = 0.18,
    linewidth = 0.9
  ) +
  
  geom_path(
    data = track_map_df %>% filter(sex_group == "99P"),
    aes(x = track_lon_wrap, y = track_lat, group = trip_id),
    color = "#6A3D9A",
    linewidth = 1.6
  ) +
  
  geom_label_repel(
    data = label_df,
    aes(x = label_x, y = label_y, label = sex_group, color = sex_group),
    fill = "white",
    fontface = "bold",
    size = 4,
    label.size = 0.25,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey40",
    segment.size = 0.3,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      Female = "#F4A6B7",
      Male = "#386fa4",
      `99P` = "#6A3D9A"
    )
  ) +
  
  coord_cartesian(
    xlim = c(160, 250),
    ylim = c(20, 65),
    expand = FALSE
  ) +
  
  scale_x_continuous(
    breaks = c(160, 180, 200, 220, 240),
    labels = c("160°E", "180°", "160°W", "140°W", "120°W")
  ) +
  
  scale_y_continuous(
    breaks = c(20, 30, 40, 50, 60),
    labels = c("20°N", "30°N", "40°N", "50°N", "60°N")
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ); p_track_map_PM

# Save horizontal track map
ggsave(
  filename = "./Tracking/Outputs/Horizontal_track_map.png",
  plot = p_track_map_PM,
  width = 9,
  height = 6,
  dpi = 600
)

##### COMPUTE METRICS FOR 99P VS. OTHER GROUPS #####

#Check from Molly's code for metrics 

#Distance from coast ----
#Determine distance from land 
coast <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(32610) %>%
  st_make_valid()

#Build coastal boundary
coast_union <- st_union(coast)

all_tracking_clean <- all_tracking_clean %>%
  mutate(row_id = row_number())

missing_pts <- all_tracking_clean %>%
  filter(is.na(DistCoast)) %>%
  filter(!is.na(track_lon), !is.na(track_lat)) %>%
  st_as_sf(coords = c("track_lon", "track_lat"), crs = 4326) %>%
  st_transform(32610) 

missing_pts <- missing_pts %>%
  mutate(
    DistCoast_calc =
      as.numeric(st_distance(geometry, coast_union)) / 1000
  )

# pull the calculated distances back out of the sf object
missing_fill <- missing_pts %>%
  st_drop_geometry() %>%
  select(row_id, DistCoast_calc)

# join back to the main dataframe and fill missing DistCoast values
all_tracking_clean <- all_tracking_clean %>%
  left_join(missing_fill, by = "row_id") %>%
  mutate(
    DistCoast = coalesce(DistCoast, DistCoast_calc)
  ) %>%
  select(-DistCoast_calc, -row_id)

table(
  is.na(all_tracking_clean$DistCoast),
  all_tracking_clean$dataset_source
)

#Distance from colony ----
# Define Año Nuevo colony reference point (longitude, latitude)
# Coordinates are projected to UTM Zone 10N (EPSG:32610) so distances are in meters
ano_point <- st_sfc(
  st_point(c(-122.3308, 37.1089)),
  crs = 4326
) %>%
  st_transform(32610)

# Add temporary row identifier so calculated distances can be joined back later
all_tracking_clean <- all_tracking_clean %>%
  mutate(row_id = row_number())

# Convert track coordinates to spatial points and calculate distance to colony
# Distances are computed in meters and converted to kilometers
ano_pts <- all_tracking_clean %>%
  filter(!is.na(track_lon), !is.na(track_lat)) %>%
  st_as_sf(coords = c("track_lon", "track_lat"), crs = 4326) %>%
  st_transform(32610) %>%
  mutate(
    DistAno = as.numeric(st_distance(geometry, ano_point)) / 1000
  )

# Extract calculated distances from spatial object
ano_fill <- ano_pts %>%
  st_drop_geometry() %>%
  select(row_id, DistAno)

# Join calculated distances back to the main tracking dataframe
all_tracking_clean <- all_tracking_clean %>%
  left_join(ano_fill, by = "row_id") %>%
  select(-row_id)

# Quick diagnostics: check missing distances by dataset
table(is.na(all_tracking_clean$DistAno), all_tracking_clean$dataset_source)

# Summary distribution of distances from colony
summary(all_tracking_clean$DistAno)

# Deployment-level summaries ----
# Summarize movement metrics for each deployment (trip)
# Metrics describe spatial behavior relative to coastline and colony
deployment_dist <- all_tracking_clean %>%
  filter(trip == "PM") %>%
  group_by(dataset_source, animal_id, trip_id) %>%
  summarise(
    mean_DistCoast = mean(DistCoast, na.rm = TRUE),
    mean_DistAno   = mean(DistAno, na.rm = TRUE),
    max_DistAno    = max(DistAno, na.rm = TRUE),
    max_DistCoast  = max(DistCoast, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = case_when(
      dataset_source == "female" ~ "Female",
      dataset_source == "male" ~ "Male",
      dataset_source == "99P" ~ "99P"
    )
  )

# Keep Female and Male deployments for group comparisons
sex_compare <- deployment_dist %>%
  filter(group %in% c("Female", "Male"))

# Remove extreme outliers using the standard 1.5 × IQR rule
# This prevents rare extreme trips from dominating boxplot summaries
sex_compare_no_outliers <- sex_compare %>%
  group_by(group) %>%
  mutate(
    Q1 = quantile(max_DistAno, 0.25, na.rm = TRUE),
    Q3 = quantile(max_DistAno, 0.75, na.rm = TRUE),
    IQR_val = IQR(max_DistAno, na.rm = TRUE),
    lower_bound = Q1 - 1.5 * IQR_val,
    upper_bound = Q3 + 1.5 * IQR_val
  ) %>%
  filter(max_DistAno >= lower_bound, max_DistAno <= upper_bound) %>%
  ungroup() %>%
  select(-Q1, -Q3, -IQR_val, -lower_bound, -upper_bound)

# Extract focal individual (99P) values for overlay on plots
p99_vals <- deployment_dist %>%
  filter(group == "99P")

# Plot: Distance from coast ----
# Compare mean distance from coastline between Female and Male groups
# 99P is plotted as a reference point
p_coast <- ggplot(
  sex_compare_no_outliers,
  aes(x = group, y = mean_DistCoast, fill = group)
) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.8,
    outlier.alpha = 0.3
  ) +
  geom_point(
    data = p99_vals,
    aes(x = "Female", y = mean_DistCoast),
    inherit.aes = FALSE,
    color = "black",
    size = 3
  ) +
  geom_text(
    data = p99_vals,
    aes(x = "Female", y = mean_DistCoast, label = "99P"),
    inherit.aes = FALSE,
    vjust = -1,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(Female = "#F4A6B7", Male = "#9EC9FF")
  ) +
  labs(
    x = NULL,
    y = "Mean distance from coast (km)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold")
  ); p_coast

# Plot: Distance from colony ----
# Compare mean distance from Año Nuevo colony between groups
# 99P is plotted as a reference point
p_colony <- ggplot(
  sex_compare_no_outliers,
  aes(x = group, y = mean_DistAno, fill = group)
) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.8,
    outlier.alpha = 0.3
  ) +
  geom_point(
    data = p99_vals,
    aes(x = "Female", y = mean_DistAno),
    inherit.aes = FALSE,
    color = "black",
    size = 3
  ) +
  geom_text(
    data = p99_vals,
    aes(x = "Female", y = mean_DistAno, label = "99P"),
    inherit.aes = FALSE,
    vjust = -1,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(Female = "#F4A6B7", Male = "#9EC9FF")
  ) +
  labs(
    x = NULL,
    y = "Mean distance from colony (km)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold")
  ); p_colony

# Save distance-from-colony figure
ggsave(
  filename = "./Tracking/Outputs/Distance_from_colony_boxplot_PM.png",
  plot = p_colony,
  width = 6,
  height = 5,
  dpi = 600
)



##### UTILIZATION DISTRIBUTION - ADAPTED MOLLY CODE ##### 
# 1. Prepare data ----
ud_df <- track_map_df %>%
  filter(
    !is.na(track_lon_wrap),
    !is.na(track_lat),
    !is.na(sex_group)
  ) %>%
  mutate(group = sex_group)

# female + male only for UDs
ud_df_fit <- ud_df %>%
  filter(group %in% c("Female", "Male")) %>%
  group_by(group) %>%
  filter(n() >= 5) %>%
  ungroup()

# 99P track for overlay
track_99P <- ud_df %>%
  filter(group == "99P") %>%
  arrange(trip_id, plot_time)


# 2. Convert to sf and project ----
tracking_sf <- st_as_sf(
  ud_df_fit,
  coords = c("track_lon_wrap", "track_lat"),
  crs = 4326,
  remove = FALSE
)

track_99P_sf <- st_as_sf(
  track_99P,
  coords = c("track_lon_wrap", "track_lat"),
  crs = 4326,
  remove = FALSE
)

pacific_crs <- "+proj=laea +lat_0=45 +lon_0=180 +datum=WGS84 +units=m +no_defs"

tracking_sf <- st_transform(tracking_sf, crs = pacific_crs)
track_99P_sf <- st_transform(track_99P_sf, crs = pacific_crs)

# land polygon for masking
land <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = pacific_crs)

# coastline boundary shapefile for barrier-constrained UD

boundary <- st_read("./Tracking/barrier_coarse.shp", quiet = TRUE) %>%
  st_transform(crs = pacific_crs)

# quick CRS check
identical(st_crs(tracking_sf), st_crs(boundary))
identical(st_crs(tracking_sf), st_crs(land))

# 3. Create shared grid ----

coords <- st_coordinates(tracking_sf)

grid_buffer <- 500000
grid_res <- 25000

x <- seq(
  min(coords[, "X"]) - grid_buffer,
  max(coords[, "X"]) + grid_buffer,
  by = grid_res
)

y <- seq(
  min(coords[, "Y"]) - grid_buffer,
  max(coords[, "Y"]) + grid_buffer,
  by = grid_res
)

xygrid <- expand.grid(x = x, y = y)
coordinates(xygrid) <- ~ x + y
gridded(xygrid) <- TRUE

# 4. Create coastline barrier object ----

barrier <- as(boundary, "Spatial")

# maximum allowable h based on boundary spacing
boundary_coords <- st_coordinates(boundary)

# if your shapefile has many repeated vertices, this helps
boundary_xy <- unique(boundary_coords[, c("X", "Y"), drop = FALSE])

maxh <- trunc(min(dist(boundary_xy)) / 3, 0)

print(maxh)


# 5. First-pass UD without barrier to get href ----

UDdata <- as(tracking_sf, "Spatial")
UDdata_group <- UDdata["group"]

uds_href <- kernelUD(
  UDdata_group,
  grid = xygrid,
  h = "href"
)

image(uds_href)

# extract href values and cap them at maxh
hvalues <- list()

for (j in seq_along(uds_href)) {
  h_j <- uds_href[[j]]@h$h
  h_j <- ifelse(h_j > maxh, maxh, h_j)
  id_j <- names(uds_href)[j]
  hvalues[[j]] <- c(h_j, id_j)
}

h_table <- as.data.frame(
  do.call("rbind", hvalues),
  stringsAsFactors = FALSE
)

names(h_table) <- c("h_opt", "group")
h_table$h_opt <- as.numeric(h_table$h_opt)

print(h_table)

# optional: shrink smoothing slightly
h_table <- h_table %>%
  mutate(h_use = h_opt * 0.8)

print(h_table)


# 6. Refit each group with shared grid + coastline barrier ----

optud <- list()

for (k in seq_len(nrow(h_table))) {
  
  this_group <- h_table$group[k]
  this_h <- h_table$h_use[k]
  
  this_pts <- UDdata_group[UDdata_group$group == this_group, ]
  
  this_ud <- kernelUD(
    this_pts,
    h = this_h,
    grid = xygrid
  )
  
  optud[[k]] <- this_ud[[1]]
  cat("Finished:", this_group, "\n")
}

names(optud) <- h_table$group
class(optud) <- "estUDm"

uddf <- optud

image(uddf)

# 7. Mask land and renormalize ----

udsgdf <- as(estUDm2spixdf(uddf), "SpatialGridDataFrame")

# raster stack of all UDs
rstack <- raster::stack(udsgdf)
names(rstack) <- names(udsgdf)

land_sp <- as(land, "Spatial")

# mask out land
rstack_msk <- mask(rstack, land_sp, inverse = TRUE)

# convert mask to 1 / NA template
mask_template <- rstack_msk[[1]]
mask_vals <- !is.na(values(mask_template))

# renormalize each UD so probabilities sum to 1 after masking
resu <- lapply(seq_len(nlayers(rstack_msk)), function(i) {
  vals <- values(rstack_msk[[i]])
  vals[is.na(vals)] <- 0
  vals <- vals / sum(vals, na.rm = TRUE)
  vals
})

resu <- as.data.frame(resu)
names(resu) <- names(rstack_msk)

# rebuild SpatialGridDataFrame
masked_grid <- as(rstack_msk, "SpatialPixelsDataFrame")
masked_grid <- as(masked_grid, "SpatialGridDataFrame")
masked_grid@data <- resu

# ungrid for estUD reconstruction
fullgrid(masked_grid) <- FALSE

re <- lapply(seq_len(ncol(masked_grid@data)), function(m) {
  so <- new("estUD", masked_grid[, m])
  so@h <- list(h = 0, meth = "specified")
  so@vol <- FALSE
  so
})

names(re) <- names(masked_grid@data)
class(re) <- "estUDm"

image(re)


# 8. Extract 90% and 50% contours ----

contours90 <- getverticeshr(
  re,
  percent = 90,
  standardize = TRUE
)

contours50 <- getverticeshr(
  re,
  percent = 50,
  standardize = TRUE
)

# convert to sf object
c90 <- st_as_sf(contoursN90)

c90 <- st_transform(c90, crs = 4326)

c90$area <- st_area(c90)/(1000*1000)

c90 <- st_as_sf(contours90) %>%
  mutate(
    level = "90%",
    area_km2 = area / 1e6
  )

c50 <- st_as_sf(contours50) %>%
  mutate(
    level = "50%",
    area_km2 = area / 1e6
  )

# 9. Overlap ----

ko90 <- kerneloverlaphr(
  uddf,
  method = "BA",
  percent = 90
)

print(ko90)
range(ko90, na.rm = TRUE)

overlap90_df <- as.data.frame(as.table(ko90)) %>%
  rename(
    ID1 = Var1,
    ID2 = Var2,
    BA = Freq
  ) %>%
  filter(ID1 != ID2)

print(overlap90_df)

### Print amount of 99P tracks within the male and female UDs 

# 99P points inside female/male 90% KUD
pts_99P <- track_99P_sf

female90 <- c90 %>% dplyr::filter(id == "Female")
male90   <- c90 %>% dplyr::filter(id == "Male")

in_female90 <- st_within(pts_99P, female90, sparse = FALSE)[, 1]
in_male90   <- st_within(pts_99P, male90, sparse = FALSE)[, 1]

track_overlap_90 <- data.frame(
  group = c("Female", "Male"),
  n_99P_points_in_KUD = c(sum(in_female90), sum(in_male90)),
  n_99P_points_total = nrow(pts_99P),
  pct_99P_points_in_KUD = c(
    100 * mean(in_female90),
    100 * mean(in_male90)
  )
)

print(track_overlap_90)

#For 50% 
female50 <- c50 %>% dplyr::filter(id == "Female")
male50   <- c50 %>% dplyr::filter(id == "Male")

in_female50 <- st_within(pts_99P, female50, sparse = FALSE)[, 1]
in_male50   <- st_within(pts_99P, male50, sparse = FALSE)[, 1]

track_overlap_50 <- data.frame(
  group = c("Female", "Male"),
  n_99P_points_in_core = c(sum(in_female50), sum(in_male50)),
  n_99P_points_total = nrow(pts_99P),
  pct_99P_points_in_core = c(
    100 * mean(in_female50),
    100 * mean(in_male50)
  )
)

print(track_overlap_50)


##### SUMMARY TABLES #####
# 1) 99P vs Female/Male
# 2) Female vs Male overlap

# Objects
pts_99P   <- track_99P_sf
line_99P  <- track_99P_line

female90 <- c90 %>% dplyr::filter(id == "Female")
male90   <- c90 %>% dplyr::filter(id == "Male")

female50 <- c50 %>% dplyr::filter(id == "Female")
male50   <- c50 %>% dplyr::filter(id == "Male")


# Helper functions ----
pct_points_inside <- function(points, polygon) {
  100 * mean(st_within(points, polygon, sparse = FALSE)[, 1])
}

length_inside_km <- function(line, polygon) {
  inter <- st_intersection(line, polygon)
  if (nrow(inter) == 0) return(0)
  as.numeric(sum(st_length(inter))) / 1000
}

safe_overlap_area_km2 <- function(poly1, poly2) {
  inter <- st_intersection(poly1, poly2)
  if (nrow(inter) == 0) return(0)
  as.numeric(sum(st_area(inter))) / 1e6
}

# 1) 99P point + length overlap ----
total_track_length_km <- as.numeric(st_length(line_99P)) / 1000
n_99P_points <- nrow(pts_99P)

len_f90 <- length_inside_km(line_99P, female90)
len_m90 <- length_inside_km(line_99P, male90)
len_f50 <- length_inside_km(line_99P, female50)
len_m50 <- length_inside_km(line_99P, male50)

track_overlap_table <- data.frame(
  comparison = c("99P vs Female", "99P vs Male"),
  pct_99P_points_in_90 = c(
    pct_points_inside(pts_99P, female90),
    pct_points_inside(pts_99P, male90)
  ),
  pct_99P_points_in_50 = c(
    pct_points_inside(pts_99P, female50),
    pct_points_inside(pts_99P, male50)
  ),
  overlap_length_90_km = c(len_f90, len_m90),
  overlap_length_50_km = c(len_f50, len_m50),
  pct_99P_length_in_90 = 100 * c(len_f90, len_m90) / total_track_length_km,
  pct_99P_length_in_50 = 100 * c(len_f50, len_m50) / total_track_length_km,
  n_99P_points = n_99P_points,
  total_99P_length_km = total_track_length_km
)

print(track_overlap_table)


# 2) Female vs Male overlap ----

# build 2D-only summary table
male90 <- female_male_overlap_table %>%
  filter(KUD_level == "90%")

male50 <- female_male_overlap_table %>%
  filter(KUD_level == "50%")

table2_2d <- data.frame(
  sex = c("male", "", "female", ""),
  kernel_density = c("90%", "50%", "90%", "50%"),
  
  area_km2 = c(
    male90$male_area_km2,
    male50$male_area_km2,
    male90$female_area_km2,
    male50$female_area_km2
  ),
  
  pct_overlap = c(
    male90$pct_male_range_overlapped_by_female,
    male50$pct_male_range_overlapped_by_female,
    male90$pct_female_range_overlapped_by_male,
    male50$pct_female_range_overlapped_by_male
  )
) %>%
  mutate(
    area_km2 = round(area_km2, 1),
    pct_overlap = round(pct_overlap, 2)
  )

table2_2d #print 

table2_2d_gt <- table2_2d %>%
  gt() %>%
  cols_label(
    sex = "sex",
    kernel_density = "kernel density",
    area_km2 = html("area (km<sup>2</sup>)"),
    pct_overlap = "% overlap"
  ) %>%
  tab_options(
    table.font.size = 16,
    data_row.padding = px(6)
  )

write.csv(track_overlap_table, "./Tracking/Outputs/99P_vs_sex_KUD_overlap_table.csv", row.names = FALSE)
write.csv(table2_2d, "./Tracking/Outputs/male_female_KUD_overlap_table.csv", row.names = FALSE)


# 10. Area tables ----

area90_df <- c90 %>%
  st_drop_geometry() %>%
  dplyr::select(id, level, area_km2)

area50_df <- c50 %>%
  st_drop_geometry() %>%
  dplyr::select(id, level, area_km2)

print(area90_df)
print(area50_df)

# 11. Build 99P line ----

track_99P_line <- track_99P_sf %>%
  arrange(plot_time) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

track_lines_sf <- tracking_sf %>%
  arrange(group, trip_id, plot_time) %>%
  group_by(group, trip_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  ungroup()

# 12. Plot extents ----

bb <- st_bbox(c90)

xpad <- 2e5
ypad <- 2e5

xlim <- c(
  as.numeric(bb["xmin"] - xpad),
  as.numeric(bb["xmax"] + xpad)
)

ylim <- c(
  as.numeric(bb["ymin"] - ypad),
  as.numeric(bb["ymax"] + ypad)
)

plot_box <- st_as_sfc(st_bbox(
  c(
    xmin = xlim[1],
    xmax = xlim[2],
    ymin = ylim[1],
    ymax = ylim[2]
  ),
  crs = st_crs(c90)
))

land_crop <- st_crop(land, plot_box)

pal <- c(
  'Female' = "#F4A6B7",
  'Male'   = "#386fa4",
  `99P`  = "#6A3D9A"
)

# 13. Build final plot ----

ud_plot <- ggplot() +
  
  geom_sf(
    data = land_crop,
    fill = "grey92",
    color = "grey65",
    linewidth = 0.2
  ) +
  
  #  geom_sf(
  #    data = track_lines_sf,
  #    aes(color = group),
  #    linewidth = 0.2,
  #    alpha = 0.20,
  #    lineend = "round",
  #    show.legend = TRUE
  #  ) +
  
  # 90% KUD: Female first, then Male
  geom_sf(
    data = dplyr::filter(c90, id == "Female"),
    aes(fill = id),
    alpha = 0.30,
    color = NA
  ) +
  geom_sf(
    data = dplyr::filter(c50, id == "Female"),
    aes(fill = id),
    alpha = 0.65,
    color = NA
  ) + 
  geom_sf(
    data = dplyr::filter(c90, id == "Male"),
    aes(fill = id),
    alpha = 0.30,
    color = NA
  ) +
  geom_sf(
    data = dplyr::filter(c50, id == "Male"),
    aes(fill = id),
    alpha = 0.65,
    color = NA
  ) +
  
  geom_sf(
    data = track_99P_line,
    aes(color = "99P"),
    linewidth = 1.2,
    alpha = 0.9,
    lineend = "round"
  ) +
  
  coord_sf(
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  ) +
  
  scale_fill_manual(
    values = pal[c("Male", "Female")],
    name = "50% and 90% KUD"
  ) +
  
  scale_color_manual(
    values = pal,
    name = "Tracks"
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 1.6,
        alpha = 1
      )
    )
  ) +
  
  theme_bw(base_size = 14) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  ); ud_plot

ggsave(
  filename = "./Tracking/Outputs/UD_99P_overlap_map_notracks.png",
  plot = ud_plot,
  width = 8,
  height = 6,
  dpi = 600,
  bg = "white"
)

write.csv(all_tracking_clean, "./Tracking/Outputs/all_groups_tracking.csv")
