## ----include = FALSE----------------------------------------------------------
run_code <- requireNamespace("sf", quietly = TRUE) &
  requireNamespace("terra", quietly = TRUE) &
  requireNamespace("ggplot2", quietly = TRUE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = run_code
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(GeoThinneR)
library(terra)
library(sf)
library(ggplot2)

## ----load data, message=FALSE, results='hide'---------------------------------
# Set seed for reproducibility
set.seed(123)

# Simulated dataset
n <- 2000
sim_data <- data.frame(
  lon = runif(n, min = -10, max = 10),
  lat = runif(n, min = -5, max = 5),
  sp = sample(c("sp1", "sp2"), n, replace = TRUE)
)

# Load real-world occurrence dataset
data("caretta")

# Load Mediterranean Sea polygon
medit <- system.file("extdata", "mediterranean_sea.gpkg", package = "GeoThinneR")
medit <- sf::st_read(medit)

## ----plot loaded data, fig.show="hold", out.width="50%", echo = FALSE---------
ggplot() +
  geom_point(data = sim_data, aes(x = lon, y = lat, shape = sp), color = "#5183B3") +
  scale_shape_manual(values = c(sp1 = 16, sp2 = 17)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Simulated Species Occurrences") +
  theme_minimal()

ggplot() +
  geom_sf(data = medit, color = "#353839", fill = "antiquewhite", alpha = 0.7)  +
  geom_point(data = caretta, aes(x = decimalLongitude, y = decimalLatitude), color = "#5183B3", alpha = 1) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("C. caretta Mediterranean Sea Occurrences") +
  theme(
      panel.grid.major = element_line(color = gray(.5),linetype = "dashed",linewidth = 0.5),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank(),
      legend.position = "bottom"
    )

## -----------------------------------------------------------------------------
# Apply spatial thinning to the simulated data
quick_thin <- thin_points(
  data = sim_data,     # Dataframe with coordinates
  lon_col = "lon",     # Longitude column name
  lat_col = "lat",     # Latitude column name
  method = "distance", # Method for thinning
  thin_dist = 20,      # Thinning distance in km,
  trials = 5,          # Number of trials
  all_trials = TRUE,   # Return all trials
  seed = 123           # Seed for reproducibility
)

quick_thin

## -----------------------------------------------------------------------------
summary(quick_thin)

## -----------------------------------------------------------------------------
plot(quick_thin)

## -----------------------------------------------------------------------------
# Number of kept points in each trial
sapply(quick_thin$retained, sum)

## -----------------------------------------------------------------------------
head(largest(quick_thin))

## -----------------------------------------------------------------------------
head(get_trial(quick_thin, trial = 2)) 

## -----------------------------------------------------------------------------
trial <- 2
head(quick_thin$original_data[!quick_thin$retained[[trial]], ])

## -----------------------------------------------------------------------------
sf_thinned <- as_sf(quick_thin)
class(sf_thinned)

## -----------------------------------------------------------------------------
# Apply spatial thinning to the real data
caretta_thin <- thin_points(
  data = caretta, # We will not specify lon_col, lat_col as they are in position 1 and 2
  lat_col = "decimalLatitude",
  lon_col = "decimalLongitude",
  method = "distance",
  thin_dist = 30, # Thinning distance in km,
  trials = 5,
  all_trials = FALSE,
  seed = 123
)

# Thinned dataframe stored in the first element of the retained list
identical(largest(caretta_thin), get_trial(caretta_thin, 1))

## ----echo = FALSE-------------------------------------------------------------
ggplot() +
  geom_sf(data = medit, color = "#353839", fill = "antiquewhite", alpha = 0.7)  +
  geom_point(data = caretta, aes(x = decimalLongitude, y = decimalLatitude),color = "#EB714B", alpha = 0.5) +
  geom_point(data = largest(caretta_thin), aes(x = decimalLongitude, y = decimalLatitude),color = "#5183B3", alpha = 1) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("C. caretta Mediterranean Sea Occurrences") +
  theme(
      panel.grid.major = element_line(color = gray(.5),linetype = "dashed",linewidth = 0.5),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank(),
      legend.position = "bottom"
    )

## -----------------------------------------------------------------------------
dist_thin <- thin_points(sim_data, method = "distance", thin_dist = 25, trials = 3)
plot(dist_thin)

## -----------------------------------------------------------------------------
brute_thin <- thin_points(
  data = sim_data,
  method = "distance",
  search_type = "brute",
  thin_dist = 20,
  trials = 10,
  all_trials = FALSE,
  seed = 123
)
nrow(largest(brute_thin))

## -----------------------------------------------------------------------------
kd_tree_thin <- thin_points(
  data = sim_data,
  method = "distance",
  search_type = "kd_tree",
  thin_dist = 20,
  trials = 10,
  all_trials = FALSE,
  seed = 123
)
nrow(largest(kd_tree_thin))

## -----------------------------------------------------------------------------
local_kd_tree_thin <- thin_points(
  data = sim_data,
  method = "distance",
  search_type = "local_kd_tree",
  thin_dist = 20,
  trials = 10,
  all_trials = FALSE,
  seed = 123
)
nrow(largest(local_kd_tree_thin))

## -----------------------------------------------------------------------------
k_estimation_thin <- thin_points(
  data = sim_data,
  method = "distance",
  search_type = "k_estimation",
  thin_dist = 20,
  trials = 10,
  all_trials = FALSE,
  seed = 123
)
nrow(largest(k_estimation_thin))

## -----------------------------------------------------------------------------
system.time(
grid_thin <- thin_points(
  data = sim_data,
  method = "grid",
  resolution = 1,
  origin = c(0, 0),
  crs = "epsg:4326",
  n = 1, # one point per grid cell
  trials = 10,
  all_trials = FALSE,
  seed = 123
))
nrow(largest(grid_thin))

## ----message=FALSE, warning=FALSE---------------------------------------------
rast_obj <- terra::rast(xmin = -10, xmax = 10, ymin = -5, ymax = 5, res = 1)
system.time(
grid_raster_thin <- thin_points(
  data = sim_data,
  method = "grid", 
  raster_obj = rast_obj,
  trials = 10,
  n = 1,
  all_trials = FALSE,
  seed = 123
))
nrow(largest(grid_raster_thin))

## ----echo = FALSE-------------------------------------------------------------
rast_obj <- terra::as.polygons(rast_obj)
rast_obj <- sf::st_as_sf(rast_obj)
ggplot() +
  geom_sf(data = rast_obj, color = "#353839") +
  geom_point(data = sim_data, aes(x = lon, y = lat),color = "#EB714B", alpha = 0.5) +
  geom_point(data = largest(grid_raster_thin), aes(x = lon, y = lat),color = "#5183B3", alpha = 1) +
  labs(title = "Grid-based thinning results",
       x = "Longitude",
       y = "Latitude",
       caption = "Blue points are kept, red points are deleted") +
  theme_minimal()

## -----------------------------------------------------------------------------
system.time(
precision_thin <- thin_points(
  data = sim_data,
  method = "precision", 
  precision = 0,
  trials = 50,
  all_trials = FALSE,
  seed = 123
))
nrow(largest(precision_thin))

## ----echo = FALSE-------------------------------------------------------------
ggplot() +
  geom_point(data = sim_data, aes(x = lon, y = lat),  color = "#EB714B", size = 3, alpha = 0.5) +
  geom_point(data = largest(precision_thin), aes(x = lon, y = lat), color = "#5183B3", size = 3) +
  labs(title = "Precision-based thinning results",
       x = "Longitude",
       y = "Latitude",
       caption = "Blue points are kept, red points are deleted") +
  theme_minimal()

## -----------------------------------------------------------------------------
all_thin <- thin_points(
  data = sim_data,
  thin_dist = 100,
  seed = 123
)
grouped_thin <- thin_points(
  data = sim_data,
  group_col = "sp",
  thin_dist = 100,
  seed = 123
)

nrow(largest(all_thin))
nrow(largest(grouped_thin))

## ----echo =FALSE, fig.show="hold",  out.width="50%"---------------------------
removed <- sim_data[!all_thin$retained[[1]], ]
removed_group <- sim_data[!grouped_thin$retained[[1]], ]

ggplot() +
  geom_point(data = removed, aes(x = lon, y = lat, shape = sp), color = "#EB714B",  alpha = 0.2) +
  geom_point(data = largest(all_thin), aes(x = lon, y = lat, shape = sp), color = "#5183B3", alpha = 1) +
  scale_shape_manual(values = c(sp1 = 15, sp2 = 17)) +
  ggtitle("Thinning without grouping") +
  theme_minimal()

ggplot() +
  geom_point(data = removed_group, aes(x = lon, y = lat, shape = sp), color = "#EB714B",  alpha = 0.2) +
  geom_point(data = largest(grouped_thin), aes(x = lon, y = lat, shape = sp), color = "#5183B3", alpha = 1) +
  scale_shape_manual(values = c(sp1 = 15, sp2 = 17)) +
  ggtitle("Thinning by group") +
  theme_minimal()

## -----------------------------------------------------------------------------
targeted_thin <- thin_points(
  data = caretta,
  lon_col = "decimalLongitude",
  lat_col = "decimalLatitude",
  target_points = 150,
  thin_dist = 30,
  all_trials = FALSE,
  seed = 123,
  verbose = TRUE
)
nrow(largest(targeted_thin))

## ----echo = FALSE-------------------------------------------------------------
ggplot() +
  geom_sf(data = medit, color = "#353839", fill = "antiquewhite", alpha = 0.7)  +
  geom_point(data = largest(targeted_thin), aes(x = decimalLongitude, y = decimalLatitude),color = "#5183B3", alpha = 1) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Exact number of occurrences") +
  theme(
      panel.grid.major = element_line(color = gray(.5),linetype = "dashed",linewidth = 0.5),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank(),
      legend.position = "bottom"
    )

## -----------------------------------------------------------------------------
grid_thin <- thin_points(
  data = caretta,
  lon_col = "decimalLongitude",
  lat_col = "decimalLatitude",
  method = "grid",
  resolution = 2,
  seed = 123
)

# Substracting the maximum - the highest uncertainty becomes the lowest priority and vice versa.
priority <- max(caretta$coordinateUncertaintyInMeters, na.rm = TRUE) - caretta$coordinateUncertaintyInMeters
priority_thin <- thin_points(
  data = caretta,
  lon_col = "decimalLongitude",
  lat_col = "decimalLatitude",
  method = "grid",
  resolution = 2,
  priority = priority,
  seed = 123
)

mean(largest(grid_thin)$coordinateUncertaintyInMeters, na.rm = TRUE)
mean(largest(priority_thin )$coordinateUncertaintyInMeters, na.rm = TRUE)

## -----------------------------------------------------------------------------
sessionInfo()

