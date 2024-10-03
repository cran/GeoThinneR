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

# Simulate the dataset
n <- 2000  # Number of points
sim_data <- data.frame(
  long = runif(n, min = -20, max = 20),
  lat = runif(n, min = -10, max = 10),
  sp = sample(c("sp1", "sp2"), n, replace = TRUE)
)

# Load the Caretta caretta occurrences
data("caretta")

# Load mediterranean sea polygon
medit <- system.file("extdata", "mediterranean_sea.gpkg", package = "GeoThinneR")
medit <- sf::st_read(medit)

## ----plot loaded data, , fig.show="hold",  out.width="50%"--------------------
ggplot() +
  geom_point(data = sim_data, aes(x = long, y = lat, color = sp)) +
  scale_color_manual(values = c(sp1 = "#5183B3", sp2 = "#EB714B")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Simulated Species Occurrences") +
  theme_minimal()

ggplot() +
  geom_sf(data = medit, color = "#353839", fill = "antiquewhite", alpha = 0.7)  +
  geom_point(data = caretta, aes(x = decimalLongitude, y = decimalLatitude),color = "#EB714B", alpha = 1) +
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
thin_sim_data <- thin_points(
  data = sim_data, # Dataframe with coordinates
  long_col = "long", # Longitude column name
  lat_col = "lat", # Latitude column name
  method = "brute_force",  # Method for thinning
  thin_dist = 20,  # Thinning distance in km,
  trials = 5, # Number of reps
  all_trials = TRUE, # Return all trials
  seed = 123 # Seed for reproducibility
)

## -----------------------------------------------------------------------------
# Number of keeped points in each trial
sapply(thin_sim_data, nrow)

## -----------------------------------------------------------------------------
# Apply spatial thinning to the real data
thin_real_data <- thin_points(
  data = caretta, # We will not specify long_col, lat_col as they are in position 1 and 2
  method = "kd_tree",
  thin_dist = 30,  # Thinning distance in km,
  trials = 5,
  all_trials = FALSE,
  seed = 123
)

# Thinned dataframe stored in the first element of the output list
dim(thin_real_data[[1]])

## ----echo = FALSE-------------------------------------------------------------
ggplot() +
  geom_sf(data = medit, color = "#353839", fill = "antiquewhite", alpha = 0.7)  +
  geom_point(data = caretta, aes(x = decimalLongitude, y = decimalLatitude),color = "#EB714B", alpha = 0.5) +
  geom_point(data = thin_real_data[[1]], aes(x = long, y = lat),color = "#5183B3", alpha = 1) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("C. caretta Mediterranean Sea Occurrences") +
  theme(
      panel.grid.major = element_line(color = gray(.5),linetype = "dashed",linewidth = 0.5),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank(),
      legend.position = "bottom"
    )

## -----------------------------------------------------------------------------
system.time(
thin_sim_data <- thin_points(
  data = sim_data,
  method = "brute_force", 
  thin_dist = 20,
  trials = 50,
  all_trials = FALSE,
  seed = 123
))
nrow(thin_sim_data[[1]])

## -----------------------------------------------------------------------------
system.time(
thin_sim_data <- thin_points(
  data = sim_data,
  method = "kd_tree", 
  thin_dist = 20,
  trials = 50,
  all_trials = FALSE,
  seed = 123
))
nrow(thin_sim_data[[1]])

## -----------------------------------------------------------------------------
system.time(
thin_sim_data <- thin_points(
  data = sim_data,
  method = "round_hash", 
  thin_dist = 20,
  trials = 50,
  all_trials = FALSE,
  seed = 123
))
nrow(thin_sim_data[[1]])

## -----------------------------------------------------------------------------
system.time(
thin_sim_data <- thin_points(
  data = sim_data,
  method = "grid", 
  resolution = 1,
  origin = c(0, 0),
  crs = "epsg:4326",
  trials = 50,
  all_trials = FALSE,
  seed = 123
))

## ----message=FALSE, warning=FALSE---------------------------------------------
rast_obj <- terra::rast(xmin = -20, xmax = 20, ymin = -10, ymax = 10, res = 1)
system.time(
thin_sim_data <- thin_points(
  data = sim_data,
  method = "grid", 
  raster_obj = rast_obj,
  trials = 50,
  all_trials = FALSE,
  seed = 123
))

## ----echo = FALSE-------------------------------------------------------------
rast_obj <- terra::as.polygons(rast_obj)
rast_obj <- sf::st_as_sf(rast_obj)
ggplot() +
  geom_sf(data = rast_obj, color = "#353839") +
  geom_point(data = sim_data, aes(x = long, y = lat),color = "#EB714B", alpha = 0.5) +
  geom_point(data = thin_sim_data[[1]], aes(x = long, y = lat),color = "#5183B3", alpha = 1) +
  theme_minimal()

## -----------------------------------------------------------------------------
system.time(
thin_sim_data <- thin_points(
  data = sim_data,
  method = "precision", 
  precision = 0,
  trials = 50,
  all_trials = FALSE,
  seed = 123
))
nrow(thin_sim_data[[1]])

## -----------------------------------------------------------------------------
thin_sim_data <- thin_points(
  data = sim_data,
  thin_dist = 20,
  seed = 123
)
thin_sim_data_group <- thin_points(
  data = sim_data,
  group_col = "sp",
  thin_dist = 20,
  seed = 123
)

nrow(thin_sim_data[[1]])
nrow(thin_sim_data_group[[1]])

## ----echo =FALSE, fig.show="hold",  out.width="50%"---------------------------
removed <- sim_data[-as.numeric(rownames(thin_sim_data[[1]])), ]
removed_group <- sim_data[-as.numeric(rownames(thin_sim_data_group[[1]])), ]

ggplot() +
  geom_point(data = sim_data, aes(x = long, y = lat, color = sp), alpha = 0.2) +
  geom_point(data = removed, aes(x = long, y = lat, color = sp), alpha = 1) +
  scale_color_manual(values = c(sp1 = "#5183B3", sp2 = "#EB714B")) +
  ggtitle("Removed points without grouping") +
  theme_minimal()

ggplot() +
  geom_point(data = sim_data, aes(x = long, y = lat, color = sp), alpha = 0.2) +
  geom_point(data = removed_group, aes(x = long, y = lat, color = sp), alpha = 1) +
  scale_color_manual(values = c(sp1 = "#5183B3", sp2 = "#EB714B")) +
  ggtitle("Removed points for each independent species") +
  theme_minimal()

## -----------------------------------------------------------------------------
thin_real_data <- thin_points(
  data = caretta, 
  target_points = 150,
  thin_dist = 30,
  all_trials = FALSE,
  seed = 123,
  verbose = TRUE
)
nrow(thin_real_data[[1]])

## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = medit, color = "#353839", fill = "antiquewhite", alpha = 0.7)  +
  geom_point(data = thin_real_data[[1]], aes(x = long, y = lat),color = "#5183B3", alpha = 1) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("C. caretta Mediterranean Sea Occurrences") +
  theme(
      panel.grid.major = element_line(color = gray(.5),linetype = "dashed",linewidth = 0.5),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank(),
      legend.position = "bottom"
    )

## -----------------------------------------------------------------------------
thin_real_data <- thin_points(
  data = caretta,
  method = "precision",
  precision = 0,
  seed = 123
)

# Substracting the maximum - the highest uncertainty becomes the lowest priority and vice versa.
priority <- max(caretta$coordinateUncertaintyInMeters) - caretta$coordinateUncertaintyInMeters
thin_real_data_uncert <- thin_points(
  data = caretta,
  method = "precision",
  precision = 0,
  priority = priority,
  seed = 123
)

mean(thin_real_data[[1]]$coordinateUncertaintyInMeters)
mean(thin_real_data_uncert[[1]]$coordinateUncertaintyInMeters)

## -----------------------------------------------------------------------------
sessionInfo()

