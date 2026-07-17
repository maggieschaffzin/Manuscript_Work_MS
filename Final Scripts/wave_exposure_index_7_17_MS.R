## WAVE EXPOSURE INDEX FOR SCB GORGONIAN SITES
## By: Maggie Schaffzin
## Created: 3/15/2026
## Last Edited: July 17th, 2026

#Installing and loading required packages
required_packages <- c("sf", "httr", "utils", "tidyr", "purrr", "waver")
install.packages(setdiff(required_packages, rownames(installed.packages())))

if (!"lwgeom" %in% rownames(installed.packages())) {
  install.packages("lwgeom", type = "binary")
}

library(lwgeom)
library(waver)
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(httr)
library(tidyr)
library(purrr)

#Downloading high resolution coastline
coast_dir <- file.path(getwd(), "coastline_data")
if (!dir.exists(coast_dir)) dir.create(coast_dir)
gshhg_url <- "https://github.com/GenericMappingTools/gshhg-gmt/releases/download/2.3.7/gshhg-shp-2.3.7.zip"
zip_path  <- file.path(coast_dir, "gshhg-shp-2.3.7.zip")
if (!file.exists(zip_path)) {
  message("Downloading GSHHG shapefile (~100MB)...")
  download.file(gshhg_url, zip_path, mode = "wb", method = "auto")
}
if (!dir.exists(file.path(coast_dir, "GSHHS_shp"))) {
  message("Unzipping GSHHG...")
  unzip(zip_path, exdir = coast_dir)
}
gshhg_path <- file.path(coast_dir, "GSHHS_shp", "h", "GSHHS_h_L1.shp")
message("Loading GSHHG coastline...")
world_coast <- st_read(gshhg_path, quiet = TRUE)
sf_use_s2(FALSE)
world_coast <- st_make_valid(world_coast)

#Cropping coastline to SCB
scb_bbox <- st_bbox(c(xmin = -121.5, xmax = -116.5,
                      ymin = 32.0,   ymax = 35.0),
                    crs = st_crs(4326))
scb_coast <- st_crop(world_coast, scb_bbox)

#Checking SCB coastline was cropped right
ggplot() +
  geom_sf(data = scb_coast, fill = "tan", color = "black", linewidth = 0.3) +
  coord_sf(xlim = c(-121.5, -116.5), ylim = c(32.0, 35.0)) +
  labs(title = "SCB Coastline Check", x = "Longitude", y = "Latitude") +
  theme_bw()
ggsave("SCB_coastline_check.png", width = 8, height = 6, dpi = 150)

#Downloading Buoy locations
buoy_info <- data.frame(
  buoy_id = c("46025", "46053", "46054", "46086", "46047"),
  name    = c("Santa Monica Basin",
              "East Santa Barbara",
              "West Santa Barbara",
              "San Clemente Basin",
              "Point Conception/San Diego"),
  lat     = c(33.755,  34.241,  34.274,  32.491,  32.418),
  lon     = c(-119.045, -119.839, -120.468, -118.021, -119.535),
  stringsAsFactors = FALSE
)

#Categorizing our 16 fetch bearings
bearings <- seq(0, 337.5, by = 22.5)

#Downloading one year of NDBC standard met data for one buoy
download_ndbc_year <- function(buoy_id, year) {
  url <- sprintf(
    "https://www.ndbc.noaa.gov/data/historical/stdmet/%sh%d.txt.gz",
    buoy_id, year
  )
  tmp <- tempfile(fileext = ".txt.gz")
  ok  <- tryCatch({
    download.file(url, tmp, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(NULL)
  
  raw <- tryCatch(
    readLines(gzcon(file(tmp, "rb")), warn = FALSE),
    error = function(e) NULL
  )
  if (is.null(raw) || length(raw) < 3) return(NULL)
  
  # Row 1 = header, Row 2 = units, Rows 3+ = data
  header    <- gsub("^#", "", strsplit(trimws(raw[1]), "\\s+")[[1]])
  data_rows <- raw[3:length(raw)]
  
  df <- tryCatch(
    read.table(text = paste(data_rows, collapse = "\n"),
               header = FALSE, col.names = header, fill = TRUE,
               na.strings = c("99", "999", "9999", "999.0", "9999.0", "MM")),
    error = function(e) NULL
  )
  
  if (is.null(df) || !"WDIR" %in% names(df)) return(NULL)
  
  df %>%
    dplyr::select(WDIR) %>%
    filter(!is.na(WDIR), WDIR >= 0, WDIR <= 360) %>%
    mutate(buoy_id = buoy_id, year = year)
}

#Binning observations and computing direction-frequency weights for one buoy
compute_wind_weights <- function(buoy_id, years = 2014:2024) {
  
  all_obs <- map_dfr(years, ~download_ndbc_year(buoy_id, .x))
  
  if (is.null(all_obs) || nrow(all_obs) == 0) {
    return(NULL)
  }
  
  #Finding nearest bearing for each observation
  all_obs <- all_obs %>%
    mutate(bearing_bin = bearings[
      apply(
        outer(WDIR, bearings, function(a, b) {
          d <- abs(a - b); pmin(d, 360 - d)
        }),
        1, which.min
      )
    ])
  
  freq <- all_obs %>%
    count(bearing_bin) %>%
    right_join(data.frame(bearing_bin = bearings), by = "bearing_bin") %>%
    replace_na(list(n = 0)) %>%
    arrange(bearing_bin)
  
  w <- freq$n / sum(freq$n)
  names(w) <- as.character(bearings)
  w
}

#Computing wind weights from NDBC for all buoys
cache_file <- file.path(getwd(), "ndbc_wind_weights.rds")

if (file.exists(cache_file)) {
  message("Loading cached wind weights from ndbc_wind_weights.rds")
  buoy_wind_weights <- readRDS(cache_file)
} else {
  message("Computing wind weights from NDBC (2014-2024)")
  
  #SCB fallback weights (46025-based) if a buoy download fails
  fallback_w <- c(0.03, 0.03, 0.02, 0.02, 0.03, 0.03, 0.03, 0.03,
                  0.04, 0.04, 0.05, 0.08, 0.14, 0.18, 0.16, 0.07)
  fallback_w <- fallback_w / sum(fallback_w)
  names(fallback_w) <- as.character(bearings)
  
  buoy_wind_weights <- list()
  for (bid in buoy_info$buoy_id) {
    w <- compute_wind_weights(bid)
    buoy_wind_weights[[bid]] <- if (is.null(w)) fallback_w else w
  }
  
  saveRDS(buoy_wind_weights, cache_file)
}

#Creating summary table of weights
weight_df <- map_dfr(names(buoy_wind_weights), function(bid) {
  data.frame(buoy_id = bid, bearing = as.numeric(names(buoy_wind_weights[[bid]])),
             weight = buoy_wind_weights[[bid]])
}) %>% left_join(buoy_info %>% dplyr::select(buoy_id, name), by = "buoy_id")

#Saving summary table of weights
write_csv(weight_df, "ndbc_wind_weight_summary.csv")

#Showing top 5 directions per buoy
weight_df %>%
  group_by(buoy_id, name) %>%
  slice_max(weight, n = 5) %>%
  arrange(buoy_id, desc(weight)) %>%
  print(n = 40)

#Creating wind rose plot per buoy
dir_labels <- c("N","NNE","NE","ENE","E","ESE","SE","SSE",
                "S","SSW","SW","WSW","W","WNW","NW","NNW")
ggplot(weight_df, aes(x = factor(bearing, levels = as.character(bearings)),
                      y = weight, fill = weight)) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~name, ncol = 3) +
  coord_polar(start = 0) +
  scale_x_discrete(labels = dir_labels) +
  labs(title = "Wind Direction Climatology per NDBC Buoy (2014-2024)",
       subtitle = "Proportion of hourly observations from each compass direction",
       x = NULL, y = NULL, fill = "Freq.") +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(size = 6),
        strip.text  = element_text(size = 7))

#Saving wind rose plot
ggsave("ndbc_wind_roses.png", width = 12, height = 7, dpi = 200)

#Loading all study site coordinates
site_coords_df <- data.frame(
  Site = c(
    "120 Reef", "3 Palms East", "3 Palms West",
    "ANACAPA_EAST_FISH_CAMP_CEN", "ANACAPA_EAST_FISH_CAMP_E", "ANACAPA_EAST_FISH_CAMP_W",
    "ANACAPA_EAST_ISLE_CEN", "ANACAPA_EAST_ISLE_E", "ANACAPA_EAST_ISLE_W",
    "ANACAPA_LIGHTHOUSE_REEF_CEN", "ANACAPA_LIGHTHOUSE_REEF_E", "ANACAPA_LIGHTHOUSE_REEF_W",
    "ANACAPA_MIDDLE_ISLE_CEN", "ANACAPA_MIDDLE_ISLE_E", "ANACAPA_MIDDLE_ISLE_W",
    "ANACAPA_WEST_ISLE_CEN", "ANACAPA_WEST_ISLE_E", "ANACAPA_WEST_ISLE_W",
    "ARROYO_QUEMADO_E", "ARROYO_QUEMADO_W",
    "Abalone Cove Kelp West", "Albondigas", "Bunker Point", "Burial Grounds",
    "Cairns", "Cape Point", "Children's Pool", "Christmas Tree Cove",
    "Crystal Cove", "Dana Point", "Escondido West", "Flat Rock North",
    "Flat Rock South", "Golden Cove",
    "HORSESHOE_REEF_E", "HORSESHOE_REEF_W",
    "Hawthorne Reef", "Heisler Park", "Hermosa Beach AR - Giants", "Honeymoon Cove",
    "IV_REEF_E", "IV_REEF_W",
    "KOU Rock", "Laguna Beach", "Lechuza", "Leucadia",
    "Little Dume West", "Long Point East", "Long Point West", "Lunada Bay",
    "Marguerite Central", "Marguerite East", "Marguerite West",
    "Marina del Rey AR - F", "Marina del Rey AR - I", "Marina del Rey AR - K",
    "Matlahuayl",
    "NAPLES_CEN", "NAPLES_E", "NAPLES_W",
    "Nicholas Canyon West", "Old 18th", "Old Marineland",
    "PVR 2A", "PVR 2B", "PVR 2C",
    "PVR 4B", "PVR 4C", "PVR 4D",
    "PVR 5A", "PVR 5B", "PVR 5C",
    "PVR 6A", "PVR 6C", "PVR 6D",
    "PVR 7A", "PVR 7B", "PVR 7C",
    "PVR 8A", "PVR 8B", "PVR 8C",
    "Point Dume", "Point Fermin", "Point Loma Central", "Point Vicente West",
    "Portuguese Bend", "Portuguese Point", "Resort Point",
    "Ridges North", "Ridges South", "Rocky Point North", "Rocky Point South",
    "SBI - Cat Canyon", "SBI - Southeast Reef", "SBI - Southeast Sealion", "SBI - Sutil",
    "SCAI - Bird Rock", "SCAI - Blue Cavern", "SCAI - Cat Harbor", "SCAI - China Point",
    "SCAI - Hen Rock", "SCAI - Indian Rock", "SCAI - Iron Bound Cove",
    "SCAI - Italian Gardens", "SCAI - Ripper's Cove", "SCAI - Salta Verde", "SCAI - Ship Rock",
    "SCI_CAVERN_POINT_E", "SCI_CAVERN_POINT_W",
    "SCI_COCHE_POINT_E", "SCI_COCHE_POINT_W",
    "SCI_FORNEY_E", "SCI_FORNEY_W",
    "SCI_GULL_ISLE_E", "SCI_GULL_ISLE_W",
    "SCI_HAZARDS_CEN", "SCI_HAZARDS_E", "SCI_HAZARDS_W",
    "SCI_PAINTED_CAVE_CEN", "SCI_PAINTED_CAVE_E", "SCI_PAINTED_CAVE_W",
    "SCI_PELICAN_CEN", "SCI_PELICAN_E", "SCI_PELICAN_W",
    "SCI_SCORPION_E", "SCI_SCORPION_W",
    "SCI_VALLEY_CEN", "SCI_VALLEY_E", "SCI_VALLEY_W",
    "SCI_YELLOWBANKS_CEN", "SCI_YELLOWBANKS_W",
    "SCLI - Pyramid Cove",
    "SRI_CLUSTER_POINT_S", "SRI_JOHNSONS_LEE_SOUTH_E", "SRI_JOHNSONS_LEE_SOUTH_W",
    "SRI_SOUTH_POINT_W",
    "San Mateo Kelp",
    "Santa Monica AR - A", "Santa Monica AR - B", "Santa Monica AR - C",
    "Segovia", "South La Jolla", "Swami's", "Underwater Arch", "Whites Point"
  ),
  Latitude = c(
    33.73779, 33.71685, 33.71853,
    34.00158, 34.00450, 34.00308,
    34.01767, 34.01703, 34.01587,
    34.01278, 34.01390, 34.01078,
    34.01006, 34.00850, 34.00783,
    34.01698, 34.01608, 34.01742,
    34.46628, 34.46835,
    33.73857, 33.75030, 33.72491, 33.72389,
    33.71042, 33.72072, 32.85215, 33.76163,
    33.56395, 33.46631, 34.02122, 33.80147,
    33.79598, 33.75277,
    34.38945, 34.39173,
    33.74701, 33.54058, 33.85363, 33.76421,
    34.40295, 34.40473,
    33.72007, 33.53096, 34.03262, 33.06438,
    34.00488, 33.73528, 33.73727, 33.77106,
    33.75668, 33.75554, 33.75796,
    33.96760, 33.96856, 33.96897,
    32.85272,
    34.42185, 34.42333, 34.42463,
    34.04052, 33.72253, 33.73738,
    33.72340, 33.72278, 33.72254,
    33.72156, 33.72110, 33.72197,
    33.72157, 33.72099, 33.72046,
    33.72102, 33.71998, 33.72051,
    33.72040, 33.72019, 33.71962,
    33.71930, 33.71884, 33.71836,
    33.99855, 33.70563, 32.70985, 33.73983,
    33.72750, 33.73632, 33.76653,
    33.78818, 33.78578, 33.78074, 33.77587,
    33.46407, 33.46355, 33.46900, 33.46542,
    33.45315, 33.44807, 33.42583, 33.32994,
    33.39964, 33.46855, 33.44765,
    33.41064, 33.42829, 33.31467, 33.46354,
    34.05428, 34.05275,
    34.04493, 34.04265,
    34.05148, 34.05388,
    33.94647, 33.94817,
    34.05658, 34.05438, 34.05810,
    34.07287, 34.07190, 34.07448,
    34.03065, 34.02805, 34.03587,
    34.04847, 34.05250,
    33.98362, 33.98355, 33.98170,
    33.98853, 33.98930,
    32.81609,
    33.92383, 33.89743, 33.89513,
    33.89417,
    33.38480,
    34.00957, 34.00932, 34.00894,
    33.76236, 32.81285, 33.03583, 33.75162, 33.71470
  ),
  Longitude = c(
    -118.39231, -118.33262, -118.33534,
    -119.39807, -119.38578, -119.40417,
    -119.36368, -119.36113, -119.37173,
    -119.36313, -119.36000, -119.37250,
    -119.38778, -119.38817, -119.39447,
    -119.43292, -119.42183, -119.43807,
    -120.11697, -120.12518,
    -118.38731, -118.41627, -118.35376, -118.34768,
    -118.30747, -118.34047, -117.27887, -118.42183,
    -117.83607, -117.72175, -118.77342, -118.40739,
    -118.41365, -118.41493,
    -119.54477, -119.55770,
    -118.41745, -117.79210, -118.41295, -118.42529,
    -119.86452, -119.87628,
    -118.33944, -117.78024, -118.86284, -117.31063,
    -118.78990, -118.40164, -118.40332, -118.43169,
    -118.41834, -118.41709, -118.41918,
    -118.48667, -118.48578, -118.48744,
    -117.27038,
    -119.95150, -119.95102, -119.95532,
    -118.92273, -118.34414, -118.39501,
    -118.34996, -118.34978, -118.34910,
    -118.34678, -118.34674, -118.34721,
    -118.34582, -118.34540, -118.34526,
    -118.34388, -118.34387, -118.34391,
    -118.34240, -118.34245, -118.34231,
    -118.34009, -118.34057, -118.34039,
    -118.80710, -118.29799, -117.26707, -118.41380,
    -118.35756, -118.37551, -118.42774,
    -118.42397, -118.42594, -118.42977, -118.43229,
    -119.04496, -119.02878, -119.02850, -119.04859,
    -118.48748, -118.47928, -118.51139, -118.46950,
    -118.36650, -118.52428, -118.57549,
    -118.37550, -118.43532, -118.42200, -118.49187,
    -119.56687, -119.57130,
    -119.60140, -119.60400,
    -119.90967, -119.91820,
    -119.82318, -119.82795,
    -119.82117, -119.81935, -119.82483,
    -119.87098, -119.85755, -119.88213,
    -119.69665, -119.69092, -119.70230,
    -119.54637, -119.55525,
    -119.63840, -119.62032, -119.66370,
    -119.54698, -119.56493,
    -118.37092,
    -120.19200, -120.10038, -120.10432,
    -120.12488,
    -117.60813,
    -118.53036, -118.53123, -118.53088,
    -118.42352, -117.28653, -117.30076, -118.41704, -118.32300
  )
)

stopifnot(nrow(site_coords_df) == 145)

#Assigning each site to its nearest buoy
sites_sf <- site_coords_df %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

buoys_sf <- buoy_info %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

dist_matrix      <- st_distance(sites_sf, buoys_sf)
nearest_buoy_idx <- apply(dist_matrix, 1, which.min)

site_coords_df <- site_coords_df %>%
  mutate(
    nearest_buoy    = buoy_info$buoy_id[nearest_buoy_idx],
    nearest_buoy_nm = buoy_info$name[nearest_buoy_idx],
    dist_to_buoy_km = as.numeric(
      dist_matrix[cbind(seq_len(nrow(site_coords_df)), nearest_buoy_idx)]
    ) / 1000
  )

#Displaying nearest buoy assignment by site
site_coords_df %>%
  count(nearest_buoy, nearest_buoy_nm) %>%
  arrange(nearest_buoy) %>%
  print()

#Updating sites_sf with buoy assignment columns
sites_sf <- site_coords_df %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#Creating map showing which buoy each site is assigned to
ggplot() +
  geom_sf(data = scb_coast, fill = "tan", color = "grey40", linewidth = 0.3) +
  geom_point(data = site_coords_df,
             aes(x = Longitude, y = Latitude, color = nearest_buoy_nm),
             size = 2, alpha = 0.85) +
  geom_point(data = buoy_info,
             aes(x = lon, y = lat),
             shape = 17, size = 4, color = "black") +
  geom_text(data = buoy_info,
            aes(x = lon, y = lat, label = buoy_id),
            vjust = -0.9, size = 2.8, fontface = "bold") +
  scale_color_brewer(palette = "Set1", name = "Assigned Buoy") +
  coord_sf(xlim = c(-121.5, -116.5), ylim = c(32.0, 35.0)) +
  labs(title = "Survey Sites by Nearest NDBC Buoy",
       subtitle = "Triangles = buoy locations",
       x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7))

#Saving buoy assignments map
ggsave("site_buoy_assignments.png", width = 10, height = 7, dpi = 300)

#Calculating fetch using sf
calculate_fetch <- function(pt, bearings, shoreline, dmax = 500000) {
  coords <- st_coordinates(pt)
  
  sapply(bearings, function(bearing) {
    bearing_rad <- bearing * pi / 180
    end_x <- coords[1] + dmax * sin(bearing_rad)
    end_y <- coords[2] + dmax * cos(bearing_rad)
    
    ray <- st_sfc(
      st_linestring(matrix(c(coords[1], coords[2], end_x, end_y),
                           ncol = 2, byrow = TRUE)),
      crs = st_crs(shoreline)
    )
    
    intersection <- tryCatch(
      st_intersection(ray, st_geometry(shoreline)),
      error = function(e) NULL
    )
    
    if (is.null(intersection) || length(intersection) == 0 ||
        all(st_is_empty(intersection))) {
      return(dmax)
    }
    
    pts <- st_cast(intersection, "POINT")
    if (length(pts) == 0) return(dmax)
    
    origin <- st_sfc(st_point(coords), crs = st_crs(shoreline))
    min(as.numeric(st_distance(origin, pts)))
  })
}

#Calculating fetch
utm_crs   <- 32611
sites_utm <- st_transform(sites_sf, utm_crs)
coast_utm <- st_transform(scb_coast, utm_crs)

message("Calculating fetch for ", nrow(sites_utm), " sites across ",
        length(bearings), " bearings...")

fetch_list <- lapply(seq_len(nrow(sites_utm)), function(i) {
  if (i %% 10 == 0) message("  Site ", i, " of ", nrow(sites_utm))
  calculate_fetch(sites_utm[i, ], bearings, coast_utm, dmax = 500000)
})

fetch_matrix <- do.call(rbind, fetch_list)
rownames(fetch_matrix) <- site_coords_df$Site
colnames(fetch_matrix) <- paste0("fetch_", bearings)

#Computing exposure using per-buoy wind weights
mean_fetch_m <- rowMeans(fetch_matrix, na.rm = TRUE)
max_fetch_m  <- apply(fetch_matrix, 1, max, na.rm = TRUE)

#Using wind climatology of nearest buoy for each site
weighted_fetch_m <- sapply(seq_len(nrow(site_coords_df)), function(i) {
  bid <- site_coords_df$nearest_buoy[i]
  w   <- buoy_wind_weights[[bid]][as.character(bearings)]
  sum(fetch_matrix[i, ] * w, na.rm = TRUE)
})

#Computing wave energy proxy
wave_energy_kW <- apply(fetch_matrix, 1, function(row) {
  mean(sapply(row, function(f) {
    tryCatch(waver::wave_energy(wind = 5.0, fetch = f, depth = 20),
             error = function(e) NA_real_)
  }), na.rm = TRUE)
})

#Compiling exposure results
exposure_results <- site_coords_df %>%
  mutate(
    mean_fetch_km         = mean_fetch_m / 1000,
    max_fetch_km          = max_fetch_m / 1000,
    weighted_fetch_km     = weighted_fetch_m / 1000,
    wave_energy_kW_m      = wave_energy_kW,
    exposure_index_scaled = (weighted_fetch_km - min(weighted_fetch_km)) /
      (max(weighted_fetch_km) - min(weighted_fetch_km))
  )

#Displaying exposure results
print(exposure_results %>%
        dplyr::select(Site, nearest_buoy, weighted_fetch_km,
                      exposure_index_scaled) %>%
        arrange(desc(weighted_fetch_km)))

#Saving wave exposure index
write_csv(exposure_results, "wave_exposure_index_by_site.csv")

#Creating exposure map
ggplot() +
  geom_sf(data = scb_coast, fill = "tan", color = "grey40", linewidth = 0.3) +
  geom_point(data = exposure_results,
             aes(x = Longitude, y = Latitude,
                 color = weighted_fetch_km, size = weighted_fetch_km),
             alpha = 0.85) +
  geom_point(data = buoy_info, aes(x = lon, y = lat),
             shape = 17, size = 3.5, color = "white") +
  scale_color_viridis_c(name = "Weighted\nFetch (km)", option = "plasma") +
  scale_size_continuous(name = "Weighted\nFetch (km)", range = c(1.5, 6)) +
  coord_sf(xlim = c(-121.5, -116.5), ylim = c(32.0, 35.0)) +
  labs(title = "Wave Exposure Index — SCB Gorgonian Sites",
       subtitle = "Per-site wind weights from nearest NDBC buoy (2014-2024) | Triangles = buoys",
       x = "Longitude", y = "Latitude") +
  theme_bw()

#Saving wave exposure map
ggsave("wave_exposure_map.png", width = 11, height = 7, dpi = 300)

#Creating ranked bar chart colored by buoy zone
exposure_results %>%
  arrange(weighted_fetch_km) %>%
  mutate(Site = factor(Site, levels = Site)) %>%
  ggplot(aes(x = Site, y = weighted_fetch_km, fill = nearest_buoy_nm)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1", name = "Buoy Zone") +
  coord_flip() +
  labs(title = "Wave Exposure by Site (Ranked)",
       subtitle = "Fill = which buoy's wind climatology was applied",
       x = NULL, y = "Wind-Weighted Fetch (km)") +
  theme_bw(base_size = 7) +
  theme(legend.position = "bottom")

#Saving bar chart
ggsave("wave_exposure_ranked.png", width = 9, height = 16, dpi = 300)

#Merging wave exposure index into gorgonian dataset
dataset_path <- "C:/Users/bethm/Documents/Manuscript Work/Datasets/Unified_VRG_PISCO_gorgonian_dataset.csv"
output_path  <- "C:/Users/bethm/Documents/Manuscript Work/Datasets/Unified_VRG_PISCO_gorgonian_dataset_with_exposure.csv"

if (file.exists(dataset_path)) {
  gorg_data <- read_csv(dataset_path, show_col_types = FALSE)
  
  gorg_data_with_exposure <- gorg_data %>%
    left_join(
      exposure_results %>%
        dplyr::select(Site, nearest_buoy, nearest_buoy_nm,
                      mean_fetch_km, max_fetch_km,
                      weighted_fetch_km, wave_energy_kW_m,
                      exposure_index_scaled),
      by = "Site"
    )
  
  write_csv(gorg_data_with_exposure, output_path)
  message("Done! Saved to: ", output_path)
  
} else {
  message("Dataset file not found: ", dataset_path)
}