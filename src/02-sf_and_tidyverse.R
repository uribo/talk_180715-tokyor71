##############################
# R script in the slide.
##############################
library(tidyverse)
source(here::here("src", "01-introduction_sf.R"))
# devtools::install_git("https://gitlab.com/uribo/jmastats")
library(jmastats) # Under Development
# library(lwgeom)

# webshot::webshot("http://www.data.jma.go.jp/obd/stats/etrn/index.php",
#                  file = "inst/jma_stats_top.png")
# webshot::webshot("http://www.data.jma.go.jp/obd/stats/etrn/view/hourly_a1.php?prec_no=74&block_no=0746&year=2018&month=7&day=6&view=",
#                  file = "inst/jma_stats_view.png")

# Base map -----------------------------------------------------------------
# Collect West Japan Polygon with purrr
# Provided by National Spatial Planning and Regional Policy Bureau, MILT of Japan
sf_west_japan <- 
  26:46 %>%
  map(jpn_pref, district = FALSE) %>% 
  # Unused map_dfr() ... bind_rows() not support sf class
  reduce(rbind)

format(object.size(sf_west_japan), units = "MB")

# Area cropped and simplified polygon
sf_west_japan <- 
  st_crop(
    sf_west_japan,
    st_bbox(c(xmin = 128.5, ymin = 31.0, xmax = 136.5, ymax = 36.5))  
  ) %>% 
  # Reduce object size
  st_simplify(preserveTopology = TRUE, dTolerance = 0.005)

format(object.size(sf_west_japan), units = "MB")
plot(st_geometry(sf_west_japan))
# readr::write_rds(sf_west_japan, here::here("inst", "sf_west_japan.rds"))

p_out <- 
  ggplot(sf_west_japan) +
  geom_sf()

ggsave(here::here("inst", "west_japan.png"), 
       p_out,
       width = 6, height = 5, dpi = "retina")

sf_west_japan <- 
  sf_west_japan %>% 
  mutate(
    # Convert prefecture name to Roman Katakana
    prefecture = dplyr::recode(prefecture,
                                    `京都府` = "Kyoto",
                                    `大阪府` = "Osaka", 
                                    `兵庫県` = "Hyogo", 
                                    `奈良県` = "Nara", 
                                    `和歌山県` = "Wakayama", 
                                    `鳥取県` = "Tottori", 
                                    `島根県` = "Shimane",
                                    `岡山県` = "Okayama", 
                                    `広島県` = "Hiroshima", 
                                    `山口県` = "Yamaguchi", 
                                    `徳島県` = "Tokushima", 
                                    `香川県` = "Kagawa", 
                                    `愛媛県` = "Ehime",
                                    `高知県` = "Kochi", 
                                    `福岡県` = "Fukuoka", 
                                    `佐賀県` = "Saga", 
                                    `長崎県` = "Nagasaki", 
                                    `熊本県` = "Kumamoto", 
                                    `大分県` = "Oita", 
                                    `宮崎県` = "Miyazaki",
                                    `鹿児島県` = "Kagoshima"
  ),
  severe_type = dplyr::case_when(
    prefecture %in% c("Nagasaki", "Fukuoka", "Saga",
                      "Hiroshima", "Okayama", "Tottori",
                      "Kyoto", "Hyogo", "Gifu", "Kochi",
                      "Ehime") ~ "emergency warning",
    prefecture %in% c("Wakayama", "Nara", "Shimane",
                      "Tokushima", "Kagawa", "Yamaguchi",
                      "Kumamoto",
                      "Osaka", "Oita", "Miyazaki", "Kagoshima") ~ "warning"
  ) 
  )

sf_west_japan <- 
  sf_west_japan %>% 
  st_transform(crs = "+proj=laea +lat_0=30 +lon_0=140")

base_map <- 
  ggplot(sf_west_japan) +
  geom_sf(aes(fill = severe_type)) +
  scale_fill_manual(values = c("emergency warning" = "#C800FF",
                               "warning" = "#FF2800")) +
  theme(legend.position = "bottom")

df_west_japan <- 
  sf_west_japan %>% 
  mutate(longitude = st_coordinates(st_centroid(geometry))[, 1],
         latitude = st_coordinates(st_centroid(geometry))[, 2]) %>% 
  st_set_geometry(NULL)

p_out <- 
  base_map + 
  ggrepel::geom_label_repel(data = df_west_japan, 
            aes(x = longitude,
                y = latitude,
                label = prefecture)) +
  coord_sf()

ggsave(here::here("inst", "west_japan2.png"), 
       p_out,
       width = 6, height = 5, dpi = "retina")

# Collect weather data ----------------------------------------------------
# jmastats:::jma_url("hourly", block_no = "0746", year = 2018, month = 7, day = 6) %>% 
#   magrittr::use_series(url) %>% 
#   browseURL()
tgt_stations <- 
  stations %>% 
  st_transform(crs = "+proj=laea +lat_0=30 +lon_0=140") %>% 
  st_cast("POINT") %>% 
  select(area, station_type, station_name, elevation, block_no, pref_code) %>% 
  distinct(block_no, .keep_all = TRUE) %>% 
  filter(pref_code %in% 26:46) %>% 
  mutate(in_area = purrr::pmap_lgl(., ~ 
                                 as.logical(sum(st_within(..7, sf_west_japan, sparse = FALSE))))) %>% 
  filter(in_area == TRUE)

check_data <- function(block_no) {
  
  block_no <- rlang::enquo(block_no)
  
  jmastats:::jma_url("hourly", !! block_no, 2017, 1, 14) %>% 
    magrittr::use_series(url) %>% 
    xml2::read_html() %>% 
    rvest::html_table(fill = TRUE) %>% 
    length()  
}

tgt_stations <- 
  tgt_stations %>% 
  mutate(check = purrr::pmap_int(., ~ check_data(block_no = ..5))) %>% 
  filter(check > 0)

# readr::write_rds(tgt_stations, here::here("inst", "target_stations363.rds"))

df_weather <- 
  list(
  block_no = rep(tgt_stations$block_no, each = 3),
  year = 2018,
  month = 7,
  day = rep(5:7, times = nrow(tgt_stations))
) %>% 
  pmap(~ jma_collect(item = "hourly",
                     block_no = ..1,
                     year = ..2,
                     month = ..3,
                     day = ..4) %>% 
         select(time, `precipitation_(mm)`) %>% 
         parse_unit() %>% 
           transmute(block_no = c(..1),
                     date = lubridate::make_datetime(..2, ..3, ..4, hour = time),
                     precipitation_mm)) %>% 
  reduce(rbind) # keep units attributes

df_weather <- 
  df_weather %>% 
  mutate(precipitation_mm = units::drop_units(precipitation_mm)) %>% 
  readr::type_convert() %>% 
  mutate(precipitation_mm = units::set_units(precipitation_mm, mm))

# readr::write_rds(df_weather, here::here("inst", "precipitation_180705-180707_72hours.rds"))

df_precipitation <- 
  df_weather %>% 
  group_by(block_no) %>% 
  summarise(precipitation = sum(precipitation_mm)) 

df_precipitation_top10 <- 
  df_precipitation %>% 
  top_n(10, wt = precipitation)

library(ggforce)
ggplot(df_precipitation_top10, 
       aes(block_no, precipitation)) +
  geom_bar(stat = "identity")

ggplot(df_precipitation_top10, 
       aes(forcats::fct_reorder(block_no, units::drop_units(precipitation)), 
           precipitation)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 81) +
  labs(x = "Station ID", 
       title = "Top 10 stations for 72 hours precipitation",
       subtitle = "Horizontal lines (81 mm) shows monthly precipitation at Tokyo last year",
       caption = "Source: Japan Meteorological Agency Data.\n
       Modified: Shinya Uryu")


# Animation map -----------------------------------------------------------
# remotes::install_github("thomasp85/gganimate")
library(gganimate)

prep_breaks <-
  c(1, 5, 10, 20, 30, 50, 80)
jma_pal <-
  rev(purrr::pmap_chr(
    list(r = c(180, 255, 255, 250,   0, 33,  160, 242),
         g = c(0,    40, 153, 245,  65, 140, 210, 242),
         b = c(104,   0,   0,   0, 255, 240, 255, 255)),
    ~ rgb(..1, ..2, ..3, max = 255)
  ))

p_out <- 
  ggplot(df_weather %>% 
           left_join(tgt_stations, by = "block_no") %>% 
           mutate(longitude = st_coordinates(geometry)[, 1],
                  latitude = st_coordinates(geometry)[, 2])) +
  geom_sf(data = sf_west_japan) +
  geom_linerange(aes(longitude,
                     ymin = latitude, 
                     ymax = latitude + 
                       units::drop_units(precipitation_mm) * 300,
                     alpha = 0.6,
                     color = units::drop_units(precipitation_mm)),
                 size = 2,
                 linetype = 1,
                 show.legend = TRUE) +
  scale_color_gradientn(colours = jma_pal,
                        breaks  = prep_breaks) +
  guides(alpha = FALSE, fill = FALSE, 
         color = guide_colorbar(title = "Precipitation per hour [mm]")) +
  labs(title = "Time: {frame_time}") + 
  transition_time(
    date
  )

# magick::image_write(animate(p_out, nframes = 200), 
#                     here::here("inst", "map_precipitation.mp4"))

ggplot(df_precipitation_top10 %>% 
         left_join(tgt_stations, by = "block_no") %>% 
         mutate(longitude = st_coordinates(geometry)[, 1],
                latitude = st_coordinates(geometry)[, 2])) +
  geom_sf(data = sf_west_japan) +
  geom_linerange(aes(longitude,
                     ymin = latitude, 
                     ymax = latitude + 
                       units::drop_units(precipitation) * 100,
                     alpha = 0.6),
                 size = 2,
                 linetype = 1,
                 show.legend = TRUE)


d <- 
  df_weather %>% 
  mutate(precipitation_mm = units::drop_units(precipitation_mm)) %>% 
  readr::type_convert() %>% 
  filter(block_no %in% df_precipitation_top10$block_no) %>% 
  left_join(tgt_stations %>% 
              select(block_no, station_name), by = "block_no")

p_animate <- 
  ggplot(d, 
       aes(station_name, precipitation_mm)) +
  geom_bar(stat = "identity", aes(fill = precipitation_mm)) +
  scale_fill_gradientn(colours = jma_pal,
                         breaks  = prep_breaks) +
  labs(title = "時刻: {frame_time}", 
       x = "観測所",
       y = "1時間あたりの降水量 [mm]") +
  transition_time(
    date
  )

# Levee failures in Mabi ---------------------------------------------------------------------
library(stars)
library(raster)

mesh_crop <- function(raster, mesh) {
  raster::crop(
    raster,
    raster::extent(mesh %>% 
                     sf::st_bbox() %>% 
                     as.vector() %>% 
                     .[c(1, 3, 2, 4)] %>% 
                     purrr::set_names(c("xmin", "xmax", "ymin", "ymax"))),
    snap = "in"
  )  
}

r <- 
  raster(here::here("inst", "N034E133_AVE_DSM.tif")) %>% 
  mesh_crop(c(51337565, 51337566, 51337567, 51337568,
              51337555, 51337556, 51337557, 51337558,
              51337545, 51337546, 51337547, 51337548) %>% 
              jpmesh::export_meshes())

r_df <- 
  r %>% 
  st_as_stars() %>% 
  st_as_sf(as_points = FALSE) %>% 
  magrittr::set_colnames(c("elevation", "geometry"))

# remotes::install_github("thomasp85/scico")
p_out <- 
  ggplot(r_df) +
  geom_sf(aes(fill = elevation), color = "transparent") +
  coord_sf() +
  theme(legend.position = "bottom") +
  scico::scale_fill_scico(palette = "vik") +
  labs(title = "Height above sea level along the Takahashi River.",
       caption = "Source: ALOS World 3D (AW3D30 Version 2.1) \u00a9 JAXA")

# bbb ---------------------------------------------------------------------
sf_river_station <- 
  data_frame(
    station = c("Higashiminari", "Hiwa", "Sakazu"),
    geometry = st_sfc(st_point(c(133.626944, 34.615278)),
                      st_point(c(133.681111, 34.715000)),
                      st_point(c(133.745000, 34.608333)), 
                      crs = 4326)
  ) %>% 
  st_sf()

mapview(jpn_cities(jis_code = c("33202", "33208", "33461")),
        map.types = mapviewGetOption("basemaps")[4]) +
  mapview(sf_river_station,
          layer.name = "Stations") +
  mapview(st_union(r_df), 
          col.region = "red",
          layer.name = "Levee failures area")

df_higashiminari <- 
  tibble::frame_data(
    ~date, ~time, ~`water_level(m)`,
    "2018/07/05", "01:00:00", 1.23,
    "2018/07/05", "02:00:00", 1.23,
    "2018/07/05", "03:00:00", 1.23,
    "2018/07/05", "04:00:00", 1.22,
    "2018/07/05", "05:00:00", 1.21,
    "2018/07/05", "06:00:00", 1.20,
    "2018/07/05", "07:00:00", 1.19,
    "2018/07/05", "08:00:00", 1.19,
    "2018/07/05", "09:00:00", 1.20,
    "2018/07/05", "10:00:00", 1.22,
    "2018/07/05", "11:00:00", 1.26,
    "2018/07/05", "12:00:00", 1.28,
    "2018/07/05", "13:00:00", 1.31,
    "2018/07/05", "14:00:00", 1.35,
    "2018/07/05", "15:00:00", 1.42,
    "2018/07/05", "16:00:00", 1.51,
    "2018/07/05", "17:00:00", 1.59,
    "2018/07/05", "18:00:00", 1.48,
    "2018/07/05", "19:00:00", 1.71,
    "2018/07/05", "20:00:00", 2.28,
    "2018/07/05", "21:00:00", 2.95,
    "2018/07/05", "22:00:00", 3.47,
    "2018/07/05", "23:00:00", 3.93,
    "2018/07/05", "24:00:00", 4.27,
    "2018/07/06", "01:00:00", 4.36,
    "2018/07/06", "02:00:00", 4.31,
    "2018/07/06", "03:00:00", 4.16,
    "2018/07/06", "04:00:00", 3.94,
    "2018/07/06", "05:00:00", 3.72,
    "2018/07/06", "06:00:00", 3.51,
    "2018/07/06", "07:00:00", 3.32,
    "2018/07/06", "08:00:00", 3.22,
    "2018/07/06", "09:00:00", 3.14,
    "2018/07/06", "10:00:00", 3.12,
    "2018/07/06", "11:00:00", 3.16,
    "2018/07/06", "12:00:00", 3.20,
    "2018/07/06", "13:00:00", 3.34,
    "2018/07/06", "14:00:00", 3.46,
    "2018/07/06", "15:00:00", 3.49,
    "2018/07/06", "16:00:00", 3.50,
    "2018/07/06", "17:00:00", 3.46,
    "2018/07/06", "18:00:00", 3.41,
    "2018/07/06", "19:00:00", 3.49,
    "2018/07/06", "20:00:00", 3.78,
    "2018/07/06", "21:00:00", 4.32,
    "2018/07/06", "22:00:00", 5.26,
    "2018/07/06", "23:00:00", 6.50,
    "2018/07/06", "24:00:00", 7.36,
    "2018/07/07", "01:00:00", 7.47,
    "2018/07/07", "02:00:00", 7.53,
    "2018/07/07", "03:00:00", 7.53,
    "2018/07/07", "04:00:00", 7.36,
    "2018/07/07", "05:00:00", 7.13,
    "2018/07/07", "06:00:00", 6.91,
    "2018/07/07", "07:00:00", 6.76,
    "2018/07/07", "08:00:00", 6.77,
    "2018/07/07", "09:00:00", 6.91,
    "2018/07/07", "10:00:00", 6.99,
    "2018/07/07", "11:00:00", 6.96,
    "2018/07/07", "12:00:00", 6.87,
    "2018/07/07", "13:00:00", 6.72,
    "2018/07/07", "14:00:00", 6.50,
    "2018/07/07", "15:00:00", 6.24,
    "2018/07/07", "16:00:00", 5.96,
    "2018/07/07", "17:00:00", 5.70,
    "2018/07/07", "18:00:00", 5.44,
    "2018/07/07", "19:00:00", 5.20,
    "2018/07/07", "20:00:00", 4.98,
    "2018/07/07", "21:00:00", 4.75,
    "2018/07/07", "22:00:00", 4.51,
    "2018/07/07", "23:00:00", 4.27,
    "2018/07/07", "24:00:00", 4.03
  ) %>% 
  mutate(station = "Higashiminari",
         max_water_level = 5.81)

df_sakadu <- 
  tibble::frame_data(
    ~date, ~time, ~`water_level(m)`,
    "2018/07/05", "01:00:00", 3.45,
    "2018/07/05", "02:00:00", 3.44,
    "2018/07/05", "03:00:00", 3.42,
    "2018/07/05", "04:00:00", 3.41,
    "2018/07/05", "05:00:00", 3.40,
    "2018/07/05", "06:00:00", 3.40,
    "2018/07/05", "07:00:00", 3.39,
    "2018/07/05", "08:00:00", 3.37,
    "2018/07/05", "09:00:00", 3.36,
    "2018/07/05", "10:00:00", 3.42,
    "2018/07/05", "11:00:00", 3.41,
    "2018/07/05", "12:00:00", 3.42,
    "2018/07/05", "13:00:00", 3.44,
    "2018/07/05", "14:00:00", 3.49,
    "2018/07/05", "15:00:00", 3.55,
    "2018/07/05", "16:00:00", 3.62,
    "2018/07/05", "17:00:00", 3.71,
    "2018/07/05", "18:00:00", 3.97,
    "2018/07/05", "19:00:00", 4.33,
    "2018/07/05", "20:00:00", 4.70,
    "2018/07/05", "21:00:00", 5.34,
    "2018/07/05", "22:00:00", 6.55,
    "2018/07/05", "23:00:00", 7.66,
    "2018/07/05", "24:00:00", 8.39,
    "2018/07/06", "01:00:00", 8.73,
    "2018/07/06", "02:00:00", 8.80,
    "2018/07/06", "03:00:00", 8.72,
    "2018/07/06", "04:00:00", 8.54,
    "2018/07/06", "05:00:00", 8.28,
    "2018/07/06", "06:00:00", 8.00,
    "2018/07/06", "07:00:00", 7.75,
    "2018/07/06", "08:00:00", 7.53,
    "2018/07/06", "09:00:00", 7.35,
    "2018/07/06", "10:00:00", 7.20,
    "2018/07/06", "11:00:00", 7.04,
    "2018/07/06", "12:00:00", 6.95,
    "2018/07/06", "13:00:00", 6.93,
    "2018/07/06", "14:00:00", 6.94,
    "2018/07/06", "15:00:00", 6.97,
    "2018/07/06", "16:00:00", 7.00,
    "2018/07/06", "17:00:00", 7.02,
    "2018/07/06", "18:00:00", 7.03,
    "2018/07/06", "19:00:00", 7.06,
    "2018/07/06", "20:00:00", 7.39,
    "2018/07/06", "21:00:00", 8.22,
    "2018/07/06", "22:00:00", 9.36,
    "2018/07/06", "23:00:00", 10.48,
    "2018/07/06", "24:00:00", 11.51,
    "2018/07/07", "01:00:00", 12.15,
    "2018/07/07", "02:00:00", 12.35,
    "2018/07/07", "03:00:00", 12.31,
    "2018/07/07", "04:00:00", 12.02,
    "2018/07/07", "05:00:00", 11.58,
    "2018/07/07", "06:00:00", 11.22,
    "2018/07/07", "07:00:00", 11.04,
    "2018/07/07", "08:00:00", 11.01,
    "2018/07/07", "09:00:00", 11.07,
    "2018/07/07", "10:00:00", 11.19,
    "2018/07/07", "11:00:00", 11.29,
    "2018/07/07", "12:00:00", 11.31,
    "2018/07/07", "13:00:00", 11.19,
    "2018/07/07", "14:00:00", 10.97,
    "2018/07/07", "15:00:00", 10.73,
    "2018/07/07", "16:00:00", 10.44,
    "2018/07/07", "17:00:00", 10.15,
    "2018/07/07", "18:00:00", 9.82,
    "2018/07/07", "19:00:00", 9.48,
    "2018/07/07", "20:00:00", 9.19,
    "2018/07/07", "21:00:00", 8.93,
    "2018/07/07", "22:00:00", 8.71,
    "2018/07/07", "23:00:00", 8.51,
    "2018/07/07", "24:00:00", 8.31
  ) %>% 
  mutate(station = "Sakadu",
         max_water_level = 10.25)

df_hiwa <- 
  tibble::frame_data(
    ~date, ~time, ~`water_level(m)`,
    "2018/07/05", "01:00:00", 3.35,
    "2018/07/05", "02:00:00", 3.34,
    "2018/07/05", "03:00:00", 3.33,
    "2018/07/05", "04:00:00", 3.32,
    "2018/07/05", "05:00:00", 3.32,
    "2018/07/05", "06:00:00", 3.32,
    "2018/07/05", "07:00:00", 3.32,
    "2018/07/05", "08:00:00", 3.32,
    "2018/07/05", "09:00:00", 3.32,
    "2018/07/05", "10:00:00", 3.32,
    "2018/07/05", "11:00:00", 3.32,
    "2018/07/05", "12:00:00", 3.32,
    "2018/07/05", "13:00:00", 3.35,
    "2018/07/05", "14:00:00", 3.39,
    "2018/07/05", "15:00:00", 3.47,
    "2018/07/05", "16:00:00", 3.59,
    "2018/07/05", "17:00:00", 3.82,
    "2018/07/05", "18:00:00", 5.00,
    "2018/07/05", "19:00:00", 5.94,
    "2018/07/05", "20:00:00", 7.03,
    "2018/07/05", "21:00:00", 8.43,
    "2018/07/05", "22:00:00", 9.08,
    "2018/07/05", "23:00:00", 9.08,
    "2018/07/05", "24:00:00", 8.90,
    "2018/07/06", "01:00:00", 8.66,
    "2018/07/06", "02:00:00", 8.31,
    "2018/07/06", "03:00:00", 7.93,
    "2018/07/06", "04:00:00", 7.56,
    "2018/07/06", "05:00:00", 7.31,
    "2018/07/06", "06:00:00", 7.11,
    "2018/07/06", "07:00:00", 6.93,
    "2018/07/06", "08:00:00", 6.71,
    "2018/07/06", "09:00:00", 6.58,
    "2018/07/06", "10:00:00", 6.46,
    "2018/07/06", "11:00:00", 6.44,
    "2018/07/06", "12:00:00", 6.45,
    "2018/07/06", "13:00:00", 6.50,
    "2018/07/06", "14:00:00", 6.53,
    "2018/07/06", "15:00:00", 6.54,
    "2018/07/06", "16:00:00", 6.56,
    "2018/07/06", "17:00:00", 6.59,
    "2018/07/06", "18:00:00", 6.98,
    "2018/07/06", "19:00:00", 8.33,
    "2018/07/06", "20:00:00", 9.55,
    "2018/07/06", "21:00:00", 10.74,
    "2018/07/06", "22:00:00", 11.62,
    "2018/07/06", "23:00:00", 12.40,
    "2018/07/06", "24:00:00", NA_real_,
    "2018/07/07", "01:00:00", NA_real_,
    "2018/07/07", "02:00:00", NA_real_,
    "2018/07/07", "03:00:00", NA_real_,
    "2018/07/07", "04:00:00", 11.99,
    "2018/07/07", "05:00:00", 11.52,
    "2018/07/07", "06:00:00", 11.29,
    "2018/07/07", "07:00:00", 11.31,
    "2018/07/07", "08:00:00", 11.54,
    "2018/07/07", "09:00:00", 11.79,
    "2018/07/07", "10:00:00", 11.98,
    "2018/07/07", "11:00:00", 11.91,
    "2018/07/07", "12:00:00", 11.60,
    "2018/07/07", "13:00:00", 11.18,
    "2018/07/07", "14:00:00", 10.71,
    "2018/07/07", "15:00:00", 10.23,
    "2018/07/07", "16:00:00", 9.68,
    "2018/07/07", "17:00:00", 9.06,
    "2018/07/07", "18:00:00", 8.61,
    "2018/07/07", "19:00:00", 8.23,
    "2018/07/07", "20:00:00", 7.96,
    "2018/07/07", "21:00:00", 7.73,
    "2018/07/07", "22:00:00", 7.52,
    "2018/07/07", "23:00:00", 7.21,
    "2018/07/07", "24:00:00", 6.94
  ) %>% 
  mutate(station = "Hiwa",
         max_water_level = 10.30)

df_river_water_level <- 
  bind_rows(
    df_higashiminari,
    df_hiwa,
    df_sakadu
  ) %>% 
  mutate(datetime = lubridate::as_datetime(paste0(date, time)))

gg_waterlevel_ts <- function(data) {
  ggplot(data, aes(datetime, `water_level(m)`)) +
    geom_point(aes(color = dplyr::if_else(`water_level(m)` > max_water_level, "red", "blue")),
               size = 2) +
    scale_color_identity() +
    geom_line(data = data %>% rename(x2 = datetime), aes(x2, `water_level(m)`, group = 1)) +
    geom_hline(data = data, aes(yintercept = max_water_level), linetype = 2) +
    geom_text(data = data, aes(x = min(datetime) + 3600*30,
                               y = max_water_level + 0.3),
              label = "historical maximum water level",
              position = "identity") +
    transition_time(datetime) +
    guides(color = FALSE) +
    labs(title = paste("Time Series of water level at",
                       data$station[1],
                       "of Takahashi Valley"),
         subtitle = "Time: {frame_time}",
         caption = "Source: Water Information Systems \u00a9 MLIT",
         x = "Date",
         y = "Water level (m)") +
    scale_x_datetime(date_labels = "%b/%d", date_minor_breaks = "1 day")# +
  #facet_wrap(~ station, ncol = 1, scales = "free_y")
}

# gg_waterlevel_ts(df_river_water_level %>% 
#                    filter(station == "Higashiminari"))
# magick::image_write(animate(last_plot(), nframes = 80),
#                     here::here("inst", "ts_water_level_station307111287708160_higashiminari.gif"))
# gg_waterlevel_ts(df_river_water_level %>% 
#                    filter(station == "Hiwa"))
# magick::image_write(animate(last_plot(), nframes = 80),
#                     here::here("inst", "ts_water_level_station307111287708020_hiwa.gif"))
# gg_waterlevel_ts(df_river_water_level %>% 
#                    filter(station == "Sakadu"))
# magick::image_write(animate(last_plot(), nframes = 80),
#                     here::here("inst", "ts_water_level_station307111287708050_sakadu.gif"))



