##############################
# R script in the slide.
##############################
library(sf)
library(jpndistrict)

# webshot::webshot("https://github.com/r-spatial",
#                  file = "inst/r_spatial.png")
# Simple feature ----------------------------------------------------------
st_point(c(1, 2)) # X, Y Coordinates
st_point(c(1, 2, 3))

st_linestring(matrix(1:4, ncol = 2))

st_polygon(list(
  rbind(
    st_point(c(1, 1)),
    st_point(c(2, 1)),
    st_point(c(2, 2)),
    st_point(c(1, 2)),
    st_point(c(1 ,1)))))


# 3 types of the sf object ------------------------------------------------
(sf_pref33 <- 
  jpn_pref(33))

class(sf_pref33)
class(sf_pref33$geometry)
class(sf_pref33$geometry[[1]])

(x <- st_point(c(1, 2)))
x %>% 
  st_sfc()
x %>% 
  st_sfc() %>% 
  st_sf()


# I/O ---------------------------------------------------------------------
# st_read(system.file("shape/nc.shp", package = "sf"))
# st_write(nc, "nc.shp")

# Geo-processing like a PostGIS -------------------------------------------
st_area(sf_pref33)
st_bbox(sf_pref33)
st_distance(sf_pref33$geometry[1],
            sf_pref33$geometry[12])


# plot.sf*(): S3 method for sf --------------------------------------------
# Drawing all fields patterns
plot(sf_pref33)

# Drawing specific field
plot(sf_pref33["city"], 
     key.pos = 3)

# Extract only sfc from sf geometry list-column
plot(st_geometry(sf_pref33), 
     col = "white")


# mapview package ---------------------------------------------------------
library(mapview)

# Okayama City Administration
x <- 
  st_sfc(
    st_point(c(133.9196, 34.65511)), 
    crs = 4326)

mapview(sf_pref33,
        map.types = mapviewGetOption("basemaps")[3]) +
  mapview(x,
          col.regions = "red")
