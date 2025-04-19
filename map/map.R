library(tidyverse)
library(sf)


partners <- data.frame(Partner = c("1", "2", "3", "4", "5", "6", "7", "8", "9","10"),
                Latitude = c("50.905235", "49.9847", "50.7292847", "48.2233187", "50.1086989", 
                             "51.4425624","51.0322072", "51.4375832", "51.3206", "51.0322072"),
                Longitude = c("6.4042735", "7.96835", "7.0731143", "11.592294", "11.4510019", 
                              "6.266977","6.6134963", "6.3253844", "6.4951", "6.6134963")
                )


# !reading the shapefile (the map)
# shapefiles are from eurostat, there are different versions, here i am using the version of 2021
# Source: https://ec.europa.eu/eurostat/de/web/gisco/geodata/statistical-units/territorial-units-statistics

shape <- read_sf(dsn = "map/German_Geo/NUTS_RG_20M_2021_3035.shp")
shape <- shape %>% dplyr::filter(CNTR_CODE == "DE",LEVL_CODE == 3)
names(shape)[4] <- "kreis"

# !transforming the soil point crs into the shapefile crs
maps <- partners %>% st_as_sf(coords = c("Longitude", "Latitude"), 
                               crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_transform(st_crs(shape))

# !checking if the points are inside the polygons of our Nuts3 Shapefile
partner_maps <- st_join(maps, shape)

# !filtering only points that are within NRW & Hessen
# NUTS code for NRW is DEA, Hessen is DEB
shape_NRW <- filter(shape, str_detect(NUTS_ID, "^DEA"))
shape_HS <- filter(shape, str_detect(NUTS_ID, "^DEB"))


# Ensure `shape_NRW` & `shape_HS` is only at the state level by merging county boundaries
Boundary_NRW <- shape_NRW %>%
  summarise(geometry = st_union(geometry))  # Dissolve internal county borders

Boundary_HS <- shape_HS %>%
  summarise(geometry = st_union(geometry))  # Dissolve internal county borders


#combine boundaries 
Boundary_Combined <- rbind(Boundary_NRW, Boundary_HS)


# create plot
Project_partners <- ggplot(Boundary_Combined) +
  geom_sf(data = Boundary_Combined, fill = "#e0e0e0", color = "#999999") + # color = "#999999" with boundary
  geom_sf(data = partner_maps_NRW_HS, aes(color = "Project Partners"),
          show.legend = TRUE, size = 1) +
  scale_color_manual(values = c("Project Partners" = "#fa744b")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.box="horizontal",
        legend.key.width= unit(2, 'cm'),
        strip.text = element_text(size = 11, face = "bold"), 
        legend.text = element_text(size = 12),
        legend.title.align = 0.5,
        legend.title = element_text(size = 14, face = "bold"))+
  guides(color=guide_legend(title="Locations"), font = 2)

#save
# units in cm
# as a tiff file: a high-quality image file format commonly used for storing raster graphics
ggsave(Project_partners, filename = paste0("map/Project_partners_3",".tiff"),
       width = 15, height = 20, units = "cm", dpi = 750, compression = "lzw")

