library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(maps)
library(geodata)
library(cowplot) 


partners <- data.frame(Partner = c("Juelich", "Geisenheim", "Bonn", "Helmholtz", "Bayreuth", 
                                   "LWK_NRW","Germes" ,"Vitarom_Neurath", "Hoffmann", "Knodt"),
                Latitude = c("50.90524", "49.98438", "50.7292847", "48.2233187", "51.50578", 
                             "51.44122", "51.505907","51.03345", "51.4375832", "51.33512"),
                Longitude = c("6.404274", "7.96139", "7.0731143", "11.592294", "11.4510019", 
                              "6.26795", "6.27601","6.61390", "6.3253844", "6.46986")
                )

#create maps with ggspatial#

world <- ne_countries(scale = "medium", returnclass = "sf")

Germany <- ne_countries(scale = "medium", returnclass = "sf",
                        country = "Germany")  

# !reading the shapefile (the map)
# shapefiles are from eurostat, there are different versions, here i am using the version of 2021
# Source: https://ec.europa.eu/eurostat/de/web/gisco/geodata/statistical-units/territorial-units-statistics

shape <- read_sf(dsn = "Final_Report/map/German_Geo/NUTS_RG_20M_2021_3035.shp")
DE_shape <- shape %>% dplyr::filter(CNTR_CODE == "DE",LEVL_CODE == 3)
names(DE_shape)[4] <- "kreis"


# !transforming the soil point crs into the shapefile crs
maps <- partners %>% st_as_sf(coords = c("Longitude", "Latitude"), 
                               crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_transform(st_crs(DE_shape))

# !checking if the points are inside the polygons of our Nuts3 Shapefile
partner_maps <- st_join(maps, DE_shape)

# !filtering only points that are within NRW & Hessen
# NUTS code for NRW is DEA, Hessen is DEB
shape_NRW <- filter(DE_shape, str_detect(NUTS_ID, "^DEA"))

shape_HS <- filter(DE_shape, str_detect(NUTS_ID, "^DEB"))

shape_Partners <- DE_shape %>% dplyr::filter(NUTS_ID %in% c("DEA1B","DEA22", "DEA1E", "DEA1D","DEA26"))

# Ensure `shape_NRW` & `shape_HS` is only at the state level by merging county boundaries
Boundary_DE <- DE_shape %>%
  summarise(geometry = st_union(geometry))  # Dissolve internal states and county borders

Boundary_NRW <- shape_NRW %>%
  summarise(geometry = st_union(geometry))  # Dissolve internal county borders

Boundary_HS <- shape_HS %>%
  summarise(geometry = st_union(geometry))  # Dissolve internal county borders


#combine boundaries 
Boundary_Combined <- rbind(Boundary_NRW, Boundary_HS)

partner_maps_within_NRW <- st_intersection(partner_maps, Boundary_NRW)

# create name of country 
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

# create plot Germany


German_map <- ggplot(world)+
  geom_sf(data = Germany, fill = "#e0e0e0", linewidth = 0.8)+
  geom_sf(data = Boundary_NRW, fill = "beige", linewidth = 0.8) +
  annotation_scale(location="br",width_hint = 0.4)+ 
  annotation_north_arrow(location="br", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_gradient(na.value = "transparent")+
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", check_overlap = FALSE,  size = 3.5 ) +
  annotate(geom = "text", x = 7.5, y = 51.5, label = "NRW", 
           color = "darkgreen", size = 3.5,  fontface = "bold") +
  annotate(geom = "text", x = 15.5, y = 52.5, label = "Poland", 
           color = "darkblue", size = 3.5) +
  annotate(geom = "text", x = 5, y = 48, label = "France", 
           color = "darkblue", size = 3.5) +
  annotate(geom = "text", x = 13, y = 54.9, label = "Denmark", 
           color = "darkblue", size = 3.5) +
  coord_sf(xlim = c(3.5,16), ylim = c(47,55))+ #zoom the map
  xlab(NULL) + ylab(NULL)



# create plot NRW


farmer_partners <- data.frame(Partner = c( "LWK_NRW","Germes" ,"Vitarom_Neurath", "Knodt"), #"LWK_NRW","Germes" ,"Vitarom_Neurath", "Knodt"
                       Latitude = c("51.44122", "51.505907","51.03345","51.33512"),
                       Longitude = c("6.26795", "6.27601","6.61390", "6.46986")
)


farmer_maps <- farmer_partners %>% st_as_sf(coords = c("Longitude", "Latitude"), 
                              crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_transform(st_crs(DE_shape))



farmer_partner_maps <- st_join(farmer_maps, DE_shape)
# DEA1B: Kleve (Straelen), DEA22: Bonn, DEA1E: Viersen (Tönisvorst), DEA1D: Neuss (Neurath)
shape_farmers_Partners <- DE_shape %>% dplyr::filter(NUTS_ID %in% c("DEA1B","DEA1E", "DEA1D"))

farmer_maps_point <- shape_farmers_Partners %>% st_as_sf(coords = c("Longitude", "Latitude"), 
                                                   crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_transform(st_crs(DE_shape))

#for labeling
farmers_points<- st_centroid(farmer_maps)
farmers_points <- cbind(shape_farmers_Partners, st_coordinates(st_centroid(shape_farmers_Partners$geometry)))

DE_shp <- st_transform(DE_shape, crs = 4326)

NRW_shp <- st_transform(shape_farmers_Partners, crs = 4326)
NRW_shp_points <- cbind(NRW_shp, st_coordinates(st_centroid(NRW_shp$geometry)))
NRW_shp_points$kreis[NRW_shp_points$kreis=="Rhein-Kreis Neuss"]<-"Neuss"


NRW_map <- ggplot(DE_shp) +
  geom_sf(data = Boundary_NRW, fill = "beige", color = "#999999") + # color = "#999999" with boundary
  geom_sf(data = shape_farmers_Partners, fill = "#fa744b", color = "#fa744b", alpha = 0.5) + # color = "#999999" with boundary
  #geom_sf(data = farmer_partner_maps, aes(color = "Project Partners"),
  #show.legend = TRUE, size = 1) + # point
  #scale_color_manual(values = c("Project Partners" = "#fa744b")) +
  geom_text(data= NRW_shp_points,aes(x=X, y=Y, label=kreis),
            color = "black", check_overlap = FALSE,  size = 3.5 ) +
  theme_bw() +
  annotation_scale(location="bl",width_hint = 0.4)+ 
  annotation_north_arrow(location="bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_gradient(na.value = "transparent")+
  theme(legend.position = "none",
        legend.box="horizontal",
        legend.key.width= unit(2, 'in'),
        strip.text = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title.align = 0.5,
        legend.title = element_text(size = 14, face = "bold"))+
  guides(color=guide_legend(title="Locations"), font = 2)+
  coord_sf(xlim = c(5.8,10.8), ylim = c(50,52.6),expand = FALSE)+ 
  xlab(NULL) + ylab(NULL)
 

  
  final_map <- ggdraw() +
  draw_plot(NRW_map) +  # main NRW map
  draw_plot(German_map, x = 0.73, y = 0.08, width = 0.28, height = 0.28) + # inset: position + size
    draw_plot_label(
      c("B", "A"),
      c(0.1, 0.78),
      c(0.96, 0.38),
      size = c(16,12)) #+
     
    # geom_curve(aes(x = 0.9, y = 0.35, xend = 0.8, yend = 0.45),
    #            arrow = arrow(length = unit(0.02, "npc")))
  
#save
# units in cm
# as a tiff file: a high-quality image file format commonly used for storing raster graphics
ggsave(Project_partners, filename = paste0("map/Project_partners_3",".tiff"),
       width = 15, height = 20, units = "cm", dpi = 750, compression = "lzw")

