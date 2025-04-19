---
title: "Final_Report"
author: "Sanghyo Moon"
date: "2025-03-24"
output: 

  html_document: 
        toc: true
        toc_float: true
        toc_depth: 3
        number_sections: true

        fig_width: 9 
        fig_height: 4
        fig_align: "center"
        fig_caption: true
        
        highlight: pygments
        theme: cerulean
        
        keep_md: true
        
        code_folding: hide
---




```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
## Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
## 
## 
## Attaching package: 'kableExtra'
## 
## 
## The following object is masked from 'package:dplyr':
## 
##     group_rows
```

# Location of Partners
These partners provide information, which is directly related to the Decision Analysis. 

However, we have also two partners in Bayern


``` r
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
partner_maps_NRW_HS <- partner_maps %>%
    filter(NUTS_ID != "DE21H")%>%
  filter(NUTS_ID != "DE24B")

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
          show.legend = TRUE, size = 3) +
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

Project_partners
```

<img src="Final_Report_files/figure-html/Mapping-1.png" style="display: block; margin: auto;" />


# Data reading 
## Yield Data from LWK



``` r
Yield_LWK <- read_csv("Final_Report/Yield.csv",show_col_types = FALSE)

Yield_LWK <- Yield_LWK[1:24] 

Yield_LWK_1 <- Yield_LWK%>% select("KK_Tross","SK_Tross","KS_Tross", "SS_Tross","KK_Tross_1", "HK_Tross", "KS_Tross_3", "HS_Tross","KH_Tross", "SH_Tross", "KH_Tross_5", "HH_Tross")

names(Yield_LWK_1) <- c("KK_1_Tross","SK_Tross","KS_1_Tross", "SS_Tross","KK_2_Tross", "HK_Tross", "KS_2_Tross", "HS_Tross","KH_1_Tross", "SH_Tross", "KH_2_Tross", "HH_Tross")
Summary_Yield_LWK <- summary(Yield_LWK_1, na.rm = TRUE,quantile.type = 9)

kable(Yield_LWK_1) %>%
  kable_styling("striped", position = "left",font_size = 10)%>%
  scroll_box(height = "200px")
```

- Here I need CV to calculate...

- Yield data: confident intervall 95% here for min and max from LWK_Data

- once CVs and Yields are clear, then add into the input table. 

- ?? if there are 2 different yield, how can I combine it properly? 

# input calculation


# scenario_mc
This function is a wrapper around the mc_Simulation function that facilitates implementation of scenarios. The standard mc_Simulation function only allows specifying one set of estimates (i.e. distribution, lower and upper bounds) for each random variable. This is inconvenient when we want to run simulations for heterogeneous populations that include subsets with particular characteristics, e.g. small and large farms. It may then make sense to specify separate distributions for input variables for each of the subsets. The scenario_mc function facilitates this.

## Smaller farm 





``` r
plot_distributions(mcSimulation_object = s_farm_Priming_scenarios, 
                                  vars = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                                  method = 'boxplot', 
                                  old_names = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                                  new_names = c("no priming","heat priming","salt priming"),
                                  base_size = 10,
                                   colors = c("tomato","#009E73","#56B4E9"),
                                   legend.position = "none") +
  labs(title = "NPV Distribution with Different Decision Options \nand no stress for Smaller Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="Final_Report_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />


## bigger farm
