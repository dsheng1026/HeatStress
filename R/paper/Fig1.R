# Fig 1 ----

library(sf)

# get data ready ----

# read in water basin shape file ----
WB <- st_read("data/maps/map_424.shp")

# Heat stress and labor productivity loss ----
Crop_GFDL_WB_IO_2 <- readRDS("C:/Model/heat_paper/Crop_GFDL_WB_IO_2.rds")
Crop_HadGEM2_WB_IO_2 <- readRDS("C:/Model/heat_paper/Crop_HadGEM2_WB_IO_2.rds")
Crop_GFDL_WB_IO_1 <- readRDS("C:/Model/heat_paper/Crop_GFDL_WB_IO_1.rds")
Crop_HadGEM2_WB_IO_1 <- readRDS("C:/Model/heat_paper/Crop_HadGEM2_WB_IO_1.rds")

Crop_GFDL_WB_IO_2 %>% ungroup() %>% select(GCAM_basin_ID, year, AgProductionTechnology, AgSupplySector, index) %>% mutate(source = "GFDL_High") %>% 
  bind_rows(Crop_GFDL_WB_IO_1 %>% ungroup() %>% select(GCAM_basin_ID, year, AgProductionTechnology, AgSupplySector, index) %>% mutate(source = "GFDL_Low")) %>% 
  bind_rows(Crop_HadGEM2_WB_IO_2 %>% ungroup() %>% select(GCAM_basin_ID, year, AgProductionTechnology, AgSupplySector, index) %>% mutate(source = "HadGEM2_High")) %>%  
  bind_rows(Crop_HadGEM2_WB_IO_1 %>% ungroup() %>% select(GCAM_basin_ID, year, AgProductionTechnology, AgSupplySector, index) %>% mutate(source = "HadGEM2_Low")) ->
  HS_all

HS_all %>% 
  filter(year %in% c(2050, 2100),
         AgSupplySector %in% c("Rice", "Wheat")) %>% 
  separate(AgProductionTechnology , into = c("crop", "WB", "IRR_RFD", "hi_lo"), sep = "_") %>% 
  mutate(mgmt = paste0(IRR_RFD, hi_lo)) %>% 
  ungroup() %>% select(GCAM_basin_ID , year, crop, WB, mgmt, source, index) %>%  # index is the labor productivity multiplier
  filter(mgmt == "IRRhi") ->
  Mult

# merge sf with HS ----
WB %>% rename(GCAM_basin_ID = basn_ID) %>% 
  left_join(Mult %>% 
              filter(crop == "Rice",
                     year == 2050) %>% 
              select(GCAM_basin_ID, source, crop, year, index) %>% 
              spread(source, index),
            by = "GCAM_basin_ID") %>% 
  bind_rows(WB %>% rename(GCAM_basin_ID = basn_ID) %>% 
              left_join(Mult %>% 
                          filter(crop == "Wheat",
                                 year == 2050) %>% 
                          select(GCAM_basin_ID, source, crop, year, index) %>% 
                          spread(source, index),
                        by = "GCAM_basin_ID")) %>% 
  bind_rows(WB %>% rename(GCAM_basin_ID = basn_ID) %>% 
              left_join(Mult %>% 
                          filter(crop == "Rice",
                                 year == 2100) %>% 
                          select(GCAM_basin_ID, source, crop, year, index) %>% 
                          spread(source, index),
                        by = "GCAM_basin_ID")) %>% 
  bind_rows(WB %>% rename(GCAM_basin_ID = basn_ID) %>% 
              left_join(Mult %>% 
                          filter(crop == "Wheat",
                                 year == 2100) %>% 
                          select(GCAM_basin_ID, source, crop, year, index) %>% 
                          spread(source, index),
                        by = "GCAM_basin_ID")) -> 
  df.plot

# plot ----
custom_palette <- c("lightblue", "white", "red")
df.plot %>% 
  gather(source, index, GFDL_High:HadGEM2_Low) %>%
  filter(source == "HadGEM2_High") %>% 
  ggplot() +
  geom_sf(aes(fill = 100*(1-index))) +
  scale_fill_gradient2(low = "lightblue", high = "red", midpoint = 0) +
  coord_sf(datum = NA) +
  facet_grid(crop ~ year) +
  labs(title = "Labor productivity loss: HadGEM2, high heat stress impact on labor productivity") +
  theme_minimal() +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60"))  +
  guides(fill = guide_colorbar(title = "%",
                               title.position = "top",
                               title.theme = element_text(size = 10,
                                                          face = "bold",
                                                          colour = "black",
                                                          angle = 0))) -> p; p


p %>% Write_png(.name = "HS_HadGEM_HS2_rice_wheat", .DIR_MODULE = DIR_MODULE, h = 16, w = 22)
