

# this script plot raw data from ISIMIP2b


# plot gobal mean trend in weather variables ----

glb <- read.csv("data/isimip/global_mean.csv")


glb %>% 
  # select(GCM, variable, index, value = mean) %>% 
  gather(stat, value, mean:max) %>%
  mutate(value = ifelse(variable == "tas", value - 273.15, value), # K to C
         value = ifelse(variable == "ps", value / 100, value)) %>%  # Pa to mbar
  mutate(year = 2000  + decade * 10 + layer) %>% 
  mutate(variable = gsub("hurs", "Relative humidity (%)", variable),
         variable = gsub("ps", "Air pressure (mbar)", variable),
         variable = gsub("tas", "Air Temperature (°C)", variable),
         variable = gsub("WBGT", "WBGT (°C)", variable)) %>% 
  ggplot(aes(x = year, y = value, color = GCM)) +
  geom_point(shape = 1) +
  geom_smooth(span = 0.5, se = F) +
  geom_vline(xintercept = 2015) +
  facet_wrap(stat ~ variable, ncol = 4, scales = "free_y") +
  labs(x = "", y ="", color = "GCM", title = "Trend of global mean of annual variable across GCMs under RCP 6.0") +
  theme_bw() + theme0 + theme1 ->
  p; p

p %>% Write_png(.name = paste0("ISIMIP_glb"), .DIR_MODULE = DIR_MODULE, h = 8, w = 16)

# load gridded-level annual data ----



# WBGT ----
# point <- readRDS("data/isimip/WBGT_point.rds")
# 
# point %>% 
#   # filter(year == 2015) %>% 
#   filter(year %in% c(2015, 2050, 2080)) %>%
#   filter(GCM == "GFDL") %>% 
#   mutate(stress = ifelse(value - 25 > 0, value - 25, 0)) %>% 
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = stress)) +
#   geom_hline(yintercept = 23.5, color = "red") +
#   geom_hline(yintercept = -23.5, color = "red") +
#   geom_hline(yintercept = 66.5, color = "blue") +
#   geom_hline(yintercept = -66.5, color = "blue") +
#   scale_fill_gradient(low = "white", high = "orange") +
#   facet_wrap(~ year) +
#   labs(x = "longitude", y = "latitude", fill = "Stress °C",
#        title = "Grid-level annual heat stress (WBGT - 25°C)") +
#   theme_bw() + theme0 + theme2 ->
#   p.wbgt
# 
# p.wbgt %>% Write_png(.name = paste0("point.WBGT"), .DIR_MODULE = DIR_MODULE, h = 6, w = 12)
# 
#   

# # tas ----
# rm(point)
# point <- readRDS("data/isimip/tas_point.rds")
# 
# point %>% 
#   group_by(x, y , GCM, var) %>% 
#   mutate(value = value - 273.15, # K to C
#          change = value - value[year == 2015]) %>% # calculate temperature increase
#   # filter(year %in% c(2015, 2050, 2080)) %>%
#   filter(year %in% c(2050, 2099)) ->
#   df.tas
# 
# df.tas %>% 
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = change)) +
#   geom_hline(yintercept = 23.5, color = "red") +
#   geom_hline(yintercept = -23.5, color = "red") +
#   geom_hline(yintercept = 66.5, color = "blue") +
#   geom_hline(yintercept = -66.5, color = "blue") +
#   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
#   # scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red")) +
#   facet_grid(GCM ~ year) +
#   labs(x = "longitude", y = "latitude", fill = "°C",
#        title = "Grid-level annual mean temperature") +
#   theme_bw() + theme0 + theme2 ->
#   p.tas
# 
# p.tas %>% Write_png(.name = paste0("point.tas"), .DIR_MODULE = DIR_MODULE, h = 10, w = 18)
# 
# rm(df.tas, p.tas, point)
#   
# # hurs ----
# 
# point <- readRDS("data/isimip/hurs_point.rds")
# 
# point %>% 
#   group_by(x, y , GCM, var) %>% 
#   mutate(change = value - value[year == 2015]) %>% # calculate temperature increase
#   # filter(year %in% c(2015, 2050, 2080)) %>%
#   filter(year %in% c(2050, 2099)) ->
#   df.hurs
# 
# df.hurs %>% 
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = change)) +
#   geom_hline(yintercept = 23.5, color = "red") +
#   geom_hline(yintercept = -23.5, color = "red") +
#   geom_hline(yintercept = 66.5, color = "blue") +
#   geom_hline(yintercept = -66.5, color = "blue") +
#   scale_fill_gradient2(low = "green", high = "purple") +
#   facet_grid(GCM ~ year) +
#   labs(x = "longitude", y = "latitude", fill = "%",
#        title = "Grid-level annual relative humidity") +
#   theme_bw() + theme0 + theme2 ->
#   p.hurs
# 
# p.hurs %>% Write_png(.name = paste0("point.hurs"), .DIR_MODULE = DIR_MODULE, h = 10, w = 18)
# 
# rm(p.hurs, df.hurs, point)
# 
# # ps ----
# 
# point <- readRDS("data/isimip/ps_point.rds")
# 
# point %>% 
#   group_by(x, y , GCM, var) %>% 
#   mutate(value = value / 100,
#          change = value - value[year == 2015]) %>% # calculate temperature increase
#   # filter(year %in% c(2015, 2050, 2080)) %>%
#   filter(year %in% c(2050, 2099)) ->
#   df.ps
# 
# df.ps %>% 
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = change)) +
#   geom_hline(yintercept = 23.5, color = "red") +
#   geom_hline(yintercept = -23.5, color = "red") +
#   geom_hline(yintercept = 66.5, color = "blue") +
#   geom_hline(yintercept = -66.5, color = "blue") +
#   scale_fill_gradient2(low = "green", high = "purple") +
#   facet_grid(GCM ~ year) +
#   labs(x = "longitude", y = "latitude", fill = "mbar",
#        title = "Grid-level annual air presure") +
#   theme_bw() + theme0 + theme2 ->
#   p.ps
# 
# p.ps %>% Write_png(.name = paste0("point.ps"), .DIR_MODULE = DIR_MODULE, h = 10, w = 18)
# 
# rm(p.ps, df.ps, point)
