# library(devtools)
# devtools::install_github('JGCRI/HELPS')
# library(HELPS)
# 
# # adding additional WBGT ----
# # Stull 2011 WBT ----
# 
# WBT_Stull <- function(hurs, tas){
#   T_a <- tas - 273.15
#   T_wb <- T_a * atan(0.151977 * sqrt(hurs + 8.313659)) +
#     atan(T_a + hurs) -
#     atan(hurs - 1.676331) +
#     0.00391838 * hurs^(1.5) * atan(0.023101 * hurs) -
#     4.686035
#   return(T_wb)
# }
# 
# Stull_sun <- function(hurs, tas){
#   T_a <- tas - 273.15
#   T_wb <- T_a * atan(0.151977 * sqrt(hurs + 8.313659)) +
#     atan(T_a + hurs) -
#     atan(hurs - 1.676331) +
#     0.00391838 * hurs^(1.5) * atan(0.023101 * hurs) -
#     4.686035
#   WBGT_shade = 0.7 * T_wb + 0.3 * (tas -273.15)
#   WBGT_sun = WBGT_shade + 3
#   return(WBGT_sun)
# }
# 
# Stull_shade <- function(hurs, tas){
#   T_a <- tas - 273.15
#   T_wb <- T_a * atan(0.151977 * sqrt(hurs + 8.313659)) +
#     atan(T_a + hurs) -
#     atan(hurs - 1.676331) +
#     0.00391838 * hurs^(1.5) * atan(0.023101 * hurs) -
#     4.686035
#   WBGT_shade = 0.7 * T_wb + 0.3 * (tas -273.15)
#   return(WBGT_shade)
# }
# 
# 

# 
# # sensitivity of WBGT ----
# 
# esi.sun.day.maiz <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = WBGT_ESI, YEAR_INPUT = 2024,
#                            "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#                            "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#                            "HELPS_Test_Data/rsds_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
# # esi.sun.day.maiz
# 
# Dunne.sun.day.maiz <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = WBGT_sun, YEAR_INPUT = 2024,
#                                "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#                                "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#                                "HELPS_Test_Data/ps_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
# # Dunne.sun.day.maiz
# 
# Dunne.shade.day.maiz <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = WBGT_shade, YEAR_INPUT = 2024,
#                                  "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#                                  "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#                                  "HELPS_Test_Data/ps_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
# # Dunne.shade.day.maiz
# 
# # Stull.WBT.day.maiz  <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = WBT_Stull, YEAR_INPUT = 2024,
# #                                   "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
# #                                   "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
# # Stull.WBT.day.maiz
# 
# 
# Stull.sun.day.maiz  <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = Stull_sun, YEAR_INPUT = 2024,
#                                     "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#                                     "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
# # Stull.sun.day.maiz
# 
# Stull.shade.day.maiz  <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = Stull_shade, YEAR_INPUT = 2024,
#                                     "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#                                     "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
# # Stull.shade.day.maiz
# 
# 
# esi.sun.glu %>% mutate(metric = "Moran (ESI)") %>%
#   bind_rows(Dunne.shade.glu %>% mutate(metric = "Dunne + Davis-Jones")) %>%
#   bind_rows(Stull.shade.glu %>% mutate(metric = "Dunne + Stull")) %>%
#   bind_rows(Dunne.sun.glu %>% mutate(metric = "Dunne + Davis-Jones*")) %>%
#   bind_rows(Stull.sun.glu %>% mutate(metric = "Dunne + Stull*")) %>% 
#   na.omit() ->
#   df.wbgt.sensitivity
# 
# 
# layer_index <- 183
# esi.sun.day.maiz[[layer_index]] %>% as.data.frame(xy = T) %>% mutate(method = "Moran (ESI)") %>% 
#   bind_rows(Dunne.sun.day.maiz[[layer_index]] %>% as.data.frame(xy = T) %>% mutate(method = "Dunne + Davis-Jones*")) %>% 
#   bind_rows(Dunne.shade.day.maiz[[layer_index]] %>% as.data.frame(xy = T) %>% mutate(method = "Dunne + Davis-Jones")) %>% 
#   bind_rows(Stull.sun.day.maiz[[layer_index]] %>% as.data.frame(xy = T) %>% mutate(method = "Dunne + Stull*")) %>% 
#   bind_rows(Stull.shade.day.maiz[[layer_index]] %>% as.data.frame(xy = T) %>% mutate(method = "Dunne + Stull")) %>% 
#   na.omit() ->
#   df.WBGT.sensitivity
# saveRDS(df.WBGT.sensitivity, file = "C:/Model/HS_package/CEE_paper_sensitivity_data.rds")

df.WBGT.sensitivity <- readRDS("data/isimip/CEE_paper_sensitivity_data.rds")


df.WBGT.sensitivity %>% 
  group_by(method) %>%
  summarise(y95 = quantile(X2024.07.01, 0.95),
            y75 = quantile(X2024.07.01, 0.75),
            y50 = quantile(X2024.07.01, 0.50),
            y25 = quantile(X2024.07.01, 0.25),
            y05 = quantile(X2024.07.01, 0.05),
            mean = mean(X2024.07.01)) %>% 
  ggplot(aes(x = method)) +
  geom_boxplot(aes(ymin = y05, lower = y25, middle = y50,
                   upper = y75, ymax = y95, color = method),
               stat = "identity") +
  geom_errorbar(aes(ymin = mean,ymax = mean),
                width = 0.9, color = "black", linetype = "dashed") +
  labs(x = "", y = expression("Calculated WBGT (" * degree * "C)"), 
       color = "Heat stress", linetype = "Experiment",
       title = "Daily gridded heat stress exposure:\nlabor producing rain-fed maize, 2024-07-01 ") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") ->
  sen.WBGT


(sen.WBGT + ggtitle("")) %>% 
  Write_png(.name = paste0("sensitivity of WBGT"), .DIR_MODULE = DIR_MODULE, h = 6, w = 4)

# # monthly HS
# esi.sun.mon.maiz <- daily_to_monthly(esi.sun.day.maiz)
# Dunne.sun.mon.maiz <- daily_to_monthly(Dunne.sun.day.maiz)
# Dunne.shade.mon.maiz <- daily_to_monthly(Dunne.shade.day.maiz)
# # Stull.WBT.mon.maiz <- daily_to_monthly(Stull.WBT.day.maiz)
# Stull.sun.mon.maiz <- daily_to_monthly(Stull.sun.day.maiz)
# Stull.shade.mon.maiz <- daily_to_monthly(Stull.shade.day.maiz)
# 
# 
# esi.sun.ann <- monthly_to_annual(input_rack = esi.sun.mon.maiz, SECTOR = "MAIZ_R")
# esi.sun.glu <- grid_to_region(grid_annual_value = esi.sun.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)
# 
# Dunne.sun.ann <- monthly_to_annual(input_rack = Dunne.sun.mon.maiz, SECTOR = "MAIZ_R")
# Dunne.sun.glu <- grid_to_region(grid_annual_value = Dunne.sun.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)
# 
# Dunne.shade.ann <- monthly_to_annual(input_rack = Dunne.shade.mon.maiz, SECTOR = "MAIZ_R")
# Dunne.shade.glu <- grid_to_region(grid_annual_value = Dunne.shade.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)
# 
# Stull.sun.ann <- monthly_to_annual(input_rack = Stull.sun.mon.maiz, SECTOR = "MAIZ_R")
# Stull.sun.glu <- grid_to_region(grid_annual_value = Stull.sun.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)
# 
# Stull.shade.ann <- monthly_to_annual(input_rack = Stull.shade.mon.maiz, SECTOR = "MAIZ_R")
# Stull.shade.glu <- grid_to_region(grid_annual_value = Stull.shade.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)
# 
# 
# summary(esi.sun.glu)
# summary(Dunne.sun.glu)
# summary(Stull.WBT.glu)
# summary(Stull.sun.glu)
# summary(Stull.shade.glu)
# summary(Dunne.shade.glu)
# 
# esi.sun.glu %>% mutate(metric = "Moran (ESI)") %>%
#   bind_rows(Dunne.shade.glu %>% mutate(metric = "Dunne + Davis-Jones")) %>%
#   bind_rows(Stull.shade.glu %>% mutate(metric = "Dunne + Stull")) %>%
#   bind_rows(Dunne.sun.glu %>% mutate(metric = "Dunne + Davis-Jones*")) %>%
#   bind_rows(Stull.sun.glu %>% mutate(metric = "Dunne + Stull*")) %>% 
#   na.omit() ->
#   df.wbgt.sensitivity
# 
# saveRDS(df.wbgt.sensitivity, file = "C:/Model/HS_package/CEE_paper_sensitivity_data.rds")
# 
# 
# df.wbgt.sensitivity %>% 
#   group_by(metric) %>%
#   summarise(y95 = quantile(value, 0.95),
#             y75 = quantile(value, 0.75),
#             y50 = quantile(value, 0.50),
#             y25 = quantile(value, 0.25),
#             y05 = quantile(value, 0.05),
#             mean = mean(value)) %>%
#   ggplot(aes(x = metric)) +
#   geom_boxplot(aes(ymin = y05, lower = y25, middle = y50,
#                    upper = y75, ymax = y95, color = metric),
#                stat = "identity") +
#   geom_errorbar(aes(ymin = mean,ymax = mean),
#     width = 0.9, color = "black", linetype = "dashed") +
#   labs(x = "", y ="Caculated WBGT (C)",
#        color = "Heat stress", linetype = "Experiment",
#        title = "Annual heat stress: agricultural labor in rain-fed maize, 2024 ") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) ->
#   sen.WBGT
# 
# ggsave("../sensitivity of WBGT.jpg", plot = sen.WBGT, width = 6, height = 4)

# sensitivity of LHR ----


# add additional LHR ----

LHR_dunne <- function(WBGT){
  PWC = 100-25*(max(0, WBGT-25))^(2/3)
  return(PWC/100)
}

LHR_sartori <- function(WBGT){
  if (WBGT <= 26) {
    return(1) # Case 1: wbgt_r <= 26
  } else if (WBGT > 26 & WBGT <= 36) {
    return(1 - 0.25 / (36 - 26) * (WBGT - 26)) # Case 2: 26 < wbgt_r <= 36
  } else {
    return(0.25) # Case 3: wbgt_r > 36
  }
}

LHR_Foster <- function(WBGT, workload = NULL){
  PWC = 1 / (1 + (33.63/max(0,WBGT))^-6.33)
  return(PWC)
}

LHR_Hothaps <- function(WBGT, workload){
  if(workload == "high"){
    a1 = 30.94
    a2 = 16.64
  } else if(workload == "moderate"){
    a1 = 32.93
    a2 = 17.81
  } else if(workload == "low"){
    a1 = 34.64
    a2 = 22.72
  } else {
    stop("Error: check input for workload: 'high', 'moderate', 'low'")
  }
  PWC = 0.1 + 0.9 / (1 + (WBGT / a1)^a2)
  return(PWC)
}

LHR_ISO <- function(WBGT, workload){
  if(workload == "high"){
    M = 400
  } else if(workload == "moderate"){
    M = 300
  } else if(workload == "low"){
    M = 200
  } else {
    stop("Error: check input for workload: 'high', 'moderate', 'low'")
  }
  WBGT_lim = 34.9 - M/46
  WBGT_lim_rest = 34.9 - 117/46
  level = min(1, (WBGT_lim_rest - WBGT)/(WBGT_lim_rest - WBGT_lim))
  PWC = max(0, level)
  return(PWC)
}

LHR_NIOSH <- function(WBGT, workload){
  if(workload == "high"){
    M = 400
  } else if(workload == "moderate"){
    M = 300
  } else if(workload == "low"){
    M = 200
  } else {
    stop("Error: check input for workload: 'high', 'moderate', 'low'")
  }
  WBGT_lim = 56.7 - 11.5*log10(M)
  WBGT_lim_rest = 56.7 - 11.5*log10(117)
  level = min(1, (WBGT_lim_rest - WBGT)/(WBGT_lim_rest - WBGT_lim))
  PWC = max(0, level)
  return(PWC)
}

WBGT_range <- seq(10, 40, by = 0.5); WBGT_range

data <- data.frame(
  WBGT = WBGT_range,
  LHR_dunne = sapply(WBGT_range, LHR_dunne),
  LHR_Foster = sapply(WBGT_range, function(x) LHR_Foster(x, workload = "high")),
  LHR_sartori = sapply(WBGT_range, LHR_sartori),
  LHR_Hothaps = sapply(WBGT_range, function(x) LHR_Hothaps(x, workload = "high")),
  LHR_ISO = sapply(WBGT_range, function(x) LHR_ISO(x, workload = "high")),
  LHR_NIOSH = sapply(WBGT_range, function(x) LHR_NIOSH(x, workload = "high"))
)

# Reshape the data for ggplot
data_long <- data %>%
  tidyr::pivot_longer(cols = LHR_dunne :LHR_NIOSH, names_to = "Function", values_to = "PWC") %>%
  filter(PWC >= 0)

ggplot(data_long, aes(x = WBGT, y = (1-PWC) * 100, color = Function)) +
  geom_line(size = 1) +
  labs(title = "LHR Functions: PWC vs WBGT", color = "Function",
    x = "WBGT", y = "Heat induced Physical Work Capacity Loss (%)") +
  theme_bw() ->
  sen_LHR

ggsave("../sensitivity of LHR.jpg", plot = sen_LHR, width = 6, height = 4)

