
library(HELPS)

# adding additional WBGT ----
# Stull 2011 WBT ----

WBT_Stull <- function(hurs, tas){
  T_a <- tas - 273.15
  T_wb <- T_a * atan(0.151977 * sqrt(hurs + 8.313659)) +
    atan(T_a + hurs) -
    atan(hurs - 1.676331) +
    0.00391838 * hurs^(1.5) * atan(0.023101 * hurs) -
    4.686035
  return(T_wb)
}

Stull_sun <- function(hurs, tas){
  T_a <- tas - 273.15
  T_wb <- T_a * atan(0.151977 * sqrt(hurs + 8.313659)) +
    atan(T_a + hurs) -
    atan(hurs - 1.676331) +
    0.00391838 * hurs^(1.5) * atan(0.023101 * hurs) -
    4.686035
  WBGT_shade = 0.7 * T_wb + 0.3 * (tas -273.15)
  WBGT_sun = WBGT_shade + 3
  return(WBGT_sun)
}

Stull_shade <- function(hurs, tas){
  T_a <- tas - 273.15
  T_wb <- T_a * atan(0.151977 * sqrt(hurs + 8.313659)) +
    atan(T_a + hurs) -
    atan(hurs - 1.676331) +
    0.00391838 * hurs^(1.5) * atan(0.023101 * hurs) -
    4.686035
  WBGT_shade = 0.7 * T_wb + 0.3 * (tas -273.15)
  return(WBGT_shade)
}


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

# sensitivity of WBGT ----

esi.sun.day.maiz <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = WBGT_ESI, YEAR_INPUT = 2024,
                           "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
                           "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
                           "HELPS_Test_Data/rsds_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20110101-20201231.nc4")
esi.sun.day.maiz

Dunne.sun.day.maiz <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = WBGT_sun, YEAR_INPUT = 2024,
                               "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
                               "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
                               "HELPS_Test_Data/ps_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
Dunne.sun.day.maiz

Dunne.shade.day.maiz <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = WBGT_shade, YEAR_INPUT = 2024,
                                 "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
                                 "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
                                 "HELPS_Test_Data/ps_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
Dunne.shade.day.maiz

# Stull.WBT.day.maiz  <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = WBT_Stull, YEAR_INPUT = 2024,
#                                   "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#                                   "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
# Stull.WBT.day.maiz


Stull.sun.day.maiz  <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = Stull_sun, YEAR_INPUT = 2024,
                                    "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
                                    "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
Stull.sun.day.maiz

Stull.shade.day.maiz  <- cal_heat_stress(TempRes = "day", SECTOR = "MAIZ_R", HS = Stull_shade, YEAR_INPUT = 2024,
                                    "HELPS_Test_Data/hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
                                    "HELPS_Test_Data/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
Stull.shade.day.maiz

# monthly HS
esi.sun.mon.maiz <- daily_to_monthly(esi.sun.day.maiz)
Dunne.sun.mon.maiz <- daily_to_monthly(Dunne.sun.day.maiz)
Dunne.shade.mon.maiz <- daily_to_monthly(Dunne.shade.day.maiz)
# Stull.WBT.mon.maiz <- daily_to_monthly(Stull.WBT.day.maiz)
Stull.sun.mon.maiz <- daily_to_monthly(Stull.sun.day.maiz)
Stull.shade.mon.maiz <- daily_to_monthly(Stull.shade.day.maiz)


esi.sun.ann <- monthly_to_annual(input_rack = esi.sun.mon.maiz, SECTOR = "MAIZ_R")
esi.sun.glu <- grid_to_region(grid_annual_value = esi.sun.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)

Dunne.sun.ann <- monthly_to_annual(input_rack = Dunne.sun.mon.maiz, SECTOR = "MAIZ_R")
Dunne.sun.glu <- grid_to_region(grid_annual_value = Dunne.sun.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)

Dunne.shade.ann <- monthly_to_annual(input_rack = Dunne.shade.mon.maiz, SECTOR = "MAIZ_R")
Dunne.shade.glu <- grid_to_region(grid_annual_value = Dunne.shade.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)

Stull.sun.ann <- monthly_to_annual(input_rack = Stull.sun.mon.maiz, SECTOR = "MAIZ_R")
Stull.sun.glu <- grid_to_region(grid_annual_value = Stull.sun.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)

Stull.shade.ann <- monthly_to_annual(input_rack = Stull.shade.mon.maiz, SECTOR = "MAIZ_R")
Stull.shade.glu <- grid_to_region(grid_annual_value = Stull.shade.ann, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)


summary(esi.sun.glu)
summary(Dunne.sun.glu)
summary(Stull.WBT.glu)
summary(Stull.sun.glu)
summary(Stull.shade.glu)
summary(Dunne.shade.glu)

esi.sun.glu %>% mutate(metric = "Moran (ESI)") %>%
  bind_rows(Dunne.shade.glu %>% mutate(metric = "Dunne + Davis-Jones")) %>%
  bind_rows(Stull.shade.glu %>% mutate(metric = "Dunne + Stull")) %>%
  bind_rows(Dunne.sun.glu %>% mutate(metric = "Dunne + Davis-Jones*")) %>%
  bind_rows(Stull.sun.glu %>% mutate(metric = "Dunne + Stull*")) %>% 
  na.omit() ->
  df.wbgt.sensitivity

saveRDS(df.wbgt.sensitivity, file = "C:/Model/HS_package/CEE_paper_sensitivity_data.rds")


df.wbgt.sensitivity %>% 
  group_by(metric) %>%
  summarise(y95 = quantile(value, 0.95),
            y75 = quantile(value, 0.75),
            y50 = quantile(value, 0.50),
            y25 = quantile(value, 0.25),
            y05 = quantile(value, 0.05),
            mean = mean(value)) %>%
  ggplot(aes(x = metric)) +
  geom_boxplot(aes(ymin = y05, lower = y25, middle = y50,
                   upper = y75, ymax = y95, color = metric),
               stat = "identity") +
  geom_errorbar(aes(ymin = mean,ymax = mean),
    width = 0.9, color = "black", linetype = "dashed") +
  labs(x = "", y ="Caculated WBGT (C)",
       color = "Heat stress", linetype = "Experiment",
       title = "Annual heat stress: agricultural labor in rain-fed maize, 2024 ") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) ->
  sen.WBGT

ggsave("../sensitivity of WBGT.jpg", plot = sen.WBGT, width = 6, height = 4)

# sensitivity of LHR ----

pwc.hothaps.mon.maiz <- PWC(WBGT = Dunne.shade.mon.maiz,  LHR = LHR_Hothaps, workload = "high")
pwc.hothaps.ann.maiz <- MON2ANN(pwc.hothaps.mon.maiz, SECTOR = "MAIZ_R")
rm(pwc.hothaps.mon.maiz)
pwc.hothaps.glu.maiz <- G2R(grid_annual_value = pwc.hothaps.ann.maiz, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)

pwc.foster.mon.maiz <- PWC(WBGT = Dunne.shade.mon.maiz,  LHR = LHR_Foster, workload = "high")
pwc.foster.ann.maiz <- MON2ANN(pwc.foster.mon.maiz, SECTOR = "MAIZ_R")
rm(pwc.foster.mon.maiz)
pwc.foster.glu.maiz <- G2R(grid_annual_value = pwc.foster.ann.maiz, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)

pwc.iso.mon.maiz <- PWC(WBGT = Dunne.shade.mon.maiz,  LHR = LHR_ISO, workload = "high")
pwc.iso.ann.maiz <- MON2ANN(pwc.iso.mon.maiz, SECTOR = "MAIZ_R")
rm(pwc.iso.mon.maiz)
pwc.iso.glu.maiz <- G2R(grid_annual_value = pwc.iso.ann.maiz, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)

pwc.niosh.mon.maiz <- PWC(WBGT = Dunne.shade.mon.maiz,  LHR = LHR_NIOSH, workload = "high")
pwc.niosh.ann.maiz <- MON2ANN(pwc.niosh.mon.maiz, SECTOR = "MAIZ_R")
rm(pwc.niosh.mon.maiz)
pwc.niosh.glu.maiz <- G2R(grid_annual_value = pwc.niosh.ann.maiz, SECTOR = "MAIZ_R", rast_boundary = reg_WB_raster)


pwc.hothaps.glu.maiz %>% mutate(LHR = "Hothaps") %>%
  bind_rows(pwc.foster.glu.maiz %>% mutate(LHR = "Foster")) %>%
  bind_rows(pwc.iso.glu.maiz %>% mutate(LHR = "ISO")) %>%
  bind_rows(pwc.niosh.glu.maiz %>% mutate(LHR = "NIOSH")) %>% head



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

