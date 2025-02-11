SCENARIO <- Scenario; SCENARIO

PluckBind <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    filter(scenario %in% SCENARIO) 
}

DELTA_L_ALL <- function(.data, time){
  .data %>%
    filter(year == time) %>% 
    spread(scenario, value) %>% 
    mutate(gepic_HS1_GFDL = CL_gepic_HS1_GFDL - C_gepic_GFDL,
           gepic_HS2_GFDL = CL_gepic_HS2_GFDL - C_gepic_GFDL,
           gepic_HS1_HadGEM = CL_gepic_HS1_HadGEM - C_gepic_HadGEM,
           gepic_HS2_HadGEM = CL_gepic_HS2_HadGEM - C_gepic_HadGEM,
           lpjml_HS1_GFDL = CL_lpjml_HS1_GFDL - C_lpjml_GFDL,
           lpjml_HS2_GFDL = CL_lpjml_HS2_GFDL - C_lpjml_GFDL,
           lpjml_HS1_HadGEM = CL_lpjml_HS1_HadGEM - C_lpjml_HadGEM,
           lpjml_HS2_HadGEM = CL_lpjml_HS2_HadGEM - C_lpjml_HadGEM) %>% 
    select(-starts_with("CL"), -starts_with("C_"), -starts_with("L_"), -ref) %>% 
    gather(delta, value, gepic_HS1_GFDL:lpjml_HS2_HadGEM) %>% 
    return()
}

DELTA_PCT_ALL <- function(.data, time){
  .data %>%
    filter(year == time) %>% 
    group_by(across(-c(value, scenario))) %>%
    # group_by(year, region, sector, element) %>% 
    mutate(value = 100*value / value[scenario == "ref"]) %>%
    spread(scenario, value) %>% 
    mutate(gepic_HS1_GFDL = CL_gepic_HS1_GFDL - C_gepic_GFDL,
           gepic_HS2_GFDL = CL_gepic_HS2_GFDL - C_gepic_GFDL,
           gepic_HS1_HadGEM = CL_gepic_HS1_HadGEM - C_gepic_HadGEM,
           gepic_HS2_HadGEM = CL_gepic_HS2_HadGEM - C_gepic_HadGEM,
           lpjml_HS1_GFDL = CL_lpjml_HS1_GFDL - C_lpjml_GFDL,
           lpjml_HS2_GFDL = CL_lpjml_HS2_GFDL - C_lpjml_GFDL,
           lpjml_HS1_HadGEM = CL_lpjml_HS1_HadGEM - C_lpjml_HadGEM,
           lpjml_HS2_HadGEM = CL_lpjml_HS2_HadGEM - C_lpjml_HadGEM) %>% 
    select(-starts_with("CL"), -starts_with("C_"), -starts_with("L_"), -ref) %>% 
    gather(delta, value, gepic_HS1_GFDL:lpjml_HS2_HadGEM) %>% 
    return()
}

CHANGE_PCT_ALL <- function(.data, time){
  .data %>%
    filter(year == time) %>% 
    spread(scenario, value) %>% 
    mutate(gepic_HS1_GFDL = 100*(CL_gepic_HS1_GFDL - C_gepic_GFDL) / abs(C_gepic_GFDL),
           gepic_HS2_GFDL = 100*(CL_gepic_HS2_GFDL - C_gepic_GFDL) / abs(C_gepic_GFDL),
           gepic_HS1_HadGEM = 100*(CL_gepic_HS1_HadGEM - C_gepic_HadGEM) / abs(C_gepic_HadGEM),
           gepic_HS2_HadGEM = 100*(CL_gepic_HS2_HadGEM - C_gepic_HadGEM) / abs(C_gepic_HadGEM),
           lpjml_HS1_GFDL = 100*(CL_lpjml_HS1_GFDL - C_lpjml_GFDL) / abs(C_lpjml_GFDL),
           lpjml_HS2_GFDL = 100*(CL_lpjml_HS2_GFDL - C_lpjml_GFDL) / abs(C_lpjml_GFDL),
           lpjml_HS1_HadGEM = 100*(CL_lpjml_HS1_HadGEM - C_lpjml_HadGEM) / abs(C_lpjml_HadGEM),
           lpjml_HS2_HadGEM = 100*(CL_lpjml_HS2_HadGEM - C_lpjml_HadGEM) / abs(C_lpjml_HadGEM)) %>% 
    select(-starts_with("CL"), -starts_with("C_"), -starts_with("L_"), -ref) %>% 
    gather(delta, value, gepic_HS1_GFDL:lpjml_HS2_HadGEM) %>% 
    return()
}

FILTER_MS <- function(.data){
  .data %>%
    filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
    mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Higher", "Lower"),
           delta = factor(delta, levels = c("Higher", "Lower"))) %>% 
    return()
}

# Fig 2: ----

## EEXO ----

basin_to_country_mapping <- read.csv("data/maps/basin_to_country_mapping.csv", skip = 7, header = T)
iso_GCAM_regID <- read.csv("data/maps/iso_GCAM_regID.csv", skip = 6, header = T)
GCAM_region_names <- read.csv("data/maps/GCAM_region_names.csv", skip = 6, header = T)
LaborTech2015 <- read.csv("data/LaborTech2015.csv", skip = 1, header = T) %>% 
  select(region, subsector, technology, sector, labor = X2015) 

# Heat stress and labor productivity loss 
Crop_GFDL_WB_IO_2 <- readRDS("C:/Model/heat_paper/Crop_GFDL_WB_IO_2.rds")
Crop_HadGEM2_WB_IO_2 <- readRDS("C:/Model/heat_paper/Crop_HadGEM2_WB_IO_2.rds")
Crop_GFDL_WB_IO_1 <- readRDS("C:/Model/heat_paper/Crop_GFDL_WB_IO_1.rds")
Crop_HadGEM2_WB_IO_1 <- readRDS("C:/Model/heat_paper/Crop_HadGEM2_WB_IO_1.rds")

Crop_GFDL_WB_IO_2 %>% ungroup() %>% select(GCAM_basin_ID, year, AgProductionTechnology, AgSupplySector, index) %>% mutate(source = "GFDL_High LHR") %>% 
  bind_rows(Crop_GFDL_WB_IO_1 %>% ungroup() %>% select(GCAM_basin_ID, year, AgProductionTechnology, AgSupplySector, index) %>% mutate(source = "GFDL_Low LHR")) %>% 
  bind_rows(Crop_HadGEM2_WB_IO_2 %>% ungroup() %>% select(GCAM_basin_ID, year, AgProductionTechnology, AgSupplySector, index) %>% mutate(source = "HadGEM2_High LHR")) %>%  
  bind_rows(Crop_HadGEM2_WB_IO_1 %>% ungroup() %>% select(GCAM_basin_ID, year, AgProductionTechnology, AgSupplySector, index) %>% mutate(source = "HadGEM2_Low LHR")) ->
  HS_all


# group waterbasin labor by tech 
LaborTech2015 %>% rename(AgProductionTechnology = technology) %>% 
  group_by(region, AgProductionTechnology) %>% 
  summarise(labor = sum(labor)) ->
  LaborTech2015_WB

HS_all %>% 
  filter(year == 2100,
         source == "HadGEM2_High LHR")%>% 
  left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
  na.omit() %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "HadGEM2_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit()) %>%
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_High LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit()) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit()) %>% 
  group_by(GCAM_basin_ID, region, source) %>% 
  summarise(mult = weighted.mean(index, labor, na.rm = T)) %>% 
  mutate(EEXO = 100*(1-mult))->
  W.HS.WB



###   W.HS.reg32 ----
HS_all %>% 
  filter(year == 2100,
         source == "HadGEM2_High LHR")%>% 
  left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
  na.omit() %>% 
  left_join_error_no_match(Regmapping) %>% 
  group_by(region, year, source) %>% 
  summarise(mult = weighted.mean(index, labor)) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "HadGEM2_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(region, year, source) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_High LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(region, year, source) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(region, year, source) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  mutate(EEXO = 100*(1-mult))->
  W.HS.reg32

###   W.HS.reg10 ----

HS_all %>% 
  filter(year == 2100,
         source == "HadGEM2_High LHR")%>% 
  left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
  na.omit() %>% 
  left_join_error_no_match(Regmapping) %>% 
  group_by(REG10_main, year, source) %>% 
  summarise(mult = weighted.mean(index, labor)) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "HadGEM2_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(REG10_main, year, source) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_High LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(REG10_main, year, source) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(REG10_main, year, source) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  mutate(EEXO = 100*(1-mult))->
  W.HS.reg10

LaborTech2015 %>% as_tibble() %>% 
  left_join_error_no_match(Regmapping) %>% 
  group_by(REG10_main) %>% 
  summarise(labor = sum(labor, na.rm = T)) -> 
  Labor2015.reg10

### df.2A ----
W.HS.reg10 %>% 
  left_join_error_no_match(Labor2015.reg10, by = "REG10_main") ->
  df.2A

###   W.HS.glb ----

HS_all %>% 
  filter(year == 2100,
         source == "HadGEM2_High LHR")%>% 
  left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
  na.omit() %>% 
  left_join_error_no_match(Regmapping) %>% 
  group_by(year, source) %>% 
  summarise(mult = weighted.mean(index, labor)) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "HadGEM2_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(year, source) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_High LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(year, source) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(year, source) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  mutate(region = "World") %>% 
  mutate(EEXO = 100*(1-mult)) ->
  W.HS.glb

HS_all %>% 
  filter(year == 2100,
         source == "HadGEM2_High LHR")%>% 
  left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
  na.omit() %>% 
  left_join_error_no_match(Regmapping) %>% 
  group_by(year, source, AgSupplySector) %>% 
  summarise(mult = weighted.mean(index, labor)) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "HadGEM2_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(year, source, AgSupplySector) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_High LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(year, source, AgSupplySector) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(year, source, AgSupplySector) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  mutate(region = "World") %>% 
  mutate(EEXO = 100*(1-mult)) ->
  W.HS.glb.sec


##   W.HS.reg.sec ----
HS_all %>% 
  filter(year == 2100,
         source == "HadGEM2_High LHR")%>% 
  left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
  na.omit() %>% 
  left_join_error_no_match(Regmapping) %>% 
  group_by(year, region, source, AgSupplySector) %>% 
  summarise(mult = weighted.mean(index, labor)) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "HadGEM2_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(year, region, source, AgSupplySector) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_High LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(year, region, source, AgSupplySector) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  bind_rows(HS_all %>% 
              filter(year == 2100,
                     source == "GFDL_Low LHR")%>% 
              left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
              na.omit() %>% 
              left_join_error_no_match(Regmapping) %>% 
              group_by(year, region, source, AgSupplySector) %>% 
              summarise(mult = weighted.mean(index, labor))) %>% 
  mutate(EEXO = 100*(1-mult)) ->
  W.HS.reg.sec

## Fig S2-1:  EEXO boxplot ----
W.HS.reg.sec %>% 
  group_by(year, source) %>% 
  summarise(y90 = quantile(EEXO, 0.90, na.rm = TRUE),
            y75 = quantile(EEXO, 0.75, na.rm = TRUE),
            y50 = quantile(EEXO, 0.50, na.rm = TRUE),
            y25 = quantile(EEXO, 0.25, na.rm = TRUE),
            y10 = quantile(EEXO, 0.10, na.rm = TRUE)) ->
  box.EEXO

box.EEXO %>% 
  left_join_error_no_match(W.HS.glb %>% select(year, source, mean = EEXO)) %>% 
  mutate(source = gsub("HadGEM2_High LHR", "Higher:\nHadGEM2_High LHR", source),
         source = gsub("GFDL_Low LHR", "Lower:\nGFDL_Low LHR", source),
         source = factor(source, levels = c("Higher:\nHadGEM2_High LHR", "GFDL_High LHR", "HadGEM2_Low LHR", "Lower:\nGFDL_Low LHR"))) %>% 
  ggplot(aes(x = source)) +
  geom_boxplot(aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),
               stat = "identity")  +
  geom_errorbar(aes(ymin=mean, ymax=mean, color = "Global"), 
                position = position_dodge(width=1), 
                linetype = "dashed", 
                linewidth = 0.8) +
  geom_point(data = df.2A %>% mutate(source = gsub("HadGEM2_High LHR", "Higher:\nHadGEM2_High LHR", source),
                                 source = gsub("GFDL_Low LHR", "Lower:\nGFDL_Low LHR", source),
                                 source = factor(source, levels = c("Higher:\nHadGEM2_High LHR", "GFDL_High LHR", "HadGEM2_Low LHR", "Lower:\nGFDL_Low LHR"))),
             aes(size = labor, x = source, y = EEXO), position = position_dodge(width = 0.2), alpha = 0.6) +
  geom_text(data = df.2A %>% mutate(source = gsub("HadGEM2_High LHR", "Higher:\nHadGEM2_High LHR", source),
                                          source = gsub("GFDL_Low LHR", "Lower:\nGFDL_Low LHR", source),
                                          source = factor(source, levels = c("Higher:\nHadGEM2_High LHR", "GFDL_High LHR", "HadGEM2_Low LHR", "Lower:\nGFDL_Low LHR"))) %>%
              filter(REG10_main %in% c("Africa", "China+", "South Asia")),  # Subset the data for values > 250
    aes(label = REG10_main, y = EEXO),  # Label points with the 'region'
    vjust = 0.5,  # Adjust vertical position of the text
    hjust = 0.5,  # Adjust horizontal position of the text
    check_overlap = TRUE) +
  scale_size_continuous(range = c(1, 8)) +
  labs(x = "", y = "Heat-induced agricultural productivity loss (%)", 
       color = "") +
  guides(size = "none") +
  theme_bw() + theme0 + theme1 +
  theme(legend.position = c(.9, .9)) ->
  plot.box.EEXO; plot.box.EEXO


# plot.box.EEXO
plot.box.EEXO %>% Write_png(.name = paste0("Fig2_S1"), .DIR_MODULE = DIR_MODULE, h = 10, w = 8)


## Fig 2A: EEXO map ----
WB <- st_read("data/maps/reg_glu_boundaries_moirai_landcells_3p1_0p5arcmin.shp")

Reg_map <- st_read("data/maps/region_boundaries_moirai_combined_3p1_0p5arcmin.shp") %>% 
  st_transform(crs = st_crs(WB))

identical(st_crs(WB), st_crs(Reg_map))


## dissolve (32 to 10) ----

Reg_map %>% 
  left_join(Regmapping %>% select(reg_nm = region, REG10_main)) %>% 
  group_by(REG10_main) %>%
  summarize(geometry = st_combine(geometry), .groups = 'drop') ->
  REG10_map

# REG10_map %>%
#   ggplot() +
#   geom_sf(aes(fill = REG10_main))

WB %>% 
  left_join(W.HS.WB %>% filter(source == "HadGEM2_High LHR") %>% 
              select(glu_id = GCAM_basin_ID, reg_nm = region, EEXO),
            by = c("reg_nm", "glu_id")) %>% 
  mutate(EEXO = coalesce(EEXO, 0),
         source = "Higher: HadGEM2-High LHR") %>% 
  bind_rows(WB %>% 
              left_join(W.HS.WB %>% filter(source == "GFDL_Low LHR") %>% 
                          select(glu_id = GCAM_basin_ID, reg_nm = region, EEXO),
                        by = c("reg_nm", "glu_id")) %>% 
              mutate(EEXO = coalesce(EEXO, 0),
                     source = "Lower: GFDL-Low LHR")) %>% 
  mutate(source = factor(source, levels = c("Higher: HadGEM2-High LHR", "Lower: GFDL-Low LHR"))) ->
  EEXO.map.WB

ggplot() +
  geom_sf(data = EEXO.map.WB, aes(fill = -EEXO)) +
  scale_fill_gradient2(low = "red", high = "lightblue", midpoint = 0) +
  coord_sf(datum = NA) +
  # geom_sf(data = REG10_map, aes(color = REG10_main), lwd = 0.8, fill = NA) +
  facet_wrap(~ source, ncol = 1) +
  theme_bw() + theme0 + theme1 +
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
                                                          angle = 0))) -> 
  WB.EEXO.map.plot; WB.EEXO.map.plot


## Labor ----
PluckBind("LaborDemandSec") %>% 
  left_join_error_no_match(Regmapping) %>% 
  left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5), by = "sector") %>% 
  Agg_reg() %>% 
  mutate(element = "Labor",
         region = "World",
         sector = "Agriculture") ->
  decomp.LABOR.glb

decomp.LABOR.glb %>% 
  DELTA_L_ALL(2100) ->
  delta.labor

PluckBind("LaborDemandSec") %>% 
  left_join_error_no_match(Regmapping) %>% 
  left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5), by = "sector") %>% 
  Agg_reg(REG10_main) %>% 
  mutate(element = "Labor",
         sector = "Agriculture") %>% 
  rename(region = REG10_main) ->
  decomp.LABOR.reg10

decomp.LABOR.reg10 %>% 
  DELTA_L_ALL(2100) ->
  delta.labor.reg10

decomp.LABOR.reg10 %>% 
  DELTA_PCT_ALL(2100) ->
  delta.pct.labor.reg10

decomp.LABOR.glb %>% 
  DELTA_PCT_ALL(2100) ->
  delta.pct.labor.glb

decomp.LABOR.glb %>% 
  CHANGE_PCT_ALL(2100) ->
  change.pct.labor.glb

PluckBind("LaborDemandSec") %>% 
  # left_join_error_no_match(Regmapping) %>%
  # left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5), by = "sector") %>%
  Agg_reg(sector) %>%
  mutate(element = "Labor",
         region = "World") ->
  decomp.LABOR.sec

decomp.LABOR.sec %>% 
  DELTA_L_ALL(2100) ->
  delta.labor.sec

PluckBind("LaborDemandSec") %>% 
  # left_join_error_no_match(Regmapping) %>%
  left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5), by = "sector") %>%
  Agg_reg(AgCOMM5) %>%
  mutate(element = "Labor",
         region = "World",
         sector = AgCOMM5) ->
  decomp.LABOR.sec

decomp.LABOR.sec %>% 
  DELTA_L_ALL(2100) ->
  delta.labor.sec

## Fig 2B: nonlinear ----

### EEXO  ----
TIME <- c(2015, 2040, 2060, 2080, 2100)
EEXO_list <- list()
for (t in 1:length(TIME)){
  time <- TIME[[t]]; time
  HS_all %>% 
    filter(year == time,
           source == "HadGEM2_High LHR")%>% 
    left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
    na.omit() %>% 
    bind_rows(HS_all %>% 
                filter(year == time,
                       source == "HadGEM2_Low LHR")%>% 
                left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
                na.omit()) %>%
    bind_rows(HS_all %>% 
                filter(year == time,
                       source == "GFDL_High LHR")%>% 
                left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
                na.omit()) %>% 
    bind_rows(HS_all %>% 
                filter(year == time,
                       source == "GFDL_Low LHR")%>% 
                left_join(LaborTech2015 %>% rename(AgProductionTechnology = technology)) %>% # assign job as weight to HS index
                na.omit()) %>% 
    group_by(source) %>% 
    summarise(mult = weighted.mean(index, labor, na.rm = T)) %>% 
    mutate(EEXO = 100*(1-mult), year = time)->
    W.HS.WB
  EEXO_list[[t]] <- W.HS.WB
}

do.call(rbind, EEXO_list) %>% 
  filter(source %in% c("GFDL_Low LHR", "HadGEM2_High LHR")) %>% 
  mutate(source = ifelse(source == "HadGEM2_High LHR", "Higher", "Lower")) %>% 
  arrange(source, year) %>% 
  group_by(source) %>% 
  rename(value = EEXO) %>% 
  mutate(index = value - lag(value),
         year = factor(year, levels = c("2100","2080", "2060", "2040", "2015")),
         source = factor(source, levels = c("Lower", "Higher"))) %>% 
  na.omit() %>% 
  ggplot() +
  geom_bar(aes(y = source, x = index, fill = year),
           stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Reds", direction=-1,
                    labels=c("2080~2100","2060~2080","2040~2060","2015~2040"))+
  labs(x = "", y = "", fill = "Period") +
  theme_bw() + theme0 + theme1 +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.9, vjust = 1))->
  delta_bar_EEXO; delta_bar_EEXO

### employment level ----

TIME <- c(2015, 2040, 2060, 2080, 2100)
LABOR_L_list <- list()
for (t in 1:length(TIME)){
  time <- TIME[[t]]; time
  decomp.LABOR.glb %>% 
    DELTA_L_ALL(time) %>% 
    mutate(year = time) ->
    df
  
  LABOR_L_list[[t]] <- df
}

do.call(rbind, LABOR_L_list) %>% 
  filter(delta %in% c("gepic_HS1_GFDL", "gepic_HS2_HadGEM")) %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Higher", "Lower")) %>% 
  arrange(delta, year) %>% 
  group_by(delta) %>% 
  mutate(index = value - lag(value),
         year = factor(year, levels = c("2100","2080", "2060", "2040", "2015")),
         delta = factor(delta, levels = c("Lower", "Higher"))) %>% 
  na.omit() %>% 
  ggplot() +
  geom_bar(aes(y = delta, x = index, fill = year),
           stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Reds", direction=-1,
                    labels=c("2080~2100","2060~2080","2040~2060","2015~2040"))+
  labs(x = "", y = "", fill = "Period") +
  theme_bw() + theme0 + theme1 +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.9, vjust = 1))->
  delta_bar_LABOR; delta_bar_LABOR

### employment pct ----

TIME <- c(2015, 2040, 2060, 2080, 2100)
LABOR_pct_list <- list()
for (t in 1:length(TIME)){
  time <- TIME[[t]]; time
  decomp.LABOR.glb %>% 
    CHANGE_PCT_ALL(time) %>% 
    mutate(year = time) ->
    df
  
  LABOR_pct_list[[t]] <- df
}


do.call(rbind, LABOR_pct_list) %>% 
  filter(delta %in% c("gepic_HS1_GFDL", "gepic_HS2_HadGEM")) %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Higher", "Lower")) %>% 
  arrange(delta, year) %>% 
  group_by(delta) %>% 
  mutate(index = value - lag(value),
         year = factor(year, levels = c("2100","2080", "2060", "2040", "2015")),
         delta = factor(delta, levels = c("Lower", "Higher"))) %>% 
  na.omit() %>% 
  ggplot() +
  geom_bar(aes(y = delta, x = index, fill = year),
           stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Reds", direction=-1,
                    labels=c("2080~2100","2060~2080","2040~2060","2015~2040"))+
  labs(x = "", y = "", fill = "Period") +
  theme_bw() + theme0 + theme1 +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.9, vjust = 1))->
  pct_bar_LABOR; pct_bar_LABOR

### Combine Fig 2B----
((delta_bar_EEXO + ggtitle("Labor productivity loss (%)")) / 
   (pct_bar_LABOR + ggtitle("Employment change (%)")) /
   (delta_bar_LABOR + ggtitle("Employment change (million people)" ))) +
  patchwork::plot_layout(guides = 'collect') ->
  nonlinear.bar; nonlinear.bar


# nonlinear.bar %>% Write_png(.name = paste0("MR1.panelD"), .DIR_MODULE = DIR_MODULE, h = 6, w = 7)




## Fig 2C: employment heat map ----
PluckBind("LaborDemandSec") %>% 
  # left_join_error_no_match(Regmapping) %>%
  left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, AgCOMM7), by = "sector") %>% 
  Agg_reg(AgCOMM7) %>% 
  mutate(element = "Labor",
         region = "World") %>% 
  rename(sector = AgCOMM7) ->
  decomp.LABOR.sec7

decomp.LABOR.sec7 %>% 
  DELTA_L_ALL(2100) ->
  delta.labor.sec7

PluckBind("LaborDemandSec") %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
  left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, AgCOMM7), by = "sector") %>% 
  Agg_reg(AgCOMM7, REG10_main) %>% 
  mutate(element = "Labor") %>% 
  rename(sector = AgCOMM7,
         region = REG10_main) ->
  decomp.LABOR.sec7.reg10

decomp.LABOR.sec7.reg10 %>% 
  DELTA_L_ALL(2100) ->
  delta.labor.sec7.reg10

PluckBind("LaborDemandSec") %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG4_HS)) %>%
  left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, AgCOMM7), by = "sector") %>% 
  Agg_reg(AgCOMM7, REG4_HS) %>% 
  mutate(element = "Labor") %>% 
  rename(sector = AgCOMM7,
         region = REG4_HS) ->
  decomp.LABOR.sec7.reg4

decomp.LABOR.sec7.reg4 %>% 
  DELTA_L_ALL(2100) ->
  delta.labor.sec7.reg4

delta.labor.sec7.reg4 %>%
  # bind_rows(delta.labor.sec7) %>% 
  left_join_error_no_match(delta.labor %>% select(delta, total = value)) %>% 
  mutate(share = 100*value / total) %>%  
  FILTER_MS() ->
  df.heat.labor

UPPER <- ceiling(max(df.heat.labor$share) / 5); UPPER
LOWER <- floor(min(df.heat.labor$share) / 5); LOWER
breaks <- seq(LOWER*5, UPPER*5, 5); breaks

df.heat.labor$bin <- cut(df.heat.labor$share, breaks = breaks)
df.heat.labor$region <- factor(df.heat.labor$region, levels = c("Africa", "Southeast Asia", "South Asia", "ROW"))


df.heat.labor %>% 
  mutate(sector = gsub("Vege&Fruits", "Vegetable\n&Fruits", sector),
         region = gsub("AFRICA", "Africa", region),
         region = gsub("SE_ASIA", "Southeast Asia", region),
         region = gsub("SOUTH_ASIA", "South Asia", region),
         region = factor(region, levels=c("Africa", "Southeast Asia", "South Asia", "ROW"))) %>% 
  ggplot(aes(x = region, y = sector)) +
  geom_tile(aes(fill = share)) +
  geom_text(aes(label= round(share, 0))) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  facet_wrap(~ delta, ncol = 1) +
  labs(x = "", y ="", fill = "%") +
  theme_bw() + theme0 + theme1 ->
  heat.labor.plot; heat.labor.plot

# Combine Fig 2: ABC ----
# (WB.EEXO.map.plot + ggtitle("(A) Agricultural labor productivity across GCAM crops, 2100")) /
#   (heat.labor.plot + ggtitle("(B) Agricultural employment increase by region and sector, 2100")) /
#   (nonlinear.bar + ggtitle("(C) Nonlinear trend in heat-induced labor productivity loss \nand employment increase")) -> p;p 


(WB.EEXO.map.plot + ggtitle("")) %>% 
  Write_png(.name = paste0("Main_Fig2.A"), .DIR_MODULE = DIR_MODULE, h = 4, w = 5)
(nonlinear.bar) %>% 
  Write_png(.name = paste0("Main_Fig2.B"), .DIR_MODULE = DIR_MODULE, h = 6, w = 6)
(heat.labor.plot + ggtitle("")) %>% 
  Write_png(.name = paste0("Main_Fig2.C"), .DIR_MODULE = DIR_MODULE, h = 5, w = 5)


rm(Crop_GFDL_WB_IO_1)
rm(Crop_GFDL_WB_IO_2)
rm(Crop_HadGEM2_WB_IO_1)
rm(Crop_HadGEM2_WB_IO_2)