COMPARE_PCT <- function(.data, time){
  .data %>%
    filter(year == time) %>% 
    group_by(across(-c(value, scenario))) %>%
    # group_by(year, region, sector, element) %>% 
    mutate(value = 100*value / value[scenario == "ref"]) %>% 
    spread(scenario, value) %>% filter(year == time) %>% 
    mutate(DL_gepic_HS1_GFDL = CL_gepic_HS1_GFDL - C_gepic_GFDL,
           DL_gepic_HS2_GFDL = CL_gepic_HS2_GFDL - C_gepic_GFDL,
           DL_gepic_HS1_HadGEM = CL_gepic_HS1_HadGEM - C_gepic_HadGEM,
           DL_gepic_HS2_HadGEM = CL_gepic_HS2_HadGEM - C_gepic_HadGEM,
           DL_lpjml_HS1_GFDL = CL_lpjml_HS1_GFDL - C_lpjml_GFDL,
           DL_lpjml_HS2_GFDL = CL_lpjml_HS2_GFDL - C_lpjml_GFDL,
           DL_lpjml_HS1_HadGEM = CL_lpjml_HS1_HadGEM - C_lpjml_HadGEM,
           DL_lpjml_HS2_HadGEM = CL_lpjml_HS2_HadGEM - C_lpjml_HadGEM,
           DC_gepic_GFDL = C_gepic_GFDL - ref,
           DC_gepic_HadGEM = C_gepic_HadGEM - ref,
           DC_lpjml_GFDL = C_lpjml_GFDL - ref,
           DC_lpjml_HadGEM = C_lpjml_HadGEM - ref,
           DCL_gepic_HS1_GFDL = CL_gepic_HS1_GFDL - ref,
           DCL_gepic_HS2_GFDL = CL_gepic_HS2_GFDL - ref,
           DCL_gepic_HS1_HadGEM = CL_gepic_HS1_HadGEM - ref,
           DCL_gepic_HS2_HadGEM = CL_gepic_HS2_HadGEM - ref,
           DCL_lpjml_HS1_GFDL = CL_lpjml_HS1_GFDL - ref,
           DCL_lpjml_HS2_GFDL = CL_lpjml_HS2_GFDL - ref,
           DCL_lpjml_HS1_HadGEM = CL_lpjml_HS1_HadGEM - ref,
           DCL_lpjml_HS2_HadGEM = CL_lpjml_HS2_HadGEM - ref) %>% 
    select(-starts_with("CL"), -starts_with("C_"), -starts_with("L_"), -ref) %>% 
    gather(delta, value, DL_gepic_HS1_GFDL:DCL_lpjml_HS2_HadGEM) %>% 
    return()
}

COMPARE_LEVEL <- function(.data, time){
  .data %>%
    filter(year == time) %>% 
    group_by(across(-c(value, scenario))) %>%
    # group_by(year, region, sector, element) %>% 
    spread(scenario, value) %>% filter(year == time) %>% 
    mutate(DL_gepic_HS1_GFDL = CL_gepic_HS1_GFDL - C_gepic_GFDL,
           DL_gepic_HS2_GFDL = CL_gepic_HS2_GFDL - C_gepic_GFDL,
           DL_gepic_HS1_HadGEM = CL_gepic_HS1_HadGEM - C_gepic_HadGEM,
           DL_gepic_HS2_HadGEM = CL_gepic_HS2_HadGEM - C_gepic_HadGEM,
           DL_lpjml_HS1_GFDL = CL_lpjml_HS1_GFDL - C_lpjml_GFDL,
           DL_lpjml_HS2_GFDL = CL_lpjml_HS2_GFDL - C_lpjml_GFDL,
           DL_lpjml_HS1_HadGEM = CL_lpjml_HS1_HadGEM - C_lpjml_HadGEM,
           DL_lpjml_HS2_HadGEM = CL_lpjml_HS2_HadGEM - C_lpjml_HadGEM,
           DC_gepic_GFDL = C_gepic_GFDL - ref,
           DC_gepic_HadGEM = C_gepic_HadGEM - ref,
           DC_lpjml_GFDL = C_lpjml_GFDL - ref,
           DC_lpjml_HadGEM = C_lpjml_HadGEM - ref,
           DCL_gepic_HS1_GFDL = CL_gepic_HS1_GFDL - ref,
           DCL_gepic_HS2_GFDL = CL_gepic_HS2_GFDL - ref,
           DCL_gepic_HS1_HadGEM = CL_gepic_HS1_HadGEM - ref,
           DCL_gepic_HS2_HadGEM = CL_gepic_HS2_HadGEM - ref,
           DCL_lpjml_HS1_GFDL = CL_lpjml_HS1_GFDL - ref,
           DCL_lpjml_HS2_GFDL = CL_lpjml_HS2_GFDL - ref,
           DCL_lpjml_HS1_HadGEM = CL_lpjml_HS1_HadGEM - ref,
           DCL_lpjml_HS2_HadGEM = CL_lpjml_HS2_HadGEM - ref) %>% 
    select(-starts_with("CL"), -starts_with("C_"), -starts_with("L_"), -ref) %>% 
    gather(delta, value, DL_gepic_HS1_GFDL:DCL_lpjml_HS2_HadGEM) %>% 
    return()
}


FILTER_MS2 <- function(.data){
  .data %>%
    filter(delta %in% c("DL_gepic_HS1_GFDL", "DL_gepic_HS2_HadGEM",
                        "DC_gepic_GFDL", "DC_gepic_HadGEM",
                        "DCL_gepic_HS1_GFDL", "DCL_gepic_HS2_HadGEM")) %>% 
    mutate(impact = ifelse(startsWith(delta, "DL_"), "Labor", "delta"),
           impact = ifelse(startsWith(delta, "DC_"), "Crop", impact),
           impact = ifelse(startsWith(delta, "DCL_"), "Crop&Labor", impact), 
           delta = ifelse(grepl("HadGEM", delta), "Higher", "Lower"),
           delta = factor(delta, levels = c("Higher", "Lower"))) %>% 
    return()
}


# Fig4A: global decomposition  ----

Key1.A.PRODUCTION.glb %>%  
  bind_rows(Key1.A.PRICE.glb) %>%
  bind_rows(Key1.A.LABOR.glb) %>%
  bind_rows(Key1.A.LAND.glb) %>%
  bind_rows(Key1.A.YLD.glb) %>%
  bind_rows(Key1.A.ETA.glb) %>%
  COMPARE_PCT(2100) %>% 
  FILTER_MS2() %>% 
  filter(sector %in% c("Key")) %>% 
  filter(element %in% c("Production", "Eta" ,"Yield" ,"Labor", "Land" , "Price"  )) %>% 
  mutate(element = factor(element, levels = c("Production", "Eta" ,"Yield" ,"Labor", "Land" , "Price"  )),
         impact = factor(impact, levels = c("Crop&Labor", "Labor", "Crop"))) %>% 
  na.omit()->
  compare.pct.glb

# ggplot() +
#   geom_hline(yintercept = 0, color = "grey") +
#   geom_bar(data = compare.pct.reg %>% filter(impact %in% c("Labor", "Crop")), 
#            aes(y = value, x = element, fill = impact), stat = "identity", position = "stack") +
#   geom_errorbar(data = compare.pct.reg %>% filter(impact %in% c("Crop&Labor")) ,
#                 aes(ymin = value, ymax = value, x = element, color = "Crop&Labor"),
#                 linewidth = 1) +
#   facet_grid(delta~sector*region) +
#   scale_fill_brewer(palette = "Set2", name = NULL, direction = -1) +
#   scale_color_manual(values = c("Crop&Labor" = "black")) +
#   labs(x = "", y ="", color = "") +
#   theme_bw() + theme0 + theme1 ->
#   glb.check; glb.check

# Decomposition: key regions ----
Key1.A.PRODUCTION.glb %>% bind_rows(Key1.A.PRODUCTION.reg10) %>% 
  bind_rows(Key1.A.CONSUMPTION.glb) %>% bind_rows(Key1.A.CONSUMPTION.reg10) %>% 
  bind_rows(Key1.A.PRICE.glb) %>% bind_rows(Key1.A.PRICE.reg10) %>%
  bind_rows(Key1.A.LABOR.glb) %>% bind_rows(Key1.A.LABOR.reg10) %>% 
  bind_rows(Key1.A.LAND.glb) %>% bind_rows(Key1.A.LAND.reg10) %>% 
  COMPARE_PCT(2100) %>% 
  FILTER_MS2 %>% 
  filter(sector %in% c("Key")) %>% 
  filter(region %in% c("Africa", "North America", "Reforming Economy", "Southeast Asia", "South Asia",  "World")) ->
  compare.pct

ggplot() +
  geom_hline(yintercept = 0, color = "grey") +
  geom_bar(data = compare.pct %>% filter(impact %in% c("Labor", "Crop")), 
           aes(y = value, x = element, fill = impact), stat = "identity", position = "stack") +
  geom_errorbar(data = compare.pct %>% filter(impact %in% c("Crop&Labor")),
                aes(ymin = value, ymax = value, x = element, color = "Crop&Labor"),
                linewidth = 1) +
  facet_grid(delta~region) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  scale_color_manual(values = c("Crop&Labor" = "black")) +
  labs(x = "", y ="", color = "") +
  theme_bw() + theme0 + theme1 

##### World ----
Key1.A.PRODUCTION.glb %>% bind_rows(Key1.A.PRODUCTION.reg10) %>% 
  bind_rows(Key1.A.CONSUMPTION.glb) %>% bind_rows(Key1.A.CONSUMPTION.reg10) %>% 
  bind_rows(Key1.A.PRICE.glb) %>% bind_rows(Key1.A.PRICE.reg10) %>%
  bind_rows(Key1.A.LABOR.glb) %>% bind_rows(Key1.A.LABOR.reg10) %>% 
  bind_rows(Key1.A.LAND.glb) %>% bind_rows(Key1.A.LAND.reg10) %>% 
  COMPARE_PCT(2100) %>% 
  FILTER_MS2 %>% 
  filter(sector %in% c("Key")) %>% 
  filter(region %in% c ("World")) %>%
  bind_rows(Key1.A.PRODUCTION.glb.sec %>% 
              bind_rows(Key1.A.CONSUMPTION.glb.sec) %>% 
              bind_rows(Key1.A.PRICE.glb.sec) %>% 
              bind_rows(Key1.A.LABOR.glb.sec) %>% 
              bind_rows(Key1.A.LAND.glb.sec) %>% 
              COMPARE_PCT(2100) %>% 
              FILTER_MS2 %>% 
              filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
              filter(region %in% c ("World"))) %>% 
  filter(element %in% c("Production", "Price", "Consumption",  "Labor","Land")) %>% 
  mutate(element = factor(element, levels = c("Production", "Price", "Consumption",  "Labor","Land")),
         impact = factor(impact, levels = c("Crop&Labor", "Labor", "Crop")),
         sector = str_to_title(sector),
         sector = factor(sector, levels = rev(c("Key","Corn", "Rice", "Soybean", "Wheat")))) ->
  compare.pct

Key1.A.PRODUCTION.glb %>% bind_rows(Key1.A.PRODUCTION.reg10) %>% 
  bind_rows(Key1.A.CONSUMPTION.glb) %>% bind_rows(Key1.A.CONSUMPTION.reg10) %>% 
  bind_rows(Key1.A.PRICE.glb) %>% bind_rows(Key1.A.PRICE.reg10) %>%
  bind_rows(Key1.A.LABOR.glb) %>% bind_rows(Key1.A.LABOR.reg10) %>% 
  bind_rows(Key1.A.LAND.glb) %>% bind_rows(Key1.A.LAND.reg10) %>% 
  COMPARE_PCT(2100) %>% 
  FILTER_MS2 %>% 
  filter(sector %in% c("Key", "Other crops")) %>% 
  filter(region %in% c ("World")) %>% 
  mutate(sector = gsub("Key", "Key crops", sector),
         sector = factor(sector, levels = c("Other crops", "Key crops")),
         element = gsub("Labor", "Employment", element),
         element = factor(element, levels = c("Production", "Price", "Consumption", "Employment", "Land"))) ->
  compare.pct



# ggplot() +
#   geom_hline(yintercept = 0, color = "grey") +
#   geom_bar(data = compare.pct %>% filter(impact %in% c("Labor", "Crop")), 
#            aes(y = value, x = element, fill = impact), stat = "identity", position = "stack") +
#   geom_errorbar(data = compare.pct %>% filter(impact %in% c("Crop&Labor")) %>% 
#                   filter(region %in% c("World"), sector %in% c("Key")),
#                 aes(ymin = value, ymax = value, x = element, color = "Crop&Labor"),
#                 linewidth = 1) +
#   facet_wrap(sector~delta, ncol = 1, scales = "free_y") +
#   scale_fill_brewer(palette = "Set2", direction = -1, name = "(Panel A)\n") +
#   scale_color_manual(values = c("Crop&Labor" = "black")) +
#   labs(x = "", y ="%", color = "") +
#   theme_bw() + theme0 + theme1 ->
#   MR3.B.glb.key; MR3.B.glb.key


ggplot() +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_bar(data = compare.pct %>% filter(impact %in% c("Labor", "Crop")), 
           aes(x = value, y = sector , fill = impact), stat = "identity", position = "stack", alpha = 0.7) +
  geom_errorbarh(data = compare.pct %>% filter(impact %in% c("Crop&Labor")) ,
                 aes(xmin = value, xmax = value, y = sector, color = "Crop&Labor"),
                 linewidth = 0.8) +
  facet_grid(element~delta) +
  # facet_grid(element~delta, scales = "free_x") +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  scale_color_manual(values = c("Crop&Labor" = "black")) +
  labs(x = "%", y ="", color = "", title = "") +
  theme_bw() + theme0 + theme1 +
  theme(legend.position = c(0.9, 0.9))->
  Fig4.A

Fig4.A %>% Write_png(.name = paste0("Main_Fig4.A"), .DIR_MODULE = DIR_MODULE, h = 10, w = 10)


##### Figs4.A: World Key crops separate----

PluckBind("LaborDemandSec") %>% 
  left_join_error_no_match(Regmapping, by = "region") %>% 
  na.omit() %>% mutate(sector = tolower(sector)) %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(sector) %>% 
  mutate(element = "Labor",
         region = "World") ->
  Key1.A.LABOR.glb.sec

LAND %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(sector) %>% 
  mutate(element = "Land") %>% 
  mutate(region = "World") ->
  Key1.A.LAND.glb.sec

PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(element, sector) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element == "Production") %>% 
  mutate(region = "World") ->
  Key1.A.PRODUCTION.glb.sec

AgMeatPrice %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM5),
    by = "sector0") %>%
  filter(sector != "Pasture", year >= 2015) %>%  
  group_by_at(vars(scenario, year, sector = sector0)) %>%
  summarise(value = weighted.mean(Price, w = Production), .groups = "drop") %>%
  drop_na() %>% 
  mutate(element = "Price",
         region = "World") ->
  Key1.A.PRICE.glb.sec

PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(element, sector) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element %in% c("Bioenergy", "Feed", "Food", "Other use")) %>% 
  group_by(scenario, sector, year) %>% 
  summarise(value = sum(value)) %>% 
  mutate(element = "Consumption",
         value = -value) %>% # demand was set negative in PSUA
  mutate(region = "World") ->
  Key1.A.CONSUMPTION.glb.sec


Key1.A.PRODUCTION.glb.sec %>% 
  bind_rows(Key1.A.CONSUMPTION.glb.sec) %>% 
  bind_rows(Key1.A.PRICE.glb.sec) %>% 
  bind_rows(Key1.A.LABOR.glb.sec) %>% 
  bind_rows(Key1.A.LAND.glb.sec) %>% 
  COMPARE_PCT(2100) %>% 
  FILTER_MS2 %>% 
  # filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
  filter(!sector %in% c("beef", "dairy", "pork", "sheepgoat", "poultry", "forest", 
                        "foddergrass", "fodderherb", "pasture", "biomass")) %>% 
  filter(region %in% c ("World")) %>% 
  filter(element %in% c("Production", "Price", "Consumption",  "Labor","Land")) %>% 
  mutate(element = factor(element, levels = c("Production", "Price", "Consumption",  "Labor","Land")),
         impact = factor(impact, levels = c("Crop&Labor", "Labor", "Crop")),
         sector = factor(sector, levels = rev(c("corn", "rice", "soybean", "wheat", "othergrain","roottuber",
                                            "fruits", "vegetables", "sugarcrop", "oilcrop", "oilpalm", "legumes",
                                            "nutsseeds", "fibercrop","misccrop" ))))->
  compare.pct

# ggplot() +
#   geom_hline(yintercept = 0, color = "grey") +
#   geom_bar(data = compare.pct %>% filter(impact %in% c("Labor", "Crop")), 
#            aes(y = value, x = element, fill = impact), stat = "identity", position = "stack") +
#   geom_errorbar(data = compare.pct %>% filter(impact %in% c("Crop&Labor")) ,
#                 aes(ymin = value, ymax = value, x = element, color = "Crop&Labor"),
#                 linewidth = 1) +
#   facet_grid(delta~sector, scales = "free_y") +
#   scale_fill_brewer(palette = "Set2", name = NULL, direction = -1) +
#   scale_color_manual(values = c("Crop&Labor" = "black")) +
#   labs(x = "", y ="", color = "") +
#   theme_bw() + theme0 + theme1 ->
#   FigS4.B.glb.sec; FigS4.B.glb.sec


ggplot() +
  geom_hline(yintercept = 0, color = "grey") +
  geom_bar(data = compare.pct %>% filter(impact %in% c("Labor", "Crop")), 
           aes(x = value, y = sector , fill = impact), stat = "identity", position = "stack", alpha = 0.7) +
  geom_errorbarh(data = compare.pct %>% filter(impact %in% c("Crop&Labor")) ,
                aes(xmin = value, xmax = value, y = sector, color = "Crop&Labor"),
                linewidth = 1) +
  facet_grid(element~delta, scales = "free_x") +
  scale_fill_brewer(palette = "Set2", name = NULL, direction = -1) +
  scale_color_manual(values = c("Crop&Labor" = "black")) +
  labs(x = "", y ="", color = "", title = "Climate impact on agricultural market") +
  theme_bw() + theme0 + theme1 ->
  FigS4.B.glb.sec; FigS4.B.glb.sec


FigS4.B.glb.sec %>% Write_png(.name = paste0("Fig4_S1"), .DIR_MODULE = DIR_MODULE, h = 16, w = 10)


## FigS4.C ----

RANGE <- function(.df){
  .df %>% 
    mutate(scenario = ifelse(grepl("CL_", scenario), "Combined", scenario),
           scenario = ifelse(grepl("C_", scenario), "Crop", scenario),
           scenario = ifelse(grepl("L_HS", scenario), "Labor", scenario),
           scenario = ifelse(grepl("ref", scenario), "Ref", scenario)) %>% 
    group_by_at(vars(-scenario, -value)) %>%
    mutate(value = value / value[scenario == "Ref"]) %>%
    filter(scenario != "Ref")
}

Key1.A.PRODUCTION.glb %>%  
  bind_rows(Key1.A.PRICE.glb) %>%
  bind_rows(Key1.A.LABOR.glb) %>%
  bind_rows(Key1.A.LAND.glb) %>%
  bind_rows(Key1.A.YLD.glb) %>%
  bind_rows(Key1.A.ETA.glb) %>%
  filter(sector %in% c("Key", "Other crops")) %>% 
  mutate(GCM = ifelse(grepl("GFDL", scenario), "GFDL", "HadGEM2"),
         GGCM = ifelse(grepl("^C", scenario), scenario, NA),
         GGCM = ifelse(grepl("gepic", GGCM), "gepic", GGCM),
         GGCM = ifelse(grepl("lpjml", GGCM), "lpjml", GGCM),
         LHR = ifelse(grepl("L_", scenario), scenario, NA),
         LHR = ifelse(grepl("HS1", LHR), "Low", LHR),
         LHR = ifelse(grepl("HS2", LHR), "High", LHR)) %>% 
  mutate(scenario = ifelse(grepl("^CL_", scenario), "Combined", scenario),
         scenario = ifelse(grepl("^C_", scenario), "Crop", scenario),
         scenario = ifelse(grepl("^L_", scenario), "Labor", scenario),
         scenario = ifelse(grepl("ref", scenario), "Ref", scenario)) ->
  df.sen0

df.sen0 %>% 
  filter(grepl("^C", scenario)) %>% 
  group_by(element, sector, year, region, GCM, GGCM) %>% 
  mutate(index = 100* value / value[is.na(LHR)] - 100) %>% 
  filter(scenario == "Combined") %>% 
  mutate(scenario = "Labor") %>% 
  filter(year >= 2015) ->
  df.overlook


  

df.sen0 %>% 
  filter(scenario != "Ref") %>% 
  left_join(df.sen0 %>% 
              filter(scenario == "Ref") %>% select(element, sector, year, ref = value)) %>% 
  mutate(index = 100 *value / ref - 100) ->
  plot.sen0
  
plot.sen0 %>% filter(!is.na(GGCM)) %>% filter(year >= 2015) %>% 
  bind_rows(df.overlook) ->
  plot.sen

# key crops range ----

# df.plot.all %>% 
#   filter(year == 2100) %>% 
#   filter(sector == "Key") %>% 
#   group_by(element, sector, year, region, GCM, scenario) %>% 
#   summarise(MAX = max(index, na.rm = T),
#             MIN = min(index, na.rm = T)) %>% 
#   ggplot() +
#   geom_errorbarh(aes(xmin = MIN, xmax = MAX, y = scenario, linetype = scenario)) +
#   facet_grid(GCM ~ element)
  
# FigS4.C: sensitivity ----
# SEC <- "Key"
SEC <- "Other crops"
plot.sen %>% 
  filter(year == 2100) %>% 
  filter(sector == SEC) %>% 
  group_by(element, sector, year, region, GGCM, scenario) %>% 
  summarise(MAX = max(index, na.rm = T),
            MIN = min(index, na.rm = T)) %>% 
  rename(group = GGCM) %>% 
  bind_rows(plot.sen %>% 
              filter(year == 2100) %>% 
              filter(sector == SEC) %>% 
              group_by(element, sector, year, region, GCM, scenario) %>% 
              summarise(MAX = max(index, na.rm = T),
                        MIN = min(index, na.rm = T)) %>% 
              rename(group = GCM)) %>% 
  bind_rows(plot.sen %>% 
              filter(year == 2100) %>% 
              filter(sector == SEC) %>% 
              group_by(element, sector, year, region, LHR, scenario) %>% 
              summarise(MAX = max(index, na.rm = T),
                        MIN = min(index, na.rm = T)) %>% 
              rename(group = LHR)) %>% 
  bind_rows(plot.sen %>% 
              filter(year == 2100) %>% 
              filter(sector == SEC) %>% 
              group_by(element, sector, year, region, scenario) %>% 
              summarise(MAX = max(index, na.rm = T),
                        MIN = min(index, na.rm = T)) %>%
              mutate(group = "All")) %>% 
  filter(!is.na(group)) %>% 
  mutate(group = gsub("gepic", "GEPIC", group),
         group = gsub("lpjml", "LPJmL", group),
         group = factor(group, levels = c("All", "HadGEM2", "GFDL",
                                          "High", "Low", "GEPIC", "LPJmL"))) %>% 
  mutate(element = gsub("Labor", "Employment", element),,
         element = gsub("Eta", "Labor productivity", element),
         element = gsub("Yield", "Crop yield", element),
         element = factor(element, levels = c("Production", "Price", "Consumption",  
                                              "Labor productivity","Employment", "Crop yield","Land")),
         scenario = factor(scenario, levels = c("Combined",  "Labor", "Crop"))) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_errorbarh(aes(xmin = MIN, xmax = MAX, y = scenario, linetype = scenario, color = group),
                 height = 0.3) +
  # facet_grid(group ~ element) +
  facet_grid(group ~ element, scales = "free_x") +
  labs(x = "%", y ="", color = "Group", linetype = "Scenario" ,title = "") +
  theme_bw() + theme0 + theme1 ->
  FigS4.C.range
  
  
# FigS4.C.range.key %>% Write_png(.name = paste0("FigS4_4_range_key_fx"), .DIR_MODULE = DIR_MODULE, h = 8, w = 12)
# FigS4.C.range %>% Write_png(.name = paste0("FigS4_4_range_key"), .DIR_MODULE = DIR_MODULE, h = 8, w = 12)
FigS4.C.range %>% Write_png(.name = paste0("FigS4_4_range_othercrop"), .DIR_MODULE = DIR_MODULE, h = 8, w = 12)




# Fig4B: regional map combined ----

Key1.A.PRODUCTION.reg32 %>% 
  bind_rows(Key1.A.CONSUMPTION.reg32) %>% 
  bind_rows(Key1.A.PRICE.reg32) %>% 
  bind_rows(Key1.A.LABOR.reg32) %>%  
  bind_rows(Key1.A.LAND.reg32) %>% 
  COMPARE_PCT(2100) %>% 
  FILTER_MS2 %>% 
  filter(sector %in% c("Key"))  ->
  df.compare.all

Reg_map %>% rename(region = reg_nm) %>% 
  left_join(df.compare.all, by = "region") %>% 
  mutate(element = gsub("Labor", "Employment", element),
         element = factor(element, levels = c("Production", "Price", "Consumption", "Employment", "Land")),
         impact = factor(impact, levels = c("Crop", "Labor", "Crop&Labor"))) %>% 
  filter(delta == "Higher") %>% 
  ggplot() +
  geom_sf(aes(fill = value)) +
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0) +
  coord_sf(datum = NA) +
  # geom_sf(data = REG10_map, aes(color = REG10_HS), lwd = 0.8, fill = NA) +
  facet_grid(element~ impact) +
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
                               title.theme = element_text(size = 16,
                                                          face = "bold",
                                                          colour = "black",
                                                          angle = 0))) ->
  Fig4.B.reg.map
  
Fig4.B.reg.map %>% Write_png(.name = paste0("Main_Fig4.B"), .DIR_MODULE = DIR_MODULE, h = 6, w = 10)


# COMBINE -----
# (Fig4.A + ggtitle("a")) +
#   (Fig4.B.reg.map + ggtitle("b")) +
#   plot_layout(widths = c(2, 2)) -> p; p


# (Fig4.A + ggtitle("a")) +
#   (Fig4.B.reg.map + ggtitle("b")) +
#   plot_layout(widths = c(1.5, 3)) -> p; p
# p %>% Write_png(.name = paste0("Main_Fig4"), .DIR_MODULE = DIR_MODULE, h = 10, w = 18)

### FigS4.B: regional map decomposition for othercrops ----

Key1.A.PRODUCTION.reg32 %>% 
  bind_rows(Key1.A.CONSUMPTION.reg32) %>% 
  bind_rows(Key1.A.PRICE.reg32) %>% 
  bind_rows(Key1.A.LABOR.reg32) %>%  
  bind_rows(Key1.A.LAND.reg32) %>% 
  COMPARE_PCT(2100) %>% 
  FILTER_MS2 %>% 
  filter(sector %in% c("Other crops"))  ->
  df.compare.all

Reg_map %>% rename(region = reg_nm) %>% 
  left_join(df.compare.all, by = "region") %>% 
  mutate(element = gsub("Labor", "Employment", element),
         element = factor(element, levels = c("Production", "Price", "Consumption", "Employment", "Land")),
         impact = factor(impact, levels = c("Crop", "Labor", "Crop&Labor"))) %>% 
  filter(delta == "Higher") %>% 
  ggplot() +
  geom_sf(aes(fill = value)) +
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0) +
  coord_sf(datum = NA) +
  # geom_sf(data = REG10_map, aes(color = REG10_HS), lwd = 0.8, fill = NA) +
  facet_grid(element~ impact) +
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
                               title.theme = element_text(size = 16,
                                                          face = "bold",
                                                          colour = "black",
                                                          angle = 0))) ->
  FigS4.B.reg.map

FigS4.B.reg.map %>% Write_png(.name = paste0("FigS4_3_othercrops"), .DIR_MODULE = DIR_MODULE, h = 6, w = 10)



Key1.A.PRODUCTION.glb %>% bind_rows(Key1.A.PRODUCTION.reg10) %>% 
  bind_rows(Key1.A.CONSUMPTION.glb) %>% bind_rows(Key1.A.CONSUMPTION.reg10) %>% 
  bind_rows(Key1.A.PRICE.glb) %>% bind_rows(Key1.A.PRICE.reg10) %>%
  bind_rows(Key1.A.LABOR.glb) %>% bind_rows(Key1.A.LABOR.reg10) %>% 
  bind_rows(Key1.A.LAND.glb) %>% bind_rows(Key1.A.LAND.reg10) %>% 
  COMPARE_PCT(2100) %>% 
  FILTER_MS2 %>% 
  filter(sector %in% c("Key")) %>% 
  filter(region != "World") %>% 
  mutate(sector = gsub("Key", "Key crops", sector),
         sector = factor(sector, levels = c("Other crops", "Key crops")),
         element = gsub("Labor", "Employment", element),
         element = factor(element, levels = c("Production", "Price", "Consumption", "Employment", "Land"))) ->
  compare.pct.key

ggplot() +
  geom_hline(yintercept = 0, color = "grey") +
  geom_bar(data = compare.pct.key %>% filter(impact %in% c("Labor", "Crop")),
           aes(y = value, x = element, fill = impact), stat = "identity", position = "stack") +
  geom_errorbar(data = compare.pct.key %>% filter(impact == "Crop&Labor"),
                aes(ymin = value, ymax = value, x = element, color = "Crop&Labor"),
                linewidth = 1) +
  facet_grid(delta~region) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  scale_color_manual(values = c("Crop&Labor" = "black")) +
  labs(x = "", y ="%", color = "") +
  theme_bw() + theme0 + theme1 +
  theme(legend.position = "bottom")->
  FigS4.C.reg.Lower

FigS4.C.reg.Lower %>% Write_png(.name = paste0("FigS4_4_key_Lower"), .DIR_MODULE = DIR_MODULE, h = 6, w = 14)

