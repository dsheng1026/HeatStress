## Price ----

AgMeatPrice %>%
  filter(sector != "Pasture", year >= 2015) %>% 
  group_by_at(vars(scenario, year, sector)) %>%
  summarise(value = weighted.mean(Price, w = Production), .groups = "drop") %>%
  drop_na() %>% 
  mutate(element = "Price",
         region = "World") ->
  Key1.A.PRICE.glb.sec

AgMeatPrice %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM5),
    by = "sector0") %>%
  filter(sector != "Pasture", year >= 2015) %>% 
  group_by_at(vars(scenario, year, sector)) %>%
  summarise(value = weighted.mean(Price, w = Production), .groups = "drop") %>%
  drop_na() %>% 
  mutate(element = "Price",
         region = "World") ->
  Key1.A.PRICE.glb

AgMeatPrice %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM5),
    by = "sector0") %>%
  filter(sector != "Pasture", year >= 2015) %>% 
  group_by_at(vars(scenario, year, region, sector)) %>%
  summarise(value = weighted.mean(Price, w = Production), .groups = "drop") %>%
  drop_na() %>% 
  mutate(element = "Price") ->
  Key1.A.PRICE.reg32

AgMeatPrice %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM5),
    by = "sector0") %>%
  filter(sector != "Pasture", year >= 2015) %>% 
  left_join_error_no_match(Regmapping) %>% 
  group_by_at(vars(scenario, year, REG10_main, sector)) %>%
  summarise(value = weighted.mean(Price, w = Production), .groups = "drop") %>%
  drop_na() %>% 
  mutate(element = "Price") %>% 
  rename(region = REG10_main) ->
  Key1.A.PRICE.reg10

### change ----
Key1.A.PRICE.glb %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  change.pct.PRICE.glb

Key1.A.PRICE.reg32 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.PRICE.reg32

Key1.A.PRICE.reg10 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.PRICE.reg10

## Production ----
PSUA %>% 
  na.omit() %>% 
  Agg_reg(element, sector) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element == "Production") %>% 
  mutate(region = "World") ->
  Key1.A.PRODUCTION.glb.sec


PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(element, AgCOMM5) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element == "Production") %>% 
  mutate(region = "World") ->
  Key1.A.PRODUCTION.glb

PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(element, AgCOMM5, region) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element == "Production") ->
  Key1.A.PRODUCTION.reg32

PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  left_join_error_no_match(Regmapping) %>% 
  Agg_reg(element, AgCOMM5, REG10_main) %>% 
  rename(sector = AgCOMM5, region = REG10_main) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element == "Production") ->
  Key1.A.PRODUCTION.reg10

### change ----
Key1.A.PRODUCTION.glb %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  change.pct.PRODUCTION.glb

Key1.A.PRODUCTION.reg32 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.PRODUCTION.reg32

Key1.A.PRODUCTION.reg10 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.PRODUCTION.reg10



### dig numbers: production level delta 
Key1.A.PRODUCTION.reg10 %>% 
  DELTA_L_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  delta.PRODUCTION.reg10

## Consumption ----

PSUA %>% 
  na.omit() %>% 
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

PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(element, AgCOMM5) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element %in% c("Bioenergy", "Feed", "Food", "Other use")) %>% 
  group_by(scenario, sector, year) %>% 
  summarise(value = sum(value)) %>% 
  mutate(element = "Consumption",
         value = -value) %>% # demand was set negative in PSUA
  mutate(region = "World") ->
  Key1.A.CONSUMPTION.glb

PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(element, AgCOMM5, region) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element %in% c("Bioenergy", "Feed", "Food", "Other use")) %>% 
  group_by(scenario, region, sector, year) %>% 
  summarise(value = sum(value)) %>% 
  mutate(element = "Consumption",
         value = -value) ->
  Key1.A.CONSUMPTION.reg32

PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  left_join_error_no_match(Regmapping) %>% 
  Agg_reg(element, AgCOMM5, REG10_main) %>% 
  rename(sector = AgCOMM5, region = REG10_main) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element %in% c("Bioenergy", "Feed", "Food", "Other use")) %>% 
  group_by(scenario, region, sector, year) %>% 
  summarise(value = sum(value)) %>% 
  mutate(element = "Consumption",
         value = -value) ->
  Key1.A.CONSUMPTION.reg10

### change ----
Key1.A.CONSUMPTION.glb %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  change.pct.CONSUMPTION.glb

Key1.A.CONSUMPTION.reg32 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.CONSUMPTION.reg32

Key1.A.CONSUMPTION.reg10 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.CONSUMPTION.reg10

## Labor ----

PluckBind("LaborDemandSec") %>% 
  mutate(sector = tolower(sector)) %>% 
  Agg_reg( sector) %>% 
  mutate(element = "Labor",
         region = "World") ->
  Key1.A.LABOR.glb.sec


PluckBind("LaborDemandSec") %>% 
  left_join_error_no_match(Regmapping) %>% 
  na.omit() %>% mutate(sector = tolower(sector)) %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg( AgCOMM5) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = "Labor",
         region = "World") ->
  Key1.A.LABOR.glb

PluckBind("LaborDemandSec") %>% 
  left_join_error_no_match(Regmapping) %>% 
  na.omit() %>% mutate(sector = tolower(sector)) %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(region, AgCOMM5) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = "Labor") ->
  Key1.A.LABOR.reg32

PluckBind("LaborDemandSec") %>% 
  left_join_error_no_match(Regmapping) %>% 
  na.omit() %>% mutate(sector = tolower(sector)) %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  left_join_error_no_match(Regmapping) %>% 
  Agg_reg(AgCOMM5, REG10_main) %>% 
  rename(sector = AgCOMM5, region = REG10_main) %>% 
  mutate(element = "Labor") ->
  Key1.A.LABOR.reg10

### change ----
Key1.A.LABOR.glb %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  change.pct.LABOR.glb

Key1.A.LABOR.reg32 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.LABOR.reg32

Key1.A.LABOR.reg10 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.LABOR.reg10


## Land ----
unique(tolower(MapAgCOMM$AgCOMM)) -> AG_SEC

"Aggland" %>% PluckBind() %>%
  mutate(LandLeaf = gsub("C4$|Tree$", "", LandLeaf) ) %>%
  group_by(scenario, region, sector = LandLeaf, year) %>%
  summarise(value = sum(value)/10, .groups = "drop") %>% 
  mutate(sector = tolower(sector)) %>% filter(sector %in% AG_SEC) %>% 
  na.omit() -> # to Mha
  LAND 


LAND %>% 
  Agg_reg(sector) %>% 
  mutate(element = "Land") %>% 
  mutate(region = "World") ->
  Key1.A.LAND.glb.sec

LAND %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(AgCOMM5) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = "Land") %>% 
  mutate(region = "World") ->
  Key1.A.LAND.glb

LAND %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(AgCOMM5, region) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = "Land") ->
  Key1.A.LAND.reg32

LAND %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>%  
  left_join_error_no_match(Regmapping) %>% 
  Agg_reg(AgCOMM5, REG10_main) %>% 
  rename(sector = AgCOMM5, region = REG10_main) %>% 
  mutate(element = "Land") ->
  Key1.A.LAND.reg10

### change ----
Key1.A.LAND.glb %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  change.pct.LAND.glb

Key1.A.LAND.reg32 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.LAND.reg32

Key1.A.LAND.reg10 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.LAND.reg10

# Yield ----

Key1.A.LAND.glb %>% 
  bind_rows(Key1.A.PRODUCTION.glb) %>% 
  spread(element, value) %>% 
  mutate(value = Production / Land,
         element = "Yield") %>% 
  select(-Land, -Production) ->
  Key1.A.YLD.glb

Key1.A.YLD.glb %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.YLD.glb

Key1.A.LAND.reg10 %>% 
  bind_rows(Key1.A.PRODUCTION.reg10) %>% 
  spread(element, value) %>% 
  mutate(value = Production / Land,
         element = "Yield") %>% 
  select(-Land, -Production) ->
  Key1.A.YLD.reg10

Key1.A.YLD.reg10 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.YLD.reg10

Key1.A.LAND.reg32 %>% 
  bind_rows(Key1.A.PRODUCTION.reg32) %>% 
  spread(element, value) %>% 
  mutate(value = Production / Land,
         element = "Yield") %>% 
  select(-Land, -Production) ->
  Key1.A.YLD.reg32

Key1.A.YLD.reg32 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.YLD.reg32


# Eta ----

Key1.A.LABOR.glb %>% 
  bind_rows(Key1.A.PRODUCTION.glb) %>% 
  spread(element, value) %>% 
  mutate(value = Production / Labor,
         element = "Eta") %>% 
  select(-Labor, -Production) ->
  Key1.A.ETA.glb

Key1.A.ETA.glb %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.ETA.glb

Key1.A.LABOR.reg10 %>% 
  bind_rows(Key1.A.PRODUCTION.reg10) %>% 
  spread(element, value) %>% 
  mutate(value = Production / Labor,
         element = "Eta") %>% 
  select(-Labor, -Production) ->
  Key1.A.ETA.reg10

Key1.A.ETA.reg10 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.ETA.reg10


Key1.A.LABOR.reg32 %>% 
  bind_rows(Key1.A.PRODUCTION.reg32) %>% 
  spread(element, value) %>% 
  mutate(value = Production / Labor,
         element = "Eta") %>% 
  select(-Labor, -Production) ->
  Key1.A.ETA.reg32

Key1.A.ETA.reg32 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.ETA.reg32

##  gather all variables----
bind_rows(change.pct.PRICE.reg32,
          change.pct.PRODUCTION.reg32,
          change.pct.CONSUMPTION.reg32,
          change.pct.LABOR.reg32,
          change.pct.LAND.reg32,
          change.pct.YLD.reg32,
          change.pct.ETA.reg32) %>% 
  filter(sector %in% c("Key", "Other crops") ) %>% 
  mutate(element = gsub("Labor", "Employment", element),,
         element = gsub("Eta", "Labor productivity", element),
         element = gsub("Yield", "Crop yield", element),
         element = factor(element, levels = c("Production", "Price", "Consumption",  "Labor productivity","Employment", "Crop yield","Land"))) -> 
  change.pct.reg32

bind_rows(change.pct.PRICE.reg10,
          change.pct.PRODUCTION.reg10,
          change.pct.CONSUMPTION.reg10,
          change.pct.LABOR.reg10,
          change.pct.LAND.reg10,
          change.pct.YLD.reg10,
          change.pct.ETA.reg10) %>% 
  filter(sector %in% c("Key", "Other crops") ) %>% 
  mutate(element = gsub("Labor", "Employment", element),,
         element = gsub("Eta", "Labor productivity", element),
         element = gsub("Yield", "Crop yield", element),
         element = factor(element, levels = c("Production", "Price", "Consumption",  "Labor productivity","Employment", "Crop yield","Land"))) -> 
  change.pct.reg10

bind_rows(change.pct.PRICE.glb,
          change.pct.PRODUCTION.glb,
          change.pct.CONSUMPTION.glb,
          change.pct.LABOR.glb,
          change.pct.LAND.glb,
          change.pct.YLD.glb,
          change.pct.ETA.glb) %>% 
  filter(sector %in% c("Key", "Other crops") ) %>% 
  mutate(element = gsub("Labor", "Employment", element),,
         element = gsub("Eta", "Labor productivity", element),
         element = gsub("Yield", "Crop yield", element),
         element = factor(element, levels = c("Production", "Price", "Consumption",  "Labor productivity","Employment", "Crop yield","Land"))) -> 
  change.pct.glb

# absolute change in level for paper 
Key1.A.LABOR.glb %>% 
  DELTA_L_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  d.LABOR.glb

Key1.A.LABOR.reg32 %>% 
  DELTA_L_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  d.LABOR.reg32

Key1.A.LAND.glb %>% 
  DELTA_L_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  d.LAND.glb

Key1.A.LAND.reg32 %>% 
  DELTA_L_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  d.LAND.reg32



# Fig S3A ----

# change.pct.reg32 %>% 
#   group_by(year, sector, element, delta) %>% 
#   summarise(y90 = quantile(value, 0.90, na.rm = TRUE),
#             y75 = quantile(value, 0.75, na.rm = TRUE),
#             y50 = quantile(value, 0.50, na.rm = TRUE),
#             y25 = quantile(value, 0.25, na.rm = TRUE),
#             y10 = quantile(value, 0.10, na.rm = TRUE)) ->
#   box.reg32

# plot: 10% - 90%
# box.reg32 %>% 
#   ggplot(aes(x = element)) +
#   geom_hline(yintercept = 0, color = "grey") +
#   geom_boxplot(aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),
#                stat = "identity")  +
#   geom_errorbar(data = change.pct.glb, aes(ymin = value, ymax = value), linetype = "dashed") +
#   geom_point(data = change.pct.reg10, aes(y = value, color = region)) +
#   scale_color_brewer(palette = "Paired") +
#   facet_grid(delta ~ sector, scales = "free_y") +
#   labs(x = "", y ="", color = "Region") +
#   theme_bw() + theme0 + theme1

## boxplot data: 5% - 95% ----
change.pct.reg32 %>% 
  group_by(year, sector, element, delta) %>% 
  summarise(y95 = quantile(value, 0.95, na.rm = TRUE),
            y75 = quantile(value, 0.75, na.rm = TRUE),
            y50 = quantile(value, 0.50, na.rm = TRUE),
            y25 = quantile(value, 0.25, na.rm = TRUE),
            y05 = quantile(value, 0.05, na.rm = TRUE)) ->
  box.reg32

## plot 5% - 95% ----
box.reg32 %>% 
  ggplot(aes(x = element)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_boxplot(aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95),
               stat = "identity")  +
  geom_errorbar(data = change.pct.glb, aes(ymin = value, ymax = value), linetype = "dashed") +
  geom_point(data = change.pct.reg10, aes(y = value, color = region)) +
  scale_color_brewer(palette = "Paired") +
  facet_grid(delta ~ sector, scales = "free_y") +
  labs(x = "", y ="", color = "Region") +
  theme_bw() + theme0 + theme1 ->
  Fig.S3A; Fig.S3A

# plot.box.EEXO
Fig.S3A %>% Write_png(.name = paste0("Fig3_S1"), .DIR_MODULE = DIR_MODULE, h = 10, w = 8)




# Fig S3B ----

## Price ----

AgMeatPrice %>% 
  na.omit() %>% 
  filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
  group_by_at(vars(scenario, year, sector)) %>%
  summarise(value = weighted.mean(Price, w = Production), .groups = "drop") %>%
  drop_na() %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")),
         element = "Price") ->
  change.pct.PRICE.glb.sec

## Production ----
PSUA %>% 
  na.omit() %>% 
  filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element == "Production") %>% 
  Agg_reg(sector, element) %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")),
         element = "Production") ->
  change.pct.PRODUCTION.glb.sec

## Land ----
LAND %>% filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
  Agg_reg(sector) %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")),
         element = "Land") ->
  change.pct.LAND.glb.sec

## Labor ----
PluckBind("LaborDemandSec") %>% 
  na.omit() %>% mutate(sector = tolower(sector)) %>% 
  filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
  Agg_reg(sector) %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")),
         element = "Labor") ->
  change.pct.LABOR.glb.sec


## Yield ----

LAND %>% filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
  Agg_reg(sector) %>%  mutate(element = "Land") %>% 
  bind_rows(PSUA %>% 
              na.omit() %>% 
              filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
              mutate(element = gsub("Supply: ", "", element),
                     element = gsub("Demand: ", "", element)) %>% 
              filter(element == "Production") %>% 
              Agg_reg(sector, element)) %>% 
  spread(element, value) %>% 
  mutate(value = Production / Land,
         element = "Yield") %>% 
  select(-Land, -Production) %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.YLD.glb.sec


## Eta ----

PluckBind("LaborDemandSec") %>% 
  na.omit() %>% mutate(sector = tolower(sector)) %>% 
  filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
  Agg_reg(sector) %>%  mutate(element = "Labor") %>% 
  bind_rows(PSUA %>% 
              na.omit() %>% 
              filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>% 
              mutate(element = gsub("Supply: ", "", element),
                     element = gsub("Demand: ", "", element)) %>% 
              filter(element == "Production") %>% 
              Agg_reg(sector, element)) %>% 
  spread(element, value) %>% 
  mutate(value = Production / Labor,
         element = "Eta") %>% 
  select(-Labor, -Production) %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  change.pct.ETA.glb.sec

## COMBINE ----
bind_rows(change.pct.PRICE.glb.sec,
          change.pct.PRODUCTION.glb.sec,
          change.pct.LABOR.glb.sec,
          change.pct.LAND.glb.sec,
          change.pct.YLD.glb.sec,
          change.pct.ETA.glb.sec) %>% 
  mutate(element = gsub("Labor", "Employment", element),,
         element = gsub("Eta", "Labor productivity", element),
         element = gsub("Yield", "Crop yield", element),
         element = factor(element, levels = c("Production", "Price", "Consumption",  "Labor productivity","Employment", "Crop yield","Land"))) -> 
  change.pct.glb.sec

change.pct.glb.sec %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_bar(aes(x = element, y = value, fill = sector), stat = "identity") +
  facet_grid(delta~sector, scales = "free_y") +
  labs(x = "", y ="%", color = "Crop") +
  theme_bw() + theme0 + theme1 ->
  Fig.S3B; Fig.S3B

# plot.box.EEXO
# Fig.S3B %>% Write_png(.name = paste0("Fig3_S2"), .DIR_MODULE = DIR_MODULE, h = 10, w = 10)



# Fig 3A ----
### subset ----
bind_rows(change.pct.PRICE.reg32,
          change.pct.PRODUCTION.reg32,
          change.pct.CONSUMPTION.reg32,
          change.pct.ETA.reg32,
          change.pct.LABOR.reg32,
          change.pct.YLD.reg32,
          change.pct.LAND.reg32) %>% 
  filter(sector %in% c("Key", "Other crops") ) %>% 
  filter(element %in% c("Production", "Price", "Consumption", "Eta","Labor", "Yield","Land")) %>% 
  mutate(element = gsub("Labor", "Employment", element),,
         element = gsub("Eta", "Labor productivity", element),
         element = gsub("Yield", "Crop yield", element),
         element = factor(element, levels = c("Production", "Price", "Consumption",  "Labor productivity","Employment", "Crop yield","Land"))) -> 
  change.pct.reg32

bind_rows(change.pct.PRICE.reg10,
          change.pct.PRODUCTION.reg10,
          change.pct.CONSUMPTION.reg10,
          change.pct.ETA.reg10,
          change.pct.LABOR.reg10,
          change.pct.YLD.reg10,
          change.pct.LAND.reg10) %>% 
  filter(sector %in% c("Key", "Other crops") ) %>% 
  filter(region %in% c("Africa", "North America", "Reforming Economy" , "Southeast Asia")) %>%
  mutate(element = gsub("Labor", "Employment", element),,
         element = gsub("Eta", "Labor productivity", element),
         element = gsub("Yield", "Crop yield", element),
         element = factor(element, levels = c("Production", "Price", "Consumption",  "Labor productivity","Employment", "Crop yield","Land"))) -> 
  change.pct.reg10

bind_rows(change.pct.PRICE.glb,
          change.pct.PRODUCTION.glb,
          change.pct.CONSUMPTION.glb,
          change.pct.ETA.glb,
          change.pct.LABOR.glb,
          change.pct.YLD.glb,
          change.pct.LAND.glb) %>% 
  filter(sector %in% c("Key", "Other crops") ) %>% 
  mutate(element = gsub("Labor", "Employment", element),,
         element = gsub("Eta", "Labor productivity", element),
         element = gsub("Yield", "Crop yield", element),
         element = factor(element, levels = c("Production", "Price", "Consumption",  "Labor productivity","Employment", "Crop yield","Land"))) -> 
  change.pct.glb

change.pct.reg32 %>% 
  group_by(year, sector, element, delta) %>% 
  summarise(y95 = quantile(value, 0.95, na.rm = TRUE),
            y75 = quantile(value, 0.75, na.rm = TRUE),
            y50 = quantile(value, 0.50, na.rm = TRUE),
            y25 = quantile(value, 0.25, na.rm = TRUE),
            y05 = quantile(value, 0.05, na.rm = TRUE)) ->
  box.reg32

box.reg32 %>% 
  # filter(sector == "Key") %>% 
  ggplot(aes(x = element)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_boxplot(aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95),
               stat = "identity")  +
  geom_errorbar(data = change.pct.glb, aes(ymin = value, ymax = value, color = "World")) +
  geom_point(data = change.pct.reg10, aes(y = value, color = region), size = 2) +
  scale_color_brewer(palette = "Set2") +
  facet_grid(delta~ sector, scales = "free_y") +
  labs(x = "", y ="%", color = "Region") +
  ggtitle("(A): changes in agricultural market") +
  theme_bw() + theme0 + theme1 ->
  Fig3.A; Fig3.A

# plot.box.EEXO
Fig3.A %>% Write_png(.name = paste0("Fig3_A"), .DIR_MODULE = DIR_MODULE, h = 10, w = 8)


# Fig 3B ----

## Food security: food price index ----
PSUA %>% 
  na.omit() %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element == "Food") %>%
  select(scenario, sector, region, year, food = value) %>% 
  mutate(food = -food) %>% # food is demand so come with negative value from PSUA
  left_join_error_no_match(PluckBind("AgTotalPrice") %>% 
                             mutate(sector = gsub("total |_", "", sector)) %>% 
                             select(scenario, region, sector, year, price = value)) %>% 
  group_by(scenario, region, year) %>% 
  summarise(value = weighted.mean(price, food)) ->
  FPI.reg32

PSUA %>% 
  na.omit() %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element == "Food") %>% 
  select(scenario, sector, region, year, food = value) %>% 
  mutate(food = -food) %>% # food is demand so come with negative value from PSUA
  left_join_error_no_match(PluckBind("AgTotalPrice") %>% 
                             mutate(sector = gsub("total |_", "", sector)) %>% 
                             select(scenario, region, sector, year, price = value)) %>% 
  group_by(scenario, year) %>% 
  summarise(value = weighted.mean(price, food)) %>% 
  mutate(region = "World") ->
  FPI.glb

PSUA %>% 
  na.omit() %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element == "Food") %>% 
  select(scenario, sector, region, year, food = value) %>% 
  mutate(food = -food) %>% # food is demand so come with negative value from PSUA
  left_join_error_no_match(PluckBind("AgTotalPrice") %>% 
                             mutate(sector = gsub("total |_", "", sector)) %>% 
                             select(scenario, region, sector, year, price = value)) %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
  group_by(scenario, region = REG10_main, year) %>% 
  summarise(value = weighted.mean(price, food))  ->
  FPI.reg10

### change ----

FPI.reg10 %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  change.pct.FPI.reg10

FPI.glb %>% 
  CHANGE_PCT_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  change.pct.FPI.glb


## Trade  ----
PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(element, AgCOMM5) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element %in% c("Export", "Import")) %>% 
  spread(element, value) %>% 
  mutate(value = Import + Export,
         element = "Net import (Mt)") %>%  
  mutate(region = "World") %>% 
  select(-Export, -Import) ->
  Key1.A.TRADE.glb

PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(element, AgCOMM5, region) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element %in% c("Export", "Import")) %>% 
  spread(element, value) %>% 
  mutate(value = Import + Export,
         element = "Net import (Mt)") %>% 
  select(-Export, -Import)->
  Key1.A.TRADE.reg32


PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  left_join_error_no_match(Regmapping) %>% 
  Agg_reg(element, AgCOMM5, REG10_main) %>% 
  rename(sector = AgCOMM5, region = REG10_main) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element %in% c("Export", "Import")) %>% 
  spread(element, value) %>% 
  mutate(value = Import + Export,
         element = "Net import (Mt)") %>% 
  select(-Export, -Import)->
  Key1.A.TRADE.reg10

### delta level ----
Key1.A.TRADE.glb %>% 
  DELTA_L_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  delta.TRADE.glb

Key1.A.TRADE.reg32 %>% 
  DELTA_L_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  delta.TRADE.reg32

Key1.A.TRADE.reg10 %>% 
  DELTA_L_ALL(2100) %>% 
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>% 
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  delta.TRADE.reg10


## Trade dependency ----
## Net import / Consumption

Key1.A.CONSUMPTION.reg32 %>% 
  # mutate(value = -value) %>%
  bind_rows(Key1.A.TRADE.reg32) %>%
  spread(element, value) %>% 
  mutate(value = 100*`Net import (Mt)` / Consumption) %>% 
  select(-Consumption, -`Net import (Mt)`) %>%
  mutate(element = "Net import to consumption (%)") ->
  Key1.A.TD.reg32

Key1.A.CONSUMPTION.glb %>%
  # mutate(value = -value) %>%
  bind_rows(Key1.A.TRADE.glb) %>%
  spread(element, value) %>% 
  mutate(value = 100*`Net import (Mt)` / Consumption) %>% 
  select(-Consumption, -`Net import (Mt)`) %>%
  mutate(element = "Net import to consumption (%)") ->
  Key1.A.TD.glb


Key1.A.CONSUMPTION.reg10 %>%
  # mutate(value = -value) %>%
  bind_rows(Key1.A.TRADE.reg10) %>%
  spread(element, value) %>%
  mutate(value = 100*`Net import (Mt)` / Consumption) %>%
  select(-Consumption, -`Net import (Mt)`) %>%
  mutate(element = "Net import to consumption (%)") ->
  Key1.A.TD.reg10

### delta level ----
Key1.A.TD.glb %>%
  DELTA_L_ALL(2100) %>%
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>%
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild"))) ->
  delta.TD.glb

Key1.A.TD.reg32 %>%
  DELTA_L_ALL(2100) %>%
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>%
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  delta.TD.reg32

Key1.A.TD.reg10 %>%
  DELTA_L_ALL(2100) %>%
  filter(delta %in% c("gepic_HS2_HadGEM", "gepic_HS1_GFDL"))  %>%
  mutate(delta = ifelse(delta == "gepic_HS2_HadGEM", "Severe", "Mild"),
         delta = factor(delta, levels = c("Severe", "Mild")))->
  delta.TD.reg10


delta.TRADE.reg32 %>%
  bind_rows(delta.TD.reg32) ->
  delta.reg32

delta.TRADE.glb %>%
  bind_rows(delta.TD.glb) %>% 
  bind_rows(change.pct.FPI.glb)->
  delta.glb

delta.TRADE.reg10 %>%
  bind_rows(delta.TD.reg10) %>% 
  bind_rows(change.pct.FPI.reg10)->
  delta.reg10

delta.TD.reg10 %>% filter(sector == "Key", delta == "Severe") %>% arrange(-value) -> check
unique(check$region)
TD_REG_KEY <- c("Southeast Asia", "China+", "Africa" , "Middle East", "South Asia" ,
                "Latin America","Europe" , "Pacific OECD", "North America", "Reforming Economy" )

change.pct.FPI.reg10  %>% arrange(-value) -> check
unique(check$region)
FPI_REG_KEY <- c("Southeast Asia" , "Africa", "Middle East", "China+" , "Latin America",
                 "Pacific OECD", "South Asia",  "North America","Europe","Reforming Economy")
 

# FPI_REG_KEY <- c("Southeast\nAsia", "Africa", "Middle\nEast", "China+", "Latin\nAmerica",
#                  "Pacific\nOECD", "South\nAsia",  "North\nAmerica", "Europe", "Reforming\nEconomies")

# plot ----

delta.reg10 %>%
  filter(sector %in% c("Key") ) %>%
  bind_rows(change.pct.FPI.reg10 %>% mutate(element = "Food price index (%)")) %>% 
  mutate(region = factor(region, levels = FPI_REG_KEY),
         element = factor(element, levels = c("Net import to consumption (%)",
                                              "Net import (Mt)",
                                              "Food price index (%)"))) %>%
  
  ggplot() +
  geom_bar(aes(x = region, y = value), stat = "identity") +
  # facet_wrap(element ~ delta, ncol = 2, scales = "free_y") +
  facet_grid(element ~ delta, scales = "free_y") +
  labs(x = "", y ="Various", color = "Region") +
  theme_bw() + theme0 + theme1 ->
  Fig3.B; Fig3.B

# plot.Fig3B
Fig3.B %>% Write_png(.name = paste0("Fig3_B"), .DIR_MODULE = DIR_MODULE, h = 10, w = 10)


# COMBINE ----
# (Fig3.A + ggtitle("a. Changes in supply utilization & producer prices")) +
#   (Fig3.B + ggtitle("b. Changes in trade and food consumer prices")) +
#   plot_layout(widths = c(1.5, 3)) -> p; p
# p %>% Write_png(.name = paste0("Main_Fig3"), .DIR_MODULE = DIR_MODULE, h = 10, w = 18)

(Fig3.A + ggtitle("a")) +
  (Fig3.B + ggtitle("b")) +
  plot_layout(widths = c(2, 2)) -> p; p
p %>% Write_png(.name = paste0("Main_Fig3"), .DIR_MODULE = DIR_MODULE, h = 10, w = 18)
