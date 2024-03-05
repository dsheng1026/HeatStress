

source("R/AgBalElement_Storage.R")

conv_75_15 = 3.507477
conv_BIL_MIL = 1000.0

# use revenue ----

AgElement_AreaYieldPrice %>% 
  filter(element == "Revenue") %>% # billion 1975 $
  group_by(scenario, region, year) %>% 
  summarise(value = sum(value)) %>% 
  ProcScenDiff() %>% 
  mutate(value = value * conv_75_15) ->
  diff.vag.reg.bil.2015

AgElement_AreaYieldPrice %>% 
  filter(element == "Revenue") %>% # billion 1975 $
  group_by(scenario, year) %>% 
  summarise(value = sum(value)) %>% 
  ProcScenDiff() %>% 
  mutate(value = value * conv_75_15) ->
  diff.vag.glb.bil.2015

AgElement_AreaYieldPrice %>% 
  filter(element == "Revenue") %>% 
  filter(scenario %in% c("C_lpjml_HadGEM", "CL_lpjml_HS2_HadGEM"),
         year == 2100) %>% 
  select(scenario, region, sector, year, value) %>% 
  spread(scenario, value) %>% 
  mutate(diff = CL_lpjml_HS2_HadGEM - C_lpjml_HadGEM) -> check

summary(check)

# use future period, value of ag increase, price increase is larger than production loss

# use base year prices ----
AgMeatPrice %>% 
  filter(year == 2015) %>% 
  # filter(sector %in% c("Corn", "Wheat", "Rice", "OtherGrain", "RootTuber")) %>% 
  # filter(sector %in% c("Beef", "Dairy", "SheepGoat", "Pork", "Poultry","OtherMeatFish")) %>% 
  select(scenario, region, sector, base = Price) %>% 
  left_join(AgMeatPrice %>% select(scenario, region, sector, year, Production, Price),
            by = c("scenario", "region", "sector")) %>% 
  mutate(VAG = Production * Price) %>% 
  group_by(scenario, sector, region, year) %>% 
  summarise(value = sum(VAG, na.rm = T)) %>% 
  ProcScenDiff() %>% 
  mutate(value = value * conv_75_15) ->
  diff.vag.reg.bil.2015

# Diff: quantity Mt ----
AgMeatPrice %>% 
  select(scenario, region, sector, year, value = Production) %>% 
  ProcScenDiff() %>% 
  filter(year == 2100) ->
  diff.q.reg.sec

diff.q.reg.sec %>% 
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>%
  # filter(sector != "Pasture", year >= 2015) %>% 
  short_name() %>% 
  ggplot() +
  geom_bar(aes(x = region, y = value, fill = sector),
           stat = "identity", position = "stack") +
  labs(x = "", y = "Milltion ton", fill = "Sector") +
  # scale_fill_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1  

# Diff: quantity % ----

AgMeatPrice %>% 
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>% 
  group_by(scenario, year, sector) %>% 
  summarise(value = sum(Production)) %>% 
  ProcScenDiff()  ->
  diff.q.glb.sec

AgMeatPrice %>% 
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>% 
  group_by(scenario, year, sector) %>% 
  summarise(value = sum(Production)) %>% 
  ProcScenDiffR()  %>% 
  mutate(delta = 100*(value - 1)) ->
  diff.q.pct.glb.sec

diff.q.glb.sec %>% 
  filter(year >= 2015) %>% 
  ggplot() +
  geom_bar(aes(x = year, y = value, fill = sector),
           stat = "identity", position = "stack") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = "Milltion ton", fill = "Sector") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1  -> P3.1

P3.1 %>% Write_png(.name = "Diff_Q_glb_Mt", .DIR_MODULE = DIR_MODULE, h = 8, w = 10)


diff.q.pct.glb.sec %>% 
  filter(year >= 2015) %>% 
  ggplot() +
  geom_smooth(aes(x = year, y = 100*(value-1), color = sector), se = F) +
  # geom_line(aes(x = year, y = 100*(value-1), color = sector)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = "%", fill = "Sector") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1  -> P3.2
P3.2 %>% Write_png(.name = "Diff_Q_glb_pct", .DIR_MODULE = DIR_MODULE, h = 8, w = 10)



# Diff: price % ----

AgMeatPrice %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>%
  filter(sector != "Pasture", year >= 2015) %>%
  group_by_at(vars(scenario, year, branch, region, sector)) %>%
  summarise(value = weighted.mean(Price, w = Production), prod = sum(Production), .groups = "drop") %>%
  drop_na() ->
  PAgPrice32

PAgPrice32 %>%
  group_by_at(vars(scenario, year, branch, sector)) %>%
  summarise(value = weighted.mean(value, w = prod), prod = sum(prod), .groups = "drop") %>%
  drop_na() %>% mutate(region = "World") %>%
  bind_rows(PAgPrice32) %>%
  mutate(sector = factor(sector, levels = COMMSector)) ->
  PAgPrice32

PAgPrice32 %>% 
  filter(region == "World") %>% 
  select(-prod, -branch) %>% 
  ProcScenDiffR() %>% 
  ggplot() +
  geom_smooth(aes(x = year, y = 100*(value - 1), color = sector), se = F) +
  # geom_line(aes(x = year, y = 100*(value - 1), color = sector)) +
  labs(x = "", y = "%", fill = "Sector") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1  -> P3.3; P3.3
  
# Diff: food purchasing power ----

## Food price ----

"Foodprice" %>% PluckBind32() -> FoodPrice # 2005$/Mcal/day

"FoodConsumption" %>% PluckBind32() -> FoodCon # Pcal

FoodCon %>% select(scenario, region, sector, year, con = value) %>% 
  left_join_error_no_match(FoodPrice %>% select(scenario, region, sector = input, year, P = value),
                           by = c("scenario", "region", "sector", "year")) %>% 
  mutate(EXP = P * con) %>%  
  group_by(scenario, region, year) %>% 
  summarise(EXP = sum(EXP),
            con = sum(con)) %>% 
  mutate(P = EXP / con) ->
  PFood

## ag wage rate ----

"LaborPrice" %>% PluckBind32() %>% 
  mutate(region = gsub("Labor_Ag", "", market)) ->
  W32

### glb ----
Plabor32 %>% select(scenario, region, year, L = value) %>% 
  filter(region != "World") %>% 
  left_join_error_no_match(W32 %>% select(scenario, region, year, W = value), 
                           by = c("scenario", "region", "year")) %>%
  mutate(EXP = W*L) %>% 
  group_by(scenario, year) %>% 
  summarise(L = sum(L),
            EXP = sum(EXP)) %>% 
  mutate(W = EXP / L) -> PWage_glb

PFood %>% group_by(scenario, year) %>% 
  summarise(EXP = sum(EXP),
            con = sum(con)) %>% 
  mutate(P = EXP / con) %>% 
  filter(year >= 2015) %>% 
  left_join_error_no_match(PWage_glb %>% select(scenario,  year, W),
                           by = c("scenario",  "year")) %>% 
  group_by(value = W / P) %>% 
  select(-P, -W) ->
  PFPP_glb

PFPP_glb %>% select(scenario, year, value) %>% 
  ProcScenDiffR() %>% 
  ggplot() +
  geom_line(aes(x = year, y = 100*(value - 1)))

### reg ----
PFood %>% filter(year >= 2015) %>% 
  left_join_error_no_match(W32 %>% select(scenario, region, year, W = value),
                           by = c("scenario", "region", "year")) %>% 
  mutate(value = W / P) ->
  PFPP_reg

PFPP_reg %>% select(scenario, region, year, value) %>% 
  ProcScenDiffR() %>% 
  filter(year == 2100) %>% 
  short_name() %>% 
  ggplot() +
  geom_bar(aes(x = reorder(region, -value), y = 100*(value - 1)), stat = "identity") +
  labs(x = "", y = "%", title = "Changes in food purchasing power index: Agricultural wage / food price") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1  -> P3.4; P3.4

  
P3.4 %>% Write_png(.name = "Diff_FPP", .DIR_MODULE = DIR_MODULE, h = 8, w = 10)

# Fig 3 ----
(P3.1 + ggtitle("(A) Global agricultural output change by sector (Million ton)")+ labs(fill = "Sector")+theme(axis.title.x = element_blank(), legend.position = "right") )/
  (P3.2 + ggtitle("(B) Global agricultural output change by sector (%)") + labs(color = "") + theme(axis.title.x = element_blank(), legend.position = "right"))/
  # (P3.3 + ggtitle("(C) Global agricultural price change by sector (%)")+ labs(fill = "Sector")+theme(axis.title.x = element_blank(), legend.position = "right"))/
  patchwork::plot_layout(guides = "collect", heights = rep(1, 2)) -> pp; pp

pp %>% Write_png(.name = "Fig3", .DIR_MODULE = DIR_MODULE, h = 10, w = 10)


# regional quantity ----
AgMeatPrice %>% 
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>% 
  group_by(scenario, region, year, sector) %>% 
  summarise(value = sum(Production)) %>% 
  ProcScenDiff()  ->
  diff.q.reg.sec

AgMeatPrice %>% 
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>% 
  group_by(scenario, region, year, sector) %>% 
  summarise(value = sum(Production)) %>% 
  ProcScenDiffR()  %>% 
  mutate(delta = 100*(value - 1)) ->
  diff.q.pct.reg.sec

diff.q.reg.sec %>% 
  filter(year >= 2015) %>% 
  short_name() %>% 
  ggplot() +
  geom_bar(aes(x = year, y = value, fill = sector),
           stat = "identity", position = "stack") +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  labs(x = "", y = "Milltion ton", fill = "Sector", 
       title = "") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1  -> PS3.1

PS3.1 %>% Write_png(.name = "Diff_Q_reg", .DIR_MODULE = DIR_MODULE, h = 8, w = 16)


diff.q.pct.reg.sec %>% 
  filter(year == 2100) %>% 
  short_name() %>% 
  ggplot() +
  geom_tile(aes(x = region, y = sector, fill = delta)) +
  scale_fill_gradient2(low = "91D1C2FF", mid = "white", high = "blue", midpoint = 0) +
  labs(x = "", y = "", fill = "%", 
       title = "") +
  theme_bw() + theme0 + theme1  -> PS3.2

PS3.2 %>% Write_png(.name = "Diff_Q_pct_reg", .DIR_MODULE = DIR_MODULE, h = 8, w = 16)

(PS3.1 + ggtitle("(A) Agricultural output change by sector (Million ton)")+ labs(fill = "Mt")+theme(axis.title.x = element_blank(), legend.position = "right") )/
  (PS3.2 + ggtitle("(B) Agricultural output change by sector in 2100 (%)") + labs(fill = "%") + theme(axis.title.x = element_blank(), legend.position = "right"))/
  patchwork::plot_layout(guides = "collect", heights = rep(1, 2)) -> pp; pp

pp %>% Write_png(.name = "FigS3", .DIR_MODULE = DIR_MODULE, h = 16, w = 16)

