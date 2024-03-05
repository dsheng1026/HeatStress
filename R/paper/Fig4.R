# compare delta_C and delta_L 

SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = ifelse(scenario == "CL", "Crop & Labor", scenario),
           scenario = ifelse(scenario == "C", "Crop only", scenario),
           scenario = ifelse(scenario == "L", "Labor only", scenario)) %>%
    return()
}


# subset scenarios ----
SCE_C <- "C_lpjml_HadGEM"
SCE_CL <- "CL_lpjml_HS2_HadGEM"
SCE_L <- "L_HS2_HadGEM" 
SCE_ref <- "ref"

CompareDiff <- function(.df){
  .df %>% filter(scenario %in% c(SCE_C, SCE_CL, SCE_L, SCE_ref)) %>%
    mutate(scenario = ifelse(grepl("CL_", scenario), "CL", scenario),
           scenario = ifelse(grepl("C_", scenario), "C", scenario),
           scenario = ifelse(grepl("L_HS", scenario), "L", scenario),
           scenario = ifelse(grepl("ref", scenario), "Ref", scenario)) %>% 
    group_by_at(vars(-scenario, -value)) %>%
    mutate(value = value - value[scenario == "Ref"]) %>%
    filter(scenario != "Ref")
}

CompareDiffR <- function(.df){
  .df %>% filter(scenario %in% c(SCE_C, SCE_CL, SCE_L, SCE_ref)) %>%
    mutate(scenario = ifelse(grepl("CL_", scenario), "CL", scenario),
           scenario = ifelse(grepl("C_", scenario), "C", scenario),
           scenario = ifelse(grepl("L_HS", scenario), "L", scenario),
           scenario = ifelse(grepl("ref", scenario), "Ref", scenario)) %>% 
    group_by_at(vars(-scenario, -value)) %>%
    mutate(value = value / value[scenario == "Ref"]) %>%
    filter(scenario != "Ref")
}

# Q, glb, Mt ----
AgMeatPrice %>% 
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>% 
  group_by(scenario, year, sector) %>% 
  summarise(value = sum(Production)) %>% 
  CompareDiff() ->
  Comp.q.glb.sec

Comp.q.glb.sec %>% 
  filter(year >= 2015) %>% 
  SCE_NM() %>% 
  ggplot() +
  geom_smooth(aes(x = year, y = value, color = scenario), se = F) +
  # geom_line(aes(x = year, y = value, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ sector, ncol = 5) +
  labs(x = "", y = "Relative to Ref (Mt)", title = "Changes in global agricultural production due to heat stress",
       color = "Heat stress \nimpact") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 ->
  P4.0; P4.0

# Q, glb, % ----
AgMeatPrice %>% 
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>% 
  group_by(scenario, year, sector) %>% 
  summarise(value = sum(Production)) %>% 
  CompareDiffR() ->
  Comp.q.glb.sec.pct

Comp.q.glb.sec.pct %>% 
  filter(year >= 2015) %>% 
  SCE_NM() %>% 
  ggplot() +
  geom_smooth(aes(x = year, y = 100*(value-1), color = scenario), se = F) +
  # geom_line(aes(x = year, y = value, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ sector, ncol = 5) +
  labs(x = "", y = "Relative to Ref (%)", title = "Changes in global agricultural production due to heat stress",
       color = "Heat stress \nimpact") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 ->
  P4.1; P4.1

# P, glb, % ----

PAgPrice %>% 
  group_by_at(vars(scenario, branch, year, sector)) %>% 
  summarise(value = weighted.mean(value, w = prod), prod = sum(prod), .groups = "drop") %>%
  drop_na() %>% 
  select(scenario, sector, year, value) %>% 
  CompareDiffR() ->
  Comp.p.glb.sec

Comp.p.glb.sec %>% 
  filter(year >= 2015) %>% 
  SCE_NM() %>% 
  ggplot() +
  geom_smooth(aes(x = year, y = 100*(value-1), color = scenario), se = F) +
  # geom_line(aes(x = year, y = value, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ sector, ncol = 5) +
  labs(x = "", y = "Relative to Ref (%)", title = "Changes in price due to heat stress",
       color = "Heat stress \nimpact") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 ->
  P4.2; P4.2

# Q, reg, Mt ----

AgMeatPrice %>% 
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>% 
  group_by(scenario, year, region, sector) %>% 
  summarise(value = sum(Production)) %>% 
  CompareDiff() ->
  Comp.q.reg.sec

Comp.q.reg.sec %>% 
  filter(year == 2100) %>% 
  SCE_NM() %>% 
  short_name() %>% 
  ggplot() +
  geom_bar(aes(x = scenario, y = value, fill = sector), stat = "identity", position = "stack") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  labs(x = "", y = "Relative to Ref (Mt)", title = "Changes in global agricultural production due to heat stress",
       color = "Heat stress \nimpact") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 ->
  P4.0.reg; P4.0.reg


P4.0.reg %>% Write_png(.name = "Comp.Q.reg", .DIR_MODULE = DIR_MODULE, h = 8, w = 16)


## line comparison ----
Comp.q.reg.sec %>% 
  bind_rows(Comp.q.glb.sec %>% mutate(region = 'World')) %>% 
  filter(region %in% c("World", "USA", "India", "Africa_Western", "Southeast Asia" )) %>% 
  SCE_NM() %>% 
  short_name() -> df.comp.q

df.comp.q %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(sector ~ region, scales = "free_y") +
  labs(x = "", y = "Relative to Ref (Mt)", title = "Changes in global agricultural production due to heat stress",
       color = "Heat stress \nimpact") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 ->
  P4.Mt; P4.Mt

P4.Mt %>% Write_png(.name = "Comp.Q.reg.line", .DIR_MODULE = DIR_MODULE, h = 8, w = 16)

# Q, reg, % ----

AgMeatPrice %>% 
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>% 
  group_by(scenario, year, region, sector) %>% 
  summarise(value = sum(Production)) %>% 
  CompareDiffR() ->
  Comp.q.reg.sec.pct

## bar comparison ----
Comp.q.reg.sec.pct %>% 
  bind_rows(Comp.q.glb.sec.pct %>% mutate(region = 'World')) %>% 
  filter(year == 2100,
         sector == "Staple crops") %>% 
  SCE_NM() %>% 
  short_name() -> df.comp.q

df.comp.q %>% 
  mutate(class = ifelse(scenario == "Crop & Labor", "Combined", "Individual")) %>% 
  ggplot() +
  geom_bar(aes(x = class, y = 100*(value-1), fill = scenario), 
           stat = "identity", position = "stack") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  labs(x = "", y = "Relative to Ref (%)", title = "Changes in global agricultural production due to heat stress",
       color = "Heat stress \nimpact") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 ->
  P4.1.reg; P4.1.reg

P4.1.reg %>% Write_png(.name = "Comp.Q.pct.reg", .DIR_MODULE = DIR_MODULE, h = 8, w = 16)

## line comparison ----
Comp.q.reg.sec.pct %>% 
  bind_rows(Comp.q.glb.sec.pct %>% mutate(region = 'World')) %>% 
  filter(region %in% c("World", "USA", "India", "Africa_Western", "Southeast Asia" )) %>% 
  SCE_NM() %>% 
  short_name() -> df.comp.q

df.comp.q %>% 
  ggplot() +
  geom_line(aes(x = year, y = 100*(value-1), color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(sector ~ region, scales = "free_y") +
  labs(x = "", y = "Relative to Ref (%)", title = "Changes in global agricultural production due to heat stress",
       color = "Heat stress \nimpact") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 ->
  P4.Q.pct; P4.Q.pct

P4.Q.pct %>% Write_png(.name = "Comp.Q.pct.line", .DIR_MODULE = DIR_MODULE, h = 8, w = 16)


# P, reg, % ----

PAgPrice %>% 
  group_by_at(vars(scenario, branch, region, year, sector)) %>% 
  summarise(value = weighted.mean(value, w = prod), prod = sum(prod), .groups = "drop") %>%
  drop_na() %>% 
  select(scenario, region, sector, year, value) %>% 
  CompareDiffR() ->
  Comp.p.reg.sec

Comp.p.reg.sec %>% 
  filter(year >= 2015) %>% 
  SCE_NM() %>% 
  ggplot() +
  geom_smooth(aes(x = year, y = 100*(value-1), color = scenario), se = F) +
  # geom_line(aes(x = year, y = value, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ sector, ncol = 5) +
  labs(x = "", y = "Relative to Ref (%)", title = "Changes in price due to heat stress",
       color = "Heat stress \nimpact") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 ->
  P4.2.reg; P4.2.reg

P4.2.reg %>% Write_png(.name = "Comp.P.pct.reg", .DIR_MODULE = DIR_MODULE, h = 8, w = 16)

# crop value % ----

PAgPrice %>% 
  filter(sector == "Staple crops") %>% 
  group_by_at(vars(scenario, branch, region, year, sector)) %>% 
  summarise(revenue = sum(value*prod), 
            prod = sum(prod), 
            .groups = "drop") %>% 
  mutate(P = revenue / prod) %>% 
  drop_na() %>% 
  gather(var, value, revenue:P) %>% 
  select(scenario, region, sector, year, var, value) %>% 
  CompareDiffR() -> 
  Comp.staple.reg

PAgPrice %>% 
  filter(sector == "Staple crops") %>% 
  group_by_at(vars(scenario, branch, year, sector)) %>% 
  summarise(revenue = sum(value*prod), 
            prod = sum(prod), 
            .groups = "drop") %>% 
  mutate(P = revenue / prod) %>% 
  drop_na() %>% 
  mutate(region = "World") %>% 
  gather(var, value, revenue:P) %>% 
  select(scenario, region, sector, year, var, value) %>% 
  CompareDiffR() ->
  Comp.staple.glb

Comp.staple.reg %>% 
  bind_rows(Comp.staple.glb) %>% 
  SCE_NM() %>% 
  filter(year >= 2015) %>% 
  mutate(var = ifelse(var == "P", "Price", var),
         var = ifelse(var == "prod", "Production", var),
         var = ifelse(var == "revenue", "Output value", var)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = 100*(value -1 ), color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(var ~ region) +
  labs(x = "", y = "Relative to Ref (%)", title = "Changes in staples due to heat stress",
       color = "Heat stress \nimpact") +
  scale_color_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 + theme(legend.position = "bottom") ->
  P4.staples; P4.staples

P4.staples %>% Write_png(.name = "Comp.staples", .DIR_MODULE = DIR_MODULE, h = 10, w = 18)
