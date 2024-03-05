# Fig 2

DIR_MODULE = "HeatStress"

ListV2024 %>% names()

REG_1 <- c("World", "AFRICA", "CHINA+", "INDIA+", "NORTH_AM")
COMMSector <- c("Energy crops", "Staple crops", "Oil crops", #"Fodder crops",
                "Other crops", "Livestock", "Forest")

# SCENARIO <- c("L_HS1_GFDL", "L_HS1_HadGEM", "L_HS2_GFDL", "L_HS2_HadGEM") 
SCENARIO <- Scenario

PluckBind <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    filter(scenario %in% SCENARIO) %>%
    rename(region0 = region) %>%
    # mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
    #                          labels = c("Evolving", "Static")) %>% 
    left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6))
}

PluckBind32 <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    filter(scenario %in% SCENARIO)
}


# Get data ready ----

## Ag labor by sector ----
PluckBind("LaborDemandSec") %>% 
  Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(
    PluckBind("LaborDemandSec") %>%
      Agg_reg(sector, region)
  ) %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM3), by = "sector"
  ) %>%
  Agg_reg(sector = AgCOMM3, region) %>%
  mutate(sector = factor(sector, levels = COMMSector)) %>%
  Agg_reg(sector, region)%>%
  filter(year >= 2015) -> Plabor

PluckBind32("LaborDemandSec") %>% 
  Agg_reg(sector) %>% mutate(region = "World") %>% 
  bind_rows(
    PluckBind32("LaborDemandSec") %>%
      Agg_reg(sector, region)
  ) %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM3), by = "sector"
  ) %>%
  Agg_reg(sector = AgCOMM3, region) %>%
  mutate(sector = factor(sector, levels = COMMSector)) %>%
  Agg_reg(sector, region)%>%
  filter(year >= 2015) -> Plabor32

## ag wage rate ----

"LaborPrice" %>% PluckBind32() %>% 
  mutate(region = gsub("Labor_Ag", "", market)) ->
  W32

Plabor32 %>% select(scenario, region, year, L = value) %>% 
  filter(region != "World") %>% 
  left_join_error_no_match(W32 %>% select(scenario, region, year, W = value), 
                           by = c("scenario", "region", "year")) %>% 
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6)) %>% 
  mutate(EXP = W*L) %>% 
  group_by(scenario, region, year) %>% 
  summarise(L = sum(L),
            EXP = sum(EXP)) %>% 
  mutate(W = EXP / L) -> PWage

## Ag land by sector ----
PluckBind("Aggland") %>% filter(year >= 2015) %>%
  left_join_error_no_match(LandMapping %>% select(LandLeaf, land = LandCover3), by = "LandLeaf") %>%
  group_by(scenario, region, land, year, branch) %>%
  # to Mha
  summarise(value = sum(value)/10, .groups = "drop") %>%
  Agg_reg(land, region) %>%
  mutate(land = factor(land, levels = c(
    "Cropland - Energy", "Cropland - Staples", "Cropland - Others",
    "Forest - Managed", "Forest - Unmanaged",
    "Pasture - Managed", "Pasture - Unmanaged",
    "Other Natural", "Other Fixed" ) )) %>% filter(year >= 2015) %>%
  filter(!grepl("Rock|Urban", land)) %>%
  #mutate(land = gsub("- Staples|- Others", "- NonEnergy", land)) %>%
  mutate(land = gsub(" - Unmanaged| - Managed", "", land)) %>%
  mutate(land = gsub("Fixed", "Land", land)) %>%
  mutate(land = factor(land, levels = c(
    "Cropland - Energy", "Cropland - Staples", "Cropland - Others",
    "Forest", "Pasture", "Other Natural", "Other Land" ),
    labels = c(
      "Cropland: Energy", "Cropland: Staples", "Cropland: Others",
      "Forest", "Pasture", "Other natural", "Other land" ))) %>%
  group_by_at(vars(-value)) %>% summarise(value = sum(value), .groups = "drop") -> Pland

Pland %>% Agg_reg(land) %>% mutate(region = "World") %>%
  bind_rows(Pland) -> Pland

## Ag water by sector ----
"Water_Wsector" %>% PluckBind %>% filter(year >= 2015) %>% filter(value >0) %>%
  mutate(sector1 = if_else(sector %in% MapAgCOMM$AgCOMM, "Ag", sector)) %>%
  mutate(sector1 = if_else(grepl("elec", sector1), "Elec.", sector1)) %>%
  mutate(sector1 = if_else(grepl("Ag|Elec", sector1), sector1, "Others")) %>%
  filter(sector1 == "Ag") %>%
  left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, sector2 = AgCOMM3), by = "sector"
  ) %>%
  mutate(sector2 = factor(sector2, levels = COMMSector)) %>%
  Agg_reg(region, sector = sector2) -> Pwater

Pwater %>% Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(Pwater) -> Pwater

## Ag Fertilizer ----
"FertizerUse" %>% PluckBind %>% filter(year >= 2015) %>% filter(sector != "Exports_fertilizer") %>%
  left_join(MapAgCOMM %>% select(sector = AgCOMM, sector2 = AgCOMM3)) %>%
  mutate(sector2 = factor(sector2, levels = COMMSector)) %>%
  Agg_reg(region, sector = sector2) -> Pfertilizer
Pfertilizer %>% Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(Pfertilizer) -> Pfertilizer


## Emissions ----

# source("R/Emissions.R")

# ## CO2 ----
# pCEM %>% Agg_reg(sector) %>% mutate(region = "World") %>%
#   bind_rows(pCEM) -> pCEM
# ## nonCO2 GHG ----
# pNCEM %>% Agg_reg(sector) %>% mutate(region = "World") %>%
#   bind_rows(pNCEM) -> pNCEM

## Ag SUA & prices----
### source AgBalElement here ----

source("R/AgBalElement_Storage.R")

c(brewer.pal(n = length(ELEMAll), name = "BrBG")[1:length(ElEMSupply)],
  brewer.pal(n = length(ELEMAll), name = "BrBG")[length(ElEMSupply)+1:length(ELEMDemand)]
) -> ColUpdate


PSUA %>% Agg_reg(sector, element) %>% mutate(region = "World") %>%
  bind_rows(PSUA) -> PSUA

PAgPrice %>%
  group_by_at(vars(scenario, year, branch, sector)) %>%
  summarise(value = weighted.mean(value, w = prod), prod = sum(prod), .groups = "drop") %>%
  drop_na() %>% mutate(region = "World") %>%
  bind_rows(PAgPrice) %>%
  mutate(sector = factor(sector, levels = COMMSector)) ->
  PAgPrice

## Ag prod, use base year price as weight ----

# PAgPrice %>% filter(year == 2015) %>% 
#   select(scenario, region, sector, weight = value) -> weights
# 
# PAgPrice %>% 
#   left_join_error_no_match(weights, by = c("scenario", "region", "sector")) %>% 
#   mutate(value = weight * prod) -> # $/kg * Mt = bil 1975$
#   VAG

## Food demand ----

# "Fooddemand" %>% PluckBind() -> 
#   FoodDemand

"Fooddemandca" %>% PluckBind32() %>% 
  Agg_reg(region, input)->
  Fooddemandca #Kcal/per/day
  
"POP" %>% PluckBind32() %>% 
  Agg_reg(region)->
  POP  # thousand ppl

Fooddemandca %>% rename(Kcalpc = value) %>% 
  left_join(POP %>% rename(thous = value), by = c("scenario", "region", "year")) %>% 
  mutate(Kcal = Kcalpc * 1000 * thous ) %>% 
  group_by(scenario, region, input, year) %>% 
  summarise(Kcal = sum(Kcal),
            POP = sum(thous)*1000) ->
  FD32


FD32 %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_AR6),
                           by = "region") %>% 
  mutate(region = REG10_AR6) %>% 
  select(-REG10_AR6) %>% 
  group_by(scenario, region, input, year) %>% 
  summarise(Kcal = sum(Kcal),
            POP = sum(POP)) %>% 
  mutate(fdpc = Kcal / POP) %>% 
  select(scenario, region, input, year, value = fdpc) ->
  FD
  
## Trade ----

"TradedAgsource" %>% PluckBind() %>% 
  filter(!input %in% c("natural gas", "gas trade statistical differences",
                       "coal", "iron and steel", "crude oil" )) -> check

## Food price ----
"Foodprice" %>% PluckBind32() -> FoodPrice # 2005$/Mcal/day
 
"FoodConsumption" %>% PluckBind32() -> FoodCon # Pcal

FoodCon %>% select(scenario, region, sector, year, con = value) %>% 
  left_join_error_no_match(FoodPrice %>% select(scenario, region, sector = input, year, P = value),
                           by = c("scenario", "region", "sector", "year")) %>% 
  mutate(EXP = P * con) %>%  
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6)) %>% 
  group_by(scenario, region, year) %>% 
  summarise(EXP = sum(EXP),
            con = sum(con)) %>% 
  mutate(P = EXP / con) ->
  PFood

PFood %>% select(-EXP, -con) %>% filter(year >= 2015) %>% 
  left_join_error_no_match(PWage %>% select(-EXP, -L),
                           by = c("scenario", "region", "year")) %>%
  group_by(value = W / P) %>% 
  select(-P, -W) ->
  PFPP
  

## food purchase power ----

FPP <- Wage / PFood 

# Ag capital by sector ----
"CapitalDemandSec" %>% PluckBind() %>% 
  Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(
    PluckBind("CapitalDemandSec") %>%
      Agg_reg(sector, region)
  ) %>%
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM3), by = "sector"
  ) %>%
  Agg_reg(sector = AgCOMM3, region) %>%
  mutate(sector = factor(sector, levels = COMMSector)) %>%
  Agg_reg(sector, region) %>%
  filter(year >= 2015) %>%
  mutate(value = value * gdp_deflator(2015, 1975))-> Pcapital

# Reference plot ----

SCE_NAME <- "C_lpjml_HadGEM"

ProcScen <- function(.df){
  .df %>% filter(scenario == SCE_NAME)
}

## theme1 ----
theme1 <- theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.grid = element_blank(),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"))

## Land ----
Pland %>% Agg_reg(land, region) %>%
  ProcScen() %>%
  mutate(value = value / 1000) %>%
  ggplot +   facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = land), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Billion hectare", fill = "Land") +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  theme_bw() + theme0 + theme1  -> A1; A1

## Labor ----
Plabor %>% #filter(region != "World") %>%
  ProcScen() %>%
  ggplot + facet_wrap(~region, scale = "free_y", nrow = 1) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Million people", fill = "Sector") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 -> A2; A2

## Capital ----
Pcapital %>% #filter(region != "World") %>%
  mutate(value = value / 1000) %>%
  ProcScen() %>%
  ggplot + facet_wrap(~region, scale = "free_y", nrow = 1) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Trillion 2015$") +
  scale_fill_brewer(palette = "Set2", name = "Sector", direction = 1) +
  theme_bw() + theme0 + theme1 -> A3; A3

## Water ----
Pwater %>% ProcScen() %>%  mutate(value = value / 1000) %>%
  ggplot +   facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Trillion cubic meter") +
  scale_fill_brewer(palette = "Set2",
                    name = "Sector", direction = 1) +
  theme_bw() + theme0 + theme1 -> A4; A4

## Fertilizer ----
Pfertilizer %>%   ProcScen() %>%
  ggplot + facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Million tonne N") +
  scale_fill_brewer(palette = "Set2",
                    name = "Sector", direction = 1) +
  theme_bw() + theme0 + theme1 -> A5; A5


## Ag SUA ----

PSUA %>% head


PSUA %>% 
  filter(sector %in% tolower(c("Corn", "Wheat", "Rice", "OtherGrain", "RootTuber"))) %>% 
  Agg_reg(element, region) %>% 
  ProcScen() %>% 
  mutate(value = value /1000) %>% 
  ggplot +
  
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  #facet_grid(~scenario, scales = "free_y") +
  facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack",
           color = "black") +
  scale_color_manual(values = "black") +
  labs(x = "Year", y = "Billion tonne", fill = "Element") +
  scale_fill_manual(values = c("Supply: Production" = ColUpdate[1],
                               "Supply: Import" = ColUpdate[2], "Demand: Export" = ColUpdate[3],
                               "Demand: Food" = ColUpdate[4],"Demand: Feed" = ColUpdate[5], "Demand: Bioenergy" = ColUpdate[6],
                               "Demand: Other use" = ColUpdate[7]) ) +
  theme_bw() + theme0 + theme1 -> A6; A6

PSUA %>%
  filter(sector %in% tolower(c("Beef"))) %>% #, "SheepGoat"
  Agg_reg(element, region) %>%
  ProcScen() %>%
  mutate(value = value) %>%
  ggplot +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  #facet_grid(~scenario, scales = "free_y") +
  facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack",
           color = "black") +
  scale_color_manual(values = "black") +
  labs(x = "Year", y = "Million tonne", fill = "Element") +
  scale_fill_manual(values = c("Supply: Production" = ColUpdate[1],
                               "Supply: Import" = ColUpdate[2], "Demand: Export" = ColUpdate[3],
                               "Demand: Food" = ColUpdate[4],"Demand: Feed" = ColUpdate[5], "Demand: Bioenergy" = ColUpdate[6],
                               "Demand: Other use" = ColUpdate[7]) ) +
  theme_bw() + theme0 + theme1 -> A7; A7

## Ag prices ----

PAgPrice %>% 
  ProcScen() %>% 
  Proc_Diff(type = "R", -year, -prod) %>%
  ggplot +
  facet_wrap(~region, nrow = 1, scales = "fixed") +
  geom_hline(yintercept = 1) +
  geom_line(aes(x=year, y=value, color=sector), size = 1.4) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  #scale_color_brewer(palette = "Set1", direction = 1) +
  #scale_color_npg() +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  labs(x = "Year", y = "Index (2015 = 1)", color = "Sector") +
  theme_bw() + theme0 + theme1  -> A8; A8


## Food demand  ---- 
# per capita per day

FD %>% ProcScen() %>% 
  # filter(region %in% c("AFRICA", "CHINA+", "INDIA+", "REST_ASIA", "NORTH_AM", "World")) %>% 
  filter(year >= 2015) %>% 
  ggplot() +
  facet_wrap(~region, nrow = 1, scales = "fixed") +
  geom_hline(yintercept = 1) +
  geom_line(aes(x=year, y=value, color=input), size = 1.4) +
  labs(x = "",y = "Kcal/person/day") +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 +
  theme(legend.position = "bottom") -> A9; A9

A9 %>% Write_png(.name = paste0("Food demand_ref"), .DIR_MODULE = DIR_MODULE, h = 8, w = 16)

## Carbon emissions ----


# pCEM %>%  ProcScen() %>%
#   group_by(scenario, region, sector) %>%
#   Fill_annual(CUMULATIVE = T) %>% mutate(value = value / 1000) %>%
#   ungroup() ->
#   pCEM1
# 
# pCEM1 %>%
#   ggplot + facet_wrap(~region, nrow = 1, scales = "free") +
#   guides(colour = guide_legend(order = 2),
#          fill = guide_legend(order = 1)) +
#   geom_hline(yintercept = 0) +
#   geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
#             color = "black", size = 0.4) +
#   geom_line(data = pCEM1 %>% Agg_reg(region) %>%  mutate(ss = "Net Total"),
#             aes(x = year, y = value, color = ss ), size = 1.2, linetype = 2) +
#   labs(x = "Year", y = expression(paste("Trillion ", tCO[2])), fill = "Sector", color = "") +
#   scale_fill_brewer(palette = "RdBu", direction = -1) +
#   scale_color_manual(values = "red") +
#   scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
#   theme_bw() + theme0 + theme1  -> A9; A9
# 
# ## NonCO2 GHG emissions ----
# 
# pNCEM %>% ProcScen() %>%
#   group_by(scenario, region, sector) %>%
#   Fill_annual(CUMULATIVE = T) %>% mutate(value = value) %>%
#   ungroup() ->
#   pNCEM1
# 
# pNCEM1 %>%
#   ggplot + facet_wrap(~region, nrow = 1, scales = "free") +
#   geom_hline(yintercept = 0) +
#   geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
#             color = "black") +
#   labs(x = "Year", fill = "Source",
#        y = expression(paste(GtCO[2]-eq))) +
#   scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
#   scale_fill_brewer(palette = "Set1", direction = -1,
#                     labels = c(expression(paste(CH[4], " Agriculture")),
#                                expression(paste(CH[4], " Energy")),
#                                expression(paste(CH[4], " Unmanaged Land")),
#                                expression(paste(N[2], "O Agriculture")),
#                                expression(paste(N[2], "O Energy")),
#                                expression(paste(CH[4], " Unmanaged Land")),
#                                "Other GHGs")) +
#   theme_bw() + theme0 + theme1 -> A10; A10



# (A1 + ggtitle("(A) Land cover and use by sector and region")+ labs(fill = "Land (Panel A)")+theme(axis.title.x = element_blank(), legend.position = "right") )/
#   (A2 + ggtitle("(B) Agricultural labor input by sector and region") + theme(axis.title.x = element_blank(), legend.position = "right")  + labs(fill = "Sector (Panels B-E)"))/
#   (A3 + ggtitle("(C) Agricultural capital input by sector and region") + theme(axis.title.x = element_blank(), legend.position = "none") )/
#   (A4 + ggtitle("(D) Agricultural water withdrawal by sector and region") + theme(axis.title.x = element_blank(),legend.position = "none")) /
#   (A5 + ggtitle("(E) Fertilizer use by sector and region") + theme(axis.title.x = element_blank(), legend.position = "none")) /
#   (A6 + ggtitle("(F) Supply utilization accounts for staple crops by region") + theme(axis.title.x = element_blank(), legend.position = "right") + labs(fill = "SUA element (Panel F-G)"))/
#   (A7 + ggtitle("(G) Supply utilization accounts for beef products by region") + theme(axis.title.x = element_blank(), legend.position = "none") )/
#   (A8 + ggtitle("(H) Agricultural prices by sector and region") + theme(axis.title.x = element_blank(),legend.position = "right")+ labs(color = "Sector (Panel H)")) +
#   (A9 + ggtitle("(I) Cumulative carbon dioxide emissions by sector and region") + theme(axis.title.x = element_blank(),legend.position = "right")+ labs(fill = "Sector (Panel I)")) +
#   (A10 + ggtitle("(J) Cumulative non-carbon dioxide GHG emissions by sector and region") + theme(legend.position = "right")+ labs(fill = "Sector (Panel J)")) +
#   patchwork::plot_layout(guides = "collect", heights = rep(1, 10)) -> pp
# 
# pp %>% Write_png(.name = "Summary", .DIR_MODULE = DIR_MODULE, h = 24, w = 22)

(A1 + ggtitle("(A) Land cover and use by sector and region")+ labs(fill = "Land (Panel A)")+theme(axis.title.x = element_blank(), legend.position = "right") )/
  (A2 + ggtitle("(B) Agricultural labor input by sector and region") + theme(axis.title.x = element_blank(), legend.position = "right")  + labs(fill = "Sector (Panels B-E)"))/
  (A3 + ggtitle("(C) Agricultural capital input by sector and region") + theme(axis.title.x = element_blank(), legend.position = "none") )/
  (A4 + ggtitle("(D) Agricultural water withdrawal by sector and region") + theme(axis.title.x = element_blank(),legend.position = "none")) /
  (A5 + ggtitle("(E) Fertilizer use by sector and region") + theme(axis.title.x = element_blank(), legend.position = "none")) /
  (A6 + ggtitle("(F) Supply utilization accounts for staple crops by region") + theme(axis.title.x = element_blank(), legend.position = "right") + labs(fill = "SUA element (Panel F-G)"))/
  (A7 + ggtitle("(G) Supply utilization accounts for beef products by region") + theme(axis.title.x = element_blank(), legend.position = "none") )/
  (A8 + ggtitle("(H) Agricultural prices by sector and region") + theme(axis.title.x = element_blank(),legend.position = "right")+ labs(color = "Sector (Panel H)")) +
  # (A9 + ggtitle("(I) Cumulative carbon dioxide emissions by sector and region") + theme(axis.title.x = element_blank(),legend.position = "right")+ labs(fill = "Sector (Panel I)")) +
  # (A10 + ggtitle("(J) Cumulative non-carbon dioxide GHG emissions by sector and region") + theme(legend.position = "right")+ labs(fill = "Sector (Panel J)")) +
  patchwork::plot_layout(guides = "collect", heights = rep(1, 8)) -> pp; pp

pp %>% Write_png(.name = paste0("Summary_", SCE_NAME), .DIR_MODULE = DIR_MODULE, h = 24, w = 22)











