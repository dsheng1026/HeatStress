# load library ----
library(sf)

# subset scenarios ----
SCE_C <- "C_lpjml_HadGEM"
SCE_CL <- "CL_lpjml_HS2_HadGEM"

ProcScenDiff <- function(.df){
  .df %>% filter(scenario %in% c(SCE_C, SCE_CL)) %>%
    mutate(scenario = ifelse(grepl("CL", scenario), "CL", "C")) %>% 
    group_by_at(vars(-scenario, -value)) %>%
    mutate(value = value - value[scenario == "C"]) %>%
    filter(scenario != "C")
}

ProcScenDiffR <- function(.df){
  .df %>% filter(scenario %in% c(SCE_C, SCE_CL)) %>%
    mutate(scenario = ifelse(grepl("CL", scenario), "CL", "C")) %>% 
    group_by_at(vars(-scenario, -value)) %>%
    mutate(value = value / value[scenario == "C"]) %>%
    filter(scenario != "C")
}


REG32 <- st_read("data/GCAM_boundary/main_outputs/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")

## Total Ag labor ----
PluckBind32("LaborDemandSec") %>%
      Agg_reg(region) -> Plabor32_all

PluckBind32("LaborDemandSec") %>%
  Agg_reg() %>% mutate(region = 'World') -> Plabor_all

# get numbers ----
Plabor_all %>% ProcScenDiff -> diff.glb.level
Plabor32_all %>% ProcScenDiff -> diff.reg.level
Plabor_all %>% ProcScenDiffR -> diff.glb.pct
Plabor32_all %>% ProcScenDiffR -> diff.reg.pct

Plabor32_all %>% group_by(scenario, year) %>% ProcScenDiff() %>% 
  group_by(scenario, year) %>% 
  mutate(total = sum(value),
         share = 100*value / total) ->
  diff.reg.share

# plot ----
REG32 %>% left_join(Plabor32_all %>% ProcScenDiff %>% rename(reg_nm = region) %>% filter(year == 2100), 
                                   by = "reg_nm") %>% 
  ggplot() +
  geom_sf(aes(fill = value)) +
  scale_fill_gradient2(low = "lightblue", high = "red", midpoint = 0) +
  coord_sf(datum = NA) +
  labs(title = "Labor employment change due to labor productivity loss: HadGEM2, high, 2100") +
  theme_minimal() +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60"))  +
  guides(fill = guide_colorbar(title = "Million people",
                               title.position = "top",
                               title.theme = element_text(size = 10,
                                                          face = "bold",
                                                          colour = "black",
                                                          angle = 0))) -> p1

p1 %>% Write_png(.name = paste0("labor change mpl 2100"), .DIR_MODULE = DIR_MODULE, h = 8, w = 16)

REG32 %>% left_join(Plabor32_all %>% ProcScenDiffR %>% rename(reg_nm = region) %>% filter(year == 2100), 
                    by = "reg_nm") %>% 
  ggplot() +
  geom_sf(aes(fill = 100*(value - 1))) +
  scale_fill_gradient2(low = "green", high = "purple", midpoint = 0) +
  coord_sf(datum = NA) +
  labs(title = "Labor employment change due to labor productivity loss: HadGEM2, high, 2100") +
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
                                                          angle = 0))) -> p2; p2

p2 %>% Write_png(.name = paste0("labor change pct 2100"), .DIR_MODULE = DIR_MODULE, h = 8, w = 16)

(p1 + ggtitle("(A) Labor change (million person)")+ labs(fill = "mpl")+theme(axis.title.x = element_blank(), legend.position = "right") )/
  (p2 + ggtitle("(B) Labor change (%)") + theme(axis.title.x = element_blank(), legend.position = "right")  + labs(fill = "%"))/
    patchwork::plot_layout(guides = "collect", heights = rep(1, 2)) -> pp; pp

pp %>% Write_png(.name = paste0("Fig2"), .DIR_MODULE = DIR_MODULE, h = 14, w = 16)

