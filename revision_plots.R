# revision:

"LaborPrice" %>% PluckBind() %>% 
  mutate(region = gsub("Labor_Ag", "", market)) %>% 
  select(scenario, region, year, thousand_ppl1975 = value) ->
  Wage

PluckBind("LaborDemandSec") %>% 
  group_by(scenario, region, year) %>% 
  summarise(mpl = sum(value), .groups = "drop") -> Labor

Labor %>% left_join_error_no_match(Wage, by = c("scenario", "region", "year")) %>% 
  left_join_error_no_match(Regmapping, by = "region") %>% 
  select(scenario, REG10_main, year, thousand_ppl1975, mpl) %>% 
  group_by(scenario, REG10_main, year) %>%  
  summarise(wage = weighted.mean(thousand_ppl1975, w = mpl),
            labor = sum(mpl), .groups = "drop") %>%  
  rename(region = REG10_main) ->
  REG10_wage

REG10_wage %>% 
  group_by(scenario, year) %>% 
  summarise(wage = weighted.mean(wage, w = labor),
            labor = sum(labor), .groups = "drop") %>% 
  mutate(region = "World") %>% 
  bind_rows(REG10_wage) %>% 
  rename(value = wage) ->
  Agg_wage


Agg_wage %>% 
  filter(grepl("_gepic_", scenario),
         grepl("GFDL", scenario)) %>% 
  bind_rows(Agg_wage %>% filter(grepl("ref", scenario))) %>% 
  filter(year >= 2015) %>% 
  group_by(region, year) %>% 
  mutate(index = value / value[scenario == "ref"]) %>% 
  mutate(exp = ifelse(grepl("CL_", scenario), "Crop&Labor", scenario),
         exp = ifelse(grepl("C_", scenario), "Crop", exp)) %>% 
  mutate(LHR = NA,
         LHR = ifelse(grepl("HS1", scenario), "Low", LHR),
         LHR = ifelse(grepl("HS2", scenario), "High", LHR),
         GCM = "GFDL",
         GGCM = "gepic")  %>% 
  filter(scenario != "ref") %>% 
  bind_rows(Agg_wage %>% 
              filter(grepl("_gepic_", scenario),
                     grepl("HadGEM", scenario)) %>% 
              bind_rows(Agg_wage %>% filter(grepl("ref", scenario))) %>% 
              filter(year >= 2015) %>% 
              group_by(region, year) %>% 
              mutate(index = value / value[scenario == "ref"]) %>% 
              mutate(exp = ifelse(grepl("CL_", scenario), "Crop&Labor", scenario),
                     exp = ifelse(grepl("C_", scenario), "Crop", exp)) %>% 
              mutate(LHR = NA,
                     LHR = ifelse(grepl("HS1", scenario), "Low", LHR),
                     LHR = ifelse(grepl("HS2", scenario), "High", LHR),
                     GCM = "HadGEM",
                     GGCM = "gepic")) %>% 
  filter(scenario != "ref") %>% 
  bind_rows(Agg_wage %>% 
              filter(grepl("_lpjml_", scenario),
                     grepl("GFDL", scenario)) %>% 
              bind_rows(Agg_wage %>% filter(grepl("ref", scenario))) %>% 
              filter(year >= 2015) %>% 
              group_by(region, year) %>% 
              mutate(index = value / value[scenario == "ref"]) %>% 
              mutate(exp = ifelse(grepl("CL_", scenario), "Crop&Labor", scenario),
                     exp = ifelse(grepl("C_", scenario), "Crop", exp)) %>% 
              mutate(LHR = NA,
                     LHR = ifelse(grepl("HS1", scenario), "Low", LHR),
                     LHR = ifelse(grepl("HS2", scenario), "High", LHR),
                     GCM = "GFDL",
                     GGCM = "lpjml") %>% 
              filter(scenario != "ref") %>% 
              bind_rows(Agg_wage %>% 
                          filter(grepl("_lpjml_", scenario),
                                 grepl("HadGEM", scenario)) %>% 
                          bind_rows(Agg_wage %>% filter(grepl("ref", scenario))) %>% 
                          filter(year >= 2015) %>% 
                          group_by(region, year) %>% 
                          mutate(index = value / value[scenario == "ref"]) %>% 
                          mutate(exp = ifelse(grepl("CL_", scenario), "Crop&Labor", scenario),
                                 exp = ifelse(grepl("C_", scenario), "Crop", exp)) %>% 
                          mutate(LHR = NA,
                                 LHR = ifelse(grepl("HS1", scenario), "Low", LHR),
                                 LHR = ifelse(grepl("HS2", scenario), "High", LHR),
                                 GCM = "HadGEM",
                                 GGCM = "lpjml"))) -> 
  wage.plot
  
wage.plot %>% 
  filter(is.na(LHR) | LHR == "High")  %>% 
  # filter(year %in% c(2015, 2025, 2050, 2075, 2100)) %>% 
  filter(scenario != "ref") %>% 
  # mutate(Model = paste0(GCM, "_", GGCM)) %>% 
  short_name() %>% 
  ggplot() +
  geom_hline(aes(yintercept = 1)) +
  geom_line(aes(x = year, y = index, color = exp, linetype = GGCM)) +
  facet_grid(GCM~ region) +
  labs(x = "", y ="Agricultural wage relative to NoCC (NoCC = 1)", 
       fill = "Ag wage relative to NoCC (NoCC = 1)",
       color = "Experiment", linetype = "GGCM",
       title = "High labor-response function") +
  theme_bw() + theme0 + theme1 
  theme(legend.position = "none") ->
  Wage_high_index


wage.plot %>% 
  filter(is.na(LHR) | LHR == "Low")  %>% 
  # filter(year %in% c(2015, 2025, 2050, 2075, 2100)) %>% 
  filter(scenario != "ref") %>% 
  mutate(Model = paste0(GCM, "_", GGCM)) %>% 
  short_name() %>% 
  ggplot() +
  geom_hline(aes(yintercept = 1))+
  geom_line(aes(x = year, y = index, color = exp, linetype = GGCM)) +
  facet_grid(GCM~ region) +
  labs(x = "", y ="Agricultural wage relative to NoCC (NoCC = 1)", 
       fill = "Ag wage relative to NoCC (NoCC = 1)",
       color = "Experiment", linetype = "GGCM",
       title = "High labor-response function") +
  theme_bw() + theme0 + theme1 +
theme(legend.position = "bottom") ->
  Wage_low_index

(Wage_high_index + ggtitle("(A) High labor-response function"))/
  (Wage_low_index + ggtitle("(B) Low labor-response function")) -> p


p %>% Write_png(.name = paste0("Rev_Wage_combined"), .DIR_MODULE = DIR_MODULE, h = 10, w = 14)

unique(wage.plot$scenario)

wage.plot %>% filter(scenario %in% c("ref", "CL_gepic_HS2_HadGEM", "CL_gepic_HS1_GFDL",
                                     "C_gepic_HadGEM", "C_gepic_GFDL")) ->
  df.rev.wage

# wage.plot %>% 
#   # filter(is.na(LHR) | LHR == "Low")  %>% 
#   # filter(GGCM == "gepic", GCM == "HadGEM") %>% 
#   # filter(year %in% c(2015, 2025, 2050, 2075, 2100)) %>% 
#   filter(scenario != "ref") %>% 
#   mutate(Model = paste0(GCM, "_", GGCM, "_", exp)) %>% 
#   short_name() %>% 
#   ggplot() +
#   geom_hline(aes(yintercept = 1))+
#   geom_line(aes(x = year, y = index, color = Model, linetype = LHR)) +
#   facet_wrap(~ region, ncol = 8) +
#   labs(x = "", y ="Agricultural wage relative to NoCC (NoCC = 1)", 
#        fill = "Ag wage relative to NoCC (NoCC = 1)",
#        color = "Model", linetype = "Experiment",
#        title = "Low labor-response function") +
#   theme_bw() + theme0 + theme1 +
#   theme(legend.position = "bottom") 


# read in material wage
"NationalAccount" %>% PluckBind() %>% 
  filter(Account == "labor-wages") %>% 
  select(scenario, region, Account, year, value) %>% 
  bind_rows("NationalAccount" %>% PluckBind() %>% 
              filter(Account == "labor-force") %>% 
              select(scenario, region, Account, year, value)) %>% 
  left_join_error_no_match(Regmapping, by = "region") %>% 
  spread(Account, value) %>% 
  group_by(scenario, REG10_HS, year) %>% 
  summarise(wage = weighted.mean(`labor-wages`, w = `labor-force`),
            labor = sum(`labor-force`), .groups = "drop") %>% 
  rename(region = REG10_HS) ->
  Wage_MA_reg10 # labor force: million people, 

Wage_MA_reg10 %>% 
  group_by(scenario, year) %>% 
  summarise(wage = weighted.mean(wage, w = labor),
            labor = sum(labor), .groups = "drop") %>% 
  mutate(region = "World") %>% bind_rows(Wage_MA_reg10) %>% 
  filter(year >= 2015) %>% 
  select(scenario, year, value = wage, region) ->
  Agg_wage

  
  Agg_wage %>% 
  filter(grepl("_gepic_", scenario),
         grepl("GFDL", scenario)) %>% 
  bind_rows(Agg_wage %>% filter(grepl("ref", scenario))) %>% 
  filter(year >= 2015) %>% 
  group_by(region, year) %>% 
  mutate(index = value / value[scenario == "ref"]) %>% 
  mutate(exp = ifelse(grepl("CL_", scenario), "Crop&Labor", scenario),
         exp = ifelse(grepl("C_", scenario), "Crop", exp)) %>% 
  mutate(LHR = NA,
         LHR = ifelse(grepl("HS1", scenario), "Low", LHR),
         LHR = ifelse(grepl("HS2", scenario), "High", LHR),
         GCM = "GFDL",
         GGCM = "gepic")  %>% 
  filter(scenario != "ref") %>% 
  bind_rows(Agg_wage %>% 
              filter(grepl("_gepic_", scenario),
                     grepl("HadGEM", scenario)) %>% 
              bind_rows(Agg_wage %>% filter(grepl("ref", scenario))) %>% 
              filter(year >= 2015) %>% 
              group_by(region, year) %>% 
              mutate(index = value / value[scenario == "ref"]) %>% 
              mutate(exp = ifelse(grepl("CL_", scenario), "Crop&Labor", scenario),
                     exp = ifelse(grepl("C_", scenario), "Crop", exp)) %>% 
              mutate(LHR = NA,
                     LHR = ifelse(grepl("HS1", scenario), "Low", LHR),
                     LHR = ifelse(grepl("HS2", scenario), "High", LHR),
                     GCM = "HadGEM",
                     GGCM = "gepic")) %>% 
  filter(scenario != "ref") %>% dim
  bind_rows(Agg_wage %>% 
              filter(grepl("_lpjml_", scenario),
                     grepl("GFDL", scenario)) %>% 
              bind_rows(Agg_wage %>% filter(grepl("ref", scenario))) %>% 
              filter(year >= 2015) %>% 
              group_by(region, year) %>% 
              mutate(index = value / value[scenario == "ref"]) %>% 
              mutate(exp = ifelse(grepl("CL_", scenario), "Crop&Labor", scenario),
                     exp = ifelse(grepl("C_", scenario), "Crop", exp)) %>% 
              mutate(LHR = NA,
                     LHR = ifelse(grepl("HS1", scenario), "Low", LHR),
                     LHR = ifelse(grepl("HS2", scenario), "High", LHR),
                     GCM = "GFDL",
                     GGCM = "lpjml") %>% 
              filter(scenario != "ref") %>% 
              bind_rows(Agg_wage %>% 
                          filter(grepl("_lpjml_", scenario),
                                 grepl("HadGEM", scenario)) %>% 
                          bind_rows(Agg_wage %>% filter(grepl("ref", scenario))) %>% 
                          filter(year >= 2015) %>% 
                          group_by(region, year) %>% 
                          mutate(index = value / value[scenario == "ref"]) %>% 
                          mutate(exp = ifelse(grepl("CL_", scenario), "Crop&Labor", scenario),
                                 exp = ifelse(grepl("C_", scenario), "Crop", exp)) %>% 
                          mutate(LHR = NA,
                                 LHR = ifelse(grepl("HS1", scenario), "Low", LHR),
                                 LHR = ifelse(grepl("HS2", scenario), "High", LHR),
                                 GCM = "HadGEM",
                                 GGCM = "lpjml"))) -> 
  wage.plot

wage.plot %>% 
  filter(is.na(LHR) | LHR == "High")  %>% 
  # filter(year %in% c(2015, 2025, 2050, 2075, 2100)) %>% 
  filter(scenario != "ref") %>% 
  # mutate(Model = paste0(GCM, "_", GGCM)) %>% 
  short_name() %>% 
  ggplot() +
  geom_hline(aes(yintercept = 1)) +
  geom_line(aes(x = year, y = index, color = exp, linetype = GGCM)) +
  facet_grid(GCM~ region) +
  labs(x = "", y ="Agricultural wage relative to NoCC (NoCC = 1)", 
       fill = "Ag wage relative to NoCC (NoCC = 1)",
       color = "Experiment", linetype = "GGCM",
       title = "High labor-response function") +
  theme_bw() + theme0 + theme1 
theme(legend.position = "none") ->
  Wage_high_index


wage.plot %>% 
  filter(is.na(LHR) | LHR == "Low")  %>% 
  # filter(year %in% c(2015, 2025, 2050, 2075, 2100)) %>% 
  filter(scenario != "ref") %>% 
  mutate(Model = paste0(GCM, "_", GGCM)) %>% 
  short_name() %>% 
  ggplot() +
  geom_hline(aes(yintercept = 1))+
  geom_line(aes(x = year, y = index, color = exp, linetype = GGCM)) +
  facet_grid(GCM~ region) +
  labs(x = "", y ="Agricultural wage relative to NoCC (NoCC = 1)", 
       fill = "Ag wage relative to NoCC (NoCC = 1)",
       color = "Experiment", linetype = "GGCM",
       title = "High labor-response function") +
  theme_bw() + theme0 + theme1 +
  theme(legend.position = "bottom") ->
  Wage_low_index

(Wage_high_index + ggtitle("(A) High labor-response function"))/
  (Wage_low_index + ggtitle("(B) Low labor-response function")) -> p


p %>% Write_png(.name = paste0("Rev_Wage_combined"), .DIR_MODULE = DIR_MODULE, h = 10, w = 14)
