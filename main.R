
# Load libs ----
library(tidyr)
library(stringr)
library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)
library(gcamdata)
library(purrr)
library(patchwork)
# library(broom)
library(sf)

source("R/LoadPackagesFuncs.R")
source("R/GCAM_module_funcs.R")

DIR_DATA <- "data"
DIR_OUTPUT <- "output"
DIR_MODULE = "HeatStress"

Project <- "HeatStress"
Version <- "V2024"
Scenario <- Load_GCAM(projnm = Project, versionnm = Version, return_availscen = T); Scenario

MODEL_FUTURE_YEARS  <- seq(2020, 2100, 5); MODEL_FUTURE_YEARS

# Check availability
Load_GCAM(projnm = Project, return_availversion = T)
Load_GCAM(projnm = Project, versionnm = Version, return_availscen = T)
Load_GCAM(projnm = Project, versionnm = Version, return_availquery = T)


# Modify/customize read csv function ----
read_csv_bind <- function(.multiCSVpath){

  library(doParallel)
  myCluster <-
    makeCluster(4, # number of cores to use
                type = "PSOCK") # type of cluster
  #detectCores()
  registerDoParallel(myCluster)

  foreach(csvDir = .multiCSVpath,
          .combine=rbind,
          .packages = "dplyr" ,.errorhandling = "remove"
  ) %dopar% {
    readr::read_csv(csvDir, skip = 1)%>%
      select(-matches("^X|\\...")) %>%
      na.omit() %>%
      filter(scenario != "scenario") %>%
      mutate(scenario = gsub(",date.*$", "", scenario)) %>%
      gcamdata::gather_years() %>%
      mutate(ss = sub(".*/([^/]+)/.*", "\\1", csvDir))
  } -> df

  stopCluster(myCluster)
  return(df)
}

rm(ListV2024)
# Load everything into lists ----
Load_GCAM(projnm = Project, versionnm = "V2024", outputlistnm = "ListV2024")


# create a project data output folder and save data
# dir.create(file.path(DIR_OUTPUT, Project, "ProjectRDS"), showWarnings = F) # somehow not working
ListV2024 %>% saveRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListV2024", ".RDS")))

# Load the list [when needed]
ListV2024 <- readRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListV2024", ".RDS")))


KEY_SEC <- c("Corn", "Wheat", "Rice", "Soybean")

# COMMSector <- c("Energy crops", "Staple crops", "Oil crops", #"Fodder crops",
#                 "Other crops", "Livestock", "Forest")
COMMSector <- c("Energy crops", "Corn", "Wheat", "Rice", "Soybean",#"Fodder crops",
                "Other crops", "Livestock", "Forest")
## theme1 ----
theme1 <- theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(linetype = 2, color = "grey80", size = 0.3),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"))

## theme2 ----
theme2 <- theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"))


source("R/AgBalElement_Storage.R")

gather_time <- function(.data){
  .data %>%
    gather(year, value, names(.)[grepl("[0-9]{4}", names(.))]) %>%
    mutate(year = as.integer(gsub("X", "", year))) %>%
    return()
}

REV_REG_10 <- function(.data){
  .data %>% 
    mutate(region = gsub("SOUTH_ASIA","South\nAsia",region),
           region = gsub("SE_ASIA","Southeast\nAsia",region),
           region = gsub("REF_ECON","Reforming\nEconomies",region),
           region = gsub("PAC_OECD","Pacific\nOECD",region),
           region = gsub("NORTH_AM","North\nAmerica",region),
           region = gsub("MIDDLE_EAST","Middle\nEast",region),
           region = gsub("LATIN_AM","Latin\nAmerica",region),
           region = gsub("EUROPE","Europe",region),
           region = gsub("CHINA+","China",region),
           region = gsub("AFRICA","Africa",region)) %>% 
    return()
}


# OUTPUT ----
# source("R/MR1.R")
source("Fig2_final.R")
source("Fig3_final.R")
source("Fig4_final.R")

# obtain hex of color
brewer.pal(n=5,"Set2")
