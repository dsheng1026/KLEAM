
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

source("R/LoadPackagesFuncs.R")
source("R/GCAM_module_funcs.R")

DIR_DATA <- "data"
DIR_OUTPUT <- "output"
DIR_MODULE = "KLEAM"

Project <- "KLEAM"
Version <- "V2024"
Scenario <- Load_GCAM(projnm = Project, versionnm = Version, return_availscen = T); Scenario

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


# Module functions ----
DIR_MODULE = "KLEAM"
PluckBind <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss)
}






# SPA plot func ----
Proc0 <- function(.data){
  .data %>%
    mutate(policy = if_else(grepl("2p6", scenario), "2p6", "ref.")) %>%
    mutate(policy = factor(policy, level = c("ref.", "2p6"))) %>%
    mutate(SSP = gsub("GCAM_|_2p6","", scenario)) %>%
    mutate(SSP = if_else(!SSP %in% paste0("SSP", seq(5)), "core", SSP))}



gg_proc0 <- function(.data, unit = bquote(~W/m^2)){
  ggplot(.data) +
    geom_line(aes(x=year, y=value, color=branch)) +
    theme_bw() + theme0 + theme_leg +
    theme(
      legend.title = element_text(angle = 0, color = "black", size = 15, hjust= 0)) +
    scale_color_brewer(name = "SSP", palette = "Dark2") +
    scale_linetype_manual(values = c (2, 1), name = "Branch") +
    scale_size_manual(values = c(1.2, 2), name = "Policy") +
    scale_alpha_manual(values = c(0.95, 1), name = "Policy") +
    labs(y = unit , x = "Year")
}



