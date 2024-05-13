# sensitivity KLEAM outcomes ----

# query names ----
ListV2024 <- readRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListV2024", ".RDS")))

ListV2024 %>% names()

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

SCENARIO <- list.files(path = input.dir, pattern = ".*KLENFMF$", full.names = FALSE); SCENARIO



SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = ifelse(scenario == "KLENFMF", "fix", scenario),
           scenario = ifelse(scenario == "2p6_KLENFMF", "2p6", scenario),
           scenario = ifelse(scenario == "net0_KLENFMF", "net0", scenario),
           scenario = ifelse(scenario == "open_KLENFMF", "open", scenario)) %>%
    return()
}

SCE_NAME <- c("open", "net0")

short_name <- function(df){
  df %>%
    mutate(region = gsub("Central America and Caribbean", "Caribbean", region),
           region = gsub("European Free Trade Association", "EU Free Trade", region),
           region = gsub("South America_Southern", "S America_S" , region),
           region = gsub("South America_Northern", "S America_N" , region),
           region = gsub("Africa_Eastern", "Africa_E", region),
           region = gsub("Africa_Northern", "Africa_N", region),
           region = gsub("Africa_Southern", "Africa_S", region),
           region = gsub("Africa_Western", "Africa_W", region),
           region = gsub("Europe_Eastern", "Europe_E", region),
           region = gsub("Southeast Asia", "SE Asia", region)) ->
    df
  return(df)
}

# Getting data ready ----

### VARIABLE: WAGE ----
ListV2024 %>% purrr::pluck("LaborPriceAll") %>%
  mutate(region = gsub("Labor_.*", "", market),
         sector = gsub(".*Labor_", "", market)) %>% 
  mutate(branch = scenario, scenario = ss) %>% 
  SCE_NM() %>% 
  short_name() ->
  AgWage_32

# reference sectoral wage ----
AgWage_32 %>% 
  select(scenario, region, sector, year, value, Units) %>% 
  filter(scenario %in% c("Ref")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = sector)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Annual wage (1990$)", color = "Sector") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = "LaborWage_ref", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)

# ag wage rate across scenarios ----
AgWage_32 %>% 
  select(scenario, region, sector, year, value, Units) %>% 
  filter(scenario != "eta0") %>% 
  filter(sector == "Ag") %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Annual wage (1990$)", color = "Sector") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = "AgWage_noeta0", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)

# sectoral wage sensitivity: eta ----

SEC <- unique(AgWage_32$sector)

for (i in 1:length(SEC)){
  SECTOR <- SEC[[i]]
AgWage_32 %>% 
  select(scenario, region, sector, year, value, Units) %>% 
  filter(scenario %in% c("Ref", "eta_high", "eta_low", "eta0")) %>% 
  filter(sector == SECTOR) %>% 
  group_by(scenario, region) %>% 
  mutate(index = value / value[year == 2015]) %>% 
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Wage index (2015 = 1)", color = "Scenario") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = paste0("LaborWage_eta_", SECTOR), .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
}



# sectoral wage sensitivity: logit ----

SEC <- unique(AgWage_32$sector)

for (i in 1:length(SEC)){
  SECTOR <- SEC[[i]]
  AgWage_32 %>% 
    select(scenario, region, sector, year, value, Units) %>% 
    filter(scenario %in% c("Ref", "logit1", "logit3" )) %>% 
    filter(sector == SECTOR) %>% 
    group_by(scenario, region) %>% 
    mutate(index = value / value[year == 2015]) %>% 
    ggplot() +
    geom_line(aes(x = year, y = index, color = scenario)) +
    facet_wrap(~ region, ncol = 8, scales = "free_y") +
    geom_vline(xintercept = 2015) +
    labs(x = "Year", y = "Wage index (2015 = 1)", color = "Scenario") +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg +
    theme(legend.position = "bottom")-> pp; pp
  
  pp %>% Write_png(.name = paste0("LaborWage_logit_", SECTOR), .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
}

### VARIABLE: LABOR ----
ListV2024 %>% purrr::pluck("LaborSupplyAll") %>% 
  mutate(region = gsub("Labor_.*", "", market),
         sector = gsub(".*Labor_", "", market)) %>% 
  mutate(branch = scenario, scenario = ss) %>% 
  SCE_NM() %>% 
  short_name() ->
  AgLabor_32

# reference sectoral labor ----
AgLabor_32 %>% 
  select(scenario, region, sector, year, value, Units) %>% 
  filter(scenario %in% c("Ref")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = sector)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Labor by sector (million person)", color = "Sector") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = "LaborLabor_ref", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)


AgLabor_32 %>% 
  select(scenario, region, sector, year, value, Units) %>% 
  filter(scenario %in% c("Ref")) %>% 
  filter(year >= 2015) %>% 
  group_by(scenario, region, year) %>% 
  mutate(share = value / value[sector == "Total"]) %>% 
  filter(sector == "Ag") %>% 
  ggplot() +
  geom_bar(aes(x = year, y = share, fill = sector),
           position="stack", stat="identity") +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = "StackedLabor_ref", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)

# sectoral labor sensitivity: eta ----

SEC <- unique(AgLabor_32$sector)

for (i in 1:length(SEC)){
  SECTOR <- SEC[[i]]
  AgLabor_32 %>% 
    select(scenario, region, sector, year, value, Units) %>% 
    filter(scenario %in% c("Ref", "eta_high", "eta_low", "eta0")) %>% 
    filter(sector == SECTOR) %>% 
    group_by(scenario, region) %>% 
    mutate(index = value / value[year == 2015]) %>% 
    ggplot() +
    geom_line(aes(x = year, y = index, color = scenario)) +
    facet_wrap(~ region, ncol = 8, scales = "free_y") +
    geom_vline(xintercept = 2015) +
    labs(x = "Year", y = "Labor index (2015 = 1)", color = "Scenario") +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg +
    theme(legend.position = "bottom")-> pp; pp
  
  pp %>% Write_png(.name = paste0("LaborLabor_eta_", SECTOR), .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
}

AgLabor_32 %>% 
  select(scenario, region, sector, year, value, Units) %>% 
  filter(scenario %in% c("Ref", "eta_high", "eta_low", "eta0")) %>% 
  filter(year >= 2015) %>% 
  group_by(scenario, region, year) %>% 
  mutate(share = value / value[sector == "Total"]) %>% 
  filter(sector == "Ag") %>% 
  ggplot() +
  geom_line(aes(x = year, y = share, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom") -> pp; pp

pp %>% Write_png(.name = "AgLaborSahre_eta", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)

AgLabor_32 %>%
  select(scenario, region, sector, year, value, Units) %>%
  filter(scenario %in% c("Ref", "eta_high", "eta_low")) %>%
  filter(year >= 2015) %>%
  group_by(scenario, region, year) %>%
  mutate(share = value / value[sector == "Total"]) %>%
  filter(sector == "Ag") %>%
  ggplot() +
  geom_line(aes(x = year, y = share, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom") -> pp; pp

pp %>% Write_png(.name = "AgLaborSahre_eta_2", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)


# sectoral labor sensitivity: logit ----

SEC <- unique(AgLabor_32$sector)

for (i in 1:length(SEC)){
  SECTOR <- SEC[[i]]
  AgLabor_32 %>% 
    select(scenario, region, sector, year, value, Units) %>% 
    filter(scenario %in% c("Ref", "logit1", "logit3" )) %>% 
    filter(sector == SECTOR) %>% 
    group_by(scenario, region) %>% 
    mutate(index = value / value[year == 2015]) %>% 
    ggplot() +
    geom_line(aes(x = year, y = index, color = scenario)) +
    facet_wrap(~ region, ncol = 8, scales = "free_y") +
    geom_vline(xintercept = 2015) +
    labs(x = "Year", y = "Labor index (2015 = 1)", color = "Scenario") +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg +
    theme(legend.position = "bottom")-> pp; pp
  
  pp %>% Write_png(.name = paste0("LaborLabor_logit_", SECTOR), .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
}

AgLabor_32 %>% 
  select(scenario, region, sector, year, value, Units) %>% 
  filter(scenario %in% c("Ref", "logit1", "logit3")) %>% 
  filter(year >= 2015) %>% 
  group_by(scenario, region, year) %>% 
  mutate(share = value / value[sector == "Total"]) %>% 
  filter(sector == "Ag") %>% 
  ggplot() +
  geom_line(aes(x = year, y = share, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = "AgLaborShare_logit", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)

### VARIABLE: AG INV ----

ListV2024 %>% purrr::pluck("Capital_Ag_INV") %>% 
  mutate(branch = scenario, scenario = ss) %>% 
  select(scenario, region, year, value, Units) %>% 
  mutate(Units = "bil 1975$, per time step") %>% 
  SCE_NM() %>% 
  short_name() ->
  AgINV_32

AgINV_32 %>% 
  filter(scenario %in% c("Ref","dep_high", "dep_low")) %>% 
  filter(year >= 2015) %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = "AgINV_dep", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)


AgINV_32 %>% 
  filter(year >= 2015) %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = "AgINV_dep_all", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
  
### VARIABLE: AG K stock ----

ListV2024 %>% purrr::pluck("AgCapitalDemand") %>% 
  mutate(branch = scenario, scenario = ss) %>% 
  select(scenario, region, year, value, Units) %>% 
  SCE_NM() %>% 
  short_name() ->
  AgK_32

AgK_32 %>% 
  filter(scenario %in% c("Ref","dep_high", "dep_low")) %>% 
  filter(year >= 2015) %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = "AgK_dep", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)

AgK_32 %>% 
  filter(year >= 2015) %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

pp %>% Write_png(.name = "AgK_dep_all", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)


### VARIABLE: Price AgK ----

ListV2024 %>% purrr::pluck("PriceAgK") %>% 
  mutate(region = gsub("Capital_Ag", "", market),
         branch = scenario, scenario = ss) %>% 
  select(scenario, region, year, value, Units) %>% 
  SCE_NM() %>% 
  short_name() ->
  PAgk_32

PAgk_32 %>% 
  filter(year >= 2015) %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  # scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg +
  theme(legend.position = "bottom")-> pp; pp

  pp %>% Write_png(.name = "P_AgK_dep", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
  
  
  # ag capital rental rate across scenarios ----
  PAgk_32 %>% 
    select(scenario, region, year, value, Units) %>% 
    filter(scenario != "eta0") %>% 
    ggplot() +
    geom_line(aes(x = year, y = value, color = scenario)) +
    facet_wrap(~ region, ncol = 8, scales = "free_y") +
    geom_vline(xintercept = 2015) +
    labs(x = "Year", y = "Ag capital rental rate", color = "Sector") +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg +
    theme(legend.position = "bottom")-> pp; pp
  
  pp %>% Write_png(.name = "AgKPrice_noeta0", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
  
  ### VARIABLE: Price K ----
  
  ListV2024 %>% purrr::pluck("PriceK") %>% 
    mutate(region = gsub("capital", "", market),
           branch = scenario, scenario = ss) %>% 
    select(scenario, region, year, value, Units) %>% 
    SCE_NM() %>% 
    filter(year >= 2015) %>% 
    short_name() ->
    r_32
  
  r_32 %>% 
    ggplot() +
    geom_line(aes(x = year, y = value, color = scenario)) +
    facet_wrap(~ region, ncol = 8, scales = "free_y") +
    # scale_color_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg +
    theme(legend.position = "bottom")-> pp; pp
  
  pp %>% Write_png(.name = "P_K_dep", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
  
  
  PAgk_32 %>% mutate(sector = "Ag") %>% 
    bind_rows(r_32 %>% mutate(sector = "economy")) %>% 
    ggplot() +
    geom_line(aes(x = year, y = value, color = scenario, linetype = sector)) +
    facet_wrap(~ region, ncol = 8, scales = "free_y") +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg +
    theme(legend.position = "bottom")-> pp; pp
  
  pp %>% Write_png(.name = "PK_all_dep", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
  
  ### VARIABLE: National Account ----
  
  ListV2024 %>% purrr::pluck("NationalAccount") %>% 
    mutate(branch = scenario, scenario = ss) %>% 
    select(scenario, region, year, Account, value, Units) %>% 
    SCE_NM() %>% 
    short_name() ->
    NA_32

  NA_32 %>% 
    filter(grepl("fac-share-", Account)) %>% 
    mutate(Account = gsub("fac-share-", "", Account)) %>% 
    spread(Account, value) %>% 
    mutate(ag = 1 - capital - energy - labor) %>% 
    gather(factor, share, capital:ag) -> 
    MA_fac_32
  
  MA_fac_32$factor <- factor(MA_fac_32$factor, 
                             levels = c("energy", "ag", "labor", "capital"))

  MA_fac_32 %>%
    filter(year == 2100) %>%
    ggplot() +
    geom_bar(aes(x = scenario, y = share, fill = factor),
             position = "fill", stat = "identity") +
    facet_wrap(~ region, ncol = 4) +
    theme_bw() + theme0 + theme1 + theme_leg +
    theme(legend.position = "bottom")-> pp; pp
  
  pp %>% Write_png(.name = "MA-fac-all", .DIR_MODULE = DIR_MODULE, h = 16, w = 10)
  
  ### VARIABLE: crop price ----
  
  ListV2024 %>% purrr::pluck("CropPrice") %>%  
    mutate(branch = scenario, scenario = ss) %>% 
    select(scenario, region, sector, year, value, Units) %>% 
    SCE_NM() %>% 
    short_name() ->
    CropPrice_32
  
  CropPrice_32 %>% 
    filter(sector %in% c("Corn", "Soybean", "Wheat", "Rice")) %>% 
    filter(scenario %in% SCE_NAME) %>% 
    group_by(scenario, region, sector) %>% 
    mutate(index = value/value[year == 2015]) %>% 
    ggplot() +
    geom_line(aes(x = year, y = index, color = scenario, linetype = sector)) +
    facet_wrap(~ region, ncol = 8, scales = "free_y") +
    labs(x = "", y = "price index (2015 = 1)") +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg -> pp; pp
  
  pp %>% Write_png(.name = "Price_compare_net0", .DIR_MODULE = DIR_MODULE, h = 10, w = 18)
  
  
  CropPrice_32 %>% 
    # filter(sector %in% c("Corn", "Soybean", "Wheat", "Rice")) %>% 
    filter(sector %in% c("Corn")) %>% 
    filter(scenario != "eta0") %>% 
    group_by(scenario, region, sector) %>% 
    mutate(index = value/value[year == 2015]) %>% 
    ggplot() +
    geom_line(aes(x = year, y = index, color = scenario, linetype = sector)) +
    facet_wrap(~ region, ncol = 8, scales = "free_y") +
    labs(x = "", y = "price index (2015 = 1)") +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg +
    theme(legend.position = "bottom")-> pp; pp
  
  pp %>% Write_png(.name = "CropPrice_noeta0", .DIR_MODULE = DIR_MODULE, h = 10, w = 16)
  
  
  ### VARIABLE: production ----
  
  ListV2024 %>% purrr::pluck("CropProdSec") %>%  
    mutate(branch = scenario, scenario = ss) %>% 
    select(scenario, region, sector, year, value, Units) %>% 
    SCE_NM() %>% 
    short_name() ->
    CropQ_32
  
  CropQ_32 %>% 
    filter(sector %in% c("Corn", "Soybean", "Wheat", "Rice")) %>% 
    filter(scenario %in% SCE_NAME) %>% 
    group_by(scenario, region, sector) %>% 
    mutate(index = value/value[year == 2015]) %>% 
    ggplot() +
    geom_line(aes(x = year, y = index, color = scenario, linetype = sector)) +
    facet_wrap(~ region, ncol = 8, scales = "free_y") +
    labs(x = "", y = "production index (2015 = 1)") +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg -> pp; pp
  
  pp %>% Write_png(.name = "Production_compare_net0", .DIR_MODULE = DIR_MODULE, h = 10, w = 18)
  
  
  # Ag labor and capital price index ----
  
  PAgk_32 %>% 
    select(scenario, region, year, value, Units) %>% 
    filter(scenario != "eta0") %>% 
    mutate(input = "Capital") %>% 
    bind_rows(AgWage_32 %>% 
                select(scenario, region, sector, year, value, Units) %>% 
                filter(scenario != "eta0") %>% 
                filter(sector == "Ag") %>% 
                mutate(input = "Labor") %>% 
                select(-sector)) %>% 
    bind_rows(CropPrice_32 %>% 
                filter(sector %in% c("Corn")) %>%
                mutate(input = "Corn price") %>%
                # filter(sector %in% c("Rice")) %>% 
                # mutate(input = "Rice price") %>% 
                filter(scenario != "eta0") %>% 
                select(-sector)) %>% 
    group_by(scenario, region, input) %>% 
    mutate(index =  value / value[year == 2015]) -> 
    df.index 
  
  # df.index %>% 
  #   ggplot() +
  #   geom_line(aes(x = year, y = index, color = input)) +
  #   facet_grid(region~ scenario, scales = "free_y")
  
  
  df.index %>% 
    filter(region %in% c("USA", "India", "China")) %>% 
    filter(year >= 2015) %>% 
    ggplot() +
    geom_line(aes(x = year, y = index, color = input)) +
    facet_grid(region~ scenario, scales = "free_y") +
    labs(y = "price index (2015 = 1)") +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg +
    theme(legend.position = "bottom")
  
  #TODO: ----
  # plot relative wage rate change across AG and MA 
  # run with the new calibrated TFP
  # check crop production 
  # get INV share of AG and MA
  
  
  # return to ag labor
  # return to ag capital
  # return to land 
  
  