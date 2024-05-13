# sensitivity KLEAM outcomes ----

# query names ----
# ListV2024 <- readRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListV2024", ".RDS")))

ListV2024 %>% names()

input.dir <- "data/KLEAM/V2024"

# SCENARIO <- list.files(path = input.dir, pattern = ".*KLENFMF$", full.names = FALSE); SCENARIO

SCENARIO <- c("valid_logit_hi", "valid_logit_lo", "open_KLENFMF") 


SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = ifelse(scenario == "KLENFMF", "fix", scenario),
           scenario = ifelse(scenario == "2p6_KLENFMF", "2p6", scenario),
           scenario = ifelse(scenario == "net0_KLENFMF", "net0", scenario),
           scenario = ifelse(scenario == "valid_logit_hi", "logit_hi", scenario),
           scenario = ifelse(scenario == "valid_logit_lo", "logit_lo", scenario)) %>%
    return()
}

SUB_SCE <- function(.data){
  .data %>%
    filter(scenario %in% SCENARIO) %>%
    return()
}



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


SUB_SY <- function(.data){
  .data %>%
    filter(year >= 2015) %>% 
    filter(scenario %in% SCENARIO) %>%
    return()
}

SUB_REG <- function(.data){
  .data %>%
    filter(region %in% c("USA", "China", "India", "Russia", 
                         "Africa_Eastern", "Africa_Western", "Indonesia", "Middle East")) %>% 
    return()
}


### VARIABLE: National Account ----

VAR <- "GDP"

ListV2024 %>% purrr::pluck("SAM_NA") %>% 
  mutate(branch = scenario, scenario = ss) %>% 
  select(scenario, region, year, Account, value, Units) ->
  NA_32

NA_32 %>% 
  filter(Account == VAR) %>% 
  Agg_reg(Account, Units) %>% 
  mutate(region = "World") %>% 
  bind_rows(NA_32 %>% 
              filter(Account == "GDP") %>% SUB_REG()) ->
  df.GDP

df.GDP %>% 
  SUB_SY() %>% 
  SCE_NM() %>% 
  short_name() %>% 
  group_by(scenario, region) %>% 
  mutate(index = value / value[year == 2015]) %>% 
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario)) +
  geom_hline(yintercept = 1, linewidth = 1) +
  facet_wrap(~ region, ncol = 9, scales = "free_y") +
  labs(x = "", y = "Relative to 2015 (2015 = 1)",
       title = VAR) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg ->
  D1; D1


### VARIABLE: LABOR ----

VAR <- "Sectoral abor share"

ListV2024 %>% purrr::pluck("LaborSupplyAll") %>% 
  mutate(region = gsub("Labor_.*", "", market),
         sector = gsub(".*Labor_", "", market)) %>% 
  mutate(branch = scenario, scenario = ss) %>% 
  select(scenario, region, sector, year, value) ->
  AgLabor_32

AgLabor_32 %>% 
  Agg_reg(sector) %>% 
  mutate(region = "World") %>% 
  bind_rows(AgLabor_32 %>% SUB_REG()) %>%
  filter(sector != "Total") %>% 
  SUB_SCE() %>%
  SCE_NM() %>%
  short_name() %>% 
  filter(year %in% c(2015, 2050, 2100)) %>% 
  ggplot() +
  geom_bar(aes(x = interaction(scenario, year), y = value, fill = sector, alpha = scenario),
           stat = "identity", position = "fill") +
  # geom_hline(yintercept = 1, linewidth = 1) +
  facet_wrap(~ region, ncol = 9, scales = "free_y") +
  labs(x = "", y = "Share",
       title = VAR) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_alpha_discrete(range = c(0.35, 0.75)) +
  theme_bw() + theme0 + theme1 + theme_leg ->
  D2; D2

# AgLabor_32 %>% 
#   Agg_reg(sector) %>% 
#   mutate(region = "World") %>% 
#   bind_rows(AgLabor_32 %>% SUB_REG()) %>%
#   SUB_SCE() %>%
#   SCE_NM() %>%
#   short_name() %>% 
#   group_by(scenario, region, sector) %>% 
#   mutate(index = value / first(value)) %>% 
#   ggplot() +
#   geom_line(aes(x = year, y = index, color = scenario, linetype = sector)) +
#   geom_hline(yintercept = 1, linewidth = 1) +
#   facet_wrap(~ region, ncol = 9, scales = "free_y") +
#   labs(x = "", y = "Relative to 2015 (2015 = 1)",
#        title = VAR) +
#   scale_color_brewer(palette = "Set1", direction = -1) +
#   scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
#   theme_bw() + theme0 + theme1 + theme_leg ->
#   D2; D2


### VARIABLE: WAGE ----

VAR <- "Wage"

ListV2024 %>% purrr::pluck("LaborPriceAll") %>%
  mutate(region = gsub("Labor_.*", "", market),
         sector = gsub(".*Labor_", "", market)) %>% 
  mutate(branch = scenario, scenario = ss) %>% 
  select(scenario, region, sector, year, value) ->
  AgWage_32

AgWage_32 %>% rename(wage = value) %>% 
  left_join(AgLabor_32 %>% rename(labor = value)) %>% 
  group_by(scenario, sector, year) %>% 
  summarise(wage = weighted.mean(wage, w = labor), labor = sum(labor), .groups = "drop") %>% 
  mutate(region = "World") %>% 
  bind_rows(AgWage_32 %>% rename(wage = value) %>% 
              left_join(AgLabor_32 %>% rename(labor = value)) %>% 
              SUB_REG()) %>% 
  group_by(scenario, region, sector) %>% 
  mutate(index = wage / wage[year == 2015]) %>% 
  SUB_SCE() %>%
  SCE_NM() %>%
  short_name() %>% 
  ggplot() +
    geom_line(aes(x = year, y = index, color = scenario, linetype = sector)) +
    geom_hline(yintercept = 1, linewidth = 1) +
    facet_wrap(~ region, ncol = 9, scales = "free_y") +
    labs(x = "", y = "Relative to 2015 (2015 = 1)",
         title = VAR) +
    scale_color_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
    theme_bw() + theme0 + theme1 + theme_leg ->
    D3; D3



### VARIABLE: AG CAPITAL ----

VAR <- "Ag Capital Stock"

ListV2024 %>% purrr::pluck("AgCapitalDemand") %>% 
  mutate(branch = scenario, scenario = ss) %>% 
  select(scenario, region, year, value) ->
  AgK_32

AgK_32 %>% 
  Agg_reg() %>% 
  mutate(region = "World") %>% 
  bind_rows(AgK_32 %>% SUB_REG()) %>%
  SUB_SCE() %>%
  SCE_NM() %>%
  short_name() %>% 
  group_by(scenario, region) %>% 
  mutate(index = value / value[year == 2015]) %>% 
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario)) +
  # geom_hline(yintercept = 1, linewidth = 1) +
  facet_wrap(~ region, ncol = 9, scales = "free_y") +
  labs(x = "", y = "Relative to 2015 (2015 = 1)",
       title = VAR) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg ->
  D4; D4


### VARIABLE: AG L/K ----
VAR <- "Ag LK ratio"

AgLabor_32 %>% 
  filter(sector == "Ag") %>% 
  select(scenario, region, year, labor = value) %>% 
  left_join(AgK_32 %>% rename(capital = value)) ->
  df.LK

df.LK %>% 
  group_by(scenario, year) %>% 
  summarise(labor = sum(labor),
            capital = sum(capital)) %>%
  mutate(region = "World") %>% 
  bind_rows(df.LK %>% SUB_REG()) %>% 
  SUB_SCE() %>%
  SCE_NM() %>%
  short_name() %>% 
  mutate(ratio = labor / capital) %>%  # mpl / bil $
  group_by(scenario, region) %>% 
  mutate(index = ratio / ratio[year == 2015]) %>% 
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario)) +
  # geom_hline(yintercept = 1, linewidth = 1) +
  facet_wrap(~ region, ncol = 9, scales = "free_y") +
  labs(x = "", y = "Relative to 2015 (2015 = 1)",
       title = VAR) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg ->
  D5; D5
  

### VARIABLE: VALUE-ADDED ----

#### WB data  ----
WB <- read.csv("C:/Model/KLEAM/WB_indicators.csv")
colnames(WB) <- sub("\\.\\..*", "", colnames(WB))

WB %>% 
  gather_time() %>% 
  filter(grepl("GDP", Series.Name)) %>% 
  select(indicator = Series.Name, country = Country.Name, iso = Country.Code, year, value) %>% 
  mutate(iso = tolower(iso),
         value = ifelse(value == "..", NA, value),
         value = as.numeric(value),
         sector = ifelse(grepl("Agriculture", indicator), "AG", indicator), # ISIC 1-5
         sector = ifelse(grepl("Industry", indicator), "IND", sector), # ISIC 10-45
         sector = ifelse(grepl("Manufacturing", indicator), "MAN", sector), # ISIC 15-37
         sector = ifelse(grepl("Services", indicator), "SER", sector), # ISIC 50-99
         sector = ifelse(grepl("GDP", sector), "GDP", sector)) %>% 
  na.omit() %>% 
  left_join(iso_GCAM_regID, by = "iso") %>% 
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>% 
  select(-indicator) %>% 
  select(region, iso, country, year, sector, value) %>% 
  spread(sector, value) -> WB_SHARE # million 2015$

WB_SHARE %>% 
  na.omit() %>% 
  mutate(GDP = GDP / 10^6 / CONV_90_15, # million 2015$ -> million $1990$
         VAL_AG = GDP * AG / 100,
         VAL_IND = GDP * IND / 100,
         VAL_SER = GDP * SER / 100,
  ) %>%  
  na.omit() %>% 
  group_by(region, year) %>% 
  summarise(VAL_AG = sum(VAL_AG),
            VAL_IND = sum(VAL_IND),
            VAL_SER = sum(VAL_SER),
            GDP = sum(GDP)) %>% 
  na.omit() ->
  WB_32

WB_32 %>% 
  gather(var, value, VAL_AG:GDP) %>% 
  group_by(year, var) %>% 
  summarise(value = sum(value)) %>% 
  mutate(region = "World") %>% 
  spread(var, value) %>% 
  bind_rows(WB_32) %>% 
  mutate(AG = VAL_AG / GDP,
         IND = VAL_IND / GDP,
         SER = VAL_SER / GDP,
         Total = AG + IND + SER) ->
  WB_SHARE_32

#### source VAL-AG ----

source("Value_added.R")

VAR <- "Value added share of GDP: Ag"

VAL_AG_SUM %>% 
  left_join(df.GDP %>% rename(GDP = value) %>% SCE_NM()) %>% 
  mutate(Ag = 100 * VAL_AG / GDP) %>% 
  select(scenario, region, year, Ag) %>% 
  ggplot() +
  geom_line(aes(x = year, y = Ag, color = scenario)) +
  facet_wrap(~ region, ncol = 9) +
  labs(x = "", y = "Share (%)",
       title = VAR) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg ->
  D6; D6

((D1)/(D2)/(D3)/(D4)/(D5)/(D6)) -> pp

# ((D1 + ggtitle("(A) Land use change relative to 2015")+ labs(fill = "Land (Panel A)", linetype = "Scenario (Panels A-C)", alpha = "Scenario (Panels A-C)")+
#     theme(axis.title.x = element_blank(), legend.position = "right") ) /
#     (A6 + ggtitle("(B) Agricultural water withdrawal") +
#        theme(axis.title.x = element_blank(),legend.position = "right") + labs(fill = "Sector (Panel B)"))/
#     (A7 + ggtitle("(C) Cumulative agricultural and land use change emissions since 2020") +
#        theme(axis.title.x = element_blank(),legend.position = "right")+
#        labs(fill = "Source (Panel C)", size = "Sector (Panel C)")) +
#     patchwork::plot_layout(guides = "collect") ) -> AA

# AA/(A8 + ggtitle("(D) Cumulative land use change emissions (2020 - 2100) by GCAM region") +
#       theme(axis.title.x = element_blank(),legend.position = "right")) +
#   patchwork::plot_layout(guides = "keep", widths = 1) ->
#   pp

pp %>% Write_png(.name = "DashBoard", .DIR_MODULE = DIR_MODULE, h = 20, w = 20)
