# Currency conversion ----
conv_MIL_BIL = 1000.0
conv_75_90 = 2.212
conv_75_15 = 3.507477
CONV_90_15 <- conv_75_15 / conv_75_90

# SCENARIO <- Scenario; SCENARIO

SCENARIO <- "KLENFMF"

SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = ifelse(scenario == "KLENFMF", "fix", scenario),
           scenario = ifelse(scenario == "2p6_KLENFMF", "2p6", scenario),
           scenario = ifelse(scenario == "net0_KLENFMF", "net0", scenario)) %>%
    return()
}

# subset scenario
SUB_SCE <- function(.data){
  .data %>%
    filter(scenario %in% SCENARIO) %>%
    return()
}

# subset year and scenarios 
SUB_SY <- function(.data){
  .data %>%
    filter(year >= 2015) %>% 
    filter(scenario %in% SCENARIO) %>%
    return()
}

# subsect key regions
SUB_REG <- function(.data){
  .data %>%
    filter(region %in% c("USA", "China", "India", "Russia", 
                         "Africa_Eastern", "Africa_Western", "Indonesia", "Middle East")) %>% 
    return()
}



# read in queried input for SAM table
  
  na_data <- ListV2024 %>% purrr::pluck("SAM_NA") %>% 
    mutate(branch = scenario, scenario = ss) %>% 
    rename(account = Account) 
  
  service_data <- ListV2024 %>% purrr::pluck("SAM_service") %>% 
    mutate(branch = scenario, scenario = ss) 
  
  labor_demand_data <- ListV2024 %>% purrr::pluck("LaborSupplyAll") %>% 
    mutate(branch = scenario, scenario = ss) 

  labor_price_data <- ListV2024 %>% purrr::pluck("LaborPriceAll") %>% 
    mutate(branch = scenario, scenario = ss) 
  
  import_data <- ListV2024 %>% purrr::pluck("SAM_import") %>% 
    mutate(branch = scenario, scenario = ss) 
  
  
  export_data <- ListV2024 %>% purrr::pluck("SAM_export") %>% 
    mutate(branch = scenario, scenario = ss) 
  
  
  trade_price_data <- ListV2024 %>% purrr::pluck("SAM_trade_price") %>% 
    mutate(branch = scenario, scenario = ss) 
  
  s_invest_data <- ListV2024 %>% purrr::pluck("SAM_s_invest") %>% 
    mutate(branch = scenario, scenario = ss) 
  
  
  r_invest_data <- ListV2024 %>% purrr::pluck("SAM_r_invest") %>% 
    mutate(branch = scenario, scenario = ss) 
  
  
  # gather info ----
  
  ## service dara ----
  # factor sharefor MA production
  # nonfood = 1 - capital - labor - energy
  na_data %>% 
    select(-Units) %>% 
    filter(year >= 2015, grepl('fac-share', account) | account == "gross-output") %>% 
    spread(account, value) %>% 
    mutate(`fac-share-nonfood` = 1 - `fac-share-capital`- `fac-share-labor` - `fac-share-energy`) %>%
    gather(input, share, `fac-share-capital`, `fac-share-labor`, `fac-share-energy`, `fac-share-nonfood`) %>%
    mutate(input = gsub('fac-share-', '', input),
           value = `gross-output` * share) %>% 
    select(-`gross-output`) ->
    factor_inputs
  
  # collect service value {energy, ag (nonfood)} for MA production
  service_data %>%
    filter(year >= 2015) %>%
    filter(!grepl('ag food service', market)) %>% 
    mutate(region = gsub('(ag|energy|ag food) service', '', market)) %>% 
    mutate(input = ifelse(grepl('ag service', market), "nonfood", market),
           input = ifelse(grepl('energy service', market), "energy", input)) %>% 
    mutate(quantity = value * conv_MIL_BIL * conv_75_90) %>%
    select(-market, -value) ->
    factor_q 
  
  # vollect food service value
  service_data %>%
    filter(year >= 2015) %>%
    filter(grepl('ag food service', market)) %>% 
    mutate(region = gsub('ag food service', '', market)) %>% 
    mutate(input = "food") %>% 
    mutate(value = value * conv_MIL_BIL * conv_75_90,
           quantity = value) %>%
    select(-market) ->
    factor_food
  
  # bind MA capital
  na_data %>%
    filter(year >= 2015, account == "capital-stock") %>% 
    mutate(input = gsub('-.*', '', account)) %>% 
    select( -account) %>%
    rename(quantity = value) %>% 
    bind_rows(factor_q) ->
    factor_q
  
  # bind MA labor
  labor_demand_data %>%
    filter(year >= 2015, grepl('Materials', market)) %>%
    mutate(region = gsub('Labor.*', '', market),
           input='labor') %>%
    select(-market) %>%
    rename(quantity = value) %>% 
    bind_rows(factor_q) ->
    factor_q
  
  factor_inputs %>% 
    left_join(factor_q) %>% 
    select(-share) %>% 
    bind_rows(factor_food) %>% 
    mutate(price_eff = value / quantity) ->
    factor_inputs
  
  ## net eport ----
  import_data %>%
    filter(input %in% unique(export_data$sector), year >= 2015) %>%
    select(scenario, region, sector = input, year, import_quant = value) ->
    import_data_filtered
  
  export_data %>%
    filter(year >= 2015) %>%
    mutate(region = gsub(' traded.*', '', subsector)) %>%
    select(scenario, region, sector, year, export = value) %>%
    full_join(import_data_filtered) %>%
    left_join(trade_price_data %>% mutate(sector = gsub('USA', '', market)) %>% 
                filter(!grepl("Demand_int", sector)) %>% 
                select(scenario, sector, year, price = value)) %>% 
    # NAs from regions that do not import some goods -- about 18 region+sector distinct combos
    mutate(export = replace_na(export, 0),
           import_quant = replace_na(import_quant, 0),
           import = import_quant * price,
           # note: sign backed into values already
           net.trade = (export + import) * conv_MIL_BIL * conv_75_90,
           input = if_else(grepl('(oil|gas|LNG|coal|biomass)$', sector), 'energy', 'ag')) %>% 
    group_by(scenario, region, input, year) %>%
    # given as net import but we want export so flip sign
    summarize(net.trade = -sum(net.trade)) ->
    factor_trade
  
  ## investment ----
  s_invest_data %>%
    filter(sector == "Capital_Ag") %>%
    mutate(account = "ag-investment",
           value = value * conv_MIL_BIL * conv_75_90 / 5) %>%
    select(scenario, region, account, year, value) %>%
    bind_rows(na_data %>% filter(account %in% c("energy-investment", "consumer-durable", "savings", "capital-stock", "depreciation")) %>%
                select(scenario, region, account, year, value)) %>% 
    spread(account, value) %>% 
    mutate(`energy-investment` = `energy-investment` - `ag-investment`,
           `materials-investment` = savings - `energy-investment` - `consumer-durable` - `ag-investment`,
           # check = (`capital-stock` - (lag(`capital-stock`) - depreciation*5))/5) 
           check = (`capital-stock` - (lag(`capital-stock`) * (1-0.048508)^5))/5) %>% 
    select(-savings, -`capital-stock`, -depreciation, -check) %>%
    gather(account, value, `energy-investment`, `consumer-durable`, `ag-investment`, `materials-investment`) %>%
    filter(year >= 2015) ->
    investment_data
  
  ## misc ----
  na_data %>%
    filter(year >= 2015, account %in% c("value-added", "savings", "investment", "capital-net-export", "materials-net-export")) %>%
    select(scenario, region, account, year, value) ->
    misc_na
  
  labor_demand_data %>%
    select(scenario, market, year, q = value) %>%
    left_join(labor_price_data %>% select(scenario, market, year, p = value)) %>%
    filter(grepl("Ag", market), year >= 2015) %>%
    mutate(region = gsub('Labor_Ag', '', market),
           value = q * p) %>%
    select(-market) ->
    labor_ag_data
  
  na_data %>%
    filter(year >= 2015, account %in% c("GDP")) %>%
    select(scenario, region, year, GDP = value) ->
    GDP
  
  # VA: bottom-up method ----
  
  ## return to land ----
  
  LAND <- ListV2024 %>% purrr::pluck("Land") %>% 
    mutate(branch = scenario, scenario = ss) %>% 
    SUB_SY()
  
  PI <- ListV2024 %>% purrr::pluck("ProfitRate") %>% 
    mutate(branch = scenario, scenario = ss) %>% 
    SUB_SY()
  
  LAND[c('sector', 'WBT')] <- str_split_fixed(LAND$LandLeaf, '_', 2)
  
  PI[c('sector', 'WBT')] <- str_split_fixed(PI$LandLeaf, '_', 2)
  
  
  LAND %>% rename(land = value) %>% 
    # biomass land are included
    # fodder and pasture are included
    # protected land, unmanaged land, grassland is excluded
    filter(sector %in% c("CornC4","FiberCrop" , "FodderGrass", "FodderHerb", "FruitsTree", "Fruits",
                         "Hardwood", "Legumes", "MiscCropTree","MiscCrop" , "NutsSeedsTree",
                         "NutsSeeds","OilCropTree","OilCrop", "OilPalmTree","OtherGrainC4",
                         "OtherGrain", "Pasture", "Rice", "RootTuber", "Softwood", "Soybean","SugarCropC4",
                         "Vegetables","Wheat", "FodderHerbC4", "SugarCrop",
                         "biomassTree", "biomassGrass")) %>% 
    select(-ss, -branch, -Units) %>% 
    left_join(PI %>% rename(pi = value), by = c("scenario", "region", "LandLeaf", "sector", "WBT", "year")) %>% 
    mutate(VAL_LAND = land * pi) %>% 
    group_by(scenario, region, year) %>% 
    summarise(value = sum(VAL_LAND) / 10^9) ->  # billion 1975$
    VAL_LAND
  
  VAL_LAND %>% Agg_reg() %>% mutate(region = "World") %>% 
    bind_rows(VAL_LAND %>% SUB_REG()) %>% 
    SUB_SY() ->
    VALA 
  
  ## return to labor ----
  labor_demand_data  %>%
    mutate(sector = "Labor_Total",
           sector = ifelse(grepl("Labor_Ag", market), "Labor_Ag", sector),
           sector = ifelse(grepl("Labor_Materials", market), "Labor_Materials", sector),
           region = gsub("Labor_Ag", "", market),
           region = gsub("Labor_Materials", "", region),
           region = gsub("Labor_Total", "", region)) %>%
    select(-market) %>% 
    filter(sector == "Labor_Ag") ->
    Ag_L
  
  labor_price_data %>%
    mutate(sector = "Labor_Total",
           sector = ifelse(grepl("Labor_Ag", market), "Labor_Ag", sector),
           sector = ifelse(grepl("Labor_Materials", market), "Labor_Materials", sector),
           region = gsub("Labor_Ag", "", market),
           region = gsub("Labor_Materials", "", region),
           region = gsub("Labor_Total", "", region)) %>% 
    select(-market) %>% 
    filter(sector == "Labor_Ag") ->
    Ag_L_P
  
  
  Ag_L %>% select(-sector, -ss, -branch, -Units) %>% 
    rename(labor = value) %>% 
    left_join(Ag_L_P %>% select(-sector) %>% 
                rename(wage = value),
              by = c("scenario", "region", "year")) %>% 
    mutate(VAL_LABOR = labor * wage / 10^3 / conv_75_90) %>%  # million per * 1990$ -> billion 1975$
    select(scenario, region, year, value = VAL_LABOR) ->
    VAL_LABOR
  
  VAL_LABOR %>% Agg_reg() %>% mutate(region = "World") %>% 
    bind_rows(VAL_LABOR %>% SUB_REG()) %>% 
    SUB_SY() ->
    VALL 
    
  
  ## return to capital ----
  
  PK_scaler <- read.csv("data/input/PK_scaler.csv")
  
  AgK <- ListV2024 %>% purrr::pluck("AgCapitalDemand") %>% 
    mutate(branch = scenario, scenario = ss) 
  
  PAgK <- ListV2024 %>% purrr::pluck("PriceAgK") %>% 
    mutate(branch = scenario, scenario = ss) %>%  
    mutate(region = gsub("Capital_Ag", "", market)) 
  
  PAgK %>% select(-ss, -branch, -Units) %>% 
    left_join(PK_scaler %>% select(-X), by = c("region", "year")) %>% 
    mutate(Pk = value * scaler) %>% 
    select(scenario, region, year, Pk)->
    PK
  
  AgK %>% select(scenario, region, year, input, K = value) %>% 
    left_join(PK, by = c("scenario", "region", "year")) %>% 
    mutate(VAL_K = K * Pk) %>% 
    select(scenario, region, year, value = VAL_K) ->
    VAL_K
  
  VAL_K %>% Agg_reg() %>% mutate(region = "World") %>% 
    bind_rows(VAL_K %>% SUB_REG()) %>% 
    SUB_SY() ->
    VALK
  
  ## sum ----
  VALK %>% 
    mutate(factor = "capital") %>% 
    bind_rows(VALL %>% mutate(factor = "labor")) %>% 
    bind_rows(VALA %>% mutate(factor = "land")) %>% 
    mutate(value = value * conv_75_90 * 1000) %>% # billion 1975$ to million 1990 $
    spread(factor, value) %>% 
    mutate(VAL_AG = capital + labor + land) -> 
    VAL_AG_SUM
  
  # VA: top-down method ----
  
  factor_inputs %>% 
    filter(input %in% c("nonfood", "food")) %>% 
    select(scenario, region, year, input, value) %>% 
    spread(input, value) %>% 
    mutate(service = food + nonfood) %>% 
    left_join(factor_trade %>% 
                filter(input == "ag") %>% 
                ungroup() %>% 
                select(scenario, region, year, net.trade),
              by = c("scenario","region", "year")) %>% 
    rowwise() %>%
    mutate(VAL_AG = sum(service, net.trade, na.rm = TRUE)) ->
    VAL_AG_SAM
  
  
  # generate inputs for SAM table ----
  
  library(readr)
  
  REG <- "USA"
  SCE_NAME <- "KLENFMF"
  SAM_path <- "output/KLEAM/KLEAM/SAM/"
  # YEAR <- 2015
  # YEAR <- 2100
  YEAR <-  2050
  

  
  write_csv(factor_inputs %>% filter(region == REG, year == YEAR, scenario == SCE_NAME), paste0(SAM_path,SCE_NAME,'/',"factor_input_value",REG,YEAR,".csv"))
  write_csv(factor_trade%>% filter(region == REG, year == YEAR, scenario == SCE_NAME), paste0(SAM_path,SCE_NAME,'/',"factor_trade_value",REG,YEAR,".csv"))
  write_csv(investment_data%>% filter(region == REG, year == YEAR, scenario == SCE_NAME), paste0(SAM_path,SCE_NAME,'/',"investment_data",REG,YEAR,".csv"))
  write_csv(misc_na%>% filter(region == REG, year == YEAR, scenario == SCE_NAME), paste0(SAM_path,SCE_NAME,'/',"misc_na",REG,YEAR,".csv"))
  write_csv(labor_ag_data%>% filter(region == REG, year == YEAR, scenario == SCE_NAME), paste0(SAM_path,SCE_NAME,'/',"labor_ag_data",REG,YEAR,".csv"))
 
  write_csv(VAL_AG_SAM %>% filter(region == REG, year == YEAR, scenario == SCE_NAME), paste0(SAM_path,SCE_NAME,'/',"VAL_AG_SAM",REG,YEAR,".csv"))
  write_csv(VAL_AG_SUM %>% filter(region == REG, year == YEAR, scenario == SCE_NAME), paste0(SAM_path,SCE_NAME,'/',"VAL_AG_SUM",REG,YEAR,".csv"))
  
  # write_csv(DDGS_data, paste0(path,SCE_NAME,'_',"DDGS_data.csv"))
  # write_csv(Fertilizer_data, paste0(path,SCE_NAME,'_',"Fertilizer_data.csv"))
  # write_csv(biomass_data, paste0(path,SCE_NAME,'_',"biomass_data.csv"))
  
  
  # check crop NLC ----
  # NLC <- ListV2024 %>% purrr::pluck("NLC") %>% 
  #   mutate(branch = scenario, scenario = ss) 
  # 
  # CropProdTech <- ListV2024 %>% purrr::pluck("CropProdTech") %>% 
  #   mutate(branch = scenario, scenario = ss) 
  # 
  # NLC %>% select(scenario, region, sector, technology, year, NLC = value) %>% 
  #   left_join(CropProdTech %>% select(scenario, region, sector, technology, year, Q = value)) %>% 
  #   mutate(ENLC = NLC * Q * conv_75_90 * conv_MIL_BIL * conv_75_90)  %>% 
  #   group_by(scenario, region, year) %>% 
  #   summarise(ENLC = sum(ENLC, na.rm = T)) -> check
  
