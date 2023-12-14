
outdir <- "output/GCAM/"
OutFolderName <- "SI"
dir.create(file.path(outdir, OutFolderName), showWarnings = F)

# Note that "CLUC_LUT_raw" is not available in github since it is too large
# please find the source data in the zenodo repo
LoadFigData("CLUC_LUT_raw")  -> CLUC_LUT
LoadFigData("Detailedland_raw")  -> Detailedland

## Read in land mapping ----
readr::read_csv("data/maps/LandMapping.csv") -> LandMapping


# CLUC_LUT: Clean up land use change emissions by tech ----
CLUC_LUT %>% separate(col = LandLeaf, into = c("LandLeaf","basin", "IRR", "Fert")) ->
  CLUC_LUT1

Detailedland %>% separate(col = LandLeaf, into = c("LandLeaf","basin", "IRR", "Fert")) ->
  Detailedland1

## Assert land mapping is good ----
assertthat::assert_that(setdiff(CLUC_LUT1 %>% distinct(LandLeaf), LandMapping %>% distinct(LandLeaf)) %>% nrow ==0)
assertthat::assert_that(setdiff(Detailedland1 %>% distinct(LandLeaf), LandMapping %>% distinct(LandLeaf)) %>% nrow ==0)


## Join land mapping and aggregate ----
CLUC_LUT1 %>% left_join_error_no_match(LandMapping, by = "LandLeaf") %>%
  group_by(scenario, region, basin, land = LandCover_LUC, year) %>%
  summarise(value = sum(value), .groups = "drop") -> CLUC_LUT2

## Join land mapping and aggregate ----
Detailedland1 %>% left_join_error_no_match(LandMapping, by = "LandLeaf") %>%
  group_by(scenario, region, basin, land = LandCover_LUC, year) %>%
  summarise(value = sum(value), .groups = "drop") -> Detailedland2

#Clean up
rm(CLUC_LUT, CLUC_LUT1, Detailedland, Detailedland1)

# annualize and cum
CLUC_LUT2 %>%
  group_by(scenario, region, land, basin) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  # GtCO2
  mutate(value = value/1000 * 44/12) ->
  CLUC_cum


Detailedland2 %>%
  group_by(scenario, region, land, basin) %>%
  Fill_annual(CUMULATIVE = F) %>% ungroup() %>%
  filter(year >= 2019) %>%
  group_by_at(vars(-value, -year)) %>% arrange(year) %>%
  mutate(value = value - first(value)) %>%
  # cumulate LUC
  mutate(value = cumsum(value)) %>%
  ungroup() %>%
  # Mha
  mutate(value = value/10) %>%
  filter(year %in% seq(2020, 2100, 5)) ->
  LUC_cum
LUC_cum %>% SaveFigData("LUC_cum")

LUC_cum %>% rename(LUC = value) %>%
  left_join_error_no_match(CLUC_cum %>% rename(CLUC = value)) ->
  LUC_CLUC_cum


LUC_CLUC_cum %>%
  # Keep only forest, GrassShrub, and others (CropPasture)
  mutate(land = if_else(grepl("Forest", land), "Forest", land)) %>%
  mutate(land = if_else(grepl("Forest|Grass", land), land, "CropPasture")) %>%
  #mutate(land = if_else(LUC >0, paste0(land, "_Inc"), paste0(land, "_Dec") )) %>%
  group_by_at(vars(scenario, region, basin, land, year)) %>%
  summarise(across(LUC:CLUC, ~ sum(.x)), .groups = "drop") %>%
  ungroup() %>% filter(year %in% c(2050, 2100)) ->
  LUC_CLUC_cum_agg

LUC_CLUC_cum_agg %>% SaveFigData("LUC_CLUC_cum_agg")

# *********************************--------
# connection starts here ----

LUC_CLUC_cum_agg <- LoadFigData("LUC_CLUC_cum_agg")
LUC_cum <- LoadFigData("LUC_cum")

# aggregate to world
LUC_CLUC_cum_agg %>%
  group_by_at(vars(scenario, land, year)) %>%
  summarise(across(LUC:CLUC, ~ sum(.x)), .groups = "drop") %>%
  mutate(density = CLUC / LUC * 1000) %>%
  gather(variable, value, CLUC, LUC, density) %>%
  proc_scen() %>%
  mutate(scenario = paste0(LandSupply, "_", LCT)) %>%
  select(scenario, year, variable, sector = land, value) %>%
  spread(scenario, value) ->
  MainTab_connection_LUC_CLUC
# MainTab_connection_LUC_CLUC ----

LUC_cum %>% filter(land != "Fixed") %>%
  mutate(land = replace(land, grepl("Cropland", land), "Cropland | NonEnergy")) %>%
  mutate(land = replace(land, grepl("Biomass", land), "Cropland | Energy")) %>%
  mutate(land = replace(land, grepl("ForestM", land), "Forest | Managed")) %>%
  mutate(land = replace(land, grepl("ForestUM", land), "Forest | Unmanaged")) %>%
  mutate(land = replace(land, grepl("GrassShrubland", land), "Other Natural Land")) %>%
  filter(year %in% c(2050, 2100)) %>%
  Agg_reg(land) %>% mutate(value = value / (year - 2019)) %>%
  mutate(variable = "Land") %>%
  proc_scen() %>%
  mutate(scenario = paste0(LandSupply, "_", LCT)) %>%
  select(scenario, year, variable, sector = land, value) %>%
  spread(scenario, value) ->
  MainTab_Connection_Land
# MainTab_Connection_Land ----

BiomassALL_SupplyToDemand <-LoadFigData("BiomassALL_SupplyToDemand_Mapped")


BiomassALL_SupplyToDemand %>%
  Agg_reg(demandsector, supplysector, CCS) %>%
  group_by_at(vars(-year,-value)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  group_by(scenario) %>%
  filter(year %in% c(2050, 2100)) %>% ungroup() %>%
  proc_scen() %>%
  Agg_reg(Policy_Scenario = LandSupply, LCP_scenario = LCT, year, supplysector, CCS,
          unit = "EJ per yr (mean)") %>%
  select(-scenario) %>%
  mutate(value = value / (year - 2019)) %>%
  mutate(supplysector = gsub("Supply: ", "", supplysector),
         supplysector = paste0(supplysector, "_", CCS)) %>%
  select(-CCS) %>% mutate(variable = "BiomassEJ") %>%
  mutate(scenario = paste0(Policy_Scenario, "_", LCP_scenario)) %>%
  select(scenario, year, variable, sector = supplysector, value, unit) %>%
  spread(scenario, value) -> MainTab_Connection_BiomassEJ1

LoadFigData("Enbioresidue") -> Enbioresidue

Enbioresidue %>% filter(year >= 2020) %>%
  mutate(sector = if_else(grepl("Forest", sector), "Forestry", "Crop")) %>%
  Agg_reg(sector, source = "Residue", target, SDR) %>%
  group_by_at(vars(-year,-value)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  filter(year %in% c(2050, 2100)) %>% ungroup() %>%
  mutate(value = value / (year - 2019)) %>%
  proc_scen() %>%
  mutate(sector = paste0("Residue_", sector)) %>%
  transmute(Policy_Scenario = LandSupply, LCP_scenario = LCT, year, sector, value,
            unit = "EJ per yr (mean)") %>%
  mutate(variable = "BiomassEJ") %>%
  mutate(scenario = paste0(Policy_Scenario, "_", LCP_scenario)) %>%
  select(scenario, year, variable, sector, value, unit) %>%
  spread(scenario, value) -> MainTab_Connection_BiomassEJ_residue

# MainTab_Connection_BiomassEJ----
MainTab_Connection_BiomassEJ1 %>%
  bind_rows(MainTab_Connection_BiomassEJ_residue) %>% arrange(year) %>%
  mutate(sector = gsub("_", " | ", sector))->
  MainTab_Connection_BiomassEJ


  LandToEMs <- LoadFigData("LandToEMs_DeeperUpdates")

  LandToEMs %>% select(-Mha) %>%
    transmute(Policy_Scenario = LandSupply, LCP_scenario = LCT, year, sector,
              value = GtCO2, unit = "GtCO2 /yr") %>%
    mutate(variable = "LandCDR") %>%
    mutate(scenario = paste0(Policy_Scenario, "_", LCP_scenario)) %>%
    select(scenario, year, variable, sector, value, unit) %>%
    spread(scenario, value) %>% arrange(year) ->
    MainTab_Connection_LandCDR
  # MainTab_Connection_LandCDR----

  LandToEMs %>% select(-GtCO2) %>%
    transmute(Policy_Scenario = LandSupply, LCP_scenario = LCT, year, sector,
              value = Mha, unit = "Mha") %>%
    mutate(variable = "Land") %>%
    mutate(scenario = paste0(Policy_Scenario, "_", LCP_scenario)) %>%
    select(scenario, year, variable, sector, value, unit) %>%
    spread(scenario, value) %>% arrange(year) ->
    MainTab_Connection_LandCDR_Land
  # MainTab_Connection_LandCDR_Land ----

  # Bind ----

  scenario0 <-
    c("2C Main_No-LCP" , "2C Main_10%-LCP", "2C Main_50%-LCP", "2C Main_100%-LCP",
      "A/R-Focused_10%-LCP",  "A/R-Focused_50%-LCP", "A/R-Focused_100%-LCP",
      "Low-Bioenergy_No-LCP", "Low-Bioenergy_10%-LCP", "Low-Bioenergy_50%-LCP",  "Low-Bioenergy_100%-LCP",
      "1.5C_No-LCP", "1.5C_10%-LCP", "1.5C_50%-LCP", "1.5C_100%-LCP")

  orderscenario <- function(.df){
    .df %>%
      gather(scenario, value, scenario0) %>%
      mutate(scenario = factor(scenario, levels = scenario0)) %>%
      spread(scenario, value)
  }


  MainTab_Connection_LandCDR %>%
    orderscenario %>%
    bind_rows(
      MainTab_Connection_BiomassEJ %>%
        gather(scenario, value, scenario0) %>%
        mutate(sector = if_else(grepl("MSW|Residue", sector), gsub(" \\| CCS| \\| NoCCS", "", sector),
                                sector )) %>%
        Agg_reg(variable, sector, unit) %>%
        spread(scenario, value)
    ) %>%
    bind_rows(
      MainTab_Connection_BiomassEJ %>%
        gather(scenario, value, scenario0) %>%
        mutate(value = value *23*44/12/1000) %>%
        mutate(unit = "GtCO2 /yr", variable = "Biogenic Carbon") %>%
        filter(grepl("Crop|For", sector) == F) %>%
        mutate(sector = gsub("MSW|Residue", "Residue & MSW", sector)) %>%
        Agg_reg(variable, sector, unit) %>%
        spread(scenario, value)
    )%>%
    bind_rows(
      MainTab_Connection_Land %>% mutate(unit = "Mha") %>%
        bind_rows(
          MainTab_Connection_LandCDR_Land %>%
            filter(sector %in% c("Land-based (CCS) BECCS", "Land-based (NonCCS) BECCS")) %>%
            mutate(sector = replace(sector, sector == "Land-based (CCS) BECCS", "Cropland | Energy | CCS" ),
                   sector = replace(sector, sector == "Land-based (NonCCS) BECCS", "Cropland | Energy | NonCCS" ))
        )
    ) %>%
    bind_rows(
      MainTab_connection_LUC_CLUC %>% filter(variable == "CLUC") %>%
        mutate(variable = "Land Carbon Stock Change") %>%
        mutate(unit = "GtCO2 /yr") %>%
        gather(scenario, value, scenario0) %>%
        mutate(value = value / (year-2019) ) %>%
        spread(scenario, value) %>%
        mutate(sector = replace(sector, sector == "GrassShrubland", "Other Natural Land")) %>%
        filter(sector != "CropPasture")
    ) %>%
    arrange(-year) ->
    MainTab_connection_binded

  MainTab_connection_binded %>% SaveFigData("MainTab_connection", .SourceForPub = T)




