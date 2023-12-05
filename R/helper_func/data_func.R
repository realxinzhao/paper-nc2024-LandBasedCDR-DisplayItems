SaveFigData <- function(.DF, .NAME, .SourceForPub = F){
  assertthat::assert_that(is.data.frame(.DF))

  if (.SourceForPub == T) {
    .DF %>% readr::write_csv(file = file.path("output/FigSourceData/", paste0(.NAME, ".csv")))
  } else {
    .DF %>% saveRDS(file.path("output/FigDataRDS/", paste0(.NAME, ".RDS")))
    #.DF %>% readr::write_csv(file = file.path("output/FigDataCSV/", paste0(.NAME, ".csv")))
  }
}


LoadFigData <- function(.NAME){
  FilePath <- file.path("output/FigDataRDS/", paste0(.NAME, ".RDS"))
  assertthat::assert_that(file.exists(FilePath))
  readRDS(FilePath)
}


read.processed.rds <- function(rdsDir,
                               querynames = gsub(pattern= "*.rds$", "", list.files(path = rdsDir, pattern="*.rds$"))){

  for (x in querynames){assign(paste0(x),readRDS(paste0(rdsDir, x,".rds")), envir = .GlobalEnv)}
}

Agg_reg <- function(.data, ...){
  .data %>%
    group_by(scenario, ... , year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    ungroup() %>%
    return()
}

Proc_FD <- function(.data, type = "A", ...){
  if (type == "A") {
    .data %>%
      group_by(...) %>%
      mutate(value = value - first(value)) %>%
      ungroup() %>%
      return()
  } else {
    .data %>%
      group_by(...) %>%
      mutate(value = value / first(value)) %>%
      ungroup() %>%
      return()
  }
}

Proc_data <- function(cropmapto = "crop2"){
  Detailedland1 <- Detailedland %>% Agg_reg(region, crop) %>%
    mutate(value = value/10, variable = "area")

  Agprod1 <- Agprod %>% Agg_reg(region, crop) %>%
    mutate(variable = "prod")

  Agdemand1 <- Agdemand %>% Agg_reg(region, crop) %>%
    mutate(variable = "consume")

  RegAgsource1 <- RegAgsource %>% filter(crop %in% unique(Agprod$crop)) %>%
    spread(source, value) %>% rename(import = imported)

  Agprice1 <- Agprice %>% Agg_reg(region, crop) %>%
    filter(crop %notin% c("unmanagedland", "biomass")) %>% bind_rows(
      Agprice %>% Agg_reg(region, crop) %>%
        filter(crop %in% c("biomass")) %>% mutate(crop = "biomass_grass")
    ) %>% bind_rows(
      Agprice %>% Agg_reg(region, crop) %>%
        filter(crop %in% c("biomass")) %>% mutate(crop = "biomass_tree")
    ) %>% mutate(variable = "price")

  LUCemissions1 <- LUCemissions %>% Agg_reg(region, crop) %>%
    mutate(variable = "LUCcarbon")

  NLUCemissions_crop1 <- NLUCemissions_crop  %>%
    Agg_reg(region, variable = GHG, crop) %>% filter(variable == "CO2") %>%
    mutate(variable = "nLUCcarbon")

  unique(Detailedland1$crop)
  unique(LUCemissions1$crop)
  unique(NLUCemissions_crop1$crop)
  unique(Agprod$crop)
  unique(Agprice1$crop)
  unique(Agdemand1$crop)
  unique(RegAgsource1$crop)

  data <-  Agprice1 %>% spread(variable, value) %>%
    left_join(Agprod1 %>% spread(variable, value)) %>%
    left_join(Agdemand1 %>% spread(variable, value)) %>%
    left_join(RegAgsource1) %>%
    left_join(LUCemissions1 %>% spread(variable, value)) %>%
    left_join(NLUCemissions_crop1 %>% spread(variable, value)) %>%
    left_join(Detailedland1 %>% spread(variable, value)) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(revenue = price * prod) %>% left_join(Cropmap) %>%
    within(rm(price)) %>%
    gather(variable, value,
           c("area", "prod", "consume", "domestic",
             "import", "revenue", "LUCcarbon", "nLUCcarbon")) %>%
    Agg_reg(region, crop = get(cropmapto), variable) %>%
    spread(variable, value) %>%
    mutate(price = replace_na(revenue / prod, 0),
           yield = replace_na(prod / area, 0),
           export = if_else(prod - domestic < 0, 0, prod - domestic),
           netexport = export - import) %>%
    gather(variable, value,
           c("area", "prod", "consume", "domestic",
             "export", "import","netexport", "yield", "price",
             "revenue", "LUCcarbon", "nLUCcarbon"))
  return(data)
}
