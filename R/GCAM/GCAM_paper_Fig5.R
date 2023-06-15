Fig5 <- function(){

  library(patchwork)
  library(cowplot)

  OutFolderName <- "Fig5"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)

  # AR6 results ----
  readRDS("output/AR6/RDS_temp/AR6_604.rds") -> AR6_604

  BREAKs <- c(175,500,825,1150,1475)

  AR6_604 %>%
    filter(grepl("Corn|Rice|Soybean|Wheat|Price_CO2", Var))-> AR6_KeyVar

  AR6_604 %>%
    filter(Variable %in% "Emissions|CO2") %>%
    # change unit to GtCO2 and cumulative
    group_by_at(vars(-value, -year)) %>%
    mutate(value = cumsum(value)/ 1000) %>% ungroup() %>%
    filter(!is.na(value)) %>%
    group_by(Pathway) %>%
    mutate(EM_CO2_2100 = value[Var == "EM_CO2" & year == 2100]) %>% ungroup() %>%
    mutate(EM_CO2_2100_cut = cut(EM_CO2_2100, breaks = BREAKs, dig.lab = 4 )) %>%
    select(Pathway, year, EM_CO2_2100_cut) %>%
    right_join(
      AR6_KeyVar, by = c("Pathway", "year")) ->
    DF_TS

  DF_TS %>% filter(year %in% c(2100), Var == "Price_CO2") %>%
    filter(value > 1500)
  #123/574

  DF_TS %>% filter(year %in% c(2100), Var == "Price_CO2") %>%
    filter(value == 0) %>% distinct(Pathway) ->
    ZeroCPPathway

  ColorViridis4 <- viridis::viridis(11, alpha = 0.8)[c(1,4,7,10)]






  LoadFigData("EMs_mitigation_CO2Price2010") -> EMs_mitigation_CO2Price2010

  EMs_mitigation_CO2Price2010 %>%
    mutate(market = factor(market,
                           levels = c("Fossil Fuels", "BECCS", "Land-Forest", "Land-NonForest"),
                           labels = c("FFI", "BECCS", "Land-Forest", "Land-NonForest"))) ->
    EMs_mitigation_CO2Price2010

# mitigation & shadow prices ----
  EMs_mitigation_CO2Price2010 %>%
    ggplot() + facet_grid(LandSupply~LCT) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 1000, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 500, linetype = 2, color = "grey50") +
    #geom_hline(yintercept = 1500, linetype = 2, color = "grey50") +
    geom_line(aes(x = year, y = CO2Price2010, group = market, color = market, linetype = market), size = 1.5) +
    scale_color_manual(name = "Sector", values = mycol) +
    labs(x = "Year", linetype = "Sector",
         y = expression(paste("2010$ per tonne ", CO[2])) ) +
    theme_bw() + theme0 + theme_leg +
    theme(axis.text.x = element_text(angle = 0))+
    theme(panel.spacing = unit(0.5, "lines"), panel.grid = element_blank())-> A1;A1

  A1 %>% Write_png(paste0(OutFolderName, "/CP_Annual_FF+NEB+Bioceiling"), h = 3400, w = 5000,  r = 300)


  EMs_mitigation_CO2Price2010 %>%
    ggplot() + facet_grid(LandSupply~LCT) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -20, linetype = 2, color = "grey50") +
    geom_hline(yintercept = -40, linetype = 2, color = "grey50") +
    geom_hline(yintercept = -60, linetype = 2, color = "grey50") +
    geom_hline(yintercept = -80, linetype = 2, color = "grey50") +
    geom_area(aes(x = year, y = RemovalGtCO2, group = market, fill = market), stat = "identity", position = "stack",
              color = "black") +
    scale_fill_manual(name = "Sector", values = mycol) +
    labs(x = "Year",
         y = expression(paste(GtCO[2], " ", yr^-1))) +
    theme_bw() + theme0 + theme_leg +
    theme(axis.text.x = element_text(angle = 0))+
    theme(panel.spacing = unit(0.6, "lines"), panel.grid = element_blank())-> A2;A2
  A2 %>% Write_png(paste0(OutFolderName, "/Mitigation_annual"), h = 3400, w = 5000,  r = 300)


  (A1 + labs(title = "(A) Shadow price of carbon by sector") +
      theme(plot.title = element_text(hjust = 0, face = "bold"), axis.title.x = element_blank() ) ) /
    (A2  +  labs(title = "(B) Carbon mitigation relative to reference by sector") +
       theme(plot.title = element_text(hjust = 0, face = "bold")) )+
    plot_layout(heights = c(0.5, 0.5),guides = 'collect') -> p
  p %>% Write_png(paste0(OutFolderName, "/CP_migitation_bind"), h = 4600, w = 4400,  r = 300)

## done first ----


  DF_TS1 <- DF_TS %>%
    filter(!Pathway %in% (ZeroCPPathway %>% pull)) %>%
    filter(year %in% c(2025, 2050, 2100), Var == "Price_CO2") %>%
    mutate(year = as.character(year)) %>%
    group_by(year, EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(`value`, probs = 0.1),
              sd25 = quantile(`value`, probs = 0.25),
              sd05 = quantile(`value`, probs = 0.5),
              sd75 = quantile(`value`, probs = 0.75),
              sd100 = quantile(`value`, probs = .9),
              mean = mean(value),
              n = n()) %>% ungroup()

  DF_TS2 <- DF_TS %>%
    filter(!Pathway %in% (ZeroCPPathway %>% pull)) %>%
    filter(year %in% c(2025, 2050, 2100), Var == "Price_CO2") %>%
    mutate(year = as.character(year)) %>%
    group_by(year) %>%
    summarise(sd0 = quantile(`value`, probs = 0.1),
              sd25 = quantile(`value`, probs = 0.25),
              sd05 = quantile(`value`, probs = 0.5),
              sd75 = quantile(`value`, probs = 0.75),
              sd100 = quantile(`value`, probs = .9),
              mean = mean(value),
              n = n()) %>% ungroup()


  DF_TS1  %>%
    mutate(sd75 = pmin(1500, sd75),
           sd100 = pmin(1500, sd100)) %>%
    ggplot(aes(x = year)) +
    geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 500, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1000, linetype = 2, color = "grey50") +

    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut), lwd=0.5, fatten = 1, stat = "identity") +
    geom_errorbar(data = DF_TS2, aes(ymin = sd05, ymax = sd05), color = "blue", linetype = 1) +


    #geom_boxplot(aes(x = year, y = value, fill = EM_CO2_2100_cut), outlier.shape = NA) +
    scale_fill_manual(values = c(ColorViridis4)) +
    labs(x = "Year", fill = expression(paste("AR6 CB (", GtCO[2], ")")),
         y = expression(paste("2010$ per t", CO[2])) ) +
    scale_y_continuous(expand = c(0,0), limits = c(-30, 1500)) +
    theme_bw() + theme0 +
    theme(legend.text.align = 0,
          panel.grid = element_blank())-> AR6_CP;AR6_CP


  DF_TS1 %>%
    bind_rows(DF_TS2 %>% mutate(EM_CO2_2100_cut = "[175, 1475]")) %>%
    SaveFigData(.NAME = "Fig2_AR6")


  EMs_mitigation_CO2Price2010 %>%
    filter(year %in% c(2025, 2050, 2100)) %>%
    select(LCT, Scenario = LandSupply, year, market, CO2Price2010) %>%
    spread(market, CO2Price2010) %>%
    SaveFigData(.NAME = "Fig2_GCAM")

  EMs_mitigation_CO2Price2010 %>%
    filter(year %in% c(2025, 2050, 2100)) %>%
    mutate(year = as.character(year)) %>%
    ggplot() + facet_wrap(~market, nrow = 1) +
    geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 500, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1000, linetype = 2, color = "grey50") +

    # geom_boxplot(aes(x = year, y = CO2Price2010, group = interaction(year, LCT),fill = LCT),
    #              fatten = NULL, outlier.shape = NA, alpha = 0.7) +
    geom_pointrange(data =   EMs_mitigation_CO2Price2010 %>%
                      filter(year %in% c(2025, 2050, 2100)) %>%
                      mutate(year = as.character(year)) %>%
                      group_by(market, year, LCT) %>%
                      summarise(max = max(CO2Price2010),
                                min = min(CO2Price2010), mid = .5*(max +min)),
                    aes(x = year, y = mid, ymax = max, ymin = min, color = LCT),
                    position=position_dodge(width= .7), alpha = 0.8, #shape = "-",
                    size = 1, fatten = 1.6
                    ) +

    geom_point(aes(x = year, y = CO2Price2010, group = interaction(LCT),
                    color = LCT, shape = LandSupply),  size = 3.5,
               position=position_dodge(width= .7)
               ) +
    scale_shape_manual(values = c(1, 0, 2, 10)) +
    scale_fill_npg() + scale_color_npg() +
    scale_y_continuous(expand = c(0,0), limits = c(-30, 1500)) +
    labs(x = "Year", fill = "LCP scenario", shape = "Policy scenario", color = "LCP scenario",
         y = expression(paste("2010$ per t", CO[2])) ) +
    theme_bw() + theme0 +
    theme(#axis.text.x = element_text(angle = 30, hjust = 0.7, vjust = 1, margin = margin(t =10, b = 0)),
      legend.text.align = 0,
      panel.grid = element_blank(),
      strip.background = element_blank(),
      panel.spacing.y = unit(0.5, "lines"),
      panel.spacing.x = unit(0.5, "lines")
    ) -> GCAM_CP; GCAM_CP


    (GCAM_CP  +  labs(title = "(A) GCAM shadow price of carbon by sector and scenario") +
       theme(plot.title = element_text(hjust = 0, face = "bold")) )+
    (AR6_CP + labs(title = "(B) AR6 carbon prices") +
       theme(plot.title = element_text(hjust = 0, face = "bold") ) ) +
    plot_layout(widths = c(1, 0.35),guides = 'collect') -> p

  p %>% Write_png(paste0(OutFolderName, "/CPAR6_GCAM1"), h = 2000, w = 5000,  r = 300)
# Good CP export ----




  # Load price and cropland data ----
  LoadFigData("StapleCropPriceReg") -> StapleCropPriceReg


  LoadFigData("LandALL") %>% filter(land %in% c("Cropland - Others", "Cropland - Staples")) %>%
    bind_rows(
      LoadFigData("Land_REF") %>% proc_scen() %>%
        filter(land %in% c("Cropland - Others", "Cropland - Staples"))
    ) -> Cropland





  StapleCropPriceReg %>%
    group_by_at(vars(-price, -prod, -sector)) %>%
    summarise(price = weighted.mean(price, prod), prod = sum(prod)) %>%
    ungroup() %>%
    proc_scen() %>%
    left_join(
      Cropland %>% filter(year >= 2020) %>% #filter(land == "Cropland - Staples") %>%
        group_by_at(vars(-value, -land)) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        rename(cropland = value),
      by = c("scenario", "Biolimit", "Prot", "LCT", "FCT", "LandSupply", "region", "year", "ref", "target")
    ) ->
    StapleCropPriceProdCropLandReg



  StapleCropPriceProdCropLandReg %>%
    group_by_at(vars(-price, -prod, -region, -cropland)) %>%
    summarise(price = weighted.mean(price, prod), prod = sum(prod), cropland = sum(cropland)) %>%
    ungroup() %>%
    filter(year >= 2020) %>%
    group_by_at(vars(-year, -cropland, -price, -prod)) %>%
    mutate(price = price / price[year == 2020],
           cropland = cropland - cropland[year == 2020]) %>%
    filter(year != 2020) %>% filter(LCT != "Reference") %>%
    left_join(
      EMs_mitigation_CO2Price2010 %>%
        filter(year %in% seq(2025, 2100, 5), market == "Fossil Fuels") %>%
        select(-RemovalGtCO2, -CO2Price2015)
    ) -> StapleCropPriceProdCropLandCP

  StapleCropPriceProdCropLandCP %>% filter(year %in% c(2050, 2100)) %>%
    ggplot() + facet_wrap(~year, nrow = 2) +
    geom_point(aes(x = cropland, y = price, group = LCT,
                   fill = LandSupply, shape = LandSupply), color = "black", size = 3) +
    geom_smooth(aes(x = cropland, y = price), method = 'lm') +
    scale_shape_manual(values = c(21, 22, 24, 23)) +
    labs(x = "Scenario", fill = "Scenario", shape = "Scenario",
         y = expression(paste("2010$ per tonne ", CO[2])) ) +
    theme0 + theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.7, vjust = 1, margin = margin(t =10, b = 0)),
          legend.text.align = 0,
          panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines"),
          axis.title.x = element_blank()
    ) -> A1;A1

  StapleCropPriceProdCropLandCP %>% filter(year %in% c(2050, 2100)) %>%
    ggplot() + facet_wrap(~year, nrow = 2) +
    geom_point(aes(x = CO2Price2010, y = price, group = LCT,
                   fill = LandSupply, shape = LandSupply), color = "black", size = 3) +
    geom_smooth(aes(x = CO2Price2010, y = price), method = 'lm') +
    scale_shape_manual(values = c(21, 22, 24, 23)) +
    labs(x = "Scenario", fill = "Scenario", shape = "Scenario",
         y = expression(paste("2010$ per tonne ", CO[2])) ) +
    theme0 + theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.7, vjust = 1, margin = margin(t =10, b = 0)),
          legend.text.align = 0,
          panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines"),
          axis.title.x = element_blank()
    ) -> A2;A2







  StapleCropPriceProdCropLandReg %>%
    group_by(region, year) %>%
    mutate(price = price / price[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"],
           cropland = cropland - cropland[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"]) %>%
    filter(scenario != "BioUn_ProtLow_LCT0_FCT0_REF") %>%
    ungroup() %>% filter(year %in% c(2030, 2050, 2100)) %>%
    mutate(year = as.character(year)) %>%
    ggplot() + facet_grid(year ~region) +

    geom_hline(yintercept = 1, linetype = 1, color = "grey50") +
    geom_hline(yintercept = 1.2, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1.4, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1.6, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1.8, linetype = 2, color = "grey50") +

    geom_smooth(aes(x = cropland, y = price), method = 'lm') +
    geom_point(aes(x = cropland, y = price, fill = LCT, color = LCT, shape = LandSupply),
                size = 3) +

    scale_shape_manual(values = c(21, 22, 24, 23)) +
    scale_fill_npg() +
    scale_color_npg() +
    scale_y_continuous(expand = c(0, 0), breaks = seq(1, 1.8, 0.2)) +
    labs(x = "Reference = 0 (Mha)",
         color = "LCP scenario", fill = "LCP scenario", shape = "Sector policy scenario",
         y = "Reference = 1 (Index)" ) +
    theme_bw() + theme0 + theme_leg +
    theme(legend.text.align = 0,
          panel.grid = element_blank(),
          #strip.background = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")
    ) + theme(legend.position = "bottom") -> A2; A2

  A2  %>%
    Write_png(paste0(OutFolderName, "/StapleCropPrice_Cropland_Relref"), h = 2000, w = 7000,  r = 300)



  StapleCropPriceProdCropLandReg %>%
    group_by_at(vars(-price, -prod, -region, -cropland)) %>%
    summarise(price = weighted.mean(price, prod), prod = sum(prod), cropland = sum(cropland)) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(price = price / price[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"],
           cropland = cropland - cropland[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"]) %>%
    filter(scenario != "BioUn_ProtLow_LCT0_FCT0_REF") %>%
    ungroup() %>% filter(year %in% c(2030, 2050, 2100)) %>%
    mutate(year = as.character(year)) %>% arrange(cropland)
    ggplot() + facet_wrap(~year) +

    geom_hline(yintercept = 1, linetype = 1, color = "grey50") +
    geom_hline(yintercept = 1.2, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1.4, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1.6, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1.8, linetype = 2, color = "grey50") +

    geom_smooth(aes(x = cropland, y = price), method = 'lm') +
    geom_point(aes(x = cropland, y = price,
                   fill = LCT, shape = LandSupply, color = LCT), size = 3) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(1, 1.8, 0.2)) +
    scale_shape_manual(values = c(21, 22, 24, 23)) +
    scale_fill_npg() +
    scale_color_npg() +
    labs(x = "Reference = 0 (Mha)",
         color = "LCP scenario", fill = "LCP scenario", shape = "Sector policy scenario",
         y = "Reference = 1 (Index)" ) +
    theme_bw() + theme0 +
    theme(legend.text.align = 0,
          panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")
    ) -> A1; A1



  library(cowplot)
  plot_grid(
    A1  +  labs(title = "(A) Staple crop price change vs. cropland change, relative to reference, world") +
      theme(plot.title = element_text(hjust = 0, face = "bold")) ,
    A2 + labs(title = "(B) Staple crop price change vs. cropland change, relative to reference, regional") +
      theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none" ), ncol = 1,
    align = c("v"), rel_heights = c(0.6, 0.5)

  ) -> p

  p %>% Write_png(paste0(OutFolderName, "/StapleCropPrice_Cropland_Relref_All"), h = 4800, w = 7000,  r = 300)






  # data for fit ----
  StapleCropPriceProdCropLandReg %>%
    group_by(scenario, year) %>%
    summarise(price = weighted.mean(price, prod)) %>%
    mutate(sector = "Staple food crops") %>%
    filter(year >= 2025) %>%
    #group_by_at(vars(-year, -price)) %>%
    #mutate(price = price / first(price)) %>% ungroup() %>%
    group_by_at(vars(-scenario, -price)) %>%
    mutate(price = price / price[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"]) %>%
    filter(scenario != "BioUn_ProtLow_LCT0_FCT0_REF") %>% ungroup() %>%
    rename(AgPrice = price) %>%
    right_join(
      EMs_mitigation_CO2Price2010 %>% filter(year %in% seq(2025, 2100, 5)) %>%
        select(-RemovalGtCO2, -CO2Price2015) %>%
        spread(market, CO2Price2010) %>%
        #select(-`LULUCF: NonForest`) %>%
        #rename( FFI = `Fossil Fuels`) %>%
        mutate(LULUCF = `Land-Forest`) %>%
        gather(CPsector, CP, FFI:LULUCF ) %>%
        group_by(scenario, CPsector) %>%
        mutate(CPRel = CP/first(CP)) %>%
        replace_na(list(CPRel = 1)),
      by = c("scenario", "year")
    ) -> StapleP_CP1




  # log price vs. C price----
  library(broom)
  library(jtools)

  StapleP_CP1  %>% select(-sector, -CPRel) %>%
    mutate(AgPrice = log(AgPrice)*100, CP = CP ) %>%
    spread(CPsector, CP) %>%
    mutate(BECCS_Tax = FFI - BECCS) %>%
    proc_scen() -> DF_regression




  lm(AgPrice ~ FFI -1, data =  DF_regression) -> lm_LogP_CPs
  tidy(lm_LogP_CPs) %>% mutate(Model = "Model1") %>%
    mutate(adjR2 = summary(lm_LogP_CPs)$adj.r.squared) -> M1 ; M1

  lm(AgPrice ~ FFI + `Land-Forest` -1, DF_regression) -> lm_LogP_CPs
  tidy(lm_LogP_CPs) %>% mutate(Model = "Model2") %>%
    mutate(adjR2 = summary(lm_LogP_CPs)$adj.r.squared) -> M2 ;M2

  lm(AgPrice ~ FFI + `Land-Forest` + `Land-NonForest` -1, DF_regression) -> lm_LogP_CPs
  tidy(lm_LogP_CPs) %>% mutate(Model = "Model3") %>%
    mutate(adjR2 = summary(lm_LogP_CPs)$adj.r.squared) -> M3 ;M3




  lm(AgPrice ~ BECCS + FFI + `Land-Forest` + `Land-NonForest` -1, DF_regression) -> lm_LogP_CPs
  tidy(lm_LogP_CPs) %>% mutate(Model = "Model4") %>%
    mutate(adjR2 = summary(lm_LogP_CPs)$adj.r.squared) -> M4 ;M4


  # lm(AgPrice ~ BECCS + FFI + `Land-Forest` + `Land-NonForest` + factor(scenario), DF_regression) -> lm_LogP_CPs
  # tidy(lm_LogP_CPs) %>% mutate(Model = "Model0") %>%
  #   mutate(adjR2 = summary(lm_LogP_CPs)$adj.r.squared) -> M0 ; M0



  lm(AgPrice ~ BECCS_Tax + FFI + `Land-Forest` + `Land-NonForest` -1, DF_regression) -> lm_LogP_CPs
  tidy(lm_LogP_CPs) %>% mutate(Model = "Model5") %>%
    mutate(adjR2 = summary(lm_LogP_CPs)$adj.r.squared) -> M5 ;M5


  lm(AgPrice ~ BECCS + FFI + `Land-Forest` * ForOnly -1 -ForOnly ,
     DF_regression %>% mutate(ForOnly = if_else(LandSupply == "A/R-Focused",
                                                "ARFocused", "NonARFocused"))) -> lm_LogP_CPs
  tidy(lm_LogP_CPs) %>% mutate(Model = "Model6") %>%
    mutate(adjR2 = summary(lm_LogP_CPs)$adj.r.squared) -> M6 ;M6

  lm(AgPrice ~ BECCS_Tax + FFI + `Land-Forest` * ForOnly -1 -ForOnly ,
     DF_regression %>% mutate(ForOnly = if_else(LandSupply == "A/R-Focused",
                                                "ARFocused", "NonARFocused"))) -> lm_LogP_CPs
  tidy(lm_LogP_CPs) %>% mutate(Model = "Model6") %>%
    mutate(adjR2 = summary(lm_LogP_CPs)$adj.r.squared) -> M6 ;M6


  lm(AgPrice ~ BECCS + FFI + `Land-Forest` + `Land-NonForest`, DF_regression) -> lm_LogP_CPs
  tidy(lm_LogP_CPs) %>% mutate(Model = "Model7") %>%
    mutate(adjR2 = summary(lm_LogP_CPs)$adj.r.squared) -> M7 ;M7

  lapply(paste0("M", 1:7), function(x){
    get(x)
  })  %>% bind_rows() -> M1_7

  M1_7 %>% transmute(estimate = if_else(term == "(Intercept)", estimate, estimate),
                     std.error = if_else(term == "(Intercept)", std.error, std.error),
                     `Explanatory variable` = term, Model, adjR2) %>%
    gather(element, value, estimate, std.error, adjR2) %>%
    filter(! (element == "adjR2" & `Explanatory variable` != "FFI")) %>%
    spread(Model, value) %>%
    arrange(element) %>% write.csv("Fig6Regression.csv")




  lm(AgPrice ~ BECCS + FFI + `Land-Forest` + `Land-NonForest` -1,
     data = StapleP_CP1 %>% select(-sector, -CPRel) %>%
       mutate(AgPrice = log(AgPrice) *100) %>%
       spread(CPsector, CP) %>%
       proc_scen() ) -> lm_LogP_CPs


  lm_LogP_CPs %>% summary -> resultsLM
  confint(lm_LogP_CPs, level = 0.95) -> conf


  resultsLM$coefficients %>% as.data.frame() %>%
    bind_cols( conf %>% as.data.frame()  ) %>%
    tibble::rownames_to_column() -> df
 names(df) <- c("var", "beta", "se", "t", "p", "p025", "p975")

 lm(AgPrice ~ BECCS_Tax + FFI + `Land-Forest` * ForOnly -1 -ForOnly ,
    DF_regression %>% mutate(ForOnly = if_else(LandSupply == "A/R-Focused",
                                               "ARFocused", "NonARFocused"))) ->
   lm_LogP_CPs

 lm_LogP_CPs %>% summary -> resultsLM
 confint(lm_LogP_CPs, level = 0.95) -> conf

 conf %>% as.data.frame() %>%
   tibble::rownames_to_column() %>%
   filter(rowname != "`Land-Forest`:ForOnlyNonARFocused") -> df0
 resultsLM$coefficients %>% as.data.frame() %>%
   tibble::rownames_to_column() %>%
   left_join(df0) -> df
 names(df) <- c("var", "beta", "se", "t", "p", "p025", "p975")


 df %>% mutate(var = gsub("`", "", var)) %>%
   # mutate(var = factor(var,
   #                     levels = c( "Land-Forest", "Land-NonForest", "BECCS", "FFI"),
   #                     labels = c( "Land:\nForest", "Land:\nNonForest", "BECCS", "Fossil\nFuels"))) %>%
   # mutate(var = factor(var,
   #                     levels = c( "FFI", "BECCS", "Land-Forest", "Land-NonForest"),
   #                     labels = c( "FFI","BECCS",  "Land:\nForest", "Land:\nNonForest"))) %>%

   mutate(var = factor(var,
                       levels = c( "FFI", "BECCS_Tax", "Land-Forest", "Land-Forest:ForOnlyARFocused"),
                       labels = c( "EIP","BECCS\nlimit",  "Land", "Land, if \nA/R-Focused"))) %>%

   ggplot() +
   geom_hline(yintercept = 0) +
   geom_hline(yintercept = -.02, linetype = 2, color = "grey50") +
   geom_hline(yintercept = .02, linetype = 2, color = "grey50") +
   geom_hline(yintercept = -.04, linetype = 2, color = "grey50") +
   geom_hline(yintercept = .04, linetype = 2, color = "grey50") +
   geom_pointrange(aes(x = var, y=beta, ymin = p025, ymax = p975, color = var),
                   size = 1) +
   labs(x = "Sector", color = "Sector",
        y = "Marginal effect of $100 (2010 USD) increase in \nshadow price of carbon dioxide  on staple crop prices") +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0, -0.02, 0.02, .04, -.04) ) +
   #scale_color_npg() +
   #scale_color_manual(values = mycol)+
   scale_color_economist() +
   theme_bw() + theme0 +
   theme(legend.text.align = 0, legend.position = "none",
         axis.text.x = element_text(vjust = 1),
         panel.grid = element_blank()) -> FitPlot; FitPlot




 # AR6 GCAM crop prices ----

 DF_TS %>% filter(year %in% c(2020, 2025, 2050, 2100)) %>%
   filter(Var != "Price_CO2") %>%
   group_by_at(vars(-value, -year)) %>%
   mutate(value = value / first(value)) %>% filter(year != 2020) %>% ungroup() %>%
   mutate(Unit = "Index (2020 = 1)") %>%
   inner_join(
     DF_TS %>% filter(year %in% c(2025, 2050, 2100), Var == "Price_CO2") %>%
       select(Pathway, year, CP = value), by = c("Pathway", "year")
   ) -> DF_TS_Price



 # DF_TS_Price %>% filter((Project_study == "NGFS2" & Model == "GCAM 5.3")) %>% distinct(Pathway)
 # Need to remove 2 GCAM pathways


 DF_TS_Price %>%
   filter(!Pathway %in% c("GCAM 5.3.NGFS2_Delayed Transition", "GCAM 5.3.NGFS2_Net-Zero 2050")) %>%
   distinct(Pathway)

 DF_TS_Price %>% select(Pathway, Var, year, value) %>% spread(year, value) %>%
   filter(`2100` < `2025`) %>% distinct(Pathway) -> PathwayLowerPrice

 # n = 42 - 2 GCAM -12 IMAGE  = 28
 DF_TS_Price %>% #distinct(Pathway)
   filter(!Pathway %in% c("GCAM 5.3.NGFS2_Delayed Transition", "GCAM 5.3.NGFS2_Net-Zero 2050")) %>%
   filter(!Pathway %in% (PathwayLowerPrice %>% pull) )->
   DF_TS_Price

 df1 <- DF_TS_Price %>%
   group_by(EM_CO2_2100_cut, year) %>%
   summarise(sd0 = quantile(`value`, probs = 0.05),
             sd25 = quantile(`value`, probs = 0.25),
             sd05 = quantile(`value`, probs = 0.5),
             sd75 = quantile(`value`, probs = 0.75),
             sd100 = quantile(`value`, probs = .95)) %>% ungroup()

 df2 <- DF_TS_Price %>%
   group_by(year) %>%
   summarise(sd0 = quantile(`value`, probs = 0.05),
             sd25 = quantile(`value`, probs = 0.25),
             sd05 = quantile(`value`, probs = 0.5),
             sd75 = quantile(`value`, probs = 0.75),
             sd100 = quantile(`value`, probs = .95)) %>% ungroup()

 df1 %>% mutate( sd100 = pmin(sd100, 1.8), sd0 = pmax(0.9, sd0)) %>%
   mutate(year = as.character(year)) %>%
   ggplot() +

   geom_hline(yintercept = 1, linetype = 1, color = "grey50") +
   geom_hline(yintercept = 1.2, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.4, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.6, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.8, linetype = 2, color = "grey50") +

   # geom_boxplot(aes(x = year, y = value, fill = EM_CO2_2100_cut)#, outlier.shape = NA
   #              ) +

   geom_boxplot(aes(x = year, ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                    ymax = sd100, fill =  EM_CO2_2100_cut, group = interaction(EM_CO2_2100_cut, year)),
                alpha= .7,  lwd=0.5, fatten = 1, stat = "identity", width = 0.8,
                position=position_dodge(width= 0.9)) +
   geom_errorbar(data = df2 %>% mutate(year = as.character(year)),
                 aes(x = year, ymin = sd05, ymax = sd05), width = 1, color = "blue", linetype = 1) +

   scale_fill_manual(values = c(ColorViridis4)) +
   scale_y_continuous(expand = c(0, 0), breaks = seq(1, 1.8, 0.2), limits = c(0.9, 1.8)) +
   labs(x = "Year", fill = expression(paste("AR6 CB (", GtCO[2], ")")),
        y = "Index (2020 = 1)" ) +
   #scale_y_continuous(expand = c(0,0), limits = c(-30, 1500)) +
   theme_bw() + theme0 + theme_leg +
   theme(legend.text.align = 0, legend.key.size = unit(0.8, "cm"),
         panel.grid = element_blank())-> AR6_CropP;AR6_CropP




 StapleCropPriceReg %>% select(-prod) %>%
   group_by_at(vars(-year, -price)) %>%
   mutate(price = price / first(price)) -> A

 StapleCropPriceProdCropLandReg %>%
   group_by_at(vars(-price, -prod, -region, -cropland)) %>%
   summarise(price = weighted.mean(price, prod), prod = sum(prod), cropland = sum(cropland)) %>%
   ungroup() %>%
   filter(year >= 2020) %>%
   group_by_at(vars(-year, -cropland, -price, -prod)) %>%
   mutate(price = price / price[year == 2020],
          cropland = cropland - cropland[year == 2020]) %>%
   filter(year != 2020) %>%
   filter(year %in% c(2025, 2050, 2100)) %>%
   ungroup() %>%
   mutate(year = as.character(year)) -> df

 df %>% filter(year == 2100) %>% arrange(price)

   df %>% filter(LCT != "Reference") %>%
   ggplot() + facet_wrap(~LandSupply, nrow = 1) +

   geom_hline(yintercept = 1, linetype = 1, color = "grey50") +
   geom_hline(yintercept = 1.2, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.4, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.6, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.8, linetype = 2, color = "grey50") +

   geom_line(aes(x = year, y = price, group = interaction(LCT),
                  color = LCT),  size = 2) +
   geom_point(aes(x = year, y = price, group = interaction(year, LCT),
                   color = LCT, fill = LCT),  size = 3, shape = 21,  color = "black") +

     geom_line(data = df %>% filter(LCT == "Reference") %>% select(-LandSupply),
               aes(x = year, y = price, color = LCT, group = LCT),  size = 2, alpha = .8) +
     geom_point(data = df %>% filter(LCT == "Reference") %>% select(-LandSupply),
                aes(x = year, y = price, group = interaction(year, LCT),
                    color = LCT, fill = LCT),  size = 3, shape = 21,  color = "black", alpha = .8) +

   scale_fill_npg(limits = c("No-LCP", "10%-LCP",  "50%-LCP", "100%-LCP",  "Reference")) +
     scale_color_npg(limits = c("No-LCP", "10%-LCP",  "50%-LCP", "100%-LCP",  "Reference")) +
   scale_y_continuous(expand = c(0, 0), breaks = seq(1, 1.8, 0.2), limits = c(0.9, 1.8)) +
   labs(x = "Year", fill = "Scenario", shape = "Sector policy scenario", color = "Scenario",
        ##y = expression(paste("2010$ per t", CO[2])),
        y = "Index (2020 = 1)") +
   theme_bw() + theme0 + theme_leg +
   theme(#axis.text.x = element_text(angle = 30, hjust = 0.7, vjust = 1, margin = margin(t =10, b = 0)),
     legend.text.align = 0,
     panel.grid = element_blank(),
     strip.background = element_blank(),
     panel.spacing.y = unit(0.5, "lines"),
     panel.spacing.x = unit(0.5, "lines")
   ) -> GCAM_CropP; GCAM_CropP



 (GCAM_CropP  +  labs(title = "(A) GCAM global staple crop prices") +
     theme(plot.title = element_text(hjust = 0, face = "bold")) ) +
 (AR6_CropP + labs(title = "(B) AR6 crop prices") +
     theme(plot.title = element_text(hjust = 0, face = "bold") ) ) +
   plot_layout(widths = c(1, 0.35),guides = 'collect') -> p0

 p0 %>% Write_png(paste0(OutFolderName, "/CropPAR6_GCAM0"), h = 2000, w = 5000,  r = 300)



 StapleCropPriceReg %>%
   group_by_at(vars(-price, -prod, -sector)) %>%
   summarise(price = weighted.mean(price, prod), prod = sum(prod)) %>%
   ungroup() %>%
   proc_scen() %>%
   left_join(
     Cropland %>% filter(year >= 2020) %>% #filter(land == "Cropland - Staples") %>%
       group_by_at(vars(-value, -land)) %>%
       summarise(value = sum(value)) %>%
       ungroup() %>%
       rename(cropland = value),
     by = c("scenario", "Biolimit", "Prot", "LCT", "FCT", "LandSupply", "region", "year", "ref", "target")
   ) ->
   StapleCropPriceProdCropLandReg

 # Add one more regional plot on the cropland -price relationship

 StapleCropPriceProdCropLandReg %>%
   group_by(region, year) %>%
   mutate(price = price / price[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"],
          cropland = cropland - cropland[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"],
          prod = prod - prod[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"]) %>%
   filter(scenario != "BioUn_ProtLow_LCT0_FCT0_REF") %>%
   ungroup() %>% filter(year %in% c(2100)) %>%
   mutate(year = as.character(year)) %>%
   ggplot() + facet_wrap(~region, nrow = 2) +

   geom_hline(yintercept = 1, linetype = 1, color = "grey50") +
   geom_hline(yintercept = 1.2, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.4, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.6, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.8, linetype = 2, color = "grey50") +

   geom_smooth(aes(x = cropland, y = price, group = region), method = 'lm') +
   geom_point(aes(x = cropland, y = price, fill = LCT, color = LCT, shape = LandSupply),
              size = 3) +
   #scale_shape_manual(values = c(1, 0, 2, 10)) +
   scale_shape_manual(values = c(21, 22, 24, 23)) +
   scale_fill_npg() +
   scale_color_npg() +
   #scale_y_continuous(expand = c(0, 0), breaks = seq(1, 1.8, 0.2), limits = c(0.8, 1.9)) +
   #scale_x_continuous(expand = c(0, 0), breaks = c(-60,-40, -20, 0), limits = c(-80, 20))+
   labs(x = "Absolute difference in nonenergy cropland (Reference = 0 Mha)",
        color = "LCP scenario", fill = "LCP scenario", shape = "Policy scenario",
        y = "Relative difference in price (Reference = 1)" ) +
   theme_bw() + theme0 + theme_leg +
   theme(legend.text.align = 0,
         panel.grid = element_blank(),
         strip.background = element_blank(),
         panel.spacing.y = unit(0.5, "lines"),
         panel.spacing.x = unit(0.5, "lines")
   )  -> CropP_Cropland_RelRef;CropP_Cropland_RelRef




# legendp0 <- get_legend(p0 + theme(legend.box.margin = margin(l = 0, r = 0)))


   (FitPlot + labs(title = "(C) Marginal impact of \npolicy on crop prices") +
       theme(plot.title = element_text(hjust = 0, face = "bold")) +
      theme(axis.text.x = element_text(
        #angle = 25, hjust = 0.7, vjust = 1,
        margin = margin(t =2, b = 0), size = 14
        ))
      #theme(plot.title = element_text(hjust = 0, face = "bold") )
     ) +
   (CropP_Cropland_RelRef +
      theme(plot.title = element_text(hjust = 0, face = "bold"),
            axis.text.x = element_text(size = 14)) +
      labs(title = "(D) Staple crop price impact vs. cropland impact, relative to reference, 2100")) +
   plot_layout(widths = c(.4, 1),guides = 'collect') -> p2


 p0/p2 +  plot_layout(heights = c(0.4, 0.5))-> p
 # bind figs good ----


 p %>% Write_png(paste0(OutFolderName, "/CropPAR6_GCAM"), h = 3600, w = 5000,  r = 300)
 p %>% Write_png(paste0(OutFolderName, "/CropPAR6_GCAM1"), h = 3600, w = 5000,  r = 300)



 LoadFigData("LandALL") %>% #filter(land %in% c("Cropland - Others", "Cropland - Staples")) %>%
   bind_rows(
     LoadFigData("Land_REF") %>% proc_scen() #%>%
       #filter(land %in% c("Cropland - Others", "Cropland - Staples"))
   ) -> LandAll

 LoadFigData("EMs_mitigation_CO2Price2010") -> EMs_mitigation_CO2Price2010

 EMs_mitigation_CO2Price2010 %>%
   mutate(market = factor(market,
                          levels = c("Fossil Fuels", "BECCS", "Land-Forest", "Land-NonForest"),
                          labels = c("FFI", "BECCS", "Land-Forest", "Land-NonForest"))) ->
   EMs_mitigation_CO2Price2010



 LandAll %>% distinct(land)

 LandAll %>% filter(grepl("Forest|Energy", land)) -> LandForBio
 LandAll %>% filter(grepl("Staples|Others", land)) -> LandForBio
 LandAll %>% filter(grepl("Energy", land)) -> LandForBio
 LandAll %>% filter(grepl("Cropland|Forest", land)) -> LandForBio

 StapleCropPriceReg %>%
   group_by_at(vars(-price, -prod, -sector, -region)) %>%
   summarise(price = weighted.mean(price, prod), prod = sum(prod)) %>%
   ungroup() %>%
   proc_scen() %>%
   left_join(
     LandForBio %>% filter(year >= 2020) %>%
       mutate(land = gsub(" - Others| - Staples| - Managed| - Unmanaged", "", land))%>%
       group_by_at(vars(-value, -region)) %>%
       summarise(value = sum(value)) %>%
       ungroup() %>% rename(area = value)
   ) %>% select(scenario, year, land, area) %>%
   filter(year >= 2025) %>%
   #filter(year %in% c(2025, 2030, 2040, 2050)) %>%
   # filter(year %in% seq(2025, 2100, 5)) %>%
   # group_by(scenario, land) %>%
   # mutate(area = area - area[year == 2025]) %>% ungroup() %>%
   group_by(year, land) %>%
   mutate(area = area - area[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"]) %>%
   filter(scenario != "BioUn_ProtLow_LCT0_FCT0_REF") %>% ungroup() %>%
   full_join(
     EMs_mitigation_CO2Price2010 %>%
       select(scenario,LandSupply, LCT, CO2Price2010, market, year) %>%
       filter(year %in% seq(2025, 2100, 5))
   ) ->
   StapleCropPriceProdCropLandReg



 StapleCropPriceProdCropLandReg %>%
   #filter(year <= 2050) %>%
   filter(year %in% c(2050, 2100)) %>%
   mutate(year = as.character(year)) %>%
   mutate(land = factor(land, levels = c("Forest", "Cropland - Energy", "Cropland"),
                        labels = c("Forest", "Energy Cropland", "NonEnergy Cropland"))) %>%
   ggplot() + facet_grid(land~market, scales = "free_y") +

   geom_hline(yintercept = 1, linetype = 1, color = "grey50") +
   geom_hline(yintercept = 1.2, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.4, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.6, linetype = 2, color = "grey50") +
   geom_hline(yintercept = 1.8, linetype = 2, color = "grey50") +

   geom_point(aes(x = area, y = CO2Price2010, fill = LCT, color = LCT, shape = LandSupply), alpha = 0.8,
              size = 2) +
   geom_smooth(aes(x = area, y = CO2Price2010, group = year, linetype = year), method = 'lm', alpha = 0.2, fill = "red") +
   #scale_shape_manual(values = c(1, 0, 2, 10)) +
   scale_shape_manual(values = c(21, 22, 24, 23)) +
   scale_fill_npg() +
   scale_color_npg() +
   #scale_y_continuous(expand = c(0, 0), breaks = seq(1, 1.8, 0.2), limits = c(0.8, 1.9)) +
   #scale_x_continuous(expand = c(0, 0), breaks = c(-60,-40, -20, 0), limits = c(-80, 20))+
   labs(y = "Shadow price of carbon (2010 USD)",
        color = "LCP scenario", fill = "LCP scenario", shape = "Policy scenario", linetype = "Year",
        x = "Absolute difference in land area (Reference = 0 Mha)" ) +
   theme_bw() + theme0 + theme_leg +
   theme(legend.text.align = 0,
         panel.grid = element_blank(),
         strip.background = element_blank(),
         panel.spacing.y = unit(0.8, "lines"),
         panel.spacing.x = unit(0.8, "lines")
   ) +  guides(linetype = guide_legend(order = 1)) -> p;p


 p %>% Write_png(paste0(OutFolderName, "/LandvsCP2100"), h = 3200, w = 5000,  r = 300)


  }
