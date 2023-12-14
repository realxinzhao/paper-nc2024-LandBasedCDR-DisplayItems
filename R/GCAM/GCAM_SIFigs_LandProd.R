SIFigs_LandProd <- function(){

  outdir <- "output/GCAM/"
  OutFolderName <- "SI"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)


  LoadFigData("LandALL")  -> Land1

  # two combinded figures  ----
  # figa  Time-series Land allocation + LUC bar chart with mean ----

  Land1 %>% Agg_reg(land) %>%
    #left_join(NZ_PeakYear, by = "scenario") %>%
    #filter(!(!is.na(NZyear) & year > NZyear)) %>% select(-NZyear) %>%
    proc_scen() %>%
    ggplot +   facet_grid( LandSupply ~ LCT) +
    geom_hline(yintercept = 0) +
    geom_area(aes(x = year, y = value, fill = land), stat = "identity", position = "stack",
              color = "black") +
    labs(x = "Year", y = "Mha") +
    scale_fill_brewer(palette = "Spectral",
                      name = "Land", direction = -1) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          strip.background = element_rect(fill="grey99"),
          strip.text = element_text(size = 16),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A1; A1


  Land1 %>% Agg_reg(land) %>%
    proc_scen() %>%  filter(year >= 2020) %>%
    group_by_at(vars(-year, -value)) %>%
    mutate(value = value - first(value)) %>% ungroup() -> DF_LUC

  # Source data saving ----
  # DF_LUC %>% group_by_at(vars(-value, -year)) %>%
  #   Fill_annual() %>%
  #   summarise(value = mean(value), .groups = "drop") %>%
  #   mutate(year = "2020-2100") %>%
  #   bind_rows(
  #     DF_LUC %>% group_by_at(vars(-value, -year)) %>%
  #       Fill_annual() %>% filter(year <= 2050) %>%
  #       summarise(value = mean(value), .groups = "drop") %>%
  #       mutate(year = "2020-2050")
  #   ) %>%
  #   transmute(Policy_Scenario = LandSupply, LCP_scenario = LCT, year, land,
  #             value, unit = "Mha") %>%
  #   filter(!grepl("Rock|Urban", land)) %>%
  #   spread(land, value) %>% arrange(year) %>%
  #   SaveFigData("TableConnection_Land", .SourceForPub = T)

  DF_LUC %>% group_by_at(vars(-value, -year)) %>%
    Fill_annual() %>%
    summarise(value = mean(value) ) %>%
    mutate(year = "Mean") %>%
    bind_rows(DF_LUC %>% mutate(year = as.character(year))) ->
    DF_LUC1

  seq(2020, 2100,5) -> x
  x[x %in% c(seq(2020, 2100,5) %>% setdiff(seq(2020, 2100,20)))] <- ""

  DF_LUC1 %>%
    ggplot +   facet_grid( LandSupply ~ LCT) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 250, linetype = 2, color = "grey") +
    geom_hline(yintercept = -250, linetype = 2, color = "grey") +
    geom_hline(yintercept = 500, linetype = 2, color = "grey") +
    geom_hline(yintercept = -500, linetype = 2, color = "grey") +
    geom_hline(yintercept = 750, linetype = 2, color = "grey") +
    geom_hline(yintercept = -750, linetype = 2, color = "grey") +
    geom_hline(yintercept = 1000, linetype = 2, color = "grey") +
    geom_hline(yintercept = -1000, linetype = 2, color = "grey") +
    geom_bar(aes(x = year, y = value, fill = land), stat = "identity", position = "stack",
             color = "black") +
    labs(x = "Year", y = "Mha") +
    scale_x_discrete( labels = c(x, "Mean")) +
    scale_fill_brewer(palette = "Spectral",
                      name = "Land", direction = -1) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90), legend.text.align = 0,
          strip.background = element_rect(fill="grey99"),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A2; A2


  (A1 + labs(title = "(A) Global land use") +
      theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none",
            axis.title.x = element_blank()) ) /
    (A2  +  labs(title = "(B) Global land use change") +
       theme(plot.title = element_text(hjust = 0, face = "bold"), axis.title.x = element_blank()) )+
    plot_layout(heights = c(0.5, 0.5),guides = 'collect') -> p

  p %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_LULUC"), h = 5850, w = 5400,  r = 300)





  Land1 %>% filter(year >= 2020) %>%
    filter(!grepl("Rock|Urban", land)) %>%
    mutate(land = gsub("- Staples", "- Others", land)) %>%
    mutate(land = gsub(" - Unmanaged| - Managed", "", land)) %>%
    mutate(land = replace(land, land %in% c("Grassland", "Shrubland"), "Other natural")) %>%
    group_by_at(vars(-value)) %>% summarise(value = sum(value), .groups = "drop") %>%
    group_by_at(vars(-year, -value)) %>%
    mutate(value = value - first(value)) %>% ungroup() -> Land2

  library(RColorBrewer)
  br_pal0 <- brewer.pal(11,"BrBG"); scales::show_col(br_pal0)
  br_pal <- brewer.pal(11,"RdYlBu"); scales::show_col(br_pal)
  br_pal2 <- brewer.pal(5,"Spectral"); scales::show_col(br_pal2)
  br_pal3 <- brewer.pal(9,"Set1"); scales::show_col(br_pal3)

  mypal <- c( br_pal[8], br_pal2[4], br_pal3[1], #br_pal[3],
              br_pal3[5], br_pal0[6]); scales::show_col(mypal)


  Land2 %>% Agg_reg(land) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual() %>%
    summarise(value = mean(value) ) %>%ungroup() %>%
    proc_scen() -> DF
  DF %>%
    ggplot +   facet_grid( ~LandSupply) +
    geom_hline(yintercept = 500, linetype = 2, color = "grey") +
    geom_hline(yintercept = -500, linetype = 2, color = "grey") +
    geom_hline(yintercept = 250, linetype = 2, color = "grey") +
    geom_hline(yintercept = -250, linetype = 2, color = "grey") +
    geom_hline(yintercept = 750, linetype = 2, color = "grey") +
    geom_hline(yintercept = -750, linetype = 2, color = "grey") +
    geom_hline(yintercept = 1000, linetype = 2, color = "grey") +
    geom_hline(yintercept = -1000, linetype = 2, color = "grey") +
    geom_hline(yintercept = 0) +
    geom_bar(aes(x = LCT, y = value, fill = land), stat = "identity", position = "stack",
             color = "black") +
    labs(x = "Year", y = "Mha") +
    geom_text(data =  DF %>% filter(land == "Cropland - Energy"),
              aes(x = LCT, y = 1250, label = round(value,0)),
              hjust = 0.5, size = 6, color = mypal[1], fontface = 4
    ) +
    geom_text(data =  DF %>% filter(land == "Cropland - Others"),
              aes(x = LCT, y = -1250, label = round(value,0)),
              hjust = 0.5, size = 6, color = mypal[2], fontface = 4
    ) +
    geom_text(data =  DF %>% filter(land == "Forest"),
              aes(x = LCT, y = 1100, label = round(value,0)),
              hjust = 0.5, size = 6, color = mypal[3], fontface = 4
    ) +
    geom_text(data =  DF %>% filter(land == "Other natural"),
              aes(x = LCT, y = -1100, label = round(value,0)),
              hjust = 0.5, size = 6, color = mypal[4], fontface = 4
    ) +

    scale_fill_manual(values = mypal, name = "Land") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          axis.title.x = element_blank(), legend.position = "right",
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A1; A1


  Land2 %>% Agg_reg(land, region) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual() %>%
    summarise(value = mean(value) ) %>%ungroup() %>%
    proc_scen() -> DF_reg
  DF_reg %>%
    ggplot +   facet_grid(LandSupply ~ region) +
    geom_hline(yintercept = 200, linetype = 2, color = "grey") +
    geom_hline(yintercept = -200, linetype = 2, color = "grey") +
    geom_hline(yintercept = 100, linetype = 2, color = "grey") +
    geom_hline(yintercept = -100, linetype = 2, color = "grey") +
    geom_hline(yintercept = 0) +
    geom_bar(aes(x = LCT, y = value, fill = land), stat = "identity", position = "stack",
             color = "black") +

    geom_text(data =  DF_reg %>% filter(land == "Cropland - Energy"),
              aes(x = LCT, y = 250, label = round(value,0)),
              hjust = 0.5, size = 4.5, color = mypal[1], fontface = 4
    ) +
    geom_text(data =  DF_reg %>% filter(land == "Cropland - Others"),
              aes(x = LCT, y = -250, label = round(value,0)),
              hjust = 0.5, size = 4.5, color = mypal[2], fontface = 4
    ) +
    geom_text(data =  DF_reg %>% filter(land == "Forest"),
              aes(x = LCT, y = 300, label = round(value,0)),
              hjust = 0.5, size = 4.5, color = mypal[3], fontface = 4
    ) +
    geom_text(data =  DF_reg %>% filter(land == "Other natural"),
              aes(x = LCT, y = -300, label = round(value,0)),
              hjust = 0.5, size = 4.5, color = mypal[4], fontface = 4
    ) +

    labs(x = "Year", y = "Mha") +
    scale_fill_manual(values = mypal, name = "Land") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          axis.title.x = element_blank(), legend.position = "right",
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A2; A2



  library(cowplot)
  plot_grid(
    A1 + labs(title = "(A) Mean (2020 - 2100) change in land use, world") +
      theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "right"),
    A2  +  labs(title = "(B) Mean (2020 - 2100) change in land use by region") +
      theme(plot.title = element_text(hjust = 0, face = "bold"),legend.position = "none"), ncol = 1,
    align = c("v"), rel_heights = c(0.5, 1)

  ) -> p


  p %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_LULUC_reg"), h = 5550, w = 5400,  r = 300)



  # Bioenergy production & yield

  # decompose to get biomass carbon yield_cum ----

  LoadFigData("BiomassCYield") -> BiomassCYield


  BiomassCYield %>%
    group_by_at(vars(scenario, year)) %>%
    summarise(yield = sum(EJ) / sum(value) * 1000, .groups = "drop") %>%
    proc_scen() %>%
    ggplot()+ facet_grid( ~LandSupply) +
    geom_hline(yintercept = 100, color = "grey60", linetype = 5) +
    geom_hline(yintercept = 200, color = "grey60", linetype = 5) +
    geom_hline(yintercept = 300, color = "grey60", linetype = 5) +
    geom_line(aes(x = year, y = yield, group = interaction(scenario), color = LCT, linetype = LCT), size = 1) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2030, 2090, 20)) +
    scale_color_manual(values = mycol) +
    scale_linetype_manual(values = c(1:3,5)) +
    labs(x = "Year", y = "GJ per hectare", linetype = "Scenario",
         color = "Scenario") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90),
          legend.key.width = unit(1,"cm"),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A1; A1


  BiomassCYield %>%
    mutate(yield = EJ/ value *1000) %>%
    proc_scen() %>%
    ggplot()+ facet_grid( LandSupply ~ region, scales = "fixed"  ) +
    geom_hline(yintercept = 100, color = "grey60", linetype = 5) +
    geom_hline(yintercept = 200, color = "grey60", linetype = 5) +
    geom_hline(yintercept = 300, color = "grey60", linetype = 5) +
    geom_hline(yintercept = 150, color = "grey60", linetype = 5) +
    geom_hline(yintercept = 250, color = "grey60", linetype = 5) +
    geom_line(aes(x = year, y = yield, group = interaction(scenario, region), color = LCT, linetype = LCT), size = 1) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2030, 2090, 20)) +
    scale_color_manual(values = mycol) +
    scale_linetype_manual(values = c(1:3,5)) +
    labs(x = "Year", y = "GJ per hectare", linetype = "Land mitigation policy",
         color = "Land mitigation policy") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90),
          legend.key.width = unit(1,"cm"),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A2; A2

  plot_grid(
    A1 + labs(title = "(A) Primary purpose-grown bioenergy yield, world") +
      theme(plot.title = element_text(hjust = 0, face = "bold"),
            axis.title.x = element_blank(), legend.position = "right"),
    A2  +  labs(title = "(B) Primary purpose-grown bioenergy yield by region") +
      theme(plot.title = element_text(hjust = 0, face = "bold"),legend.position = "none"), ncol = 1,
    align = c("v"), rel_heights = c(0.5, 1)

  ) -> p


  p %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_BiomassCarbonYield"), h = 5550, w = 5400,  r = 300)



  LoadFigData("AgBalYield") -> df


  library(ggsci)
  df %>%
    #filter(year %in% c(2025, 2050, 2100)) %>%
    filter(element %in% c("Production", "Land", "Yield")) %>%
    group_by_at(vars(year, element)) %>%
    mutate(value = value / value[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"]) %>%
    filter(scenario != "BioUn_ProtLow_LCT0_FCT0_REF") %>% ungroup() %>%
    ggplot() +
    facet_grid(LandSupply~LCT) +
    geom_hline(yintercept = 0.8, linetype = 5, color = "grey85") +
    geom_hline(yintercept = 0.9, linetype = 5, color = "grey85") +
    geom_hline(yintercept = 1.1, linetype = 5, color = "grey85") +
    geom_hline(yintercept = 1.2, linetype = 5, color = "grey85") +
    geom_hline(yintercept = 1) +
    #geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack", color = "black")+
    geom_line(aes(x = year, y = value, color = element), size = 1.5)+
    scale_color_brewer(palette = "Set1") +
    labs(x = "Year", y = "Index (Reference = 1)", color = "Variable") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          panel.grid = element_blank(),
          panel.spacing.y = unit(.8, "lines"),
          panel.spacing.x = unit(.8, "lines")) -> A; A

  A %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_YieldChange"), h = 2800, w = 3800,  r = 300)


  df %>%
    filter(year %in% c(2025, 2050, 2100)) %>%
    filter(!element %in% c("Production", "Land", "Yield")) %>%
    group_by_at(vars(year, element)) %>%
    mutate(value = value - value[scenario == "BioUn_ProtLow_LCT0_FCT0_REF"]) %>%
    filter(scenario != "BioUn_ProtLow_LCT0_FCT0_REF") %>% ungroup() %>%
    mutate(year = as.character(year)) %>%
    ggplot() +
    facet_grid(LandSupply~LCT) +
    geom_hline(yintercept = -400, linetype = 5, color = "grey85") +
    geom_hline(yintercept = -200, linetype = 5, color = "grey85") +
    geom_hline(yintercept = 200, linetype = 5, color = "grey85") +
    geom_hline(yintercept = 0) +
    geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack", color = "black")+
    #geom_line(aes(x = year, y = value, color = element), size = 1.5)+
    scale_fill_brewer(palette = "Set2") +
    labs(x = "Year", y = "Mt (Reference = 0)", fill = "Variable") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          panel.grid = element_blank(),
          panel.spacing.y = unit(.8, "lines"),
          panel.spacing.x = unit(.8, "lines")) -> A; A


  A %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_BalanceChange"), h = 2800, w = 3800,  r = 300)





  LoadFigData("FoodCalories") %>% filter(year >= 2020) -> FoodCalories

  FoodCalories %>% filter(year %in% c(2020, 2100,2050)) %>%
    group_by_at(vars(scenario, year)) %>%
    summarise(value = weighted.mean(value, w = pop), .groups = "drop") %>%
    spread(year, value) %>% proc_scen() -> A

  FoodCalories %>% filter(year %in% c(2020, 2100,2050)) %>%
    group_by_at(vars(scenario, year)) %>%
    summarise(value = weighted.mean(value, w = pop), .groups = "drop") %>%
    group_by_at(vars(-scenario, -value)) %>%
    mutate(value = value - value[scenario == "DB_BioUn_ProtLow_LCT0_FCT0_REF"]) %>%
    spread(year, value) %>% proc_scen() -> A

  FoodCalories %>% filter(year %in% c(2100)) %>%
    group_by_at(vars(-scenario, -value, -pop)) %>%
    mutate(value = value - value[scenario == "DB_BioUn_ProtLow_LCT0_FCT0_REF"]) %>%
    proc_scen() -> A

  FoodCalories %>% #filter(year %in% c(2050, 2100)) %>%
    proc_scen() %>% filter(LandSupply != "Reference") %>%
    ggplot()  + facet_grid(LandSupply ~ region, scales = "fixed") +
    geom_hline(yintercept = 4000, color = "grey60", linetype = 5) +
    geom_hline(yintercept = 3000, color = "grey60", linetype = 5) +
    geom_hline(yintercept = 3500, color = "grey60", linetype = 5) +
    geom_hline(yintercept = 2500, color = "grey60", linetype = 5) +
    geom_line(aes(x = year, y = value, group = interaction(scenario, region), color = LCT, linetype = LCT), size = 0.8) +
    geom_line(data = FoodCalories %>%  #filter(year %in% c(2050, 2100)) %>%
                proc_scen() %>% filter(LandSupply == "Reference") %>% select(-LandSupply),
              aes(x = year, y = value, group = interaction(scenario, region)), size = 0.5, color = "black") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2030, 2090, 20)) +
    scale_color_manual(values = mycol) +
    scale_linetype_manual(values = c(1:3,5)) +
    labs(x = "Year", y = "kilocalories per capita per day", linetype = "Land mitigation policy",
         color = "Land mitigation policy") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90),
          legend.key.width = unit(1,"cm"),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A2; A2


  FoodCalories %>%
    group_by_at(vars(scenario, year)) %>%
    summarise(value = weighted.mean(value, w = pop), .groups = "drop") %>%
    proc_scen() %>% filter(LandSupply != "Reference") %>%
    ggplot()+ facet_grid( ~LandSupply) +
    geom_line(aes(x = year, y = value, group = interaction(scenario), color = LCT, linetype = LCT), size = 1) +
    geom_line(data = FoodCalories %>%
                group_by_at(vars(scenario, year)) %>%
                summarise(value = weighted.mean(value, w = pop), .groups = "drop") %>%
                proc_scen() %>% filter(LandSupply == "Reference") %>% select(-LandSupply),
              aes(x = year, y = value, group = interaction(scenario)), size = 0.5, color = "black") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2030, 2090, 20)) +
    scale_color_manual(values = mycol) +
    scale_linetype_manual(values = c(1:3,5)) +
    labs(x = "Year", y = "kilocalories per capita per day", linetype = "Scenario",
         color = "Scenario") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90),
          legend.key.width = unit(1,"cm"),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A1; A1

  plot_grid(
    A1 + labs(title = "(A) Dietary energy availability, world") +
      theme(plot.title = element_text(hjust = 0, face = "bold"),
            axis.title.x = element_blank(), legend.position = "right"),
    A2  +  labs(title = "(B) Dietary energy availability by region") +
      theme(plot.title = element_text(hjust = 0, face = "bold"),legend.position = "none"), ncol = 1,
    align = c("v"), rel_heights = c(0.5, 1)

  ) -> p


  p %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_FoodCalories_Reg"), h = 5550, w = 5400,  r = 300)


  }
