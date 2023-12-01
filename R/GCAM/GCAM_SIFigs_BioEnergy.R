SIFigs_BioEnergy <- function(){

  outdir <- "output/GCAM/"
  OutFolderName <- "SI"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)


  LoadFigData("BiomassALL_PrimaryEnergyBal") -> BiomassALL

  BiomassALL %>% proc_scen() %>%
    mutate(sector = factor(sector,
                           levels = c("Supply: Purpose-grown","Supply: Residue", "Supply: MSW",
                                      "Demand: Final energy",  "Demand: Gas", "Demand: Hydrogen", "Demand: Refining", "Demand: Electricity") ) )  ->
    BiomassALL


  BiomassALL %>% mutate(value = if_else(DS == "demand", -value, value)) %>%
    Agg_reg(sector, DS) %>%
    proc_scen() %>%
    ggplot +   facet_grid( LandSupply ~ LCT) +
    geom_hline(yintercept = 0) +
    geom_area(aes(x = year, y = value, fill = sector),
              stat = "identity", position = "stack",
              color = "black") +
    labs(x = "Year", y =  expression(paste(EJ, " ", yr^-1))) +
    scale_fill_brewer(palette = "RdYlGn", name = "Sector", direction = 1,
                      limits = c(
                        "Supply: Purpose-grown","Supply: Residue","Supply: MSW",
                        "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                      )) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A1; A1



  BiomassALL %>% mutate(value = if_else(DS == "demand", -value, value)) %>%
    Agg_reg(sector, LandSupply, LCT) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
    mutate(value = value / 81) %>%
    group_by(scenario, sector, LandSupply, LCT) %>%
    filter(year == unique(last(year))) %>% ungroup() %>%
    proc_scen() %>%
    ggplot +  facet_wrap( ~LandSupply, nrow = 1) +
    geom_hline(yintercept = 0) +
    geom_bar(aes(x = LCT, y = value, fill = sector),
             stat = "identity", position = "stack",
             color = "black") +
    labs(x = "Land C pricing strength scenario", y =  expression(paste(EJ, " ", yr^-1)) ) +
    #scale_fill_brewer(palette = "RdYlGn", name = "Sector", direction = -1) +
    scale_fill_brewer(palette = "RdYlGn", name = "Sector", direction = 1,
                      limits = c(
                        "Supply: Purpose-grown","Supply: Residue","Supply: MSW",
                        "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                      )) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A; A



  BiomassALL %>% mutate(value = if_else(DS == "demand", -value, value)) %>%
    Agg_reg(sector, LandSupply, LCT) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
    mutate(value = value / 81) %>%
    group_by(scenario, sector, LandSupply, LCT) %>%
    filter(year == unique(last(year))) %>% ungroup() %>%
    proc_scen() -> BiomassALLMean



  BiomassALL %>% mutate(value = if_else(DS == "demand", -value, value)) %>%
    mutate(value = value / 1000) %>%
    Agg_reg(sector) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
    group_by(scenario, sector) %>%
    filter(year == unique(last(year))) %>% ungroup() %>%
    proc_scen() %>%
    ggplot +   facet_wrap( ~LandSupply, nrow = 1) +
    geom_hline(yintercept = 0) +
    geom_bar(aes(x = LCT, y = value, fill = sector),
             stat = "identity", position = "stack",
             color = "black") +

    geom_text(data =  BiomassALLMean %>% filter(grepl("Demand", sector)) %>%
                group_by(LCT, LandSupply) %>% mutate(value = value /sum(value)) %>%
                filter(sector == "Demand: Electricity"),
              aes(x = LCT, y = -15, label = paste0(round(value * 100,0), "%")),
              hjust = 0.5, size = 5.5, color = "green", fontface = 4
    ) +


    geom_text(data =  BiomassALLMean %>% filter(sector == "Supply: Purpose-grown"),
              aes(x = LCT, y = 15, label = -round(value,0)),
              hjust = 0.5, size = 5.5, color = "red", fontface = 4
    ) +


    geom_text(data =  BiomassALLMean %>% filter(grepl("Demand", sector)) %>%
                group_by(LCT, LandSupply) %>% summarise(value = sum(value)),
              aes(x = LCT, y = -18, label = round(value,0)),
              hjust = 0.5, size = 5.5, color = "black", fontface = 4
    ) +

    labs(x = "Land C pricing strength scenario", y = "1000 EJ") +
    scale_fill_brewer(palette = "RdYlGn", name = "Sector", direction = 1,
                      limits = c(
                        "Supply: Purpose-grown","Supply: Residue","Supply: MSW",
                        "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                      )) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A2; A2



  (A2 + labs(title = "(A) Cumulative primary bioenergy demand and supply in 2020 - 2100") +
      theme(plot.title = element_text(hjust = 0, face = "bold") ) ) /
    (A1  +  labs(title = "(B) Annual primary bioenergy demand and supply") +
       theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none") )+
    plot_layout(heights = c(0.4, 1),guides = 'collect') -> p

  p %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_PrimaryBiomass"), h = 4500, w = 4400,  r = 300)



  BiomassALL %>%
    filter(DS == "demand") %>%
    Agg_reg(CCS, sector) %>%
    #mutate(value = if_else(CCS == "CCS", value, -value)) %>%
    #left_join(NZ_PeakYear, by = "scenario") %>%
    #filter(!(!is.na(NZyear) & year > NZyear)) %>% select(-NZyear) %>%
    proc_scen() %>%
    ggplot +   facet_grid( LandSupply ~ LCT) +
    guides(fill = guide_legend(order = 1),
           linetype = guide_legend(order = 2),
           alpha = guide_legend(order = 3)) +
    geom_hline(yintercept = 0) +
    geom_bar(aes(x = year, y = value, fill = sector, alpha = CCS, linetype = CCS, size = CCS),
             stat = "identity", position = "stack", color = "black",
    ) +
    labs(x = "Year", y =  expression(paste(EJ, " ", yr^-1)), alpha = "CCS technology", size = "CCS technology",
         linetype = "CCS technology") +
    scale_alpha_manual(values = c(0.8, 1)) +
    scale_linetype_manual(values = c(1, 2)) +
    scale_size_manual(values = c(0.5, 0.3)) +
    #scale_fill_brewer(palette = "RdYlGn", name = "Demand sector", direction = -1) +
    scale_fill_brewer(palette = "RdYlGn", name = "Sector", direction = -1,
                      limits = c(
                        "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                      )) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A1; A1


  BiomassALL %>%
    filter(DS == "demand") %>%
    mutate(value = value / 1000) %>%
    Agg_reg(CCS) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
    group_by(scenario) %>%
    filter(year == unique(last(year))) %>% ungroup() %>%
    mutate(value = if_else(CCS == "CCS", value, -value)) %>%
    spread(CCS, value) %>%
    mutate(CCSShare = CCS / (CCS - NoCCS)) -> BiomassALL_CCSShare

  BiomassALL %>%
    filter(DS == "demand") %>%
    mutate(value = value / 1000) %>%
    Agg_reg(CCS, sector) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
    group_by(scenario) %>%
    filter(year == unique(last(year))) %>% ungroup() %>%
    #mutate(value = if_else(CCS == "CCS", value, -value)) %>%
    proc_scen() %>%
    ggplot +   facet_grid( ~LandSupply) +
    guides(fill = guide_legend(order = 1),
           linetype = guide_legend(order = 2),
           alpha = guide_legend(order = 3)) +
    geom_hline(yintercept = 0) +
    geom_bar(aes(x = LCT, y = value, fill = sector, alpha = CCS, linetype = CCS),
             stat = "identity", position = "stack", size = 0.5,
             color = "black") +

    geom_text(data =  BiomassALL_CCSShare %>% proc_scen(),
              aes(x = LCT, y = 14, label = paste0(round(CCSShare * 100,0), "%")),
              hjust = 0.5, size = 5.5, color = "black", fontface = 4
    ) +

    labs(x = "Land mitigation policy", y = "1000 EJ", alpha = "CCS technology", linetype = "CCS technology") +
    scale_alpha_manual(values = c(0.8, 1)) +
    scale_linetype_manual(values = c(1, 2)) +
    scale_fill_brewer(palette = "RdYlGn", name = "Sector", direction = -1,
                      limits = c(
                        "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                      )) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A2; A2


  (A2 + labs(title = "(A) Cumulative primary bioenergy demand in 2020 - 2100 by CCS technology") +
      theme(plot.title = element_text(hjust = 0, face = "bold") ) ) /
    (A1  +  labs(title = "(B) Annual primary bioenergy demand by CCS technology") +
       theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none") )+
    plot_layout(heights = c(0.4, 1),guides = 'collect') -> p

  p %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_PrimaryBiomassCCS_update"), h = 4500, w = 4400,  r = 300)

  # Note that we tested different versions of this SI figure
  # please find the code here
  # source("R/GCAM/GCAM_SIFigs_BioEnergy_AddonTests.R")




  BiomassALL %>%
    mutate(value = if_else(DS == "demand", -value, value)) %>%
    mutate(value = value / 81) %>%
    Agg_reg(sector, region) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
    group_by(scenario) %>%
    filter(year == unique(last(year))) %>% ungroup() %>%
    proc_scen() -> BiomassALLReg

  BiomassALLReg %>%
    ggplot +   facet_grid(LandSupply ~ region) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 10, linetype = 2, color = "grey") +
    geom_hline(yintercept = -10, linetype = 2, color = "grey") +
    geom_hline(yintercept = 20, linetype = 2, color = "grey") +
    geom_hline(yintercept = -20, linetype = 2, color = "grey") +
    geom_hline(yintercept = 30, linetype = 2, color = "grey") +
    geom_hline(yintercept = -30, linetype = 2, color = "grey") +
    geom_bar(aes(x = LCT, y = value, fill = sector),
             stat = "identity", position = "stack",
             color = "black") +

    # check net
    # geom_point(data =  BiomassALLReg %>% group_by(LCT, LandSupply, region) %>%
    #              summarise(value = sum(value))%>% mutate(ss = "Net Total"),
    #            aes(x = LCT, y = value, color = ss), size = 10, shape = "-") +
    # scale_color_manual(values = c("blue", "red", "Black")) +



    geom_text(data =  BiomassALLReg %>% filter(grepl("Demand", sector)) %>%
                group_by(LCT, LandSupply, region) %>% mutate(value = value /sum(value)) %>%
                filter(sector == "Demand: Electricity"),
              aes(x = LCT, y = -39, label = paste0(round(value * 100,0), "%")),
              hjust = 0.5, size = 4, color = "green", fontface = 4
    ) +

    geom_text(data =  BiomassALLReg %>% filter(sector == "Supply: Purpose-grown"),
              aes(x = LCT, y = 43, label = -round(value,0)),
              hjust = 0.5, size = 4.5, color = "red", fontface = 4
    ) +


    geom_text(data =  BiomassALLReg %>% filter(grepl("Demand", sector)) %>%
                group_by(LCT, LandSupply) %>% summarise(value = sum(value)),
              aes(x = LCT, y = -47, label = round(value,0)),
              hjust = 0.5, size = 4.5, color = "black", fontface = 4
    ) +
    scale_y_continuous(breaks = seq(-40, 40, 20)) +
    labs(x = "Year", y =  expression(paste(EJ, " ", yr^-1))) +
    scale_fill_brewer(palette = "RdYlGn", name = "Sector", direction = 1,
                      limits = c(
                        "Supply: Purpose-grown","Supply: Residue","Supply: MSW",
                        "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                      )) +
    scale_alpha_manual(name = "CCS", values = c(0.7, 1)) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          axis.title.x = element_blank(), legend.position = "bottom",
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A; A


  A %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_PrimaryBiomass_cumulative_reg"), h = 3700, w = 5400,  r = 300)



}
