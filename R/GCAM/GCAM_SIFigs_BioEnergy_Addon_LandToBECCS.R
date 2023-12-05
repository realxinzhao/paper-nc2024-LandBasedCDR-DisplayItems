SIFigs_BioEnergy_addon_LandToBECCS <- function(){

  outdir <- "output/GCAM/"
  OutFolderName <- "SI"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)


  LoadFigData("BiomassALL_PrimaryEnergyBal") -> BiomassALL

  # Calcualte import share which will be applied to international supply share
  BiomassALL %>%
    group_by_at(vars(scenario, DS, region, year)) %>%
    summarize(value = sum(value)) %>%
    spread(DS, value) %>%
    mutate(importshare = (demand - supply ) / demand,
           importshare = pmax(0, importshare)) %>%
    select(scenario, region, year, importshare) ->
    BiomassImportShare

  # Calcualte supply share by source for both region (R10) and world
  BiomassALL %>%
    filter(DS == "supply") %>%
    group_by(scenario, region, year) %>%
    mutate(supplyshareReg = value /sum(value)) %>%
    replace_na(list(supplyshareReg = 0)) %>% ungroup() %>%
    transmute(scenario, supplysector = sector, region, year, supplyshareReg) %>%
    left_join(
      BiomassALL %>%
        group_by_at(vars(-region, -value)) %>%
        summarize(value = sum(value))  %>% ungroup() %>%
        filter(DS == "supply") %>%
        group_by(scenario,  year) %>%
        mutate(supplyshareWorld = value /sum(value)) %>%
        replace_na(list(supplyshareWorld = 0)) %>% ungroup() %>%
        transmute(scenario, supplysector = sector, year, supplyshareWorld),
      by = c("scenario", "supplysector", "year")
    ) -> BiomassSupplyShare


  # Apply sharing
  BiomassALL %>%
    filter(DS == "demand") %>%
    left_join_error_no_match(BiomassImportShare, by = c("scenario", "region", "year")) %>%
    mutate(domesticshare = 1-importshare) %>%
    left_join(BiomassSupplyShare, by = c("scenario", "region", "year")) %>%
    mutate(value = value * domesticshare * supplyshareReg + value * importshare * supplyshareWorld) %>%
    transmute(scenario, demandsector = sector, CCS, supplysector, region, year, value) ->
    BiomassALL_SupplyToDemand



  BiomassALL_SupplyToDemand %>%
    proc_scen() %>%
    mutate(demandsector = factor(demandsector,
                           levels = c("Demand: Final energy",  "Demand: Gas", "Demand: Hydrogen", "Demand: Refining", "Demand: Electricity") ),
           supplysector = factor(supplysector,
                                 levels = c("Supply: Purpose-grown","Supply: Residue", "Supply: MSW") ) )  ->
    BiomassALL_SupplyToDemand




  BiomassALL_SupplyToDemand %>%
    mutate(value = value / 1000) %>%
    Agg_reg(CCS, supplysector) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
    group_by(scenario, supplysector) %>%
    filter(year == unique(last(year))) %>% ungroup() %>%
    mutate(value = if_else(CCS == "CCS", value, -value)) %>%
    spread(CCS, value) %>%
    mutate(CCSShare = CCS / (CCS - NoCCS)) -> BiomassALL_CCSShare
  BiomassALL_CCSShare %>% left_join(
    BiomassALL_CCSShare %>% distinct(supplysector) %>% mutate(h = c(9, 4.5, 0.65))
  )-> BiomassALL_CCSShare


  BiomassALL_SupplyToDemand %>%
    mutate(value = value / 1000) %>%
    Agg_reg(supplysector, demandsector, CCS) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
    group_by(supplysector, scenario) %>%
    filter(year == unique(last(year))) %>% ungroup() %>%
    mutate(value = if_else(CCS == "CCS", value, -value)) %>%
    spread(CCS, value) %>% replace_na(list(CCS = 0)) %>%
    mutate(CCSShare = CCS / (CCS - NoCCS)) -> BiomassALL_CCSShare_Sector
  BiomassALL_CCSShare_Sector%>% left_join(
    BiomassALL_CCSShare_Sector %>% distinct(supplysector) %>% mutate(hh = c(0.6, 0.36, 0.05))
  )-> BiomassALL_CCSShare_Sector


  BiomassALL_SupplyToDemand %>%
    mutate(value = value / 1000) %>%
    Agg_reg(demandsector, supplysector, CCS) %>%
    group_by_at(vars(-year,-value)) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
    group_by(scenario) %>%
    filter(year == unique(last(year))) %>% ungroup() %>%
    proc_scen() %>%
    ggplot +   facet_grid(supplysector~LandSupply, scale = "free_y") +
    guides(fill = guide_legend(order = 1),
           linetype = guide_legend(order = 2),
           alpha = guide_legend(order = 3)) +
    geom_hline(yintercept = 0) +
    geom_bar(aes(x = LCT, y = value, fill = demandsector, alpha = CCS, linetype = CCS),
             stat = "identity", position = "stack", size = 0.5,
             color = "black") +

    geom_text(data =  BiomassALL_CCSShare %>% proc_scen(),
              aes(x = LCT, y = h, label = paste0(round(CCSShare * 100,0), "%")),
              hjust = 0.5, size = 5.5, color = "black", fontface = 4
    ) +

    geom_text(data = BiomassALL_CCSShare_Sector %>% proc_scen() %>%
                mutate(y = as.numeric(demandsector), y = (-(y - min(y) ) -1) * hh   ), # %>% distinct(supplysector, y)
              aes(x = LCT, y = y, label = paste0(round(CCSShare * 100,0), "%"), color = demandsector), size = 3.5, nudge_y = 0.01 ) +

    labs(x = "Land mitigation policy", y = "1000 EJ", alpha = "CCS technology", linetype = "CCS technology") +
    scale_alpha_manual(values = c(0.8, 1)) +
    scale_linetype_manual(values = c(1, 2)) +
    scale_color_brewer(palette = "Set1", name = "CCS tech. share"
    ) +
    scale_fill_brewer(palette = "Set1",  name = "Sector"# limits = c("Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy")
    ) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A2; A2


  A2 %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_PrimaryBiomassCCS_bySupply"), h = 4000, w = 4400,  r = 300)

  BiomassALL_SupplyToDemand %>% SaveFigData("BiomassALL_SupplyToDemand_Mapped")


}
