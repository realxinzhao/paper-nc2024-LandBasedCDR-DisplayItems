

Load_AR6_vetted <- function(FromCache = T, QuickVariableFromSource = NULL){

  if (is.character(QuickVariableFromSource) ) {
    FromCache = F
  }

  if (FromCache == T) {
    readRDS("data/AR6/RDS_temp/AR6_vettedC1_4.rds")

  } else {

    # Load C1-C8 category from meta
    MS_Category <- readxl::read_excel("data/AR6/1668008312256-AR6_Scenarios_Database_World_v1.1.csv/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx",
                                      sheet = 2) %>%
      select(Model, Scenario, Category, Policy_category, Project_study)


    # Load AR6 source data
    # https://data.ene.iiasa.ac.at/ar6/#/workspaces
    AR6 <- readr::read_csv("data/AR6/1668008312256-AR6_Scenarios_Database_World_v1.1.csv/AR6_Scenarios_Database_World_v1.1.CSV")

    # Check variable
    # AR6 %>% distinct(Variable) %>%
    #   filter(grepl("Land Use", Variable, ignore.case = T) == T)

    if ( is.character(QuickVariableFromSource) ) {

      assertthat::assert_that( AR6 %>% filter(Variable %in% QuickVariableFromSource) %>% nrow >0)

      c("Emissions|CO2",
        "Emissions|CO2|Energy and Industrial Processes",
        "Emissions|CO2|AFOLU",
        "Carbon Sequestration|CCS|Biomass",
        QuickVariableFromSource) %>% unique ->
        QuickVariableFromSource1

      AR6 %>% filter(Variable %in% QuickVariableFromSource1) -> AR6

    }

    # Gather, clean, fill, and join
    AR6 %>%
      mutate(Pathway = interaction(Model, Scenario)) %>%
      filter(Pathway %in% c(MS_Category %>% distinct(interaction(Model, Scenario)) %>% pull) ) %>%
      left_join(MS_Category, by = c("Model", "Scenario")) %>%
      gcamdata::gather_years() %>%
      group_by(Pathway, Variable, Scenario, Model, Region, Unit) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year >= 2020) ->
      AR6_vetted

    # saveRDS(AR6_vetted, "data/AR6/RDS_temp/AR6_vetted.rds")
    # readRDS("data/AR6/RDS_temp/AR6_vetted.rds") -> AR6_vetted

    # 1202 and 48 GCAM
    # AR6_vetted %>% distinct(Pathway)
    # AR6_vetted %>% distinct(Pathway, Model) %>%
    #  filter(grepl("GCAM", Model, ignore.case = T) == T)

    # Check variable
    # AR6_vetted %>% distinct(Variable) %>%
    #   filter(grepl("Land Use", Variable, ignore.case = T) == T)

    # "Carbon Sequestration|Land Use|Other" is not in vetted scenarios

    # AR6_vetted %>% distinct(Variable, Unit) %>%
    #     filter(grepl("Discount rate", Variable, ignore.case = T) == T)

    # Load Variable mapping
    VariableMapping <- readr::read_csv("data/AR6/VariableMapping.csv", comment = "#") %>%
      filter(!is.na(Var)) %>% select(-Unit)

    AR6_vetted %>% filter(Category %in% paste0("C", 1:4)) %>%
      right_join(VariableMapping, by = "Variable") ->
      AR6_vettedC1_4
    AR6_vettedC1_4 %>% distinct(Pathway) #700



    AR6_vettedC1_4 %>%
      filter(Var %in% c("EM_CO2", "EM_CO2_EIP", "EM_CO2_AFOLU", "BECCS")) %>%
      mutate(Var = factor(Var, levels = c("EM_CO2", "EM_CO2_EIP",  "EM_CO2_AFOLU", "BECCS"),
                          labels = c("CB", "FFI", "AFOLU", "BECCS"))) %>%
      select(-Unit, -Variable, -Element) %>%
      spread(Var, value) %>%  filter(year == 2100) %>%
      filter(is.na(BECCS) ) %>% distinct(Pathway) %>% pull ->
      Pathway_BECCS_NotReported
    # 30 GEM-E3_V2021 and 4 EPPA pathways are excluded as BECCS not reported
    # https://iopscience.iop.org/article/10.1088/1748-9326/ac09ae
    # EPPA has BECCS
    # https://www.worldscientific.com/doi/abs/10.1142/S2010007821500019
    # Rm these

    # Add an EPPA pathway back as we got BECCS info from Fajardy et al. 2021 GEC
    Pathway_BECCS_NotReported[Pathway_BECCS_NotReported != "EPPA 6.Paris2C_OptTax"] %>%
      as.character() ->
      Pathway_BECCS_NotReported

    AR6_vettedC1_4 %>% filter(!Pathway %in% Pathway_BECCS_NotReported) ->
      AR6_vettedC1_4


    # Fill in AFOLU and EIP missing based on calculation
    AR6_vettedC1_4 %>%
      filter(Var %in% c("EM_CO2", "EM_CO2_EIP", "EM_CO2_AFOLU", "BECCS")) %>%
      select(-Unit, -Variable, -Element) %>%
      spread(Var, value) %>%
      # Fill in EPPA BECCS!!! based on Fajardy et al. 2021 GEC
      mutate(BECCS = if_else(Pathway == "EPPA 6.Paris2C_OptTax" & year == 2020, 0, BECCS),
             BECCS = if_else(Pathway == "EPPA 6.Paris2C_OptTax" & year == 2049, 0, BECCS),
             BECCS = if_else(Pathway == "EPPA 6.Paris2C_OptTax" & year == 2050, 3.3137 * 1000, BECCS),
             BECCS = if_else(Pathway == "EPPA 6.Paris2C_OptTax" & year == 2100, 21 * 1000, BECCS)) %>%
      group_by(Pathway, Scenario, Model, Region) %>%
      mutate(BECCS = if_else(Pathway == "EPPA 6.Paris2C_OptTax", approx_fun(year, BECCS, rule = 2), BECCS)) %>%
      ungroup() %>%
      mutate(EM_CO2_AFOLU = if_else(is.na(EM_CO2_AFOLU), EM_CO2 - EM_CO2_EIP, EM_CO2_AFOLU),
             EM_CO2_EIP = if_else(is.na(EM_CO2_EIP), EM_CO2 - EM_CO2_AFOLU, EM_CO2_EIP)) %>%
      gather(Var, value, c("EM_CO2", "EM_CO2_EIP", "EM_CO2_AFOLU", "BECCS")) %>%
      left_join(AR6_vettedC1_4 %>% distinct(Var, Unit, Variable, Element), by = "Var") %>%
      bind_rows(AR6_vettedC1_4 %>%
                  filter(!Var %in% c("EM_CO2", "EM_CO2_EIP", "EM_CO2_AFOLU", "BECCS"))) ->
      AR6_vettedC1_4


    #saveRDS(AR6_vettedC1_4, "data/AR6/RDS_temp/AR6_vettedC1_4.rds")

    # Create element
    # AR6_vetttedC1_4 %>% distinct(Variable, Unit) %>%
    #   mutate(Element = head(strsplit(Variable, "\\|")[[1]][[1]]), n = 1))

    return(AR6_vettedC1_4)
  }
}


TCRP <- function(OUTDIR = "output/AR6/", CB_only = T){

  outdir <- OUTDIR

  ##Historical emissions in Fig.SPM10 ----
  WPI_SPM10 <- readr::read_csv("data/AR6/IPCC_AR6/WPI_SPM10/Top_panel_HISTORY.CSV", comment = "#") %>%
    gather_years() %>% select(-unit) %>% spread(variable, value)

  AR6_vettedC1_4 %>%
    filter(Var %in% c("Temp", "EM_CO2")) %>%
    select(-Unit, -Variable, -Element) %>%
    spread(Var, value) %>%
    group_by_at(vars(-EM_CO2, -Temp, -year)) %>%
    mutate(EM_CO2 = cumsum(EM_CO2)/ 1000) %>% ungroup() %>%
    group_by(Pathway) %>% mutate(CEM2100 = EM_CO2[year == 2100]) %>%
    mutate(CEMPeak = max(EM_CO2), PeakYear = first(year[CEMPeak == EM_CO2]) ) %>%
    ungroup() -> TCRP

  TCRP %>% filter(year == 2100) %>%
    filter(EM_CO2 <175) %>% nrow

  TCRP %>% filter(year == 2100) %>%
    filter(EM_CO2 >1475) %>% nrow

  if (CB_only == T) {
    # narrowed ranges
    TCRP %>% filter(year == 2100) %>%
      filter( EM_CO2 >= 175  & EM_CO2 <= 1475) %>%
      mutate(Pathway = as.character(Pathway)) %>%
      distinct(Pathway) %>% pull->
      Pathway175_1475

    return(Pathway175_1475)
  }


  TCRP %>%
    #filter(year <= PeakYear) %>%
    mutate(EM_CO2 = EM_CO2 + 2410) %>%
    ggplot() +
    geom_vline(xintercept = 2410 + 0, color = "black", linetype = 1) +
    geom_vline(xintercept = 2410 + 500, color = "red", linetype = 5) +
    geom_vline(xintercept = 2410 + 1150, color = "red", linetype = 5) +

    geom_vline(xintercept = 2410 + 175, color = "blue", linetype = 2) +
    geom_vline(xintercept = 2410 + 1475, color = "blue", linetype = 2) +

    # geom_path(data = WPI_SPM10, aes(x = `Cumulative CO2 emissions since 1850`,
    #                                 y = `Human-caused warming (central estimate)`)) +
    geom_path(aes(x = EM_CO2, y = Temp, group = Pathway,
                  color = Model), alpha = 0.5) +
    geom_point(data =  TCRP %>% mutate(EM_CO2 = EM_CO2 + 2410) %>% filter(year == 2100),
               aes(x = EM_CO2, y = Temp), size = 0.5
    ) +
    labs(x = expression(paste("Cumulative ", CO[2], " emissions since 1850 (", GtCO[2],")")),
         y = expression("°C") ) +
    theme_bw() + theme0 -> p;p

  Write_png(.plot = p + theme(legend.position = "none"),
            name = "TCRP1", w = 2500, h = 2000, r = 300)

  Write_png(.plot = p + facet_wrap(~Category) ,
            name = "TCRP2", w = 4700, h = 3000, r = 300)

  # narrowed ranges
  TCRP %>% filter(year == 2100) %>%
    filter( EM_CO2 >= 175  & EM_CO2 <= 1475) %>%
    mutate(Pathway = as.character(Pathway)) %>%
    distinct(Pathway) %>% pull->
    Pathway175_1475

  return(Pathway175_1475)
}



# Generate BioEnergy_ModernBioCCSResidue figures ----
BioEnergyCCSResidue <- function(INPUT_DF, OUTDIR = "output/AR6/"){

  # Bioenergy CCS and residue rates ----

  outdir <- OUTDIR

  c("Biomass Modern CCS",
    "Biomass Modern noCCS",
    "PE_Biomass",
    "Biomass Traditional",
    "Biomass 1G",
    "Biomass Modern",
    "Biomass Residues",
    "Biomass Solids") -> VarBioEnergy

  INPUT_DF %>%
    filter(Var %in% VarBioEnergy) %>%
    select(-Unit, -Element, -Variable) %>%
    spread(Var, value) %>%
    filter(!is.na(`Biomass Modern noCCS`)) %>%
    group_by(Pathway) %>%
    filter(min(`Biomass Modern noCCS`) > 0) %>% ungroup() %>%
    filter(year %in% seq(2020, 2100, 5)) %>%
    mutate(ModernBiomassCCSRate = `Biomass Modern CCS`/(`Biomass Modern noCCS` + `Biomass Modern CCS`)) %>%
    mutate(ModernBiomassResidueRate = `Biomass Residues`/(`Biomass Modern noCCS` + `Biomass Modern CCS`)) %>%
    mutate(`Modern biomass noResidue` = `Biomass Modern noCCS` + `Biomass Modern CCS` -  `Biomass Residues`) ->
    Biomass

  Biomass %>% gather(Var, value, `Biomass 1G`:`Modern biomass noResidue`) %>%
    filter(year == 2100, !is.na(value))%>%
    group_by(Var) %>%
    summarise(n = n(), .groups = "drop") -> A; A
  A %>% write.csv(file.path(outdir, "BioEnergy_ModernBioCCSResidueSTAT.csv"))

  Biomass %>%
    filter(!is.na(`Biomass Modern CCS`), !is.na(`Biomass Modern noCCS`)) %>%
    ggplot() + facet_wrap(~Category) +
    geom_abline(slope = 1) +
    scale_x_continuous(limits = c(0, 550)) +
    scale_y_continuous(limits = c(0, 550)) +
    geom_path(aes(x = `Biomass Modern CCS`,
                  y = `Biomass Modern noCCS`,
                  group = Pathway, color = Model))+
    labs(x = "Modern biomass, w/ CCS (EJ/year)", y = "Modern biomass, w/o CCS (EJ/year)") +
    theme_bw() + theme0 -> p;p

  p %>% Write_png("BioEnergy_ModernBioCCSResidue1", w = 4700, h = 3000, r = 300)

  Biomass %>%
    filter(!is.na(`Biomass Residues`), !is.na(`Modern biomass noResidue`)) %>%
    ggplot() + facet_wrap(~Category) +
    geom_abline(slope = 1) +
    scale_x_continuous(limits = c(0, 250)) +
    scale_y_continuous(limits = c(0, 250)) +
    geom_path(aes(x = `Modern biomass noResidue`,
                  y = `Biomass Residues`,
                  group = Pathway, color = Model))+
    labs(x = "Modern biomass, non-residue (EJ/year)", y = "Modern biomass, residue (EJ/year)") +
    theme_bw() + theme0 -> p;p
  p %>% Write_png("BioEnergy_ModernBioCCSResidue2", w = 4200, h = 3000, r = 300)

  Biomass %>%
    filter(!is.na(ModernBiomassCCSRate)) %>%
    ggplot()+ facet_wrap(~Category) +
    geom_line(aes(x = year, y = ModernBiomassCCSRate, group = Pathway, color = Model)) +
    labs(x = "Year", y = "CCS Share in modern biomass") +
    theme_bw() + theme0 -> p;p
  p %>% Write_png("BioEnergy_ModernBioCCSResidue3", w = 4700, h = 3000, r = 300)

  Biomass %>%
    filter(!is.na(ModernBiomassResidueRate)) %>%
    ggplot()+ facet_wrap(~Category) +
    geom_line(aes(x = year, y = ModernBiomassResidueRate, group = Pathway, color = Model)) +
    labs(x = "Year", y = "Residue share in modern biomass") +
    theme_bw() + theme0 -> p;p
  p %>% Write_png("BioEnergy_ModernBioCCSResidue4", w = 4200, h = 3000, r = 300)

  Biomass %>% filter(year %in% c(2020, 2050, 2100)) %>%
    filter(!is.na(ModernBiomassCCSRate), !is.na(ModernBiomassResidueRate)) %>%
    ggplot()+ facet_wrap(~Category) +
    geom_path(aes(x = ModernBiomassCCSRate, y = ModernBiomassResidueRate, group = Pathway, color = Model)) +
    labs(x = "CCS share in modern biomass", y = "Residue share in modern biomass") +
    theme_bw() + theme0 -> p;p
  p %>% Write_png("BioEnergy_ModernBioCCSResidue5", w = 4200, h = 3000, r = 300)

}









