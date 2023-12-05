
SIFigs_LandToEMs_DeeperDecompose <- function(){

  outdir <- "output/GCAM/"
  OutFolderName <- "SI"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)

Fig_CEM_decompose_BECCS_LUC <- LoadFigData("CEM_decompose_BECCS_LUC")

BiomassALL_SupplyToDemand <- LoadFigData("BiomassALL_SupplyToDemand_Mapped")

Fig_CEM_decompose_BECCS_LUC %>%
  filter(grepl("BECCS|LULUCF", sector)) %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = F) %>% filter(year >= 2020) %>%
  mutate(GtCO2 = cumsum(value) /(year + 1 - first(year) )) %>%
  select(-value) %>%
  #summarise(GtCO2 = mean(value)) %>%
  ungroup() %>% mutate(sector = as.character(sector)) %>%
  mutate(sector = replace(sector, sector %in% c("BECCS: Purpose-grown"), "Land-based BECCS")) %>%
  mutate(sector = replace(sector, sector %in% c("BECCS: Residue & MSW"), "NonLand-based BECCS")) ->
  LandToEMs1

# ----- "LULUCF vs. Forest"
"LULUCF vs. Forest" -> LULUCF_FOR

LandALL <- LoadFigData("LandALL")
LandALL %>%
  Agg_reg(land, LCT, LandSupply) %>%
  group_by(scenario, land,LCT, LandSupply) %>%
  Fill_annual %>% filter(year >= 2019) %>% ungroup() %>%
  group_by_at(vars(-year, -value)) %>%
  mutate(value = value - first(value)) %>%  ungroup() %>%
  filter(year != 2019) %>%
  filter(!grepl("Rock|Urban", land)) %>%
  mutate(land = gsub("- Staples", "- Others", land)) %>%
  mutate(land = gsub(" - Unmanaged| - Managed", "", land)) %>%
  mutate(land = replace(land, land %in% c("Grassland", "Shrubland"), "Other natural")) %>%
  mutate(land = gsub(" - Energy", ": Energy", land)) %>%
  mutate(land = gsub(" - Others", ": NonEnergy", land)) %>%
  filter(land %in% c( "Cropland: Energy", "Forest", "Other natural")) %>%
  group_by_at(vars(-value)) %>% summarise(value = sum(value), .groups = "drop") %>%
  group_by_at(vars(-year, -value)) %>%
  mutate(value = cumsum(value) /(year + 1 - first(year) )) %>%
  #filter(year %in% c(2050, 2100)) %>%
  #summarise(value = mean(value)) %>%
  ungroup() %>% spread(land, value) %>%
  # # adding ref
  # bind_rows(LandToEMs2_REF) %>%
  transmute(year, scenario, LCT, LandSupply, `Land-based BECCS` = `Cropland: Energy`,
            # [SWITH] define LULUCF land ----
            LULUCF = Forest, # + `Other natural`,
            `NonLand-based BECCS` = 0) %>%
  gather(sector, Mha, `Land-based BECCS`:`NonLand-based BECCS`) ->
  LandToEMs2



# Bind LandToEMs ----
LandToEMs1 %>%
  left_join(LandToEMs2) %>%
  filter(year %in% c(2050, 2100)) ->
  LandToEMs

LandToEMs %>%
  filter(sector == "Land-based BECCS") %>%
  left_join_error_no_match(
    BiomassALL_SupplyToDemand %>%
      filter(supplysector == "Supply: Purpose-grown") %>%
      Agg_reg(LCT, LandSupply, CCS) %>%
      group_by_at(vars(-year,-value)) %>%
      Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
      spread(CCS, value) %>%
      mutate(sector = "Land-based BECCS") %>% mutate(CCSshare = CCS / (CCS + NoCCS)) %>%
      replace_na(list(CCSshare = 0)) %>%
      select(-CCS, -NoCCS)
  ) %>% mutate(CCS = Mha * CCSshare, NoCCS = Mha  - CCS) %>%
  select(-Mha, -CCSshare) %>%
  gather(CCS, Mha, CCS, NoCCS) %>%
  mutate(sector = paste0(sector,  CCS),
         GtCO2 = if_else(CCS == "NoCCS", 0, GtCO2)) %>%
  mutate(sector = if_else(CCS == "CCS", "Land-based (CCS) BECCS", "Land-based (NonCCS) BECCS")) %>%
  select(-CCS) %>%
  bind_rows(
    LandToEMs %>% filter(sector != "Land-based BECCS")
  ) -> LandToEMs

LandToEMs %>%
  filter(year == 2100) ->
  LandToEMs

LandToEMs -> LandToEMs_dep

LandToEMs_dep %>% SaveFigData(.NAME = paste0("Fig_LandCarbonEffciencyDepcompose_", LULUCF_FOR), .SourceForPub = T)


# LandToEMs_dep ----

LandToEMs_dep %>%
  filter(sector == "NonLand-based BECCS") %>% mutate(Arrow = 2) %>%
  bind_rows(LandToEMs_dep %>%
              filter(sector == "NonLand-based BECCS") %>% mutate(GtCO2 = 0, Arrow = 1)) %>%
  mutate(`Land CDR efficiency` = "NonLand-based BECCS") ->
  LandToEMsP1

LandToEMs_dep %>%
  filter(sector %in% c("Land-based (NonCCS) BECCS", "NonLand-based BECCS") ) %>%
  mutate(`Land CDR efficiency` = "Land-based (NonCCS) BECCS",
         sector = "Land-based (NonCCS) BECCS") %>%
  group_by_at(vars(-GtCO2, -Mha)) %>%
  summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
  mutate(Arrow = 3) %>%
  bind_rows(
    LandToEMs_dep %>%
      filter( sector == "NonLand-based BECCS") %>%
      mutate(`Land CDR efficiency` = "Land-based (NonCCS) BECCS",
             sector = "Land-based (NonCCS) BECCS",  Arrow = 2)) ->
  LandToEMsP2

LandToEMs_dep %>%
  filter(sector %in% c("NonLand-based BECCS", "Land-based (NonCCS) BECCS", "Land-based (CCS) BECCS") ) %>%
  mutate(`Land CDR efficiency` = "Land-based (CCS) BECCS",
         sector = "Land-based (CCS) BECCS") %>%
  group_by_at(vars(-GtCO2, -Mha)) %>%
  summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
  mutate(Arrow = 4) %>%
  bind_rows(
    LandToEMs_dep %>%
      filter(sector %in% c("Land-based (NonCCS) BECCS", "NonLand-based BECCS") ) %>%
      mutate(`Land CDR efficiency` = "Land-based (CCS) BECCS",
             sector = "Land-based (CCS) BECCS") %>%
      group_by_at(vars(-GtCO2, -Mha)) %>%
      summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
      mutate(Arrow = 3) ) ->
  LandToEMsP2p5

LandToEMs_dep %>%
  filter(sector %in% c("NonLand-based BECCS", "Land-based (NonCCS) BECCS", "Land-based (CCS) BECCS") ) %>%
  mutate(`Land CDR efficiency` = "LULUCF vs. A/R",
         sector = "LULUCF vs. A/R") %>%
  group_by_at(vars(-GtCO2, -Mha)) %>%
  summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
  mutate(Arrow = 4) %>%
  bind_rows(
    LandToEMs_dep %>%
      mutate(`Land CDR efficiency` = "LULUCF vs. A/R",
             sector = "LULUCF vs. A/R") %>%
      group_by_at(vars(-GtCO2, -Mha)) %>%
      summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
      mutate(Arrow = 5)
  ) -> LandToEMsP3


LandToEMsP1 %>%
  bind_rows(LandToEMsP2) %>%
  bind_rows(LandToEMsP2p5) %>%
  bind_rows(LandToEMsP3) %>%
  mutate(`Land CDR efficiency` =
           factor(`Land CDR efficiency`,
                  levels = c( "NonLand-based BECCS", "Land-based (NonCCS) BECCS", "Land-based (CCS) BECCS",  "LULUCF vs. A/R"),
                  labels = c("NonLand-based BECCS", "Land-based (NonCCS) BECCS", "Land-based (CCS) BECCS", LULUCF_FOR)))->
  LandToEMs_decompose



ggplot() +
  guides(colour = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) +
  facet_grid(LandSupply~LCT) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = 2, color = "grey50") +
  geom_hline(yintercept = -1, linetype = 2, color = "grey50") +
  geom_path(data = LandToEMs_decompose %>% arrange(`Land CDR efficiency`, Arrow) %>%
              filter(LCT != "Reference") %>%
              mutate(LCT = factor(LCT, levels = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP", "Reference"))),
            aes(x = Mha, y = GtCO2, linetype = `Land CDR efficiency`,
                color = LCT ), size = 1.5,
            arrow = arrow(length = unit(0.2, "cm")))  +
  geom_point(data = LandToEMs_decompose %>% filter(LCT != "Reference"),
             aes(x = Mha, y = GtCO2), size = 2.5, shape = 21, fill = "red") +

  geom_abline(intercept = 0, slope = -10/ 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -15/ 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -20 / 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -25/ 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -30/ 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -35/ 1000, linetype = 2, color = "grey80") +

  geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
            aes(y = -7.5, x = 820, label = "`-10tCO`[2]/ha/yr"), parse = TRUE,
            angle = -40, #atan(-35)/pi * 180 ,
            hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +

  geom_text(data = LandToEMsTotal ,
            aes(y = -9.8, x = 260, label = "-35"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
  geom_text(data = LandToEMsTotal ,
            aes(y = -9.8, x = 400, label = "-25"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
  geom_text(data = LandToEMsTotal ,
            aes(y = -9.8, x = 680, label = "-15"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +


  geom_text(data = LandToEMs_decompose %>% filter(LCT != "Reference") %>%
              filter(`Land CDR efficiency` == "Land-based (CCS) BECCS") %>%
              group_by(LCT, LandSupply) %>%
              summarise(EFF = round((GtCO2 - first(GtCO2)) /(Mha-first(Mha)) * 1000, 1),
                        Mha = mean(Mha), GtCO2 = mean(GtCO2) ) %>% filter(!is.na(EFF)) %>% ungroup() %>%
              mutate(Mha = pmax(0.8* Mha, 100)),
            aes(y = GtCO2 -1.5, x = Mha, label = EFF),  hjust = 0.5, size = 4.5, color = "orange", fontface = 4 ) +

  geom_text(data = LandToEMs_decompose %>% filter(LCT != "Reference") %>%
              filter(`Land CDR efficiency` == LULUCF_FOR) %>%
              group_by(LCT, LandSupply) %>%
              summarise(EFF = round((GtCO2 - first(GtCO2)) /(Mha-first(Mha)) * 1000, 1),
                        Mha = mean(Mha), GtCO2 = mean(GtCO2) ) %>% filter(!is.na(EFF)) %>% ungroup() %>%
              mutate(Mha = pmax(1.2 * Mha, 100)),
            aes(y = GtCO2, x = Mha + 80, label = EFF),  hjust = 0.5, size = 4.5, color = "green", fontface = 4 ) +


  scale_x_continuous(expand = c(0, 0), limits = c(-5, 1100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-10.5, 0.5)) +
  labs(x = expression(paste(Mha)), y = expression(paste(GtCO[2], " ", yr^-1))) +
  scale_color_npg(name = "Scenario", limits = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP")) +
  scale_linetype_manual(values = c(3, 2, 4, 1), name = "Land CDR efficiency") +
  theme_bw() + theme0 +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        #strip.background = element_rect(colour = "black", fill = NA),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(.5, "lines")) + theme_leg -> A3;A3

A3 %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_LandToEMs_All_bind_Mit_UpdatedV2", LULUCF_FOR), h = 3800, w = 4400,  r = 300)

# ----- LULUCF vs. Forest & Natural ------
"LULUCF vs. Forest & Natural" -> LULUCF_FOR

LandALL <- LoadFigData("LandALL")
LandALL %>%
  Agg_reg(land, LCT, LandSupply) %>%
  group_by(scenario, land,LCT, LandSupply) %>%
  Fill_annual %>% filter(year >= 2019) %>% ungroup() %>%
  group_by_at(vars(-year, -value)) %>%
  mutate(value = value - first(value)) %>%  ungroup() %>%
  filter(year != 2019) %>%
  filter(!grepl("Rock|Urban", land)) %>%
  mutate(land = gsub("- Staples", "- Others", land)) %>%
  mutate(land = gsub(" - Unmanaged| - Managed", "", land)) %>%
  mutate(land = replace(land, land %in% c("Grassland", "Shrubland"), "Other natural")) %>%
  mutate(land = gsub(" - Energy", ": Energy", land)) %>%
  mutate(land = gsub(" - Others", ": NonEnergy", land)) %>%
  filter(land %in% c( "Cropland: Energy", "Forest", "Other natural")) %>%
  group_by_at(vars(-value)) %>% summarise(value = sum(value), .groups = "drop") %>%
  group_by_at(vars(-year, -value)) %>%
  mutate(value = cumsum(value) /(year + 1 - first(year) )) %>%
  #filter(year %in% c(2050, 2100)) %>%
  #summarise(value = mean(value)) %>%
  ungroup() %>% spread(land, value) %>%
  # # adding ref
  # bind_rows(LandToEMs2_REF) %>%
  transmute(year, scenario, LCT, LandSupply, `Land-based BECCS` = `Cropland: Energy`,
            # [SWITH] define LULUCF land ----
            LULUCF = Forest + `Other natural`,
            `NonLand-based BECCS` = 0) %>%
  gather(sector, Mha, `Land-based BECCS`:`NonLand-based BECCS`) ->
  LandToEMs2


# Bind LandToEMs ----
LandToEMs1 %>%
  left_join(LandToEMs2) %>%
  filter(year %in% c(2050, 2100)) ->
  LandToEMs

LandToEMs %>%
  filter(sector == "Land-based BECCS") %>%
  left_join_error_no_match(
    BiomassALL_SupplyToDemand %>%
      filter(supplysector == "Supply: Purpose-grown") %>%
      Agg_reg(LCT, LandSupply, CCS) %>%
      group_by_at(vars(-year,-value)) %>%
      Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
      spread(CCS, value) %>%
      mutate(sector = "Land-based BECCS") %>% mutate(CCSshare = CCS / (CCS + NoCCS)) %>%
      replace_na(list(CCSshare = 0)) %>%
      select(-CCS, -NoCCS)
  ) %>% mutate(CCS = Mha * CCSshare, NoCCS = Mha  - CCS) %>%
  select(-Mha, -CCSshare) %>%
  gather(CCS, Mha, CCS, NoCCS) %>%
  mutate(sector = paste0(sector,  CCS),
         GtCO2 = if_else(CCS == "NoCCS", 0, GtCO2)) %>%
  mutate(sector = if_else(CCS == "CCS", "Land-based (CCS) BECCS", "Land-based (NonCCS) BECCS")) %>%
  select(-CCS) %>%
  bind_rows(
    LandToEMs %>% filter(sector != "Land-based BECCS")
  ) -> LandToEMs

LandToEMs %>%
  filter(year == 2100) ->
  LandToEMs

LandToEMs -> LandToEMs_dep

LandToEMs_dep %>%
  transmute(Policy_Scenario = LandSupply, LCP_scenario = LCT, sector,
            Cum_Land_Mha = Mha, Cum_CDR_GtCO2PerYr = GtCO2) %>%
  SaveFigData(.NAME = paste0("Fig_LandCarbonEffciencyDepcompose_", LULUCF_FOR), .SourceForPub = T)

# LandToEMs_dep ----

LandToEMs_dep %>%
  filter(sector == "NonLand-based BECCS") %>% mutate(Arrow = 2) %>%
  bind_rows(LandToEMs_dep %>%
              filter(sector == "NonLand-based BECCS") %>% mutate(GtCO2 = 0, Arrow = 1)) %>%
  mutate(`Land CDR efficiency` = "NonLand-based BECCS") ->
  LandToEMsP1

LandToEMs_dep %>%
  filter(sector %in% c("Land-based (NonCCS) BECCS", "NonLand-based BECCS") ) %>%
  mutate(`Land CDR efficiency` = "Land-based (NonCCS) BECCS",
         sector = "Land-based (NonCCS) BECCS") %>%
  group_by_at(vars(-GtCO2, -Mha)) %>%
  summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
  mutate(Arrow = 3) %>%
  bind_rows(
    LandToEMs_dep %>%
      filter( sector == "NonLand-based BECCS") %>%
      mutate(`Land CDR efficiency` = "Land-based (NonCCS) BECCS",
             sector = "Land-based (NonCCS) BECCS",  Arrow = 2)) ->
  LandToEMsP2

LandToEMs_dep %>%
  filter(sector %in% c("NonLand-based BECCS", "Land-based (NonCCS) BECCS", "Land-based (CCS) BECCS") ) %>%
  mutate(`Land CDR efficiency` = "Land-based (CCS) BECCS",
         sector = "Land-based (CCS) BECCS") %>%
  group_by_at(vars(-GtCO2, -Mha)) %>%
  summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
  mutate(Arrow = 4) %>%
  bind_rows(
    LandToEMs_dep %>%
      filter(sector %in% c("Land-based (NonCCS) BECCS", "NonLand-based BECCS") ) %>%
      mutate(`Land CDR efficiency` = "Land-based (CCS) BECCS",
             sector = "Land-based (CCS) BECCS") %>%
      group_by_at(vars(-GtCO2, -Mha)) %>%
      summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
      mutate(Arrow = 3) ) ->
  LandToEMsP2p5

LandToEMs_dep %>%
  filter(sector %in% c("NonLand-based BECCS", "Land-based (NonCCS) BECCS", "Land-based (CCS) BECCS") ) %>%
  mutate(`Land CDR efficiency` = "LULUCF vs. A/R",
         sector = "LULUCF vs. A/R") %>%
  group_by_at(vars(-GtCO2, -Mha)) %>%
  summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
  mutate(Arrow = 4) %>%
  bind_rows(
    LandToEMs_dep %>%
      mutate(`Land CDR efficiency` = "LULUCF vs. A/R",
             sector = "LULUCF vs. A/R") %>%
      group_by_at(vars(-GtCO2, -Mha)) %>%
      summarise_at(vars(GtCO2, Mha), sum) %>% ungroup() %>%
      mutate(Arrow = 5)
  ) -> LandToEMsP3


LandToEMsP1 %>%
  bind_rows(LandToEMsP2) %>%
  bind_rows(LandToEMsP2p5) %>%
  bind_rows(LandToEMsP3) %>%
  mutate(`Land CDR efficiency` =
           factor(`Land CDR efficiency`,
                  levels = c( "NonLand-based BECCS", "Land-based (NonCCS) BECCS", "Land-based (CCS) BECCS",  "LULUCF vs. A/R"),
                  labels = c("NonLand-based BECCS", "Land-based (NonCCS) BECCS", "Land-based (CCS) BECCS", LULUCF_FOR)))->
  LandToEMs_decompose



ggplot() +
  guides(colour = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) +
  facet_grid(LandSupply~LCT) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = 2, color = "grey50") +
  geom_hline(yintercept = -1, linetype = 2, color = "grey50") +
  geom_path(data = LandToEMs_decompose %>% arrange(`Land CDR efficiency`, Arrow) %>%
              filter(LCT != "Reference") %>%
              mutate(LCT = factor(LCT, levels = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP", "Reference"))),
            aes(x = Mha, y = GtCO2, linetype = `Land CDR efficiency`,
                color = LCT ), size = 1.5,
            arrow = arrow(length = unit(0.2, "cm")))  +
  geom_point(data = LandToEMs_decompose %>% filter(LCT != "Reference"),
             aes(x = Mha, y = GtCO2), size = 2.5, shape = 21, fill = "red") +

  geom_abline(intercept = 0, slope = -10/ 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -15/ 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -20 / 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -25/ 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -30/ 1000, linetype = 2, color = "grey80") +
  geom_abline(intercept = 0, slope = -35/ 1000, linetype = 2, color = "grey80") +

  geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
            aes(y = -7.5, x = 820, label = "`-10tCO`[2]/ha/yr"), parse = TRUE,
            angle = -40, #atan(-35)/pi * 180 ,
            hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +

  geom_text(data = LandToEMsTotal ,
            aes(y = -9.8, x = 260, label = "-35"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
  geom_text(data = LandToEMsTotal ,
            aes(y = -9.8, x = 400, label = "-25"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
  geom_text(data = LandToEMsTotal ,
            aes(y = -9.8, x = 680, label = "-15"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +


  geom_text(data = LandToEMs_decompose %>% filter(LCT != "Reference") %>%
              filter(`Land CDR efficiency` == "Land-based (CCS) BECCS") %>%
              group_by(LCT, LandSupply) %>%
              summarise(EFF = round((GtCO2 - first(GtCO2)) /(Mha-first(Mha)) * 1000, 1),
                        Mha = mean(Mha), GtCO2 = mean(GtCO2) ) %>% filter(!is.na(EFF)) %>% ungroup() %>%
              mutate(Mha = pmax(0.8* Mha, 100)),
            aes(y = GtCO2 -1.5, x = Mha, label = EFF),  hjust = 0.5, size = 4.5, color = "orange", fontface = 4 ) +

  geom_text(data = LandToEMs_decompose %>% filter(LCT != "Reference") %>%
              filter(`Land CDR efficiency` == LULUCF_FOR) %>%
              group_by(LCT, LandSupply) %>%
              summarise(EFF = round((GtCO2 - first(GtCO2)) /(Mha-first(Mha)) * 1000, 1),
                        Mha = mean(Mha), GtCO2 = mean(GtCO2) ) %>% filter(!is.na(EFF)) %>% ungroup() %>%
              mutate(Mha = pmax(1.2 * Mha, 100)),
            aes(y = GtCO2, x = Mha + 80, label = EFF),  hjust = 0.5, size = 4.5, color = "green", fontface = 4 ) +


  scale_x_continuous(expand = c(0, 0), limits = c(-5, 1100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-10.5, 0.5)) +
  labs(x = expression(paste(Mha)), y = expression(paste(GtCO[2], " ", yr^-1))) +
  scale_color_npg(name = "Scenario", limits = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP")) +
  scale_linetype_manual(values = c(3, 2, 4, 1), name = "Land CDR efficiency") +
  theme_bw() + theme0 +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        #strip.background = element_rect(colour = "black", fill = NA),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(.5, "lines")) + theme_leg -> A3;A3

A3 %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_LandToEMs_All_bind_Mit_UpdatedV2", LULUCF_FOR), h = 3800, w = 4400,  r = 300)


}

