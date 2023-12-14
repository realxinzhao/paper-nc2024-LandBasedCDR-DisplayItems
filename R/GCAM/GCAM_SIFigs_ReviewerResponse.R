

outdir <- "output/GCAM/"
OutFolderName <- "SI"
dir.create(file.path(outdir, OutFolderName), showWarnings = F)

LoadFigData("Enbioresidue") -> Enbioresidue

Enbioresidue %>% filter(year >= 2020) %>%
  mutate(sector = if_else(grepl("Forest", sector), "Forestry", "Crop")) %>%
  Agg_reg(sector, source = "Residue", target, SDR) %>%
  group_by_at(vars(-year,-value)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  filter(year == 2100) %>%
  spread(sector, value) %>%
  mutate(ForShare = Forestry / (Forestry + Crop)) %>% proc_scen()-> A


Enbioresidue %>% filter(year >= 2020) %>%
  mutate(sector = if_else(grepl("Forest", sector), "Forestry", "Crop")) %>%
  Agg_reg(sector, source = "Residue", target, SDR) %>%
  group_by_at(vars(-year,-value)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  filter(year == 2100) %>% mutate(value = value /1000) %>% proc_scen() %>%
  ggplot +   facet_grid( ~LandSupply) +
  guides(fill = guide_legend(order = 1),
         linetype = guide_legend(order = 2),
         alpha = guide_legend(order = 3)) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = LCT, y = value, fill = sector),
           stat = "identity", position = "stack", size = 0.5,
           color = "black") +

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

A2 %>% Write_png(paste0(OutFolderName, "/SIFig_GCAM_PrimaryBiomass_Residue"), h = 2000, w = 4400,  r = 300)



LoadFigData("LandALL")  -> Land1

Land1 %>% Agg_reg(land) %>%
  proc_scen() %>%  filter(year >= 2020) %>%
  group_by_at(vars(-year, -value)) %>%
  mutate(value = value - first(value)) %>% ungroup() -> DF_LUC

DF_LUC %>% group_by_at(vars(-value, -year)) %>%
  Fill_annual() %>%
  summarise(value = mean(value) ) %>%
  mutate(year = "Mean") %>%
  bind_rows(DF_LUC %>% mutate(year = as.character(year))) ->
  DF_LUC1

seq(2020, 2100,5) -> x
x[x %in% c(seq(2020, 2100,5) %>% setdiff(seq(2020, 2100,20)))] <- ""

DF_LUC1 %>%
  filter(grepl("Forest", land)) %>%
  ggplot +   facet_grid( LandSupply ~ LCT) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 250, linetype = 2, color = "grey") +
  geom_hline(yintercept = -250, linetype = 2, color = "grey") +
  geom_hline(yintercept = 500, linetype = 2, color = "grey") +
  geom_hline(yintercept = -500, linetype = 2, color = "grey") +
  geom_hline(yintercept = 750, linetype = 2, color = "grey") +
  #geom_hline(yintercept = -750, linetype = 2, color = "grey") +
  # geom_hline(yintercept = 1000, linetype = 2, color = "grey") +
  # geom_hline(yintercept = -1000, linetype = 2, color = "grey") +
  geom_bar(aes(x = year, y = value, fill = land), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Mha", fill = "Land") +
  scale_x_discrete( labels = c(x, "Mean")) +
  #scale_fill_brewer(palette = "Spectral", name = "Land", direction = -1) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90), legend.text.align = 0,
        strip.background = element_rect(fill="grey99"),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A2; A2

A2 %>% Write_png(paste0(OutFolderName, "/SIFig_GCAM_LULUC_Forest"), h = 3200, w = 4900,  r = 300)




NonCO2Check <- LoadFigData("NonCO2Check")

NonCO2Check %>% filter(sector != "Other GHGs_UnMGMTLand") %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  group_by(scenario) %>%
  filter(year == unique(last(year))) %>% ungroup() %>%
  proc_scen() %>% mutate(year = as.integer(year)) %>%
  select(scenario, sector, year, value) %>%
  proc_scen() %>% filter(grepl("UnMGMT", sector)) -> df

df %>% ggplot +   facet_grid( ~LandSupply, scales = "free_x") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = LCT, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +

  geom_text(data =  df %>%
              group_by(scenario) %>% summarise(value = round(sum(value),0)) %>%
              proc_scen() ,
            aes(x = LCT, y = 40, label = value),
            hjust = 0.5, size = 6, color = "blue", fontface = 4
  ) +
  labs(x = "Scenario", y = expression(paste(GtCO[2]-equivalent))) +
  scale_fill_brewer(palette = "Set3", name = "Source", direction = -1,
                    labels = c(expression(paste(CH[4], " Unmanaged Land")),
                               expression(paste(N[2], "O Unmanaged Land"))) ) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A2; A2
A2 %>% Write_png(paste0(OutFolderName, "/SIFig_GCAM_NonCO2_cumulative_unmanaged"), h = 2000, w = 4900,  r = 300)

