
BiomassALL %>%
  filter(DS == "demand") %>%
  mutate(value = value / 1000) %>%
  Agg_reg(CCS, sector) %>%
  group_by_at(vars(-year,-value)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  #left_join(NZ_PeakYear, by = "scenario") %>%
  #filter(!(!is.na(NZyear) & year > NZyear)) %>% select(-NZyear) %>%
  group_by(scenario) %>%
  filter(year == unique(last(year))) %>% ungroup() %>%
  mutate(value = if_else(CCS == "CCS", value, -value)) %>%
  proc_scen()  -> data

data %>%
  ggplot +   facet_grid( ~LandSupply) +
  guides(fill = guide_legend(order = 1),
         alpha = guide_legend(order = 2)) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = LCT, y = value, fill = sector, alpha = CCS),
           stat = "identity", position = "stack",
           color = "black") +

  geom_text(data =  BiomassALL_CCSShare %>% proc_scen(),
            aes(x = LCT, y = 13, label = paste0(round(CCSShare * 100,0), "%")),
            hjust = 0.5, size = 5.5, color = "black", fontface = 4
  ) +

  labs(x = "Land mitigation policy", y = "1000 EJ", alpha = "CCS technology") +
  scale_alpha_manual(values = c(1, 0.5)) +
  # scale_fill_brewer(palette = "RdYlGn",
  #                   name = "Demand sector", direction = 1) +
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

A2 %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_PrimaryBiomassCCS_PanelA_test0"), h = 2000, w = 4400,  r = 300)

data %>%
    ggplot +   facet_grid( ~LandSupply) +
  guides(fill = guide_legend(order = 1),
         linetype = guide_legend(order = 2),
         alpha = guide_legend(order = 3)) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = LCT, y = value, fill = sector, linetype = CCS, alpha = CCS),
           stat = "identity", position = "stack",
           color = "black") +

  geom_text(data =  BiomassALL_CCSShare %>% proc_scen(),
            aes(x = LCT, y = 13, label = paste0(round(CCSShare * 100,0), "%")),
            hjust = 0.5, size = 5.5, color = "black", fontface = 4
  ) +

  labs(x = "Land mitigation policy", y = "1000 EJ", linetype = "CCS technology", alpha = "CCS technology"
       ) +
  scale_alpha_manual(values = c(0.8, 1)) +
  scale_linetype_manual(values = c(1, 2)) +
  scale_fill_brewer(palette = "Set1", name = "Sector", direction = -1,
                    limits = c(
                      "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                    )) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A2; A2

A2 %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_PrimaryBiomassCCS_PanelA_test1"), h = 2000, w = 4400,  r = 300)


data %>%  mutate(value = if_else(CCS == "CCS", value, -value)) %>%
  ggplot +   facet_grid( ~LandSupply) +
  guides(fill = guide_legend(order = 1),
         linetype = guide_legend(order = 2),
         alpha = guide_legend(order = 3)) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = LCT, y = value, fill = sector, linetype = CCS, alpha = CCS),
           stat = "identity", position = "stack",
           color = "black") +

  geom_text(data =  BiomassALL_CCSShare %>% proc_scen(),
            aes(x = LCT, y = 14, label = paste0(round(CCSShare * 100,0), "%")),
            hjust = 0.5, size = 5.5, color = "black", fontface = 4
  ) +

  labs(x = "Land mitigation policy", y = "1000 EJ", linetype = "CCS technology", alpha = "CCS technology"
  ) +
  scale_alpha_manual(values = c(0.8, 1)) +
  scale_linetype_manual(values = c(1, 2)) +
  scale_fill_brewer(palette = "Set1", name = "Sector", direction = -1,
                    limits = c(
                      "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                    )) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A2; A2

A2 %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_PrimaryBiomassCCS_PanelA_test2"), h = 2000, w = 4400,  r = 300)


data %>%
  ggplot +   facet_grid( ~LandSupply) +
  guides(fill = guide_legend(order = 1),
         linetype = guide_legend(order = 2),
         alpha = guide_legend(order = 3)) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = LCT, y = value, fill = sector, linetype = CCS, alpha = CCS),
           stat = "identity", position = "stack",
           color = "black") +

  geom_text(data =  BiomassALL_CCSShare %>% proc_scen(),
            aes(x = LCT, y = 14, label = paste0(round(CCSShare * 100,0), "%")),
            hjust = 0.5, size = 5.5, color = "black", fontface = 4
  ) +

  geom_point(data = data %>%  mutate(value = if_else(CCS == "CCS", value, -value)) %>%
               Agg_reg(LandSupply, LCT) %>% mutate(color = "Total Demand"),
             aes(x= LCT, y = value, color = color), size = 2  ) +

  labs(x = "Land mitigation policy", y = "1000 EJ", linetype = "CCS technology", alpha = "CCS technology",
       color = "CCS + NoCCS"
  ) +
  scale_alpha_manual(values = c(0.8, 1)) +
  scale_linetype_manual(values = c(1, 2)) +
  scale_color_manual(values = "blue") +
  scale_fill_brewer(palette = "Set1", name = "Sector", direction = -1,
                    limits = c(
                      "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                    )) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A2; A2

A2 %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_PrimaryBiomassCCS_PanelA_test3"), h = 2000, w = 4400,  r = 300)




# testing ggpattern
# package is not stable
# note that this may take very long and likely won't work for facets
library(ggpattern)

  data %>%  mutate(value = if_else(CCS == "CCS", value, -value)) %>%
    filter(LandSupply == "2C Main") %>%
    ggplot +   #facet_grid( ~LandSupply) +
    guides(fill = guide_legend(order = 1),
           alpha = guide_legend(order = 2)) +
    geom_hline(yintercept = 0) +
    geom_bar_pattern(position="stack",stat="identity", color = "black",
                     mapping=aes(x = LCT, y = value,  fill=sector, linetype = CCS, pattern=CCS)) +

    geom_text(data =  BiomassALL_CCSShare %>% proc_scen() %>% filter(LandSupply == "2C Main"),
              aes(x = LCT, y = 13, label = paste0(round(CCSShare * 100,0), "%")),
              hjust = 0.5, size = 5.5, color = "black", fontface = 4
    ) +
    labs(x = "Land mitigation policy", y = "1000 EJ", linetype = "CCS technology",
         pattern = "CCS technology" ) +
    scale_pattern_manual(values = c(NoCCS = "stripe", CCS = "none")) +
    scale_fill_brewer(palette = "Set1", name = "Sector", direction = -1,
                      limits = c(
                        "Demand: Electricity", "Demand: Refining", "Demand: Gas", "Demand: Hydrogen", "Demand: Final energy"
                      )) +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black", size=1),
          axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
          axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
          text = element_text(family= fontfamily),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0))
          ) +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines")) -> A2; A2
  A2 %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_PrimaryBiomassCCS_PanelA_test3"), h = 2500, w = 2000,  r = 300)








