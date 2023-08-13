SIFigs_LandToEMs <- function(){

  outdir <- "output/GCAM/"
  OutFolderName <- "SI"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)


  # LULUCF vs. Forest ----
  "LULUCF vs. Forest" -> LULUCF_FOR

  LoadFigData("LandToEMsTotal") -> LandToEMsTotal


  ggplot() +
    facet_wrap(~LandSupply, nrow = 1) +
    geom_path(data = LandToEMsTotal %>%
                bind_rows(
                  LandToEMsTotal %>% mutate(Mha = 0, GtCO2 = 0 )
                ),
              aes(x = Mha, y = GtCO2, color = LCT), size = 2, alpha = 0.7 ) +
    geom_point(data = LandToEMsTotal,
               aes(x = Mha, y = GtCO2, fill = LCT), size = 2.5, shape = 21) +

    geom_abline(intercept = 0, slope = -10/ 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -15/ 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -20 / 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -25/ 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -30/ 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -35/ 1000, linetype = 2, color = "grey80") +

    geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
              aes(y = -7.5, x = 820, label = "`-10tCO`[2]/ha/yr"), parse = TRUE,
              angle = -49, #atan(-35)/pi * 180 ,
              hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
    geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
              aes(y = -9.8, x = 260, label = "-35"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
    geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
              aes(y = -9.8, x = 400, label = "-25"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
    geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
              aes(y = -9.8, x = 680, label = "-15"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-10.5, 0)) +
    geom_vline(xintercept = 0) +
    labs(x = expression(paste(Mha, " ", yr^-1)), y = expression(paste(GtCO[2], " ", yr^-1)),
         linetype = "Scenario", color = "Scenario", fill = "Scenario") +
    theme_bw() + theme0 +
    scale_color_npg(name = "Scenario") +
    scale_fill_npg(name = "Scenario") +

    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          panel.spacing = unit(0.5, "lines")) -> A2;A2


  LandToEMs_decompose <- LoadFigData("LandToEMs_decompose")
  LandToEMs_decomposePoint <- LoadFigData("LandToEMs_decomposePoint")

  ggplot() +
    guides(colour = guide_legend(order = 1),
           linetype = guide_legend(order = 2)) +
    facet_grid(LandSupply~LCT) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -2, linetype = 2, color = "grey50") +
    geom_hline(yintercept = -1, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1, linetype = 2, color = "grey50") +
    geom_path(data = LandToEMs_decompose %>% arrange(`Land CDR efficiency`, Arrow) %>%
                filter(LCT != "Reference") %>%
                mutate(LCT = factor(LCT, levels = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP", "Reference"))),
              aes(x = Mha, y = GtCO2, linetype = `Land CDR efficiency`,
                  color = LCT ), size = 1.5,
              arrow = arrow(length = unit(0.2, "cm")))  +
    geom_point(data = LandToEMs_decompose %>% filter(LCT != "Reference"),
               aes(x = Mha, y = GtCO2), size = 2.5, shape = 21, fill = "red") +

    geom_path(data = LandToEMs_decompose %>% arrange(`Land CDR efficiency`, Arrow) %>%
                filter(LCT == "Reference") %>% select(-LCT, -LandSupply) %>%
                filter(`Land CDR efficiency` != "NonLand-based BECCS"),
              aes(x = Mha, y = GtCO2, linetype = `Land CDR efficiency`,
                  color = "Reference" ), size = 1.5,
              arrow = arrow(length = unit(0.2, "cm")))  +
    geom_point(data = LandToEMs_decompose %>% filter(LCT == "Reference") %>%
                 select(-LCT, -LandSupply),
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
                filter(`Land CDR efficiency` == "Land-based BECCS") %>%
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
    scale_y_continuous(expand = c(0, 0), limits = c(-10.5, 2)) +
    labs(x = expression(paste(Mha, " ", yr^-1)), y = expression(paste(GtCO[2], " ", yr^-1))) +
    scale_color_npg(name = "Scenario", limits = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP", "Reference")) +
    scale_linetype_manual(values = c(3, 2, 1), name = "Land CDR efficiency") +
    theme_bw() + theme0 +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          #strip.background = element_rect(colour = "black", fill = NA),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(.5, "lines")) + theme_leg -> A3;A3


  (A2 + labs(title = "(A) Cumulative land CDR vs. 2020 - 2100 mean land use in mitigation scenarios") +
      theme(plot.title = element_text(hjust = 0, face = "bold") , legend.position = "none",
            axis.title.x = element_blank()) ) /
    (A3  +  labs(title = "(B) Decomposition of land carbon removal efficiency") +
       theme(plot.title = element_text(hjust = 0, face = "bold")) )+
    plot_layout(heights = c(0.27, 1),guides = 'collect') -> p


  p %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_LandToEMs_All_bind_Mit_", LULUCF_FOR), h = 4400, w = 4400,  r = 300)



# LULUCF vs. Forest & Natural----
  "LULUCF vs. Forest & Natural" -> LULUCF_FOR

  LoadFigData("LandToEMsTotal_ForNat") -> LandToEMsTotal

  ggplot() +
    facet_wrap(~LandSupply, nrow = 1) +
    geom_path(data = LandToEMsTotal %>%
                bind_rows(
                  LandToEMsTotal %>% mutate(Mha = 0, GtCO2 = 0 )
                ),
              aes(x = Mha, y = GtCO2, color = LCT), size = 2, alpha = 0.7 ) +
    geom_point(data = LandToEMsTotal,
               aes(x = Mha, y = GtCO2, fill = LCT), size = 2.5, shape = 21) +

    geom_abline(intercept = 0, slope = -10/ 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -15/ 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -20 / 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -25/ 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -30/ 1000, linetype = 2, color = "grey80") +
    geom_abline(intercept = 0, slope = -35/ 1000, linetype = 2, color = "grey80") +

    geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
              aes(y = -7.5, x = 820, label = "`-10tCO`[2]/ha/yr"), parse = TRUE,
              angle = -49, #atan(-35)/pi * 180 ,
              hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
    geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
              aes(y = -9.8, x = 260, label = "-35"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
    geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
              aes(y = -9.8, x = 400, label = "-25"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
    geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
              aes(y = -9.8, x = 680, label = "-15"), parse = TRUE, hjust = 0.5, size = 3.5, color = "blue", fontface = 1 ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-10.5, 0)) +
    geom_vline(xintercept = 0) +
    labs(x = expression(paste(Mha, " ", yr^-1)), y = expression(paste(GtCO[2], " ", yr^-1)),
         linetype = "Scenario", color = "Scenario", fill = "Scenario") +
    theme_bw() + theme0 +
    scale_color_npg(name = "Scenario") +
    scale_fill_npg(name = "Scenario") +

    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          panel.spacing = unit(0.5, "lines")) -> A2;A2


  ggplot() +
    guides(colour = guide_legend(order = 1),
           linetype = guide_legend(order = 2)) +
    facet_grid(LandSupply~LCT) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -2, linetype = 2, color = "grey50") +
    geom_hline(yintercept = -1, linetype = 2, color = "grey50") +
    geom_hline(yintercept = 1, linetype = 2, color = "grey50") +
    geom_path(data = LandToEMs_decompose %>% arrange(`Land CDR efficiency`, Arrow) %>%
                filter(LCT != "Reference") %>%
                mutate(LCT = factor(LCT, levels = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP", "Reference"))),
              aes(x = Mha, y = GtCO2, linetype = `Land CDR efficiency`,
                  color = LCT ), size = 1.5,
              arrow = arrow(length = unit(0.2, "cm")))  +
    geom_point(data = LandToEMs_decompose %>% filter(LCT != "Reference"),
               aes(x = Mha, y = GtCO2), size = 2.5, shape = 21, fill = "red") +

    geom_path(data = LandToEMs_decompose %>% arrange(`Land CDR efficiency`, Arrow) %>%
                filter(LCT == "Reference") %>% select(-LCT, -LandSupply) %>%
                filter(`Land CDR efficiency` != "NonLand-based BECCS"),
              aes(x = Mha, y = GtCO2, linetype = `Land CDR efficiency`,
                  color = "Reference" ), size = 1.5,
              arrow = arrow(length = unit(0.2, "cm")))  +
    geom_point(data = LandToEMs_decompose %>% filter(LCT == "Reference") %>%
                 select(-LCT, -LandSupply),
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
                filter(`Land CDR efficiency` == "Land-based BECCS") %>%
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
    scale_y_continuous(expand = c(0, 0), limits = c(-10.5, 2)) +
    labs(x = expression(paste(Mha, " ", yr^-1)), y = expression(paste(GtCO[2], " ", yr^-1))) +
    scale_color_npg(name = "Scenario", limits = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP", "Reference")) +
    scale_linetype_manual(values = c(3, 2, 1), name = "Land CDR efficiency") +
    theme_bw() + theme0 +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
          #strip.background = element_rect(colour = "black", fill = NA),
          panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(.5, "lines")) + theme_leg -> A3;A3


  (A2 + labs(title = "(A) Cumulative land CDR vs. 2020 - 2100 mean land use in mitigation scenarios") +
      theme(plot.title = element_text(hjust = 0, face = "bold") , legend.position = "none",
            axis.title.x = element_blank()) ) /
    (A3  +  labs(title = "(B) Decomposition of land carbon removal efficiency") +
       theme(plot.title = element_text(hjust = 0, face = "bold")) )+
    plot_layout(heights = c(0.27, 1),guides = 'collect') -> p



  p %>% Write_png(paste0(OutFolderName,"/SIFig_GCAM_LandToEMs_All_bind_Mit_", LULUCF_FOR), h = 4400, w = 4400,  r = 300)


  }
