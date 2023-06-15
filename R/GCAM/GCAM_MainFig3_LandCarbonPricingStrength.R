MainFig3_LandCarbonPricingStrength <- function(){



  outdir <- "output/GCAM/"
  OutFolderName <- "Main"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)

    #FigA & B----
    # Load data
    Fig_CEM_decompose_BECCS_LUC <- LoadFigData("CEM_decompose_BECCS_LUC") %>% filter(LandSupply == "2C Main")

    Fig_CEM_decompose_BECCS_LUC %>%
      filter(LCT %in% c("No-LCP", "50%-LCP", "100%-LCP")) %>%
      Agg_reg(LCT, LandSupply) -> NCEM_sector_agg_Total

    NCEM_sector_agg_Total %>%
      group_by(scenario, LCT, LandSupply) %>%
      Fill_annual(CUMULATIVE = T) %>%
      mutate(max = max(value)) %>%
      filter(value == max) %>% ungroup() -> NZyears



    br_pal1 <- brewer.pal(7,"RdBu") %>% rev;
    br_pal0 <- brewer.pal(11,"BrBG");

    mypal <- c( br_pal1[c(1:3)], #"grey85",
                br_pal0[5],
                br_pal1[c(5:7)]);



    Fig_CEM_decompose_BECCS_LUC %>%
      filter(LCT %in% c("No-LCP", "50%-LCP", "100%-LCP")) %>%
      group_by_at(vars(-value, - year)) %>%
      Fill_annual() %>% ungroup() %>%
      ggplot + facet_grid( ~ LCT) +
      guides(colour = guide_legend(order = 2),
             fill = guide_legend(order = 1)) +
      geom_hline(yintercept = 0) +
      geom_vline(data = NZyears, aes(xintercept = year), color = "red", linetype = 1, size = 1) +
      geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
                color = "black", size = 0.4) +
      geom_line(data = NCEM_sector_agg_Total %>% proc_scen() %>% mutate(year = as.integer(year), ss = "Net Total"),
                aes(x = year, y = value, color = ss ), size = 1.2, linetype = 2) +
      geom_text(data = NZyears, aes(label = paste0("Peak Year \n(", round(value,0), "Gt)"), x = year -1, y = 38),
                 size = 9, color = "red", hjust = 1) +
      labs(x = "Year", y = expression(paste(GtCO[2], " ", yr^-1)), fill = "Sector", color = "") +
      #scale_fill_brewer(palette = "RdBu", name = "Source", direction = -1) +
      scale_fill_manual(values = mypal, name = "Source") +
      scale_color_manual(values = "black") +
      scale_y_continuous(breaks = seq(-20, 40, 10)) +
      theme_bw() + theme0 +
      theme(axis.text.x = element_text(angle = 90), legend.text.align = 0,
            strip.background = element_blank(),
            panel.grid = element_blank(),
            panel.spacing.x = unit(1.5, "lines")) -> FigB; FigB
    ##FigB* ----


    Fig_CEM_decompose_BECCS_LUC %>%
      group_by_at(vars(-value, -year)) %>%
      Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
      filter(year == 2100) %>%
      Agg_reg() %>% mutate(ss = "Net Total") %>%
      bind_rows(
        Fig_CEM_decompose_BECCS_LUC %>%
          filter(grepl("BECCS|LULUCF", sector)) %>%
          group_by_at(vars(-value, -year)) %>%
          Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
          filter(year == 2100) %>%
          Agg_reg() %>% mutate(ss = "Land-based CDR")
      ) %>%
      bind_rows(
        Fig_CEM_decompose_BECCS_LUC %>%
          filter(!grepl("BECCS|LULUCF", sector)) %>%
          group_by_at(vars(-value, -year)) %>%
          Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
          filter(year == 2100) %>%
          Agg_reg() %>% mutate(ss = "FFI")
      )-> NCEM_sector_agg_Total


    Fig_CEM_decompose_BECCS_LUC %>%
      group_by_at(vars(-value, -year)) %>%
      Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
      group_by(scenario) %>%
      filter(year == unique(last(year))) %>% ungroup() %>%
      mutate(year = as.integer(year)) %>%
      ggplot +
      guides(colour = guide_legend(order = 2),
             fill = guide_legend(order = 1)) +
      geom_bar(aes(x = LCT, y = value, fill = sector), stat = "identity", position = "stack",
               color = "black") +
      geom_hline(yintercept = 0) +
      geom_point(data = NCEM_sector_agg_Total %>% proc_scen() %>% mutate(year = as.integer(year)),
                 aes(x = LCT, y = value, color = ss), size = 20, shape = "-") +
      labs(x = "Land C pricing strength scenario", y = expression(paste(GtCO[2])), fill = "Sector", color = "") +
      scale_fill_manual(values = mypal, name = "Source") +
      scale_color_manual(values = c("blue", "red", "Black")) +
      scale_y_continuous(breaks = c(-500, 0, 500, 1000, 1500)) +
      theme_bw() + theme0 +
      theme(axis.text.x = element_text(angle = 90), legend.text.align = 0,
            panel.grid = element_blank()
      ) -> FigA; FigA
    ##FigA* ----

    FigA + theme(plot.title = element_text(face = "bold", margin = margin(t=5, b=10)),
                 text = element_text(size = 28), strip.text = element_text(size = 26),
                 axis.text.x = element_text(size = 24, angle = 0, margin = margin(t =10, b = 0)),
                 axis.text.y = element_text(size = 24),
                 axis.title.x = element_text(size = 24, margin = margin(t = 20, b = 5), vjust = 0.5),
                 axis.title.y = element_text(size = 24),
                 legend.key.size = unit(1, "cm"),
                 legend.spacing.y = unit(0.5, 'cm'),
                 plot.margin = margin(r = 10, l = 10)
                 ) +
      labs(title = "(A) Cumulative Carbon emissions") -> FigA1
      FigB +  theme(legend.position = "none",
                    plot.title = element_text(face = "bold", margin = margin(t=5, b=10)),
                    text = element_text(size = 28), strip.text = element_text(size = 26),
                    axis.text.x = element_text(size = 24, angle = 30, hjust = 0.7, vjust = 1,
                                               margin = margin(t =10, b = 0)),
                    axis.text.y = element_text(size = 24),
                    axis.title.x = element_text(size = 24, margin = margin(t = 20, b = 5), vjust = 0.5),
                    axis.title.y = element_text(size = 24),
                    plot.margin = margin(r = 10, l = 10) ) +
      labs(title = "(B) Annual Carbon emissions") -> FigB1



    legendA1 <- get_legend(FigA1 + theme(legend.box.margin = margin(l = 0, r = 10)))
    plot_grid(FigA1 +  theme(legend.position = "none", plot.title = element_blank()),
              FigB1 +  theme(plot.title = element_blank()),
              legendA1, nrow = 1,
              rel_widths = c(0.4, 0.78, 0.22),
              align = c('hv'), axis = "tb") -> FigAB0
    ##FigAB0* ----


    ggdraw() + draw_label("(A) Cumulative carbon emissions", size = 30,
                          fontface = 'bold', x = 0, hjust = 0) +
      theme(plot.margin = margin(t=5, b=10, l = 20)) -> FigA1Title

    ggdraw() + draw_label("(B) Annual carbon emissions", size = 30,
                          fontface = 'bold', x = 0, hjust = 0) +
      theme(plot.margin = margin(t=5, b=10, l = 10)) -> FigB1Title

    plot_grid(
      FigA1Title, FigB1Title, NULL, ncol = 3, rel_widths = c(0.4, 0.78, 0.22)
    ) -> FigABTitles

    plot_grid(
      FigABTitles, FigAB0, ncol = 1, rel_heights = c(0.1, 1) ) ->
      FigAB


    #FigC Biomass ----

    LoadFigData("BiomassALL_PrimaryEnergyBal") %>%
      filter(LandSupply == "2C Main")-> BiomassALL

    BiomassALL %>%
      mutate(sector = factor(sector,
                             levels = c("Supply: Purpose-grown","Supply: Residue", "Supply: MSW",
                                        "Demand: Final energy",  "Demand: Gas", "Demand: Hydrogen", "Demand: Refining", "Demand: Electricity"),
                           labels = c("Supply: Purpose-grown","Supply: Residue", "Supply: MSW",
                                      "Demand: Final energy",  "Demand: Gas & Hydrogen", "Demand: Gas & Hydrogen", "Demand: Refining", "Demand: Electricity"
                                       ) ) ) %>%
      group_by_at(vars(-value)) %>%
      summarise(value = sum(value)) %>% ungroup() %>%
      replace_na(list(CCS = "CCS")) %>%
      mutate(CCS = factor(CCS,
                          levels = c("CCS", "NoCCS"),
                          labels = c("Demand: with CCS tech.", "Demand: without CCS tech.") ))->
      BiomassALL1


    BiomassALL1 %>% mutate(value = if_else(DS == "demand", -value, value)) %>%
      mutate(value = value / 81) %>%
      Agg_reg(sector, LandSupply, LCT) %>%
      group_by_at(vars(-year,-value)) %>%
      Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
      group_by(scenario, sector, LandSupply, LCT) %>%
      filter(year == unique(last(year))) %>% ungroup() %>%
      ggplot +
      geom_hline(yintercept = 0) +
      geom_bar(aes(x = LCT, y = value, fill = sector),
               stat = "identity", position = "stack",
               color = "black") +
      labs(x = "Land C pricing strength scenario", y =  expression(paste(EJ, " ", yr^-1)) ) +
      scale_fill_brewer(palette = "RdYlGn", name = "Sector (Panel C)", direction = 1,
                        limits = c(
                          "Supply: Purpose-grown","Supply: Residue","Supply: MSW",
                          "Demand: Electricity", "Demand: Refining", "Demand: Gas & Hydrogen", "Demand: Final energy"
                          )) +
      scale_y_continuous(limits = c(-160, 160), breaks = seq(-150, 150, 50)) +
      theme_bw() + theme0 +
      theme(axis.text.x = element_text(angle = 90),
            panel.grid = element_blank(),
            panel.spacing.y = unit(0.5, "lines"),
            panel.spacing.x = unit(0.5, "lines")) -> FigC; FigC


    #FigD LUC ----

    br_pal0 <- brewer.pal(11,"BrBG");
    br_pal <- brewer.pal(11,"RdYlBu");
    br_pal2 <- brewer.pal(5,"Spectral");
    br_pal3 <- brewer.pal(9,"Set1");



    mypal <- c( br_pal[8], br_pal2[4], br_pal3[1], #br_pal[3],
                br_pal3[5], br_pal0[6]);

    br_pal2 <- brewer.pal(11,"Spectral");
    mypal <- c(br_pal2[c(5, 2, 9, 8)],  br_pal0[3] #"grey85" #br_pal0[6]
                );

    LoadFigData("LandALL") %>%
      filter(LandSupply == "2C Main") -> LandALL


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
      group_by_at(vars(-value)) %>% summarise(value = sum(value), .groups = "drop") %>%
      group_by_at(vars(-year, -value)) %>%
      summarise(Mean = mean(value),
                `2050` = value[year == 2050],
                `2100` = value[year == 2100]) %>% ungroup() %>%
      gather(year, value, Mean:`2100`) %>%
      mutate(year = factor(year, levels = c("2050", "Mean", "2100"))) %>%
      mutate(land = factor(land, levels = c("Cropland: NonEnergy", "Cropland: Energy",
                                            "Forest", "Other natural","Pasture" ))) %>%
      ggplot +   facet_grid(~LCT) +
      geom_hline(yintercept = 0) +
      geom_bar(aes(x = year, y = value, fill = land), stat = "identity", position = "stack", alpha = 0.9,
                color = "black") +
      labs(x = "Year & Mean", y = expression(paste(Mha))) +
      scale_fill_manual(values = mypal, name = "Land (Panel D)") +
      #scale_fill_brewer(palette = "Set1", name = "Land", direction = -1) +
      scale_y_continuous(breaks = seq(-750, 750, 250)) +
      theme_bw() + theme0 +
      theme(axis.text.x = element_text(angle = 90),
            strip.background = element_blank(),
            panel.grid = element_blank(),
            panel.spacing.y = unit(0.5, "lines"),
            panel.spacing.x = unit(0.5, "lines")) -> FigD; FigD


##FigCD0----
    FigC + theme(text = element_text(size = 28), strip.text = element_text(size = 26),
                 axis.text.x = element_text(size = 24, angle = 0, margin = margin(t =10, b = 0)),
                 axis.text.y = element_text(size = 24),
                 axis.title.x = element_text(size = 24, margin = margin(t = 20, b = 5), vjust = 0.5),
                 axis.title.y = element_text(size = 24),
                 legend.key.size = unit(1, "cm"),
                 legend.spacing.y = unit(0.5, 'cm'),
                 plot.margin = margin(r = 5, l = 10)
    )  -> FigC1;FigC1


    FigD  + theme(text = element_text(size = 28), strip.text = element_text(size = 26),
                  axis.text.x = element_text(size = 24, angle = 0, margin = margin(t =10, b = 0)),
                  axis.text.y = element_text(size = 24),
                  axis.title.x = element_text(size = 24, margin = margin(t = 20, b = 5), vjust = 0.5),
                  axis.title.y = element_text(size = 24),
                  legend.key.size = unit(1, "cm"),
                  legend.spacing.y = unit(0.5, 'cm'),
                  plot.margin = margin(r = 10, l = 5)
    )  -> FigD1;FigD1


    legendC1 <- get_legend(FigC1 + theme(legend.box.margin = margin(l = 0, r = 0)))
    legendD1 <- get_legend(FigD1 + theme(legend.box.margin = margin(l = 0, r = 0)))
    legendCD <- plot_grid(legendC1, legendD1, nrow = 2, align = c('hv'))


    plot_grid(FigC1 +  theme(plot.title = element_blank(), legend.position = "none"),
              legendCD,
              FigD1 +  theme(plot.title = element_blank(),legend.position = "none"),
              nrow = 1,
              rel_widths = c(0.4, 0.22, 0.78),
              align = c('hv'), axis = "tb") -> FigCD0


    ggdraw() + draw_label("(C) Primary bioenergy demand & supply \n      (2020 - 2100 mean)", size = 30,
                          fontface = 'bold', x = 0, y = 1, hjust = 0, vjust = 1) +
      theme(plot.margin = margin(t=5, b=10, l = 20)) -> FigC1Title

    ggdraw() + draw_label("(D) Changes in land use relative to 2020", size = 30,
                          fontface = 'bold', x = 0, y = 1, hjust = 0, vjust = 1) +
      theme(plot.margin = margin(t=5, b=10, l = 10)) -> FigD1Title

    plot_grid(
      FigC1Title, NULL, FigD1Title, ncol = 3, align = c('h'), rel_widths = c(0.4, 0.22, 0.78)
    ) -> FigCDTitles

    plot_grid(
      FigCDTitles, FigCD0, ncol = 1, rel_heights = c(0.11, 1) ) ->
      FigCD;FigCD


    #FigEF ----

    # Fig. E ----
    LoadFigData("LandToEMsTotal") %>%
    filter(LandSupply == "2C Main") -> LandToEMsTotal

    ggplot() +
      geom_abline(intercept = 0, slope = -10/ 1000, size = 1.5,  linetype = 2, color = "grey80") +
      geom_abline(intercept = 0, slope = -15/ 1000, size = 1.5,linetype = 2, color = "grey80") +
      geom_abline(intercept = 0, slope = -20 / 1000, size = 1.5,linetype = 2, color = "grey80") +
      geom_abline(intercept = 0, slope = -25/ 1000, size = 1.5,linetype = 2, color = "grey80") +
      geom_path(data = LandToEMsTotal %>%
                  bind_rows(
                    LandToEMsTotal %>% mutate(Mha = 0, GtCO2 = 0 )
                  ),
                aes(x = Mha, y = GtCO2, color = LCT), size = 3.5, alpha = 0.7 ) +
      geom_point(data = LandToEMsTotal,
                 aes(x = Mha, y = GtCO2, fill = LCT), size = 5, shape = 21) +


      geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
                aes(y = -3.5, x = 400, label = "`-10tCO`[2]/ha/yr"), parse = TRUE,
                angle = -40, #atan(-35)/pi * 180 ,
                hjust = 0.5, size = 9, color = "blue", fontface = 2 ) +

      geom_text(data = LandToEMsTotal ,
                aes(y = -6.8, x = 240, label = "-25"), parse = TRUE, hjust = 0.5, size = 8, color = "blue", fontface = 1 ) +
      geom_text(data = LandToEMsTotal ,
                aes(y = -6.8, x = 330, label = "-20"), parse = TRUE, hjust = 0.5, size = 8, color = "blue", fontface = 1 ) +
      geom_text(data = LandToEMsTotal ,
                aes(y = -6.8, x = 440, label = "-15"), parse = TRUE, hjust = 0.5, size = 8, color = "blue", fontface = 1 ) +


      scale_x_continuous(expand = c(0, 0), limits = c(-1, 600), breaks = c(0, 150, 300, 450)) +
      scale_y_continuous(expand = c(0, 0), limits = c(-7, 0)) +
      geom_vline(xintercept = 0) +
      labs(x = expression(paste(Mha, " ", yr^-1)), y = expression(paste(GtCO[2], " ", yr^-1)),
           linetype = "Scenario", color = "Scenario", fill = "Scenario") +
      theme_bw() + theme0 +
      scale_color_npg(name = "Scenario") +
      scale_fill_npg(name = "Scenario") +

      theme(panel.grid = element_blank(),
            legend.text.align = 0,
            panel.spacing = unit(0.5, "lines")) -> FigE



    # Fig. F ----
    LandToEMs_decompose <- LoadFigData("LandToEMs_decompose") %>%  filter(LandSupply == "2C Main")
    LandToEMs_decomposePoint <- LoadFigData("LandToEMs_decomposePoint") %>%  filter(LandSupply == "2C Main")


    #"LULUCF vs. Forest & Natural" -> LULUCF_FOR
    "LULUCF vs. Forest" -> LULUCF_FOR

    ggplot() +
      guides(colour = guide_legend(order = 1),
             linetype = guide_legend(order = 2),
             fill = "none") +
      facet_grid(~LCT) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = -2, linetype = 2, color = "grey50", size = 1.5) +

      geom_abline(intercept = 0, slope = -10/ 1000, size = 1.5, linetype = 2, color = "grey80") +
      geom_abline(intercept = 0, slope = -15/ 1000, size = 1.5,linetype = 2, color = "grey80") +
      geom_abline(intercept = 0, slope = -20 / 1000,size = 1.5, linetype = 2, color = "grey80") +
      geom_abline(intercept = 0, slope = -25/ 1000, size = 1.5,linetype = 2, color = "grey80") +
      geom_path(data = LandToEMs_decompose %>% arrange(`Land CDR efficiency`, Arrow) %>%
                  filter(LCT != "Reference") %>%
                  mutate(LCT = factor(LCT, levels = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP", "Reference"))),
                aes(x = Mha, y = GtCO2, linetype = `Land CDR efficiency`,
                    color = LCT ), size = 2.5,
                arrow = arrow(length = unit(0.8, "cm")))  +
      geom_point(data = LandToEMs_decompose %>% filter(LCT != "Reference"),
                 aes(x = Mha, y = GtCO2, fill = LCT), size = 5, shape = 21#, fill = "black"
                   ) +


      geom_text(data = LandToEMsTotal %>% filter(LCT == "100%-LCP"),
                aes(y = -3.5, x = 400, label = "`-10tCO`[2]/ha/yr"), parse = TRUE,
                angle = -48, #atan(-35)/pi * 180 ,
                hjust = 0.5, size = 9, color = "blue", fontface = 1 ) +

      geom_text(data = LandToEMsTotal ,
                aes(y = -6.8, x = 240, label = "-25"), parse = TRUE, hjust = 0.5, size = 8, color = "blue", fontface = 1 ) +
      geom_text(data = LandToEMsTotal ,
                aes(y = -6.8, x = 330, label = "-20"), parse = TRUE, hjust = 0.5, size = 8, color = "blue", fontface = 1 ) +
      geom_text(data = LandToEMsTotal ,
                aes(y = -6.8, x = 440, label = "-15"), parse = TRUE, hjust = 0.5, size = 8, color = "blue", fontface = 1 ) +


      geom_text(data = LandToEMs_decompose %>% filter(LCT != "Reference") %>%
                  filter(`Land CDR efficiency` == "Land-based BECCS") %>%
                  group_by(LCT, LandSupply) %>%
                  summarise(EFF = round((GtCO2 - first(GtCO2)) /(Mha-first(Mha)) * 1000, 1),
                            Mha = mean(Mha), GtCO2 = mean(GtCO2) ) %>% filter(!is.na(EFF)) %>% ungroup() %>%
                  mutate(Mha = pmax(0.8* Mha, 100)),
                aes(y = GtCO2 -0.6, x = Mha, label = EFF),  hjust = 0.5, size = 9, color = "orange", fontface = 4 ) +

      geom_text(data = LandToEMs_decompose %>% filter(LCT != "Reference") %>%
                  filter(`Land CDR efficiency` == LULUCF_FOR) %>%
                  group_by(LCT, LandSupply) %>%
                  summarise(EFF = round((GtCO2 - first(GtCO2)) /(Mha-first(Mha)) * 1000, 1),
                            Mha = mean(Mha), GtCO2 = mean(GtCO2) ) %>% filter(!is.na(EFF)) %>% ungroup() %>%
                  mutate(Mha = pmax(1.1 * Mha, 100)),
                aes(y = GtCO2, x = Mha + 60, label = EFF),  hjust = 0.5, size = 9, color = "green", fontface = 4 ) +


      scale_x_continuous(expand = c(0, 0), limits = c(-5, 600), breaks = c(0, 150, 300, 450)) +
      scale_y_continuous(expand = c(0, 0), limits = c(-7, 0)) +
      labs(x = expression(paste(Mha, " ", yr^-1)), y = expression(paste(GtCO[2], " ", yr^-1))) +
      scale_color_npg(name = "Scenario", limits = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP")) +
      scale_fill_npg(name = "Scenario", limits = c("No-LCP", "10%-LCP", "50%-LCP", "100%-LCP")) +
      scale_linetype_manual(values = c(3, 2, 1), name = "Land CDR efficiency") +
      theme_bw() + theme0 +
      theme(panel.grid = element_blank(),
            legend.text.align = 0,
            strip.background = element_blank(),
            panel.spacing.y = unit(0.5, "lines"),
            panel.spacing.x = unit(.5, "lines")) + theme_leg -> FigF



    FigE + theme(plot.title = element_text(face = "bold", margin = margin(t=5, b=10)),
                 text = element_text(size = 28), strip.text = element_text(size = 26),
                 axis.text.x = element_text(size = 24, angle = 0, margin = margin(t =10, b = 0)),
                 axis.text.y = element_text(size = 24),
                 axis.title.x = element_text(size = 24, margin = margin(t = 20, b = 5), vjust = 0.5),
                 axis.title.y = element_text(size = 24),
                 legend.key.size = unit(1, "cm"),
                 legend.spacing.y = unit(0.5, 'cm'),
                 plot.margin = margin(r = 10, l = 10)
    ) -> FigE1
    FigF +  theme(plot.title = element_text(face = "bold", margin = margin(t=5, b=10)),
                  text = element_text(size = 28), strip.text = element_text(size = 26),
                  axis.text.x = element_text(size = 24, angle = 0, margin = margin(t =10, b = 0)),
                  axis.text.y = element_text(size = 24),
                  axis.title.x = element_text(size = 24, margin = margin(t = 20, b = 5), vjust = 0.5),
                  axis.title.y = element_text(size = 24),
                  legend.key.size = unit(2, "cm"),
                  legend.spacing.y = unit(0.8, 'cm'),
                  legend.key.height=unit(2.2,"line"),
                  legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 22),
                  plot.margin = margin(r = 10, l = 10) )  -> FigF1



    legendF1 <- get_legend(FigF1 + theme(legend.box.margin = margin(l = 0, r = 10)))
    plot_grid(FigE1 +  theme(legend.position = "none", plot.title = element_blank()),
              FigF1 +  theme(legend.position = "none",plot.title = element_blank()),
              legendF1, nrow = 1,
              rel_widths = c(0.27, 0.75, 0.18),
              align = c('hv'), axis = "tb") -> FigEF0

    ggdraw() + draw_label("(E) Land CDR efficiency", size = 30,
                          fontface = 'bold', x = 0, hjust = 0) +
      theme(plot.margin = margin(t=5, b=10, l = 20)) -> FigE1Title

    ggdraw() + draw_label("(F) Decomposition of land CDR efficiency", size = 30,
                          fontface = 'bold', x = 0, hjust = 0) +
      theme(plot.margin = margin(t=5, b=10, l = 10)) -> FigF1Title


    plot_grid(
      FigE1Title, FigF1Title, NULL, ncol = 3, #rel_widths =  c(0.27, 0.75, 0.18)
      rel_widths =  c(0.27, 0.75, 0.18)
    ) -> FigEFTitles

    plot_grid(
      FigEFTitles, FigEF0, ncol = 1, rel_heights = c(0.1, 1) ) ->
      FigEF
    ##FigEF* ----


    #FigABCDEF----
    plot_grid(FigAB,NULL,
              FigCD,NULL,
              FigEF, nrow = 5, rel_heights = c(0.6, 0.04, 0.6, 0.04, 0.5)) -> FigABCDEF


    FigABCDEF %>% Write_png(paste0(OutFolderName, "/MainFig3_LandCarbonPricingStrength"), h = 8000, w = 9000,  r = 300)


    # Done

}
