MainFig2_CarbonPrices <- function(){

  outdir <- "output/GCAM/"
  OutFolderName <- "Main"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)

  SIOutFolderName <- "SI"
  dir.create(file.path(outdir, SIOutFolderName), showWarnings = F)

  # AR6 results ----
  AR6_604 <- LoadFigData("AR6_604")

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

  (A1 + labs(title = "(A) Shadow price of carbon by sector") +
      theme(plot.title = element_text(hjust = 0, face = "bold"), axis.title.x = element_blank() ) ) /
    (A2  +  labs(title = "(B) Carbon mitigation relative to reference by sector") +
       theme(plot.title = element_text(hjust = 0, face = "bold")) )+
    plot_layout(heights = c(0.5, 0.5),guides = 'collect') -> p

  p %>% Write_png(paste0(SIOutFolderName, "/SIFig_ShadowCPrice_Migitation"), h = 4600, w = 4400,  r = 300)

## done SI figure ----


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

  p %>% Write_png(paste0(OutFolderName, "/MainFig2_CarbonPrices"), h = 2000, w = 5000,  r = 300)
# Gexport ----


  }
