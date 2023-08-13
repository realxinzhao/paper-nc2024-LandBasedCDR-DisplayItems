
SIFigs_LandCDR_AR6_byModel<- function(PALPHA = 0.8, PSIZE = 1.8){

  outdir <- "output/GCAM/"
  SIOutFolderName <- "SI"
  dir.create(file.path(outdir, SIOutFolderName), showWarnings = F)


  # Load data ----
  INPUT_DF <- LoadFigData("AR6_604")


  PALPHA0 = PALPHA;  PSIZE0 = PSIZE
  ColorViridis4 <- viridis::viridis(11, alpha = PALPHA0)[c(1,4,7,10)]

  YLIMITs <- c(-850, 850);  XMAX <- 1700
  XBREAKs_right <- c(0, 300, 600, 1025, 1350, 1675)
  XBREAKs_left <- c(0, -150, -600, -1000,  -1400)
  YBREAKS <- c( -800, -600, -400, -200, 0, 175, 500, 825)


  TEXT_ANGLE <- 315

  # themeblank----
  theme(axis.title = element_blank(), axis.text = element_blank(), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), panel.border = element_blank()) ->
    themeblank


  # Fig. 1 AFOLU_vs_BECCS_vs_EIP
  # Get data ready ----
  INPUT_DF %>%
    filter(Var %in% c("EM_CO2", "EM_CO2_AFOLU", "BECCS")) %>%
    group_by_at(vars(-value, -year)) %>%
    mutate(value = cumsum(value)/ 1000) %>% ungroup() %>%
    select(-Variable, -Unit, -Element) %>% spread(Var, value) %>%
    mutate(BECCS = -abs(BECCS),
           `AFOLU & BECCS` = EM_CO2_AFOLU + BECCS,
           `FFI without BECCS` = EM_CO2 - `AFOLU & BECCS`) %>%
    gather(Var, value, BECCS:`FFI without BECCS`) %>%
    filter(!is.na(value)) %>%
    group_by(Pathway) %>%
    mutate(EM_CO2_2100 = value[Var == "EM_CO2" & year == 2100]) %>% ungroup() %>%
    mutate(EM_CO2_2100_cut = cut(EM_CO2_2100, breaks = c(175,500,825,1150,1475), dig.lab = 4 )) %>%
    mutate(Var = factor(Var, levels = c("EM_CO2",  "FFI without BECCS", "AFOLU & BECCS", "EM_CO2_AFOLU", "BECCS"),
                        labels = c("Carbon budget",  "FFI without BECCS", "AFOLU & BECCS", "AFOLU", "BECCS"))) ->
    EM_AFOLU_BECCS

  CutRange <- EM_AFOLU_BECCS %>% distinct(EM_CO2_2100_cut) %>% pull %>% as.character()


  EM_AFOLU_BECCS %>%
      mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                          "", Model)) %>%
      mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-5.005| ",
                          "", Model)) %>%
    mutate(Model = if_else(grepl("MESSAGEix-GLOBIOM_1.1", Pathway), "MESSAGEix-GLOBIOM_1.1", Model))->
    EM_AFOLU_BECCS



  ## BECCS vs. AFOLU ----


  x_title = "BECCS"; y_title = "AFOLU"
  x_title0 = "BECCS"; y_title0 = "AFOLU"
  y_x_data <- EM_AFOLU_BECCS %>% spread(Var, value) %>%
    filter(!is.na(BECCS)) %>% filter(year == 2100)

  y_x_data %>%
    group_by(Model) %>%
    summarise(n = n(), IQR_LULUCF = IQR(AFOLU), IQR_BECCS = IQR(BECCS)) %>%
    mutate(BECCS = -1650, AFOLU = -750)-> PDataStat


  ggplot() + facet_wrap(~Model) +
    guides(shape =guide_legend( order = 1),
           linetype = guide_legend( order = 2),
           color = guide_legend(order = 3))+

    geom_hline(yintercept = 0, color = "black") +
    #geom_abline(slope = -1, intercept = -1100) +
    geom_abline(slope = -1, intercept = -1000) +
    geom_abline(slope = -1, intercept = -800) +
    geom_abline(slope = -1, intercept = -600) +
    geom_abline(slope = -1, intercept = -400) +
    geom_abline(slope = -1, intercept = -200) +


    scale_x_continuous(expand = c(0,0), limits = c(-XMAX, 0), breaks = XBREAKs_left) +
    scale_y_continuous(expand = c(0,0), limits = YLIMITs, breaks = seq(-800, 800, 200),position = "right") +

    geom_point(data = y_x_data %>% filter(EM_CO2_2100_cut %in% CutRange),
               aes(y = get(y_title0), x = get(x_title0), group = Pathway,
                   fill = EM_CO2_2100), color = "black", alpha= PALPHA0, size = PSIZE0, shape = 22
    ) +

    geom_text(data = PDataStat, aes(x = get(x_title0), y = AFOLU + 300, label = paste0("n = ", n)),
              hjust = 0, size = 7, color = "black", fontface = 4 ) +

    geom_text(data = PDataStat, aes(x = get(x_title0), y = AFOLU +150, label = paste0("IQR(LULUCF) = ", round(IQR_LULUCF,0))),
              hjust = 0, size = 7, color = "red", fontface = 4 ) +

    geom_text(data = PDataStat, aes(x = get(x_title0), y = AFOLU, label = paste0("IQR(BECCS) = ", round(IQR_BECCS,0))),
              hjust = 0, size = 7, color = "blue", fontface = 4 ) +

    binned_scale(aesthetics = "fill",
                 scale_name = "stepsn",
                 palette = function(x) c(ColorViridis4),
                 breaks = c(500, 825, 1150),
                 limits = c(175, 1475),
                 show.limits = TRUE,
                 guide = "colorsteps",
                 name = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) +
    labs(y = expression(paste("LULUCF ", "(",GtCO[2],")")),
         x = expression(paste("BECCS ", "(",GtCO[2],")"))) +
    theme_bw()+ theme0 +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.width = unit(0.8,"cm"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5, b = 0)),
          axis.text.y.right = element_text(hjust = 0.25, margin = margin(r = 10, l = 5))) +
    annotate("text", x = -1015, y = 800, label = "-200", angle = TEXT_ANGLE, size = 4, hjust = 0, vjust = 1) +
    annotate("text", x = -1015-200, y = 800, label = "-400", angle = TEXT_ANGLE, size = 4, hjust = 0, vjust = 1) +
    annotate("text", x = -1015-400, y = 800, label = "-600", angle = TEXT_ANGLE, size = 4, hjust = 0, vjust = 1) +
    annotate("text", x = -1015-600, y = 800, label = "-800", angle = TEXT_ANGLE, size = 4, hjust = 0, vjust = 1) +
    annotate("text", x = -1015-600, y = 600, label = expression(paste("-1000 ",GtCO[2], " (LULUCF + BECCS)")),
             angle = TEXT_ANGLE, size = 4, hjust = 0, vjust = 1) +

    theme(panel.spacing = unit(1.5, "lines"),
          plot.margin = margin(l = 8, t = 5)) -> LUC_vs_BECCS;LUC_vs_BECCS

  LUC_vs_BECCS %>% Write_png(paste0(SIOutFolderName, "/SIFig_AR6_BECCS_LULUCF_EIP_byModel"), h = 5700, w = 6000, r = 300)


  }
