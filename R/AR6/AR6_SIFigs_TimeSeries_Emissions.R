
SIFigs_TimeSeries_Emissions <- function(){

  # start from AR6.R----

  outdir <- "output/GCAM/"
  SIOutFolderName <- "SI"
  dir.create(file.path(outdir, SIOutFolderName), showWarnings = F)

  # Load data ----
  AR6_604 <- LoadFigData("AR6_604")

  BREAKs <- c(175,500,825,1150,1475)

  c("Emissions|CO2",
    "Emissions|CO2|AFOLU",
    "Carbon Sequestration|Direct Air Capture",
    "Carbon Sequestration|Enhanced Weathering",
    "Carbon Sequestration|CCS|Biomass",
    "Carbon Sequestration|Land Use|Afforestation",
    "Carbon Sequestration|Land Use|Biochar",
    "Carbon Sequestration|Land Use|Soil Carbon Management") -> Variable_EMs

  assertthat::assert_that(
    all(Variable_EMs %in% c(AR6_604 %>% distinct(Variable) %>% pull)) )


  LEVEL_Var <- c("EM_CO2",  "FFI without BECCS", "AFOLU & BECCS", "EM_CO2_AFOLU", "BECCS",
                 "CS_LU_AF", "CS_LU_biochar", "CS_LU_SOC", "CS_DAC", "CS_EW")
  LABEL_Var <- c("Total Carbon",  "FFI (excl. BECCS)", "LULUCF & BECCS", "LULUCF", "BECCS",
                 "Land: Afforestation", "Land: Biochar", "Land: Soil Carbon",
                   "DACCS", "Enhanced Weathering")

  AR6_604 %>%
    filter(Variable %in% Variable_EMs) %>%
    # make carbon sequestration negative values
    mutate(value = if_else(grepl("^Carbon Sequestration\\|", Variable), -abs(value), value)) %>%
    # change unit to GtCO2 and cumulative
    group_by_at(vars(-value, -year)) %>%
    mutate(value = cumsum(value)/ 1000) %>% ungroup() %>%
    select(-Variable, -Unit, -Element) %>% spread(Var, value) %>%
    mutate(`AFOLU & BECCS` = EM_CO2_AFOLU + BECCS,
           `FFI without BECCS` = EM_CO2 - `AFOLU & BECCS`) %>%
    gather(Var, value, BECCS:`FFI without BECCS`) %>%
    filter(!is.na(value)) %>%
    group_by(Pathway) %>%
    mutate(EM_CO2_2100 = value[Var == "EM_CO2" & year == 2100]) %>% ungroup() %>%
    mutate(EM_CO2_2100_cut = cut(EM_CO2_2100, breaks = BREAKs, dig.lab = 4 )) %>%
    mutate(Var = factor(Var,
                        levels = LEVEL_Var,
                        labels = LABEL_Var)) ->
    EM_AFOLU_BECCS


  EM_AFOLU_BECCS %>%
    filter(year %in% c(2050, 2100))%>%
    group_by(year, Var, EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(value, probs = 0),
              sd25 = quantile(value, probs = 0.25),
              sd05 = quantile(value, probs = 0.5),
              sd75 = quantile(value, probs = 0.75),
              sd100 = quantile(value, probs = 1), .groups = "drop") %>% ungroup() %>%
    mutate(year = case_when(
      EM_CO2_2100_cut == unique(EM_CO2_2100_cut)[1]~ year + as.integer(3),
      EM_CO2_2100_cut == unique(EM_CO2_2100_cut)[2]~ year + as.integer(2),
      EM_CO2_2100_cut == unique(EM_CO2_2100_cut)[3]~ year + as.integer(1),
      EM_CO2_2100_cut == unique(EM_CO2_2100_cut)[5]~ year - as.integer(1),
      EM_CO2_2100_cut == unique(EM_CO2_2100_cut)[6]~ year - as.integer(2),
      TRUE ~ year
    ))-> EM_AFOLU_BECCS_box

  EM_AFOLU_BECCS %>%
    filter(year %in% c(2050, 2100))%>%
    group_by(year, Var, EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(value, probs = 0),
              sd25 = quantile(value, probs = 0.25),
              sd05 = quantile(value, probs = 0.5),
              sd75 = quantile(value, probs = 0.75),
              sd100 = quantile(value, probs = 1),
              mean = mean(value),
              sd = sd(value),
              obs = n(), .groups = "drop") %>% ungroup() %>%
    bind_rows(
      EM_AFOLU_BECCS %>%
        group_by(year, Var) %>%
        summarise(sd0 = quantile(value, probs = 0),
                  sd25 = quantile(value, probs = 0.25),
                  sd05 = quantile(value, probs = 0.5),
                  sd75 = quantile(value, probs = 0.75),
                  sd100 = quantile(value, probs = 1),
                  mean = mean(value),
                  sd = sd(value),
                  obs = n(), .groups = "drop") %>% ungroup() %>%
        mutate(EM_CO2_2100_cut = "[175, 1475]")
    ) ->
    EM_AFOLU_BECCS_box_summary



  EM_AFOLU_BECCS_box_summary %>% filter(year == 2100) %>%
     readr::write_csv(file.path(outdir, SIOutFolderName, "AR6_Emissions_TS_cum_stat_2100.csv"))


  ggplot() + facet_wrap(~Var, scales = "fixed", nrow = 2) +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(data = EM_AFOLU_BECCS,
              aes(x = year, y = value,
                  group = interaction(Model, Scenario, Var),
                  color = EM_CO2_2100_cut), alpha = 0.4) +
    geom_boxplot(data = EM_AFOLU_BECCS_box,
                 aes(x= year, ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100,
                     group = interaction(year, EM_CO2_2100_cut),  fill = EM_CO2_2100_cut),
                 alpha=0.9, lwd=0.4, fatten = 1, stat = "identity") +

    geom_text(data = EM_AFOLU_BECCS_box_summary %>%
                filter(year == 2100, EM_CO2_2100_cut == "[175, 1475]"),
              aes(x = 2020, y = 1600,
                                label = paste0("Obs. = ",obs,"\nMean = ",round(mean, 0),
                                               " Gt\nMedian = ",
                                               round(sd05, 0), " Gt\nSD = ",
                                               round(sd), " Gt" )), hjust = 0,
              size = 4) +

    scale_y_continuous(expand = c(0,0)) +
    labs(y = expression(GtCO[2]), x = "Year") +
    scale_fill_viridis_d(direction = -1, name = expression(paste("AR6 CB ", "(",GtCO[2],")"))) +
    scale_color_viridis_d(direction = -1, name = expression(paste("AR6 CB ", "(",GtCO[2],")"))) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid = element_blank()) -> p;p

  p %>% Write_png(paste0(SIOutFolderName, "/SIFig_AR6_TS_EMs"), h = 3000, w = 4500, r = 300)

  EM_AFOLU_BECCS %>%
    filter(Var %in% c("Land: Afforestation", "LULUCF"), year == 2100) %>%
    spread(Var, value) %>%
    filter(!is.na(LULUCF), !is.na(`Land: Afforestation`)) %>%
    ggplot() +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    geom_abline(slope = 1, intercept = 0) +
    geom_point(aes(x = LULUCF, y = `Land: Afforestation`, fill = Model),
               shape = 21, size = 3, alpha = 0.8) +
    geom_smooth(aes(x = LULUCF, y = `Land: Afforestation`), method = "lm",
                color = "red", alpha = 0.3, linetype =2) +
    scale_x_continuous(expand = c(0,0), limits = c(-600,100)) +
    scale_y_continuous(expand = c(0,0), limits = c(-600,100)) +
    theme_bw() + theme0  -> p

  p %>% Write_png(paste0(SIOutFolderName, "/SIFig_AR6_2100CumEMs_AFF_LULUCF"), h = 4500, w = 6000)


}
# Done with large figures to time-series with distributions ----

