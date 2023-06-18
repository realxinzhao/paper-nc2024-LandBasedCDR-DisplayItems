

SIFigs_AR6Pathways_Summary <- function(){

  outdir <- "output/GCAM/"
  OutFolderName <- "Main"
  SIOutFolderName <- "SI"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)


  # Load data ----
  AR6_604 <- LoadFigData("AR6_604")

  SR15_r2 <- readxl::read_excel("data/SR15/iamc15_scenario_data_world_r2.0.xlsx", sheet = 2)
  SR15_r2 %>% distinct(Model, Scenario) -> MS_SR15


  AR6_604 %>% distinct(Model, Scenario, Pathway)  %>%
    mutate(DB = if_else(paste0(Model, Scenario) %in%
                          c(MS_SR15 %>%
                              mutate(MS_SR15 = paste0(Model, Scenario)) %>% pull(MS_SR15)),
                        "AR6 SR15", "AR6 New")) %>%
    group_by(DB, Model) %>% summarise(n = n(), .groups = "drop") %>%
    arrange(DB, n) %>%
    mutate(Model = factor(Model, levels = unique(B$Model))) %>%
    arrange(DB) %>%
    ggplot() + #facet_wrap(~DB, scales = "free") +
    geom_bar(aes(x = Model, y = n, group = Model, fill = DB), alpha = 0.8,
             color = "black", size = 0.5, stat = "identity") +
    labs(x = "Model", y = "Count") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_npg(name = "Database") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0, vjust = 0.5, size = 10, color = "black"),
          axis.text.x = element_text(angle = 90, size = 12) ) +
    theme0 +
    coord_flip() -> model_count; model_count

  ## Generate a temp and CB histogram chart*** ----
  # AR6 < 1400 Gt 1030

  c("AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile",
    "Emissions|CO2",
    "Emissions|CO2|Energy and Industrial Processes",
    "Emissions|CO2|AFOLU",
    "Emissions|CO2|Other",
    "Carbon Sequestration|CCS",
    "Carbon Sequestration|Direct Air Capture",
    "Carbon Sequestration|Land Use",
    "Carbon Sequestration|Enhanced Weathering",
    "Carbon Sequestration|CCS|Biomass",
    "Carbon Sequestration|Land Use|Afforestation",
    "Carbon Sequestration|Land Use|Biochar",
    "Carbon Sequestration|Land Use|Soil Carbon Management",
    "Carbon Sequestration|Land Use|Other" ) -> Variable

  c("Temp",
    "EM_CO2",
    "EM_CO2_EIP",
    "EM_CO2_AFOLU",
    "EM_CO2_Other",
    "CS_CCS",
    "CS_DAC",
    "CS_LU",
    "CS_EW",
    "BECCS",
    "CS_LU_AF",
    "CS_LU_biochar",
    "CS_LU_SOC",
    "CS_LU_other"
  ) -> Var

  data.frame(Variable = Variable, Var = Var) %>%
    as_tibble() -> map_var

  map_var$Var <-  factor(map_var$Var,
                         levels = Var)


  AR6_604 %>% distinct()

  # 2100 Temp FYI
  map_var %>% left_join(AR6_604 %>% select(-Var), by = "Variable") %>%
    mutate(DB = if_else(paste0(Model, Scenario) %in%
                          c(MS_SR15 %>%
                              mutate(MS_SR15 = paste0(Model, Scenario)) %>% pull(MS_SR15)),
                        "AR6 SR15", "AR6 New")) %>%
    filter(Var %in% c("EM_CO2")) %>%
    mutate(value = value / 1000) %>%
    group_by(Variable, Scenario, Model, Region) %>%
    mutate(value = approx_fun(year, value, rule = 2)) %>%
    filter(year >= 2020) %>%
    # consistent with ENGAGE (it is 2019 not 2018!)
    # updated again for AR6, using the beginning of 2020 at the starting point
    mutate(value = cumsum(value)) %>%
    ungroup() %>% filter(year == 2100) -> df
  df %>% summarise(mean = mean(value),sd = sd(value),
                   median = median(value), n = n()) %>%
    mutate(DB ="AR6 New")-> df1

  df %>%
    ggplot(aes(x = value, fill = DB, group = DB)) +
    geom_histogram(binwidth = 50, colour="black", alpha = 0.8, size = 0.5) +
    geom_density(aes(y = 50 * ..count.., color = DB), fill = "white", alpha = 0, size = 1) +
    geom_vline(aes(xintercept = 1150), color="blue", linetype="dashed", size= 0.5) +
    geom_vline(aes(xintercept = 500), color="blue", linetype="dashed",  size= 0.5) +
    geom_vline(aes(xintercept = df1$mean), color="black", linetype="dashed",  size= 0.8) +
    geom_text(data = df1, aes(x = mean - 20, y = 50,
                              label = paste0("Obs. = ", n,"\nMean = ",round(mean, 0),
                                             " Gt\nMedian = ", round(median),
                                             " Gt\nSD = ",
                                             round(sd), " Gt" )),
              vjust = 1, hjust = 0, size = 5) +
    scale_x_continuous(breaks = c(300, 500, 1000, 1150, 1400)) +
    scale_fill_npg(name = "Database") +
    scale_color_npg(name = "Database") +
    labs(y = "Count", x = expression(GtCO[2])) +
    theme_bw() + theme0 +
    coord_flip() -> Model_CEM_cum; Model_CEM_cum


  map_var %>% left_join(AR6_604 %>% select(-Var), by = "Variable") %>%
    mutate(DB = if_else(paste0(Model, Scenario) %in%
                          c(MS_SR15 %>%
                              mutate(MS_SR15 = paste0(Model, Scenario)) %>% pull(MS_SR15)),
                        "AR6 SR15", "AR6 New")) %>%
    filter(Var %in% c("Temp")) %>%
    filter(year == 2100)  -> dft
  dft %>% summarise(mean = mean(value),sd = sd(value),
                    median = median(value), n = n()) %>%
    mutate(DB ="AR6 New")-> dft1

  dft %>%
    ggplot(aes(x = value, fill = DB, group = DB)) +
    geom_histogram(binwidth = 0.08, colour="black", alpha = 0.8, size = 0.5) +
    geom_density(aes(y = 0.08 * ..count.., color = DB), fill = "white", alpha = 0, size = 1) +
    geom_vline(aes(xintercept = 2), color="blue", linetype="dashed", size= 0.5) +
    geom_vline(aes(xintercept = 1.5), color="blue", linetype="dashed",  size= 0.5) +
    geom_vline(aes(xintercept = dft1$mean), color="black", linetype="dashed",  size= 0.8) +
    geom_text(data = dft1, aes(x = mean +0.4 , y = 75,
                               label = paste0("Obs. = ", n,"\nMean = ",round(mean, 1),
                                              " °C\nMedian = ", round(median, 1),
                                              " °C\nSD = ",
                                              round(sd, 1), " °C" )),
              vjust = 1, hjust = 0, size = 5) +
    scale_fill_npg(name = "Database") +
    scale_color_npg(name = "Database") +
    labs(y = "Count", x = "°C") +
    theme_bw() + theme0 +
    coord_flip() -> Model_TEMP; Model_TEMP


  ## Patchwork plots----
  model_count +
    theme(plot.title.position = "plot", plot.title = element_text(hjust = 0.1, face = "bold")) +
    labs(title = "(A) AR6 pathways by model") +
    theme(legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)) +
    plot_spacer() +
    ( (Model_TEMP + theme(plot.title.position = "plot", plot.title = element_text(hjust = 0.1, face = "bold")) +
         labs(title = "(B) Temperature change") +
         theme(legend.position = "none") )   /
        (Model_CEM_cum + theme(plot.title.position = "plot", plot.title = element_text(hjust = 0.1, face = "bold")) +
           labs(title = "(C) Cumulative emissions") +
           theme(legend.position = "none") ) #+ plot_layout(guides = 'collect')
    ) + plot_layout(guides = 'collect', widths = c(3,0.1,3)) -> p;p



  p %>% Write_png(paste0(SIOutFolderName, "/SIFig_AR6SR15_temp_model_175_1470"),
                    w = 9000, h = 8500)

}

