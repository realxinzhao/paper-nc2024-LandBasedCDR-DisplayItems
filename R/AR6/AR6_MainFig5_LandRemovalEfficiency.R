MainFig5_LandRemovalEfficiency <-
  function(PALPHA = 0.8, PSIZE = 1.8){


  outdir <- "output/GCAM/"
  OutFolderName <- "Main"
  SIOutFolderName <- "SI"

  dir.create(file.path(outdir, OutFolderName), showWarnings = F)
  dir.create(file.path(outdir, SIOutFolderName, "LandCDREfficiency"), showWarnings = F)


  # Load data ----
  INPUT_DF <- LoadFigData("AR6_604")


  LoadFigData("LandToEMs_For") -> LandToEMs_For


  LoadFigData("LandALL") %>%
    Agg_reg(LCT, LandSupply, land) %>%
    filter(year %in% c(2020, 2050, 2100)) %>%
    filter(land %in% c("Cropland - Energy")) %>%
    group_by(LCT, LandSupply) %>%
    transmute(LCT, LandSupply, year, LandEndYear = value - value[year == 2020]) %>%
    filter(year %in% c(2050, 2100)) %>%  ungroup() %>%
    filter(LCT %in% c("50%-LCP", "100%-LCP")) -> Land_BECCS

  LoadFigData("LandALL") %>%
    filter(grepl("Forest", land)) %>%
    Agg_reg(LCT, LandSupply) %>%
    filter(year %in% c(2020, 2050, 2100)) %>%
    group_by(LCT, LandSupply) %>%
    transmute(LCT, LandSupply, year, LandEndYear = value - value[year == 2020]) %>%
    filter(year %in% c(2050, 2100)) %>%  ungroup() %>%
    filter(LCT %in% c("50%-LCP", "100%-LCP")) -> Land_For

  LoadFigData("LandALL") %>%
    filter(grepl("Forest|Grassland|Shrubland", land)) %>%
    Agg_reg(LCT, LandSupply) %>%
    filter(year %in% c(2020, 2050, 2100)) %>%
    group_by(LCT, LandSupply) %>%
    transmute(LCT, LandSupply, year, LandEndYear = value - value[year == 2020]) %>%
    filter(year %in% c(2050, 2100)) %>%  ungroup() %>%
    filter(LCT %in% c("50%-LCP", "100%-LCP")) -> Land_ForNat


  Land_BECCS %>% mutate(sector = "Land-based BECCS") %>%
    bind_rows(Land_For %>% mutate(sector = "LULUCF")) %>%
    bind_rows(
      Land_BECCS %>%
        bind_rows(Land_For) %>%
        group_by(LCT, LandSupply, year) %>%
        summarise(LandEndYear = sum(LandEndYear)) %>% mutate(sector = "BothLandbased" )
    )  %>%   mutate(method = "Method1") %>%
    bind_rows(
      Land_BECCS %>% mutate(sector = "Land-based BECCS") %>%
        bind_rows(Land_ForNat %>% mutate(sector = "LULUCF")) %>%
        bind_rows(
          Land_BECCS %>%
            bind_rows(Land_ForNat) %>%
            group_by(LCT, LandSupply, year) %>%
            summarise(LandEndYear = sum(LandEndYear)) %>% mutate(sector = "BothLandbased" )
        )  %>%   mutate(method = "Method2")
    ) -> LandEndYear





  LandToEMs_For %>%
    #mutate(sector = if_else(grepl("BECCS", sector), "BECCS", sector)) %>%
    group_by_at(vars(-GtCO2, -Mha)) %>%
    summarize(Mha = sum(Mha), GtCO2 = sum(GtCO2), .groups = "drop") %>%
    filter(LCT != "Reference") ->
    GCAMResults

  LandToEMs_For %>% filter(sector == "LULUCF") %>%
    group_by(year, LandSupply, LCT) %>%
    summarise_at(vars(Mha:GtCO2), sum) %>%
    mutate(CDR = GtCO2/Mha * 1000) %>%
    filter(LCT != "10%-LCP", LCT != "No-LCP") %>%
    filter(LCT != "Reference") %>%
    group_by(year) %>% summarise(CDR = mean(CDR), n = n())

  GCAMResults %>% filter(!grepl("NonLand", sector)) %>%
    group_by(year, LandSupply, LCT) %>%
    summarise_at(vars(Mha:GtCO2), sum) %>%
    mutate(CDR = GtCO2/Mha * 1000) %>%
    mutate(sector = "BothLandbased")  %>%
    bind_rows(
      GCAMResults %>% filter(!grepl("NonLand", sector)) %>%
        group_by(year, LandSupply, LCT, sector) %>%
        summarise_at(vars(Mha:GtCO2), sum) %>%
        mutate(CDR = GtCO2/Mha * 1000)
    ) %>% filter(LCT %in% c("50%-LCP", "100%-LCP")) %>%
    mutate(method = "Method1") -> M1

  LoadFigData("LandToEMs_ForNat") %>%
    group_by_at(vars(-GtCO2, -Mha)) %>%
    summarize(Mha = sum(Mha), GtCO2 = sum(GtCO2), .groups = "drop") %>%
    filter(LCT != "Reference") ->
    GCAMResults1

  GCAMResults1 %>% filter(!grepl("NonLand", sector)) %>%
    group_by(year, LandSupply, LCT) %>%
    summarise_at(vars(Mha:GtCO2), sum) %>%
    mutate(CDR = GtCO2/Mha * 1000) %>%
    mutate(sector = "BothLandbased")  %>%
    bind_rows(
      GCAMResults1 %>% filter(!grepl("NonLand", sector)) %>%
        group_by(year, LandSupply, LCT, sector) %>%
        summarise_at(vars(Mha:GtCO2), sum) %>%
        mutate(CDR = GtCO2/Mha * 1000)
    ) %>% filter(LCT %in% c("50%-LCP", "100%-LCP")) %>%
    mutate(method = "Method2") %>%
    bind_rows(M1) %>%
    left_join(LandEndYear) %>%
    write.csv( paste0(outdir, "/", SIOutFolderName, "/SITable_LandCDREfficiency.csv") )



  GCAMResults %>% filter(sector == "LULUCF") %>% mutate(year = as.character(year)) %>%
    mutate(CDR = GtCO2/Mha * 1000) %>% filter(CDR < 0, GtCO2 <0) %>%
    group_by(year) %>% summarise(mean = mean(CDR))


  GCAMResults %>% filter(sector == "Land-based BECCS") %>% mutate(year = as.character(year)) %>%
    mutate(BECCSCDR = GtCO2/Mha * 1000) %>%
    group_by(year) %>% summarise(mean = mean(BECCSCDR))


  #PALPHA = 0.6
  PALPHA0 = PALPHA;  PSIZE0 = PSIZE
  ColorViridis4 <- viridis::viridis(11, alpha = PALPHA0)[c(1,4,7,10)]


  TEXT_ANGLE <- 315

  # themeblank----
  theme(axis.title = element_blank(), axis.text = element_blank(), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), panel.border = element_blank()) ->
    themeblank


  # Fig. 1 AFOLU_vs_BECCS_vs_EIP
  # Get data ready ----


  ## Emissions ----
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

  # Land-based BECCS share ----
  INPUT_DF %>% filter(Var %in% c("Biomass Modern", "Biomass EnergyCrop", "Biomass Modern CCS",
                                 "Biomass Modern noCCS")) %>%
    filter(year >= 2020) %>%
    group_by_at(vars(-value, -year)) %>%
    mutate(value = cumsum(value)) %>% ungroup() %>%
    filter(year %in% c(2050, 2100)) %>%
    select(-Variable) %>% spread(Var, value) %>%
    filter(!is.na(`Biomass Modern`), !is.na(`Biomass EnergyCrop`)) %>% #n = 322
    mutate(LandBasedBECCSShare = `Biomass EnergyCrop` / `Biomass Modern`,
           BiomassNonLand = `Biomass Modern` - `Biomass EnergyCrop`) %>%
    left_join(EM_AFOLU_BECCS %>%
                filter(Var == "Carbon budget") %>%
                select(Model,Scenario, year, EM_CO2_2100, EM_CO2_2100_cut),
              by = c("Model", "Scenario", "year") )->
    PE_Biomass

  # Land use change ----

  # Note that many models have energy cropland already in 2020!
    #  so only do delta for forest !!!!!!!!!!!
  INPUT_DF %>% filter(Var %in% c("Forest", "Cropland_Energy")) %>%
    filter(year >= 2019) %>%
    group_by_at(vars(-value, -year)) %>%
    # only do delta for forest !!!!!!!!!!! ----
    mutate(value = if_else(Var == "Forest", value - first(value), value) ) %>% filter(year != 2019) %>%
    #mutate(value =  value - first(value)) %>% filter(year != 2019) %>%
    ungroup() -> land_TS0

  land_TS0 %>% select(-Variable) %>%
    spread(Var, value) %>% mutate(For_En = Forest + Cropland_Energy) %>%
    filter(!is.na(For_En)) %>%
    gather(Var, value, c("Forest", "For_En", "Cropland_Energy")) %>%
    left_join(EM_AFOLU_BECCS %>%
                filter(Var == "Carbon budget") %>%
                select(Model,Scenario, year, EM_CO2_2100, EM_CO2_2100_cut),
              by = c("Model", "Scenario", "year")) %>%
    mutate(Var = factor(Var, levels = c("For_En", "Cropland_Energy", "Forest"),
                        labels = c("Energy crop & Forest", "Energy cropland",
                                   "Forest")))-> land_TS


  land_TS %>% filter(year <= 2100) %>%
    #filter(Var %in% c("Energy cropland", "Forest")) %>%
    spread(Var, value) %>%
    group_by(Pathway) %>%
    mutate(Forest = cumsum(Forest)/80) %>%
    mutate(`Energy cropland` = cumsum(`Energy cropland`)/81) %>%
    ungroup() %>% filter(year == 2100) %>%
    bind_rows(
      land_TS %>% filter(year <= 2050) %>%
        #filter(Var %in% c("Energy cropland", "Forest")) %>%
        spread(Var, value) %>%
        group_by(Pathway) %>%
        mutate(Forest = cumsum(Forest)/30) %>%
        mutate(`Energy cropland` = cumsum(`Energy cropland`)/31) %>%
        ungroup() %>% filter(year == 2050)
    ) -> LandUse_For_Bio_cum



  # Get data ready forest

  EM_AFOLU_BECCS %>% spread(Var, value) %>%
    filter(!is.na(BECCS)) %>% filter(year == 2100) %>%
    mutate(AFOLU = AFOLU /81, BECCS = BECCS / 81, `Carbon budget` = `Carbon budget`/81) %>%
    bind_rows(
      EM_AFOLU_BECCS %>% spread(Var, value) %>%
        filter(!is.na(BECCS)) %>% filter(year == 2050) %>%
        mutate(AFOLU = AFOLU /31, BECCS = BECCS / 31, `Carbon budget` = `Carbon budget`/31)
    ) %>%
    select(Pathway, year, `Carbon budget`, BECCS, AFOLU) %>%
    left_join(LandUse_For_Bio_cum) %>% filter(`Energy cropland` >=0) %>%
    filter(!is.na(Forest), `Energy cropland` >=0) %>%
    mutate(LULUCFCDR = AFOLU/Forest * 1000)  ->
    LandUse_EMs


  ColorViridis4 <- viridis::viridis(11, alpha = PALPHA0)[c(1,4,7,10)]
  x_title0 <- "LULUCFCDR"
  df1 <- LandUse_EMs %>%  filter(LULUCFCDR <0, AFOLU<0) %>%
    group_by(year, EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(x_title0), probs = 0.05), n = n(),
              sd25 = quantile(get(x_title0), probs = 0.25),
              sd05 = quantile(get(x_title0), probs = 0.5),
              sd75 = quantile(get(x_title0), probs = 0.75),
              sd100 = quantile(get(x_title0), probs = .95)) %>% ungroup()%>%
    mutate(year = as.character(year))

  df2 <- LandUse_EMs %>%  filter(LULUCFCDR <0, AFOLU<0) %>%
    group_by(year) %>%
    summarise(sd05 = quantile(get(x_title0), probs = 0.5), n = n()) %>%
    mutate(year = as.character(year))

  ggplot(data = df1 %>% mutate(year = as.character(year)), aes(x = year)) +

    guides(shape =guide_legend( order = 1),
           linetype = guide_legend( order = 2),
           color = guide_legend(order = 3))+
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = interaction(EM_CO2_2100_cut, year)),
                 alpha= .7,  lwd=0.5, fatten = 1, stat = "identity", width = 0.8,
                 position=position_dodge(width= 0.9)) +
    geom_errorbar(data = df2, aes(ymin = sd05, ymax = sd05), width = 1, color = "blue", linetype = 2) +

    geom_point(data = GCAMResults %>% filter(sector == "LULUCF") %>% mutate(year = as.character(year)) %>%
                 mutate(CDR = GtCO2/Mha * 1000) %>% filter(CDR < 0, GtCO2 <0) %>%
                 #bind a No-LCP value (dummy) for legend
               bind_rows(
                 GCAMResults %>% filter(sector == "LULUCF") %>% mutate(year = as.character(year)) %>%
                   mutate(CDR = GtCO2/Mha * 1000) %>% filter(LCT == "No-LCP") %>% mutate(CDR = 1)
               ),
               aes(x = year, y = CDR, shape = LandSupply, color = LCT),
               alpha = 0.95, size = 4, position=position_dodge(width= -.5)) +

    geom_text(data = df1,
              aes(x = year, y = -26.5, label = n, group = interaction(EM_CO2_2100_cut, year)),
              hjust = 0.5, size = 4, color = "blue", fontface = 4, position=position_dodge(width= .9) ) +

    geom_errorbar(aes(ymin = -27, ymax = -27), width = 0.8, color = "blue", linetype = 1) +

    geom_text(data = df2,
              aes(x = year, y = -27.5, label = paste0("n = ", n) ),
              hjust = 0.5, size = 4, color = "blue", fontface = 4 ) +

    scale_y_continuous(expand = c(0,0), limits = c(-28, 0), breaks = seq(-25, 0, 5))+
    scale_x_discrete(labels = c("2020 - 2050", "2020 - 2100")) +
    theme_bw() + theme0 +
    scale_color_npg()+
    labs(shape = "Policy scenario \n(This study)", color = "LCP scenario \n(This study)")+
    scale_shape_manual(values = c(19, 15, 17, 18)) +
    scale_fill_manual(values = c(ColorViridis4)) +
    labs(x = "Study period", y = expression(paste(tCO[2]," ", ha^-1," ", yr^-1)) ,
         fill = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) -> p;p


  p + labs(title = "(B) LULUCF vs. Forest Land") +
    theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none") ->
    B


  # Get data ready for land-based BECCS vs. biomass land
  PE_Biomass %>% select(Pathway, year, LandBasedBECCSShare) %>%
    left_join(LandUse_EMs, by = c("Pathway", "year")) %>%
    filter( !is.na(BECCS)) %>%
    transmute(Pathway, Model, year, EM_CO2_2100_cut, EM_CO2_2100,
              BECCS_land = BECCS * LandBasedBECCSShare, `Energy cropland`,
              BECCS_NonLand = BECCS * (1-LandBasedBECCSShare),
              BECCSCDR = BECCS_land/`Energy cropland` * 1000,
              BECCSCDRALL = (BECCS_land+ BECCS_NonLand)/`Energy cropland` * 1000,

              ALLCDRALL = (BECCS_land+ BECCS_NonLand + AFOLU)/ (Forest +`Energy cropland`) * 1000
              ) ->
    LandUse_EMs_BECCS




  x_title0 <- "BECCSCDR"
  df1 <- LandUse_EMs_BECCS %>% #filter(BECCSCDR <0) %>%
    group_by(year, EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(x_title0), probs = 0.05), n = n(),
              sd25 = quantile(get(x_title0), probs = 0.25),
              sd05 = quantile(get(x_title0), probs = 0.5),
              sd75 = quantile(get(x_title0), probs = 0.75),
              sd100 = quantile(get(x_title0), probs = .95)) %>% ungroup() %>%
    mutate(year = as.character(year))

  df2 <- LandUse_EMs_BECCS %>% #filter(BECCSCDR <0) %>%
    group_by(year) %>%
    summarise(sd05 = quantile(get(x_title0), probs = 0.5), n = n()) %>%
    mutate(year = as.character(year))

  ggplot(data = df1 %>% mutate(year = as.character(year)) ,
         aes(x = year)) +
    guides(shape =guide_legend( order = 1),
           linetype = guide_legend( order = 2),
           color = guide_legend(order = 3))+
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = interaction(EM_CO2_2100_cut, year)),
                 alpha= .7,  lwd=0.5, fatten = 1, stat = "identity", width = 0.8,
                 position=position_dodge(width= 0.9)) +
    geom_errorbar(data = df2, aes(ymin = sd05, ymax = sd05), width = 1, color = "blue", linetype = 2) +

    geom_point(data = GCAMResults %>% filter(sector == "Land-based BECCS") %>% mutate(year = as.character(year)) %>%
                 mutate(BECCSCDR = GtCO2/Mha * 1000),
               aes(x = year, y = BECCSCDR, shape = LandSupply, color = LCT),
               alpha = 0.95, size = 4, position=position_dodge(width= -.5)) +

    geom_text(data = df1,
              aes(x = year, y = -26.5, label = n, group = interaction(EM_CO2_2100_cut, year)),
              hjust = 0.5, size = 4, color = "blue", fontface = 4, position=position_dodge(width= .9) ) +

    geom_errorbar(aes(ymin = -27, ymax = -27), width = 0.8, color = "blue", linetype = 1) +

    geom_text(data = df2,
              aes(x = year, y = -27.5, label = paste0("n = ", n) ),
              hjust = 0.5, size = 4, color = "blue", fontface = 4 ) +

    scale_y_continuous(expand = c(0,0), limits = c(-28, 0), breaks = seq(-25, 0, 5))+
    scale_x_discrete(labels = c("2020 - 2050", "2020 - 2100")) +
    theme_bw() + theme0 +
    scale_color_npg()+
    labs(shape = "Policy scenario \n(This study)", color = "LCP scenario \n(This study)")+
    scale_shape_manual(values = c(19, 15, 17, 18)) +
    scale_fill_manual(values = c(ColorViridis4)) +
    labs(x = "Study period", y = expression(paste(tCO[2]," ", ha^-1," ", yr^-1)) ,
         fill = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) -> p;p


  p + labs(title = "(A) Land-based BECCS vs. Purpose-grown") + theme(plot.title = element_text(hjust = 0, face = "bold")) ->
    A

A + B +
  plot_layout(guides = 'collect') -> pp; pp


pp %>% Write_png(paste0(OutFolderName, "/MainFig5_LandCDREfficiency_AR6_GCAM"), h = 2000, w = 4400, r = 300)

# **********Updates ------------------




LandUse_EMs_BECCS %>%
  inner_join(
    PE_Biomass %>% filter(!is.na(`Biomass Modern CCS`), !is.na(`Biomass Modern noCCS`)) %>%
      select(Pathway, year, `Biomass Modern CCS`, `Biomass Modern noCCS`), by = c("Pathway", "year")
  ) %>% mutate(CCS_sector_share = `Biomass Modern CCS` / (`Biomass Modern CCS` + `Biomass Modern noCCS`),
               CCS_sector_share = pmin(1, CCS_sector_share)) %>%
  filter(CCS_sector_share > 0) %>%
  mutate(`Energy cropland` = `Energy cropland` * CCS_sector_share) %>%
  mutate(BECCSCDR = BECCS_land/`Energy cropland` * 1000) -> LandUse_EMs_BECCS1

#LandUse_EMs_BECCS1 %>% filter(year == 2100) -> A

x_title0 <- "BECCSCDR"
df1 <- LandUse_EMs_BECCS1 %>% #filter(BECCSCDR <0) %>%
  group_by(year, EM_CO2_2100_cut) %>%
  summarise(sd0 = quantile(get(x_title0), probs = 0.05), n = n(),
            sd25 = quantile(get(x_title0), probs = 0.25),
            sd05 = quantile(get(x_title0), probs = 0.5),
            sd75 = quantile(get(x_title0), probs = 0.75),
            sd100 = quantile(get(x_title0), probs = .95)) %>% ungroup() %>%
  mutate(year = as.character(year))

df2 <- LandUse_EMs_BECCS1 %>% #filter(BECCSCDR <0) %>%
  group_by(year) %>%
  summarise(sd05 = quantile(get(x_title0), probs = 0.5), n = n()) %>%
  mutate(year = as.character(year))

ggplot(data = df1 %>% mutate(year = as.character(year)) ,
       aes(x = year)) +
  guides(shape =guide_legend( order = 1),
         linetype = guide_legend( order = 2),
         color = guide_legend(order = 3))+
  geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                   ymax = sd100, fill =  EM_CO2_2100_cut, group = interaction(EM_CO2_2100_cut, year)),
               alpha= .7,  lwd=0.5, fatten = 1, stat = "identity", width = 0.8,
               position=position_dodge(width= 0.9)) +
  geom_errorbar(data = df2, aes(ymin = sd05, ymax = sd05), width = 1, color = "blue", linetype = 2) +

  geom_text(data = df1,
            aes(x = year, y = -79, label = n, group = interaction(EM_CO2_2100_cut, year)),
            hjust = 0.5, size = 4, color = "blue", fontface = 4, position=position_dodge(width= .9) ) +

  geom_errorbar(aes(ymin = -80, ymax = -80), width = 0.8, color = "blue", linetype = 1) +

  geom_text(data = df2,
            aes(x = year, y = -81, label = paste0("n = ", n) ),
            hjust = 0.5, size = 4, color = "blue", fontface = 4 ) +

  #scale_y_continuous(expand = c(0,0), limits = c(-28, 0), breaks = seq(-25, 0, 5))+
  scale_x_discrete(labels = c("2020 - 2050", "2020 - 2100")) +
  theme_bw() + theme0 +
  scale_color_npg()+
  labs(shape = "Policy scenario \n(This study)", color = "LCP scenario \n(This study)")+
  scale_shape_manual(values = c(19, 15, 17, 18)) +
  scale_fill_manual(values = c(ColorViridis4)) +
  labs(x = "Study period", y = expression(paste(tCO[2]," ", ha^-1," ", yr^-1)) ,
       fill = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) -> p;p

p + labs(title = "(A) Land-based BECCS vs. Purpose-grown (CCS)") + theme(plot.title = element_text(hjust = 0, face = "bold")) ->
  pp

pp %>% Write_png(paste0(SIOutFolderName, "/SI_MainFig5_LandCDREfficiency_AR6_CCSLand"), h = 2000, w = 2400, r = 300)

# 12/14/2023 udpate Fig. 5----


# Check time series
c("Biomass Modern CCS",
  "Biomass Modern noCCS",
  "Biomass Residues") -> VarBioEnergy

INPUT_DF %>%
  filter(Var %in% VarBioEnergy) %>%
  select(-Unit, -Element, -Variable) %>%
  group_by_at(vars(-year, -value)) %>%
  mutate(value = cumsum(value)) %>%
  spread(Var, value) %>%
  filter(!is.na(`Biomass Modern noCCS`)) %>%
  group_by(Pathway) %>%
  filter(min(`Biomass Modern noCCS`) > 0) %>% ungroup() %>%
  filter(year %in% seq(2020, 2100, 5)) %>%
  mutate(ModernBiomassCCSRate = `Biomass Modern CCS`/(`Biomass Modern noCCS` + `Biomass Modern CCS`)) %>%
  mutate(ModernBiomassResidueRate = `Biomass Residues`/(`Biomass Modern noCCS` + `Biomass Modern CCS`)) %>%
  mutate(`Modern biomass noResidue` = `Biomass Modern noCCS` + `Biomass Modern CCS` -  `Biomass Residues`) ->
  Biomass

Biomass %>%
  filter(!is.na(ModernBiomassCCSRate)) %>%
  ggplot()+ #facet_wrap(~Category) +
  geom_line(aes(x = year, y = ModernBiomassCCSRate, group = Pathway, color = Model)) +
  labs(x = "Year", y = "Cumulative CCS Share in modern biomass") +
  theme_bw() + theme0 -> p;p

p %>% Write_png(paste0(SIOutFolderName, "/SI_MainFig5_LandCDREfficiency_AR6_CCSSectorShare"), h = 2000, w = 3400, r = 300)


Biomass %>%
  filter(!is.na(ModernBiomassCCSRate)) %>% inner_join(LandUse_EMs_BECCS1 %>% distinct(Pathway)) %>%
  ggplot()+ #facet_wrap(~Category) +
  geom_line(aes(x = year, y = ModernBiomassCCSRate, group = Pathway, color = Model)) +
  labs(x = "Year", y = "Cumulative CCS Share in modern biomass") +
  theme_bw() + theme0 -> p;p

p %>% Write_png(paste0(SIOutFolderName, "/SI_MainFig5_LandCDREfficiency_AR6_CCSSectorShare_subset"), h = 2000, w = 2400, r = 300)








## Done ------------------------------



x_title0 <- "BECCS_NonLand"
df1 <- LandUse_EMs_BECCS %>%
  group_by(year, EM_CO2_2100_cut) %>%
  summarise(sd0 = quantile(get(x_title0), probs = 0.05), n = n(),
            sd25 = quantile(get(x_title0), probs = 0.25),
            sd05 = quantile(get(x_title0), probs = 0.5),
            sd75 = quantile(get(x_title0), probs = 0.75),
            sd100 = quantile(get(x_title0), probs = .95)) %>% ungroup() %>%
  mutate(year = as.character(year))
df2 <- LandUse_EMs_BECCS %>%
  group_by(year) %>%
  summarise(sd05 = quantile(get(x_title0), probs = 0.5), n = n()) %>%
  mutate(year = as.character(year))

ggplot(data = df1  ,
       aes(x = year)) +
  guides(shape =guide_legend( order = 1),
         linetype = guide_legend( order = 2),
         color = guide_legend(order = 3))+
  geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                   ymax = sd100, fill =  EM_CO2_2100_cut, group = interaction(EM_CO2_2100_cut, year)),
               alpha= 0.7,  lwd=0.5, fatten = 1, stat = "identity", width = 0.8,
               position=position_dodge(width= 0.9)) +
  geom_errorbar(data = df2, aes(ymin = sd05, ymax = sd05), width = 1, color = "blue", linetype = 2) +
  geom_point(data = GCAMResults %>% filter(sector == "NonLand-based BECCS") %>% mutate(year = as.character(year)) ,
             aes(x = year, y = GtCO2, shape = LandSupply, color = LCT),
             alpha = 0.95, size = 4, position=position_dodge(width= -.5)) +

  geom_text(data = df1,
            aes(x = year, y = -7, label = n, group = interaction(EM_CO2_2100_cut, year)),
            hjust = 0.5, size = 4, color = "blue", fontface = 4, position=position_dodge(width= .9) ) +

  geom_errorbar(aes(ymin = -7.15, ymax = -27), width = 0.8, color = "blue", linetype = 1) +

  geom_text(data = df2,
            aes(x = year, y = -7.3, label = paste0("n = ", n) ),
            hjust = 0.5, size = 4, color = "blue", fontface = 4 ) +

  scale_y_continuous(expand = c(0,0), limits = c(-7.4, 0))+
  scale_color_npg()+
  labs(shape = "Policy scenario \n(This study)", color = "LCP scenario \n(This study)")+
  scale_shape_manual(values = c(19, 15, 17, 18)) +
  scale_x_discrete(labels = c("2020 - 2050", "2020 - 2100")) +
  theme_bw() + theme0 +
  scale_fill_manual(values = c(ColorViridis4)) +
  labs(x = "Study period", y = expression(paste(GtCO[2]," ", yr^-1)),
       fill = expression(paste("AR6 CB ", "(",GtCO[2],")")) )-> p;p


p + labs(title = "(B) Nonland-based BECCS") +
  theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none") ->
  B



x_title0 <- "BECCSCDRALL"
#x_title0 <- "ALLCDRALL"
df1 <- LandUse_EMs_BECCS %>% filter(BECCSCDRALL >-60) %>%
  group_by(year, EM_CO2_2100_cut) %>%
  summarise(sd0 = quantile(get(x_title0), probs = 0.05), n = n(),
            sd25 = quantile(get(x_title0), probs = 0.25),
            sd05 = quantile(get(x_title0), probs = 0.5),
            sd75 = quantile(get(x_title0), probs = 0.75),
            sd100 = quantile(get(x_title0), probs = .95)) %>% ungroup() %>%
  mutate(year = as.character(year))

df2 <- LandUse_EMs_BECCS %>% filter(BECCSCDRALL >-60) %>%
  group_by(year) %>%
  summarise(sd05 = quantile(get(x_title0), probs = 0.5), n = n()) %>%
  mutate(year = as.character(year))

ggplot(data = df1 %>% mutate(year = as.character(year)) ,
       aes(x = year)) +
  guides(shape =guide_legend( order = 1),
         linetype = guide_legend( order = 2),
         color = guide_legend(order = 3))+
  geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                   ymax = sd100, fill =  EM_CO2_2100_cut, group = interaction(EM_CO2_2100_cut, year)),
               alpha= .7,  lwd=0.5, fatten = 1, stat = "identity", width = 0.8,
               position=position_dodge(width= 0.9)) +
  geom_errorbar(data = df2, aes(ymin = sd05, ymax = sd05), width = 1, color = "blue", linetype = 2) +

  geom_point(data = GCAMResults %>%
               filter(sector %in% c("NonLand-based BECCS", "Land-based BECCS") ) %>%
               mutate(year = as.character(year)) %>%
               group_by_at(vars(-sector, -Mha, -GtCO2)) %>%
               summarise(Mha = sum(Mha), GtCO2 = sum(GtCO2)) %>%
               mutate(BECCSCDR = GtCO2/Mha * 1000),
             aes(x = year, y = BECCSCDR, shape = LandSupply, color = LCT),
             alpha = 0.95, size = 4, position=position_dodge(width= -.5)) +

  geom_text(data = df1,
            aes(x = year, y = -55.5, label = n, group = interaction(EM_CO2_2100_cut, year)),
            hjust = 0.5, size = 4, color = "blue", fontface = 4, position=position_dodge(width= .9) ) +

  geom_errorbar(aes(ymin = -57, ymax = -57), width = 0.8, color = "blue", linetype = 1) +

  geom_text(data = df2,
            aes(x = year, y = -58.5, label = paste0("n = ", n) ),
            hjust = 0.5, size = 4, color = "blue", fontface = 4 ) +

  #scale_y_continuous(expand = c(0,0), limits = c(-28, 0), breaks = seq(-25, 0, 5))+
  scale_x_discrete(labels = c("2020 - 2050", "2020 - 2100")) +
  theme_bw() + theme0 +
  scale_color_npg()+
  labs(shape = "Policy scenario \n(This study)", color = "LCP scenario \n(This study)")+
  scale_shape_manual(values = c(19, 15, 17, 18)) +
  scale_fill_manual(values = c(ColorViridis4)) +
  labs(x = "Study period", y = expression(paste(tCO[2]," ", ha^-1," ", yr^-1)) ,
       fill = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) -> p;p


p + labs(title = "(A) All BECCS") +
  theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "right") ->
  A

A + B + plot_layout(guides = 'collect') -> pp; pp

pp %>% Write_png(paste0(SIOutFolderName, "/SIFig_LandCDREfficiency_AR6_GCAM"), h = 2000, w = 4400, r = 300)







  # Fig1a BECCS vs. biomass land----

  LandUse_EMs_BECCS %>% filter(year == 2100) %>%
    mutate(BECCS_land = 0, `Energy cropland` = 0, year = 2020) %>%
    bind_rows(LandUse_EMs_BECCS) ->  y_x_data_path

  LandUse_EMs_BECCS %>% filter(year == 2100) %>%
    mutate(year = as.character(year)) -> y_x_data

  y_x_data %>%
    group_by(year) %>%
    summarise(sd50 = median(BECCSCDR)) %>%
    pull(sd50) -> medvalue

  #x_title = "Energy cropland"; y_title = "BECCS_land"
  x_title0 = "Energy cropland"; y_title0 = "BECCS_land"


  XMAX = 900
  YLIMITs <- c(-9, 0.05)
  XBREAKs_left <- c(0, 200, 400)
  YBREAKs <- c(0, 200, 400, 600, 800)


  ggplot() +
    guides(#fill =guide_legend( order = 1),
      shape =guide_legend( order = 2),
      #linetype = guide_legend( order = 2),
      color = guide_legend(order = 3))+

    geom_abline(slope = -5.68 /1000, intercept = 0, linetype = 2, color = "grey") +
    geom_abline(slope = -9.49063 /1000, intercept = 0, linetype = 2, color = "red") +

    #scale_x_continuous(expand = c(0,0), limits = c(0, XMAX), breaks = XBREAKs_left) +
    #scale_y_continuous(expand = c(0,0), limits = YLIMITs, breaks = YBREAKs) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, XMAX)) +
    scale_y_continuous(expand = c(0, 0), limits = YLIMITs) +

    geom_point(data = y_x_data %>% filter(EM_CO2_2100_cut %in% CutRange),
               aes(y = get(y_title0), x = get(x_title0), group = Pathway,
                   fill = EM_CO2_2100), color = "black", alpha= PALPHA0, size = PSIZE0, shape = 22) +

    binned_scale(aesthetics = "fill",
                 scale_name = "stepsn",
                 palette = function(x) c(ColorViridis4),
                 breaks = c(500, 825, 1150),
                 limits = c(175, 1475),
                 show.limits = TRUE,
                 guide = "colorsteps",
                 name = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) +

    labs(x = "Mha",
         y = expression(paste(GtCO[2], " ", yr^-1)) ) +
    theme_bw()+ theme0 +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.width = unit(0.8,"cm"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5, b = 0)),
          axis.text.y.right = element_text(hjust = 0.25, margin = margin(r = 10, l = 5))) +
    theme(panel.spacing = unit(1.5, "lines"),
          plot.margin = margin(l = 8, t = 5)) -> A1;A1


  # add GCAM ----

    A1 +
      geom_point(data = GCAMResults %>% mutate(DB = "GCAM") %>% filter(year == 2100) %>%
                   filter(sector == "Land-based BECCS"),
                 aes(y = GtCO2, x = Mha,  group = interaction(scenario, target),
                     shape = LandSupply, color = LCT), alpha = 0.8, size = 4) +
      #scale_shape_manual(values = c(1, 0, 2, 10)) +
      #scale_color_manual(values = mycol)+
      scale_color_npg()+
      labs(shape = "Policy scenario \n(This study)", color = "LCP scenario \n(This study)")+
      scale_shape_manual(values = c(19, 15, 17, 18)) -> A1;A1

    cowplot:: get_legend(A1) -> legendA1

    cowplot::ggdraw(legendA1)  %>%
      Write_png(paste0(SIOutFolderName, "/LandCDREfficiency/SIFig_efficiency_legend"), h = 4300, w = 1300, r = 600)





  df1 <- y_x_data %>%
    group_by(EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(x_title0), probs = 0.05),
              sd25 = quantile(get(x_title0), probs = 0.25),
              sd05 = quantile(get(x_title0), probs = 0.5),
              sd75 = quantile(get(x_title0), probs = 0.75),
              sd100 = quantile(get(x_title0), probs = .95)) %>% ungroup()
  ggplot(data = df1,
         aes(x = EM_CO2_2100_cut)) +
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = EM_CO2_2100_cut), alpha= PALPHA0,  lwd=0.5, fatten = 1, stat = "identity") +
    scale_y_continuous(expand = c(0,0), limits = c(0, XMAX))+
    theme_bw() +
    scale_fill_manual(values = c(ColorViridis4)) +
    themeblank +
    theme(plot.margin = margin(b = 0)) +
    coord_flip() -> ptop

  df1 <- y_x_data %>%
    group_by(EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(y_title0), probs = 0.05),
              sd25 = quantile(get(y_title0), probs = 0.25),
              sd05 = quantile(get(y_title0), probs = 0.5),
              sd75 = quantile(get(y_title0), probs = 0.75),
              sd100 = quantile(get(y_title0), probs = .95)) %>% ungroup()

  ggplot(df1, aes(x = EM_CO2_2100_cut)) +
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = EM_CO2_2100_cut), alpha= PALPHA0, lwd=0.5, fatten = 1, stat = "identity") +
    scale_y_continuous(expand = c(0,0), limits = YLIMITs)+
    theme_bw() +  scale_fill_manual(values = c(ColorViridis4)) +
    themeblank +
    theme(plot.margin = margin(b = 0, l = 0)) -> pright



  ### combine p----
  ptop + (A1 +theme(plot.margin = margin(t = 0,l = 5, r = 0), legend.position = "none"
  )) + plot_layout(heights = c(0.07, 1)) -> p2;p2

  (p2|(plot_spacer() / pright + plot_layout(heights = c(0.07, 1))) )+
    plot_layout(widths = c(1, 0.1), guides = 'collect') -> ppA; ppA

    ppA %>% Write_png(paste0(SIOutFolderName, "/LandCDREfficiency/SIFig_efficiency_BECCS_AR6_addGCAM_2100"),
                      h = 4300, w = 4300, r = 600)



  # Fig1b LULUCF vs. forest----

  LandUse_EMs %>% filter(year == 2100) %>%
    mutate( AFOLU = 0, Forest = 0, year = 2020) %>%
    bind_rows(LandUse_EMs) %>% arrange(year)->  y_x_data_path

  LandUse_EMs %>% filter(year == 2100) %>%
    filter(AFOLU < 0, Forest >0) %>%
    mutate(year = as.character(year)) -> y_x_data

  y_x_data %>%
    group_by(year) %>%
    summarise(sd50 = median(LULUCFCDR)) -> medvalue


  x_title = "Forest"; y_title = "AFOLU"
  #x_title0 = "Forest"; y_title0 = "AFOLU"


  XMAX = 900
  YLIMITs <- c(-9, 0.05)
  XBREAKs_left <- c(0, 200, 400)
  YBREAKs <- c(0, 200, 400, 600, 800)

  ggplot() +
    guides(#fill =guide_legend( order = 1),
      shape =guide_legend( order = 2),
      #linetype = guide_legend( order = 2),
      color = guide_legend(order = 3))+
    geom_abline(slope = -5.68 /1000, intercept = 0, linetype = 2, color = "blue") +
    geom_abline(slope = -9.49063 /1000, intercept = 0, linetype = 2, color = "grey") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, XMAX)) +
    scale_y_continuous(expand = c(0, 0), limits = YLIMITs) +

    geom_point(data = y_x_data %>% filter(EM_CO2_2100_cut %in% CutRange),
               aes(y = get(y_title), x = get(x_title), group = Pathway,
                   fill = EM_CO2_2100), color = "black", alpha= PALPHA0, size = PSIZE0, shape = 22) +

    binned_scale(aesthetics = "fill",
                 scale_name = "stepsn",
                 palette = function(x) c(ColorViridis4),
                 breaks = c(500, 825, 1150),
                 limits = c(175, 1475),
                 show.limits = TRUE,
                 guide = "colorsteps",
                 name = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) +

    labs(x = "Mha",
         y = expression(paste(GtCO[2], " ", yr^-1)) ) +
    theme_bw()+ theme0 +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.width = unit(0.8,"cm"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5, b = 0)),
          axis.text.y.right = element_text(hjust = 0.25, margin = margin(r = 10, l = 5))) +
    theme(panel.spacing = unit(1.5, "lines"),
          plot.margin = margin(l = 8, t = 5)) -> A1;A1



  # add GCAM ----
    A1 +
      geom_point(data = GCAMResults %>% mutate(DB = "GCAM") %>% filter(year == 2100) %>%
                   filter(sector == "LULUCF") %>% filter(GtCO2 <0) %>%
                   bind_rows(
                     GCAMResults %>% mutate(DB = "GCAM") %>% filter(year == 2100) %>%
                       filter(sector == "LULUCF") %>% filter(LCT == "No-LCP")
                   ),
                 aes(y = GtCO2, x = Mha,  group = interaction(scenario, target),
                     shape = LandSupply, color = LCT), alpha = .8, size = 4) +
      #scale_shape_manual(values = c(1, 0, 2, 10)) +
      scale_color_npg()+
      labs(shape = "Policy scenario \n(This study)", color = "LCP scenario \n(This study)")+
      #scale_color_manual(values = mycol)+
      scale_shape_manual(values = c(19, 15, 17, 18)) -> A1;A1



  df1 <- y_x_data %>%
    group_by(EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(x_title0), probs = 0.05),
              sd25 = quantile(get(x_title0), probs = 0.25),
              sd05 = quantile(get(x_title0), probs = 0.5),
              sd75 = quantile(get(x_title0), probs = 0.75),
              sd100 = quantile(get(x_title0), probs = .95)) %>% ungroup()
  ggplot(data = df1,
         aes(x = EM_CO2_2100_cut)) +
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = EM_CO2_2100_cut),
                 alpha= PALPHA0,  lwd=0.5, fatten = 1, stat = "identity") +
    scale_y_continuous(expand = c(0,0), limits = c(0, XMAX))+
    theme_bw() +
    scale_fill_manual(values = c(ColorViridis4)) +
    themeblank +
    theme(plot.margin = margin(b = 0)) +
    coord_flip() -> ptop

  df1 <- y_x_data %>%
    group_by(EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(y_title), probs = 0.05),
              sd25 = quantile(get(y_title), probs = 0.25),
              sd05 = quantile(get(y_title), probs = 0.5),
              sd75 = quantile(get(y_title), probs = 0.75),
              sd100 = quantile(get(y_title), probs = .95)) %>% ungroup()

  ggplot(df1, aes(x = EM_CO2_2100_cut)) +
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = EM_CO2_2100_cut), alpha= PALPHA0, lwd=0.5, fatten = 1, stat = "identity") +
    scale_y_continuous(expand = c(0,0), limits = YLIMITs)+
    theme_bw() +  scale_fill_manual(values = c(ColorViridis4)) +
    themeblank +
    theme(plot.margin = margin(b = 0, l = 0)) -> pright



  ### combine p----
  ptop + (A1 +theme(plot.margin = margin(t = 0,l = 5, r = 0), legend.position = "none"
  )) + plot_layout(heights = c(0.07, 1)) -> p2;p2

  (p2|(plot_spacer() / pright + plot_layout(heights = c(0.07, 1))) )+
    plot_layout(widths = c(1, 0.1), guides = 'collect') -> ppB; ppB

    ppB %>% Write_png(paste0(SIOutFolderName, "/LandCDREfficiency/SIFig_efficiency_For_AR6_addGCAM_2100"),
                      h = 4300, w = 4300, r = 600)



#--------------
  # 2050 ----


  LandUse_EMs_BECCS %>% filter(year == 2050) %>%
    mutate(BECCS_land = 0, `Energy cropland` = 0, year = 2020) %>%
    bind_rows(LandUse_EMs_BECCS) ->  y_x_data_path

  LandUse_EMs_BECCS %>% filter(year == 2050) %>%
    mutate(year = as.character(year)) -> y_x_data

  y_x_data %>%
    group_by(year) %>%
    summarise(sd50 = median(BECCSCDR)) %>%
    pull(sd50) -> medvalue

  #x_title = "Energy cropland"; y_title = "BECCS_land"
  x_title0 = "Energy cropland"; y_title0 = "BECCS_land"


  XMAX = 900
  YLIMITs <- c(-9, 0.05)
  XBREAKs_left <- c(0, 200, 400)
  YBREAKs <- c(0, 200, 400, 600, 800)


  ggplot() +
    guides(#fill =guide_legend( order = 1),
      shape =guide_legend( order = 2),
      #linetype = guide_legend( order = 2),
      color = guide_legend(order = 3))+

    geom_abline(slope =  -3.93 /1000, intercept = 0, linetype = 2, color = "grey") +
    geom_abline(slope = -1.508632 /1000, intercept = 0, linetype = 2, color = "red") +

    scale_x_continuous(expand = c(0, 0), limits = c(0, XMAX)) +
    scale_y_continuous(expand = c(0, 0), limits = YLIMITs) +

    geom_point(data = y_x_data %>% filter(EM_CO2_2100_cut %in% CutRange),
               aes(y = get(y_title0), x = get(x_title0), group = Pathway,
                   fill = EM_CO2_2100), color = "black", alpha= PALPHA0, size = PSIZE0, shape = 22) +

    binned_scale(aesthetics = "fill",
                 scale_name = "stepsn",
                 palette = function(x) c(ColorViridis4),
                 breaks = c(500, 825, 1150),
                 limits = c(175, 1475),
                 show.limits = TRUE,
                 guide = "colorsteps",
                 name = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) +

    labs(x = "Mha",
         y = expression(paste(GtCO[2], " ", yr^-1)) ) +
    theme_bw()+ theme0 +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.width = unit(0.8,"cm"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5, b = 0)),
          axis.text.y.right = element_text(hjust = 0.25, margin = margin(r = 10, l = 5))) +
    theme(panel.spacing = unit(1.5, "lines"),
          plot.margin = margin(l = 8, t = 5)) -> A1;A1


  # add GCAM ----

    A1 +
      geom_point(data = GCAMResults %>% mutate(DB = "GCAM") %>% filter(year == 2050) %>%
                   filter(sector == "Land-based BECCS"),
                 aes(y = GtCO2, x = Mha,  group = interaction(scenario, target),
                     shape = LandSupply, color = LCT), alpha = 0.8, size = 4) +
      #scale_shape_manual(values = c(1, 0, 2, 10)) +
      #scale_color_manual(values = mycol)+
      scale_color_npg()+
      labs(shape = "Policy scenario \n(This study)", color = "LCP scenario \n(This study)")+
      scale_shape_manual(values = c(19, 15, 17, 18)) -> A1;A1

    cowplot:: get_legend(A1) -> legendA1

    # cowplot::ggdraw(legendA1)  %>% Write_png(paste0(SIOutFolderName, "/SIFig_efficiency_AR6_legend"),
    #                   h = 4300, w = 1300, r = 600)



  df1 <- y_x_data %>%
    group_by(EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(x_title0), probs = 0.05),
              sd25 = quantile(get(x_title0), probs = 0.25),
              sd05 = quantile(get(x_title0), probs = 0.5),
              sd75 = quantile(get(x_title0), probs = 0.75),
              sd100 = quantile(get(x_title0), probs = .95)) %>% ungroup()
  ggplot(data = df1,
         aes(x = EM_CO2_2100_cut)) +
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = EM_CO2_2100_cut), alpha= PALPHA0,  lwd=0.5, fatten = 1, stat = "identity") +
    scale_y_continuous(expand = c(0,0), limits = c(0, XMAX))+
    theme_bw() +
    scale_fill_manual(values = c(ColorViridis4)) +
    themeblank +
    theme(plot.margin = margin(b = 0)) +
    coord_flip() -> ptop

  df1 <- y_x_data %>%
    group_by(EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(y_title0), probs = 0.05),
              sd25 = quantile(get(y_title0), probs = 0.25),
              sd05 = quantile(get(y_title0), probs = 0.5),
              sd75 = quantile(get(y_title0), probs = 0.75),
              sd100 = quantile(get(y_title0), probs = .95)) %>% ungroup()

  ggplot(df1, aes(x = EM_CO2_2100_cut)) +
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = EM_CO2_2100_cut), alpha= PALPHA0, lwd=0.5, fatten = 1, stat = "identity") +
    scale_y_continuous(expand = c(0,0), limits = YLIMITs)+
    theme_bw() +  scale_fill_manual(values = c(ColorViridis4)) +
    themeblank +
    theme(plot.margin = margin(b = 0, l = 0)) -> pright



  ### combine p----
  ptop + (A1 +theme(plot.margin = margin(t = 0,l = 5, r = 0), legend.position = "none"
  )) + plot_layout(heights = c(0.07, 1)) -> p2;p2

  (p2|(plot_spacer() / pright + plot_layout(heights = c(0.07, 1))) )+
    plot_layout(widths = c(1, 0.1), guides = 'collect') -> ppA; ppA

    ppA %>% Write_png(paste0(SIOutFolderName, "/LandCDREfficiency/SIFig_efficiency_BECCS_AR6_addGCAM_2050"),
                      h = 4300, w = 4300, r = 600)



  # Fig1b LULUCF vs. forest----

  LandUse_EMs %>% filter(year == 2050) %>%
    mutate( AFOLU = 0, Forest = 0, year = 2020) %>%
    bind_rows(LandUse_EMs) %>% arrange(year)->  y_x_data_path

  LandUse_EMs %>% filter(year == 2050) %>%
    filter(AFOLU < 0, Forest >0) %>%
    mutate(year = as.character(year)) -> y_x_data

  y_x_data %>%
    group_by(year) %>%
    summarise(sd50 = median(LULUCFCDR)) -> medvalue


  x_title = "Forest"; y_title = "AFOLU"
  #x_title0 = "Forest"; y_title0 = "AFOLU"


  XMAX = 900
  YLIMITs <- c(-9, 0.05)
  XBREAKs_left <- c(0, 200, 400)
  YBREAKs <- c(0, 200, 400, 600, 800)


  ggplot() +
    guides(#fill =guide_legend( order = 1),
      shape =guide_legend( order = 2),
      #linetype = guide_legend( order = 2),
      color = guide_legend(order = 3))+

    geom_abline(slope = -3.93 /1000, intercept = 0, linetype = 2, color = "blue") +
    geom_abline(slope = -1.508632 /1000, intercept = 0, linetype = 2, color = "grey") +

    scale_x_continuous(expand = c(0, 0), limits = c(0, XMAX)) +
    scale_y_continuous(expand = c(0, 0), limits = YLIMITs) +

    geom_point(data = y_x_data %>% filter(EM_CO2_2100_cut %in% CutRange),
               aes(y = get(y_title), x = get(x_title), group = Pathway,
                   fill = EM_CO2_2100), color = "black", alpha= PALPHA0, size = PSIZE0, shape = 22) +

    binned_scale(aesthetics = "fill",
                 scale_name = "stepsn",
                 palette = function(x) c(ColorViridis4),
                 breaks = c(500, 825, 1150),
                 limits = c(175, 1475),
                 show.limits = TRUE,
                 guide = "colorsteps",
                 name = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) +

    labs(x = "Mha",
         y = expression(paste(GtCO[2], " ", yr^-1)) ) +
    theme_bw()+ theme0 +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.width = unit(0.8,"cm"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5, b = 0)),
          axis.text.y.right = element_text(hjust = 0.25, margin = margin(r = 10, l = 5))) +
    theme(panel.spacing = unit(1.5, "lines"),
          plot.margin = margin(l = 8, t = 5)) -> A1;A1



  # add GCAM ----

    A1 +
      geom_point(data = GCAMResults %>% mutate(DB = "GCAM") %>% filter(year == 2050) %>%
                   filter(sector == "LULUCF") %>% filter(GtCO2 <0) %>%
                   bind_rows(
                     GCAMResults %>% mutate(DB = "GCAM") %>% filter(year == 2050) %>%
                       filter(sector == "LULUCF") %>% filter(LCT %in% c("No-LCP", "10%-LCP"))
                   ),
                 aes(y = GtCO2, x = Mha,  group = interaction(scenario, target),
                     shape = LandSupply, color = LCT), alpha = .8, size = 4) +
      #scale_shape_manual(values = c(1, 0, 2, 10)) +
      scale_color_npg()+
      labs(shape = "Policy scenario \n(This study)", color = "LCP scenario \n(This study)")+
      #scale_color_manual(values = mycol)+
      scale_shape_manual(values = c(19, 15, 17, 18)) -> A1;A1





  df1 <- y_x_data %>%
    group_by(EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(x_title0), probs = 0.05),
              sd25 = quantile(get(x_title0), probs = 0.25),
              sd05 = quantile(get(x_title0), probs = 0.5),
              sd75 = quantile(get(x_title0), probs = 0.75),
              sd100 = quantile(get(x_title0), probs = .95)) %>% ungroup()
  ggplot(data = df1,
         aes(x = EM_CO2_2100_cut)) +
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = EM_CO2_2100_cut),
                 alpha= PALPHA0,  lwd=0.5, fatten = 1, stat = "identity") +
    scale_y_continuous(expand = c(0,0), limits = c(0, XMAX))+
    theme_bw() +
    scale_fill_manual(values = c(ColorViridis4)) +
    themeblank +
    theme(plot.margin = margin(b = 0)) +
    coord_flip() -> ptop

  df1 <- y_x_data %>%
    group_by(EM_CO2_2100_cut) %>%
    summarise(sd0 = quantile(get(y_title), probs = 0.05),
              sd25 = quantile(get(y_title), probs = 0.25),
              sd05 = quantile(get(y_title), probs = 0.5),
              sd75 = quantile(get(y_title), probs = 0.75),
              sd100 = quantile(get(y_title), probs = .95)) %>% ungroup()

  ggplot(df1, aes(x = EM_CO2_2100_cut)) +
    geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                     ymax = sd100, fill =  EM_CO2_2100_cut, group = EM_CO2_2100_cut), alpha= PALPHA0, lwd=0.5, fatten = 1, stat = "identity") +
    scale_y_continuous(expand = c(0,0), limits = YLIMITs)+
    theme_bw() +  scale_fill_manual(values = c(ColorViridis4)) +
    themeblank +
    theme(plot.margin = margin(b = 0, l = 0)) -> pright



  ### combine p----
  ptop + (A1 +theme(plot.margin = margin(t = 0,l = 5, r = 0), legend.position = "none"
  )) + plot_layout(heights = c(0.07, 1)) -> p2;p2

  (p2|(plot_spacer() / pright + plot_layout(heights = c(0.07, 1))) )+
    plot_layout(widths = c(1, 0.1), guides = 'collect') -> ppB; ppB


  ppB %>% Write_png(paste0(SIOutFolderName, "/LandCDREfficiency/SIFig_efficiency_For_AR6_addGCAM_2050"),
                    h = 4300, w = 4300, r = 600)



}






