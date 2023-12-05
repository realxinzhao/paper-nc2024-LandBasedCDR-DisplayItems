

MainFig1_LandCDR_AR6GCAM <-
  function(ADD_GCAM = F, PALPHA = 0.8, PSIZE = 1.8){


  outdir <- "output/GCAM/"
  OutFolderName <- "Main"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)


  # Load data ----
  INPUT_DF <- LoadFigData("AR6_604")



  if (ADD_GCAM == T) {
    EMs <- LoadFigData("GCAM_EMs") %>% filter(LandSupply %in% c("A/R-Focused", "2C Main") ) %>%
      filter(!(LandSupply == "2C Main" & LCT != "No-LCP"))

    EMs %>% spread(GHG, value) -> EMS_LUC_FFI_BECCS_GCAM1

    EMS_LUC_FFI_BECCS_GCAM1 %>% filter(year == 2100) %>%
      rename(`Carbon budget` = EMs)-> y_x_data

    x_title = "BECCS"; y_title = "AFOLU"
    LADQ5 <- rq(get(y_title)~ get(x_title) + `Carbon budget`, tau = 0.5, data = y_x_data)
    LADQ9 <- rq(get(y_title)~ get(x_title)+ `Carbon budget`, tau = 0.95, data = y_x_data)
    LADQ1 <- rq(get(y_title)~ get(x_title)+ `Carbon budget`, tau = 0.05, data = y_x_data)
    OLS <- lm(get(y_title)~ get(x_title)+ `Carbon budget`, data = y_x_data)
  }


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
  #CutRange <- CutRange[c(1, 2,  4)]
  #CutRange <- ""

# Fig1a BECCS vs. AFOLU vs. FFI----


## BECCS vs. AFOLU ----

EM_AFOLU_BECCS %>% spread(Var, value) %>%
  filter(!is.na(BECCS)) %>% filter(year %in% c(2050, 2100)) -> EMS_LUC_FFI_BECCS

x_title = "BECCS"; y_title = "AFOLU"
x_title0 = "BECCS"; y_title0 = "AFOLU"
y_x_data <- EMS_LUC_FFI_BECCS %>% filter(year == 2100)

# -0.16 [-0.19, 0.09] substitution
LADQ5 <- rq(get(y_title)~ get(x_title) + `Carbon budget`, tau = 0.5, data = y_x_data)
LADQ9 <- rq(get(y_title)~ get(x_title)+ `Carbon budget`, tau = 0.95, data = y_x_data)
LADQ1 <- rq(get(y_title)~ get(x_title)+ `Carbon budget`, tau = 0.05, data = y_x_data)
OLS <- lm(get(y_title)~ get(x_title)+ `Carbon budget`, data = y_x_data)
# summary(LADQ5, se="ker")
# summary(OLS)
# cor(y_x_data$BECCS, y_x_data$AFOLU)
ggplot() +
  guides(shape =guide_legend( order = 1),
         linetype = guide_legend( order = 2),
         color = guide_legend(order = 3))+

  geom_abline(slope = LADQ1$coefficients[2],intercept = LADQ1$coefficients[1] + LADQ1$coefficients[3] * quantile(y_x_data$`Carbon budget`, probs = 0.05), linetype = 5, color = "blue")+
  geom_abline(slope = LADQ5$coefficients[2],intercept  = LADQ5$coefficients[1]+ LADQ5$coefficients[3] * quantile(y_x_data$`Carbon budget`, probs = 0.5), linetype = 5, color = "blue")+
  geom_abline(slope = LADQ9$coefficients[2],intercept  = LADQ9$coefficients[1] + LADQ9$coefficients[3] * quantile(y_x_data$`Carbon budget`, probs = 0.95), linetype = 5, color = "blue")+

  #geom_abline(slope = OLS$coefficients[2],intercept  = OLS$coefficients[1], linetype = 5, color = "red")+

  geom_hline(yintercept = 0, color = "black") +
  #geom_abline(slope = -1, intercept = -1100) +
  geom_abline(slope = -1, intercept = -1000) +
  geom_abline(slope = -1, intercept = -800) +
  geom_abline(slope = -1, intercept = -600) +
  geom_abline(slope = -1, intercept = -400) +
  geom_abline(slope = -1, intercept = -200) +



  scale_x_continuous(expand = c(0,0), limits = c(-XMAX, 0), breaks = XBREAKs_left) +
  scale_y_continuous(expand = c(0,0), limits = YLIMITs, breaks = c(0, -200, -400, -600, -800)) +

  geom_point(data = y_x_data %>% filter(EM_CO2_2100_cut %in% CutRange),
             aes(y = get(y_title0), x = get(x_title0), group = Pathway,
                 fill = EM_CO2_2100), color = "black", alpha= PALPHA0, size = PSIZE0, shape = 22
  ) +

  binned_scale(aesthetics = "fill",
               scale_name = "stepsn",
               palette = function(x) c(ColorViridis4),
               breaks = c(500, 825, 1150),
               limits = c(175, 1475),
               show.limits = TRUE,
               guide = "colorsteps",
               name = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) +


  scale_shape_manual(values = c(21, 22, 24), name = "This study") +
  #scale_color_npg(name = "UCT (This study)") +
  #scale_linetype_manual(values = c(1, 5), name = "Target (This study)") +

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

  # add GCAM ----
  if (ADD_GCAM == T) {
    LUC_vs_BECCS +
      # geom_path(data = EMS_LUC_FFI_BECCS_GCAM1,
      #           aes(y = AFOLU, x = BECCS, group = interaction(scenario, target),
      #               color = LCT, linetype = target), size = 1) +
      geom_point(data = EMS_LUC_FFI_BECCS_GCAM1 %>% filter(year == 2100) %>% mutate(DB = "GCAM"),
                 aes(y = AFOLU, x = BECCS, group = interaction(scenario, target), shape = DB),
                 color = "black",
                 alpha = 0.8, size = 3, fill = "red") -> LUC_vs_BECCS;LUC_vs_BECCS
  }



df1 <- y_x_data %>%
  group_by(EM_CO2_2100_cut) %>%
  summarise(sd0 = quantile(`BECCS`, probs = 0.05),
            sd25 = quantile(`BECCS`, probs = 0.25),
            sd05 = quantile(`BECCS`, probs = 0.5),
            sd75 = quantile(`BECCS`, probs = 0.75),
            sd100 = quantile(`BECCS`, probs = .95)) %>% ungroup()
ggplot(data = df1,
       aes(x = EM_CO2_2100_cut)) +
  geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                   ymax = sd100, fill =  EM_CO2_2100_cut), alpha= PALPHA0,  lwd=0.5, fatten = 1, stat = "identity") +
  geom_hline(yintercept = y_x_data %>%
               summarise(sd05 = quantile(`BECCS`, probs = 0.5)) %>% pull(), color = "blue", linetype = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(-XMAX, 0))+
  theme_bw() +
  scale_fill_manual(values = c(ColorViridis4)) +
  themeblank +
  theme(plot.margin = margin(b = 0)) +
  coord_flip() -> ptop_BECCS



### combine p----
ptop_BECCS + (LUC_vs_BECCS +theme(plot.margin = margin(t = 0,l = 5, r = 7), legend.position = "none",
                                  axis.text.y = element_blank())) + plot_layout(heights = c(0.1, 1)) -> p2;p2


# Fig1b BECCS vs. AFOLU vs. FFI----

EM_AFOLU_BECCS %>% spread(Var, value) %>%
  filter(!is.na(BECCS)) %>% filter(year %in% c(2050, 2100)) -> EMS_LUC_FFI_BECCS

y_title = "AFOLU"; x_title = "FFI"
y_x_data <- EMS_LUC_FFI_BECCS %>% filter(year == 2100) %>%  mutate(FFI = `FFI without BECCS` + BECCS)

LADQ5 <- rq(get(y_title)~ get(x_title) + BECCS, tau = 0.5, data = y_x_data)
LADQ9 <- rq(get(y_title)~ get(x_title) + BECCS, tau = 0.95, data = y_x_data)
LADQ1 <- rq(get(y_title)~ get(x_title) + BECCS, tau = 0.05, data = y_x_data)

ggplot() +
  guides(shape =guide_legend( order = 1),
         linetype = guide_legend( order = 2),
         color = guide_legend(order = 3))+

  geom_abline(slope = LADQ1$coefficients[2],intercept = LADQ1$coefficients[1] + LADQ1$coefficients[3] * quantile(y_x_data$BECCS, probs = 0.95), linetype = 5, color = "blue")+
  geom_abline(slope = LADQ5$coefficients[2],intercept = LADQ5$coefficients[1] + LADQ5$coefficients[3] * quantile(y_x_data$BECCS, probs = 0.5), linetype = 5, color = "blue")+
  geom_abline(slope = LADQ9$coefficients[2],intercept = LADQ9$coefficients[1] + LADQ9$coefficients[3] * quantile(y_x_data$BECCS, probs = 0.05), linetype = 5, color = "blue")+
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(slope = -1, intercept = 1475) +
  geom_abline(slope = -1, intercept = 1150) +
  geom_abline(slope = -1, intercept = 825) +
  geom_abline(slope = -1, intercept = 500) +
  geom_abline(slope = -1, intercept = 175) +


  scale_x_continuous(expand = c(0,0), limits = c(0, XMAX), breaks = XBREAKs_right) +
  scale_y_continuous(expand = c(0,0), limits = YLIMITs, breaks = YBREAKS) +
  geom_point(data = y_x_data %>% filter(EM_CO2_2100_cut %in% CutRange),
             aes(y = get(y_title), x = get(x_title), group = Pathway,
                 fill = EM_CO2_2100), color = "black", alpha= PALPHA0, size = PSIZE0, shape = 22
  ) +
  #scales::show_col(viridis::viridis(11, alpha = PALPHA0))
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn",
               palette = function(x) c(ColorViridis4),
               breaks = c(500, 825, 1150),
               limits = c(175, 1475),
               show.limits = TRUE,
               guide = "colorsteps",
               name = expression(paste("AR6 CB ", "(",GtCO[2],")")) ) +

  scale_shape_manual(values = c(21, 22, 24), name = "This study") +
  scale_color_npg(name = "UCT (This study)") +
  scale_linetype_manual(values = c(1, 5), name = "Target (This study)") +


  labs(y = expression(paste("LULUCF ", "(",GtCO[2],")")),
       x = expression(paste("EIP ", "(",GtCO[2],")"))) +
  theme_bw()+ theme0 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.width = unit(0.8,"cm"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5, b = 0))) +
  theme(panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(l = 8, t = 0)) +
  annotate("text", x = 170, y = 210, label = "Q5-fitted", color = "blue")+
  annotate("text", x = 170, y = -100, label = "Q50-fitted", color = "blue")+
  annotate("text", x = 170, y = -280, label = "Q95-fitted", color = "blue")+

  annotate("text", x = 925, y = -800, label = "CB = 175", angle = TEXT_ANGLE, size = 4, hjust = 1)+
  annotate("text", x = 930 + 325, y = -800, label = "CB = 500", angle = TEXT_ANGLE, color = "red", size = 4, hjust = 1)+
  annotate("text", x = 930 + 325 * 2, y = -800, label = "CB = 825", angle = TEXT_ANGLE, size = 4, hjust = 1)+
  annotate("text", x = 305, y = 800, label = "CB = 1150", angle = TEXT_ANGLE, color = "red", size = 4, hjust = 0)+
  annotate("text", x = 305 + 325, y = 800, label = expression(paste("CB = 1475 ",GtCO[2])),
           angle = TEXT_ANGLE, size = 4, hjust = 0) ->
  LUC_vs_FFI;LUC_vs_FFI

  # add GCAM ----
  if (ADD_GCAM == T) {
  LUC_vs_FFI +
    # geom_path(data = EMS_LUC_FFI_BECCS_GCAM1,
    #           aes(x = `FFI`, y = `AFOLU`, group = interaction(scenario, target),
    #               color = LCT, linetype = target), size = 1) +
    geom_point(data = EMS_LUC_FFI_BECCS_GCAM1 %>% filter(year == 2100) %>% mutate(DB = "GCAM"),
               aes(x = `FFI`, y = `AFOLU`, group = interaction(scenario, target),
                   shape = DB),
               color = "black",
               alpha = 0.8, size = 3, fill = "red") -> LUC_vs_FFI
  }


df1 <- y_x_data %>%
  group_by(EM_CO2_2100_cut) %>%
  summarise(sd0 = quantile(`FFI`, probs = 0.05),
            sd25 = quantile(`FFI`, probs = 0.25),
            sd05 = quantile(`FFI`, probs = 0.5),
            sd75 = quantile(`FFI`, probs = 0.75),
            sd100 = quantile(`FFI`, probs = .95)) %>% ungroup()
ggplot(data = df1,
       aes(x = EM_CO2_2100_cut)) +
  geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                   ymax = sd100, fill =  EM_CO2_2100_cut), alpha= PALPHA0, lwd=0.5, fatten = 1, stat = "identity") +
  geom_hline(yintercept = y_x_data %>%
               summarise(sd05 = quantile(`FFI`, probs = 0.5)) %>% pull(), color = "blue", linetype = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0, XMAX))+
  theme_bw() +
  scale_fill_manual(values = c(ColorViridis4)) +
  #scale_fill_viridis_d(direction = 1)  +
  themeblank +
  theme(plot.margin = margin(b = 0)) +
  coord_flip() -> ptop_FFI


df1 <- y_x_data %>%
  group_by(EM_CO2_2100_cut) %>%
  summarise(sd0 = quantile(`AFOLU`, probs = 0.05),
            sd25 = quantile(`AFOLU`, probs = 0.25),
            sd05 = quantile(`AFOLU`, probs = 0.5),
            sd75 = quantile(`AFOLU`, probs = 0.75),
            sd100 = quantile(`AFOLU`, probs = .95)) %>% ungroup()

ggplot(df1, aes(x = EM_CO2_2100_cut)) +
  geom_boxplot(aes(ymin = sd0, lower = sd25, middle = sd05, upper = sd75,
                   ymax = sd100, fill =  EM_CO2_2100_cut), alpha= PALPHA0, lwd=0.5, fatten = 1, stat = "identity") +
  geom_hline(yintercept = y_x_data %>% filter(year == 2100) %>%
               summarise(sd05 = quantile(`AFOLU`, probs = 0.5)) %>% pull(), color = "blue", linetype = 1) +
  scale_y_continuous(expand = c(0,0), limits = YLIMITs)+
  theme_bw() +  scale_fill_manual(values = c(ColorViridis4)) +
  themeblank +
  theme(plot.margin = margin(b = 0, l = 0)) -> pright_AFOLU

### combine ----
(ptop_FFI + plot_spacer() + plot_layout(widths = c(1, 0.1)) )/
  ((LUC_vs_FFI + theme(plot.margin = margin(t = 0),#, legend.position = "none"
                       axis.title.y = element_blank(),
                       axis.text.y = element_text(hjust = 0.6)) ) +
     pright_AFOLU + plot_layout(widths = c(1, 0.1)) ) +
  plot_layout(heights = c(0.1, 1),guides = 'collect') -> p1;p1



## merge all ----

LUC_vs_BECCS +theme(plot.margin = margin(t = 0,l = 5, r = 7), legend.position = "none",
                    axis.text.y = element_blank()) -> p1_1

LUC_vs_FFI +
  theme(plot.margin = margin(t = 0, r = 10),#, legend.position = "none"
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 0.6)) -> p1_2



((ptop_BECCS / p1_1 + plot_layout(heights = c(0.07, 1))) |
  (ptop_FFI / p1_2 + plot_layout(heights = c(0.07, 1))) |
  (plot_spacer() / pright_AFOLU + plot_layout(heights = c(0.07, 1))) )+
  plot_layout(widths = c(0.4, 0.4, 0.03), guides = 'collect') -> p3; p3


  if (ADD_GCAM == T) {
    p3 %>% Write_png(paste0(OutFolderName, "/MainFig1_BECCS_LULUCF_EIP_AR6_GCAM"), h = 4300, w = 9600, r = 600)
    }else{
    p3 %>% Write_png(paste0(OutFolderName, "/MainFgi1_BECCS_LULUCF_EIP_AR6"), h = 4300, w = 9600, r = 600)
  }

}

