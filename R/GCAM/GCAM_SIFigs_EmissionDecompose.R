SIFigs_EmissionDecompose <- function(){

  outdir <- "output/GCAM/"
  OutFolderName <- "SI"
  dir.create(file.path(outdir, OutFolderName), showWarnings = F)



# Step1: Decompose biogenic carbon by supply and demand ----

BioGenicToBECCS <- LoadFigData("BioGenicToBECCS")


## Check capture rates ----
BioGenicToBECCS %>%
  group_by_at(vars(-year, -value)) %>%
  Fill_annual(CUMULATIVE = T) %>% filter(year == 2100) %>%
  ungroup() %>%
  spread(source, value) %>%
  # need to shift biogenic carbon from NoCCS to CCS for Heat & End-use (mainly cheimical & alumina)
  # adjustment when CCS exist in Biogenic_CCS_Captured but not Biogenic_CCS
  mutate(Biogenic_CCS_Captured = BECCS,
         Biogenic_CCS_NotCaptured = Biogenic_CCS - Biogenic_CCS_Captured) %>%
  mutate(Biogenic_NoCCS = if_else(Biogenic_CCS_NotCaptured > 0, Biogenic_NoCCS + Biogenic_CCS_NotCaptured, Biogenic_NoCCS),
         Biogenic_CCS_NotCaptured = if_else(Biogenic_CCS_NotCaptured > 0, 0, Biogenic_CCS_NotCaptured ) ) ->
  BioGenicToBECCS1


BioGenicToBECCS %>%
  spread(source, value) %>%
  # need to shift biogenic carbon from NoCCS to CCS for Heat & End-use (mainly cheimical & alumina)
  # adjustment when CCS exist in Biogenic_CCS_Captured but not Biogenic_CCS
  mutate(Biogenic_CCS_Captured = BECCS,
         Biogenic_CCS_NotCaptured = Biogenic_CCS - Biogenic_CCS_Captured) %>%
  mutate(Biogenic_NoCCS = if_else(Biogenic_CCS_NotCaptured > 0, Biogenic_NoCCS + Biogenic_CCS_NotCaptured, Biogenic_NoCCS),
         Biogenic_CCS_NotCaptured = if_else(Biogenic_CCS_NotCaptured > 0, 0, Biogenic_CCS_NotCaptured ) ) %>%
  select(scenario, sector, year, Biogenic_CCS_Captured, Biogenic_CCS_NotCaptured ) %>%
  mutate(Biogenic_CCS_NotCaptured = -Biogenic_CCS_NotCaptured ) %>%

  gather(variable, value, -scenario, -sector, -year) %>%
  mutate(sector = case_when(
    grepl("Electricity", sector) ~ "Electricity",
    grepl("Refining", sector) ~ "Refining",
    TRUE ~ "Hydrogen & Final energy")) %>%
  Agg_reg(sector, variable) %>%
  mutate(sector = factor(sector, levels = c("Electricity", "Refining", "Hydrogen & Final energy"))) %>%
  proc_scen() %>%
  ggplot() +  facet_grid( LandSupply ~ LCT) +
  geom_hline(yintercept = 0) +
  guides(fill = guide_legend(order = 1),
         alpha = guide_legend(order = 2)) +
  geom_bar(aes(x = year, y = value, fill = sector, alpha = variable), stat = "identity", position = "stack",
           color = "black") +
  scale_alpha_manual(values = c(1, 0.6), name = "CCS Biogenic C", labels = c("Captured", "Not captured")) +
  labs(x = "Scenario",
       y = expression(paste(GtCO[2], " ", yr^-1)) , fill = "Sector") +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A1; A1



BioGenicToBECCS1 %>%
  select(scenario, sector, year,
         Biogenic_CCS_Captured, Biogenic_CCS_NotCaptured
  ) %>%
  mutate(Biogenic_CCS_NotCaptured = -Biogenic_CCS_NotCaptured ) %>%

  gather(variable, value, -scenario, -sector, -year) %>%
  mutate(sector = case_when(
    grepl("Electricity", sector) ~ "Electricity",
    grepl("Refining", sector) ~ "Refining",
    TRUE ~ "Hydrogen & Final energy")) %>%
  Agg_reg(sector, variable) %>%
  mutate(sector = factor(sector, levels = c("Electricity", "Refining", "Hydrogen & Final energy"))) %>%
  proc_scen() ->
  BioGenicToBECCS2

BioGenicToBECCS2 %>%
  ggplot() +  facet_grid( ~LandSupply ) +
  geom_hline(yintercept = 0) +
  guides(fill = guide_legend(order = 1),
         alpha = guide_legend(order = 2)) +
  geom_bar(aes(x = LCT, y = value, fill = sector, alpha = variable), stat = "identity", position = "stack",
           color = "black") +

  geom_text(data =  BioGenicToBECCS2 %>%
              mutate(value = abs(value)) %>%
              group_by(LCT, LandSupply, sector) %>% mutate(value = value /sum(value)) %>%
              filter(sector == "Electricity", variable == "Biogenic_CCS_Captured"),
            aes(x = LCT, y = 500, label = paste0(round(value * 100,0), "%")),
            hjust = 0.5, size = 4, color = "red", fontface = 4
  ) +

  geom_text(data =  BioGenicToBECCS2 %>%
              mutate(value = abs(value)) %>%
              group_by(LCT, LandSupply, sector) %>% mutate(value = value /sum(value)) %>%
              filter(sector == "Refining", variable == "Biogenic_CCS_Captured"),
            aes(x = LCT, y = 400, label = paste0(round(value * 100,0), "%")),
            hjust = 0.5, size = 4, color = "green", fontface = 4
  ) +

  geom_text(data =  BioGenicToBECCS2 %>%
              mutate(value = abs(value)) %>%
              group_by(LCT, LandSupply, sector) %>% mutate(value = value /sum(value)) %>%
              filter(sector == "Hydrogen & Final energy", variable == "Biogenic_CCS_Captured"),
            aes(x = LCT, y = 300, label = paste0(round(value * 100,0), "%")),
            hjust = 0.5, size = 4, color = "blue", fontface = 4
  ) +


  geom_text(data =  BioGenicToBECCS2 %>%
              mutate(value = abs(value)) %>%
              Agg_reg(LCT, LandSupply, variable) %>%
              group_by(LCT, LandSupply) %>%
              mutate(value = value /sum(value)) %>%
              filter( variable == "Biogenic_CCS_Captured"),
            aes(x = LCT, y = -800, label = paste0(round(value * 100,0), "%")),
            hjust = 0.5, size = 4, color = "black", fontface = 4
  ) +


  scale_alpha_manual(values = c(1, 0.6), name = "CCS Biogenic C", labels = c("Captured", "Not captured")) +
  labs(x = "Scenario", y = expression(paste(GtCO[2])), fill = "Sector") +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A2; A2


(A2 + labs(title = "(A) Cumulative biogenic carbon flow in 2020 - 2100 in CCS sectors") +
    theme(plot.title = element_text(hjust = 0, face = "bold") ) ) /
  (A1  +  labs(title = "(B) Annual biogenic carbon flow in CCS sectors") +
     theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none") )+
  plot_layout(heights = c(0.4, 1),guides = 'collect') -> p


p %>% Write_png(paste0(OutFolderName, "/SIFig_GCAM_CCS_capture"), h = 4500, w = 4400,  r = 300)



# Non Co2 ----

NonCO2Check <- LoadFigData("NonCO2Check")

NonCO2Check %>% filter(sector != "Other GHGs_UnMGMTLand") %>%
  #left_join(NZ_PeakYear, by = "scenario") %>%
  #filter(!(!is.na(NZyear) & year > NZyear)) %>% select(-NZyear) %>%
  proc_scen() %>% mutate(year = as.integer(year)) %>%
  ggplot +   facet_grid( LandSupply ~ LCT) +
  geom_hline(yintercept = 0) +
  geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
            color = "black") +
  labs(x = "Year",
       y = expression(paste(GtCO[2]-equivalent, " ", yr^-1)) ) +
  scale_fill_brewer(palette = "Set1", name = "Source", direction = -1,
                    labels = c(expression(paste(CH[4], " Agriculture")),
                               expression(paste(CH[4], " Energy")),
                               expression(paste(CH[4], " Unmanaged Land")),
                               expression(paste(N[2], "O Agriculture")),
                               expression(paste(N[2], "O Energy")),
                               expression(paste(N[2], "O Unmanaged Land")),
                               "Other GHGs")
  ) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90), legend.text.align = 0,
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A1; A1


NonCO2Check %>% filter(sector != "Other GHGs_UnMGMTLand") %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  #left_join(NZ_PeakYear, by = "scenario") %>%
  #filter(!(!is.na(NZyear) & year > NZyear)) %>% select(-NZyear) %>%
  group_by(scenario) %>%
  filter(year == unique(last(year))) %>% ungroup() %>%
  proc_scen() %>% mutate(year = as.integer(year)) %>%
  ggplot +   facet_grid( ~LandSupply) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = LCT, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  geom_hline(yintercept = 900, color = "blue", linetype = 2) +

  geom_text(data =   NonCO2Check %>% filter(sector != "Other GHGs_UnMGMTLand") %>%
              group_by_at(vars(-value, -year)) %>%
              Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
              group_by(scenario) %>%
              filter(year == unique(last(year))) %>% ungroup() %>%
              group_by(scenario) %>% summarise(value = round(sum(value),0)) %>%
              proc_scen() ,
            aes(x = LCT, y = 1050, label = value),
            hjust = 0.5, size = 6, color = "blue", fontface = 4
  ) +
  labs(x = "Scenario", y = expression(paste(GtCO[2]-equivalent))) +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 900, 1000)) +
  scale_fill_brewer(palette = "Set1", name = "Source", direction = -1,
                    labels = c(expression(paste(CH[4], " Agriculture")),
                               expression(paste(CH[4], " Energy")),
                               expression(paste(CH[4], " Unmanaged Land")),
                               expression(paste(N[2], "O Agriculture")),
                               expression(paste(N[2], "O Energy")),
                               expression(paste(N[2], "O Unmanaged Land")),
                               "Other GHGs")
  ) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A2; A2


(A2 + labs(title = "(A) Cumulative non-carbon dioxide GHG emissions in 2020 - 2100") +
    theme(plot.title = element_text(hjust = 0, face = "bold")) ) /
  (A1  +  labs(title = "(B) Annual non-carbon dioxide GHG emissions") +
     theme(plot.title = element_text(hjust = 0, face = "bold")) )+
  plot_layout(heights = c(0.4, 1),guides = 'collect') -> p

p %>% Write_png(paste0(OutFolderName, "/SIFig_GCAM_NonCO2_cumulative"), h = 4500, w = 4400,  r = 300)



# emission decomposition ----

NCEM_sector_agg_reg <- LoadFigData("NCEM_sector_agg_reg")

NCEM_sector_agg_reg %>% Agg_reg(sector) ->
  NCEM_sector_agg

NCEM_sector_agg %>%
  Agg_reg() -> NCEM_sector_agg_Total

NCEM_sector_agg_Total %>%
  group_by(scenario) %>%
  Fill_annual(CUMULATIVE = T) %>%
  mutate(max = max(value)) %>%
  filter(value == max) %>% proc_scen() ->NZyears

NCEM_sector_agg %>%
  group_by_at(vars(-value, - year)) %>%
  Fill_annual() %>% ungroup() %>%
  proc_scen() %>%mutate(year = as.integer(year)) %>%
  mutate(sector = factor(sector,
                         levels = c("Electricity", "Refining", "Gas & Hydrogen",
                                    "Building & Heating", "Industry", "Transport",
                                    "1G bio-feedstock", "2G bio-feedstock", "LULUCF")
  ) ) %>%
  ggplot + facet_grid( LandSupply ~ LCT) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  geom_hline(yintercept = 0) +
  geom_vline(data = NZyears, aes(xintercept = year), color = "red", linetype = 1, size = 1) +
  geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
            color = "black", size = 0.4) +
  geom_line(data = NCEM_sector_agg_Total %>% proc_scen() %>% mutate(year = as.integer(year), ss = "Net Total"),
            aes(x = year, y = value, color = ss ), size = 1.2, linetype = 2) +
  labs(x = "Year",
       y = expression(paste(GtCO[2], " ", yr^-1)) , fill = "Sector", color = "") +
  scale_fill_brewer(palette = "RdBu", name = "Source", direction = -1) +
  scale_color_manual(values = "black") +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90), legend.text.align = 0,
        legend.key.width = unit(2,"line"),
        panel.grid = element_blank()) -> A1; A1


NCEM_sector_agg %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  filter(year == 2100) %>%
  Agg_reg() -> NCEM_sector_agg_Total

NCEM_sector_agg %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  group_by(scenario) %>%
  filter(year == unique(last(year))) %>% ungroup() %>%
  proc_scen() %>%mutate(year = as.integer(year)) %>%
  mutate(sector = factor(sector,
                         levels = c("Electricity", "Refining", "Gas & Hydrogen",
                                    "Building & Heating", "Industry", "Transport",
                                    "1G bio-feedstock",
                                    "2G bio-feedstock", "LULUCF") ) ) %>%
  ggplot + facet_grid(~LandSupply) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +

  geom_bar(aes(x = LCT, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +

  geom_hline(yintercept = 0) +
  geom_point(data = NCEM_sector_agg_Total %>% proc_scen() %>% mutate(year = as.integer(year), ss = "Net Total"),
             aes(x = LCT, y = value, color = ss), size = 10, shape = "-") +
  labs(x = "Year", y = expression(paste(GtCO[2])), fill = "Sector", color = "") +
  scale_fill_brewer(palette = "RdBu", name = "Source", direction = -1) +
  scale_color_manual(values = "black") +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A2; A2



(A2 + labs(title = "(A) Cumulative carbon dioxide emissions in 2020 - 2100") +
    theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none") ) /
  (A1  +  labs(title = "(B) Annual carbon dioxide emissions") +
     theme(plot.title = element_text(hjust = 0, face = "bold")) )+
  plot_layout(heights = c(0.4, 1),guides = 'collect') -> p


p %>% Write_png(paste0(OutFolderName, "/SIFig_GCAM_CarbonEmissions"), h = 4500, w = 4400,  r = 300)


## regional results ----

readr::read_csv("data/maps/Regmapping.csv") -> Regmapping

NCEM_sector_agg_reg %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6), by = "region0") %>%
  Agg_reg(region, sector) %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  group_by(scenario) %>%
  filter(year == unique(last(year))) %>% ungroup() %>%
  proc_scen() %>%mutate(year = as.integer(year)) %>%
  mutate(sector = factor(sector,
                         levels = c("Electricity", "Refining", "Gas & Hydrogen",
                                    "Building & Heating", "Industry", "Transport",
                                    "1G bio-feedstock",
                                    "2G bio-feedstock", "LULUCF") ) ) ->
  NCEM_sector_agg_reg1

NCEM_sector_agg_reg1 %>%
  Agg_reg(region) -> NCEM_sector_agg_reg_Total

NCEM_sector_agg_reg1 %>%
  ggplot + facet_grid(LandSupply ~ region) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  geom_hline(yintercept = 250, linetype = 2, color = "grey") +
  geom_hline(yintercept = -250, linetype = 2, color = "grey") +

  geom_bar(aes(x = LCT, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +

  geom_hline(yintercept = 0) +
  geom_point(data = NCEM_sector_agg_reg_Total %>% proc_scen() %>% mutate(year = as.integer(year), ss = "Net Total"),
             aes(x = LCT, y = value, color = ss), size = 10, shape = "-") +

  geom_text(data =  NCEM_sector_agg_reg1 %>%
              group_by(LCT, LandSupply, region) %>% summarise(value = sum(value)),
            aes(x = LCT, y = 500, label = round(value,0)),
            hjust = 0.5, size = 4.5, color = "black", fontface = 4
  ) +

  labs(x = "Year", y = expression(paste(GtCO[2])), fill = "Sector", color = "") +
  scale_fill_brewer(palette = "RdBu", name = "Source", direction = -1) +
  scale_color_manual(values = "black") +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        axis.title.x = element_blank(), legend.position = "bottom",
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A3; A3

plot_grid(
  A2 + labs(title = "(A) Global cumulative carbon dioxide emissions in 2020 - 2100") +
    theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "right"),
  A3  +  labs(title = "(B) Regional cumulative carbon dioxide emissions in 2020 - 2100") +
    theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none"), ncol = 1,
  align = c("v"), rel_heights = c(0.5, 1)

) -> p

p %>% Write_png(paste0(OutFolderName, "/SIFig_GCAM_CarbonEmissions_reg"), h = 5550, w = 5400,  r = 300)



# alternative method ---*
NCEM_sector_agg2 <- LoadFigData("CEM_decompose_BECCS_LUC")



NCEM_sector_agg2 %>%
  Agg_reg() -> NCEM_sector_agg_Total

NCEM_sector_agg_Total %>%
  group_by(scenario) %>%
  Fill_annual(CUMULATIVE = T) %>%
  mutate(max = max(value)) %>%
  filter(value == max) %>% proc_scen() -> NZyears

NCEM_sector_agg2 %>%
  group_by_at(vars(-value, - year)) %>%
  Fill_annual() %>% ungroup() %>%
  proc_scen() %>% mutate(year = as.integer(year)) %>%
  mutate(sector = factor(sector,
                         levels = c("Electricity", "Refining", "Gas & Hydrogen", "Final energy",
                                    "BECCS: Residue & MSW", "BECCS: Purpose-grown",
                                    "LULUCF") ) ) %>%
  ggplot + facet_grid(LandSupply ~ LCT) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  geom_hline(yintercept = 0) +
  geom_vline(data = NZyears, aes(xintercept = year), color = "red", linetype = 1, size = 1) +
  geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
            color = "black", size = 0.4) +
  geom_line(data = NCEM_sector_agg_Total %>% proc_scen() %>% mutate(year = as.integer(year), ss = "Net Total"),
            aes(x = year, y = value, color = ss ), size = 1.2, linetype = 2) +
  labs(x = "Year", y = expression(paste(GtCO[2], " ", yr^-1)) , fill = "Sector", color = "") +
  scale_fill_brewer(palette = "RdBu", name = "Source", direction = -1) +
  scale_color_manual(values = "black") +
  scale_y_continuous(breaks = seq(-20, 40, 10)) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90), legend.text.align = 0,
        legend.key.width = unit(2,"line"),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A1; A1


NCEM_sector_agg2 %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  filter(year == 2100) %>%
  Agg_reg() %>% mutate(ss = "Net Total") %>%
  bind_rows(
    NCEM_sector_agg2 %>%
      filter(grepl("BECCS|LULUCF", sector)) %>%
      group_by_at(vars(-value, -year)) %>%
      Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
      filter(year == 2100) %>%
      Agg_reg() %>% mutate(ss = "Land-based CDR")
  ) %>%
  bind_rows(
    NCEM_sector_agg2 %>%
      filter(!grepl("BECCS|LULUCF", sector)) %>%
      group_by_at(vars(-value, -year)) %>%
      Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
      filter(year == 2100) %>%
      Agg_reg() %>% mutate(ss = "FFI")
  )-> NCEM_sector_agg_Total


NCEM_sector_agg2 %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = T) %>% ungroup() %>%
  group_by(scenario) %>%
  filter(year == unique(last(year))) %>% ungroup() %>%
  proc_scen() %>%mutate(year = as.integer(year)) %>%
  mutate(sector = factor(sector,
                         levels = c("Electricity", "Refining", "Gas & Hydrogen", "Final energy",
                                    "BECCS: Residue & MSW", "BECCS: Purpose-grown",
                                    "LULUCF") ) ) %>%
  ggplot + facet_grid(~LandSupply) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  geom_bar(aes(x = LCT, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  geom_hline(yintercept = 0) +
  geom_point(data = NCEM_sector_agg_Total %>% proc_scen() %>% mutate(year = as.integer(year)),
             aes(x = LCT, y = value, color = ss), size = 10, shape = "-") +

  geom_text(data =  NCEM_sector_agg_Total %>% proc_scen() %>%
              select(LCT, LandSupply, ss, value) %>% spread(ss, value),
            aes(x = LCT, y = 1825, label = round(FFI, 0)),
            hjust = 0.5, size = 6, color = "blue", fontface = 4
  ) +
  geom_text(data =  NCEM_sector_agg_Total %>% proc_scen() %>%
              select(LCT, LandSupply, ss, value) %>% spread(ss, value),
            aes(x = LCT, y = -850, label = round(`Land-based CDR`,0)),
            hjust = 0.5, size = 5.5, color = "red", fontface = 4
  ) +

  labs(x = "Year", y = expression(paste(GtCO[2])), fill = "Sector", color = "") +
  scale_fill_brewer(palette = "RdBu", name = "Source", direction = -1) +
  scale_color_manual(values = c("blue", "red", "Black")) +
  scale_y_continuous(breaks = c(-500, 0, 500, 1000, 1500)) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines")) -> A2; A2


(A2 + labs(title = "(A) Cumulative carbon dioxide emissions in 2020 - 2100") +
    theme(plot.title = element_text(hjust = 0, face = "bold") ) ) /
  (A1  +  labs(title = "(B) Annual carbon dioxide emissions") +
     theme(plot.title = element_text(hjust = 0, face = "bold"), legend.position = "none") )+
  plot_layout(heights = c(0.4, 1),guides = 'collect') -> p

p %>% Write_png(paste0(OutFolderName, "/SIFig_GCAM_CarbonEmissions_attribute"), h = 5550, w = 5400,  r = 300)

}


