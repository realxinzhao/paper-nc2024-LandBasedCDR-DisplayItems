
Load_AR6_vetted(FromCache = T) -> AR6_vettedC1_4 # a subset of 1300+ Variables

#AR6_vettedC1_4 %>% distinct(Pathway)

#saveRDS(AR6_vettedC1_4, "data/AR6/RDS_temp/AR6_vettedC1_4.rds")

# Generate TCRP plots and narrowed pathway list ----
TCRP(CB_only = T) -> Pathway175_1475


# Only keep pathways in [175, 1475] GtCO2 ----
AR6_vettedC1_4 %>% filter(Pathway %in% Pathway175_1475) -> AR6_604
AR6_604 %>% distinct(Pathway)
saveRDS(AR6_604, "output/AR6/RDS_temp/AR6_604.rds")



library(dplyr)
library(ggplot2)
library(tidyr)
library(gcamdata)

library(patchwork)
library(quantreg)

outdir <- "output/GCAM/"

# Sources ----
source("R/info.R")
source("R/Figure_func.R")
source("R/data_func.R")
source("R/AR6/AR6_module_funcs.R")


# AR6 load data ----


source("R/AR6/AR6_MainFig1_LandCDR_AR6GCAM.R")
MainFig1_LandCDR_AR6GCAM(ADD_GCAM = F)
MainFig1_LandCDR_AR6GCAM(ADD_GCAM = T)


source("R/AR6/AR6_MainFig5_LandRemovalEfficiency.R")
MainFig5_LandRemovalEfficiency()


readRDS("output/AR6/RDS_temp/AR6_604.rds") -> AR6_604


AR6_604 %>%
  filter(year == 2100)%>% mutate(year = "2100") %>%
  # add cumulative values
  bind_rows(
    AR6_604 %>% group_by_at(vars(-year, -value)) %>%
      mutate(value = sum(value)/1000) %>%
      mutate(year = "Cumulative2020_2100",
             Unit = paste0("1000 ", Unit))
  ) %>%
  group_by(Variable, Var, Element, Unit, year) %>%
  summarise(n = n(),
            Q10 = quantile(value, 0.1),
            Q25 = quantile(value, 0.25),
            median = median(value),
            Q75 = quantile(value, 0.75),
            Q90 = quantile(value, 0.9),
            mean = mean(value),
            .groups = "drop") %>%
  arrange(year) -> A; A
A %>% write.csv(file.path(outdir, "AR6_vettedC1_4_STAT_603Cum.csv"))


AR6_604 %>% filter(Var %in% c("EM_CO2_AFOLU", "BECCS")) %>%
  group_by_at(vars(-year, -value, -Variable, -Element)) %>%
  summarise(value = sum(value)/1000) %>%
  mutate(year = "Cumulative2020_2100",
         Unit = paste0("1000 ", Unit)) %>%
  spread(Var, value) %>%
  filter(EM_CO2_AFOLU < 0)-> A



# Generate BioEnergy_ModernBioCCSResidue figures ----
BioEnergyCCSResidue(INPUT_DF = AR6_603)


# run GCAM_EMs() -> EMs in GCAM_main.R
source("R/AR6/AR6_module_funcs_Fig1.R")
Fig1(INPUT_DF = AR6_604, ADD_GCAM = F, PSIZE = 1.8, PALPHA = 0.8)
Fig1(INPUT_DF = AR6_604, ADD_GCAM = T, PSIZE = 1.8)
Fig1(INPUT_DF = AR6_604, ADD_GCAM = T, ADD_GCAM_MainOnly = F, PSIZE = 1.8)

source("R/AR6/AR6_module_funcs_TS.R")
AR6_TimeSeries_EMs(CUMULATIVE = T)
AR6_TimeSeries_Land()
AR6_TimeSeries_PrimaryEnergy()
AR6_TimeSeries_KeyVar(GREPL_Char = "^Primary Energy", Plot_Height = 4500)
AR6_TimeSeries_KeyVar(GREPL_Char = "^Final Energy", Plot_Height = 3800)

AR6_TimeSeries_KeyVar(GREPL_Char = "^Price", Plot_Height = 4500, UNIT = "Index")

AR6_TimeSeries_KeyVar(GREPL_Char = "Yield", Plot_Height = 3800, UNIT = "t DM/ha/yr")






# AR6_vettedC1_4 %>% distinct(Pathway)
# # Load C1-C8 category from meta
# MS_Category <- readxl::read_excel("data/AR6/1648976687084-AR6_Scenarios_Database_World_v1.0/AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx",
#                                   sheet = 2) %>%
#   select(Model, Scenario, Category, Category_name, Policy_category, Policy_category_name,
#          Project_study, Lit = `Literature Reference (if applicable)`,
#          `Time horizon`, Scenario_family, Scenario_scope, Sectoral_scope,
#          Ssp_family, Technology_category_name)
# names(MS_Category)
#
#
# EM_AFOLU_BECCS %>%
#   filter(year == 2100) %>%
#   left_join(MS_Category ) %>%
#   spread(Var, value) -> A
# write.csv(A, "AllScen.csv")


