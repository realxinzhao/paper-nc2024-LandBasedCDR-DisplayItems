
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
require(readxl)

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

source("R/AR6/AR6_SIFigs_AR6Pathways_Summary.R")
SIFigs_AR6Pathways_Summary()

source("R/AR6/AR6_SIFigs_TimeSeries_Emissions.R")
SIFigs_TimeSeries_Emissions()

source("R/AR6/AR6_SIFigs_LandCDR_AR6_byModel.R")
SIFigs_LandCDR_AR6_byModel()


